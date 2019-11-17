/**
dlsl.dual_quaternion


Authors: Peter Particle
License: MIT

Based on: Ben Kenwright: A Beginners Guide to Dual-Quaternions 
*/

module dlsl.dual_quaternion;

public import dlsl.quaternion;

/// Pre-defined quaternion of type float.
alias DualQuaternion!( float ) dualQ;


/// If T is a quaternion, this evaluates to true, otherwise false.
template isDualQuaternion( T )  {  enum isDualQuaternion = is( typeof( isDualQuaternionImpl( T.init )));  }
private void isDualQuaternionImpl( T )( Quaternion!( T ) q )  {}
alias isDualQ = isDualQuaternion;


/// Base template for all quaternion-types.
/// Params:
///  type = all values get stored as this type
struct DualQuaternion( type ) {
    union {
        type[8] data;
        struct {
            Quaternion!type r;
            Quaternion!type d;
        }
    }

    alias type valueType;   /// Holds the internal type of the vector.
    alias Quaternion!( type ) quaternionType;
   

    /// Returns a pointer to the data in memory, it starts with the w coordinate.
    @property auto ptr()        {  return data.ptr;  }

    /// Returns the current dual quternion formatted as string, useful for printing
    @property string asString() { import std.string : format; return format( "%s", data ); }
    alias asString toString;

    /// Returns an identity vector ( x=0, y=0, z=0, w=1 ).
    static @property DualQuaternion identity() {
        return DualQuaternion( 
            cast( type )0, cast( type )0, cast( type )0, cast( type )1, 
            cast( type )0, cast( type )0, cast( type )0, cast( type )0
        );
    }


    /// Construct from eight values of type $( I valueType )
    /// first four the real part, rotation quaternion with last value being the w value
    /// last four being the dual, translation part
    this()(
        valueType rx, valueType ry, valueType rz, valueType rw, 
        valueType dx, valueType dy, valueType dz, valueType dw
        ) {
        data[0] = rx; data[1] = ry; data[2] = rz; data[3] = rw;
        data[4] = dx; data[5] = dy; data[6] = dz; data[7] = dw;
    }


    /// Construct from two Quaternions
    this()( Quaternion!valueType r, Quaternion!valueType d ) {
        this.r = r.normalize;
        this.d = d;
    }

    this()( Quaternion!valueType r, Vector!( valueType, 3 ) v ) {
        this.r = r.normalize;
        this.d = Quaternion!valueType( v, 0 ) * this.r * 0.5;
        // Todo(pp): confirm that the above is an associative operation and the same as bellow
        // this.d = ( Quaternion!valueType( v, 0 ) * this.r ) * 0.5;
    }



    ///////////////
    // Operators //
    ///////////////

    /// DualQuternion add or subtract DUalQuaternion
    DualQuaternion opBinary( string op )( DualQuaternion dq ) const if(( op == "+" ) || ( op == "-" )) {
        DualQuaternion result;
        mixin( "result[] = data[] " ~ op ~ " dq[];" );
        return result;
    }

    // Multiplication order - left to right 
    //static DualQuaternion operator * ( DualQuaternion lhs, DualQuaternion rhs ) {
    //    return DualQuaternion( rhs.m_real * lhs.m_real, rhs.m_dual * lhs.m_real + rhs.m_real * lhs.m_dual );
    //}

    DualQuaternion opBinary( string op : "*" )( DualQuaternion dq ) const {
        return DualQuternion( dq.r * r, dq.d * r + dq.r * d );
    }


    DualQuaternion opBinary( string op : "*" )( valueType s ) const {
        auto result = this;
        result[] *= s;
        return result;
        //return DualQuaternion( 
        //  s * r.x, s * r.y, s * r.z, s * r.w,
        //  s * d.x, s * d.y, s * d.z, s * d.w  
        //);
    }


    Quaternion!valueType rotation() {
        return r;
    }


    Vector!( valueType, 3 ) translation() {
        //Quaternion t = dq.m_dual;
        //t.scaleIt( 2.0 );
        //t *= dq.m_real.conjugate();
        //return MVector( t.x, t.y, t.z );
        return Vector!( valueType, 3 )( d * 2 * dlsl.quaternion.invert( r ));
    }
}


/// Convenience functions returning the vector as corresponding matrices
/// Params:
///     q = convert quternion 
/// Returns: mat4 / mat4x4, mat4x3
import dlsl.matrix;
auto toMat4(   DQ )( DQ dq ) if( isDualQuaternion!DQ && is( DQ.valueType : float )) { return mat4(   dq ); }
auto toMat4x3( DQ )( DQ dq ) if( isDualQuaternion!DQ && is( DQ.valueType : float )) { return mat4x3( dq ); }
auto toMat4x4( DQ )( DQ dq ) if( isDualQuaternion!DQ && is( DQ.valueType : float )) { return mat4x4( dq ); }


/// Dual Quaternion dot product
DQ.valueType dot( DQ )( in DQ a, in DQ b ) if( isDualQuaternion!DQ ) {
    return dot( a.r, b.r );
}


/// Dual Quaternion normalize
DQ normalize( DQ )( in DQ dq ) if( isDualQuaternion!DQ ) {
    auto l = dq.r.lengthSquared;
    import dlsl.math : almostEqual;
    if( l == 0 || almostEqual( 1.0f, l, 0.00000001f ))
        return dq;
    auto recipLength = 1.0 / l.sqrt;
    DQ result = dq;
    result.r *= recipLength;
    result.d *= recipLength;
    return result;
}

DQ invert( DQ )( in DQ dq ) if( isDualQuaternion!DQ ) {
    return DQ(
        dlsl.quaternion.invert( dq.r ),
        dlsl.quaternion.invert( dq.d )
    );
} alias conjugate = invert;

