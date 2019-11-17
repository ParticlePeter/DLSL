/**
dlsl.quternion


Authors: Peter Particle ( based on gl3n by David Herberth )
License: MIT

Note: All methods marked with pure are weakly pure since, they all access an instance member.
All static methods are strongly pure.
*/

module dlsl.quaternion;

import std.conv : to;
import std.math : asin, atan2, cos, sin, sqrt;
import std.format : /*s*/format;
import std.traits : isFloatingPoint, isArray;

import dlsl.matrix;
public import dlsl.vector;

/// Pre-defined quaternion of type float.
alias Quaternion!( float ) quat;


/// If T is a quaternion, this evaluates to true, otherwise false.
template isQuaternion( T )  {  enum isQuaternion = is( typeof( isQuaternionImpl( T.init )));  }
private void isQuaternionImpl( T )( Quaternion!( T ) q )  {}


/// Base template for all quaternion-types.
/// Params:
///  type = all values get stored as this type
struct Quaternion( type ) {
    Vector!( type, 4 ) vector;

    alias vector this;      /// The Quaternion can be treated as a vector, which cen be treated as an array
    alias type valueType;   /// Holds the internal type of the vector.


    /// Returns a pointer to the vector in memory
    @property auto ptr() {
        return vector.data.ptr;
    }

    /// Returns the current quternion formatted as string, useful for printing
    @property auto toString( char[] buffer ) {
        assert( buffer.length >= 64, "At least 64 chars buffer capacity required!" );
        return vector.toString( buffer );
    }

    /// Returns an identity vector ( x=0, y=0, z=0, w=1 ).
    static @property Quaternion identity() {
        return Quaternion( 0, 0, 0, 1 );
    }


    /// Construct from four values of type $( I valueType ), last one the w coordinate
    this( T )( T x, T y, T z, T w ) if( is( T : type )) {
        vector.x = x;
        vector.y = y;
        vector.z = z;
        vector.w = w;
    }

    /// Construct from a 3d vector / array imaginary part and a real value w of type $( I valueType )
    this()( Vector!( valueType, 3 ) vec, valueType w ) {
        vector[0..3] = vec[];
        vector.w = w;
    }

    /// Construct from 4d vector / array, w or index 3 respectively being the quternions w ccord
    this()( valueType[ 4 ] array ) {
        vector = array;
    }

    /// Construct / convert from 3x3 matrix
    this()( const ref Matrix!( valueType, 3, 3 ) mat ) {
        valueType trace = mat[0][0] + mat[1][1] + mat[2][2];
        if( trace > 0 ) {
            real s = 0.5 / sqrt( trace + 1.0f );
            x = to!valueType(( mat[2][1] - mat[1][2] ) * s );
            y = to!valueType(( mat[0][2] - mat[2][0] ) * s );
            z = to!valueType(( mat[1][0] - mat[0][1] ) * s );
            w = to!valueType( 0.25 / s );
        } else if(( mat[0][0] > mat[1][1] ) && ( mat[0][0] > mat[2][2] )) {
            real s = 2.0 * sqrt( 1.0 + mat[0][0] - mat[1][1] - mat[2][2] );
            x = to!valueType( 0.25f * s );
            y = to!valueType(( mat[0][1] + mat[1][0] ) / s );
            z = to!valueType(( mat[0][2] + mat[2][0] ) / s );
            w = to!valueType(( mat[2][1] - mat[1][2] ) / s );
        } else if( mat[1][1] > mat[2][2] ) {
            real s = 2.0 * sqrt( 1 + mat[1][1] - mat[0][0] - mat[2][2] );
            x = to!valueType(( mat[0][1] + mat[1][0] ) / s );
            y = to!valueType( 0.25f * s );
            z = to!valueType(( mat[1][2] + mat[2][1] ) / s );
            w = to!valueType(( mat[0][2] - mat[2][0] ) / s );
        } else {
            real s = 2.0 * sqrt( 1 + mat[2][2] - mat[0][0] - mat[1][1] );
            x = to!valueType(( mat[0][2] + mat[2][0] ) / s );
            y = to!valueType(( mat[1][2] + mat[2][1] ) / s );
            z = to!valueType( 0.25f * s );
            w = to!valueType(( mat[1][0] - mat[0][1] ) / s );
        }
    }



    //////////////
    // Rotation //
    //////////////

    /// Returns a vector with applied rotation around the x-axis.
    static Quaternion rotationX( real angle ) {
        Quaternion result;

        angle   /= 2;
        result.x = to!valueType( sin( angle ));
        result.y = 0;
        result.z = 0;
        result.w = to!valueType( cos( angle ));

        return result;
    }

    /// Returns a vector with applied rotation around the y-axis.
    static Quaternion rotationY( real angle ) {
        Quaternion result;

        angle   /= 2;
        result.x = 0;
        result.y = to!valueType( sin( angle ));
        result.z = 0;
        result.w = to!valueType( cos( angle ));

        return result;
    }

    /// Returns a vector with applied rotation around the z-axis.
    static Quaternion rotationZ( real angle ) {
        Quaternion result;

        angle   /= 2;
        result.x = 0;
        result.y = 0;
        result.z = to!valueType( sin( angle ));
        result.w = to!valueType( cos( angle ));

        return result;
    }

    /// Returns a vector with applied rotation around an axis.
    static Quaternion rotation( real angle, Vector!( valueType, 3 ) axis ) {
        if( angle == 0 ) {
            return Quaternion.identity;
        }
        Quaternion result;

        angle /= 2;
        valueType sin_angle = to!valueType( sin( angle ));

        result.x = axis.x * sin_angle;
        result.y = axis.y * sin_angle;
        result.z = axis.z * sin_angle;
        result.w = to!valueType( cos( angle ));

        return result;
    }

    /// Creates a vector from an euler rotation.
    static Quaternion rotation( real roll, real pitch, real yaw ) {
        Quaternion result;

        auto cr = cos( roll / 2.0 );
        auto cp = cos( pitch / 2.0 );
        auto cy = cos( yaw / 2.0 );
        auto sr = sin( roll / 2.0 );
        auto sp = sin( pitch / 2.0 );
        auto sy = sin( yaw / 2.0 );

        result.x = sr * cp * cy - cr * sp * sy;
        result.y = cr * sp * cy + sr * cp * sy;
        result.z = cr * cp * sy - sr * sp * cy;
        result.w = cr * cp * cy + sr * sp * sy;

        return result;
    }

    unittest {
        enum startPitch = 0.1;
        enum startYaw = -0.2;
        enum startRoll = 0.6;

        auto q = quat.euler_rotation( startRoll, startPitch, startYaw );

        assert( almostEqual( q.pitch, startPitch ));
        assert( almostEqual( q.yaw, startYaw ));
        assert( almostEqual( q.roll, startRoll ));
    }

    /// Rotates the current vector around the x-axis and returns $( I this ).
    Quaternion rotateX( real angle ) {
        this = rotationX( angle ) * this;
        return this;
    }

    /// Rotates the current vector around the y-axis and returns $( I this ).
    Quaternion rotateY( real angle ) {
        this = rotationY( angle ) * this;
        return this;
    }

    /// Rotates the current vector around the z-axis and returns $( I this ).
    Quaternion rotateZ( real angle ) {
        this = rotationZ( angle ) * this;
        return this;
    }

    /// Rotates the current vector around an axis and returns $( I this ).
    Quaternion rotate( real angle, Vector!( valueType, 3 ) axis ) {
        this = rotation( angle, axis ) * this;
        return this;
    }

    /// Applies an euler rotation to the current vector and returns $( I this ).
    Quaternion rotate( real heading, real attitude, real bank ) {
        this = rotation( heading, attitude, bank ) * this;
        return this;
    }

    unittest {
        assert( quat.xrotation( PI ).vector[1..4] == [1.0f, 0.0f, 0.0f] );
        assert( quat.yrotation( PI ).vector[1..4] == [0.0f, 1.0f, 0.0f] );
        assert( quat.zrotation( PI ).vector[1..4] == [0.0f, 0.0f, 1.0f] );
        assert(( quat.xrotation( PI ).w == quat.yrotation( PI ).w ) && ( quat.yrotation( PI ).w == quat.zrotation( PI ).w ));
        //assert( quat.rotatex( PI ).w == to!( quat.valueType )( cos( PI )) );
        assert( quat.xrotation( PI ).vector == quat.identity.rotatex( PI ).vector );
        assert( quat.yrotation( PI ).vector == quat.identity.rotatey( PI ).vector );
        assert( quat.zrotation( PI ).vector == quat.identity.rotatez( PI ).vector );

        assert( quat.axis_rotation( PI, vec3( 1.0f, 1.0f, 1.0f )).vector[1..4] == [1.0f, 1.0f, 1.0f] );
        assert( quat.axis_rotation( PI, vec3( 1.0f, 1.0f, 1.0f )).w == quat.xrotation( PI ).w );
        assert( quat.axis_rotation( PI, vec3( 1.0f, 1.0f, 1.0f )).vector ==
               quat.identity.rotate_axis( PI, vec3( 1.0f, 1.0f, 1.0f )).vector );

        quat q1 = quat.euler_rotation( PI, PI, PI );
        assert(( q1.x > -1e-16 ) && ( q1.x < 1e-16 ));
        assert(( q1.y > -1e-16 ) && ( q1.y < 1e-16 ));
        assert(( q1.z > -1e-16 ) && ( q1.z < 1e-16 ));
        //assert( q1.w == -1.0f );
        assert( quat.euler_rotation( PI, PI, PI ).vector == quat.identity.rotate_euler( PI, PI, PI ).vector );
    }



    ///////////////
    // Operators //
    ///////////////

    Quaternion opBinary( string op )( Quaternion q ) const  if(( op == "+" ) || ( op == "-" )) {
        Quaternion result;

        mixin( "result.x = x" ~ op ~ "q.x;" );
        mixin( "result.y = y" ~ op ~ "q.y;" );
        mixin( "result.z = z" ~ op ~ "q.z;" );
        mixin( "result.w = w" ~ op ~ "q.w;" );

        return result;
    }

    Quaternion opBinary( string op : "*" )( Quaternion q ) const {
        Quaternion result;

        result.x =  x * q.w + y * q.z - z * q.y + w * q.x;
        result.y = -x * q.z + y * q.w + z * q.x + w * q.y;
        result.z =  x * q.y - y * q.x + z * q.w + w * q.z;
        result.w = -x * q.x - y * q.y - z * q.z + w * q.w;

        return result;
    }

    Vector!( valueType, 3 ) opBinary( string op : "*" )( Vector!( valueType, 3 ) v ) const {
        Vector!( valueType, 3 ) result;

        valueType w_w = w ^^ 2;
        valueType w_2 = w * 2;
        valueType wx2 = w_2 * x;
        valueType wy2 = w_2 * y;
        valueType wz2 = w_2 * z;
        valueType x_x = x ^^ 2;
        valueType x_2 = x * 2;
        valueType xy2 = x_2 * y;
        valueType xz2 = x_2 * z;
        valueType y_y = y ^^ 2;
        valueType yz2 = 2 * y * z;
        valueType z_z = z * z;

        result.x =  w_w * v.x + wy2 * v.z - wz2 * v.y + x_x * v.x +
                    xy2 * v.y + xz2 * v.z - z_z * v.x - y_y * v.x;
        result.y =  xy2 * v.x + y_y * v.y + yz2 * v.z + wz2 * v.x -
                    z_z * v.y + w_w * v.y - wx2 * v.z - x_x * v.y;
        result.z =  xz2 * v.x + yz2 * v.y + z_z * v.z - wy2 * v.x -
                    y_y * v.z + wx2 * v.y - x_x * v.z + w_w * v.z;

       return result;
    }

    Quaternion opBinary( string op : "*" )( valueType s ) const {
        return Quaternion( s * x, s * y, s * z, s * w );
    }

    auto opBinaryRight( string op, T )( T q ) const if( !isQuaternion!T ) {
        return this.opBinary!( op )( q );
    }

    void opOpAssign( string op : "*" )( Quaternion q ) {
        valueType w2 = -x * q.x - y * q.y - z * q.z + w * q.w;
        valueType x2 =  x * q.w + y * q.z - z * q.y + w * q.x;
        valueType y2 = -x * q.z + y * q.w + z * q.x + w * q.y;
        valueType z2 =  x * q.y - y * q.x + z * q.w + w * q.z;
        w = w2; x = x2; y = y2; z = z2;
    }

    void opOpAssign( string op : "*" )( valueType q ) {
        vector[0] *= q;
        vector[1] *= q;
        vector[2] *= q;
        vector[3] *= q;
    }

    void opOpAssign( string op )( Quaternion q ) if(( op == "+" ) || ( op == "-" )) {
        mixin( "w = w" ~ op ~ "q.w;" );
        mixin( "x = x" ~ op ~ "q.x;" );
        mixin( "y = y" ~ op ~ "q.y;" );
        mixin( "z = z" ~ op ~ "q.z;" );
    }



    unittest {
        quat q1 = quat.identity;
        quat q2 = quat( 3.0f, 0.0f, 1.0f, 2.0f );
        quat q3 = quat( 3.4f, 0.1f, 1.2f, 2.3f );

        assert(( q1 * q1 ).vector == q1.vector );
        assert(( q1 * q2 ).vector == q2.vector );
        assert(( q2 * q1 ).vector == q2.vector );
        quat q4 = q3 * q2;
        assert(( q2 * q3 ).vector != q4.vector );
        q3 *= q2;
        assert( q4.vector == q3.vector );
        assert( almostEqual( q4.x, 0.4f ));
        assert( almostEqual( q4.y, 6.8f ));
        assert( almostEqual( q4.z, 13.8f ));
        assert( almostEqual( q4.w, 4.4f ));

        quat q5 = quat( 1.0f, 2.0f, 3.0f, 4.0f );
        quat q6 = quat( 3.0f, 1.0f, 6.0f, 2.0f );

        assert(( q5 - q6 ).vector == [-2.0f, 1.0f, -3.0f, 2.0f] );
        assert(( q5 + q6 ).vector == [4.0f, 3.0f, 9.0f, 6.0f] );
        assert(( q6 - q5 ).vector == [2.0f, -1.0f, 3.0f, -2.0f] );
        assert(( q6 + q5 ).vector == [4.0f, 3.0f, 9.0f, 6.0f] );
        q5 += q6;
        assert( q5.vector == [4.0f, 3.0f, 9.0f, 6.0f] );
        q6 -= q6;
        assert( q6.vector == [0.0f, 0.0f, 0.0f, 0.0f] );

        quat q7 = quat( 2.0f, 2.0f, 2.0f, 2.0f );
        assert(( q7 * 2 ).vector == [4.0f, 4.0f, 4.0f, 4.0f] );
        assert(( 2 * q7 ).vector == ( q7 * 2 ).vector );
        q7 *= 2;
        assert( q7.vector == [4.0f, 4.0f, 4.0f, 4.0f] );

        vec3 v1 = vec3( 1.0f, 2.0f, 3.0f );
        assert(( q1 * v1 ) == v1 );
        assert(( v1 * q1 ) == ( q1 * v1 ) );
        assert(( q2 * v1 ) == [-2.0f, 36.0f, 38.0f] );
    }

    int opCmp( ref const Quaternion q ) const {
        foreach( i, a; vector ) {
            if( a < q.vector[i] ) {
                return -1;
            } else if( a > q.vector[i] ) {
                return 1;
            }
        }
        return 0;   // Quaternions are the same
    }

    bool opEquals( const Quaternion q ) const {
        return vector == q.vector;
    }

    bool opCast( T : bool )() const  {
        return isFinite( this );
    }

    unittest {
        assert( quat( 1.0f, 2.0f, 3.0f, 4.0f ) == quat( 1.0f, 2.0f, 3.0f, 4.0f ));
        assert( quat( 1.0f, 2.0f, 3.0f, 4.0f ) != quat( 1.0f, 2.0f, 3.0f, 3.0f ));

        assert( !( quat( float.nan, float.nan, float.nan, float.nan )) );
        if( quat( 1.0f, 1.0f, 1.0f, 1.0f )) { }
        else { assert( false ); }
    }
}



/////////////////////////////////
// free functions akin to glsl //
/////////////////////////////////


/// Returns true if all values are not nan and finite, otherwise false.
bool isFinite( Q )( const ref Q q ) if( isQuaternion!Q ) {
    import std.math : isNaN, isInfinity;
    foreach( q; vector ) {
        if( isNaN( q ) || isInfinity( q )) {
            return false;
        }
    }
    return true;
}


unittest {
    quat q1 = quat( 0.0f, 0.0f, 0.0f, 1.0f );
    assert( q1.vector == [0.0f, 0.0f, 0.0f, 1.0f] );
    assert( q1.vector == quat( 0.0f, 0.0f, 0.0f, 1.0f ).vector );
    assert( q1.vector == quat( 0.0f, vec3( 0.0f, 0.0f, 1.0f )).vector );
    assert( q1.vector == quat( vec4( 0.0f, 0.0f, 0.0f, 1.0f )).vector );

    assert( q1.isFinite );
    q1.x = float.infinity;
    assert( !q1.isFinite );
    q1.x = float.nan;
    assert( !q1.isFinite );
    q1.x = 0.0f;
    assert( q1.isFinite );
}


/// Returns an inverted copy of the current vector
Q invert( Q )( const ref Q q ) if ( isQuaternion!Q ) {
    return Q( -q.x, -q.y, -q.z, q.w );
} alias conjugate = invert;


unittest {
    quat q1 = quat( 1.0f, 1.0f, 1.0f, 1.0f );

    assert( q1.length == 2.0f );
    assert( q1.lengthSquared == 4.0f );
    assert( q1.length == quat( 0.0f, 0.0f, 2.0f, 0.0f ).length );

    quat q2 = quat.identity;
    assert( q2.vector == [ 0.0f, 0.0f, 0.0f, 1.0f ] );
    assert( q2.x == 0.0f );
    assert( q2.y == 0.0f );
    assert( q2.z == 0.0f );
    assert( q2.w == 1.0f );

    assert( q1.invert.vector == [ -1.0f, -1.0f, -1.0f, 1.0f ] );
    q1 = q1.invert;
    assert( q1.vector == [ 1.0f, 1.0f, 1.0f, 1.0f ] );

    q1.make_identity();
    assert( q1.vector == q2.vector );

}


/// Convenience functions returning the vector as corresponding matrices
/// Params:
///     q = convert quternion
/// Returns: mat3 / mat3x3, mat4 / mat4x4, mat3x4, mat4x3
import dlsl.matrix;
auto toMat3(   Q )( Q q ) if( isQuaternion!Q && is( Q.valueType : float )) { return mat3(   q ); }
auto toMat4(   Q )( Q q ) if( isQuaternion!Q && is( Q.valueType : float )) { return mat4(   q ); }
auto toMat3x3( Q )( Q q ) if( isQuaternion!Q && is( Q.valueType : float )) { return mat3x3( q ); }
auto toMat3x4( Q )( Q q ) if( isQuaternion!Q && is( Q.valueType : float )) { return mat3x4( q ); }
auto toMat4x3( Q )( Q q ) if( isQuaternion!Q && is( Q.valueType : float )) { return mat4x3( q ); }
auto toMat4x4( Q )( Q q ) if( isQuaternion!Q && is( Q.valueType : float )) { return mat4x4( q ); }


unittest {
    quat q1 = quat( 4.0f, 1.0f, 2.0f, 3.0f );

    assert( q1.to_matrix!( 3, 3 ) == [[-25.0f, -20.0f, 22.0f], [28.0f, -19.0f, 4.0f], [-10.0f, 20.0f, -9.0f]] );
    assert( q1.to_matrix!( 4, 4 ) == [[-25.0f, -20.0f, 22.0f, 0.0f],
                                          [28.0f, -19.0f, 4.0f, 0.0f],
                                          [-10.0f, 20.0f, -9.0f, 0.0f],
                                          [0.0f, 0.0f, 0.0f, 1.0f]] );
    assert( quat.identity.to_matrix!( 3, 3 ) == Matrix!( valueType, 3, 3 ).identity );
    assert( q1.vector == quat.from_matrix( q1.to_matrix!( 3, 3 )).vector );

    assert( quat( 1.0f, 0.0f, 0.0f, 0.0f ).vector == quat.from_matrix( mat3.identity ).vector );

    quat q2 = quat.from_matrix( mat3( 1.0f, 3.0f, 2.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f ));
    assert( q2.x == 0.0f );
    assert( almostEqual( q2.y, 0.7071067f ));
    assert( almostEqual( q2.z, -1.060660 ));
    assert( almostEqual( q2.w, 0.7071067f ));
}



/// lenghtSquared, length and normalize through vector
unittest {
    quat q1 = quat( 1.0f, 2.0f, 3.0f, 4.0f );
    quat q2 = quat( 1.0f, 2.0f, 3.0f, 4.0f );

    q1.normalize();
    assert( q1.vector == q2.normalized.vector );
    //assert( q1.vector == q1.normalized.vector );
    assert( almostEqual( q1.length, 1.0 ));
}


/// Returns the yaw
Q.valueType yaw( Q )( const ref Q q ) if ( isQuaternion!Q ) {
    return atan2( to!( Q.valueType )( 2.0*( w*z + x*y )), to!( Q.valueType )( 1.0 - 2.0*( y*y + z*z )));
}


/// Returns the pitch
Q.valueType pitch( Q )( const ref Q q ) if ( isQuaternion!Q ) {
    return asin( to!( Q.valueType )( 2.0*( w*y - z*x )));
}


/// Returns the roll
Q.valueType roll( Q )( const ref Q q ) if ( isQuaternion!Q ) {
    return atan2( to!( Q.valueType )( 2.0*( w*x + y*z )), to!( Q.valueType )( 1.0 - 2.0*( x*x + y*y )));
}


unittest {
    quat q1 = quat.identity;
    assert( q1.pitch == 0.0f );
    assert( q1.yaw == 0.0f );
    assert( q1.roll == 0.0f );

    quat q2 = quat( 1.0f, 1.0f, 1.0f, 1.0f );
    assert( almostEqual( q2.yaw, q2.roll ));
    assert( almostEqual( q2.yaw, 1.570796f ));
    assert( q2.pitch == 0.0f );

    quat q3 = quat( 0.1f, 1.9f, 2.1f, 1.3f );
    assert( almostEqual( q3.yaw, 2.4382f ));
    assert( isNaN( q3.pitch ));
    assert( almostEqual( q3.roll, 1.67719f ));

    assert(almostEqual(quat(0.0f, 0.0f, 0.0f, 0.0f), quat(0.0f, 0.0f, 0.0f, 0.0f)));
    assert(almostEqual(quat(0.0f, 0.0f, 0.0f, 0.0f), quat(0.000001f, 0.000001f, 0.000001f, 0.000001f)));

}
