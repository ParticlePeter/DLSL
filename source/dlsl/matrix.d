/**
dlsl.matrix


Authors: Peter Particle ( based on gl3n by David Herberth )
License: MIT

Note: All methods marked with pure are weakly pure since, they all access an instance member.
All static methods are strongly pure.
*/

module dlsl.matrix;

public import dlsl.vector;

import std.conv : to;
import std.math : sqrt, sin, cos, tan, PI;
import std.traits : isNumeric, isFloatingPoint;
import std.algorithm : max, min, reduce;


nothrow @nogc:


/// predefined matrix types, the first number represents the number of cols
/// and the second the number of columns, if there's just one it's a nxn matrix.
/// All of these matrices are floating - point matrices.
alias Matrix!( float, 2, 2 ) mat2;
alias Matrix!( float, 3, 3 ) mat3;
alias Matrix!( float, 4, 4 ) mat4;

alias Matrix!( float, 2, 2 ) mat2x2;
alias Matrix!( float, 2, 3 ) mat2x3;
alias Matrix!( float, 2, 4 ) mat2x4;

alias Matrix!( float, 3, 2 ) mat3x2;
alias Matrix!( float, 3, 3 ) mat3x3;
alias Matrix!( float, 3, 4 ) mat3x4;

alias Matrix!( float, 4, 2 ) mat4x2;
alias Matrix!( float, 4, 3 ) mat4x3;
alias Matrix!( float, 4, 4 ) mat4x4;



enum float  deg2rad = 0.0174532925199432957692369076849f;
enum float  rad2deg = 57.295779513082320876798154814105f;

version( NoReciprocalMul ) {
    private enum rmul = false;
}   else {
    private enum rmul = true;
}

/// If T is a matrix, this evaluates to true, otherwise false
template isMatrix( T ) {  enum isMatrix = is( typeof( isMatrixImpl( T.init )));  }
private void isMatrixImpl( T, int cols, int rows )( Matrix!( T, cols, rows ) mat ) {}


/// Base template for all matrix types. See predefined matrix types
/// Params:
/// type = the value type of each matrix element
/// cols = count of columns of the matrix
/// rows = count of rows of the matrix
/// - - -
struct Matrix( type, int numCols, int numRows ) if(( numCols > 1 ) && ( numRows > 1 )) {

    nothrow @nogc:

    /// Returns the current matrix formatted as flat string.
    char[] toString( char[] buffer ) {
        import core.stdc.stdio : sprintf;

        size_t s = 0;
        s += sprintf( buffer.ptr + s, "[ ");
        s += data[0].toString( buffer[ s .. $ ] ).length;

        s += sprintf( buffer.ptr + s, ", ");
        s += data[1].toString( buffer[ s .. $ ] ).length;

        static if( cols >= 3 ) {
            s += sprintf( buffer.ptr + s, ", ");
            s += data[2].toString( buffer[ s .. $ ] ).length;
        }

        static if( cols == 4 ) {
            s += sprintf( buffer.ptr + s, ", ");
            s += data[3].toString( buffer[ s .. $ ] ).length;
        }

        s += sprintf( buffer.ptr + s, " ]");

        return buffer[ 0 .. s ];
    }


    pure nothrow @nogc:

    /// Returns the pointer to the stored values as OpenGL requires it.
    /// Note this will return a pointer to a $( RED column - major ) matrix,
    /// $( RED this is the OpneGL convention and expected in programs via Uniforms or UBOs ).
    auto ptr() {
        return data[ 0 ].ptr;
    }


    pure nothrow @nogc @safe:

    alias type valueType;                       /// Holds the internal primitive type of the matrix;
    alias Vector!( type, rows ) vectorType;     /// Holds the internal vectorType of the matrix;

    /// Holds the matrix $( RED column - major ) in memory.
    vectorType[ cols ] data;                    /// Each Column is Vector of length rows
    alias data this;                            /// Treat this matrix as an nxm array
    alias cols = numCols;                       /// Holds the number of cols;
    alias rows = numRows;                       /// Holds the number of columns;


    unittest {      // Construction through alias this static array of Vectors

        mat2 m2 = mat2( 0.0f, 1.0f, 2.0f, 3.0f );
        m2 = mat2( [ 0.0f, 1.0f ], [ 2.0f, 3.0f ] );
        m2 = mat2( vec2( 0.0f, 1.0f ), vec2( 2.0f, 3.0f ));
        assert( m2[ 0 ][ 0 ] == 0.0f );
        assert( m2[ 0 ][ 1 ] == 1.0f );
        assert( m2[ 1 ][ 0 ] == 2.0f );
        assert( m2[ 1 ][ 1 ] == 3.0f );

        m2[ 0 ] = [ 2.0, 2.0 ];
        m2[ 0 ] = vec2( 2.0, 2.0 );
        //m2[ 0 .. 1 ] = [ [ 2.0f, 2.0f ] ];
        m2[ 0 .. 1 ] = [ vec2( 2.0, 2.0 ) ];

        assert( m2 == [ [ 2.0f, 2.0f ], [ 2.0f, 3.0f ] ] );

        mat3 m3 = mat3( 0.0f, 0.1f, 0.2f, 1.0f, 1.1f, 1.2f, 2.0f, 2.1f, 2.2f );
        assert( m3[ 0 ][ 1 ] == 0.1f );
        assert( m3[ 2 ][ 0 ] == 2.0f );
        assert( m3[ 1 ][ 2 ] == 1.2f );

        m3[ 0 ][ 0 .. $ ] = 0.0f;
        assert(  m3 == [[ 0.0f, 0.0f, 0.0f ],
                        [ 1.0f, 1.1f, 1.2f ],
                        [ 2.0f, 2.1f, 2.2f ] ] );
        //m3[ 1 .. 3 ]  = [ [ 1, 1, 1 ], [ 2, 2, 2 ] ];
        m3[ 1 .. 3 ] =  [ vec3( 1, 1, 1 ), vec3( 2, 2, 2 ) ];
        assert(  m3 == [[ 0.0f, 0.0f, 0.0f ],
                        [ 1.0f, 1.0f, 1.0f ],
                        [ 2.0f, 2.0f, 2.0f ]] );

        mat4 m4 = mat4( 0.0f, 0.1f, 0.2f, 0.3f,
                             1.0f, 1.1f, 1.2f, 1.3f,
                             2.0f, 2.1f, 2.2f, 2.3f,
                             3.0f, 3.1f, 3.2f, 3.3f );
        assert( m4[ 0 ][ 3 ] == 0.3f );
        assert( m4[ 1 ][ 1 ] == 1.1f );
        assert( m4[ 2 ][ 0 ] == 2.0f );
        assert( m4[ 3 ][ 2 ] == 3.2f );

        m4[ 2 ][ 1 .. 3 ] = [ 1.0f, 2.0f ];
        assert(  m4 == [[ 0.0f, 0.1f, 0.2f, 0.3f ],
                        [ 1.0f, 1.1f, 1.2f, 1.3f ],
                        [ 2.0f, 1.0f, 2.0f, 2.3f ],
                        [ 3.0f, 3.1f, 3.2f, 3.3f ]] );

    }







    /// Returns the current matrix as pretty formatted string.
    /// TODO : Check This
    /*
    string asPrettyString() {
        string fmtr = "%s";

        size_t rjust = max( format( fmtr, reduce!( max )( data[] )).length,
                                  format( fmtr, reduce!( min )( data[] )).length ) - 1;

        string[] outer_parts;
        foreach( type[] col; data ) {
            string[] inner_parts;
            foreach( type row; col ) {
                inner_parts ~= rightJustify( format( fmtr, row ), rjust );
            }
            outer_parts ~= " [ " ~ join( inner_parts, ", " ) ~ " ]";
        }

        return "[ " ~ join( outer_parts, "\n" )[ 1 .. $ ] ~ " ]";
    } alias asPrettyString toPrettyString;
    */


    @safe pure nothrow :
    template isCompatibleMatrix( T ) {  enum isCompatibleMatrix = is( typeof( isCompatibleMatrixImpl( T.init )));  }
    static void isCompatibleMatrixImpl( int col, int row )( Matrix!( type, col, row ) mat ) {}

    template isCompatibleVector( T ) {  enum isCompatibleVector = is( typeof( isCompatibleVectorImpl( T.init )));  }
    static void isCompatibleVectorImpl( int dim )( Vector!( type, dim ) vec ) {}


    /// TODO: Fix Construction with combination of numeric, static and dynamic array, vector and matrix
    private void construct( int i, T, Tail... )( T head, Tail tail ) {
        static if( i >= cols * rows ) {
            static assert( false, "constructor has too many arguments" );
        }   else static if( is( T : type )) {
            data[ i / rows ][ i % rows ] = head;
            construct!( i + 1 )( tail );
        }   else static if( is( T == Vector!( type, rows ))) {
            static if( i % rows == 0 ) {
                data[ i / rows ] = head;
                construct!( i + T.dimension )( tail );
            }   else {
                static assert( false, "Can't convert Vector into the matrix. Maybe it doesn't align to the columns correctly or dimension doesn't fit" );
            }
        }   /*else {
            static assert( false, "Matrix constructor argument must be of type " ~ type.stringof ~ " or Vector, not " ~ T.stringof );
        }*/
    }

    private void construct( int i )() {}    // terminate


    /// Constructs the matrix:
    /// If a single value is passed, the matrix will be cleared with this value ( each column in each col will contain this value ).
    /// If a matrix with more cols and columns is passed, the matrix will be the upper left nxm matrix.
    /// If a matrix with less cols and columns is passed, the passed matrix will be stored in the upper left of an identity matrix.
    /// It's also allowed to pass vectors and scalars at a time, but the vectors dimension must match the number of columns and align correctly.
    /// Examples:
    /// - - -
    /// mat2 m2 = mat2( 0.0f ); // mat2 m2 = mat2( 0.0f, 0.0f, 0.0f, 0.0f );
    /// mat3 m3 = mat3( m2 ); // mat3 m3 = mat3( 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f );
    /// mat3 m3_2 = mat3( vec3( 1.0f, 2.0f, 3.0f ), 4.0f, 5.0f, 6.0f, vec3( 7.0f, 8.0f, 9.0f ));
    /// mat4 m4 = mat4.identity; // just an identity matrix
    /// mat3 m3_3 = mat3( m4 ); // mat3 m3_3 = mat3.identity
    /// - - -
    this( Args... )( Args args ) {  construct!( 0 )( args );  }

    /// Construct a Matrix from another Matrix, equal sized or bigger
//  this( T )( T mat ) if( isMatrix!T && ( T.rows >= rows ) && ( T.cols >= cols )) {
//      for( int c = 0; c < cols; ++c ) {
//          for( int r = 0; r < rows; ++r ) {
//              data[ c ][ r ] = mat.data[ c ][ r ];
//          }
//      }
//  }


    /// Construct a Matrix from another Matrix
    this( T )( T mat ) if( isMatrix!T ) {
        this( 1 );  // Could be optimized with not initializing and setting missing values within static if range checks
        //alias Cols = cols < T.cols ? cols : T.cols;
        //alias Rows = rows < T.rows ? rows : T.rows;
        foreach( c; 0 .. cols < T.cols ? cols : T.cols ) {
            foreach( r; 0 .. rows < T.rows ? rows : T.rows ) {
                data[ c ][ r ] = mat.data[ c ][ r ];
            }
        }
    }


    /// Construct a Matrix from a single value, for non- and square matrices
    /// see GLSL 4.5 Spec, section 5.4.2 Vector and Matrix Constructors
    this()( type value ) {
        clear( 0 );
        foreach( rc; 0 .. cols < rows ? cols : rows ) {
            data[ rc ][ rc ] = value;
        }
    }





    /// Construct a matrix from a quaternion or dual quaternion
    static if( cols >= 3 && rows >= 3 ) {
        import dlsl.quaternion, dlsl.dual_quaternion;

        /// common code for quternion and dual quaternion
        private void rotationFromQuternion( const ref Quaternion!type q ) {
            type xx = q.x ^^ 2;     //     xx      = X * X;
            type xy = q.x * q.y;    //     xy      = X * Y;
            type xz = q.x * q.z;    //     xz      = X * Z;
            type xw = q.x * q.w;    //     xw      = X * W;

            type yy = q.y ^^ 2;     //     yy      = Y * Y;
            type yz = q.y * q.z;    //     yz      = Y * Z;
            type yw = q.y * q.w;    //     yw      = Y * W;

            type zz = q.z ^^ 2;     //     zz      = Z * Z;
            type zw = q.z * q.w;    //     zw      = Z * W;

            data[0][0] = 1 - 2 * ( yy + zz );   // m00 = 1 - 2 * ( yy + zz )
            data[1][0] = 2 * ( xy - zw );       // m01 =     2 * ( xy - zw )
            data[2][0] = 2 * ( xz + yw );       // m02 =     2 * ( xz + yw )
            data[0][1] = 2 * ( xy + zw );       // m10 =     2 * ( xy + zw )
            data[1][1] = 1 - 2 * ( xx + zz );   // m11 = 1 - 2 * ( xx + zz )
            data[2][1] = 2 * ( yz - xw );       // m12 =     2 * ( yz - xw )
            data[0][2] = 2 * ( xz - yw );       // m20 =     2 * ( xz - yw )
            data[1][2] = 2 * ( yz + xw );       // m21 =     2 * ( yz + xw )
            data[2][2] = 1 - 2 * ( xx + yy );   // m22 = 1 - 2 * ( xx + yy );
        }

        this( Q_OR_DQ )( Q_OR_DQ q ) if( isQuaternion!Q_OR_DQ || isDualQuaternion!Q_OR_DQ ) {

            // common to quaternion and dual quaternion
            static if( rows == 4 ) data[3][0] = data[3][1] = data[3][2] = 0;
            static if( cols == 4 && rows == 4 ) data[3][3] = 1;

            // quaternion
            static if( isQuaternion!Q_OR_DQ ) {
                static if( cols == 4 ) data[0][3] = data[1][3] = data[2][3] = 0;
                rotationFromQuternion( q );

            // dual quaternion
            } else {    // isDualQuaternion!Q_OR_DQ = true
                static if( cols == 4 )
                    data[ 0..3 ] = dq.translation()[];

                rotationFromQuternion( dq.r );
            }
        }
    }


    /// Sets all values of the matrix to value ( each column in each col will contain this value ).
    void clear( type value ) {
        foreach( ref vec; data )
            vec[] = value;
    }

    unittest {      /// Matrix.isvalid, Constructing

        mat2 m2 = mat2( 1.0f, 1.0f, vec2( 2.0f, 2.0f ));
        assert( m2.data == [[ 1.0f, 1.0f ], [ 2.0f, 2.0f ]] );
        m2.clear( 3.0f );
        assert( m2.data == [[ 3.0f, 3.0f ], [ 3.0f, 3.0f ]] );
        assert( m2.isvalid );
        m2.clear( float.nan );
        assert( !m2.isvalid );
        m2.clear( float.infinity );
        assert( !m2.isvalid );
        m2.clear( 0.0f );
        assert( m2.isvalid );

        mat3 m3 = mat3( 1.0f );
        assert( m3.data == [[ 1.0f, 0.0f, 0.0f ],
                            [ 0.0f, 1.0f, 0.0f ],
                            [ 0.0f, 0.0f, 1.0f ]] );

        mat4 m4 = mat4( vec4(   1.0f, 1.0f, 1.0f, 1.0f ),
                                2.0f, 2.0f, 2.0f, 2.0f,
                                3.0f, 3.0f, 3.0f, 3.0f,
                             vec4( 4.0f, 4.0f, 4.0f, 4.0f ));
        assert( m4.data == [[ 1.0f, 1.0f, 1.0f, 1.0f ],
                            [ 2.0f, 2.0f, 2.0f, 2.0f ],
                            [ 3.0f, 3.0f, 3.0f, 3.0f ],
                            [ 4.0f, 4.0f, 4.0f, 4.0f ]] );
        assert( mat3( m4 ).data == [[ 1.0f, 1.0f, 1.0f ],
                                    [ 2.0f, 2.0f, 2.0f ],
                                    [ 3.0f, 3.0f, 3.0f ]] );
        assert( mat2( mat3( m4 )).data == [[ 1.0f, 1.0f ],
                                                        [ 2.0f, 2.0f ] ] );
        assert( mat2( m4 ).data == mat2( mat3( m4 )).data );
        assert( mat4( mat3( m4 )).data ==  [[ 1.0f, 1.0f, 1.0f, 0.0f ],
                                            [ 2.0f, 2.0f, 2.0f, 0.0f ],
                                            [ 3.0f, 3.0f, 3.0f, 0.0f ],
                                            [ 0.0f, 0.0f, 0.0f, 1.0f ]] );

        Matrix!( float, 2, 3 ) mt1 = Matrix!( float, 2, 3 )( 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f );
        Matrix!( float, 3, 2 ) mt2 = Matrix!( float, 3, 2 )( 6.0f, - 1.0f, 3.0f, 2.0f, 0.0f, - 3.0f );

        assert( mt1.data == [[ 1.0f, 2.0f, 3.0f ], [ 4.0f, 5.0f, 6.0f ]] );
        assert( mt2.data == [[ 6.0f, - 1.0f ], [ 3.0f, 2.0f ], [ 0.0f, - 3.0f ]] );
    }


    //void opAssign( T )( T array )  if( is( T array == U[][], U )) { //if( isArray( A )) {
    //  pragma( msg, "Instantiat opAssign" );
    //  data[] = array[];
    //}

    static if( cols == rows ) {
        /// static create identity matrix
        static Matrix identity() {
            return Matrix( 1 );
        }

        /// Returns a identity matrix.
        bool isIdentity() {
            return data == Matrix( 1 ).data;
        }

        /// Unittest identity, isIdentity
        unittest {
            mat2 m2 = mat2( 1.0f );
            assert( m2.isIdentity );
            assert( m2.data == mat2( 1 ).data );

            m2 = mat2.identity;
            assert( m2.data == [[ 1.0f, 0.0f ],
                                [ 0.0f, 1.0f ]] );

            mat3 m3 = mat3.identity;
            assert( m3.isIdentity );
            assert( m3.data == mat3( 1.0f ).data );

            mat4 m4 = mat4( 2.0f );
            assert( m4.data == mat4( 2.0f ).data );

            m4 = mat4.identity;
            assert( m4.isIdentity );
            assert( m4.data == [[ 1.0f, 0.0f, 0.0f, 0.0f ],
                                [ 0.0f, 1.0f, 0.0f, 0.0f ],
                                [ 0.0f, 0.0f, 1.0f, 0.0f ],
                                [ 0.0f, 0.0f, 0.0f, 1.0f ]] );
            assert( m4.data == m4.identity.data );
        }




        /////////////////
        // Translation //
        /////////////////

        static if( cols == 2 ) {            // homogeneous 1D translations
            /// static construction of a translation matrix
            static Matrix translation( type x ) {
                Matrix result = Matrix.identity;
                result.data[ 1 ][ 0 ] = x;
                return result;
            }

            /// translate an existing matrix
            Matrix translate( type x ) {
                this = Matrix.translation( x ) * this;
                return this;
            }
        }

        else static if( cols == 3 ) {       // homogeneous 2D translations
            /// static construction of a translation matrix from an array/vector
            static Matrix translation( type[ 2 ] vec )  {
                Matrix result = Matrix.identity;
                result.data[ 2 ][ 0..2 ] = vec;
                return result;
            }

            /// static construction of a translation matrix from two coordinates
            static Matrix translation( type x, type y )  {
                return translation( [ x, y ] );
            }

            /// translate an existing matrix with an array/vector
            Matrix translate( type[ 2 ] vec ) {
                this = Matrix.translation( vec ) * this;
                return this;
            }

            /// translate an existing matrix with two scalars
            Matrix translate( type x, type y ) {
                this = Matrix.translation( [ x, y ] ) * this;
                return this;
            }
        }

        else static if( cols == 4 ) {       // homogeneous 3D translations
            /// static construction of a translation matrix from an array/vector
            static Matrix translation( type[ 3 ] vec )  {
                Matrix result = Matrix.identity;
                result.data[ 3 ][ 0..3 ] = vec;
                return result;
            }

            /// static construction of a translation matrix from three coordinates
            static Matrix translation( type x, type y, type z )  {
                return translation( [ x, y, z ] );
            }

            /// translate an existing matrix with a vector
            Matrix translate( type[ 3 ] vec ) {
                this = Matrix.translation( vec ) * this;
                return this;
            }

            /// translate an existing matrix with three scalars
            Matrix translate( type x, type y, type z ) {
                this = Matrix.translation( [ x, y, z ] ) * this;
                return this;
            }

            unittest {
                mat4 m4 = mat4( 1.0f );
                assert( m4.translation( 1.0f, 2.0f, 3.0f ).data == mat4.translation( 1.0f, 2.0f, 3.0f ).data );
                assert( mat4.translation( 1.0f, 2.0f, 3.0f ).data  ==  [[ 1.0f, 0.0f, 0.0f, 0.0f ],
                                                                        [ 0.0f, 1.0f, 0.0f, 0.0f ],
                                                                        [ 0.0f, 0.0f, 1.0f, 0.0f ],
                                                                        [ 1.0f, 2.0f, 3.0f, 1.0f ] ] );
                assert( mat4.identity.translate( 0.0f, 1.0f, 2.0f ).data == mat4.translation( 0.0f, 1.0f, 2.0f ).data );
            }
        }

        /// extract translation part of the matrix in a copy
        Matrix translation() {
            Matrix result = Matrix.identity;
            result[ $-1 ][ 0 .. $-1 ] = data[ $-1 ][ 0 .. $-1 ];
            return result;
        }

        unittest  {
            mat3 m3 = mat3( 0.0f, 1.0f, 2.0f,
                            3.0f, 4.0f, 5.0f,
                            6.0f, 7.0f, 1.0f );
            assert( m3.translation.data == [ [ 1.0f, 0.0f, 2.0f ], [ 0.0f, 1.0f, 5.0f ], [ 0.0f, 0.0f, 1.0f ] ] );


            mat4 m4 = mat4(  0.0f,  1.0f,  2.0f,  3.0f,
                             4.0f,  5.0f,  6.0f,  7.0f,
                             8.0f,  9.0f, 10.0f, 11.0f,
                            12.0f, 13.0f, 14.0f,  1.0f );
            assert( m4.translation.data == [ [ 1.0f, 0.0f, 0.0f,  3.0f ],
                                             [ 0.0f, 1.0f, 0.0f,  7.0f ],
                                             [ 0.0f, 0.0f, 1.0f, 11.0f ],
                                             [ 0.0f, 0.0f, 0.0f,  1.0f ] ] );
        }



        //////////////
        // Rotation // - integer rotations might not be useful, but should they be disabled?
        //////////////

        static if( cols == 2 || cols == 3 ) {       // non- and homogeneous 2D rotations
            /// static construction of a rotation matrix
            static Matrix rotation( real angle ) {
                Matrix result = Matrix.identity;
                type cosAngle = to!type( cos( angle ));
                type sinAngle = to!type( sin( angle ));
                result.data[ 0 ][ 0 ] = cosAngle;
                result.data[ 1 ][ 0 ] = - sinAngle;
                result.data[ 0 ][ 1 ] = sinAngle;
                result.data[ 1 ][ 1 ] = cosAngle;
                return result;
            }

            /// rotate an existing matrix
            Matrix rotate( real angle ) {
                this = rotation( angle ) * this;
                return this;
            }
        }

        static if( cols >= 3 ) {                        // non- and homogeneous 3D rotations
            /// static construction of a rotation matrix angle and axis
            static Matrix rotation( real angle, Vector!( type, 3 ) axis ) {
                Matrix result = Matrix.identity;

                auto length = axis.length;
                if( length != 1 ) {
                    version( NoReciprocalMul ) {
                        axis /= length;
                    }   else    {
                        auto invLength = 1.0 / length;
                        axis *= invLength;
                    }
                }

                const real cosAngle = cos( angle );
                const real sinAngle = sin( angle );

                Vector!( type, 3 ) temp = ( 1 - cosAngle ) * axis;

                result.data[ 0 ][ 0 ] = to!type( cosAngle   +   temp.x * axis.x );
                result.data[ 0 ][ 1 ] = to!type(                temp.x * axis.y + sinAngle * axis.z );
                result.data[ 0 ][ 2 ] = to!type(                temp.x * axis.z - sinAngle * axis.y );
                result.data[ 1 ][ 0 ] = to!type(                temp.y * axis.x - sinAngle * axis.z );
                result.data[ 1 ][ 1 ] = to!type( cosAngle   +   temp.y * axis.y );
                result.data[ 1 ][ 2 ] = to!type(                temp.y * axis.z + sinAngle * axis.x );
                result.data[ 2 ][ 0 ] = to!type(                temp.z * axis.x + sinAngle * axis.y );
                result.data[ 2 ][ 1 ] = to!type(                temp.z * axis.y - sinAngle * axis.x );
                result.data[ 2 ][ 2 ] = to!type( cosAngle   +   temp.z * axis.z );

                return result;
            }

            /// static construction of a rotation matrix angle and axis coordinates
            static Matrix rotation( real angle, type x, type y, type z ) {
                return Matrix.rotation( angle, Vector!( type, 3 )( x, y, z ));
            }

            /// rotate an existing matrix with an angle around an axis
            Matrix rotate( real angle, Vector!( type, 3 ) axis ) {
                this = rotation( angle, axis ) * this;
                return this;
            }

            /// rotate an existing matrix with an angle around axis coordinates
            Matrix rotate( real angle, type x, type y, type z  ) {
                this = rotation( angle, x, y, z ) * this;
                return this;
            }

            /// static construction of a rotation matrix with an angle around a canonical axis
            private static Matrix rotationA( ubyte a, ubyte b )( real angle ) {
                Matrix result = Matrix.identity;

                type cosAngle = to!type( cos( angle ));
                type sinAngle = to!type( sin( angle ));

                result.data[ a ][ a ] = cosAngle;
                result.data[ b ][ a ] = - sinAngle;
                result.data[ a ][ b ] = sinAngle;
                result.data[ b ][ b ] = cosAngle;

                return result;
            }

            /// static construction of a rotation matrix with an angle around X axis
            static Matrix rotationX( real angle ) { return rotationA!( 1, 2 )( angle ); } /// A-canonical = X

            /// static construction of a rotation matrix with an angle around Y axis
            static Matrix rotationY( real angle ) { return rotationA!( 2, 0 )( angle ); } /// A-canonical = Y

            /// static construction of a rotation matrix with an angle around Z axis
            static Matrix rotationZ( real angle ) { return rotationA!( 0, 1 )( angle ); } /// A-canonical = Z

            /// rotate an existing matrix with an angle around X axis
            Matrix rotateX( real angle ) {
                this = rotationX( angle ) * this;
                return this;
            }

            /// rotate an existing matrix with an angle around Y axis
            Matrix rotateY( real angle ) {
                this = rotationY( angle ) * this;
                return this;
            }

            /// rotate an existing matrix with an angle around Z axis
            Matrix rotateZ( real angle ) {
                this = rotationZ( angle ) * this;
                return this;
            }

            unittest {
                assert( mat4.rotationX( 0 ).data == [[ 1.0f,   0.0f,   0.0f, 0.0f ],
                                                     [ 0.0f,   1.0f, - 0.0f, 0.0f ],
                                                     [ 0.0f,   0.0f,   1.0f, 0.0f ],
                                                     [ 0.0f,   0.0f,   0.0f, 1.0f ]] );
                assert( mat4.rotationY( 0 ).data == [[ 1.0f,   0.0f,   0.0f, 0.0f ],
                                                     [ 0.0f,   1.0f,   0.0f, 0.0f ],
                                                     [ 0.0f,   0.0f,   1.0f, 0.0f ],
                                                     [ 0.0f,   0.0f,   0.0f, 1.0f ]] );
                assert( mat4.rotationZ( 0 ).data == [[ 1.0f, - 0.0f,   0.0f, 0.0f ],
                                                     [ 0.0f,   1.0f,   0.0f, 0.0f ],
                                                     [ 0.0f,   0.0f,   1.0f, 0.0f ],
                                                     [ 0.0f,   0.0f,   0.0f, 1.0f ]] );

                //mat4 rotX = mat4.identity;
                //rotX.rotateX( 1 );
                //assert( mat4.rotationX( 1 ).data == rotX.data );
                //assert( rotX.data == mat4.identity.rotateX( 1 ).data );
                //assert( rotX.data == mat4.rotation( 1, vec3( 1.0f, 0.0f, 0.0f )).data );

                //mat4 rotY = mat4.identity;
                //rotY.rotateY( 2 );
                //assert( mat4.rotationY( 2 ).data == rotY.data );
                //assert( rotY.data == mat4.identity.rotateY( 2 ).data );
                //assert( rotY.data == mat4.rotation( 2, vec3( 0.0f, 1.0f, 0.0f )).data );

                //mat4 rotZ = mat4.identity;
                //rotZ.rotateZ( 3 );
                //assert( mat4.rotationZ( 3 ).data == rotZ.data );
                //assert( rotZ.data == mat4.identity.rotateZ( 3 ).data );
                //assert( rotZ.data == mat4.rotation( 3, vec3( 0.0f, 0.0f, 1.0f )).data );

            }

            /// extract rotation part of the matrix in a copy
            // TODO(pp): fix proper extraction, currently works only right if determinant = 1
            Matrix rotation() {
                Matrix result = this;
                result[ rows - 1 ][ 0 ] = 0;
                result[ rows - 1 ][ 1 ] = 0;
                static if( cols == 4 )
                    result[ rows - 1 ][ 2 ] = 0;

                result[ 0 ][ cols - 1 ] = 0;
                result[ 1 ][ cols - 1 ] = 0;
                static if( rows == 4 )
                    result[ 2 ][ cols - 1 ] = 0;

                result[ rows - 1 ][ cols - 1 ] = 1;
                return result;
            }

            unittest {
                mat3 m3 = mat3( 0.0f, 1.0f, 2.0f,
                                3.0f, 4.0f, 5.0f,
                                6.0f, 7.0f, 1.0f );
                assert( m3.rotation.data == [[ 0.0f, 1.0f, 0.0f ],
                                             [ 3.0f, 4.0f, 0.0f ],
                                             [ 0.0f, 0.0f, 1.0f ]] );

                mat4 m4 = mat4(  0.0f,  1.0f,  2.0f,  3.0f,
                                 4.0f,  5.0f,  6.0f,  7.0f,
                                 8.0f,  9.0f, 10.0f, 11.0f,
                                12.0f, 13.0f, 14.0f,  1.0f );

                assert( m4.rotation.data == [[  0.0f,  1.0f,  2.0f, 0.0f ],
                                             [  4.0f,  5.0f,  6.0f, 0.0f ],
                                             [  8.0f,  9.0f, 10.0f, 0.0f ],
                                             [  0.0f,  0.0f,  0.0f, 1.0f ]] );
            }
        }



        /////////////
        // Scaling //
        /////////////

        static if( cols == 2 ) {                        // homogeneous 1D scaling
            /// static construction of a scaling matrix
            static Matrix scaling( type x ) {
                Matrix result = Matrix.identity;
                result.data[ 0 ][ 0 ] = x;
                return result;
            }

            /// scale an existing matrix
            Matrix scale( type x ) {
                this = Matrix.scaling( x ) * this;
                return this;
            }
        }

        static if( cols == 2 || cols == 3 ) {       // non- and homogeneous 2D scaling
            /// static construction of a scaling matrix from an array/vector
            static Matrix scaling( type[ 2 ] vec ) {
                Matrix result = Matrix.identity;
                result.data[ 0 ][ 0 ] = vec[ 0 ];
                result.data[ 1 ][ 1 ] = vec[ 1 ];
                return result;
            }

            /// static construction of a scaling matrix from two scalars
            static Matrix scaling( type x, type y ) {
                return scaling( [ x, y ] );
            }

            /// scale an existing matrix with an array/vector
            Matrix scale( type[ 2 ] vec ) {
                this = Matrix.scaling( vec ) * this;
                return this;
            }

            /// scale an existing matrix with two scalars
            Matrix scale( type x, type y ) {
                this = Matrix.scaling( [ x, y ] ) * this;
                return this;
            }
        }

        else static if( cols >= 3 ) {
            /// static construction of a scaling matrix from an array/vector
            static Matrix scaling( type[ 3 ] vec ) {
                Matrix result = Matrix.identity;
                result.data[ 0 ][ 0 ] = vec[ 0 ];
                result.data[ 1 ][ 1 ] = vec[ 1 ];
                result.data[ 2 ][ 2 ] = vec[ 2 ];
                return result;
            }

            /// static construction of a scaling matrix from three scalars
            static Matrix scaling( type x, type y, type z ) {
                return scaling( [ x, y, z ] );
            }

            /// scale an existing matrix with an array/vector
            Matrix scale( type[ 3 ] vec ) {
                this = Matrix.scaling( vec ) * this;
                return this;
            }

            /// scale an existing matrix with three scalars
            Matrix scale( type x, type y, type z ) {
                this = Matrix.scaling( [ x, y, z ] ) * this;
                return this;
            }

            unittest {
                mat4 m4 = mat4( 1.0f );
                assert( m4.scaling( 0.0f, 1.0f, 2.0f ).data == mat4.scaling( 0.0f, 1.0f, 2.0f ).data );
                assert( mat4.scaling( 0.0f, 1.0f, 2.0f ).data  ==  [[ 0.0f, 0.0f, 0.0f, 0.0f ],
                                                                    [ 0.0f, 1.0f, 0.0f, 0.0f ],
                                                                    [ 0.0f, 0.0f, 2.0f, 0.0f ],
                                                                    [ 0.0f, 0.0f, 0.0f, 1.0f ] ] );
                assert( mat4.identity.scale( 0.0f, 1.0f, 2.0f ).data == mat4.scaling( 0.0f, 1.0f, 2.0f ).data );
            }
        }

        /// extract scaling part of the matrix in a copy
        Matrix scale() {
            Matrix result = Matrix.identity;
            foreach( rc; 0 .. cols - 1 )
                result[ rc ][ rc ] = data[ rc ][ rc ];
            return result;
        }

        unittest {
            mat3 m3 = mat3( 0.0f, 1.0f, 2.0f,
                            3.0f, 4.0f, 5.0f,
                            6.0f, 7.0f, 1.0f );

            assert( m3.scale.data  ==  [[ 0.0f, 0.0f, 0.0f ],
                                        [ 0.0f, 4.0f, 0.0f ],
                                        [ 0.0f, 0.0f, 1.0f ] ] );


            mat4 m4 = mat4(  0.0f,  1.0f,  2.0f,  3.0f,
                             4.0f,  5.0f,  6.0f,  7.0f,
                             8.0f,  9.0f, 10.0f, 11.0f,
                            12.0f, 13.0f, 14.0f,  1.0f );

            assert( m4.scale.data  ==  [[ 0.0f, 0.0f,  0.0f, 0.0f ],
                                        [ 0.0f, 5.0f,  0.0f, 0.0f ],
                                        [ 0.0f, 0.0f, 10.0f, 0.0f ],
                                        [ 0.0f, 0.0f,  0.0f, 1.0f ]] );
        }
    }   // end static if( cols == rows )



    ///////////////
    // Operators //
    ///////////////

    /// Component-wise binary matrix-scalar operation: addition, subtraction, multiplication, division
    auto opBinary( string op, T )( T s ) const if( isNumeric!T && (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))) {
        Matrix result;
        mixin( "result[0] = data[0] " ~ op ~ " s;" );
        mixin( "result[1] = data[1] " ~ op ~ " s;" );
        static if( cols >= 3 )  mixin( "result[2] = data[2] " ~ op ~ " s;" );
        static if( cols == 4 )  mixin( "result[3] = data[3] " ~ op ~ " s;" );
        return result;
    }

    /// Component-wise binary scalar-matrix operation: addition, subtraction, multiplication, division
    auto opBinaryRight( string op, T )( T s ) const if( isNumeric!T && (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))) {
        Matrix result;
        mixin( "result[0] = s " ~ op ~ "data[0];" );
        mixin( "result[1] = s " ~ op ~ "data[1];" );
        static if( cols >= 3 )  mixin( "result[2] = s " ~ op ~ "data[2];" );
        static if( cols == 4 )  mixin( "result[3] = s " ~ op ~ "data[3];" );
        return result;
    }

    unittest {      /// Matrix-Scalar Operations
        mat2 m2 = mat2( 1.0f, 2.0f, 3.0f, 4.0f );
        assert(( m2 * 2.0f ).data == [ [ 2.0f, 4.0f ], [ 6.0f, 8.0f ] ] );
        assert(( 2 * m2 ).data == ( m2 * 2 ).data );

        mat3 m3 = mat3( 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f );
        assert(( m3 * 2 ).data == [ [ 2.0f, 4.0f, 6.0f ], [ 8.0f, 10.0f, 12.0f ], [ 14.0f, 16.0f, 18.0f ] ] );
        assert(( 2 * m3 ).data == ( m3 * 2 ).data );

        //TODO: tests for mat4, mat3x4
    }


    /// matrix-vector multiplication
    auto opBinary( string op : "*", V )( V vec ) const if( isVector!V && V.dimension == cols ) {
        vectorType result;
        result.clear( 0 );
        foreach( r; 0 .. rows )
            foreach( c; 0 .. cols )
                result[ r ] += data[ c ][ r ] * vec[ c ];
        return result;
    }

    /// vector-Matrix multiplication, optimized instead of transposing matrix and multiplying
    auto opBinaryRight( string op : "*", V )( V vec ) const if( isVector!V && V.dimension == rows ) {
        Vector!( type, cols ) result;
        result.clear( 0 );
        foreach( c; 0 .. cols )
            foreach( r; 0 .. rows )
                result[ c ] += data[ c ][ r ] * vec[ r ];
        return result;
    }

    unittest {      /// matrix-vector, vector-matrix multiplication
        mat2 m2 = mat2( 2.0f, 4.0f, 6.0f, 8.0f );
        vec2 v2 = vec2( 2.0f, 2.0f );
        assert(( m2 * v2 ) == [ 16.0f, 24.0f ] );
        assert(( v2 * m2 ) == [ 12.0f, 28.0f ] );

        mat3 m3 = mat3( 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f );
        vec3 v3 = vec3( 2.0f, 2.0f, 2.0f );
        assert(( m3 * v3 ) == [ 48.0f, 60.0f, 72.0f ] );
        assert(( v3 * m3 ) == [ 24.0f, 60.0f, 96.0f ] );

        mat4 m4 = mat4( 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32 );
        vec4 v4 = vec4( 2, 2, 2, 2 );
        assert(( m4 * v4 ) == [ 112, 128, 144, 160 ] );
        assert(( v4 * m4 ) == [  40, 104, 168, 232 ] );

        //TODO: tests for mat4, mat3x4, mat4x3, mat2x4, mat 42
    }


    /// matrix-matrix component-wise operations addition, subtraction and division, using vector-vector operations of all colums
    Matrix opBinary( string op )( Matrix mat ) const if( op == "+" || op == "-" || op == "/" ) {
        Matrix result;
        mixin( "result[0] = data[0] " ~ op ~ "ma t[0];" );
        mixin( "result[1] = data[1] " ~ op ~ "ma t[1];" );
        static if( cols >= 3 )  mixin( "result[2] = data[2] " ~ op ~ "ma t[2];" );
        static if( cols == 4 )  mixin( "result[3] = data[3] " ~ op ~ "ma t[3];" );
        return result;
    }

    /// matrix-matrix multiplication, using matrix-vector multiplication for each column of mat
    Matrix opBinary( string op : "*", M )( M mat ) const if( isCompatibleMatrix!M && cols == M.rows ) {
        Matrix!( type, cols, M.cols ) result;
        result[0] = this * mat[0];
        result[1] = this * mat[1];
        static if( M.cols >= 3 )  result[2] = this * mat[2];
        static if( M.cols == 4 )  result[3] = this * mat[3];
        return result;
    }


    unittest {      /// Matrix-Matrix Operations
        mat2 m2 = mat2( 2.0f, 4.0f, 6.0f, 8.0f );
        assert(( m2 * m2 ) == [ [ 28.0f, 40.0f ], [ 60.0f, 88.0f ] ] );
        assert(( m2 - m2 ) == [ [ 0.0f, 0.0f ], [ 0.0f, 0.0f ] ] );
        mat2 m2_2 = m2 + m2;
        assert(  m2_2  == [ [ 4.0f, 8.0f ], [ 12.0f, 16.0f ] ] );
        assert(( m2_2 / m2 ) == [ vec2( 2.0f, 2.0f ), vec2( 2.0f, 2.0f ) ] );

        mat3 m3 = mat3( 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f );
        assert(( m3 * m3 ) == [ [ 120.0f, 144.0f, 168.0f ], [ 264.0f, 324.0f, 384.0f ], [ 408.0f, 504.0f, 600.0f ] ] );
        assert(( m3 - m3 ) == [ [ 0.0f, 0.0f, 0.0f ], [ 0.0f, 0.0f, 0.0f ], [ 0.0f, 0.0f, 0.0f ] ] );
        assert(( m3 + m3 ) == [ [ 4.0f, 8.0f, 12.0f ], [ 16.0f, 20.0f, 24.0f ], [ 28.0f, 32.0f, 36.0f ] ] );
        assert(( m3 / m3 ) == [ [ 1.0f, 1.0f, 1.0f ], [ 1.0f, 1.0f, 1.0f ], vec3( 1.0f, 1.0f, 1.0f ) ] );

        //TODO: tests for mat4, mat3x4
    }


    void opOpAssign( string op, T )( T val ) {
        mixin( "this = this " ~ op ~  "val;" );
    }

    unittest {      /// Matrix Unary Operations
        mat2  m2 = mat2( 2.0f, 4.0f, 6.0f, 8.0f );
        m2 += m2; assert( m2 == [ [ 4.0f, 8.0f ], [ 12.0f, 16.0f ] ] );
        m2 /= m2; assert( m2 == [ [ 1.0f, 1.0f ], [ 1.0f, 1.0f ] ] );
        m2 -= m2; assert( m2 == [ [ 0.0f, 0.0f ], [ 0.0f, 0.0f ] ] );

        mat3  m3 = mat3( 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f );
        m3 += m3; assert( m3 == [ [ 4.0f, 8.0f, 12.0f ], [ 16.0f, 20.0f, 24.0f ], [ 28.0f, 32.0f, 36.0f ] ] );
        m3 /= m3; assert( m3 == [ [ 1.0f, 1.0f, 1.0f ], [ 1.0f, 1.0f, 1.0f ], [ 1.0f, 1.0f, 1.0f ] ] );
        m3 -= m3; assert( m3 == [ [ 0.0f, 0.0f, 0.0f ], [ 0.0f, 0.0f, 0.0f ], [ 0.0f, 0.0f, 0.0f ] ] );

        //TODO: tests for mat4, mat3x4
    }


    bool opCast( T : bool )() const {
        return isvalid;
    }

    unittest {
        assert( mat2( 1.0f, 2.0f, 1.0f, 1.0f ) == mat2( 1.0f, 2.0f, 1.0f, 1.0f ));
        assert( mat2( 1.0f, 2.0f, 1.0f, 1.0f ) != mat2( 1.0f, 1.0f, 1.0f, 1.0f ));

        assert( mat3( 1.0f ) == mat3( 1.0f ));
        assert( mat3( 1.0f ) != mat3( 2.0f ));

        assert( mat4( 1.0f ) == mat4( 1.0f ));
        assert( mat4( 1.0f ) != mat4( 2.0f ));

        assert( !( mat4( float.nan )));
        if( mat4( 1.0f )) { }
        else { assert( false ); }
    }

}



/////////////////////////////////
// free functions akin to glsl //
/////////////////////////////////

pure nothrow @nogc @safe:

/////////////////
/// Transpose ///
/////////////////
auto transpose( M )( M mat ) if( isMatrix!M && M.cols == M.rows ) {
    M result;

    foreach( c; 0 .. M.cols ) {
        foreach( r; 0 .. M.rows ) {
            result[ r ][ c ] = mat[ c ][ r ];
        }
    }

    return result;
}

/// Unittest transpose
unittest {
    mat2 m2 = mat2( 1.0f );
    assert( m2.transpose.data == mat2( 1.0f ).data );

    assert( m2.transpose.data  ==  [[ 1.0f, 0.0f ],
                                    [ 0.0f, 1.0f ]] );
    assert( m2.data == mat2.identity.data );

    mat3 m3 = mat3( 1.1f, 1.2f, 1.3f,
                    2.1f, 2.2f, 2.3f,
                    3.1f, 3.2f, 3.3f );
    m3 = m3.transpose;
    assert( m3.data == [[ 1.1f, 2.1f, 3.1f ],
                        [ 1.2f, 2.2f, 3.2f ],
                        [ 1.3f, 2.3f, 3.3f ]] );

    mat4 m4 = mat4( 2.0f );
    assert( m4.transpose.data == mat4( 2.0f ).data );
}




/////////////////
// Determinant //
/////////////////
auto determinant( Matrix )( Matrix mat ) if( isMatrix!Matrix && Matrix.cols == Matrix.rows ) {
    static if( mat.cols == 2 ) {
        return ( mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] );
    }

    else static if( mat.cols == 3 ) {
        return (  mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ]
                + mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ]
                + mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ]
                - mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ]
                - mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ]
                - mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] );
    }

    else {
        return (  mat[ 0 ][ 3 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 0 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 0 ]
                - mat[ 0 ][ 3 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 0 ] + mat[ 0 ][ 1 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 0 ]
                + mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 0 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 0 ]
                - mat[ 0 ][ 3 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 1 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 1 ]
                + mat[ 0 ][ 3 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 1 ] - mat[ 0 ][ 0 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 1 ]
                - mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 1 ] + mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 1 ]
                + mat[ 0 ][ 3 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 2 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 2 ]
                - mat[ 0 ][ 3 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 2 ] + mat[ 0 ][ 0 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 2 ]
                + mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 2 ] - mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 2 ]
                - mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 3 ]
                + mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 3 ]
                - mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 3 ] );
    }
}



////////////
// Invert //
////////////
auto invert( Matrix )( Matrix mat ) if( isMatrix!Matrix && Matrix.cols == Matrix.rows ) {

    static if( isFloatingPoint!( Matrix.valueType ) && rmul ) {
        Matrix.valueType d = 1 / mat.determinant;
        enum op = "*";
    }   else {
        Matrix.valueType d = mat.determinant;
        enum op = "/";
    }

    Matrix result;
    alias vectorType = Matrix.vectorType;

    static if( Matrix.cols == 2 )
        mixin( "
            result = [
                vectorType(   mat[ 1 ][ 1 ], - mat[ 0 ][ 1 ] ) " ~ op ~ " d,
                vectorType( - mat[ 1 ][ 0 ],   mat[ 0 ][ 0 ] ) " ~ op ~ " d ];" );

    else static if( Matrix.cols == 3 )
        mixin( "
            result = [
                vectorType((  mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] - mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 2 ] * mat[ 2 ][ 1 ] - mat[ 0 ][ 1 ] * mat[ 2 ][ 2 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] ) " ~ op ~ " d ),
                vectorType((  mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] - mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 2 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 2 ][ 0 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] - mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] ) " ~ op ~ " d ),
                vectorType((  mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ] - mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 1 ] * mat[ 2 ][ 0 ] - mat[ 0 ][ 0 ] * mat[ 2 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] ) " ~ op ~ " d ) ];" );

    else
        mixin( "
            result = [
                vectorType((  mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 3 ] + mat[ 1 ][ 2 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 1 ] + mat[ 1 ][ 3 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 2 ]
                            - mat[ 1 ][ 1 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 2 ] - mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 3 ] - mat[ 1 ][ 3 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 1 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 2 ] + mat[ 0 ][ 2 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 3 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 1 ]
                            - mat[ 0 ][ 1 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 2 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 1 ] - mat[ 0 ][ 3 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 2 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 3 ] * mat[ 3 ][ 1 ] + mat[ 0 ][ 3 ] * mat[ 1 ][ 1 ] * mat[ 3 ][ 2 ]
                            - mat[ 0 ][ 1 ] * mat[ 1 ][ 3 ] * mat[ 3 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 3 ] * mat[ 1 ][ 2 ] * mat[ 3 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 1 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 2 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 3 ] + mat[ 0 ][ 3 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ]
                            - mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 3 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 1 ] - mat[ 0 ][ 3 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] ) " ~ op ~ " d ),
                vectorType((  mat[ 1 ][ 0 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 2 ] + mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 3 ] + mat[ 1 ][ 3 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 0 ]
                            - mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 3 ] - mat[ 1 ][ 2 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 0 ] - mat[ 1 ][ 3 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 2 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 2 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 0 ] + mat[ 0 ][ 3 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 2 ]
                            - mat[ 0 ][ 0 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 3 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 0 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 3 ] * mat[ 3 ][ 2 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 3 ] * mat[ 1 ][ 2 ] * mat[ 3 ][ 0 ]
                            - mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 3 ] * mat[ 3 ][ 0 ] - mat[ 0 ][ 3 ] * mat[ 1 ][ 0 ] * mat[ 3 ][ 2 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 3 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 0 ] + mat[ 0 ][ 3 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ]
                            - mat[ 0 ][ 0 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 3 ] - mat[ 0 ][ 3 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] ) " ~ op ~ " d ),
                vectorType((  mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 3 ] + mat[ 1 ][ 1 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 0 ] + mat[ 1 ][ 3 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 1 ]
                            - mat[ 1 ][ 0 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 1 ] - mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 3 ] - mat[ 1 ][ 3 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 0 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 1 ] + mat[ 0 ][ 1 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 3 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 0 ]
                            - mat[ 0 ][ 0 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 1 ] * mat[ 2 ][ 3 ] * mat[ 3 ][ 0 ] - mat[ 0 ][ 3 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 3 ][ 3 ] + mat[ 0 ][ 1 ] * mat[ 1 ][ 3 ] * mat[ 3 ][ 0 ] + mat[ 0 ][ 3 ] * mat[ 1 ][ 0 ] * mat[ 3 ][ 1 ]
                            - mat[ 0 ][ 0 ] * mat[ 1 ][ 3 ] * mat[ 3 ][ 1 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 3 ][ 3 ] - mat[ 0 ][ 3 ] * mat[ 1 ][ 1 ] * mat[ 3 ][ 0 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 1 ] + mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 3 ] + mat[ 0 ][ 3 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ]
                            - mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 3 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 3 ] * mat[ 2 ][ 0 ] - mat[ 0 ][ 3 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ] ) " ~ op ~ " d ),
                vectorType((  mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 1 ] + mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 2 ] + mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 0 ]
                            - mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 2 ] - mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 0 ] - mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 2 ] + mat[ 0 ][ 1 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 0 ] + mat[ 0 ][ 2 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 1 ]
                            - mat[ 0 ][ 0 ] * mat[ 2 ][ 2 ] * mat[ 3 ][ 1 ] - mat[ 0 ][ 1 ] * mat[ 2 ][ 0 ] * mat[ 3 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 2 ][ 1 ] * mat[ 3 ][ 0 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 3 ][ 1 ] + mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 3 ][ 2 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 3 ][ 0 ]
                            - mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 3 ][ 2 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 3 ][ 0 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 3 ][ 1 ] ) " ~ op ~ " d,
                            ( mat[ 0 ][ 0 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 2 ] + mat[ 0 ][ 1 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 0 ] + mat[ 0 ][ 2 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 1 ]
                            - mat[ 0 ][ 0 ] * mat[ 1 ][ 2 ] * mat[ 2 ][ 1 ] - mat[ 0 ][ 1 ] * mat[ 1 ][ 0 ] * mat[ 2 ][ 2 ] - mat[ 0 ][ 2 ] * mat[ 1 ][ 1 ] * mat[ 2 ][ 0 ] ) " ~ op ~ " d ) ];" );

    return result;
}


///////////////////////////////////////////////
// Invert purely Translate-Rotate 4*4 matrix //
///////////////////////////////////////////////
auto invertTR( M )( M mat ) if( isMatrix!M && M.cols == 4 && M.rows == 4 ) {
    M result = mat.rotation.transpose;
    result[3] = - result.opBinary!( "*", vec4 )( mat[3] );  // why does result * mat3[3] not work ???
    return result;
}


unittest {
    mat2 m2 = mat2( 1.0f, 2.0f, vec2( 3.0f, 4.0f ));
    assert( m2.determinant == - 2.0f );
    assert( m2.invert.data ==  [[ - 2.0f,   1.0f ],
                                [   1.5f, - 0.5f ]  );

    mat3 m3 = mat3( 1.0f, - 2.0f,   3.0f,
                    7.0f, - 1.0f,   0.0f,
                    3.0f,   2.0f, - 4.0f );
    assert( m3.determinant == - 1.0f );
    assert( m3.invert.data ==  [[ -  4.0f,  2.0f, -  3.0f ],
                                [ - 28.0f, 13.0f, - 21.0f ],
                                [ - 17.0f,  8.0f, - 13.0f ]] );

    mat4 m4 = mat4(   1.0f,   2.0f,   3.0f,   4.0f,
                    - 2.0f,   1.0f,   5.0f, - 2.0f,
                      2.0f, - 1.0f,   7.0f,   1.0f,
                      3.0f, - 3.0f,   2.0f,   0.0f );
    assert( m4.determinant == - 8.0f );
    assert( m4.invert.data ==  [[   6.875f,   7.875f, - 11.75f,  11.125f ],
                                [   6.625f,   7.625f, - 11.25f,  10.375f ],
                                [ - 0.375f, - 0.375f,    0.75f, - 0.625f ],
                                [ - 4.5f,   - 5.5f,      8.0f,  - 7.5f ]] );
}


/// query if any entry is nan
bool isnan( M )( const ref M mat ) if( isMatrix!M && isFloatingPoint!( M.valueType )) {
    foreach( const ref vec; mat )
        if( vec.isnan )
            return true;
    return false;
}


/// query if any entry is inf
bool isinf( M )( const ref M mat ) if( isMatrix!M && isFloatingPoint!( M.valueType )) {
    foreach( const ref vec; mat )
        if( vec.isinf )
            return true;
    return false;
}


/// query if all entries are not nan and not inf
bool isvalid( M )( const ref M mat ) if( isMatrix!M && isFloatingPoint!( M.valueType )) {
    return !( mat.isinf || mat.isnan );
}


unittest {      /// Matrix.isvalid

    mat2 m2 = mat2( 1.0f, 1.0f, vec2( 2.0f, 2.0f ));
    assert( m2.data == [[ 1.0f, 1.0f ], [ 2.0f, 2.0f ]] );
    m2.clear( 3.0f );
    assert( m2.data == [[ 3.0f, 3.0f ], [ 3.0f, 3.0f ]] );
    assert( m2.isvalid );
    m2.clear( float.nan );
    assert( !m2.isvalid );
    m2.clear( float.infinity );
    assert( !m2.isvalid );
    m2.clear( 0.0f );
    assert( m2.isvalid );

    mat3 m3 = mat3( 1.0f );
    assert( m3.data == [[ 1.0f, 0.0f, 0.0f ],
                        [ 0.0f, 1.0f, 0.0f ],
                        [ 0.0f, 0.0f, 1.0f ]] );

    mat4 m4 = mat4( vec4(   1.0f, 1.0f, 1.0f, 1.0f ),
                            2.0f, 2.0f, 2.0f, 2.0f,
                            3.0f, 3.0f, 3.0f, 3.0f,
                         vec4( 4.0f, 4.0f, 4.0f, 4.0f ));
    assert( m4.data == [[ 1.0f, 1.0f, 1.0f, 1.0f ],
                        [ 2.0f, 2.0f, 2.0f, 2.0f ],
                        [ 3.0f, 3.0f, 3.0f, 3.0f ],
                        [ 4.0f, 4.0f, 4.0f, 4.0f ]] );
    assert( mat3( m4 ).data == [[ 1.0f, 1.0f, 1.0f ],
                                [ 2.0f, 2.0f, 2.0f ],
                                [ 3.0f, 3.0f, 3.0f ]] );
    assert( mat2( mat3( m4 )).data == [[ 1.0f, 1.0f ],
                                                    [ 2.0f, 2.0f ] ] );
    assert( mat2( m4 ).data == mat2( mat3( m4 )).data );
    assert( mat4( mat3( m4 )).data ==  [[ 1.0f, 1.0f, 1.0f, 0.0f ],
                                        [ 2.0f, 2.0f, 2.0f, 0.0f ],
                                        [ 3.0f, 3.0f, 3.0f, 0.0f ],
                                        [ 0.0f, 0.0f, 0.0f, 1.0f ]] );

    Matrix!( float, 2, 3 ) mt1 = Matrix!( float, 2, 3 )( 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f );
    Matrix!( float, 3, 2 ) mt2 = Matrix!( float, 3, 2 )( 6.0f, - 1.0f, 3.0f, 2.0f, 0.0f, - 3.0f );

    assert( mt1.data == [[ 1.0f, 2.0f, 3.0f ], [ 4.0f, 5.0f, 6.0f ]] );
    assert( mt2.data == [[ 6.0f, - 1.0f ], [ 3.0f, 2.0f ], [ 0.0f, - 3.0f ]] );
}




