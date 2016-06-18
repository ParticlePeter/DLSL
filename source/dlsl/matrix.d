/**
math.matrix


Authors: Peter Particle ( based on gl3n by David Herberth )
License: MIT

Note: All methods marked with pure are weakly pure since, they all access an instance member.
All static methods are strongly pure.
*/

module dlsl.matrix;

import dlsl.vector;

import std.conv	: to;
import std.math	: isNaN, isInfinity, sqrt, sin, cos, tan, PI;
import std.array	: join;
import std.traits : isNumeric, isFloatingPoint, isArray;
import std.string : format, rightJustify;
import std.algorithm : max, min, reduce;


enum float	deg2rad	= 0.0174532925199432957692369076849f;
enum float	rad2deg	= 57.295779513082320876798154814105f;

version(NoReciprocalMul) {
	private enum rmul = false;
}	else {
	private enum rmul = true;
}

/// If T is a matrix, this evaluates to true, otherwise false
template isMatrix( T )  {  enum isMatrix = is( typeof( isMatrixImpl( T.init )));  }
private void isMatrixImpl( T, int cols, int rows )( Matrix!( T, cols, rows ) mat )  {}


/// Base template for all matrix - types.
/// Params:
/// type = the value type of each matrix element
/// numCols = count of columns of the matrix
/// numRows = count of rows of the matrix
/// Examples:
/// - - - 
/// alias Matrix!( float, 4, 4 ) mat4;
/// alias Matrix!( double, 3, 4 ) mat34d;
/// alias Matrix!( real, 2, 2 ) mat2r;
/// - - - 
struct Matrix( type, int numCols, int numRows ) if (( numCols > 1 ) && ( numRows > 1 ))  {
	alias type valueType;								/// Holds the internal primitive type of the matrix;
	static const int cols = numCols;				/// Holds the number of cols;
	static const int rows = numRows;				/// Holds the number of columns;
	alias Vector!( type, rows ) vectorType;		/// Holds the internal vectorType of the matrix;

	/// Holds the matrix $( RED column - major ) in memory.
	vectorType[ cols ] data;							/// Each Column is Vector of length rows
	alias data this;


	unittest  {		// Construction through aliased this static array of Vectors

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
		assert( m3 == [ [ 0.0f, 0.0f, 0.0f ],
							 [ 1.0f, 1.1f, 1.2f ], 
							 [ 2.0f, 2.1f, 2.2f ] ] );
		//m3[ 1 .. 3 ]  = [ [ 1, 1, 1 ], [ 2, 2, 2 ] ];
		m3[ 1 .. 3 ] =  [ vec3( 1, 1, 1 ), vec3( 2, 2, 2 ) ];
		assert( m3 == [ [ 0.0f, 0.0f, 0.0f ],
							 [ 1.0f, 1.0f, 1.0f ], 
							 [ 2.0f, 2.0f, 2.0f ] ] );

		mat4 m4 = mat4( 0.0f, 0.1f, 0.2f, 0.3f,
							 1.0f, 1.1f, 1.2f, 1.3f,
							 2.0f, 2.1f, 2.2f, 2.3f,
							 3.0f, 3.1f, 3.2f, 3.3f );
		assert( m4[ 0 ][ 3 ] == 0.3f );
		assert( m4[ 1 ][ 1 ] == 1.1f );
		assert( m4[ 2 ][ 0 ] == 2.0f );
		assert( m4[ 3 ][ 2 ] == 3.2f );

		m4[ 2 ][ 1 .. 3 ] = [ 1.0f, 2.0f ];
		assert( m4 == [ [ 0.0f, 0.1f, 0.2f, 0.3f ],
							 [ 1.0f, 1.1f, 1.2f, 1.3f ],
							 [ 2.0f, 1.0f, 2.0f, 2.3f ],
							 [ 3.0f, 3.1f, 3.2f, 3.3f ] ] );

	}


	/// Returns the pointer to the stored values as OpenGL requires it.
	/// Note this will return a pointer to a $( RED column - major ) matrix, 
	/// $( RED this is the OpneGL convention and expected in programs via Uniforms or UBOs ).
	@property auto ptr()  { return data[ 0 ].ptr; }

	/// Returns the current matrix formatted as flat string.
	@property string asString()  {  return format( "%s", data );  }
	alias asString toString;

	/// Returns the current matrix as pretty formatted string.
	/// TODO : Check This
	@property string asPrettyString()  {
		string fmtr = "%s";

		size_t rjust = max( format( fmtr, reduce!( max )( data[] )).length,
								  format( fmtr, reduce!( min )( data[] )).length ) - 1;

		string[] outer_parts;
		foreach( valueType[] col; data )  {
			string[] inner_parts;
			foreach( valueType row; col )  {
				inner_parts ~= rightJustify( format( fmtr, row ), rjust );
			}
			outer_parts ~= " [ " ~ join( inner_parts, ", " ) ~ " ]";
		}

		return "[ " ~ join( outer_parts, "\n" )[ 1 .. $ ] ~ " ]";
	}
	alias asPrettyString toPrettyString;

	@safe pure nothrow :
	template isCompatibleMatrix( T )  {  enum isCompatibleMatrix = is( typeof( isCompatibleMatrixImpl( T.init )));  }
	static void isCompatibleMatrixImpl( int col, int row )( Matrix!( valueType, col, row ) mat )  {}

	template isCompatibleVector( T )  {  enum isCompatibleVector = is( typeof( isCompatibleVectorImpl( T.init )));  }
	static void isCompatibleVectorImpl( int dim )( Vector!( valueType, dim ) vec )  {}

	
	/// TODO: Fix Construction with combination of numeric, static and dynamic array, vector and matrix
	private void construct( int i, T, Tail... )( T head, Tail tail )  {
		static if ( i >= cols * rows )  {
			static assert( false, "constructor has too many arguments" );
		}	else static if ( is( T : valueType ))  {
			data[ i / rows ][ i % rows ] = head;
			construct!( i + 1 )( tail );
		}	else static if ( is( T == Vector!( valueType, rows )))  {
			static if ( i % rows == 0 )  {
				data[ i / rows ] = head;
				construct!( i + T.dimension )( tail );
			}	else  {
				static assert( false, "Can't convert Vector into the matrix. Maybe it doesn't align to the columns correctly or dimension doesn't fit" );
			}
		}	/*else  {
			static assert( false, "Matrix constructor argument must be of type " ~ valueType.stringof ~ " or Vector, not " ~ T.stringof );
		}*/
	}

	private void construct( int i )()  {}	// terminate


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
	this( Args... )( Args args )  {  construct!( 0 )( args );  }

	/// Construct a Matrix from another Matrix, equal sized or bigger
	this( T )( T mat ) if ( isMatrix!T && ( T.rows >= rows ) && ( T.cols >= cols ))  {
		for( int c = 0; c < cols; ++c )  {
			for( int r = 0; r < rows; ++r )  {
				data[ c ][ r ] = mat.data[ c ][ r ];
			}
		}
	}


	/// Construct a Matrix from another Matrix, smaler sized
	this( T )( T mat ) if ( isMatrix!T && ( T.rows < rows ) && ( T.cols < cols ))  {
		makeIdentity();
		for( int r = 0; r < T.cols; ++r )  {
			for( int c = 0; c < T.rows; ++c )  {
				data[ r ][ c ] = mat.data[ r ][ c ];
			}
		}
	}

	/// Construct a Matrix from a single value
	this()( valueType value )  {
		makeDiagonal(  value );
	}

	/// Returns true if all values are not nan and finite, otherwise false.
	@property bool ok() const  {
		foreach( col; data )  {
			foreach( row; col )  {
				if ( isNaN( row ) || isInfinity( row ))  {
					return false;
				}
			}
		}
		return true;
	}

	/// Sets all values of the matrix to value ( each column in each col will contain this value ).
	void clear( valueType value )  {
		for( int r = 0; r < cols; ++r )  {
			for( int c = 0; c < rows; ++c )  {
				data[ r ][ c ] = value;
			}
		}
	}

	
	unittest  {		/// Matrix.ok, Constructing

		mat2 m2 = mat2( 1.0f, 1.0f, vec2( 2.0f, 2.0f ));
		assert( m2.data == [ [ 1.0f, 1.0f ], [ 2.0f, 2.0f ] ] );
		m2.clear( 3.0f );
		assert( m2.data == [ [ 3.0f, 3.0f ], [ 3.0f, 3.0f ] ] );
		assert( m2.ok );
		m2.clear( float.nan );
		assert( !m2.ok );
		m2.clear( float.infinity );
		assert( !m2.ok );
		m2.clear( 0.0f );
		assert( m2.ok );

		mat3 m3 = mat3( 1.0f );
		assert( m3.data == [ [ 1.0f, 0.0f, 0.0f ],
									[ 0.0f, 1.0f, 0.0f ],
									[ 0.0f, 0.0f, 1.0f ] ] );

		mat4 m4 = mat4( vec4( 1.0f, 1.0f, 1.0f, 1.0f ),
									 2.0f, 2.0f, 2.0f, 2.0f,
									 3.0f, 3.0f, 3.0f, 3.0f,
							 vec4( 4.0f, 4.0f, 4.0f, 4.0f ));
		assert( m4.data == [ [ 1.0f, 1.0f, 1.0f, 1.0f ],
									[ 2.0f, 2.0f, 2.0f, 2.0f ],
									[ 3.0f, 3.0f, 3.0f, 3.0f ],
									[ 4.0f, 4.0f, 4.0f, 4.0f ] ] );
		assert( mat3( m4 ).data == [ [ 1.0f, 1.0f, 1.0f ],
											  [ 2.0f, 2.0f, 2.0f ],
											  [ 3.0f, 3.0f, 3.0f ] ] );
		assert( mat2( mat3( m4 )).data == [ [ 1.0f, 1.0f ],
														[ 2.0f, 2.0f ] ] );
		assert( mat2( m4 ).data == mat2( mat3( m4 )).data );
		assert( mat4( mat3( m4 )).data == [ [ 1.0f, 1.0f, 1.0f, 0.0f ],
														[ 2.0f, 2.0f, 2.0f, 0.0f ],
														[ 3.0f, 3.0f, 3.0f, 0.0f ],
														[ 0.0f, 0.0f, 0.0f, 1.0f ] ] );

		Matrix!( float, 2, 3 ) mt1 = Matrix!( float, 2, 3 )( 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f );
		Matrix!( float, 3, 2 ) mt2 = Matrix!( float, 3, 2 )( 6.0f, - 1.0f, 3.0f, 2.0f, 0.0f, - 3.0f );

		assert( mt1.data == [ [ 1.0f, 2.0f, 3.0f ], [ 4.0f, 5.0f, 6.0f ] ] );
		assert( mt2.data == [ [ 6.0f, - 1.0f ], [ 3.0f, 2.0f ], [ 0.0f, - 3.0f ] ] );
	}


	//void opAssign( T )( T array )  if( is( T array == U[][], U ))  { //if ( isArray( A ))  {
	//	pragma( msg, "Instantiat opAssign" );
	//	data[] = array[];
	//}

	static if ( cols == rows )  {
		/// Makes the current matrix an identity matrix
		void makeIdentity()  {  makeDiagonal( 1 );  }

		/// Makes the current matrix an scaled identity matrix
		void makeDiagonal( valueType val )  {
			clear( 0 );
			for( int c = 0; c < cols; ++c )  {
				data[ c ][ c ] = val;
			}
		}

		/// Returns a identity matrix.
		static @property Matrix identity()  {
			Matrix result;
			result.makeIdentity;
			return result;
		}

		/// Transposes the current matrix
		/// TODO : as free function and use here
		void transpose()  {  data = transposed().data;  }

		/// Returns a transposed copy of the matrix.
		/// TODO : Remove this, create a function which returns a transposed matrix like
		///	Wrong logic anyway, as transpose( mat3x2 ) = mat2x3
		@property Matrix transposed() const  {
			Matrix result;

			for( int c = 0; c < cols; ++c )  {
				for( int r = 0; r < rows; ++r )  {
					result.data[ r ][ c ] = data[ c ][ r ];
				}
			}

			return result;
		}

		/// Unittest makeIdentity, transpose 
		unittest  {
			mat2 m2 = mat2( 1.0f );
			m2.transpose();
			assert( m2.data == mat2( 1.0f ).data );

			m2.makeIdentity();
			assert( m2.data == [ [ 1.0f, 0.0f ],
										[ 0.0f, 1.0f ] ] );

			m2.transpose();
			assert( m2.data == [ [ 1.0f, 0.0f ],
										[ 0.0f, 1.0f ] ] );
			assert( m2.data == m2.identity.data );

			mat3 m3 = mat3( 1.1f, 1.2f, 1.3f,
								 2.1f, 2.2f, 2.3f,
								 3.1f, 3.2f, 3.3f );
			m3.transpose();
			assert( m3.data == [ [ 1.1f, 2.1f, 3.1f ],
										[ 1.2f, 2.2f, 3.2f ],
										[ 1.3f, 2.3f, 3.3f ] ] );

			mat4 m4 = mat4( 2.0f );
			m4.transpose();
			assert( m4.data == mat4( 2.0f ).data );

			m4.makeIdentity();
			assert( m4.data == [ [ 1.0f, 0.0f, 0.0f, 0.0f ],
										[ 0.0f, 1.0f, 0.0f, 0.0f ],
										[ 0.0f, 0.0f, 1.0f, 0.0f ],
										[ 0.0f, 0.0f, 0.0f, 1.0f ] ] );
			assert( m4.data == m4.identity.data );
		}
	}

	/// mat2
	static if (( cols == 2 ) && ( rows == 2 ))  {
		alias det = determinant;
		@property valueType determinant() const  {
			return ( data[ 0 ][ 0 ] * data[ 1 ][ 1 ] - data[ 0 ][ 1 ] * data[ 1 ][ 0 ] );
		}

		/// invert
		/// TODO : is rmul really required ?
		private Matrix invert( ref Matrix mat ) const  {
			static if ( isFloatingPoint!valueType && rmul )  {
				valueType d = 1 / determinant;
				mat.data = [ vectorType(   data[ 1 ][ 1 ] * d, - data[ 0 ][ 1 ] * d ),
								 vectorType( - data[ 1 ][ 0 ] * d,   data[ 0 ][ 0 ] * d ) ];
			}	else  {
				valueType d = determinant;
				mat.data = [ vectorType(   data[ 1 ][ 1 ] * d, - data[ 0 ][ 1 ] * d ),
								 vectorType( - data[ 1 ][ 0 ] * d,   data[ 0 ][ 0 ] * d ) ];
			}
			return mat;
		}

		/// translation, static construction of a translation matrix
		static Matrix translation( valueType x )  {
			Matrix result = Matrix.identity;
			result.data[ 1 ][ 0 ] = x;
			return result;            
		}

		/// translate an existing matrix
		Matrix translate( valueType x )  {
			this = Matrix.translation( x ) * this;
			return this;
		}

		/// scaling, static construction of a scaling matrix
		static Matrix scaling( valueType x )  {
			Matrix result = Matrix.identity;
			result.data[ 0 ][ 0 ] = x;
			return result;
		}

		/// scale an existing matrix 
		Matrix scale( valueType x )  {
			this = Matrix.scaling( x ) * this;
			return this;
		}

		/// Returns an identity matrix with an applied 2D rotation.
		static Matrix rotation( real angle )  {
			Matrix result = Matrix.identity;
			valueType cosAngle = to!valueType( cos( angle ));
			valueType sinAngle = to!valueType( sin( angle ));
			result.data[ 0 ][ 0 ] = cosAngle;
			result.data[ 1 ][ 0 ] = - sinAngle;
			result.data[ 0 ][ 1 ] = sinAngle;
			result.data[ 1 ][ 1 ] = cosAngle;
			return result;
		}

		/// Rotate the current matrix arround an arbitrary axis
		Matrix rotate( real angle )  {
			this = rotation( angle ) * this;
			return this;
		}
	}

	/// mat3
	else static if (( cols == 3 ) && ( rows == 3 ))  {
		alias det = determinant;
		@property valueType determinant() const  {
			return ( data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 2 ][ 2 ]
					 + data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 2 ][ 0 ]
					 + data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 2 ][ 1 ]
					 - data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 2 ][ 0 ]
					 - data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 2 ][ 2 ]
					 - data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 2 ][ 1 ] );
		}

		/// invert
		private Matrix invert( ref Matrix mat ) const  {
			static if ( isFloatingPoint!valueType && rmul )  {
				valueType d = 1 / determinant;
				enum op = "*";
			}	else  {
				valueType d = determinant;
				enum op = "/";
			}

			mixin( "
				mat.data = [
					vectorType( ( data[ 1 ][ 1 ] * data[ 2 ][ 2 ] - data[ 1 ][ 2 ] * data[ 2 ][ 1 ] )" ~ op ~ "d,
									( data[ 0 ][ 2 ] * data[ 2 ][ 1 ] - data[ 0 ][ 1 ] * data[ 2 ][ 2 ] )" ~ op ~ "d,
									( data[ 0 ][ 1 ] * data[ 1 ][ 2 ] - data[ 0 ][ 2 ] * data[ 1 ][ 1 ] )" ~ op ~ "d ),
					vectorType( ( data[ 1 ][ 2 ] * data[ 2 ][ 0 ] - data[ 1 ][ 0 ] * data[ 2 ][ 2 ] )" ~ op ~ "d,
									( data[ 0 ][ 0 ] * data[ 2 ][ 2 ] - data[ 0 ][ 2 ] * data[ 2 ][ 0 ] )" ~ op ~ "d,
									( data[ 0 ][ 2 ] * data[ 1 ][ 0 ] - data[ 0 ][ 0 ] * data[ 1 ][ 2 ] )" ~ op ~ "d ),
					vectorType( ( data[ 1 ][ 0 ] * data[ 2 ][ 1 ] - data[ 1 ][ 1 ] * data[ 2 ][ 0 ] )" ~ op ~ "d,
									( data[ 0 ][ 1 ] * data[ 2 ][ 0 ] - data[ 0 ][ 0 ] * data[ 2 ][ 1 ] )" ~ op ~ "d,
									( data[ 0 ][ 0 ] * data[ 1 ][ 1 ] - data[ 0 ][ 1 ] * data[ 1 ][ 0 ] )" ~ op ~ "d ) ];
				" );

			return mat;
		}

		/// translation, static construction of a translation matrix
		// TODO : make sure that translation always returns a floating point matrix
		static Matrix translation( valueType x, valueType y )  {
			Matrix result = Matrix.identity;
			result.data[ 2 ][ 0 ] = x;
			result.data[ 2 ][ 1 ] = y;
			return result;            
		}

		/// translate an existing matrix
		Matrix translate( valueType x, valueType y )  {
			this = Matrix.translation( x, y ) * this;
			return this;
		}

		/// scaling, static construction of a scaling matrix
		static Matrix scaling( valueType x, valueType y )  {
			Matrix result = Matrix.identity;
			result.data[ 0 ][ 0 ] = x;
			result.data[ 1 ][ 1 ] = y;
			return result;
		}

		/// scale an existing matrix 
		Matrix scale( valueType x, valueType y )  {
			this = Matrix.scaling( x, y ) * this;
			return this;
		}
	}
	
	/// mat4
	else static if (( cols == 4 ) && ( rows == 4 ))  {
		/// Returns the determinant of the current data ( 2x2, 3x3 and 4x4 matrices ).
		alias det = determinant;
		@property valueType determinant() const  {
			return ( data[ 0 ][ 3 ] * data[ 1 ][ 2 ] * data[ 2 ][ 1 ] * data[ 3 ][ 0 ] - data[ 0 ][ 2 ] * data[ 1 ][ 3 ] * data[ 2 ][ 1 ] * data[ 3 ][ 0 ]
					 - data[ 0 ][ 3 ] * data[ 1 ][ 1 ] * data[ 2 ][ 2 ] * data[ 3 ][ 0 ] + data[ 0 ][ 1 ] * data[ 1 ][ 3 ] * data[ 2 ][ 2 ] * data[ 3 ][ 0 ]
					 + data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 2 ][ 3 ] * data[ 3 ][ 0 ] - data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 2 ][ 3 ] * data[ 3 ][ 0 ]
					 - data[ 0 ][ 3 ] * data[ 1 ][ 2 ] * data[ 2 ][ 0 ] * data[ 3 ][ 1 ] + data[ 0 ][ 2 ] * data[ 1 ][ 3 ] * data[ 2 ][ 0 ] * data[ 3 ][ 1 ]
					 + data[ 0 ][ 3 ] * data[ 1 ][ 0 ] * data[ 2 ][ 2 ] * data[ 3 ][ 1 ] - data[ 0 ][ 0 ] * data[ 1 ][ 3 ] * data[ 2 ][ 2 ] * data[ 3 ][ 1 ]
					 - data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 2 ][ 3 ] * data[ 3 ][ 1 ] + data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 2 ][ 3 ] * data[ 3 ][ 1 ]
					 + data[ 0 ][ 3 ] * data[ 1 ][ 1 ] * data[ 2 ][ 0 ] * data[ 3 ][ 2 ] - data[ 0 ][ 1 ] * data[ 1 ][ 3 ] * data[ 2 ][ 0 ] * data[ 3 ][ 2 ]
					 - data[ 0 ][ 3 ] * data[ 1 ][ 0 ] * data[ 2 ][ 1 ] * data[ 3 ][ 2 ] + data[ 0 ][ 0 ] * data[ 1 ][ 3 ] * data[ 2 ][ 1 ] * data[ 3 ][ 2 ]
					 + data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 2 ][ 3 ] * data[ 3 ][ 2 ] - data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 2 ][ 3 ] * data[ 3 ][ 2 ]
					 - data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 2 ][ 0 ] * data[ 3 ][ 3 ] + data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 2 ][ 0 ] * data[ 3 ][ 3 ]
					 + data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 2 ][ 1 ] * data[ 3 ][ 3 ] - data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 2 ][ 1 ] * data[ 3 ][ 3 ]
					 - data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 2 ][ 2 ] * data[ 3 ][ 3 ] + data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 2 ][ 2 ] * data[ 3 ][ 3 ] );
		}

		private Matrix invert( ref Matrix mat ) const  {
			static if ( isFloatingPoint!valueType && rmul )  {
				valueType d = 1 / determinant;
				enum op = "*";
			}	else  {
				valueType d = determinant;
				enum op = "/";
			}

			mixin( "
					mat.data = [
						vectorType( ( data[ 1 ][ 1 ] * data[ 2 ][ 2 ] * data[ 3 ][ 3 ] + data[ 1 ][ 2 ] * data[ 2 ][ 3 ] * data[ 3 ][ 1 ] + data[ 1 ][ 3 ] * data[ 2 ][ 1 ] * data[ 3 ][ 2 ]
										- data[ 1 ][ 1 ] * data[ 2 ][ 3 ] * data[ 3 ][ 2 ] - data[ 1 ][ 2 ] * data[ 2 ][ 1 ] * data[ 3 ][ 3 ] - data[ 1 ][ 3 ] * data[ 2 ][ 2 ] * data[ 3 ][ 1 ] )" ~ op ~ "d,
										( data[ 0 ][ 1 ] * data[ 2 ][ 3 ] * data[ 3 ][ 2 ] + data[ 0 ][ 2 ] * data[ 2 ][ 1 ] * data[ 3 ][ 3 ] + data[ 0 ][ 3 ] * data[ 2 ][ 2 ] * data[ 3 ][ 1 ]
										- data[ 0 ][ 1 ] * data[ 2 ][ 2 ] * data[ 3 ][ 3 ] - data[ 0 ][ 2 ] * data[ 2 ][ 3 ] * data[ 3 ][ 1 ] - data[ 0 ][ 3 ] * data[ 2 ][ 1 ] * data[ 3 ][ 2 ] )" ~ op ~ "d,
										( data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 3 ][ 3 ] + data[ 0 ][ 2 ] * data[ 1 ][ 3 ] * data[ 3 ][ 1 ] + data[ 0 ][ 3 ] * data[ 1 ][ 1 ] * data[ 3 ][ 2 ]
										- data[ 0 ][ 1 ] * data[ 1 ][ 3 ] * data[ 3 ][ 2 ] - data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 3 ][ 3 ] - data[ 0 ][ 3 ] * data[ 1 ][ 2 ] * data[ 3 ][ 1 ] )" ~ op ~ "d,
										( data[ 0 ][ 1 ] * data[ 1 ][ 3 ] * data[ 2 ][ 2 ] + data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 2 ][ 3 ] + data[ 0 ][ 3 ] * data[ 1 ][ 2 ] * data[ 2 ][ 1 ]
										- data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 2 ][ 3 ] - data[ 0 ][ 2 ] * data[ 1 ][ 3 ] * data[ 2 ][ 1 ] - data[ 0 ][ 3 ] * data[ 1 ][ 1 ] * data[ 2 ][ 2 ] )" ~ op ~ "d ),
						vectorType( ( data[ 1 ][ 0 ] * data[ 2 ][ 3 ] * data[ 3 ][ 2 ] + data[ 1 ][ 2 ] * data[ 2 ][ 0 ] * data[ 3 ][ 3 ] + data[ 1 ][ 3 ] * data[ 2 ][ 2 ] * data[ 3 ][ 0 ]
										- data[ 1 ][ 0 ] * data[ 2 ][ 2 ] * data[ 3 ][ 3 ] - data[ 1 ][ 2 ] * data[ 2 ][ 3 ] * data[ 3 ][ 0 ] - data[ 1 ][ 3 ] * data[ 2 ][ 0 ] * data[ 3 ][ 2 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 2 ][ 2 ] * data[ 3 ][ 3 ] + data[ 0 ][ 2 ] * data[ 2 ][ 3 ] * data[ 3 ][ 0 ] + data[ 0 ][ 3 ] * data[ 2 ][ 0 ] * data[ 3 ][ 2 ]
										- data[ 0 ][ 0 ] * data[ 2 ][ 3 ] * data[ 3 ][ 2 ] - data[ 0 ][ 2 ] * data[ 2 ][ 0 ] * data[ 3 ][ 3 ] - data[ 0 ][ 3 ] * data[ 2 ][ 2 ] * data[ 3 ][ 0 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 1 ][ 3 ] * data[ 3 ][ 2 ] + data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 3 ][ 3 ] + data[ 0 ][ 3 ] * data[ 1 ][ 2 ] * data[ 3 ][ 0 ]
										- data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 3 ][ 3 ] - data[ 0 ][ 2 ] * data[ 1 ][ 3 ] * data[ 3 ][ 0 ] - data[ 0 ][ 3 ] * data[ 1 ][ 0 ] * data[ 3 ][ 2 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 2 ][ 3 ] + data[ 0 ][ 2 ] * data[ 1 ][ 3 ] * data[ 2 ][ 0 ] + data[ 0 ][ 3 ] * data[ 1 ][ 0 ] * data[ 2 ][ 2 ]
										- data[ 0 ][ 0 ] * data[ 1 ][ 3 ] * data[ 2 ][ 2 ] - data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 2 ][ 3 ] - data[ 0 ][ 3 ] * data[ 1 ][ 2 ] * data[ 2 ][ 0 ] )" ~ op ~ "d ),
						vectorType( ( data[ 1 ][ 0 ] * data[ 2 ][ 1 ] * data[ 3 ][ 3 ] + data[ 1 ][ 1 ] * data[ 2 ][ 3 ] * data[ 3 ][ 0 ] + data[ 1 ][ 3 ] * data[ 2 ][ 0 ] * data[ 3 ][ 1 ]
										- data[ 1 ][ 0 ] * data[ 2 ][ 3 ] * data[ 3 ][ 1 ] - data[ 1 ][ 1 ] * data[ 2 ][ 0 ] * data[ 3 ][ 3 ] - data[ 1 ][ 3 ] * data[ 2 ][ 1 ] * data[ 3 ][ 0 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 2 ][ 3 ] * data[ 3 ][ 1 ] + data[ 0 ][ 1 ] * data[ 2 ][ 0 ] * data[ 3 ][ 3 ] + data[ 0 ][ 3 ] * data[ 2 ][ 1 ] * data[ 3 ][ 0 ]
										- data[ 0 ][ 0 ] * data[ 2 ][ 1 ] * data[ 3 ][ 3 ] - data[ 0 ][ 1 ] * data[ 2 ][ 3 ] * data[ 3 ][ 0 ] - data[ 0 ][ 3 ] * data[ 2 ][ 0 ] * data[ 3 ][ 1 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 3 ][ 3 ] + data[ 0 ][ 1 ] * data[ 1 ][ 3 ] * data[ 3 ][ 0 ] + data[ 0 ][ 3 ] * data[ 1 ][ 0 ] * data[ 3 ][ 1 ]
										- data[ 0 ][ 0 ] * data[ 1 ][ 3 ] * data[ 3 ][ 1 ] - data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 3 ][ 3 ] - data[ 0 ][ 3 ] * data[ 1 ][ 1 ] * data[ 3 ][ 0 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 1 ][ 3 ] * data[ 2 ][ 1 ] + data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 2 ][ 3 ] + data[ 0 ][ 3 ] * data[ 1 ][ 1 ] * data[ 2 ][ 0 ]
										- data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 2 ][ 3 ] - data[ 0 ][ 1 ] * data[ 1 ][ 3 ] * data[ 2 ][ 0 ] - data[ 0 ][ 3 ] * data[ 1 ][ 0 ] * data[ 2 ][ 1 ] )" ~ op ~ "d ),
						vectorType( ( data[ 1 ][ 0 ] * data[ 2 ][ 2 ] * data[ 3 ][ 1 ] + data[ 1 ][ 1 ] * data[ 2 ][ 0 ] * data[ 3 ][ 2 ] + data[ 1 ][ 2 ] * data[ 2 ][ 1 ] * data[ 3 ][ 0 ]
										- data[ 1 ][ 0 ] * data[ 2 ][ 1 ] * data[ 3 ][ 2 ] - data[ 1 ][ 1 ] * data[ 2 ][ 2 ] * data[ 3 ][ 0 ] - data[ 1 ][ 2 ] * data[ 2 ][ 0 ] * data[ 3 ][ 1 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 2 ][ 1 ] * data[ 3 ][ 2 ] + data[ 0 ][ 1 ] * data[ 2 ][ 2 ] * data[ 3 ][ 0 ] + data[ 0 ][ 2 ] * data[ 2 ][ 0 ] * data[ 3 ][ 1 ]
										- data[ 0 ][ 0 ] * data[ 2 ][ 2 ] * data[ 3 ][ 1 ] - data[ 0 ][ 1 ] * data[ 2 ][ 0 ] * data[ 3 ][ 2 ] - data[ 0 ][ 2 ] * data[ 2 ][ 1 ] * data[ 3 ][ 0 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 3 ][ 1 ] + data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 3 ][ 2 ] + data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 3 ][ 0 ]
										- data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 3 ][ 2 ] - data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 3 ][ 0 ] - data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 3 ][ 1 ] )" ~ op ~ "d,
										( data[ 0 ][ 0 ] * data[ 1 ][ 1 ] * data[ 2 ][ 2 ] + data[ 0 ][ 1 ] * data[ 1 ][ 2 ] * data[ 2 ][ 0 ] + data[ 0 ][ 2 ] * data[ 1 ][ 0 ] * data[ 2 ][ 1 ]
										- data[ 0 ][ 0 ] * data[ 1 ][ 2 ] * data[ 2 ][ 1 ] - data[ 0 ][ 1 ] * data[ 1 ][ 0 ] * data[ 2 ][ 2 ] - data[ 0 ][ 2 ] * data[ 1 ][ 1 ] * data[ 2 ][ 0 ] )" ~ op ~ "d ) ];
					" );

			return mat;
		}

		// some static fun ...
		// ( 1 ) glprogramming.com / red / appendixf.html - ortographic is broken!
		// ( 2 ) http://fly.cc.fer.hr / ~unreal / theredbook / appendixg.html
		// ( 3 ) http://en.wikipedia.org / wiki / Orthographic_projection_( geometry )

		/// Returns a translation matrix ( 3x3 and 4x4 matrices ).
		static Matrix translation( valueType x, valueType y, valueType z )  {
			Matrix result = Matrix.identity;
			result.data[ 3 ][ 0 ] = x;
			result.data[ 3 ][ 1 ] = y;
			result.data[ 3 ][ 2 ] = z;
			return result;            
		}

		/// Applys a translation on the current matrix and returns $( I this ) ( 3x3 and 4x4 matrices ).
		Matrix translate( valueType x, valueType y, valueType z )  {
			this = Matrix.translation( x, y, z ) * this;
			return this;
		}

		/// Returns a scaling matrix ( 3x3 and 4x4 matrices );
		static Matrix scaling( valueType x, valueType y, valueType z )  {
			Matrix result = Matrix.identity;
			result.data[ 0 ][ 0 ] = x;
			result.data[ 1 ][ 1 ] = y;
			result.data[ 2 ][ 2 ] = z;
			return result;
		}

		/// Applys a scale to the current matrix and returns $( I this ) ( 3x3 and 4x4 matrices ).
		Matrix scale( valueType x, valueType y, valueType z )  {
			this = Matrix.scaling( x, y, z ) * this;
			return this;
		}

		unittest  {
			mat4 m4 = mat4( 1.0f );
			assert( m4.translation( 1.0f, 2.0f, 3.0f ).data == mat4.translation( 1.0f, 2.0f, 3.0f ).data );
			assert( mat4.translation( 1.0f, 2.0f, 3.0f ).data == [ [ 1.0f, 0.0f, 0.0f, 0.0f ],
																						[ 0.0f, 1.0f, 0.0f, 0.0f ],
																						[ 0.0f, 0.0f, 1.0f, 0.0f ],
																						[ 1.0f, 2.0f, 3.0f, 1.0f ] ] );
			assert( mat4.identity.translate( 0.0f, 1.0f, 2.0f ).data == mat4.translation( 0.0f, 1.0f, 2.0f ).data );
			assert( m4.scaling( 0.0f, 1.0f, 2.0f ).data == mat4.scaling( 0.0f, 1.0f, 2.0f ).data );
			assert( mat4.scaling( 0.0f, 1.0f, 2.0f ).data == [ [ 0.0f, 0.0f, 0.0f, 0.0f ],
																				  [ 0.0f, 1.0f, 0.0f, 0.0f ],
																				  [ 0.0f, 0.0f, 2.0f, 0.0f ],
																				  [ 0.0f, 0.0f, 0.0f, 1.0f ] ] );
			assert( mat4.identity.scale( 0.0f, 1.0f, 2.0f ).data == mat4.scaling( 0.0f, 1.0f, 2.0f ).data );
		}

		/// TODO : Remove redundant argument, use fovy, aspect, near, far
		/// http://www.geeks3d.com/20090729/howto-perspective-projection-matrix-in-opengl/
		static if ( isFloatingPoint!valueType )  {
			static private valueType[ 6 ] cperspective( valueType fovy, valueType aspect, valueType near, valueType far )  {
				valueType top = near * tan( fovy * ( PI / 360.0 ));
				valueType bottom = - top;
				valueType right = top * aspect;
				valueType left = - right;

				return [ left, right, bottom, top, near, far ];
			}

			/// Construct a symmetric perspective matrix ( 4x4 and floating - point matrices only ).
			static Matrix perspective( valueType fovy, valueType aspect, valueType near, valueType far )  {
				valueType[ 6 ] cdata = cperspective( fovy, aspect, near, far );
				return perspective( cdata[ 0 ], cdata[ 1 ], cdata[ 2 ], cdata[ 3 ], cdata[ 4 ], cdata[ 5 ] );
			}

			/// Construct an optionally non-symmetric perspective matrix 
			static Matrix perspective( valueType left, valueType right, valueType bottom, valueType top, valueType near, valueType far )
			in  {
				assert( right - left != 0 );
				assert( top - bottom != 0 );
				assert( far - near != 0 );
			}
			body  {
				Matrix result;
				result.clear( 0 );
				result.data[ 0 ][ 0 ] = ( 2 * near ) / ( right - left );
				result.data[ 2 ][ 0 ] = ( right + left ) / ( right - left );
				result.data[ 1 ][ 1 ] = ( 2 * near ) / ( top - bottom );
				result.data[ 2 ][ 1 ] = ( top + bottom ) / ( top - bottom );
				result.data[ 2 ][ 2 ] = - ( far + near ) / ( far - near );
				result.data[ 3 ][ 2 ] = - ( 2 * far * near ) / ( far - near );
				result.data[ 2 ][ 3 ] = - 1;

				return result;
			}

			/// Construct an inverse, symmetric perspective matrix ( 4x4 and floating - point matrices only ).
			static Matrix inversePerspective( valueType fovy, valueType aspect, valueType near, valueType far )  {
				valueType[ 6 ] cdata = cperspective( fovy, aspect, near, far );
				return inversePerspective( cdata[ 0 ], cdata[ 1 ], cdata[ 2 ], cdata[ 3 ], cdata[ 4 ], cdata[ 5 ] );
			}

			/// Construct an inverse, optionally non-symmetric perspective matrix 
			static Matrix inversePerspective( valueType left, valueType right, valueType bottom, valueType top, valueType near, valueType far )
			in  {
				assert( right - left != 0 );
				assert( top - bottom != 0 );
				assert( far - near != 0 );
			}
			body  {
				Matrix result;
				result.clear( 0 );

				result.data[ 0 ][ 0 ] = ( right - left ) / ( 2 * near );
				result.data[ 3 ][ 0 ] = ( right + left ) / ( 2 * near );
				result.data[ 1 ][ 1 ] = ( top - bottom ) / ( 2 * near );
				result.data[ 3 ][ 1 ] = ( top + bottom ) / ( 2 * near );
				result.data[ 3 ][ 2 ] = - 1;
				result.data[ 2 ][ 3 ] = - ( far - near ) / ( 2 * far * near );
				result.data[ 3 ][ 3 ] =   ( far + near ) / ( 2 * far * near );

				return result;
			}

			// ( 2 ) and ( 3 ) say this one is correct
			/// Construct an orthographic matrix ( 4x4 and floating - point matrices only ).
			static Matrix orthographic( valueType left, valueType right, valueType bottom, valueType top, valueType near, valueType far )
			in  {
				assert( right - left != 0 );
				assert( top - bottom != 0 );
				assert( far - near != 0 );
			}
			body  {
				Matrix result;
				result.clear( 0 );

				result.data[ 0 ][ 0 ] = 2 / ( right - left );
				result.data[ 3 ][ 0 ] = - ( right + left ) / ( right - left );
				result.data[ 1 ][ 1 ] = 2 / ( top - bottom );
				result.data[ 3 ][ 1 ] = - ( top + bottom ) / ( top - bottom );
				result.data[ 2 ][ 2 ] = - 2 / ( far - near );
				result.data[ 3 ][ 2 ] = - ( far + near ) / ( far - near );
				result.data[ 3 ][ 3 ] = 1;

				return result;
			}

			// ( 1 ) and ( 2 ) say this one is correct 
			/// Returns an inverse ortographic matrix ( 4x4 and floating - point matrices only ).
			static Matrix inverseOrthographic( valueType left, valueType right, valueType bottom, valueType top, valueType near, valueType far )  {
				Matrix result;
				result.clear( 0 );

				result.data[ 0 ][ 0 ] = ( right - left ) / 2;
				result.data[ 3 ][ 0 ] = ( right + left ) / 2;
				result.data[ 1 ][ 1 ] = ( top - bottom ) / 2;
				result.data[ 3 ][ 1 ] = ( top + bottom ) / 2;
				result.data[ 2 ][ 2 ] = ( far - near ) / - 2;
				result.data[ 3 ][ 2 ] = ( far + near ) / 2;
				result.data[ 3 ][ 3 ] = 1;

				return result;
			}

			/// Construct a look at matrix ( 4x4 and floating - point matrices only ).
			static Matrix lookAt( Vector!( valueType, 3 ) eye, Vector!( valueType, 3 ) target, Vector!( valueType, 3 ) up )  {
				alias Vector!( valueType, 3 ) vec3mt;
				vec3mt look_dir = normalize( target - eye );
				vec3mt up_dir = normalize( up );

				vec3mt right_dir = normalize( cross( look_dir, up_dir ));
				vec3mt perp_up_dir = cross( right_dir, look_dir );

				Matrix result = Matrix.identity;
				result.data[ 0 ][ 0 .. 3 ] = right_dir.data;
				result.data[ 1 ][ 0 .. 3 ] = perp_up_dir.data;
				result.data[ 2 ][ 0 .. 3 ] = ( - look_dir ).data;

				result.data[ 0 ][ 3 ] = - dot( eye, right_dir );
				result.data[ 1 ][ 3 ] = - dot( eye, perp_up_dir );
				result.data[ 2 ][ 3 ] = dot( eye, look_dir );

				return result;
			}

			unittest  {
				valueType aspect = 6.0 / 9.0;              
				valueType[ 6 ] cp = cperspective( 60f, aspect, 1f, 100f );
				assert(  cp[ 4 ] == 1.0f );
				assert(  cp[ 5 ] == 100.0f );
				assert(  cp[ 0 ] == - cp[ 1 ] );
				assert(( cp[ 0 ] < - 0.38489f ) && ( cp[ 0 ] > - 0.38491f ));
				assert(  cp[ 2 ] == - cp[ 3 ] );
				assert(( cp[ 2 ] < - 0.577349f ) && ( cp[ 2 ] > - 0.577351f ));

				assert( mat4.perspective( 60.0, aspect, 1.0, 100.0 ) == mat4.perspective( cp[ 0 ], cp[ 1 ], cp[ 2 ], cp[ 3 ], cp[ 4 ], cp[ 5 ] ));
				float[ 4 ][ 4 ] m4p = mat4.perspective( 60.0, aspect, 1.0, 100.0 ).data;
				assert(( m4p[ 0 ][ 0 ] < 2.598077f ) && ( m4p[ 0 ][ 0 ] > 2.598075f ));
				assert(  m4p[ 0 ][ 2 ] == 0.0f );
				assert(( m4p[ 1 ][ 1 ] < 1.732052 ) && ( m4p[ 1 ][ 1 ] > 1.732050 ));
				assert(  m4p[ 1 ][ 2 ] == 0.0f );
				assert(( m4p[ 2 ][ 2 ] < - 1.020201 ) && ( m4p[ 2 ][ 2 ] > - 1.020203 ));
				assert(( m4p[ 3 ][ 2 ] < - 2.020201 ) && ( m4p[ 3 ][ 2 ] > - 2.020203 ));
				assert(( m4p[ 2 ][ 3 ] < - 0.90000f ) && ( m4p[ 2 ][ 3 ] > - 1.10000f ));

				float[ 4 ][ 4 ] m4pi = mat4.inversePerspective( 60.0, aspect, 1.0, 100.0 ).data;
				assert(( m4pi[ 0 ][ 0 ] < 0.384901 ) && ( m4pi[ 0 ][ 0 ] > 0.384899 ));
				assert(  m4pi[ 0 ][ 3 ] == 0.0f );
				assert(( m4pi[ 1 ][ 1 ] < 0.577351 ) && ( m4pi[ 1 ][ 1 ] > 0.577349 ));
				assert(  m4pi[ 1 ][ 3 ] == 0.0f );
				assert(  m4pi[ 3 ][ 2 ] == - 1.0f );
				assert(( m4pi[ 2 ][ 3 ] < - 0.494999 ) && ( m4pi[ 2 ][ 3 ] > - 0.495001 ));
				assert(( m4pi[ 3 ][ 3 ] <   0.505001 ) && ( m4pi[ 3 ][ 3 ] > 0.504999 ));

				// maybe the next tests should be improved
				float[ 4 ][ 4 ] m4o = mat4.orthographic( - 1.0f, 1.0f, - 1.0f, 1.0f, - 1.0f, 1.0f ).data;
				assert( m4o == [ 
					[ 1.0f, 0.0f,   0.0f, 0.0f ],
					[ 0.0f, 1.0f,   0.0f, 0.0f ],
					[ 0.0f, 0.0f, - 1.0f, 0.0f ],
					[ 0.0f, 0.0f,   0.0f, 1.0f ] ] );

				float[ 4 ][ 4 ] m4oi = mat4.inverseOrthographic( - 1.0f, 1.0f, - 1.0f, 1.0f, - 1.0f, 1.0f ).data;
				assert( m4oi == [ 
					[ 1.0f, 0.0f,   0.0f, 0.0f ],
					[ 0.0f, 1.0f,   0.0f, 0.0f ],
					[ 0.0f, 0.0f, - 1.0f, 0.0f ],
					[ 0.0f, 0.0f,   0.0f, 1.0f ] ] );

				//TODO: look_at tests
			}

		}

	}

	static if (( cols == rows ) && ( cols >= 3 ))  {
		/// Returns an identity matrix with an applied rotate_axis around an arbitrary axis ( nxn matrices, n >= 3 ).
		static Matrix rotation( real angle, Vector!( valueType, 3 ) axis )  {
			Matrix result = Matrix.identity;

			auto length = axis.length;
			if ( length != 1 )  {
				version( NoReciprocalMul )  {
					axis /= length;
				}	else	{
					auto invLength = 1.0 / length;
					axis *= invLength;
				}
			}

			real cosAngle = cos( angle );
			real sinAngle = sin( angle );

			Vector!( valueType, 3 ) temp = ( 1 - cosAngle ) * axis;

			result.data[ 0 ][ 0 ] = to!valueType( cosAngle	+	temp.x * axis.x );
			result.data[ 0 ][ 1 ] = to!valueType(				temp.x * axis.y + sinAngle * axis.z );
			result.data[ 0 ][ 2 ] = to!valueType(				temp.x * axis.z - sinAngle * axis.y );
			result.data[ 1 ][ 0 ] = to!valueType(				temp.y * axis.x - sinAngle * axis.z );
			result.data[ 1 ][ 1 ] = to!valueType( cosAngle	+	temp.y * axis.y );
			result.data[ 1 ][ 2 ] = to!valueType(				temp.y * axis.z + sinAngle * axis.x );
			result.data[ 2 ][ 0 ] = to!valueType(				temp.z * axis.x + sinAngle * axis.y );
			result.data[ 2 ][ 1 ] = to!valueType(				temp.z * axis.y - sinAngle * axis.x );
			result.data[ 2 ][ 2 ] = to!valueType( cosAngle	+	temp.z * axis.z );

			return result;
		}

		static Matrix rotation( real angle, valueType x, valueType y, valueType z )  {
			return Matrix.rotation( angle, Vector!( valueType, 3 )( x, y, z ));
		}

		/// Returns an identity matrix with an applied rotation around the A-Canonical - axis ( nxn matrices, n >= 3 ).
		static Matrix rotationA( ubyte a, ubyte b )( real angle )  {
			Matrix result = Matrix.identity;

			valueType cosAngle = to!valueType( cos( angle ));
			valueType sinAngle = to!valueType( sin( angle ));

			result.data[ a ][ a ] = cosAngle;
			result.data[ b ][ a ] = - sinAngle;
			result.data[ a ][ b ] = sinAngle;
			result.data[ b ][ b ] = cosAngle;

			return result;
		}

		alias rotationA!( 1, 2 ) rotationX;	/// A-Cannonical = X
		alias rotationA!( 2, 0 ) rotationY;	/// A-Cannonical = Y
		alias rotationA!( 0, 1 ) rotationZ;	/// A-Cannonical = Z

		/// Rotate the current matrix arround an arbitrary axis
		Matrix rotate( real angle, Vector!( valueType, 3 ) axis )  {
			this = rotation( angle, axis ) * this;
			return this;
		}

		/// Rotates the current matrix around the x - axis and returns $( I this ) ( nxn matrices, n >= 3 ).
		Matrix rotateX( real angle )  {
			this = rotationX( angle ) * this;
			return this;
		}

		/// Rotates the current matrix around the y - axis and returns $( I this ) ( nxn matrices, n >= 3 ).
		Matrix rotateY( real angle )  {
			this = rotationY( angle ) * this;
			return this;
		}

		/// Rotates the current matrix around the z - axis and returns $( I this ) ( nxn matrices, n >= 3 ).
		Matrix rotateZ( real angle )  {
			this = rotationZ( angle ) * this;
			return this;
		}

		unittest  {
			assert( mat4.rotationX( 0 ).data == [ [ 1.0f,   0.0f,   0.0f, 0.0f ],
															  [ 0.0f,   1.0f, - 0.0f, 0.0f ],
															  [ 0.0f,   0.0f,   1.0f, 0.0f ],
															  [ 0.0f,   0.0f,   0.0f, 1.0f ] ] );
			assert( mat4.rotationY( 0 ).data == [ [ 1.0f,   0.0f,   0.0f, 0.0f ],
															  [ 0.0f,   1.0f,   0.0f, 0.0f ],
															  [ 0.0f,   0.0f,   1.0f, 0.0f ],
															  [ 0.0f,   0.0f,   0.0f, 1.0f ] ] );
			assert( mat4.rotationZ( 0 ).data == [ [ 1.0f, - 0.0f,   0.0f, 0.0f ],
															  [ 0.0f,   1.0f,   0.0f, 0.0f ],
															  [ 0.0f,   0.0f,   1.0f, 0.0f ],
															  [ 0.0f,   0.0f,   0.0f, 1.0f ] ] );

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


		/// Returns an identity matrix with the current translation applied ( nxn matrices, n >= 3 ) .. 
		Matrix translation()  {
			Matrix result = Matrix.identity;

			for( int c = 0; c < ( cols - 1 ); ++c )  {
				result.data[ c ][ cols - 1 ] = data[ c ][ cols - 1 ];
			}

			return result;
		}

		unittest  {
			mat3 m3 = mat3( 0.0f, 1.0f, 2.0f,
								 3.0f, 4.0f, 5.0f,
								 6.0f, 7.0f, 1.0f );
			assert( m3.translation.data == [ [ 1.0f, 0.0f, 2.0f ], [ 0.0f, 1.0f, 5.0f ], [ 0.0f, 0.0f, 1.0f ] ] );

			//m3.translation = mat3.identity;
			//assert( mat3.identity.data == m3.translation.data );

			//m3.translation = [ 2.0f, 5.0f ];
			//assert( m3.translation.data == [ [ 1.0f, 0.0f, 2.0f ], [ 0.0f, 1.0f, 5.0f ], [ 0.0f, 0.0f, 1.0f ] ] );
			//assert( mat3.identity.data == mat3.identity.translation.data );

			mat4 m4 = mat4(  0.0f,  1.0f,  2.0f,  3.0f,
								  4.0f,  5.0f,  6.0f,  7.0f,
								  8.0f,  9.0f, 10.0f, 11.0f,
								 12.0f, 13.0f, 14.0f,  1.0f );
			assert( m4.translation.data == [ [ 1.0f, 0.0f, 0.0f,  3.0f ],
														[ 0.0f, 1.0f, 0.0f,  7.0f ],
														[ 0.0f, 0.0f, 1.0f, 11.0f ],
														[ 0.0f, 0.0f, 0.0f,  1.0f ] ] );

			//m4.translation = mat4.identity;
			//assert( mat4.identity.data == m4.translation.data );

			//m4.translation = [ 3.0f, 7.0f, 11.0f ];
			//assert( m4.translation.data == [ [ 1.0f, 0.0f, 0.0f,  3.0f ],
			//											[ 0.0f, 1.0f, 0.0f,  7.0f ],
			//											[ 0.0f, 0.0f, 1.0f, 11.0f ],
			//											[ 0.0f, 0.0f, 0.0f,  1.0f ] ] );
			//assert( mat4.identity.data == mat4.identity.translation.data );
		}


		/// Returns an identity matrix with the current scale applied ( nxn matrices, n >= 3 ).
		Matrix scale()  { 
			Matrix result = Matrix.identity;

			for( int c = 0; c < ( cols - 1 ); ++c )  {
				result.data[ c ][ c ] = data[ c ][ c ];
			}

			return result;
		}

		unittest  {
			mat3 m3 = mat3( 0.0f, 1.0f, 2.0f,
								 3.0f, 4.0f, 5.0f,
								 6.0f, 7.0f, 1.0f );
			assert( m3.scale.data == [ [ 0.0f, 0.0f, 0.0f ], 
												[ 0.0f, 4.0f, 0.0f ],
												[ 0.0f, 0.0f, 1.0f ] ] );

			//m3.scale = mat3.identity;
			//assert( mat3.identity.data == m3.scale.data );

			//m3.scale = [ 0.0f, 4.0f ];
			//assert( m3.scale.data == [ [ 0.0f, 0.0f, 0.0f ],
			//									[ 0.0f, 4.0f, 0.0f ],
			//									[ 0.0f, 0.0f, 1.0f ] ] );
			assert( mat3.identity.data == mat3.identity.scale.data );

			mat4 m4 = mat4(  0.0f,  1.0f,  2.0f,  3.0f,
								  4.0f,  5.0f,  6.0f,  7.0f,
								  8.0f,  9.0f, 10.0f, 11.0f,
								 12.0f, 13.0f, 14.0f,  1.0f );
			assert( m4.scale.data == [ [ 0.0f, 0.0f,  0.0f, 0.0f ],
												[ 0.0f, 5.0f,  0.0f, 0.0f ],
												[ 0.0f, 0.0f, 10.0f, 0.0f ],
												[ 0.0f, 0.0f,  0.0f, 1.0f ] ] );

			//m4.scale = mat4.identity;
			//assert( mat4.identity.data == m4.scale.data );

			//m4.scale = [ 0.0f, 5.0f, 10.0f ];
			//assert( m4.scale.data == [ [ 0.0f, 0.0f,  0.0f, 0.0f ],
			//									[ 0.0f, 5.0f,  0.0f, 0.0f ],
			//									[ 0.0f, 0.0f, 10.0f, 0.0f ],
			//									[ 0.0f, 0.0f,  0.0f, 1.0f ] ] );
			//assert( mat4.identity.data == mat4.identity.scale.data );
		}


		/// Returns an identity matrix with the current rotation applied ( nxn matrices, n >= 3 ).
		Matrix!( valueType, 3, 3 ) rotation()  {
			Matrix!( valueType, 3, 3 ) result = Matrix!( valueType, 3, 3 ).identity;

			for( int c = 0; c < 3; ++c )  {
				for( int r = 0; r < 3; ++r )  {
					result.data[ c ][ r ] = data[ c ][ r ];
				}
			}

			return result;
		}

		unittest  {
			mat3 m3 = mat3( 0.0f, 1.0f, 2.0f,
								 3.0f, 4.0f, 5.0f,
								 6.0f, 7.0f, 1.0f );
			assert( m3.rotation.data == [ [ 0.0f, 1.0f, 2.0f ],
													[ 3.0f, 4.0f, 5.0f ],
													[ 6.0f, 7.0f, 1.0f ] ] );

			//m3.rotation = mat3.identity;
			//assert( mat3.identity.data == m3.rotation.data );

			//m3.rotation = mat3( 0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 1.0f );
			//assert( m3.rotation.data == [ [ 0.0f, 1.0f, 2.0f ],
			//										[ 3.0f, 4.0f, 5.0f ],
			//										[ 6.0f, 7.0f, 1.0f ] ] );
			assert( mat3.identity.data == mat3.identity.rotation.data );

			mat4 m4 = mat4(  0.0f,  1.0f,  2.0f,  3.0f,
								  4.0f,  5.0f,  6.0f,  7.0f,
								  8.0f,  9.0f, 10.0f, 11.0f,
								 12.0f, 13.0f, 14.0f,  1.0f );
			assert( m4.rotation.data == [ [ 0.0f, 1.0f, 2.0f ],
													[ 4.0f, 5.0f, 6.0f ],
													[ 8.0f, 9.0f, 10.0f ] ] );

			//m4.rotation = mat3.identity;
			//assert( mat3.identity.data == m4.rotation.data );

			//m4.rotation = mat3( 0.0f, 1.0f, 2.0f, 4.0f, 5.0f, 6.0f, 8.0f, 9.0f, 10.0f );
			//assert( m4.rotation.data == [ [ 0.0f, 1.0f, 2.0f ],
			//										[ 4.0f, 5.0f, 6.0f ],
			//										[ 8.0f, 9.0f, 10.0f ] ] );
			//assert( mat3.identity.data == mat4.identity.rotation.data );
		}
	}

	static if (( cols == rows ) && ( cols <= 4 ))  {
		/// Returns an inverted copy of the current matrix ( nxn matrices, n <= 4 ).
		@property Matrix inverse() const  {
			Matrix  result;
			invert( result );
			return  result;
		}

		/// Inverts the current matrix ( nxn matrices, n <= 4 ).
		void invert()  {
			invert( this );
		}
	}


	unittest  {
		mat2 m2 = mat2( 1.0f, 2.0f, vec2( 3.0f, 4.0f ));
		assert( m2.determinant == - 2.0f );
		assert( m2.inverse.data == [ [ - 2.0f, 1.0f ],
											  [ 1.5f, - 0.5f ] ] );

		mat3 m3 = mat3( 1.0f, - 2.0f,   3.0f,
							 7.0f, - 1.0f,   0.0f,
							 3.0f,   2.0f, - 4.0f );
		assert( m3.determinant == - 1.0f );
		assert( m3.inverse.data == [ [ -  4.0f,  2.0f, -  3.0f ],
											  [ - 28.0f, 13.0f, - 21.0f ],
											  [ - 17.0f,  8.0f, - 13.0f ] ] );

		mat4 m4 = mat4(	1.0f,	2.0f, 3.0f,   4.0f,
							 - 2.0f,	1.0f, 5.0f, - 2.0f,
								2.0f, - 1.0f, 7.0f,   1.0f,
								3.0f, - 3.0f, 2.0f,   0.0f );
		assert( m4.determinant == - 8.0f );
		assert( m4.inverse.data == [ [   6.875f,   7.875f, - 11.75f, 11.125f  ],
											  [   6.625f,   7.625f, - 11.25f, 10.375f  ],
											  [ - 0.375f, - 0.375f,    0.75f, - 0.625f ],
											  [ - 4.5f, - 5.5f,    8.0f, - 7.5f   ] ] );
	}





	/// Componentwise binary matrix-skalar operation: addition, subtraction, multiplication, division
	auto opBinary( string op, T )( T s ) const if ( isNumeric!T && (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" )))  {
		Matrix result;
		mixin( "result[0] = data[0]" ~ op ~ "s;" );
		mixin( "result[1] = data[1]" ~ op ~ "s;" );
		static if( cols >= 3 )  mixin( "result[2] = data[2]" ~ op ~ "s;" );
		static if( cols == 4 )  mixin( "result[3] = data[3]" ~ op ~ "s;" );
		return result;
	}

	/// Componentwise binary skalar-matrix operation: addition, subtraction, multiplication, division
	auto opBinaryRight( string op, T )( T s ) const if ( isNumeric!T && (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" )))  {
		Matrix result;
		mixin( "result[0] = s" ~ op ~ "data[0];" );
		mixin( "result[1] = s" ~ op ~ "data[1];" );
		static if( cols >= 3 )  mixin( "result[2] = s" ~ op ~ "data[2];" );
		static if( cols == 4 )  mixin( "result[3] = s" ~ op ~ "data[3];" );
		return result;
	}

	unittest  {		/// Matrix-Scalar Operations
		mat2 m2 = mat2( 1.0f, 2.0f, 3.0f, 4.0f );
		assert(( m2 * 2.0f ).data == [ [ 2.0f, 4.0f ], [ 6.0f, 8.0f ] ] );
		assert(( 2 * m2 ).data == ( m2 * 2 ).data );

		mat3 m3 = mat3( 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f );
		assert(( m3 * 2 ).data == [ [ 2.0f, 4.0f, 6.0f ], [ 8.0f, 10.0f, 12.0f ], [ 14.0f, 16.0f, 18.0f ] ] );
		assert(( 2 * m3 ).data == ( m3 * 2 ).data );

		//TODO: tests for mat4, mat34
	}


	/// Matrix-vector multiplication
	auto opBinary( string op : "*", V )( V vec ) const if ( isCompatibleVector!V && V.dimension == cols )  {
		vectorType result;
		result.clear( 0 );
		foreach( c; 0 .. cols )
			foreach( r; 0 .. rows )
				result[ r ] += data[ c ][ r ] * vec[ c ];
		return result;
	}

	/// Vector-Matrix multiplication
	auto opBinaryRight( string op : "*", V : vectorType )( V vec ) const if ( isVector!V && V.dimension == rows )  {
		auto transposedMatrix = transposed();
		return transposedMatrix * vec;
	}

	unittest  {		/// matrix-vector, vector-matrix multiplacation
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

		//TODO: tests for mat4, mat34, mat43, mat24, mat 42 
	}


	/// matrix-matrix componentwise operations addition, subtraction and division, using vector-vector operations of all colums
	Matrix opBinary( string op )( Matrix mat ) const if ( op == "+" || op == "-" || op == "/" )  {
		Matrix result;
		mixin( "result[0] = data[0]" ~ op ~ "mat[0];" );
		mixin( "result[1] = data[1]" ~ op ~ "mat[1];" );
		static if( cols >= 3 )  mixin( "result[2] = data[2]" ~ op ~ "mat[2];" );
		static if( cols == 4 )  mixin( "result[3] = data[3]" ~ op ~ "mat[3];" );
		return result;
	}

	/// matrix-matrix multiplication, using matrix-vector multiplication for each column of mat
	Matrix opBinary( string op : "*", M )( M mat ) const if ( isCompatibleMatrix!M && cols == M.rows )  {
		Matrix!( valueType, cols, M.cols ) result;
		result[0] = this * mat[0];
		result[1] = this * mat[1];
		static if( M.cols >= 3 )  result[2] = this * mat[2];
		static if( M.cols == 4 )  result[3] = this * mat[3];
		return result;
	}


	unittest  {		/// Matrix-Matrix Operations
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

		//TODO: tests for mat4, mat34
	}


	void opOpAssign( string op, T )( T val )  {
		mixin( "this = this " ~ op ~  "val;" );
	}

	unittest  {		/// Matrix Unary Operations
		mat2  m2 = mat2( 2.0f, 4.0f, 6.0f, 8.0f );
		m2 += m2; assert( m2 == [ [ 4.0f, 8.0f ], [ 12.0f, 16.0f ] ] );
		m2 /= m2; assert( m2 == [ [ 1.0f, 1.0f ], [ 1.0f, 1.0f ] ] );
		m2 -= m2; assert( m2 == [ [ 0.0f, 0.0f ], [ 0.0f, 0.0f ] ] );

		mat3  m3 = mat3( 2.0f, 4.0f, 6.0f, 8.0f, 10.0f, 12.0f, 14.0f, 16.0f, 18.0f );
		m3 += m3; assert( m3 == [ [ 4.0f, 8.0f, 12.0f ], [ 16.0f, 20.0f, 24.0f ], [ 28.0f, 32.0f, 36.0f ] ] );
		m3 /= m3; assert( m3 == [ [ 1.0f, 1.0f, 1.0f ], [ 1.0f, 1.0f, 1.0f ], [ 1.0f, 1.0f, 1.0f ] ] );
		m3 -= m3; assert( m3 == [ [ 0.0f, 0.0f, 0.0f ], [ 0.0f, 0.0f, 0.0f ], [ 0.0f, 0.0f, 0.0f ] ] );

		//TODO: tests for mat4, mat34
	}


	bool opCast( T : bool )() const  {
		return ok;
	}

	unittest  {
		assert( mat2( 1.0f, 2.0f, 1.0f, 1.0f ) == mat2( 1.0f, 2.0f, 1.0f, 1.0f ));
		assert( mat2( 1.0f, 2.0f, 1.0f, 1.0f ) != mat2( 1.0f, 1.0f, 1.0f, 1.0f ));

		assert( mat3( 1.0f ) == mat3( 1.0f ));
		assert( mat3( 1.0f ) != mat3( 2.0f ));

		assert( mat4( 1.0f ) == mat4( 1.0f ));
		assert( mat4( 1.0f ) != mat4( 2.0f ));

		assert( !( mat4( float.nan )));
		if ( mat4( 1.0f ))  { }
		else  { assert( false ); }
	}

}

/// Pre - defined matrix types, the first number represents the number of cols 
/// and the second the number of columns, if there's just one it's a nxn matrix.
/// All of these matrices are floating - point matrices.
alias Matrix!( float, 2, 2 ) mat2;
alias Matrix!( float, 2, 3 ) mat23;
alias Matrix!( float, 2, 4 ) mat24;

alias Matrix!( float, 3, 2 ) mat32;
alias Matrix!( float, 3, 3 ) mat3;
alias Matrix!( float, 3, 4 ) mat34;

alias Matrix!( float, 4, 2 ) mat42;
alias Matrix!( float, 4, 3 ) mat43;
alias Matrix!( float, 4, 4 ) mat4;

