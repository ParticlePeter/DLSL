/**
dlsl.vector


Authors: Peter Particle ( based on gl3n by David Herberth )
License: MIT

Note: All methods marked with pure are weakly pure since, they all access an instance member.
All static methods are strongly pure.
*/

module dlsl.vector;

import std.math : sqrt;
import std.format : sformat;
import std.traits : isFloatingPoint, isArray;


/// Pre-defined vector types
alias Vector!( float, 2 ) vec2;
alias Vector!( float, 3 ) vec3;
alias Vector!( float, 4 ) vec4;

alias Vector!( double, 2 ) dvec2;
alias Vector!( double, 3 ) dvec3;
alias Vector!( double, 4 ) dvec4;

alias Vector!( int, 2 ) ivec2;
alias Vector!( int, 3 ) ivec3;
alias Vector!( int, 4 ) ivec4;

alias Vector!( uint, 2 ) uvec2;
alias Vector!( uint, 3 ) uvec3;
alias Vector!( uint, 4 ) uvec4;

alias Vector!( byte, 2 ) ivec2b;
alias Vector!( byte, 3 ) ivec3b;
alias Vector!( byte, 4 ) ivec4b;

alias Vector!( ubyte, 2 ) uvec2b;
alias Vector!( ubyte, 3 ) uvec3b;
alias Vector!( ubyte, 4 ) uvec4b;

alias Vector!( short, 2 ) ivec2s;
alias Vector!( short, 3 ) ivec3s;
alias Vector!( short, 4 ) ivec4s;

alias Vector!( ushort, 2 ) uvec2s;
alias Vector!( ushort, 3 ) uvec3s;
alias Vector!( ushort, 4 ) uvec4s;


/// If T is a vector, this evaluates to true, otherwise false
template isVector( T )  {  enum isVector = is( typeof( isVectorImpl( T.init )));  }
private void isVectorImpl( T, int dim )( Vector!( T, dim ) vec )  {}

/// Base template for all vector-types.
/// Params:
/// type = the value type of each vector element
/// dim  = specifies the dimension of the vector, can be 1, 2, 3 or 4
/// Examples:
/// ---
/// alias Vector!(int, 3) vec3i;
/// alias Vector!(float, 4) vec4;
/// alias Vector!(real, 2) vec2r;
/// ---
struct Vector( type, int dim ) if (( dim >= 2 ) && ( dim <= 4 ))  {
	type[ dim ] data;					/// Holds all coordinates, length conforms dimension
	alias data this;					/// The Vector can be treated as an array
	alias dimension = dim;				/// Holds the dimension of the vector
	alias type valueType;				/// Holds the internal type of the vector


	// Unittest Construction via aliased this static array
	unittest  {
		vec2 v2 = [ 0.0f, 1.0f ];
		assert( v2[ 0 ] == 0.0f );
		assert( v2[ 1 ] == 1.0f );

		v2[ 0 .. 1 ] = [ 2.0 ];
		assert( v2 == [ 2.0f, 1.0f ] );

		vec3 v3 = vec3( 0.0f, 0.1f, 0.2f );
		assert( v3[ 0 ] == 0.0f );
		assert( v3[ 1 ] == 0.1f );
		assert( v3[ 2 ] == 0.2f );

		v3[ 0 .. $ ] = 0.0f;
		assert( v3 == [ 0.0f, 0.0f, 0.0f ] );

		vec4 v4 = vec4( 0.0f, 0.1f, 0.2f, 0.3f );
		assert( v4[ 0 ] == 0.0f );
		assert( v4[ 1 ] == 0.1f );
		assert( v4[ 2 ] == 0.2f );
		assert( v4[ 3 ] == 0.3f );

		v4[ 1 .. 3 ] = [ 1.0f, 2.0f ];
		assert( v4 == [ 0.0f, 1.0f, 2.0f, 0.3f ] );
	}

	/// Returns a pointer to the coordinates.
	@property auto ptr()		{  return data.ptr;  }

	/// Returns the current vector formatted as string, useful for printing the vector
	@property string asString()	{  import std.string : format; return format( "%s", data );  }
	alias asString toString;

	@safe pure nothrow :
	template isCompatibleVector( T )  {  enum isCompatibleVector = is( typeof( isCompatibleVectorImpl( T.init )));  }
	//static void isCompatibleVectorImpl( int dim )( Vector!( valueType, dim ) vec ) if( dim <= dimension )  {}	// no valueType conversion
	static void isCompatibleVectorImpl( vt, int dim )( Vector!( vt, dim ) vec ) if( dim <= dimension )  {}			// implicit valueType conversion

	/// Unittest : isCompatibleType
	unittest  {
		// isCompatibleType without implicit valueType conversion
		//vec2 v2; assert( v2.isCompatibleVector!vec2 ); assert( !v2.isCompatibleVector!vec3 ); assert( !v2.isCompatibleVector!vec4 );
		//vec3 v3; assert( v3.isCompatibleVector!vec2 ); assert(  v3.isCompatibleVector!vec3 ); assert( !v3.isCompatibleVector!vec4 );
		//vec4 v4; assert( v4.isCompatibleVector!vec2 ); assert(  v4.isCompatibleVector!vec3 ); assert(  v4.isCompatibleVector!vec4 );

		// isCompatibleType with implicit valueType conversion
		vec2  v2; assert( v2.isCompatibleVector!vec2i ); assert( !v2.isCompatibleVector!vec3d ); assert( !v2.isCompatibleVector!vec4  );
		vec3i v3; assert( v3.isCompatibleVector!vec2d ); assert(  v3.isCompatibleVector!vec3  ); assert( !v3.isCompatibleVector!vec4i );
		vec4d v4; assert( v4.isCompatibleVector!vec2  ); assert(  v4.isCompatibleVector!vec3i ); assert(  v4.isCompatibleVector!vec4d );
	}


	/// Helper method to recursivly construct a vector from an argument list with different types
	/// TODO: Fix Construction with combination of numeric, vector, static and dynamic array
	private void construct( int i, T, Tail... )( T head, Tail tail )  {
		import std.traits : isDynamicArray, isStaticArray;
		static if ( i >= dimension )  {
			static assert( false, "Constructor has too many arguments!" );
		} else static if ( is( T : valueType )) {
			data[i] = head;
			construct!( i + 1 )( tail );
		} else static if ( isDynamicArray!T )  {
			static assert(( Tail.length == 0 ) && ( i == 0 ), "dynamic array can not be passed together with other arguments" );
			data = head;
		} else static if( isStaticArray!T )  {
			data[ i .. i + T.length ] = head;
			construct!( i + T.length )( tail );
		} else static if ( isCompatibleVector!T )  {
			data[ i .. i + T.dimension ] = head.data;
			construct!( i + T.dimension )( tail );
		} else {
			char[128] formatBuffer;
			auto formatString = sformat( formatBuffer, "Vector constructor argument must be of type %s or Vector, not %s", valueType.stringof, T.stringof );
			static assert( false, formatString );
		}
	}

	private void construct( int i )() {}		// terminate

	/// TODO: Unittest: Construction with combination of numeric, vector, static and dynamic array
	unittest  {
		auto v2 = vec2( 2.0f, 1 );
	}


	/// Constructs the vector
	/// If a single value is passed the vector, the vector will be cleared with this value
	/// If a vector with a higher dimension is passed the vector will hold the first values up to its dimension
	/// If mixed types are passed they will be joined together ( allowed types: vector, static array, $( I vt ))
	/// Examples:
	/// ---
	/// vec4 v4 = vec4( 1.0f, vec2( 2.0f, 3.0f ), 4.0f );
	/// vec3 v3 = vec3( v4 ); // v3 = vec3( 1.0f, 2.0f, 3.0f );
	/// vec2 v2 = v3.xy; // swizzling returns a static array.
	/// vec3 v3_2 = vec3( 1.0f ); // vec3 v3_2 = vec3( 1.0f, 1.0f, 1.0f );
	/// ---
	this( Args... )( Args args )  {  construct!( 0 )( args );  }


	/// Construct a Vector from another Vector
	this( V )( V vec ) if ( isVector!V && ( V.dimension >= dimension )) {
		auto minDimension = dimension < V.dimension ? dimension : V.dimension;
		static if( __traits( compiles, data = vec.data[ 0 .. minDimension ] ))  {
			data = vec.data[ 0 .. dimension ];
		} else {
			foreach( i; 0 .. minDimension )  {
				data[i] = cast( valueType )vec.data[i];
			}
		}
	}


	/// Unittest: Construct a Vector from another Vector
	unittest  {
		auto v4 = vec4( 1.0f, 2, 3, 4 );
		auto v3 = vec3( v4 );
		assert( v3 == [ 1, 2, 3 ] );

		auto v2 = vec2( v4 );
		assert( v2 == [ 1, 2 ] );

		/// Different valueTypes
		auto v4i = vec4i( 1, 2, 3, 4 );
		auto v3d = vec3d( v4i );
		assert( v3d == [ 1, 2, 3 ] );

		v3d.y = 3.9;
		auto v2i = vec2i( v3d );
		assert( v2i == [ 1, 3 ] );
	}


	/// Construct a Vector from a single value
	this()( valueType value ) {
		clear( value );
	}


	/// Unittest Construct a Vector from a single value
	unittest  {
		auto v2 = vec2( 2 );
		assert( v2 == [ 2, 2 ] );

		auto v3i = vec3i( 3 );
		assert( v3i == [ 3, 3, 3 ] );

		auto v4ub = vec4ub( 4 );
		assert( v4ub == [ 4, 4, 4, 4 ] );
	}


	/// FloatingPoint valueType, Returns true if all values are not nan and finite, otherwise false
	static if ( isFloatingPoint!valueType )  {
		bool opCast( T : bool )() const  {  return ok;  }
		import std.math : isNaN, isInfinity;
		@property bool ok() const   {
			foreach( ref val; data )  {
				if ( isNaN( val ) || isInfinity( val ))  return false;
			}
			return true;
		}
	}


	/// Sets all values of the vector to value
	void clear( valueType value )  {
		foreach( ref v; data )  {
			v = value;
		}
	}


	/// Unittest Construction
	/// TODO: Split up these Unittests
	unittest  {
		vec3 vecClear;
		assert( !vecClear.ok );
		vecClear.clear( 1.0f );
		assert( vecClear.ok );
		assert( vecClear.data == [ 1.0f, 1.0f, 1.0f ] );
		assert( vecClear.data == vec3( 1.0f ).data );
		vecClear.clear( float.infinity );
		assert( !vecClear.ok );
		vecClear.clear( float.nan );
		assert( !vecClear.ok );
		vecClear.clear( 1.0f );
		assert( vecClear.ok );

		vec4 b = vec4( 1.0f, vecClear );
		assert( b.ok );
		assert( b.data == [ 1.0f, 1.0f, 1.0f, 1.0f ] );
		assert( b.data == vec4( 1.0f ).data );

		vec2 v2_1 = vec2( vec2( 0.0f, 1.0f ));
		assert( v2_1.data == [ 0.0f, 1.0f ] );

		vec2 v2_2 = vec2( 1.0f, 1.0f );
		assert( v2_2.data == [ 1.0f, 1.0f ] );

		vec3 v3 = vec3( v2_1, 2.0f );
		assert( v3.data == [ 0.0f, 1.0f, 2.0f ] );

		vec4 v4_1 = vec4( 1.0f, vec2( 2.0f, 3.0f ), 4.0f );
		assert( v4_1.data == [  1.0f, 2.0f, 3.0f, 4.0f ] );
		assert( vec3( v4_1 ).data == [ 1.0f, 2.0f, 3.0f ] );
		assert( vec2( vec3( v4_1 )).data == [ 1.0f, 2.0f ] );
		assert( vec2( vec3( v4_1 )).data == vec2( v4_1 ).data );
		assert( v4_1.data == vec4( [ 1.0f, 2.0f, 3.0f, 4.0f ] ).data );

		vec4 v4_2 = vec4( vec2( 1.0f, 2.0f ), vec2( 3.0f, 4.0f ));
		assert( v4_2.data == [  1.0f, 2.0f, 3.0f, 4.0f ] );
		assert( vec3( v4_2 ).data == [ 1.0f, 2.0f, 3.0f ] );
		assert( vec2( vec3( v4_2 )).data == [ 1.0f, 2.0f ] );
		assert( vec2( vec3( v4_2 )).data == vec2( v4_2 ).data );
		assert( v4_2.data == vec4([ 1.0f, 2.0f, 3.0f, 4.0f ] ).data );

		float[2] f2 = [ 1.0f, 2.0f ];
		float[3] f3 = [ 1.0f, 2.0f, 3.0f ];
		float[4] f4 = [ 1.0f, 2.0f, 3.0f, 4.0f ];
		assert( vec2( 1.0f, 2.0f ).data == vec2(f2).data);
		assert( vec3( 1.0f, 2.0f, 3.0f ).data == vec3( f3 ).data );
		assert( vec3( 1.0f, 2.0f, 3.0f ).data == vec3( f2, 3.0f ).data );
		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ).data == vec4( f4 ).data );
		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ).data == vec4( f3, 4.0f ).data );
		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ).data == vec4( f2, 3.0f, 4.0f ).data );
		// useful for: "vec4 v4 = […]" or "vec4 v4 = other_vector.rgba"

		assert( vec3( vec3i( 1, 2, 3 )) == vec3( 1.0, 2.0, 3.0 ));
		assert( vec3d( vec3( 1.0, 2.0, 3.0 )) == vec3d( 1.0, 2.0, 3.0 ));
	}

	/// Build swizzle setter and getter properties
	import std.conv : to;
	enum string[dimension] comp = ( [ "x", "y", "z", "w" ] )[ 0 .. dimension ];
	enum string[dimension] fill = ( [ "", " ", "  ", "   " ] )[ 0 .. dimension ];

	/// generates one swizzle setter property for a given permutation
	static string genSetSwizz( int[] idx )  {
		string property;
		string funcBody;
		foreach( i; 0 .. idx.length ) {
			property ~= comp[ idx[i] ];
			funcBody ~= "data[" ~ to!string( idx[i] ) ~ "] = vec[" ~ to!string( i ) ~ "]; ";
		}
		string funcHead = "( Vector!( valueType, " ~ to!string( idx.length ) ~ " ) vec )  { ";
		return "@property void " ~ fill[ dimension - idx.length ] ~ property ~ funcHead ~ funcBody ~ "}\n";
	}

	/// generates all possible swizzle setter properties for the default permutation ( e.g. [0,1,2,3] )
	static string setSwizz( int[] idx )  {
		string result;
		if ( idx.length == 1 )  {
			auto funcBody = "( valueType val )  { " ~ "data[" ~ to!string( idx[0] ) ~ "] = val; }\n";
			result = "@property void " ~ fill[ dimension - idx.length ] ~ comp[ idx[0] ] ~ funcBody;
		}
		else if ( idx.length == 2 )  {
			foreach( i; 0 .. 2 )  {
				result ~= "\n" ~ setSwizz( [ idx[ i ] ] );
				result ~= genSetSwizz( [ idx[ i ], idx[ (i+1) % 2 ] ] );
			}
		}
		else if ( idx.length == 3 )  {
			foreach( i; 0 .. 3 )  {
				result ~= "\n" ~ setSwizz( [ idx[ i ] ] );
				foreach( j; 0 .. 2 )  result ~= genSetSwizz( [ idx[ i ], idx[ ( i + j + 1 ) % 3 ] ] );
				foreach( j; 0 .. 2 )  result ~= genSetSwizz( [ idx[ i ], idx[ ( i + j + 1 ) % 3 ], idx[ ( i + ( j + 1 ) % 2 + 1 ) % 3 ] ] );
			}
		}
		else if ( idx.length == 4 )  {
			foreach( i; 0 .. 4 )  {
				result ~= "\n" ~ setSwizz( [ idx[ i ] ] );
				foreach( j; 0 .. 3 )  {
					result ~= genSetSwizz( [ idx[ i ], idx[ ( i + j + 1 ) % 4 ] ] );
					foreach( k; 0 .. 2 )  result ~= genSetSwizz( [ idx[ i ], idx[ ( i + j + 1 ) % 4 ], idx[ ( i + ( j + k + 1 ) % 3 + 1 ) % 4 ] ] );
					foreach( k; 0 .. 2 )  result ~= genSetSwizz( [ idx[ i ], idx[ ( i + j + 1 ) % 4 ], idx[ ( i + ( j + k + 1 ) % 3 + 1 ) % 4 ], idx[ ( i + ( j + ( k + 1 ) % 2 + 1 ) % 3 + 1 ) % 4 ] ] );
				}
			}
		}
		return result;
	}

	enum string[dimension] aCol = ( [ "r", "g", "b", "a" ] )[ 0 .. dimension ];
	enum string[dimension] aTex = ( [ "s", "t", "p", "q" ] )[ 0 .. dimension ];

	/// Generates all possible swizzle getter properties, creates also property aliases e.g. rgba
	static string getSwizz( string component, string alias1, string alias2, string r, int term )  {
		string returnDim = to!string( dimension - term + 1 );
		string returnType = "@property auto ";
		string returnPrefix = "()  " ~ fill[ term - 1 ] ~ "const  {  return ";
		string result;

		foreach( i; 0 .. dimension )  {
			string property = component ~ comp[ i ];
			string aliColor = alias1 ~ aCol[ i ];
			string aliTexST = alias2 ~ aTex[ i ];

			string returnData = r ~ "data[" ~ to!string( i ) ~ "]";
			string returnString = returnPrefix ~ ( term == dimension
				?	returnData ~ ";  }"
				:	"Vector!( valueType, " ~ returnDim ~ " )( " ~ returnData ~ " );  }" );
			/// skip vec2.xy, vec3.xyz, vec4.xyzw which all return identity
			result ~= "alias " ~ aliColor~ fill[ term - 1 ] ~ " = " ~ property ~ fill[ term - 1 ] ~ "; ";
			result ~= "alias " ~ aliTexST~ fill[ term - 1 ] ~ " = " ~ property ~ fill[ term - 1 ] ~ "; ";
			if		( dimension == 2 && property == "xy"   )  result ~= returnType ~ property ~ "()  const  {  return this;  }\n";
			else  if( dimension == 3 && property == "xyz"  )  result ~= returnType ~ property ~ "()  const  {  return this;  }\n";
			else  if( dimension == 4 && property == "xyzw" )  result ~= returnType ~ property ~ "()  const  {  return this;  }\n";
			else {
				result ~= returnType ~ property ~ returnString ~ "\n";
				if ( term > 1 )  result ~= getSwizz( property, aliColor, aliTexST, returnData ~ ", ", term - 1 );
			}
		}
		return result;
	}

	enum int[ dimension ] idcs = ( [ 0, 1, 2, 3 ] )[ 0 .. dimension ];
	mixin( setSwizz( idcs ));
	mixin( getSwizz( "", "", "", "", dimension ));
	//pragma( msg, setSwizz( idcs ));
	//pragma( msg, getSwizz( "", "", "", "", dimension ));

	// another swizzle variant based on opDispatch, source: http://www.mmartins.me/view/2015/9/27/vector-swizzle-in-d
	// drawback is that lib is always recompiled if any swizzle operator changes, implementation at bottom of file


	// Unittest swizzle setter
	unittest  {
		vec2 v2 = vec2( 1 );
		v2.x = 0;
		assert( v2 == [ 0, 1 ] );

		vec3 v3 = vec3( 0 );
		//v3.yx = [ 1, 2 ];
		v3.yx =  vec2( 1, 2 );
		assert(  v3 == [ 2, 1, 0 ] );
		v3.zx =  vec2( 2, 0 );
		assert(  v3 == [ 0, 1, 2 ] );
		v3.zyx = vec3( 3, 2, 1 );
		assert(  v3 == [ 1, 2, 3 ] );

		vec4 v4 = vec4( 0 );
		//v3.yx = [ 1, 2 ];
		v4.wx = vec2( 1, 2 );
		assert(  v4 == [ 2, 0, 0, 1 ] );
		v4.zx =  vec2( 2, 0 );
		assert(  v4 == [ 0, 0, 2, 1 ] );
		v4.zyx = vec3( 3, 2, 1 );
		assert(  v4 == [ 1, 2, 3, 1 ] );
	}

	// Unittest swizzle getter
	unittest  {
		vec2 v2 = vec2( 1 );
		assert( v2 == v2.xy );
		v2.x = 0;
		assert( v2 == [ 0, 1 ] );
		assert( v2.x == 0 );

		vec3 v3 = vec3( 0 );
		v3.yx = vec2( 1, 2 );
		//v3.yx = [ 1, 2 ];
		assert( v3 == [ 2, 1, 0 ] );
	}
//*/
	/// Updates the vector with the values from other.
	//void update( Vector!( valueType, dimension ) vec )  {  data = vec.data;	 }

	/// Unittest for setting Values
	unittest  {
		vec2 v2 = vec2( 1.0f, 2.0f );
		assert( v2.x == 1.0f );
		assert( v2.y == 2.0f );
		v2.x = 3.0f;
		assert( v2.data == [ 3.0f, 2.0f ] );
		v2.y = 4.0f;
		assert( v2.data == [ 3.0f, 4.0f ] );
		assert(( v2.x == 3.0f ) && ( v2.x == v2.r ) && ( v2.x == v2.s ));
		assert( v2.y == 4.0f );
		assert(( v2.y == 4.0f ) && ( v2.y == v2.g ) && ( v2.y == v2.t ));
		v2 = [ 0.0f, 1.0f ];
		assert( v2.data == [ 0.0f, 1.0f ] );
		//v2.update( vec2( 3.0f, 4.0f ));
		//assert( v2.data == [ 3.0f, 4.0f ] );

		vec3 v3 = vec3( 1.0f, 2.0f, 3.0f );
		assert( v3.x == 1.0f );
		assert( v3.y == 2.0f );
		assert( v3.z == 3.0f );
		v3.x = 3.0f;
		assert( v3.data == [ 3.0f, 2.0f, 3.0f ] );
		v3.y = 4.0f;
		assert( v3.data == [ 3.0f, 4.0f, 3.0f ] );
		v3.z = 5.0f;
		assert( v3.data == [ 3.0f, 4.0f, 5.0f ] );
		assert(( v3.x == 3.0f ) && ( v3.x == v3.r ) && ( v3.x == v3.s ));
		assert(( v3.y == 4.0f ) && ( v3.y == v3.g ) && ( v3.y == v3.t ));
		assert(( v3.z == 5.0f ) && ( v3.z == v3.b ) && ( v3.z == v3.p ));
		v3 = [ 0.0f, 1.0f, 2.0f ];
		assert( v3.data == [ 0.0f, 1.0f, 2.0f ] );
		//v3.update( vec3( 3.0f, 4.0f, 5.0f ));
		//assert( v3.data == [ 3.0f, 4.0f, 5.0f ] );

		vec4 v4 = vec4( 1.0f, 2.0f, vec2( 3.0f, 4.0f ));
		assert( v4.x == 1.0f );
		assert( v4.y == 2.0f );
		assert( v4.z == 3.0f );
		assert( v4.w == 4.0f );
		v4.x = 3.0f;
		assert( v4.data == [ 3.0f, 2.0f, 3.0f, 4.0f ] );
		v4.y = 4.0f;
		assert( v4.data == [ 3.0f, 4.0f, 3.0f, 4.0f ] );
		v4.z = 5.0f;
		assert( v4.data == [ 3.0f, 4.0f, 5.0f, 4.0f ] );
		v4.w = 6.0f;
		assert( v4.data == [ 3.0f, 4.0f, 5.0f, 6.0f ] );
		assert(( v4.x == 3.0f ) && ( v4.x == v4.r ) && ( v4.x == v4.s ));
		assert(( v4.y == 4.0f ) && ( v4.y == v4.g ) && ( v4.y == v4.t ));
		assert(( v4.z == 5.0f ) && ( v4.z == v4.b ) && ( v4.z == v4.p ));
		assert(( v4.w == 6.0f ) && ( v4.w == v4.a ) && ( v4.w == v4.q ));
		v4 = [ 0.0f, 1.0f, 2.0f, 3.0f ];
		assert( v4.data == [ 0.0f, 1.0f, 2.0f, 3.0f ] );
		//v4.update( vec4( 3.0/, 4.0f, 5.0f, 6.0f ));
		//assert( v4.data == [ 3.0f, 4.0f, 5.0f, 6.0f ] );
	}


	/// TODO : patch unittest according to access sets !!!
	unittest  {
		vec2 v2 = vec2( 1.0f, 2.0f );
		assert( v2.ts == [ 2.0f, 1.0f ] );

		assert( vec3( 1.0f, 2.0f, 3.0f ).xyz == [ 1.0f, 2.0f, 3.0f ] );
		assert( vec4( v2, 3.0f, 4.0f  ).xyzw == [ 1.0f, 2.0f, 3.0f, 4.0f ] );
		assert( vec4( v2, 3.0f, 4.0f  ).wxyz == [ 4.0f, 1.0f, 2.0f, 3.0f ] );
		assert( vec4( 1.0f, v2.yx, 2.0f ).data == [ 1.0f, 2.0f, 1.0f, 2.0f ] );
	}


	/// Returns the euclidean length of the vector
	@property auto length() const  {	// Required to overwrite array.length
		return .length( this );			// .operator calls free module scope length function
	}


	/// TODO : This does not work
	//XForm.xy += vec2( md.rx, md.ry );

	/// Negate the vector
	Vector opUnary( string op : "-" )() const  {
		Vector result;
		result.data[0] = - data[0];
		result.data[1] = - data[1];
		static if( dimension >= 3 )  {  result.data[2] = - data[2];  }
		static if( dimension == 4 )  {  result.data[3] = - data[3];  }
		return result;
	}


	/// Unittest OpUnary Negate
	unittest  {
		assert( vec2(  1.0f, 1.0f ) == -vec2( -1.0f, -1.0f ));
		assert( vec2( -1.0f, 1.0f ) == -vec2(  1.0f, -1.0f ));

		assert( - vec3(  1.0f, 1.0f,  1.0f ) == vec3( -1.0f, -1.0f, -1.0f ));
		assert( - vec3( -1.0f, 1.0f, -1.0f ) == vec3(  1.0f, -1.0f,  1.0f ));

		assert( vec4(  1.0f, 1.0f,  1.0f, 1.0f ) == -vec4( -1.0f, -1.0f, -1.0f, -1.0f ));
		assert( vec4( -1.0f, 1.0f, -1.0f, 1.0f ) == -vec4(  1.0f, -1.0f,  1.0f, -1.0f ));
	}


	/// Componentwise binary vector-skalar operation: addition, subtraction, multiplication, division
	Vector opBinary( string op )( valueType s ) const if (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))  {
		Vector result;
		result.data[0] = mixin( "data[0]" ~ op ~ "s" );
		result.data[1] = mixin( "data[1]" ~ op ~ "s" );
		static if( dimension >= 3 )  {  result.data[2] = mixin( "data[2]" ~ op ~ "s" );  }
		static if( dimension == 4 )  {  result.data[3] = mixin( "data[3]" ~ op ~ "s" );  }
		return result;
	}


	/// Componentwise binary skalar-vector operation: addition, subtraction, multiplication, division
	auto opBinaryRight( string op )( valueType s ) const if (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))  {
		Vector result;
		result.data[0] = mixin( "s" ~ op ~ "data[0]" );
		result.data[1] = mixin( "s" ~ op ~ "data[1]" );
		static if( dimension >= 3 )  {  result.data[2] = mixin( "s" ~ op ~ "data[2]" );  }
		static if( dimension == 4 )  {  result.data[3] = mixin( "s" ~ op ~ "data[3]" );  }
		return result;
	}


	/// Componentwise binary operation with aonther vector: addition, subtraction, multiplication, division
	Vector opBinary( string op )( Vector v ) const if (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))  {
		Vector result;
		result.data[0] = mixin( "data[0]" ~ op ~ "v.data[0]" );
		result.data[1] = mixin( "data[1]" ~ op ~ "v.data[1]" );
		static if( dimension >= 3 )  {  result.data[2] = mixin( "data[2]" ~ op ~ "v.data[2]" );  }
		static if( dimension == 4 )  {  result.data[3] = mixin( "data[3]" ~ op ~ "v.data[3]" );  }
		return result;
	}


	/// Unittest OpBinary
	unittest  {
		vec2 v2 = vec2( 1.0f, 3.0f );
		cast( void )( 2 * v2 );
		assert(( v2 * 2.5f ).data == [ 2.5f, 7.5f ] );
		assert(( v2 + vec2( 3.0f, 1.0f )).data == [ 4.0f, 4.0f ] );
		assert(( v2 - vec2( 1.0f, 3.0f )).data == [ 0.0f, 0.0f ] );
		assert(( v2 * vec2( 2.0f, 2.0f ))  == vec2( 2.0f, 6.0f ));

		vec3 v3 = vec3( 1.0f, 3.0f, 5.0f );
		assert(( v3 * 2.5f ).data == [ 2.5f, 7.5f, 12.5f ] );
		assert(( v3 + vec3( 3.0f, 1.0f,  - 1.0f )).data == [ 4.0f, 4.0f, 4.0f ] );
		assert(( v3 - vec3( 1.0f, 3.0f, 5.0f )).data == [ 0.0f, 0.0f,  0.0f ] );
		assert(( v3 * vec3( 2.0f, 2.0f, 2.0f ))  == vec3( 2.0f, 6.0f, 10.0f ));

		vec4 v4 = vec4( 1.0f, 3.0f, 5.0f, 7.0f );
		assert(( v4 * 2.5f ).data == [ 2.5f, 7.5f, 12.5f, 17.5 ] );
		assert(( v4 + vec4( 3.0f, 1.0f, - 1.0f, - 3.0f )).data == [ 4.0f, 4.0f, 4.0f, 4.0f ] );
		assert(( v4 - vec4( 1.0f, 3.0f, 5.0f, 7.0f )).data == [ 0.0f, 0.0f,  0.0f,  0.0f ] );
		assert(( v4 * vec4( 2.0f, 2.0f, 2.0f, 2.0f ))  == vec4( 2.0f, 6.0f, 10.0f, 14.0f ));

	}

	/// Op= Operation with a scalar
	void opOpAssign( string op )( valueType val ) if (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))  {
		mixin( "data[0] " ~ op ~ "= val;" );
		mixin( "data[1] " ~ op ~ "= val;" );
		static if( dimension >= 3 )  mixin( "data[2] " ~ op ~ "= val;" );
		static if( dimension == 4 )  mixin( "data[3] " ~ op ~ "= val;" );
	}

	/// Componentwise Op= Operation with another vector
	void opOpAssign( string op )( Vector vec ) if (( op == "+" ) || ( op == "-" ) || ( op == "*" ) || ( op == "/" ))  {
		mixin( "data[0] " ~ op ~ "= vec.data[0];" );
		mixin( "data[1] " ~ op ~ "= vec.data[1];" );
		static if( dimension >= 3 )  mixin( "data[2] " ~ op ~ "= vec.data[2];" );
		static if( dimension == 4 )  mixin( "data[3] " ~ op ~ "= vec.data[3];" );
	}

	unittest  {
		vec2 v2 = vec2( 1.0f, 3.0f );
		v2 *= 2.5f;
		assert( v2.data == [ 2.5f, 7.5f ] );
		v2 -= vec2( 2.5f, 7.5f );
		assert( v2.data == [ 0.0f, 0.0f ] );
		v2 += vec2( 1.0f, 3.0f );
		assert( v2.data == [ 1.0f, 3.0f ] );
		//assert( almost_equal( v2.normalized, vec2( 1.0f/sqrt( 10.0f ), 3.0f/sqrt( 10.0f ))));

		vec3 v3 = vec3( 1.0f, 3.0f, 5.0f );
		v3 *= 2.5f;
		assert( v3.data == [ 2.5f, 7.5f, 12.5f ] );
		v3 -= vec3( 2.5f, 7.5f, 12.5f );
		assert( v3.data == [ 0.0f, 0.0f, 0.0f ] );
		v3 += vec3( 1.0f, 3.0f, 5.0f );
		assert( v3.data == [ 1.0f, 3.0f, 5.0f ] );
		//assert( almost_equal( v3.normalized, vec3( 1.0f/sqrt( 35.0f ), 3.0f/sqrt( 35.0f ), 5.0f/sqrt( 35.0f ))));

		vec4 v4 = vec4( 1.0f, 3.0f, 5.0f, 7.0f );
		v4 *= 2.5f;
		assert( v4.data == [ 2.5f, 7.5f, 12.5f, 17.5 ] );
		v4 -= vec4( 2.5f, 7.5f, 12.5f, 17.5f );
		assert( v4.data == [ 0.0f, 0.0f, 0.0f, 0.0f ] );
		v4 += vec4( 1.0f, 3.0f, 5.0f, 7.0f );
		assert( v4.data == [ 1.0f, 3.0f, 5.0f, 7.0f ] );
		//assert( almost_equal( v4.normalized, vec4( 1.0f/sqrt( 84.0f ), 3.0f/sqrt( 84.0f ), 5.0f/sqrt( 84.0f ), 7.0f/sqrt( 84.0f ))));
	}

	//void opAssign( A )( A a )  if ( isArray! A )  {
	//	data[] = cast( valueType[] )a[];
	//}

	/// Comparisson Operator
	const bool opEquals( T )( T vec ) if ( T.dimension == dimension )  {  return data == vec.data;  }

	/// Unittest Comparisson Operator
	unittest  {
		assert( vec2( 1.0f, 2.0f ) == vec2( 1.0f, 2.0f ));
		assert( vec2( 1.0f, 2.0f ) != vec2( 1.0f, 1.0f ));
		assert( vec2( 1.0f, 2.0f ) == vec2d( 1.0, 2.0 ));
		assert( vec2( 1.0f, 2.0f ) != vec2d( 1.0, 1.0 ));

		assert( vec3( 1.0f, 2.0f, 3.0f ) == vec3( 1.0f, 2.0f, 3.0f ));
		assert( vec3( 1.0f, 2.0f, 3.0f ) != vec3( 1.0f, 2.0f, 2.0f ));
		assert( vec3( 1.0f, 2.0f, 3.0f ) == vec3d( 1.0, 2.0, 3.0 ));
		assert( vec3( 1.0f, 2.0f, 3.0f ) != vec3d( 1.0, 2.0, 2.0 ));

		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ) == vec4( 1.0f, 2.0f, 3.0f, 4.0f ));
		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ) != vec4( 1.0f, 2.0f, 3.0f, 3.0f ));
		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ) == vec4d( 1.0, 2.0, 3.0, 4.0 ));
		assert( vec4( 1.0f, 2.0f, 3.0f, 4.0f ) != vec4d( 1.0, 2.0, 3.0, 3.0 ));

		assert( !( vec4( float.nan )));
		if ( vec4( 1.0f )) {}
		else {  assert( false );  }
	}
}



/////////////////////////////////
// free functions akin to glsl //
/////////////////////////////////

@safe pure nothrow @nogc:

/// Vector dot product
genType.valueType dot( genType )( in genType a, in genType b ) if ( isVector!genType )  {
	static if ( !isFloatingPoint!( genType.valueType ) && genType.valueType.sizeof < 32 )  {
		genType.valueType result = cast( genType.valueType )( a.data[ 0 ] * b.data[ 0 ] + a.data[ 1 ] * b.data[ 1 ] );
		static if ( genType.dimension >= 3 ) { result += cast( genType.valueType )( a.data[ 2 ] * b.data[ 2 ] ); }
		static if ( genType.dimension == 4 ) { result += cast( genType.valueType )( a.data[ 3 ] * b.data[ 3 ] ); }
		return result;
	} else {
		genType.valueType result = a.data[ 0 ] * b.data[ 0 ] + a.data[ 1 ] * b.data[ 1 ];
		static if ( genType.dimension >= 3 ) { result += a.data[ 2 ] * b.data[ 2 ]; }
		static if ( genType.dimension == 4 ) { result += a.data[ 3 ] * b.data[ 3 ]; }
		return result;
	}
}


/// Vector cross product
genType cross( genType )( in genType a, in genType b ) if ( isVector!genType && ( genType.dimension == 3 ))  {
   return genType( a.y * b.z - b.y * a.z,
				   a.z * b.x - b.z * a.x,
				   a.x * b.y - b.x * a.y );
}


/// Vector length floating point valueType
auto length( genType )( in genType v ) if ( isVector!genType )  {
	static if ( isFloatingPoint!( genType.valueType ))	return sqrt( dot( v, v ));
	else												return sqrt( cast( real )dot( v, v ));
}


/// Vector normalize
genType normalize( genType )( in genType v ) if ( isVector!genType )  {
	auto l = v.length;
	if ( l == 0 )  return v;
	auto invLength = 1.0 / l;
	genType result = v;
	result.data[0] *= invLength;
	result.data[1] *= invLength;
	static if( genType.dimension >= 3 )  {  result.data[2] *= invLength;  }
	static if( genType.dimension == 4 )  {  result.data[3] *= invLength;  }
	return result;
}


/// Distance between two vectors
genType.valueType distance( genType )( const genType a, const genType b ) if ( isVector!genType ) {
	return length( a - b );
}


/// Flip the Vector N based on an incident vector I and a reference Vector Nref
genType faceforward( genType )( in genType N, in genType I, in genType Nref ) if ( isVector!genType )  {
	return  dot( Nref, I ) < 0 ? N : -N;
}


/// Reflect the Vector I on a plane with normal N
/// The normal N must already to be normalized
genType reflect( genType )( in genType I, in genType N ) if ( isVector!genType )  {
	return I - 2 * dot( N, I ) * N;
}


/// For the incident vector I and surface normal N, and the ratio of indices of refraction eta, return the refraction vector
/// The input parameters for the incident vector I and the surface normal N must already be normalized
genType.valueType refract( genType )( genType I, genType N, genType.valueType eta ) if ( isVector!genType )  {
	auto dotNI = dot( N, I );
	auto k = 1.0 - eta * eta * ( 1.0 - dotNI * dotNI );
	if ( k < 0.0 ) return 0.0;
	return eta * I - ( eta * dotNI + sqrt( k )) * N;
}


/// Unittest Geometric functions
/// TODO : add tests for faceforward, reflect and refract
unittest  {

	// dot
	vec2 v2 = vec2( 1.0f,  3.0f );
	assert( dot( v2, vec2( 2.0f, 2.0f )) == 8.0f );

	vec3 v3 = vec3( 1.0f,  3.0f, 5.0f );
	assert( dot( v3, vec3( 2.0f, 2.0f, 2.0f )) == 18.0f );

	vec4 v4 = vec4( 1.0f,  3.0f, 5.0f, 7.0f );
	assert( dot( v4, vec4( 2.0f, 2.0f, 2.0f, 2.0f )) == 32.0f );

	vec3 v3_1 = vec3( 1.0f, 2.0f, -3.0f );
	vec3 v3_2 = vec3( 1.0f, 3.0f,  2.0f );

	assert( dot( v3_1, v3_2 ) == 1.0f );
	assert( dot( v3_1, v3_2 ) == dot( v3_2, v3_1 ));
	assert( v3_1 * v3_2 == v3_1 * v3_2 );
	assert( v3_1 * v3_2 == vec3( 1.0f, 6.0f, -6.0f ));

	// cross
	assert( cross( v3_1, v3_2 ).data == [ 13.0f, -5.0f, 1.0f ] );
	assert( cross( v3_2, v3_1 ).data == [ -13.0f, 5.0f, -1.0f ] );

	// normalize
	assert( normalize( vec2( 1 )) == [ 1.0f / sqrt( 2.0f ), 1.0f / sqrt( 2.0f ) ] );
	assert( vec3( 1 ).normalize   == [ 1.0f / sqrt( 3.0f ), 1.0f / sqrt( 3.0f ), 1.0f / sqrt( 3.0f ) ] );
	assert( normalize( vec4( 1 )) == [ 0.5, 0.5, 0.5, 0.5 ] );

	// length
	assert( length( v2 ) == sqrt( 10.0f ));
	assert( v3.length    == sqrt( 35.0f ));
	assert( length( v4 ) == sqrt( 84.0f ));

	// distance
	assert( distance( vec2( 0.0f, 0.0f ), vec2( 0.0f, 10.0f )) == 10.0 );
}


/// query if any entry is nan
alias isNaN = isnan;	// as std.math.isNaN
bool isnan( genType )( const ref genType vec ) if( isVector!genType && isFloatingPoint!( genType.valueType )) {
	import std.math : isNaN;
	foreach( const ref val; vec )
		if( std.math.isNaN( val ))
			return true;
	return false;
}


/// query if any entry is inf
alias isInfinity = isinf;	// as std.math.isInfinity
bool isinf( genType )( const ref genType vec ) if( isVector!genType && isFloatingPoint!( genType.valueType )) {
	import std.math : isInfinity;
	foreach( const ref val; vec )
		if( std.math.isInfinity( val ))
			return true;
	return false;
}


/// query if all entries are not nan and not inf
alias isValid = isvalid;	// consistency
bool isvalid( genType )( const ref genType vec ) if( isVector!genType && isFloatingPoint!( genType.valueType )) {
	return !( vec.isinf || vec.isnan );
}


unittest {		/// Matrix.isvalid
/*
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

	mat4 m4 = mat4( vec4( 	1.0f, 1.0f, 1.0f, 1.0f ),
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
*/
}




	// this is another variant based on opDispatch, source: http://www.mmartins.me/view/2015/9/27/vector-swizzle-in-d
	// drawback is that it is always recompiled if any swizzle operator changes, implementation at bottom of file
/*	private enum vec_swizz_get_xyzw = "xyzw";
	private enum vec_swizz_get_rgba = "rgba";
	private enum vec_swizz_get_stpq = "stpq";
	@property auto opDispatch(string swizzle)() if (swizzle.length > 0 && swizzle.length < 5) {
		import std.string : indexOf, join;

		static if (swizzle.length == 1) {
			pragma( msg, "length = 1" );
			static if		(vec_swizz_get_xyzw.indexOf( swizzle[0] ) >= 0 )
				enum index = vec_swizz_get_xyzw.indexOf( swizzle[0] );
			else static if	(vec_swizz_get_rgba.indexOf( swizzle[0] ) >= 0 )
				enum index = vec_swizz_get_rgba.indexOf( swizzle[0] );
			else static if	(vec_swizz_get_stpq.indexOf( swizzle[0] ) >= 0 )
				enum index = vec_swizz_get_stpq.indexOf( swizzle[0] );
			else {
				char[128] formatBuffer;
				//auto formatString = sformat( formatBuffer, "Invalid swizzle property: %s", swizzle );
				static assert( false, sformat( formatBuffer, "Invalid swizzle property: %s", swizzle ) );
			}
			return data[index];
		} else {
			import std.conv : to;
			import std.array : array;
			import std.algorithm : map;
			pragma( msg, "length > 1, ", swizzle );
			static if		(vec_swizz_get_xyzw.indexOf(swizzle[0]) >= 0) {
				pragma( msg, vec_swizz_get_xyzw );
				enum indices = swizzle.map!(x => vec_swizz_get_xyzw.indexOf(x)).array;
				enum args = "Vector!(valueType, swizzle.length)(" ~ indices.map!(x => "data[" ~ x.to!string ~ "]").join(",") ~ ")";
				pragma( msg, to!string( indices ) );
				return mixin(args);
			} else static if(vec_swizz_get_rgba.indexOf(swizzle[0]) >= 0) {
				pragma( msg, vec_swizz_get_rgba );
				enum indices = swizzle.map!(x => vec_swizz_get_rgba.indexOf(x)).array;
				enum args = "Vector!(valueType, swizzle.length)(" ~ indices.map!(x => "data[" ~ x.to!string ~ "]").join(",") ~ ")";
				return mixin(args);
			} else static if(vec_swizz_get_stpq.indexOf(swizzle[0]) >= 0) {
				pragma( msg, vec_swizz_get_stpq );
				enum indices = swizzle.map!(x => vec_swizz_get_stpq.indexOf(x)).array;
				enum args = "Vector!(valueType, swizzle.length)(" ~ indices.map!(x => "data[" ~ x.to!string ~ "]").join(",") ~ ")";
				return mixin(args);
			} else {
				char[128] formatBuffer;
				//auto formatString = sformat( formatBuffer, "Invalid swizzle property: %s", swizzle );
				static assert( false, sformat( formatBuffer, "Invalid swizzle property: %s", swizzle ) );
			}

			//pragma( msg, args );
			//return mixin(args);
		}
	}

*/