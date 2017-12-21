/**
dlsl.projection


Authors: Peter Particle ( based on gl3n by David Herberth )
License: MIT

Note: All methods marked with pure are weakly pure since, they all access an instance member.
All static methods are strongly pure.
*/

module dlsl.projection;

public import dlsl.matrix;
import dlsl.vector;

import std.math	: sqrt, sin, cos, tan, PI;
import std.traits : isFloatingPoint;


/// http://www.geeks3d.com/20090729/howto-perspective-projection-matrix-in-opengl/


private float[ 6 ] cperspective( float fovy, float aspect, float near, float far ) {
	float top = near * tan( fovy * ( PI / 360.0 ));
	float bottom = - top;
	float right = top * aspect;
	float left = - right;

	return [ left, right, bottom, top, near, far ];
}

/// Construct a symmetric perspective matrix ( 4x4 and floating - point matrices only ).
mat4 glPerspective( float fovy, float aspect, float near, float far ) {
	float[ 6 ] cdata = cperspective( fovy, aspect, near, far );
	return glPerspective( cdata[ 0 ], cdata[ 1 ], cdata[ 2 ], cdata[ 3 ], cdata[ 4 ], cdata[ 5 ] );
}

/// Construct an optionally non-symmetric perspective matrix 
mat4 glPerspective( float left, float right, float bottom, float top, float near, float far )
in {
	assert( right - left != 0 );
	assert( top - bottom != 0 );
	assert( far - near != 0 );
}
body {
	mat4 result;
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
mat4 glInversePerspective( float fovy, float aspect, float near, float far ) {
	float[ 6 ] cdata = cperspective( fovy, aspect, near, far );
	return glInversePerspective( cdata[ 0 ], cdata[ 1 ], cdata[ 2 ], cdata[ 3 ], cdata[ 4 ], cdata[ 5 ] );
}

/// Construct an inverse, optionally non-symmetric perspective matrix 
mat4 glInversePerspective( float left, float right, float bottom, float top, float near, float far )
in {
	assert( right - left != 0 );
	assert( top - bottom != 0 );
	assert( far - near != 0 );
}
body {
	mat4 result;
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
mat4 glOrthographic( float left, float right, float bottom, float top, float near, float far )
in {
	assert( right - left != 0 );
	assert( top - bottom != 0 );
	assert( far - near != 0 );
}
body {
	mat4 result = void;
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
/// Returns an inverse orographic matrix ( 4x4 and floating - point matrices only ).
mat4 glInverseOrthographic( float left, float right, float bottom, float top, float near, float far ) {
	mat4 result;
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


unittest {
	float aspect = 6.0 / 9.0;              
	float[ 6 ] cp = cperspective( 60f, aspect, 1f, 100f );
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

	float[ 4 ][ 4 ] m4pi = mat4.glInversePerspective( 60.0, aspect, 1.0, 100.0 ).data;
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


////////////////////////
// Vulkan Projections //
////////////////////////

mat4 vkPerspective( float fovy, float aspect, float near, float far ) {
	mat4 result = void;
	result.clear( 0 );

	const float t = 1.0f / tan( 0.5f * fovy * deg2rad );
	const float n_f = near - far;
/*
	result[0][0] = t / aspect;
	result[1][1] = t;
	result[2][2] = - ( near + far ) / n_f;
	result[2][3] = 1;
	result[3][2] = ( 2 * near * far ) / n_f;

	// premultiplying a glProjection with this clip matrix results in expected vkProjection
	// source: https://matthewwellings.com/blog/the-new-vulkan-coordinate-system/
	auto clip = mat4( vec4( 1, 0, 0, 0 ), vec4( 0, -1, 0, 0 ), vec4( 0, 0, 0.5, 0 ), vec4( 0, 0, 0.5, 1 ));
	return clip * result;
*/
	// we can avoid matrix multiply of clip and projection with following variant for the matrix entries
	result[0][0] = t / aspect;
	result[1][1] = - t;
	result[2][2] = 0.5 - 0.5 * ( near + far ) / n_f;
	result[2][3] = 1;
	result[3][2] = near * far / n_f;

	return result;
}
