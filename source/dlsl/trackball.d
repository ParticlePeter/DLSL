/**
dlsl.trackball


Authors: Peter Particle
License: MIT

Note: All methods marked with pure are weakly pure since, they all access an instance member.
All static methods are strongly pure.
*/

module dlsl.trackball;

import dlsl.vector;
import dlsl.matrix;
import dlsl.quaternion;

const float	deg2rad	= 0.0174532925199432957692369076849f;
const float	rad2deg	= 57.295779513082320876798154814105f;




struct Trackball {
nothrow:
private:
	float	m_phi			= 0.0f;
	float	m_theta			= 0.0f;
	float	m_dolly			= 10.0f;

	float	m_velXform		= 1;
	float	m_velOrbit		= 1;
	float	m_velDolly		= 1;

	float	m_absX 			= 0.0f;
	float	m_absY			= 0.0f;   

	mat4	m_matrix 		= mat4.identity;

	vec3 	m_target 		= vec3( 0 );
	float	m_recipFocus	= 1.0f;
	float	m_recipHeight	= 0.01f;
	bool	m_dirty			= false;	

	void abs2rel( ref float x, ref float y ) {
		float ax = m_absX;
		float ay = m_absY;
		m_absX = x;
		m_absY = y;
		x = m_absX - ax;
		y = m_absY - ay;
		m_dirty = true;
	}

	void update() {
		m_matrix
			= mat4.translation( 0, 0, m_dolly )
			* mat4.rotationX( - deg2rad * m_theta ) 
			* mat4.rotationY( - deg2rad * m_phi )
			* mat4.translation( m_target );
	}


public:
	this( vec3 eye, vec3 target = vec3( 0 ), vec3 up = vec3( 0, 1, 0 )) {
		lookAt( eye, target, up );
	}

	void xformVelocity( float vel )		{ m_velXform  = vel; }
	void orbitVelocity( float vel )		{ m_velOrbit = vel; }
	void dollyVelocity( float vel )		{ m_velDolly = vel; }
	mat4 matrix() 						{ m_dirty = false; return m_matrix; }
	void focus(  float f )				{ m_recipFocus  = 1 / f; }
	void height( float h )				{ m_recipHeight = 2 / h; }
	bool dirty()  { return m_dirty; }
	//void dirty( bool b ) { m_dirty = b; }

	void height_and_focus_from_fovy( float height, float fovy ) {
		import std.math : tan;
		m_recipHeight	= 2 / height;
		m_recipFocus	= - 4 * tan( 0.5f * fovy ) / height;
	}

	void reference( float x, float y )	{
		m_absX = x;
		m_absY = y;
	}		// set 2D reference point for each navigation gesture (e.g. any click)

	mat4 viewMatrix() {
		m_dirty = false; return m_matrix; 
	}


	void orbit( float x, float y ) {
		abs2rel( x, y );
		m_phi += m_velOrbit * x;
		m_theta += m_velOrbit * y;
		update;
	}


	void xform( float x, float y ) {
		abs2rel( x, y );
		//float d = abs( camDolly );	/// Tweak the distance to Rotation center, this Z Value gets rotated by Rotation Matrix
		/// Use X and Y Vectors from Rotation Matrix to calculate Camera Offset Parallel to Screen. Both use camRes.x, otherwise different speed in x and y !
		m_target += m_velXform * m_recipFocus * m_recipHeight * m_dolly * ( x * vec3( m_matrix[0].x, m_matrix[1].x, m_matrix[2].x ) - y * vec3( m_matrix[0].y, m_matrix[1].y, m_matrix[2].y )); // Both use camRes.x, otherwise different speed in x and y
		update;
	}


	void dolly( float x, float y ) {
		abs2rel( x, y );
		float d  = m_dolly;
		m_dolly -= m_velDolly * m_recipHeight * m_recipFocus * m_dolly * ( x + y );
		if ( m_dolly < 0.001f ) m_dolly = 0.001f;
		update;
		//m_matrix[ 3 ].xyz = m_matrix[ 3 ].xyz * ( m_dolly - dolly );
	}

	void dolly( float d ) {
		m_dolly = d < 0.001f ? 0.001f : d;
		m_dirty = true;
	}

	float  dolly() const {
		return m_dolly;
	}


	/// look at function with two points and an up vector
	void lookAt( vec3 eye, vec3 target = vec3( 0 ), vec3 up = vec3( 0, 1, 0 )) {
		vec3 vecZ	= eye - target;	// vector from target to eye equals the camera z axis as camera look direction is neagtive z
		m_dolly		= length( eye - target );
		m_target	= target;
		vecZ /= m_dolly;
		import std.math : asin, atan2;
		m_phi = - rad2deg * atan2( vecZ.x(), vecZ.z());
		m_theta = rad2deg * asin( vecZ.y());

		// TODO(pp): compute twist from up vector

		update;
		m_dirty = true; 
	}

	/// look at function with nine floats representing two points and an up vector
	void lookAt( float ex, float ey, float ez, float tx = 0, float ty = 0, float tz = 0, float ux = 0, float uy = 1, float uz = 0 ) {
		this.lookAt( vec3( ex, ey, ez ), vec3( tx, ty, tz ), vec3( ux, uy, uz ));
	}
}


/// look at function with two points and an up vector
auto lookAt( vec3 eye, vec3 target = vec3( 0 ), vec3 up = vec3( 0, 1, 0 )) {
	vec3 vecZ = normalize( eye - target );	// vector from target to eye equals the camera z axis as camera look direction is negative z
	vec3 vecX = normalize( cross( up, vecZ ));

	// TODO( pp ): fix bellow, matrix constructor
	//return mat4( vecX, 0, cross( vecZ, vecX ), 0, vecZ, 0, eye, 1 );
	mat4 result;
	result[0] = vec4( vecX, 0 );
	result[1] = vec4( cross( vecZ, vecX ), 0 );
	result[2] = vec4( vecZ, 0 );
	result[3] = vec4( eye , 1 );

	return result;
 
}

/// look at function with nine floats representing two points and an up vector
auto lookAt( float ex, float ey, float ez, float tx = 0, float ty = 0, float tz = 0, float ux = 0, float uy = 1, float uz = 0 ) {
	return lookAt( vec3( ex, ey, ez ), vec3( tx, ty, tz ), vec3( ux, uy, uz ));
}