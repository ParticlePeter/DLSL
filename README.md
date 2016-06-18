# DLSL
A variant of glsl for DLang, based on [gl3n](https://github.com/Dav1dde/gl3n) but tries to be closer to glsl

Differences to gl3n similar to glsl
-----------------------------------

* vector aliases follow glsl, e.g. ivec3 instead of vec3i
* vector swizzle operators on left- and right hand side of assignment
* swizzling is limited to four property letters
* swizzle sets {x,y,z,w}, {r,g,b,a} and {s,t,p,q} are distinct and cannot be combined (e.g. v.rx is not possible)
* matrices are row major, besides following glsl its easier to upload a struct of matrices into an UBO
* matrix is an array of vectors, access matrix column vector with index and field/swizzle properties (e.g. mat[2].zx)


Differences to glsl similar to gl3n
-----------------------------------

* matrix determinants and inversions
* creating and editing matrices with translate, rotate, scale
* creating orthographic and projection matrices (later case one param less than gl3n's)
* quaternions (soon to come)

