c **********************************************************************
c
	SUBROUTINE Ctracking(x,y,u,v,track_length,nreflex)
c
c subroutine to track a Cerenkov photon in a C-hut to the PM
c
c-----------------------------------------------------------------------
	common /Chutgeom/ r(0:3),zu(0:3),ta(3),ca(3),nz(3),zd(3)
	real nx,ny,nz,ni0
	common /sphere_PM/ r_sph,zu_sph,sph1,sph2
	common /st_line/ a2,b2,c2
	double precision a2 , b2 , c2
	double precision a , b , c , discrim
	double precision ax , bx , ay , by
	double precision u0 , v0 , w0 , u1 , v1 , w1
	double precision norm
c
	data epsilon / 1e-3 / 
	radius = sqrt( x ** 2 + y ** 2 )
c
	if ( radius . gt . r(3) ) then 
		track_length = 0.
		return
	endif
c
	uv2 = u ** 2 + v ** 2
	if ( uv2 . gt . 1. ) then
		track_length = -1.
		return
	endif
c
	tl = 0.
	nreflex = 0
	ic = 3
	x0 = x
	y0 = y
	u0 = u
	v0 = v
	z0 = zu(3)
	w0 = - dsqrt( 1. - u0 ** 2 - v0 ** 2 )
c
10	ax = x0 - u0 * z0 / w0
	ay = y0 - v0 * z0 / w0
	bx = u0 / w0
	by = v0 / w0
	a2 = bx ** 2 + by ** 2
	b2 = 2 * ( ax * bx + ay * by )
	c2 = ax ** 2 + ay ** 2
	call hit_sphere(zu_sph,zd_sph)
	if ( zu_sph . ge . 0. ) then
		if ( z0 . ge . zu_sph ) then
			z1 = zu_sph
		else
			z1 = zd_sph
		endif		
		x1 = ax + bx * z1
		y1 = ay + by * z1
		dx = x1 - x0
		dy = y1 - y0
		dz = z1 - z0
		step = sqrt ( dx ** 2 + dy ** 2 + dz ** 2 )
		track_length = tl + step
		return
	endif
c
	do i = ic , 1 ,-1
		a = (1d0*ta(i)) ** 2 - a2
		b = 2 * ( (1d0*ta(i)) ** 2 ) * zd(i) - b2
		c = ( 1d0 * ta(i) * zd(i) ) ** 2 - c2
	if ( a . ne . 0. ) then
		discrim = dsqrt ( b ** 2 - 4 * a * c )
		z2 = ( - b + discrim ) / ( 2 * a ) 
		z3 = ( - b - discrim ) / ( 2 * a ) 
		z1 = amin1 ( z2 , z3 )
		if ( z1 . lt . - zd(i) ) z1 = amax1 ( z2 , z3 )
	else
		z1 = - c / b
	endif
	if ( (i . gt . 1) . and . (z1 . gt . zu(i-1)) ) then
		ic = i
		goto 20
	endif
	enddo
c
	ic = 1	
20	ratio = abs ( z1 - z0 ) / r(ic-1)
c
c	check if C-photon is moving out of the hut
c
	if ( (ratio . lt . epsilon) . and . (nreflex . gt . 0) ) then
		track_length = 0.
		return
	endif
c
	x1 = ax + bx * z1
	y1 = ay + by * z1
	r1 = sqrt ( x1 ** 2 + y1 ** 2 )
	dx = x1 - x0
	dy = y1 - y0
	dz = z1 - z0
	step = sqrt ( dx ** 2 + dy ** 2 + dz ** 2 )
	tl = tl + step
	nx = ( - x1 / r1 ) * ca(ic)
	ny = ( - y1 / r1 ) * ca(ic)
	ni0 = nx * u0 + ny * v0 + nz(ic) * w0
	u1 = 2 * ni0 * nx - u0
	v1 = 2 * ni0 * ny - v0
	w1 = 2 * ni0 * nz(ic) - w0
	x0 = x1
	y0 = y1
	z0 = z1
	u0 = u1
	v0 = v1
	w0 = w1
	norm = dsqrt ( u0 ** 2 + v0 ** 2 + w0 ** 2 )
	u0 = u0 / norm
	v0 = v0 / norm
	w0 = w0 / norm
	if ( dabs ( w0 ) . lt . 1d-5 ) then
		track_length = 0.
		return
	endif
	nreflex = nreflex + 1
	goto 10
	end
c
c **********************************************************************
c
	subroutine hit_sphere(z_u,z_d)
c
c subroutine to calculate the meeting point of the straight path
c of the C-photon with the photocathode of the (spherical) PM 
c
 	common /sphere_PM/ r_sph,zu_sph,sph1,sph2
	common /st_line/ a2,b2,c2
	double precision a2 , b2 , c2
	double precision a , b , c
	a = 1. + a2
	b = sph1 + b2
	c = sph2 + c2
	disc = b ** 2 - 4 * a * c
	if ( disc . ge . 0. ) then
		z_u = ( - b + sqrt ( disc ) ) / ( 2 * a ) 
		z_d = ( - b - sqrt ( disc ) ) / ( 2 * a ) 
	else
		z_u = -1.
		z_d = -1.
	endif
	return
	end
c
c **********************************************************************
