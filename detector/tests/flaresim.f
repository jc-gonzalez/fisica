	  program flaresim
c-- Written by O.C. de Jager

	  implicit real*8 (a-h,o-z)
	  real*4 ran1
	  dimension f(1000),t(1000),tsim(1000),fsim(1000)
	  dimension fg(1000),tg(1000),ef(1000),gemp(100),varp(100)

	  open(1,file='/home/okkie/for/data/alldata.dat')
	  open(8,file='/home/okkie/for/data/mknsim1.dat')
	  open(2,file='/home/okkie/for/data/flares6.dat')
	  idum=32875
	  n=158
	  tpi=2d0*3.1415926535d0
	  rate=4d0
	  do i=1,n
		read(1,*)t(i),f(i),ef(i)
	  enddo
	  do kf=1,98
		gemp(kf)=0d0
		varp(kf)=0d0
	  enddo
	  do 12 jsim=1,1000
		print*,jsim
		tsim(1)=t(1)
		tsim(n)=t(n)
		do j=1,1000
		  tsim(j)=t(1)+dfloat(j-1)*(t(n)-t(1))/1000d0
		  fsim(j)=0.2d0
		enddo
		tp=t(1)
		do k=1,1000
		  r=ran1(idum)
		  dt=-rate*dlog(r)
		  tp=tp+dt
		  if(tp.gt.t(n))goto 88
		  r=ran1(idum)
		  fp=2d0+(8d0-1d0)*r*0d0
		  r=ran1(idum)
		  corr1=r*(2d0-0.5d0)+0.5d0
		  r=ran1(idum)
		  corr2=r*(2d0-0.5d0)+0.5d0
		  corr1=.6d0
		  corr2=.6d0
		  jmin=1000d0*(tp-4d0*corr1-t(1))/(t(n)-t(1))
		  jmax=1000d0*(tp+4d0*corr2-t(1))/(t(n)-t(1))
		  jmp=1000d0*(tp-t(1))/(t(n)-t(1))
		  if(jmin.lt.1)jmin=1
		  if(jmax.gt.1000)jmax=1000
		  do j=jmin,jmp
			delt=dabs(tsim(j)-tp)/corr1
			fcor=fp*dexp(-delt)
			fsim(j)=fsim(j)+fcor
		  enddo
		  do j=jmp+1,jmax
			delt=dabs(tsim(j)-tp)/corr2
			fcor=fp*dexp(-delt)
			fsim(j)=fsim(j)+fcor
		  enddo	
		enddo
 88		continue
		do j=1,n
		  in=999*(t(j)-t(1))/(t(n)-t(1))+1
		  tg(j)=t(j)
		  rr=ran1(idum)
		  fg(j)=fsim(in)+2d0*(rr-0.5d0)*ef(j)
		enddo
		df=1d0/(t(n)-t(1))
		do 11 kf=1,98
		  fr=dfloat(kf)*df
		  fd=0d0
		  t0=50615.5d0
		  gemg=0d0
		  ev=0d0
		  do i=1,n
			gemg=gemg+fg(i)/dfloat(n)
			ev=ev+fg(i)*fg(i)/dfloat(n)
		  enddo
		  stg=ev-gemg**2.
		  pg=power(fr,fd,t0,fg,tg,n,gemg,stg,tpi,ccg,ssg,hg)
		  gemp(kf)=gemp(kf)+pg
		  varp(kf)=varp(kf)+pg*pg
 11		continue
 12	  continue
	  do 13 kf=1,98
		gemp(kf)=gemp(kf)/1000d0
		stanp=varp(kf)/1000d0
		stp=dsqrt(stanp-gemp(kf)*gemp(kf))	
		write(2,14)kf*df,gemp(kf),stp
 14		format(f8.4,2f6.3)
 13	  continue	
	  stop
	  end	


      function ran1(idum)
      dimension r(97)
      parameter (m1=259200,ia1=7141,ic1=54773,rm1=3.8580247e-6)
      parameter (m2=134456,ia2=8121,ic2=28411,rm2=7.4373773e-6)
      parameter (m3=243000,ia3=4561,ic3=51349)
      data iff /0/
      if (idum.lt.0.or.iff.eq.0) then
		iff=1
		ix1=mod(ic1-idum,m1)
		ix1=mod(ia1*ix1+ic1,m1)
		ix2=mod(ix1,m2)
		ix1=mod(ia1*ix1+ic1,m1)
		ix3=mod(ix1,m3)
		do 11 j=1,97
		  ix1=mod(ia1*ix1+ic1,m1)
		  ix2=mod(ia2*ix2+ic2,m2)
		  r(j)=(float(ix1)+float(ix2)*rm2)*rm1
 11		continue
		idum=1
      endif
      ix1=mod(ia1*ix1+ic1,m1)
      ix2=mod(ia2*ix2+ic2,m2)
      ix3=mod(ia3*ix3+ic3,m3)
      j=1+(97*ix3)/m3
      if(j.gt.97.or.j.lt.1)pause
      ran1=r(j)
      r(j)=(float(ix1)+float(ix2)*rm2)*rm1
      return
      end


      function power(f,fd,t0,fl,t,n,gem,var,tpi,c,s,ho)
      implicit real*8 (a-h,o-z)
      dimension fl(1000),t(1000)
      c=0d0
      s=0d0
      rn=dfloat(n)
      do i=1,n
		dt=t(i)-t0
		ph=f*dt+0.5d0*fd*dt*dt
		nph=ph
		phi=ph-dfloat(nph)
		if(phi.lt.0d0)phi=phi+1d0
		c=c+(fl(i)-gem)*dcos(phi*tpi)
		s=s+(fl(i)-gem)*dsin(phi*tpi)
      enddo
      power=(c*c+s*s)/(rn*var)
	  cg=dsqrt(2d0/(rn*var))*c
	  ho=datan(s/c)/tpi
	  if(s.lt.0d0.and.c.ge.0d0)ho=ho+1d0
	  if(c.lt.0d0)ho=ho+0.5d0
      return
      end


	  function gauss(z0)
	  implicit real*8 (a-h,o-z)
	  tpi=2d0*3.1415926535d0
	  som=0d0
	  dz=0.01d0
	  do i=0,10000
		z=z0+dz*dfloat(i)
		som=som+dexp(-0.5d0*z*z)
	  enddo
	  prob=som*dz/dsqrt(tpi)
	  gauss=-dlog10(prob)
	  return
	  end
