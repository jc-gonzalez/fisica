      program tsttrig

      integer pi(7),pj(7)
      integer si,sj,sai,saj,sa,s
      integer t(7,50),tr(12)
      real mi,mj

      data (pi(i),i=1,7) / 0,1,0,-1,-1,0,1 /
      data (pj(i),i=1,7) / 0,0,1,1,0,-1,-1 /
      
      data (tr(j),j=1,12) / 1,1,1,1,1,1,0,0,0,0,0,0 /
      data ((t(i,j),i=1,7),j=1,12)
     +    /
     +    
     +    1,1,1,1,0,0,0,
     +    1,0,1,1,1,0,0,
     +    1,0,0,1,1,1,0,
     +    1,0,0,0,1,1,1,
     +    1,1,0,0,0,1,1,
     +    1,1,1,0,0,0,1,
     +
     +    1,1,1,0,1,0,0,
     +    1,0,1,1,0,1,0,
     +    1,0,0,1,1,0,1,
     +    1,1,0,0,1,1,0,
     +    1,0,1,0,0,1,1,
     +    1,1,0,1,0,0,1
     +
     +    /

      do 999 j=1,12



c        write(*,91) t(4,j),t(3,j)      
c        write(*,92) t(5,j),t(1,j),t(2,j)
c        write(*,91) t(6,j),t(7,j)

        si = 0
        sj = 0
        mi = 1
        mj = 1
        sai = 0
        saj = 0
        
        do 10 i=1,6
          if (t(i,j).gt.0) then
            si = si + pi(i)
            sj = sj + pj(i)
            sai = sai + abs(pi(i))
            saj = saj + abs(pj(i))
            mi = mi*pi(i)
            mj = mj*pj(i)
          endif
 10     continue

        sa = sai + saj
        s = si + sj

        write(*,96) tr(j),(t(i,j),i=1,7),si,sj,s,sai,saj,sa,mi,mj

 999  continue

 91   format(9X,2(I4,2X))
 92   format(6X,3(I4,2X))
 93   format(12X,2(I3,I3,4X))
 94   format(6X,3(I3,I3,4X))
 95   format(' Hay ',I4,' pixels vecinos distintos de cero.')
 96   format('# ',20(I4))


 99   stop
      end
