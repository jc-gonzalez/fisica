      program tsttrig

      integer pi(7),pj(7)
      real px(7),py(7),ph(7),ap
      integer neig(1,6),ix(7),iy(7)
      integer si,sj,sai,saj,sa,s

      data (neig(1,i),i=1,6) / 2,3,4,5,6,7 /

      open(unit=21,file='prueba.pix1',status='OLD')
      do 5 i=1,7
        read(21,*) n,pi(i),pj(i),px(i),py(i),ph(i)
        write(*,*) n,px(i),py(i),ph(i)
 5    continue
      close(21)

      ap = px(2)/2

      write(*,91) ph(4),ph(3)      
      write(*,92) ph(5),ph(1),ph(2)
      write(*,91) ph(6),ph(7)

      print *,ap
      nent = 0
      do 10 i=1,6
        if (ph(neig(1,i)).gt.0.) then
          ix(neig(1,i)) = nint(px(neig(1,i))/ap*1.1)
          iy(neig(1,i)) = nint(py(neig(1,i))/ap)
          nent = nent+1
          si = si + pi(i)
          sj = sj + pj(i)
          sai = sai + abs(pi(i))
          saj = saj + abs(pj(i))
        else
          ix(neig(1,i)) = 0
          iy(neig(1,i)) = 0
        endif
 10   continue

      sa = sai + saj
      s = si + sj

      write(*,93) ix(4),iy(4),ix(3),iy(3)
      write(*,94) ix(5),iy(5),ix(1),iy(1),ix(2),iy(2)
      write(*,93) ix(6),iy(6),ix(7),iy(7)

      mix = 0
      miy = 0
      do 20 i=1,7
        mix = mix+ix(i)
        miy = miy+iy(i)
 20   continue
      
      write(*,95) nent
      
      if (nent.lt.3) then
        write(*,*) 'NO hay trigger.'
        goto 99
      elseif (nent.gt.4) then
        write(*,*) 'TRIGGER Impepinable.'
        goto 99
      else 
        write(*,*) 'Buscando trigger...'
      endif

      write(*,*) mix,miy
      
 40   if ((abs(mix)+abs(miy).gt.2).or.
     $    ((mix.eq.0).and.(miy.eq.0))) then
        write(*,*) 'PRESUNTO TRIGGER NO encontrado.'
      else
        write(*,*) 'PRESUNTO TRIGGER encontrado.'
      endif

      print *,'# ',si,sj,s,sai,saj,sa

 91   format(9X,2(F4.1,2X))
 92   format(6X,3(F4.1,2X))
 93   format(12X,2(I3,I3,4X))
 94   format(6X,3(I3,I3,4X))
 95   format(' Hay ',I4,' pixels vecinos distintos de cero.')

 99   stop
      end
