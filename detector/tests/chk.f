      program chk

      real w, h, t
      real tr

      do 10 w=295.,601.,10.
      
c      w=323.

        do 20 h=2.2,62.3,1.

          call attenu(w, h*100000., 0., tr, trr, trm, tro)
          
          write(*,99) w,h*100000.,tr, trr, trm, tro

 20     continue
        
 10   continue

 99   format(6(1X,E12.4))

      stop
      end

	
