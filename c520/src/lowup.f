      SUBROUTINE LOWUP(CHAR)
 
C-----------------------------------------------------------------------
C  (CONVERTS) LOW(ER CASE CHARACTER TO) UP(PPER CASE CHARACTER)
C
C  THIS SUBROUTINE IS CALLED FROM DATAC
C  ARGUMENT :
C   CHAR    =  CHARACTER TO BE CONVERTED
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
      CHARACTER*1 CHAR
C-----------------------------------------------------------------------
 
      IF     ( CHAR .EQ. 'a' ) THEN
        CHAR='A'
      ELSEIF ( CHAR .EQ. 'b' ) THEN
        CHAR='B'
      ELSEIF ( CHAR .EQ. 'c' ) THEN
        CHAR='C'
      ELSEIF ( CHAR .EQ. 'd' ) THEN
        CHAR='D'
      ELSEIF ( CHAR .EQ. 'e' ) THEN
        CHAR='E'
      ELSEIF ( CHAR .EQ. 'f' ) THEN
        CHAR='F'
      ELSEIF ( CHAR .EQ. 'g' ) THEN
        CHAR='G'
      ELSEIF ( CHAR .EQ. 'h' ) THEN
        CHAR='H'
      ELSEIF ( CHAR .EQ. 'i' ) THEN
        CHAR='I'
      ELSEIF ( CHAR .EQ. 'j' ) THEN
        CHAR='J'
      ELSEIF ( CHAR .EQ. 'k' ) THEN
        CHAR='K'
      ELSEIF ( CHAR .EQ. 'l' ) THEN
        CHAR='L'
      ELSEIF ( CHAR .EQ. 'm' ) THEN
        CHAR='M'
      ELSEIF ( CHAR .EQ. 'n' ) THEN
        CHAR='N'
      ELSEIF ( CHAR .EQ. 'o' ) THEN
        CHAR='O'
      ELSEIF ( CHAR .EQ. 'p' ) THEN
        CHAR='P'
      ELSEIF ( CHAR .EQ. 'q' ) THEN
        CHAR='Q'
      ELSEIF ( CHAR .EQ. 'r' ) THEN
        CHAR='R'
      ELSEIF ( CHAR .EQ. 's' ) THEN
        CHAR='S'
      ELSEIF ( CHAR .EQ. 't' ) THEN
        CHAR='T'
      ELSEIF ( CHAR .EQ. 'u' ) THEN
        CHAR='U'
      ELSEIF ( CHAR .EQ. 'v' ) THEN
        CHAR='V'
      ELSEIF ( CHAR .EQ. 'w' ) THEN
        CHAR='W'
      ELSEIF ( CHAR .EQ. 'x' ) THEN
        CHAR='X'
      ELSEIF ( CHAR .EQ. 'y' ) THEN
        CHAR='Y'
      ELSEIF ( CHAR .EQ. 'z' ) THEN
        CHAR='Z'
      ENDIF
 
      RETURN
      END
