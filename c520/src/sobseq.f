      SUBROUTINE SOBSEQ(N,X)
 
C-----------------------------------------------------------------------
C  SOB(OL) SEQ(UENCE)
C
C  SOBOL QUASI RANDOM NUMBER GENERATOR
C  REFERENCE : NUMERICAL RECIPES, W.H. PRESS ET AL.,
C              CAMBRIDGE UNIVERSITY PRESS, 1992  ISBN 0 521 43064 X
C  THIS SUBROUTINE IS CALLED FROM SELCOR
C-----------------------------------------------------------------------
 
      INTEGER     N,MAXBIT,MAXDIM
      REAL        X(*),FAC
      PARAMETER   (MAXBIT=30,MAXDIM=6)
      INTEGER     I,IM,IN,IPP,J,K,L,IP(MAXDIM),IU(MAXDIM,MAXBIT),
     *            IV(MAXBIT*MAXDIM),IX(MAXDIM),MDEG(MAXDIM)
      SAVE        IP,MDEG,IX,IV,IN,FAC
      EQUIVALENCE (IV,IU)
      DATA IP /0,1,1,2,1,4/, MDEG /1,2,3,3,4,4/, IX /6*0/
      DATA IV /6*1,3,1,3,3,1,1,5,7,7,3,3,5,15,11,5,15,13,9,156*0/
C-----------------------------------------------------------------------
 
      IF (N.LT.0) THEN
        DO 14 K=1,MAXDIM
          DO 11 J=1,MDEG(K)
            IU(K,J)=IU(K,J)*2**(MAXBIT-J)
 11       CONTINUE
          DO 13 J=MDEG(K)+1,MAXBIT
            IPP=IP(K)
            I=IU(K,J-MDEG(K))
            I=IEOR(I,I/2**MDEG(K))
            DO 12 L=MDEG(K)-1,1,-1
              IF(IAND(IPP,1).NE.0)I=IEOR(I,IU(K,J-L))
              IPP=IPP/2
 12         CONTINUE
            IU(K,J)=I
 13       CONTINUE
 14     CONTINUE
        FAC=1./2.**MAXBIT
        IN=0
      ELSE
        IM=IN
        DO 15 J=1,MAXBIT
          IF(IAND(IM,1).EQ.0)GOTO 1
          IM=IM/2
 15     CONTINUE
        PAUSE 'MAXBIT TOO SMALL IN SOBSEQ'
 1      IM=(J-1)*MAXDIM
        DO 16 K=1,MIN(N,MAXDIM)
          IX(K)=IEOR(IX(K),IV(IM+K))
          X(K)=IX(K)*FAC
 16     CONTINUE
        IN=IN+1
      ENDIF
      RETURN
      END
