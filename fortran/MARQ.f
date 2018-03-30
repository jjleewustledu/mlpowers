*******************************************************************
*                                                                 *
*	>>>>>>>> CHANGE to DOUBLE PRECISION FOR MATRIX CALC 
*                and for calculation of fitted function  
*   
*                compute fitted function in DP so that
*                slope /DERATIVE WILL BE MORE ACCURATE
*                still didn't work well for 5 parameters 
*                so changed matrix calc to DP
*     Joanne Markham  Sept -NOV. 1992 
*
*    JMMARQ.FTN                                                   *
C   WEIGHTED SUM-OF-SQUARES
C   WEIGHTS COMPUTED IN MAIN
*    (MODIFIED FROM RPOMARQ (REV 1 , 2/3/85, BY MARK MINTUN)      *
*                                                                 *
*     THIS SUBROUTINE USES THE MARQUART METHOD OF PARAMETER       *
*    OPTIMIZATION TO SOLVE FOR THE BEST FIT TO A MEASURED         *
*    SET OF FUNCTION VALUES.  THE FUNCTION CAN BE ANY             *
*    WRITTEN MATHEMATICAL RELATION BETWEEN THE PARAMETERS AND     *
*    THE SINGLE SET OF MEASURED VALUES.  NO OTHER RELATION NEED   *
*    BE WRITTEN (i.e. DERIVATIVES).                               *
*                                                                 *
*     I HAVE BEEN UNABLE TO FIND REFERENCE FOR THIS PARTICULAR
*     VERSION OF MARQUARDT'S METHOD  (JOANNE MARKHAM)
*                                                                 *
*   THE ORIGINAL ENCODING OF THE MARQUART METHOD ON PERKIN-       *
*   ELMER MACHINES IN FORTRAN WAS DONE BY.....                    *
*        MONTGOMERY A. MARTIN, 1983                               *
*                                                                 *
*******************************************************************
C

      SUBROUTINE MARQ(NBLD,NPOINT,NPBLOCK,NPARM,ERRSUM,ITER,
     %           ICONV,N2PRINT,TIMBLD,BLD,TIME1,TIME2,PBLOCK,IOPARM,
     %           PARM1,TISACT,TISACT1,TISMISC,WEIGHTS,PBNAME)

      
       DOUBLE PRECISION DEQ1(10,10),DEQ2(10,10),DEQ1A(10),
     1 DEQ2A(10),DEQ1B(10),DEQ2B(10),dtemp,SLOPE(100,10)
   
       double precision dact(100),dact1(100),dactemp(100),dww,dtemp2

      REAL TIMBLD(NBLD),BLD(NBLD),PBLOCK(NPBLOCK),PARM1(NPARM)
      REAL TISACT(NPOINT),TISACT1(NPOINT),TISMISC(NPOINT)
      INTEGER*4 TIME1(NPOINT),TIME2(NPOINT),IOPARM(NPARM)
      CHARACTER*30 PBNAME(20)
       REAL WEIGHTS (100)

      REAL PARM2(10),PARM3(10),TEMP(100),TEMPD(100)
c     REAL CEQ1(6,6),CEQ2(6,6),CEQ1A(6),CEQ1B(6),CEQ2A(6),CEQ2B(6)
      REAL BMAX(10),BMIN(10),PARMSCALE(10),PARMREAL(10),TISMISC1(100)


      DATA BMAX/10*1.E+10/
      DATA BMIN/10*0.0/
      data luerr/6/

       do 2111 i=1,nparm
       if (IOPARM(i).eq.9) then
       BMIN(i) = -200.
       endif
2111   continue

      FV = 0.
      FNU = 0.
      FLA = 0.
      TAU = 0.
      EPS = 0.
      PHMIN = 0.
      ICONV=NPARM
      ITER=0

************ DEBUG INSTRUCTIONS *******************

*     WRITE(12,*)' MARQ SUB ENTERED'
*     WRITE(12,*)' PBLOCK=',(PBLOCK(I),I=1,20)
*     WRITE(12,*)' PARM1 =',(PARM1(I),I=1,NPARM)
*     WRITE(12,*)' TISACT = ',(TISACT(I),I=1,NPOINT)

*****************************************************
C
C
C
      DO 20 I=1,NPARM
      PARMSCALE(I)=PARM1(I)
      IF(PARM1(I).EQ.0.)THEN
      WRITE(luerr,*)' ...ERROR... ONE OF STARTING PARAMETERS = 0.'
      STOP
      ENDIF
20    PARM1(I)=1.

100   IF(ICONV.LE.0)THEN
      DO 110 IPARM=1,NPARM
110   PARM1(IPARM)=PARM1(IPARM)*PARMSCALE(IPARM)
      RETURN
      ENDIF
C
C
      IF( FNU .LE. 0. ) FNU = 10.0
      IF( FLA .LE. 0. ) FLA = 0.01
      IF( TAU .LE. 0. ) TAU = 0.00001
      IF( EPS .LE. 0. ) EPS = 0.00010
      IF ( PHMIN .LE. 0.) PHMIN = 0.

************ DEBUG INSTRUCTIONS *******************

**     WRITE(12,*)' ICONV = ',ICONV
**     WRITE(12,*)' CONV VARIABLES ',FNU,FLA,TAU,EPS,PHMIN

*****************************************************
C
C
C
      ITERNEW=1
530   IF(ITER.GT.0) GOTO 1530
      DO 560 J1 = 1,NPARM
      PARM2(J1)=PARM1(J1)
560   PARM3(J1)=PARM1(J1)+1.0E-02
      GO TO 1030


590   IF (PHMIN .GT. PH .AND. ITER .GT. 1) GO TO 625

*****DEBUG******
**     WRITE(12,*)' BEGIN CALC OF SLOPE'
****************

      DO 620 J1 = 1,NPARM

      DO 606 J2 = 1,NPARM
606   PARM2(J2)=PARM1(J2)
      DEN=0.01*AMAX1(PARM3(J1),ABS(PARM2(J1)))
      IF (PARM2(J1) + DEN .LE. BMAX(J1)) GO TO 55
      PARM2(J1) = PARM2(J1) - DEN
      DEN = -DEN
      GO TO 56
55    PARM2(J1) = PARM2(J1) + DEN
56    DO 57 IPARM=1,NPARM
57    PARMREAL(IPARM)=PARMSCALE(IPARM)*PARM2(IPARM)
      CALL FUNC(NBLD,NPOINT,NPBLOCK,NPARM,PBNAME,N2PRINT,
     %       TIMBLD,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARMREAL,dactemp,
     %       TISMISC1)
      DO 610 J2 = 1,NPOINT
      dtemp = dactemp(j2) -dact1(j2)
      dtemp2 = dact1(j2)
      if (dtemp2.eq.0.) dtemp2 = 1.0
      dtemp2 =  dabs(dtemp/dtemp2)
      if (dtemp2.lt.1.0d-6)dtemp =0.
610   SLOPE(J2,J1)=dtemp/DEN
620   CONTINUE
C
C      SET UP CORRECTION EQUATIONS
C

******DEBUG*******
**     WRITE(12,*)' SET UP CORRRECTION EQS'
*******************

625   DO 725 J1 = 1,NPARM
      DEQ1A(J1)=0.
      DO 640 J2 = 1,NPOINT
      dWW = WEIGHTS(J2)
      dtemp = TISACT(J2) - dACT1(J2)
640   DEQ1A(J1)=DEQ1A(J1)+SLOPE(J2,J1)*DWW*dtemp
      DO 680 J2 = 1,NPARM
      DEQ1(J1,J2)=0.
      DO 680 J3 = 1,NPOINT
680   DEQ1(J1,J2)=DEQ1(J1,J2)+WEIGHTS(J3)*SLOPE(J3,J1)*SLOPE(J3,J2)
      IF(DEQ1(J1,J1).GT.1.D-20) GO TO 725
      DO 694 J2 = 1,NPARM
694   DEQ1(J1,J2) = 0.
      DEQ1A(J1)=0.
      DEQ1(J1,J1) = 1.0D+0
725   CONTINUE

      dtemp =0.
      DO 729 J1 = 1,NPARM
729   dtemp = dtemp + DEQ1A(J1)*DEQ1A(J1)
       GN = dtemp
C
C     SCALE CORRECTION EQUATIONS
C

******DEBUG*******
**     WRITE(12,*)' SCALE CORRECTION EQS'
******************

      DO 726 J1 = 1,NPARM
726   DEQ1B(J1)=DSQRT(DEQ1(J1,J1))
      DO 727 J1=1,NPARM
      DEQ1A(J1)=DEQ1A(J1)/DEQ1B(J1)
      DO 727 J2 = 1,NPARM
727   DEQ1(J1,J2)=DEQ1(J1,J2)/(DEQ1B(J1)*DEQ1B(J2))


      FL = FLA/FNU
      GO TO 810
800   FL = FNU*FL
810   DO 840 J1 = 1,NPARM
      DO 830 J2 = 1,NPARM
830   DEQ2(J1,J2)=DEQ1(J1,J2)
      DEQ2A(J1)=DEQ1A(J1)
840   DEQ2(J1,J1)=DEQ2(J1,J1)+FL
C
C     SOLVE CORRECTION EQUATIONS
C

******DEBUG***********
**     WRITE(12,*)' SOLVE CORRECTION EQS'
*********************

      DO 930 L1 = 1,NPARM
      L2=L1+1
      DO 910 L3 = L2,NPARM
910   DEQ2(L1,L3)=DEQ2(L1,L3)/DEQ2(L1,L1)
      DEQ2A(L1)=DEQ2A(L1)/DEQ2(L1,L1)
      DO 930 L3 = 1,NPARM
      IF(L1.EQ.L3)GOTO 930
      DO 925 L4 = L2,NPARM
925   DEQ2(L3,L4)=DEQ2(L3,L4)-DEQ2(L1,L4)*DEQ2(L3,L1)
      DEQ2A(L3)=DEQ2A(L3)-DEQ2A(L1)*DEQ2(L3,L1)
930   CONTINUE
C
      DN = 0.
      DG = 0.
      DO 1028 J1 = 1,NPARM
      DEQ2B(J1)=DEQ2A(J1)/DEQ1B(J1)
      tempa = deq2b(j1)
      PARM2(J1)=AMAX1(BMIN(J1),AMIN1(BMAX(J1),
     %               (PARM1(J1)+tempa)))
      IF(PARM2(J1).LT.(0.1*PARM1(J1)))THEN
      PARM2(J1)=0.1*PARM1(J1)
      ENDIF
      DG=DG+DEQ2B(J1)*DEQ1A(J1)*DEQ1B(J1)
      DN=DN+DEQ2B(J1)*DEQ2B(J1)
1028  DEQ2B(J1)=PARM2(J1)-PARM1(J1)

      COSG = DG/SQRT (DN*GN)
      JGAM = 0
      IF( COSG ) 1100,1110,1110
1100  JGAM = 2
      COSG = -COSG
1110  CONTINUE
      COSG = AMIN1(COSG, 1.0)
      GAMM = ACOS(COSG)*180./(3.14159265)
      IF( JGAM .GT. 0 ) GAMM = 180. - GAMM
1030  DO 1035 IPARM=1,NPARM
*********************DEBUG*********************
*     WRITE(luerr,*)' DEBUG,1  ',NPARM,IPARM,PARMREAL,PARMSCALE,PARM2
***********************************************
1035  PARMREAL(IPARM)=PARMSCALE(IPARM)*PARM2(IPARM)

*     WRITE(luerr,*)' DUMMY WRITE VARIABLE'
*     WRITE(luerr,*)' DUMMY WRITE HERE'
*     WRITE(luerr,*)' LINE 3   '
*     WRITE(luerr,*)' DEBUG-MARQ CALLIN FUNC'

      CALL FUNC(NBLD,NPOINT,NPBLOCK,NPARM,PBNAME,N2PRINT,
     %   TIMBLD,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARMREAL,dact,TISMISC1)
      PHI = 0.
      DO 1520 J1 = 1,NPOINT

*******************DEBUG********************
      IF(N2PRINT.EQ.3)THEN
      DEBUG1=TIME2(J1)-TIME1(J1)+1
      DEBUG2=TISACT(J1)/DEBUG1
      DEBUG3=TEMP(J1)/DEBUG1
      DEBUG4=DEBUG1/2.+TIME1(J1)
*     WRITE(12,1519)I,DEBUG4,DEBUG2,DEBUG3,(DEBUG3-DEBUG2),
*    &              (TEMP(J1)-TISACT(J1))
*1519  FORMAT(1X,I3,1X,F8.1,1X,4(G12.4,1X))
      ENDIF
*********************************************
      tempa = dact(j1) - tisact(j1)
1520  PHI=PHI+ WEIGHTS(J1)*tempa*tempa

*******************DEBUG**********************
*     WRITE(12,*)'  SUM OF SQUARED ERROR   =  ',PHI
**********************************************

      IF(PHI .LT. 1.E-10) GO TO 3000
      IF( ITER .GT. 0 ) GO TO 1540
      GO TO 2110
1540  IF( PHI .GE. PH ) GO TO 1530
C
C     EPSILON TEST
C
      ICONV=0
      DO 1220 J1 = 1,NPARM
      tempa = deq2b(j1)
1220  IF(ABS(tempa)/(TAU+ABS(PARM2(J1))).GT.EPS)ICONV=ICONV+1
      IF(ICONV.EQ.0) GOTO 1400
C
C     GAMMA LAMBDA TEST
C
      IF (FL .GT. 1.0 .AND. GAMM .GT. 90.0 ) ICONV= -1
      GO TO 2105
C
C     GAMMA EPSILON TEST
C
1400  IF (FL .GT. 1.0 .AND. GAMM .LE. 45.0  ) ICONV= -4
      GO TO 2105
C
1530  IF(ITERNEW.GT.2)GOTO 2310
      ITERNEW=ITERNEW+1
      GO TO (530,590,800),ITERNEW
2310  IF( FL .LT. 1.0E+8 ) GO TO 800
      ICONV= 0
C
2105  FLA = FL
      DO 2091 J2 = 1,NPARM
2091  PARM1(J2)=PARM2(J2)
2110  DO 2050 J2 = 1,NPOINT
      TISMISC(J2)=TISMISC1(J2)
      dact1(j2) = dact(j2)
2050  TISACT1(J2)=dact(J2)
      PH =PHI
      ITER = ITER + 1
      GOTO 100
3000  ICONV= 0
      GO TO 2105
      END

