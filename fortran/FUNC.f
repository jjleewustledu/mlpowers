******************************************************************
*                                                                *
*   JMFUNCG.FTN                                                  *
*                                                                *
*     THIS SUBROUTINE 1) CALCULATES THE TISSUE ACTIVITY GIVEN    *
*   A SET OF MODEL PARAMETERS AND INPUT CURVE (BLOOD ACTIVITY).  *
*   AND 2) CALCULATED PHYSIOLOGIC PARAMTERS FROM GIVEN MODEL     *
*   PARAMETERS.                                                  *
*                                                                *
*   MODIFIED BY JOANNE MARKHAM NOV 1989
*   FOR GLUCOSE MODELS ONLY
C
C
C   Modified by Joanne Markham Oct 1992
C               to use DP for result of convolution
C
C
C
C   CALL SUBROUTNE BY Joanne Markham  FOR "NOFLOW-FDG " GLUCOSE MODEL
C    COMPUTE IMPULSE RESPONSE FOR COMPARTMENTAL MODELS
C
C   CALLS SUBROUTINE JMCONV FOR CONVOLUTION OF IMPULSE RESPONSE WITH
C   BLOOD CURVE
******************************************************************

      SUBROUTINE FUNC(NBLD,NPOINT,NPBLOCK,NPARM,PBNAME,N2PRINT,
     %          TIMBLD,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARM,TISACT,
     %          TISMISC)

      Double precision  TISACT(npoint),Q(4000),Q1,Q2,BLDINT(8000)
      double precision dtemp 
      REAL BLD(NBLD),PBLOCK(NPBLOCK),PARM(NPARM)
      REAL RATES(6),QS(8000)
      REAL TISMISC(NPOINT),TIMBLD(NBLD)
      REAL REALT1(100),REALT2(100)

      INTEGER*4 TIME1(NPOINT),TIME2(NPOINT),IOPARM(NPARM)

      CHARACTER*30 PBNAME(20)
      DATA JFLAG/0/

C
C     DEFINITION OF ARGUMENTS
C
C     NBLD    -   NUMBER OF POINTS IN INTERPOLATED BLOOD CURVE
C                INTERPOLATED TO 1 SEC
C     NPOINT  -   NUMBER OF PET SCANS
C     NPBLOCK -  NUMBER OF PARAMETERS--VALUE SET IN THIS ROUTINE
C                MAXIMUM OF 20
C     NPARM  -   NUMBER OF VARIABLE PARAMETERS
C     PBNAME -   IDENTIFICATION OF PARMETERS, ASSIGED HERE
C     N2PRINT -   PRINT SWITCH
C     TIMBLD -   ARRAY OF TIMES FOR  BLOOD CURVE
C     BLD    -   ARRAY OF ACTIVITY VALUES FOR BLOOD CURVE
C     TIME1  -   START TIME OF PET SCANS (ARRAY)
C     TIME2  -   END TIME FOR PET SCANS
C                LENGTH OF SCAN IS TIME2-TIME1
C     PBLOCK -   PARAMETER VALUES
C     IOPARM -   INDEX OF VARIABLE PARAMETERS IN PBLOCK ARRAY
C     PARM   -   ARRAY CONTAINING LATEST VALUE OF VARIABLE PARAMETERS
C     TISACT -   TISSUE ACTIVITY, COMPUTED HERE
C     TISMISC -  NOT USED HERE
C
C       ****   LOCAL VARIABLES  ****
C
C    Q      -   TEMPORARY FOR TISSUE ACTIVITY COMPUTATIONS
C    QS     -   TEMPORARY FOR IMPULSE RESPONSE
C    BLDINT -   INTEGRAL OF BLOOD CURVE - USED TO AVOID
C               INTEGRATION AT EACH STEP
C
C    REALT1  - REAL VARIABLE FOR TIME1
C    REALT2  - REAL VARIABLE FOR TIME2 ARRAY
C
C

********************************************************

      IF(JFLAG.EQ.0)THEN
      PBNAME(01)='BLOOD FLOW       ML/MIN/100G'
      PBNAME(02)='BLOOD VOL        ML/100G'
      PBNAME(03)='BLD GLUCOSE      uMOL/ML'
      PBNAME(04)='K-01 (F/V1)      PER MIN '
      PBNAME(05)='K-21             PER MIN'
      PBNAME(06)='K-12/K-02        PER MIN'
      PBNAME(07)='K-32             PER MIN'
      PBNAME(08)='K-23             PER MIN'
      PBNAME(09)='T0               SEC    '
      PBNAME(10)='T(1/2)           SECONDS'
      PBNAME(11)='CHI              PER MIN '
      PBNAME(12)='KD               ML/MIN/100G'
      PBNAME(13)='FDG MET INDEX    uMOL/MIN/100G'

      JFLAG=13
      NPBLOCK=13
C
C  MOVE TIME INFORMATION FOR PET SCANS TO REAL ARRAY
C
        DO 1207 I=1,NPOINT
        REALT1(I) = TIME1(I)
        REALT2(I) = TIME2(I)
 1207   CONTINUE
      TDEL =TIMBLD(2)-TIMBLD(1)
       OUTDEL=2*TDEL
C
C   INTEGRATE BLOOD CURVE AFTER
C   ADDING 100 POINTS AT END TO ASSURE BLOOD DATA IS LONG ENOUGH
C
C
C
C
C    AVERAGE LAST 4 POINTS AND DUPLICATE FOR 100 POINTS
C
C
       NCA = NBLD
       IF ((NCA+100).GT.8000)THEN
       WRITE (6,*)' ERROR - TIME + PADDING > 7999 SEC ',NCA
       STOP
       ENDIF
       AV = (BLD(NCA) + BLD(NCA-1) + BLD(NCA-2) + BLD(NCA-3))/4.
      TT = TIMBLD(NCA)
       DO 1260 I=1,100
       NCA=NCA +1
       TT = TT + TDEL
       TIMBLD(NCA) = TT
 1260  BLD(NCA) =AV
       BLDINT(1) =0.
       dtemp  =0.
        DO 1270 I = 2,NCA
       dtemp  = dtemp +0.5*(BLD(I) + BLD(I-1))
 1270   BLDINT(I) = dtemp *TDEL
      ENDIF
********************************************************

*  LOAD NEW PARAMETERS INTO PARAMETER BLOCK FOR ALL CALCULATIONS

      DO 50 I=1,NPARM
      J=IOPARM(I)
50    PBLOCK(J)=PARM(I)
*******
*
*
*    PARAMETER BLOCK KEY
*
*      1       FLOW
*      2       BLOOD VOLUME
*      3       BLOOD GLUCOSE CONCENTRATION
*      4       FLOW/VOL (INVERSVE OF MEAN TRANSIT TIME)
*      5       K-2-1     (K1)  K1 = k21 *VB
*      6       K-1-2     (K2)
*      7       K-3-2     (K3)
*      8       K-2-3     (K4)
*      9       T0   - TIME SHIFT FOR BLOOD CURVE (SEC)
*     10       THALF - HALF-LIFE FOR TRACER (SEC)
*              DATA SHOULD BE EITHER BOTH BLOOD AND TISSUE
*              VALUES CORRECTED (SET T1/2 TO 0) OR BOTH
*              UNCORRECTED
*
*
*******

*
*  CALCULATE SECONDARY VARIABLES FROM RATE CONSTANTS
*  USE MIN FOR ALL VARIABLES EXCEPT THOSE REQUIRED FOR COMPUTATIONS
*
      BLDFLW=PBLOCK(1) * 1.05/6000.
      BLDVOL=PBLOCK(2) * 0.0105
       TRANSIT = BLDVOL/(BLDFLW*60.)
       PBLOCK(4) = 1.0/TRANSIT
      IF (PBLOCK(7).NE.0.)THEN
      CHI=PBLOCK(5)*PBLOCK(7)/(PBLOCK(6)+PBLOCK(7))
      ELSE
      CHI =PBLOCK(5)
      ENDIF
      PBLOCK(11)=CHI
      TEMP=1.0
      PBLOCK(13)= CHI*PBLOCK(2)*PBLOCK(3)

       PS = PBLOCK(5)*PBLOCK(2)
       PBLOCK(12)=PS
C
C    CHANGE UNITS TO SEC FOR GLUCOSE RESPONSE COMPUTATIONS
C
        DO 60 I=1,5
 60     RATES(I) = PBLOCK(I+3)/60.

        NPETI = TIME2(NPOINT)/TDEL +1.05
C
C        IF (NPETI.GT.NCA) THEN
C
C
C

************** DEBUG INSTRUCTIONS **********************

*     WRITE(12,*)' FUNC ENTERED'
      WRITE(12,*)' PBLOCK = ',(PBLOCK(I),I=1,NPBLOCK)
*     WRITE(12,*)' IOPARM = ',(IOPARM(I),I=1,NPARM)
*     WRITE(12,*)' PARM   = ',(PARM(I),I=1,NPARM)

********************************************************


*

C
C     CALL FDG (' no flow ')  SUBROUTINE FOR COMPUTING IMPULSE RESPONSE
C   >>>> NOTE :  THIS SUBROUTINE PERFORMS DECAY CORRECTION ALSO
C                IF PBLOCK(10) NOT EQUAL 0
C
C
       CALL FDGNF (nca, timbld, rates, pblock(10),qs)
c       write (12,*) ' response '
c      write (12,3131) timbld(11),qs(11)
c      write (12,3131) timbld(101),qs(101)
c      write (12,3131)timbld(1001),qs(1001)
c      write (12,3131)timbld(3001),qs(3001)
3131   format (f9.2,f12.6)
**************DEBUG****************
*     WRITE(12,*)(QS(I),I=1,NPETI)
***********************************

C
C   NOTE CONVOLUTION ASSUME EQUALLY SPACES INTERVALS OF TIME
C   WITH INTERVAL OF OUTDEL/2 FOR INPUT AND OUTDEL FOR OUTPUT
C    AND FIRST POINT AT T =0

       NH = NCA/2 +1
C
       CALL JMCONV (NH,OUTDEL,BLDINT,qs,Q)
C
C  TRUE Q = VB [Q(i) + BLOOD(I)]
C
C
	 do 220 i=1,nh
	 q(i) = bldvol*(q(i) + bldint(2*i-1))
220     continue
C    SHIFT PET TISSUE ACTIVITY CURVE INSTEAD OF BLOOD CURVE
C   SHIFT CAN BE EITHER NEGATIVE OR POSITIVE ALTHOUGH IT
C   SHOULD BE POSITIVE FOR MOST CASES
C
        TSHIFT = PBLOCK(9)
        IF (TSHIFT.LT.0.) GO TO 400

       DO 350 I=1,NPOINT
       TSS =  REALT1(I) + TSHIFT
       J = TSS/OUTDEL +1
       TEMP= (J-1)*OUTDEL
       RATIO = (TSS-TEMP)/OUTDEL
       Q1 = (1.0-RATIO)*Q(J) + RATIO*Q(J+1)
       TE = REALT2(I) + TSHIFT
       K = TE/OUTDEL +1
       TEMP = (K-1)*OUTDEL
        RATIO = (TE-TEMP)/OUTDEL
        Q2 = (1.0-RATIO)*Q(K) + RATIO*Q(K+1)
       TISACT(I) = (Q2-Q1)
 350   CONTINUE
       GO TO 500
 400   CONTINUE
C
      DO 480 I = 1,NPOINT
      TSS = REALT1(I)+TSHIFT
      IF (TSS.GE.0.)GO TO 420
      TSS = TSS + OUTDEL
      IF (TSS.LE.0.) THEN
      Q1 = 0.
      GO TO 430
      ELSE
      Q1 = Q(2)* (TSS)/OUTDEL
      GO TO 430
      ENDIF
 420  CONTINUE
      J = TSS/OUTDEL +1
      TEMP = (J-1)*OUTDEL
      RATIO = (TSS-TEMP)/OUTDEL
      Q1 = Q(J) *(1.0-RATIO) + RATIO*Q(J+1)
 430  CONTINUE
      TE = REALT2(I) + TSHIFT
      IF (TE.GE.0.) GO TO 440
      TE = TE +OUTDEL
      IF (TE.LE.0) THEN
      Q2 =0.
      GO TO 450
      ELSE
      Q2 = Q(2)*(TE/OUTDEL)
      GO TO 450
      ENDIF
 440  CONTINUE
       J = TE/OUTDEL +1
       TEMP = (J-1)*OUTDEL
       RATIO = (TE-TEMP)/OUTDEL
       Q2 = (1.0-RATIO)*Q(J) + RATIO*Q(J+1)
 450   CONTINUE
       TISACT(I) = (Q2-Q1)
 480  CONTINUE
 500    CONTINUE

*
*
C
C
C

******************DEBUG****************
*     WRITE(12,*)(Q(I),I=1,NPETI)
***************************************

***************** DEBUG INSTRUCTIONS ******************
*
*      WRITE(12,*)' FUNC ENDING'
*      WRITE(12,*)' TISACT = ',(TISACT(I),I=1,NPOINT)
*
*******************************************************

      RETURN

      END