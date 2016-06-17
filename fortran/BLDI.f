********************************************************************
*                                                                  *
*    READBLD.FTN                                                    *
*                                                                  *
*      THIS PROGRAM READS IN A BLOOD CURVE FROM A DESIGNATED       *
*    FILE, THEN CONVERTS THE IRREGULARLY TIMED MEASURED POINTS     *
*    INTO AN EVENLY SPACED INPUT FUNCTION.  THIS IS DONE USING     *
*    SIMPLE INTERPOLATION.                                         *
*                                                                  *
*                                                                  *
********************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          WRITTEN BY MARK MINUTN
C          MODIFIED BY JOANNE MARKHAM  SEPT. 1989
C
C       INTERPOLATE CURVE FROM 0 BY VARIABLE TDEL
C       AND CHECK TO SEE IF CURVE ENDS BEFORE SCANS
C       IF SO, EXTRAPOLATE BY REPEATING LAST DATA VALUE
C       UNTIL BLOOD CURVE EXTENDS TO END OF SCAN TIME
C
C    MODIFIED FOR:  OPTMAIN
C
C     TB  ADDED AS NEW ARGUMENT
C     TDEL ADDED AS NEW ARGUMENT
C     TEND  ADDED AS NEW ARGUMENT
C
C
C     NBLD    -  NUMBER OF POINTS IN INTERPOLATED CURVE
C     BLD     -  BLOOD ACTIVITY FOR INTERPOLATED TIME POINTS
C     NUMBLD  -  NUMBER OF POINTS IN ORIGINAL BLOOD CURVE
C     A       -  BLOOD ACTIVITY IN ORIGINAL CURVE
C     T       -  TIME POINTS FOR ORIGINAL BLOOD CURVE
C     TB      -  TIME POINTS FOR INTERPOLATED BLOOD CURVE
C     TDEL    -  TIME INCREMENT ( SET TO 1 SEC NOW)
C     TEND    -  ENDING TIME FOR TISSUE SCAN DATA
C     FNAME   -  FILENAME FOR BLOOD CURVE
C
C   LOGICAL UNITS:
C           LUIN     -  .CMD FILE CONTAINING FILE NAME FOR BLOOD CURVE
C           LU  2   -   ASSIGNED TO BLOOD CURVE
C
C          LUERR      -   ERROR MESSAGES
C
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE BLDI(NUMBLD,T,A,NBLD,TB,BLD,TDEL,TEND,FNAME)


      REAL TB(2),BLD(2)
      REAL T(100),A(100)

      CHARACTER*70 HEADER
      CHARACTER*80 FNAME
      data luin,luerr/5,6/

      WRITE(luerr ,*)
      WRITE(luerr ,*)' ENTER NAME OF FILE CONTAINING BLOOD CURVE DATA'
      READ(luin,1020)FNAME
1020   FORMAT(A80)

      OPEN(2,FILE=FNAME,STATUS='OLD',ERR=950)

      READ(2,1040)HEADER
1040   FORMAT(A70)
      write (luerr,*)' FILE ', fname
      WRITE(luerr,*)'  HAS BEEN OPENED, HEADER IS: '
      WRITE(luerr,*)' ',HEADER

      READ(2,*,ERR=940)N
      NUMBLD=N
      IF (N.GT.500)THEN
      WRITE (LUERR,*) ' ERROR - NO OF DATA POINTS > 500 ',N
      STOP 3
      ENDIF

      IF(N.LT.2)GOTO 940

      DO 50 I=1,N
      READ(2,*,ERR=940)T(I),A(I)
 50     CONTINUE

C

C
C
       CLOSE (UNIT=2)

65     CONTINUE
C
C     CHECK FOR TOTAL TIME INTERAL AND SET TDEL
C     ACCORDINGLY
C
C     JOANNE MARKHAM  AUG 1991
C
         IF (T(N).LE.1000.)TDEL = 0.5

C
C   REPLACED ORGINIAL CODE  --JOANNE MARKHAM ,SEPT 1989
C
       IF(T(1).LE.0.) THEN
       TT =0.
       JJ = 0
       ELSE
       TB(1) =0.
       JJ =1
       TT = TDEL
       ENDIF
       JK =1
 100    CONTINUE
        DO 130 I =JK,N
        IF (T(I)-TT) 130,140,150
 130    CONTINUE
 140    CONTINUE
        JJ = JJ+1
        TB(JJ) =TT
        BLD(JJ) =A(I)
        JK =I
        GO TO 180
 150    JJ=JJ +1
        TB(JJ) = TT
        IF (I.EQ.1) GO TO 155
        II=I-1
        R = (TT-T(II))/(T(I)-T(II))
        BLD(JJ)=A(II) + R*(A(I)-A(II))
        JK = II
        GO TO 180
 155    CONTINUE
        BLD(JJ) = TT*A(1)/T(I)
 180    TEMP = TT +TDEL
        IF (TEMP.GT.T(N)) GO TO 200
        TT = TEMP
        GO TO 100
 200    NBLD =JJ
        IF (TEND.LE.TB(JJ)) GO TO 230
        M = (TEND-TB(JJ))/TDEL
        WRITE (LUERR,1102)TEND,TB(JJ)
 1102   FORMAT (' ERROR , SCAN TIME EXTENDS PAST END OF BLOOD ',
     1'SAMPLES', 2F12.2)
        IF (M.GT.20) GO TO 960
        WRITE (LUERR,1104)M
 1104   FORMAT (I10, ' POINTS WILL BE ADDED AT END BY DUPLICATING',
     1' THE LAST BLOOD SAMPLE ')
       C = A(N)
       DO 220 I = 1,100
       JJ =JJ+1
       TT =TT+TDEL
       TB(JJ) =TT
       BLD(JJ) =C
220    IF (TT.GE.TEND) GO TO 230
230    NBLD =JJ

      RETURN


940   WRITE(luerr,*)
      WRITE (LUERR,*)' ...ERROR...INCORRECT NO OF BLOOD SAMPLE VALUES '
      WRITE(luerr,*)' ...ERROR WHILE READING BLOOD DATA FILE ',FNAME
      GOTO 900
950   WRITE(luerr,*)
      WRITE(luerr,*)' ...ERROR...UNABLE TO OPEN/FIND FILE NAMED ',FNAME
      GO TO 900
 960   WRITE (luerr,*)
       WRITE (luerr,*) '...ERROR...SCAN TIME EXCEEDS BLOOD SAMPLES'
       WRITE (luerr,*) ' BY MORE THAN 20 POINTS ',M
900   STOP 7
      END
