CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                C
C   OPTMAIN.FTN                                                  C
C
C     VERSION FOR BILL POWERS --GLUCOSE UTILIZATION
C     USES WEIGHTED OPT
C
C
C   MODIFIED VERSION OF RPOMAIN.FTN WRITTEN BY MARK MINTUN
C   FEW CHANGES WERE MADE TO THE FORMAT STATEMENTS AND PRINTOUT
C   BUT CONTENT WAS  CHANGED
C
C
C   MODIFIED BY:  JOANNE MARKHAM
C   DATE:         OCTOBER 1989
C   MODIFIED:  JOANNE MARKHAM  JULY 1991
C              SHIFT PET TISSUE ACTIVITY INSTEAD OF BLOOD CURVE
C
C
C   MODIFIED BY:  JOANNE MARKHAM
C   DATE:         OCTOBER 1992
C                  convert to unix version 
C                 changes for I/O only required
C   
C   MODIFIED BY:  Joanne Markham
C                 October 1992
C                 change to DP for computation of function 
C                  (so that slopei/derivative is more accurate)
C                   (change convolution output only)
C
C   MODIFIED BY:  Joanne Markham  October 1993
C                 change to FDG model
C                 change weights for ECAT scanner 
C                   variance of data depends on total counts/region
C                    not counts/pixel
C
C   THIS IS THE MAIN ROUTINE FOR THE Regional Parameter          C
C   Estimation PROGRAM.  THIS PROGRAM USES THE MARQUART METHOD OF
C   PARAMETER ESTIMATION AND ANALYTICAL MODELS DEFINED BY
C   VARIOUS FUNC SUBROUTINES TO ESTIMATE VALUES FOR THE
C   UNKNOWN PHYSIOLOGIC PARAMETERS
C    GIVEN:
C   (1) THE PET SCAN TISSUE TIME-ACTIVITY DATA,
C   (2) BLOOD CURVE RESPRESENTING THE INPUT FUNCTION
C
C     SUBROUTINES CALLED:
C      READTIS ------  READ THE TISSUE TIME-ACTIVITY CURVES
C
C      BLDI    ------  READS THE BLOOD CURVE AND INTERPOLATES TO
C                      1 SEC INTERVALS
C      MARQ    ------  MARQUARDT ITERATIONS
C      PBLK    ------  READS THE PARAMETER INFORMATION
C      FUNC    ------  SUBROUTINE WHICH EVALUATES THE VALUE OF
C                      TISSUE CURVE GIVEN PARAMETER VALUES AND
C                      INPUT BLOOD CURVE
C      PARVAR  ------  COMPUTES VARIANCE OF PARAMETER ESTIMATES
C
C
C                                                                C
C   INITIAL LOGICAL UNIT ASSIGNMENTS ARE SIMPLY:                 C
C      LU 5  = CONSOLE READ, OR BATCH READ                       C
C      LU 6  = CONSOLE WRITE, AND ERROR PRINTOUT                 C
C                                                                C
C   ALL OTHER LOGICAL UNIT ASSIGNMENTS ARE MADE IN THE PROGRAM,  C
C   AND ARE AT THE DESCRETION OF THE USER.                       C
C                                                                C
C                                                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VARIABLES FOR REGIONAL TISSUE ACTIVITY CURVES
C
C
      INTEGER*4 NPOINT,NREG,TIME1(100),TIME2(100)
      REAL TEND,TISCURVS(100,100),pixels(100)
      CHARACTER*80 FNAMET
      CHARACTER*70 PATID
      CHARACTER*50 REGID(100)
C
C
C    NPOINTS   -  NUMBER OF TIME POINTS FOR TISSUE DATA
C    NREG      -  NUMBER OF REGIONS TO BE ANALYSED
C    TIME1     -  START TIME FOR SCAN DATA   (SECONDS)
C    TIME2     -  END OF SCAN (SCAN-LEN = TIME2-TIME1)
C    TEND      -  TIME OF END OF PET STUDY = TIME2(NPOINT)
C    TISCURVS  -  ACTIVITY VALUES FOR ALL ROI
C                 TISCURVS(I,J) - ACTIVITY FOR SCAN I AND REGION J
C    TDECAY    -  HALF-LIFE USED FOR DECAY CORRECTION (SEC)
C    FNAMET    -  FILE NAME FOR TISSUE DATA
C    PATID     -  PATIENT/SUBJECT IDENTIFICATION
C    REGID     -  IDENTIFICATION FOR REGIONS
C
C
C PARAMETERS FOR BLOOD ACTIVITY CURVE
C
      INTEGER*4 NBLD,NUMBLD
      REAL TDEL,TB(8000),BLD(8000),BLDACT(500),BLDTIM(500)
      CHARACTER*80 FNAMEB
      REAL BLOODACT(100)
      COMMON /BLOOD/BLOODACT
C
C    NUMBLD    -   NUMBER OF POINTS IN ORIGINAL BLOOD CURVE
C    BLDTIM    -  TIME VALUES FOR ORIGINAL BLOOD CURVE
C    BLDACT    -  ACTIVITY VALUES FOR ORIGINAL BLOOD CURVE
C    TB        -  TIME VALUES FOR INTERPOLATED BLOOD CURVE
C    BLD       -  BLOOD ACTIVITY VALUES FOR INTERPOLATED CURVE
C    TDEL      -  TIME INCREMENT FOR INTERPOLATION
C                 SET TO 1 SEC IN DATA STATEMENT
C    BLOODACT  -    INTEGRATED BLOOD ACTIVITY IN SCAN INTERVAL
C
C PARAMETER BLOCK DATA
C
      INTEGER*4 NUMCURV,IOPARM(6),NPARM,NPETPARM(10)
      REAL PBLOCK(20),PARM(6)
      CHARACTER*80 FNAMEPB
      CHARACTER*30 PBNAME(20)
C
C
C    NUMCURV   -  NUMBER OF CURVES (REGIONS)
C    IOPARM    -  ARRAY OF INDICIES FOR PARAMETERS TO BE ESTIMATED
C                 INDEX IS LOCATION IN PARAMETER ARRAY
C    NPARM     -  NUMBER OF VARIABLE PARAMETERS
C    NPET      -  NUMBER OF FIXED VALUE PARAMETERS WHICH VARY BY
C                 TISSUE REGION
C    NPETPARM  -  INDEX OF PARAMETERS WHICH VARY BY REGION
C    PBLOCK    -  VALUES OF ALL PARAMETERS IN ORDER AS INPUT
C    PARM      -  VALUES OF VARIABLE PARAMETERS
C    FNAMEPB   -  FILE NAME FOR PARAMETER BLOCK FILE
C    PBNAME    -  IDENTIFICATION FOR ALL PARAMETERS
C
C
C PARAMETER ESTIMATION VARIABLES
C
      double precision DACT(100) 
      INTEGER*4 ITERNUM,ICONV
      REAL TISORIG(100),TISEST(100),TISMISC(100),ERRSUM
      REAL WEIGHTS(100)
C
C    TISORIG   -  DATA FOR TISSUE REGION--TISSUE DATA MOVED TO THIS
C                 ARRAY AS EACH REGION IS PROCESSED
C    TISEST    -  ESTIMATED TISSUE CURVE OR FIT
C    TISMISC   -  NOT USED FOR GLUCOSE MODEL:( FOR SOME PROBLEMS
C                 USED FOR FIT TO INDIVIDUAL COMPARTMENTS)
C    WEIGHTS   -  WEIGHTS FOR OPTIMIZATION (WEIGHTED SUM-OF-SQUARES)
C    ICONV     -  CONVERSION INDICATOR
C    ITERNUM   -  ITERATION NUMBER (NOT USED?)
C
C INPUT/OUTPUT CHARACTER CONSTANTS
C
c
c    for SUN version
c
       CHARACTER*24 DATETIME
c
c     for HP
c
c     CHARACTER*9 DATED,DATIME
      CHARACTER*10 STARS,SPACE
      CHARACTER*80 FNAME1,FNAME2,fname3,fname4
      DATA STARS/'**********'/
      DATA SPACE/'          '/
      DATA TDEL/1.0/
      DATA LUIN, LUOUT/5,6/

******************************************************************


C
C  CALL SUBROUTINE READTIS TO READ REGIONAL TISSUE ACTIVITY CURVES
C
C

      CALL READTIS (NPOINT,NREG,TIME1,TIME2,TISCURVS,REGID,
     1PATID,FNAMET,pixels)

       TEND = TIME2(NPOINT)
       IF (TEND/TDEL.GT. 8000) THEN
       WRITE (LUOUT,*) ' MORE THAN 8000 BLOOD CURVE VALUES NEEDED'
       WRITE (LUOUT,*)' LAST SCAN TIME AND TIME INCREMENT',TEND,TDEL
       STOP 5
       ENDIF
C
C  CALL BLDI TO OBTAIN BLOOD ACTIVITY CURVE AND INTERPOLATE
C    TO A CONSTANT SAMPLING DENSITY (TDEL)
C

       CALL BLDI (NUMBLD,BLDTIM,BLDACT,NBLD,TB,BLD,TDEL,TEND,FNAMEB)

C       WRITE (6,*)' BLOOD CURVE ' ,NBLD
C       WRITE (6,1515)(TB(I),BLD(I),I=1,20)
 1515   FORMAT (2F20.2)
C
C
C  CALL RPOPBLK TO SET UP INITIAL PARAMETER BLOCK INFORMATION
C
C
       NUMCURV=0
       CALL PBLK (NUMCURV,NPARM,IOPARM,PBLOCK,FNAMEPB,NPET,
     1            NPETPARM, NREG)
      WRITE (LUOUT,*) ' RETURN FROM 1ST PBLK CALL'
C
C
******************************************************************

C
C  comments not correct for unix version 
c    READ IN PRINT-OUT SELECTION
C     PRIMARY:    0..........MINIMAL OUTPUT, RESULTS ONLY
C                 1..........NORMAL OUTPUT, BLOOD CURVE, RESULTS
C                            WITH IDENTIFYING INFORMATION
C                 2..........EXPANDED OUTPUT, ABOVE INFO WITH
C                            ANALYSIS
C
C    SECONDARY    0..........NO SECONDARY OUTPUT
C                 1..........OUTPUT OF ALL FINAL TISSUE ACTIVITY
C                            CURVES, WITH ORIGINALS
C                 2..........OUTPUT OF ALL INTERMEDIATE PARAMETER
C                            ESTIMATIONS, AND FINAL TISSUE ACTIVITY
C                 3..........OUTPUT OF ALL INFO ABOVE, AND ALSO
C                            INTERMEDIATE TISSUE ACTIVITY CURVES!
C

      WRITE(LUOUT,*)' ENTER DESTINATION FOR PRIMARY PRINT-OUT'
      READ(LUIN,110)FNAME1
110   FORMAT(A80)
      OPEN(11,FILE=FNAME1,STATUS='UNKNOWN',ERR=950)

      WRITE(LUOUT,*)' ENTER DESTINATION FOR SECONDARY PRINT-OUT'
      READ(LUIN,110)FNAME2
      OPEN(14,FILE=FNAME2,STATUS='UNKNOWN',ERR=955)
      read (LUIN,110)fname3
      OPEN (12, file=fname3,status='unknown',err=960)
      read (luin,110)fname4
      OPEN (9,file=fname4,status='unknown',err=965)
      n1print =1
      n2print = 3


**************************************************************

C
C PRINT-OUT SECTION
C

      WRITE(11,*)
      WRITE(11,*)
      WRITE(11,*)(STARS,I=1,10)
      WRITE(11,*)(SPACE,I=1,4),'  REGIONAL'
      WRITE(11,*)(SPACE,I=1,4),'  PARAMETER'
      WRITE(11,*)(SPACE,I=1,4),'  OPTIMIZATION'
      WRITE(11,*)(STARS,I=1,10)
      WRITE(11,*)
c
c  SUN version 
c 
        CALL FDATE(DATETIME)
       WRITE(11,510)DATETIME
510   FORMAT(2X,'R.P.O. RUN ON  ',a24 )
c
c
c   HP version 
c
c     CALL DATE (DATED)
c     CALL TIME (DATIME)
c     WRITE(11,510)DATED, DATIME
c510   FORMAT(2X,'R.P.O. RUN ON  ',a9,2x,a9)
      WRITE(11,*)
      WRITE(11,*)
      write (11,*)' ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
      write (11,*)'           FDG-NOFLOW MODEL  --   ECAT SCANNER   '
      write (11,*)' ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
      write (11,*)
      WRITE(11,*)'**************** ECHO INPUT DATA *****************'
      WRITE(11,*)
      WRITE(11,*)'  NUMBER OF REGIONS READ IN .....',NREG
      WRITE(11,*)' TISSUE ACTIVITY DATA FILE       ',FNAMET
      IF(N1PRINT.GE.2)THEN
      WRITE(11,*)
      WRITE (11,*)' TISSUE ACTIVITY FILE ',FNAMET
      WRITE(11,*)'     TIME START',
     * '  TIME STOP'
      DO 520 I=1,NPOINT
520   WRITE(11,*)'           ',TIME1(I),
     * '  ',TIME2(I)
      ENDIF
      WRITE(11,*)' BLOOD ACTIVITY FILE             ',FNAMEB
       MM = (NBLD-1)*TDEL
      WRITE(11,*)' NUMBER OF SECONDS BLOOD CURVE OBTAINED: ',MM
      IF(N1PRINT.GE.2)THEN
      WRITE(11,*)'  POINT NUMBER          TIME         ACTIVITY',
     &           '            ACTIVITY (EST)'
      DO 530 I=1,NUMBLD
      J=BLDTIM(I) +1
530   WRITE(11,*)'    ',I,'            ',BLDTIM(I),'         ',
     &           BLDACT(I),'                  ',BLD(J)
      ENDIF
      WRITE(11,*)' PARAMETER BLOCK FILE            ',FNAMEPB
      WRITE(11,*)'   NUMBER OF PARAMETERS TO BE ESTIMATED:',NPARM
      WRITE(11,*)'   PARAMETERS NUMBERS...................',
     * (IOPARM(I),I=1,NPARM)
      WRITE(11,*)'   NUMBER OF REGION-DEPENDENT PARAMETERS:',NPET
      IF(NPET.GT.0 .AND. N1PRINT.GE.1)THEN
      WRITE(11,*)'          PARAMETER NUMBER    '
      DO 540 I=1,NPET
540   WRITE(11,*)'          ',NPETPARM(I)
      ENDIF



*************************************************************

C
C  BEGIN ACTUAL PARAMETER ESTIMATION
C


C  CALL PBLK TO SET ALL STARTING PARAMETERS INTO
C  PARAMETER BLOCK, INCLUDING PET-DERIVED LOCAL PARAMETERS

       NUMCURV = 0
      DO 600 IREG = 1,NREG
      NUMCURV=NUMCURV+1
      CALL PBLK(NUMCURV,NPARM,IOPARM,PBLOCK,
     *          FNAMEPB,NPET,NPETPARM,NREG)
      WRITE (LUOUT,*) ' RETURN FROM PBLK CALL'

C  PLACE STARTING VALUES INTO -INITIAL GUESS- ARRAY

      DO 560 I=1,NPARM
      L=IOPARM(I)
560   PARM(I)=PBLOCK(L)

C
C  LOAD TISSUE ACTIVITY CURVE INTO CURRENT WORKING ARRAY
C
      DO 570 I=1,100
570   TISORIG(I)=TISCURVS(I,NUMCURV)
C
C  COMPUTE WEIGHTS FOR DATA TISORIG
C   CONVERT TO ACTIVITY/SEC FIRST
C     GUESS FOR LOWER ACTIVITY SINCE NO DATA AVAILABLE FOR
C     LOWER VALUES
C
C 
       pixno = pixels(ireg)
       temp = pixno*pixno
        DO 577 I=1,NPOINT
       XX = TISORIG(I)*pixno
       var = 1058.4*xx + 0.00085667*xx*xx + 6.67e+4
        WEIGHTS(I) = temp/var
 577    CONTINUE
C
C

C
C  LOAD BLOOD CURVE INTO INTEGRATED BLOODACT ARRAY
C
      DO 574 I=1,NPOINT
      BLOODACT(I)=0.0
      DO 573 JJ=TIME1(I),TIME2(I)
573   BLOODACT(I)=BLOODACT(I)+BLD(JJ)
574   CONTINUE
      WRITE(12,*)' BLOODACT = ',(BLOODACT(JJ),JJ=1,NPOINT)

************* DEBUG INSTRUCTIONS *****************
*     WRITE(12,*)' RPOMAIN-READY TO CALL RPOMARQ'
*     WRITE(12,*)' PBLOCK =',PBLOCK
*     WRITE(12,*)' PARM = ',PARM
      WRITE(12,*)' TISORIG = ',(TISORIG(I),I=1,NPOINT)
**************************************************************

C
C  CALL MARQUART PARAMETER OPTIMIZER, ANSWERS RETURNED IN
C  ARRAY -PARM- AND COMPUTER FITTED TISSUE ACTIVITY DATA
C  RETURNED IN -TISEST-.
C
       IF (NPARM.NE.0) THEN
      CALL MARQ(NBLD,NPOINT,NPBLOCK,NPARM,ERRSUM,ITERNUM,ICONV,
     *          N2PRINT,TB,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARM,
     *          TISORIG,TISEST,TISMISC,WEIGHTS,PBNAME)
       WRITE (LUOUT,*) ' RETURN FROM MARQ '

       ELSE
       CALL FUNC (NBLD,NPOINT,NPBLOCK,NPARM,PBNAME,N2PRINT,
     1TB,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARM,dact,TISMISC)
       do 2003 i = 1,npoint
2003    tisest(i) =dact(i)
      ENDIF
C
C   COMPUTE SUM-OF-SQUARES >> NOTE THAT THE RMSE IS
C   NOT MEANINGFUL IS SIMULATION OPTION IS USED BECAUSE
C   NUMBER OF ESTIMATED PARAMETERS IS UNKNOWN
C
         SUMSQ =0.
         WSUMSQ =0.

         DO 2100 I=1,NPOINT
         TEMP = TISORIG(I)-TISEST(I)
         WSUMSQ = WSUMSQ + WEIGHTS(I)*TEMP*TEMP
 2100    SUMSQ = SUMSQ + TEMP*TEMP
         RMSE = SQRT (SUMSQ/(NPOINT-NPARM))
         RMSEW = SQRT (WSUMSQ/(NPOINT-NPARM))
C  LOAD ESTIMATED PARAMETERS INTO PARAMETER BLOCK

      DO 580 I=1,NPARM
      L=IOPARM(I)
580   PBLOCK(L)=PARM(I)

C
C
C  PRINT-OUT OF TISSUE CURVES (ORIG AND ESTIMATED)
C
      WRITE(14,*)' -- REGION # ',IREG,' NAME = ',REGID(IREG)
      WRITE(14,*)'  ',NPOINT,4
      DO 575 IP=1,NPOINT
      TT1=TIME1(IP)
      TT2=TIME2(IP)
      T1=TT1 + (TT2-TT1)/2.
      A1=TISORIG(IP)/(TT2-TT1)
      A2= TISEST(IP)/(TT2-TT1)
      A3=TISMISC(IP)/(TT2-TT1)
575   WRITE(14,*)'  ',T1,'  ',A1,'  ',A2,'  ',A3


C
C  REGION-BY-REGION CONDENSED PRINT-OUT
C

      WRITE(11,*)
      WRITE(11,*)' DATA SET = ',NUMCURV
      WRITE(11,*)' -- REGION NUMBER ',IREG,' -- REGION NAME ',
     * REGID(IREG)

C  CHECK TO SEE IF OPTIMIZER CONVERGED

       IF (NPARM.EQ.0)THEN
       WRITE (11,1213)
 1213  FORMAT (//' >>>>>>>>   SIMULATION ONLY -- NO PARAMETER ES'
     1' ESTIMATION <<<<<<<'//)
       ELSE
      IF(ICONV.NE.0)THEN

C  IF UNABLE TO CONVERGE REPORT TO PRINT-OUT

      WRITE(11,*)' UNABLE TO CONVERGE WITH THIS REGION, ',
     * ' ICONV VARIABLE = ',ICONV

      ENDIF
      ENDIF

      WRITE(11,585)(PBLOCK(I),PBNAME(I),I=1,NPBLOCK)
585   FORMAT(10X,G15.5,3X,A30)
C
C    CALL SUBROUTINE PARVAR TO COMPUTE VARIANCE OF PARAMETERS
C    ESTIMATED -- FOR ESTIMATION OPTION ONLY
C
       IF (NPARM.NE.0) THEN
       CALL PARVAR (NBLD,NPOINT,NPBLOCK,NPARM,PBNAME,N2PRINT,
     1TB,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARM,TISORIG,TISEST,WEIGHTS,
     2RMSE)
        ENDIF
        WRITE (11,1217)WSUMSQ,RMSEW
 1217   FORMAT (/' WEIGHTED SUM-OF-SQUARES & RMSE ',2F20.3)
         WRITE (11,1212)SUMSQ,RMSE
 1212    FORMAT (//' UNWEIGHTED SUM-OF-SQUARES & RMSE ', 2F20.0)
      IF(NUMCURV.EQ.1)THEN
      WRITE(9,*)'    ',FNAMET,FNAMEB
      WRITE(9,*)NREG,20
      ENDIF
      WRITE(9,*)(PBLOCK(I),I=1,20)


C  END OF LOOP FOR THIS REGION

590   CONTINUE


600   CONTINUE


      WRITE(11,*)
      WRITE(11,*)'************* END OF JOB *******************'

      STOP


*********************************************************************

C
C ERROR MESSAGES
C

950   WRITE(LUOUT,*)
      WRITE(luout,*)' ...ERROR...UNABLE TO OPEN FILE ',FNAME1
      GOTO 999

955   WRITE(luout,*)
      WRITE(luout,*)' ...ERROR...UNABLE TO OPEN FILE ',FNAME2
      GOTO 999
960   write (luout,*)
      write (luout,*)' ... ERROR...UNABLE TO OPEN FILE ',FNAME3
      GO TO 999
965   WRITE (LUOUT,*)
      WRITE (LUOUT,*)' ... ERROR ... UNABLE TO OPEN FILE',FNAME4
      GO TO 999
999   continue 
      STOP


      END