******************************************************************
*                                                                *
*   JMPBLK.FTN                                                  *
*                                                                *
*     THIS SUBROUTINE ESTABLISHES THE VARIABLES OF THE           *
*   PARAMETER BLOCK FILES.  IT READS IN FIXED VALUES FOR         *
*   ALL REGIONS ANALYZED, AND DETERMINES LOCAL VARIABLE VALUES   *
*   FROM SEPARATE PET SCAN DATA (WHICH HAVE BEEN SEPERATELY      *
*   METEVAL-ED).  THE NAMES OF THESE PET SCANS ARE READ IN FROM  *
*   AN INPUT FILE.                                               *
*                                                                *
******************************************************************

      SUBROUTINE PBLK(NUMCURV,NPARM,IOPARM,PBLOCK,
     *           FNAME,NPET,NPETPARM,NREG)


      INTEGER*4 IOPARM(6),NUMCURV,NPARM,NREG,NPETPARM(10)
      REAL PBLOCK(20),PBLOCKIN(20)
      REAL PETPARM(10,100)
      CHARACTER*80 FNAME,FNAME1
      data luin,luout/5,6/
C
C  IF -NUMCURV- = 0 THEN THIS IS ORIGINAL SET-UP CALL TO SUBROUTINE
C

      IF(NUMCURV.GT.0)GOTO 700
C
C  NOW OBTAIN PET SCAN DATA FILE
C
      WRITE(luout,*)
      WRITE(luout,*)' ENTER NAME OF FILE WITH PARAMETER BLOCK INFO'
      READ (LUIN,110) fname1
110   FORMAT(A80)
      FNAME=FNAME1

      OPEN(2,FILE=FNAME1,STATUS='OLD',ERR=910)




C
C  READ ORIGINAL VALUES OF PBLOCK INTO STORAGE
C

      READ(2,*)(PBLOCKIN(I),I=1,20)

C
C  READ IN NUMBER OF PARAMETERS TO BE OPTIMIZED
C

      READ(2,*)NPARM

C
C  READ IN THE PARAMETER NUMBER OF THOSE TO BE OPTIMIZED
C

        IF (NPARM.NE.0) THEN
      READ(2,*)(IOPARM(I),I=1,NPARM)
        ENDIF

C
C  READ IN NUMBER OF REGIONAL DEPENDENT PARAMETERS
C

      READ(2,*)NPET
      IF(NPET.GT.0)THEN

C
C  FOR EACH PARAMETER, READ IN PARAMETER INDEX
        READ (2,*) (NPETPARM(I),I=1,NPET)
      DO 150 I=1,NREG
      READ (2,*)(PETPARM(J,I),J=1,NPET)
150   CONTINUE
      ENDIF

      CLOSE(2)

C
C  ALL DATA READ IN
C

C
C  ALL PET-RELATED PARAMETERS NEED TO BE STORED IN ARRAYS
C

      IF(NPET.GT.0)THEN


C
C  ALL PARAMETER BLOCK VARIABLES NOW STORED,
C  SET UP DONE
C
***********************DEBUG*********************
C      WRITE(luout,*)PETPARM
*************************************************

      ENDIF


      RETURN


*********************

C
C  SET UP NEW PARAMETER BLOCK WITH REGION-SPECIFIC PARAMETER VALUES
C

700   DO 720 I=1,20
720   PBLOCK(I)=PBLOCKIN(I)

C
C  IF NO PET-DEPENDENT PARAMETER VALUES, RETURN NOW
C

      IF(NPET.EQ.0)RETURN

C
C  LOAD ALL REGION-SPECIFIC PARAMETER VALUES INTO MAIN P-BLOCK
C

      DO 740 I=1,NPET
      N=NPETPARM(I)
740   PBLOCK(N)=PETPARM(I,NUMCURV)

      RETURN


******************************************

C
C  ERROR MESSAGES
C

910   WRITE(luout,*)' ...ERROR... CANNOT OPEN FILE NAME ',FNAME1
      GOTO 1000
1000  continue 
      STOP


      END