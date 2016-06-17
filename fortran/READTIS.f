CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Subroutine:  READTIS
C
C   Author:     JOANNE MARKHAM
C   Date:       SEPT 1989
C   Written For:   OPTMAIN
C
C   Intent:   Subroutine to read file containing ROI tissue-activity
C             curves for optimization program
C
C   Variables Passed:
C             NPOINT     -  number of time points (max 100)
C             NREG       -  number of regions (max 100)
C             SCANT0     -  start time of scan (seconds)
C             SCANEND    -  end of scan (seconds)
C             TISACT     -  tissue activity
C             REGID      -  region id (10 characters)
C             FNAME     -   filename for ROI tissue activity data
C   Logical Units:
C                   LUCMD   -   5, .CMD file containing file name
C                   LUERR   -   6, ERROR MESSAGES
C                   LU 2    -   assigned to ROI tissue activity file
C
C
C   Uses Subroutines:  None
C   Called By:         OPTMAIN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
       SUBROUTINE READTIS (NPOINT,NREG,SCANT0,SCANEND,TISACT,
     1REGID,TITLE,FNAME)
        INTEGER*4 NPOINT, NREG, LUCMD, LUERR
        INTEGER*4 SCANT0(100),SCANEND(100)
        REAL  TISACT(100,100)
        REAL  TSTART,TLEN
        CHARACTER*50 REGID(100)
        CHARACTER *80 FNAME
        CHARACTER*70  TITLE
        DATA LUCMD, LUERR/5, 6/
C
C       READ FILENAME FROM .CMD FILE
C
        READ (LUCMD,1001)FNAME
 1001   FORMAT (A80)
        OPEN (2,FILE=FNAME,STATUS='OLD',ERR=920)

       READ (2,1002) TITLE
 1002  FORMAT (A70)
       READ (2,*,ERR=930) NPOINT,NREG
       NREG=NREG -2
       IF (NREG.GT.100) THEN
       WRITE (LUERR,*) ' MORE THAN 100 TISSUE VALUES ',NREG
       STOP 6
       ENDIF
       DO 20 I=1,NPOINT
       READ (2,*,ERR=930) TSTART,TLEN,(TISACT(I,J),J=1,NREG)
       SCANT0(I)=TSTART
       SCANEND(I)= TSTART+TLEN
 20    CONTINUE
       DO 40 I=1,NREG
 40    READ (2,1003,ERR=930)REGID(I)
 1003  FORMAT (A50)
       CLOSE (UNIT=2)
       RETURN
 920   WRITE (LUERR,1101) FNAME
 1101  FORMAT (' *** FILE NOT FOUND ***   ',/' ', A80)
       STOP
 930   WRITE (LUERR,1102) FNAME
 1102   FORMAT (' **** ERROR WHILE READING FILE ***  ',/' ',A80)
      STOP
       END

