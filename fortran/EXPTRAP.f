
      FUNCTION EXPTRAP(X)
     0 DATA  XMIN, XMAX, EMIN, EMAX
     1    / -70., 70., 0., 2.51544E 30 /
      IF (X .LE. XMIN) GO TO 1
      IF (X .GE. XMAX) GO TO 2
      EXPTRAP = EXP(X)
      RETURN
  1   EXPTRAP = EMIN
      RETURN
  2   EXPTRAP = EMAX
      RETURN
      END