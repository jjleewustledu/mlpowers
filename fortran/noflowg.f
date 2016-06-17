c
c    compute response for model with q1(t) = Vb Ca(t);
c    i. e., ignore effects of flow
C    COMPUTE ONLY FOR COMPARTMENTS 2-4
C    Q1 MUST BE ADDED IN CALLING PROGRAM and multiply
C    by Vb
C    JOANNE MARKHAM
C    JULY 1991
c
       subroutine noflowg (n,t,rate,DECAY,QT)
       dimension t(2),rate(6),qt(2)
       xk21 = rate(2)
       xk12 = rate(3)
         xk32 = rate(4)
       xk43 =rate(5)
       xk04 = rate(1)
       xk22 = xk12 + xk32
       cona = xk22 - xk43
       conb = xk22 - xk04
       conc = xk04 - xk43
       con1 = (xk21*xk32)/cona
       cond = con1*xk43
       con2 = cond/conb
       con3 = cond/conc
       do 100 i=1,n
       tt = t(i)
         exa = exptrap(-xk22*tt)
       exb = exptrap(-xk43*tt)
       exc = exptrap(-xk04*tt)
       q2 = xk21*exa
       q3 = con1*(exb-exa)
       q4 = con2*(exa -exc) + con3*(exb - exc)
       qt(i)  = q2 + q3 + q4
100      continue
C
C   CORRECT FOR DECAY IF NECESSARY
C
         IF (DECAY.EQ.0.) RETURN
         RC = ALOG(2.0)/DECAY
         DO 200 I=1,N
         QT(I) = QT(I) *EXP(-RC*T(I))
 200     CONTINUE
       return
       end