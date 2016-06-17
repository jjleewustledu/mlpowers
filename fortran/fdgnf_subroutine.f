c  *********************************************************
c    compute response for model with q1(t) = Vb Ca(t);
c    i. e., ignore effects of flow
C    COMPUTE ONLY FOR COMPARTMENTS 2-3
C    ****  Q1 MUST BE ADDED IN CALLING PROGRAM and multiply
C    all terms (sum) by Vb  ******
C    JOANNE MARKHAM
C    JULY 1991
c
c      April 1997
c      add checks for parameter values = 0 so 
c     program can be used for simplier models
c     perform preliminary computation in double precision
c
       subroutine fdgnf (n,t,rate,DECAY,QT)
       double precision  xk21, xk02, xk32, xk23, xk22, sa, sb
       double precision  temp,e1,e2 ,cona,conb
       dimension t(2),rate(6),qt(2)
       data luerr/6/
       xk21 = rate(2)
       xk02 = rate(3)
       xk32 = rate(4)
       xk23 =rate(5)
       xk22 = xk02 + xk32
       if (xk21 .eq. 0.) then
       write (luerr, *) ' ERROR:  k21 (K1) = 0'
       write (11, *) ' ERROR:  k21 (K1) = 0'
       stop 4 
	endif

	if (xk02.eq.0.) go to 300
	if (xk32.eq.0.) go to 400
	if (xk23.eq. 0.) go to 500

       sa = xk22 +xk23
       sb = xk02 * xk23 
       temp = dsqrt (sa*sa -4.0*sb)
       e1 = 0.5*(sa + temp)
       e2 = 0.5*(sa - temp)
       cona = xk21/(e1-e2)
       conb  = xk32 + xk23 
       con1 = cona*(e1-conb)
       con2 = cona*(conb -e2)
       se1=e1
       se2 = e2
c       write (6,*) ' e1, e2 ', se1, se2
c	write (6,*) ' con1, con2 ', con1, con2
       do 100 i=1,n
       tt = t(i)
       exa = exptrap(-se2 *tt)
       exb = exptrap(-se1* tt)
       qt(i) = con1*exb + con2*exa
100      continue

	 go to 600
300     continue
c 
c  model has one/tow compartments that is sink for
c  input  -- solution is integral of input *k21
c
      do 320 i = 1,n
      qt(i) = xk21
320    continue
	go to 600
c
c
400    continue
c
c    model consist of one compartment with outflow
c
             do 420 i=1,n
	       qt(i) = xk21 *exp(-xk22*t(i))
420    continue
        go to 600
500     continue
c
c
c  model  consists of 2 compartments without
c   back transfer between them;
c
	 cona = xk21/xk22
	 do 520 i = 1,n
         qt(i) = cona *(xk32 + xk02 *exp(-xk22*t(i)))
520      continue
	 go to 600

C
C   CORRECT FOR DECAY IF NECESSARY
C

600       continue

         IF (DECAY.EQ.0.) RETURN
         RC = ALOG(2.0)/DECAY
         DO 200 I=1,N
         QT(I) = QT(I) *EXP(-RC*T(I))
 200     CONTINUE
       return
       end
