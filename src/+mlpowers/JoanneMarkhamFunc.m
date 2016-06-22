classdef JoanneMarkhamFunc 
	%% JOANNEMARKHAMFUNC  

	%  $Revision$
 	%  was created 17-Jun-2016 11:24:28
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.341360 (R2016a) for MACI64.
 	

	properties
 		
 	end

	methods 

        % ******************************************************************
        % *                                                                *
        % *   JMFUNCG.FTN                                                  *
        % *                                                                *
        % *     THIS SUBROUTINE 1) CALCULATES THE TISSUE ACTIVITY GIVEN    *
        % *   A SET OF MODEL PARAMETERS AND INPUT CURVE (BLOOD ACTIVITY).  *
        % *   AND 2) CALCULATED PHYSIOLOGIC PARAMTERS FROM GIVEN MODEL     *
        % *   PARAMETERS.                                                  *
        % *                                                                *
        % *   MODifIED BY JOANNE MARKHAM NOV 1989
        % *   FOR GLUCOSE MODELS ONLY
        % C
        % C
        % C   Modified by Joanne Markham Oct 1992
        % C               to use DP for result of convolution
        % C
        % C
        % C
        % C   CALL SUBROUTNE BY Joanne Markham  FOR "NOFLOW-FDG " GLUCOSE MODEL
        % C    COMPUTE IMPULSE RESPONSE FOR COMPARTMENTAL MODELS
        % C
        % C   CALLS SUBROUTINE JMCONV FOR CONVOLUTION OF IMPULSE RESPONSE WITH
        % C   BLOOD CURVE
        % ******************************************************************

        function [PBNAME,TISACT] = ...
                FUNC(this, NBLD,NPOINT,~,NPARM,~, ...
                TIMBLD,BLD,TIME1,TIME2,PBLOCK,IOPARM,PARM,~)
            
            %       Double precision  TISACT(npoint),Q(4000),Q1,Q2,BLDINT(8000)
            %       double precision DTEMP
            %       REAL BLD(NBLD),PBLOCK(NPBLOCK),PARM(NPARM)
            %       REAL RATES(6),QS(8000)
            %       REAL TISMISC(NPOINT),TIMBLD(NBLD)
            %       REAL REALT1(100),REALT2(100)
            %       INTEGER*4 TIME1(NPOINT),TIME2(NPOINT),IOPARM(NPARM)
            %       CHARACTER*30 PBNAME(20)
            %       DATA JFLAG/0/
            
            JFLAG = 0;
            
            % C     DEFINITION OF ARGUMENTS
            % C
            % C     NBLD    -   NUMBER OF POINTS IN INTERPOLATED BLOOD CURVE
            % C                INTERPOLATED TO 1 SEC
            % C     NPOINT  -   NUMBER OF PET SCANS
            % C     NPBLOCK -  NUMBER OF PARAMETERS--VALUE SET IN THIS ROUTINE
            % C                MAXIMUM OF 20
            % C     NPARM  -   NUMBER OF VARIABLE PARAMETERS
            % C     PBNAME -   IDENTifICATION OF PARMETERS, ASSIGED HERE
            % C     N2PRINT -   PRINT SWITCH
            % C     TIMBLD -   ARRAY OF TIMES FOR  BLOOD CURVE
            % C     BLD    -   ARRAY OF ACTIVITY VALUES FOR BLOOD CURVE
            % C     TIME1  -   START TIME OF PET SCANS (ARRAY)
            % C     TIME2  -   end TIME FOR PET SCANS
            % C                LENGTH OF SCAN IS TIME2-TIME1
            % C     PBLOCK -   PARAMETER VALUES
            % C     IOPARM -   INDEX OF VARIABLE PARAMETERS IN PBLOCK ARRAY
            % C     PARM   -   ARRAY CONTAINING LATEST VALUE OF VARIABLE PARAMETERS
            % C     TISACT -   TISSUE ACTIVITY, COMPUTED HERE
            % C     TISMISC -  NOT USED HERE
            % C
            % C       ****   LOCAL VARIABLES  ****
            % C
            % C    Q      -   TEMPORARY FOR TISSUE ACTIVITY COMPUTATIONS
            % C    QS     -   TEMPORARY FOR IMPULSE RESPONSE
            % C    BLDINT -   INTEGRAL OF BLOOD CURVE - USED TO AVOID
            % C               INTEGRATION AT EACH STEP
            % C
            % C    REALT1  - REAL VARIABLE FOR TIME1
            % C    REALT2  - REAL VARIABLE FOR TIME2 ARRAY
            
            if(JFLAG==0)
                PBNAME{01}='BLOOD FLOW       ML/MIN/100G';
                PBNAME{02}='BLOOD VOL        ML/100G';
                PBNAME{03}='BLD GLUCOSE      uMOL/ML';
                PBNAME{04}='K-01 (F/V1)      PER MIN ';
                PBNAME{05}='K-21             PER MIN';
                PBNAME{06}='K-12/K-02        PER MIN';
                PBNAME{07}='K-32             PER MIN';
                PBNAME{08}='K-23             PER MIN';
                PBNAME{09}='T0               SEC    ';
                PBNAME{10}='T(1/2)           SECONDS';
                PBNAME{11}='CHI              PER MIN ';
                PBNAME{12}='KD               ML/MIN/100G';
                PBNAME{13}='FDG MET INDEX    uMOL/MIN/100G';
                
                JFLAG=13; %#ok<NASGU>
                NPBLOCK=13; %#ok<NASGU>
                
                % C  MOVE TIME INFORMATION FOR PET SCANS TO REAL ARRAY
                
                for I=1:NPOINT
                    REALT1(I) = TIME1(I);
                    REALT2(I) = TIME2(I);
                end
                TDEL =TIMBLD(2)-TIMBLD(1);
                OUTDEL=2*TDEL;
                
                % C   INTEGRATE BLOOD CURVE AFTER
                % C   ADDING 100 POINTS AT end TO ASSURE BLOOD DATA IS LONG ENOUGH
                
                % C    AVERAGE LAST 4 POINTS AND DUPLICATE FOR 100 POINTS
                
                NCA = NBLD;
                if ((NCA+100)>8000)
                    error('mlpowers:markhamError', ' ERROR - TIME + PADDING > 7999 SEC %g ', NCA);
                end
                AV = (BLD(NCA) + BLD(NCA-1) + BLD(NCA-2) + BLD(NCA-3))/4.;
                TT = TIMBLD(NCA);
                for I=1:100
                    NCA=NCA +1;
                    TT = TT + TDEL;
                    TIMBLD(NCA) = TT;
                    BLD(NCA) =AV;
                end
                BLDINT(1) =0.;
                DTEMP  =0.;
                for I = 2:NCA
                    DTEMP  = DTEMP +0.5*(BLD(I) + BLD(I-1)); % trapezoidal rule for numerical quadrature
                    BLDINT(I) = DTEMP *TDEL;
                end
            end
            
            % ********************************************************
            % *  LOAD NEW PARAMETERS INTO PARAMETER BLOCK FOR ALL CALCULATIONS
            
            for I=1:NPARM
                J=IOPARM(I);
                PBLOCK(J)=PARM(I);
            end
            
            % *******
            % *    PARAMETER BLOCK KEY
            % *
            % *      1       FLOW
            % *      2       BLOOD VOLUME
            % *      3       BLOOD GLUCOSE CONCENTRATION
            % *      4       FLOW/VOL (INVERSVE OF MEAN TRANSIT TIME)
            % *      5       K-2-1     (K1)  K1 = k21 *VB
            % *      6       K-1-2     (K2)
            % *      7       K-3-2     (K3)
            % *      8       K-2-3     (K4)
            % *      9       T0   - TIME SHifT FOR BLOOD CURVE (SEC)
            % *     10       THALF - HALF-LifE FOR TRACER (SEC)
            % *              DATA SHOULD BE EITHER BOTH BLOOD AND TISSUE
            % *              VALUES CORRECTED (SET T1/2 TO 0) OR BOTH
            % *              UNCORRECTED
            % *******            
            
            % *  CALCULATE SECONDARY VARIABLES FROM RATE CONSTANTS
            % *  USE MIN FOR ALL VARIABLES EXCEPT THOSE REQUIRED FOR COMPUTATIONS
            
            BLDFLW=PBLOCK(1) * 1.05/6000.;
            BLDVOL=PBLOCK(2) * 0.0105;
            TRANSIT = BLDVOL/(BLDFLW*60.);
            PBLOCK(4) = 1.0/TRANSIT;
            if (PBLOCK(7)~=0.)
                CHI=PBLOCK(5)*PBLOCK(7)/(PBLOCK(6)+PBLOCK(7));
            else
                CHI =PBLOCK(5);
            end
            PBLOCK(11)=CHI;
            TEMP=1.0;
            PBLOCK(13)= CHI*PBLOCK(2)*PBLOCK(3);
            
            PS = PBLOCK(5)*PBLOCK(2);
            PBLOCK(12)=PS;
            
            % C    CHANGE UNITS TO SEC FOR GLUCOSE RESPONSE COMPUTATIONS
            
            for I=1:5
                RATES(I) = PBLOCK(I+3)/60.;
            end
            
            NPETI = TIME2(NPOINT)/TDEL +1.05; %#ok<NASGU>
            
            % ************** DEBUG INSTRUCTIONS **********************
            % *     WRITE(12,*)' FUNC ENTERED'
            %       WRITE(12,*)' PBLOCK = ',(PBLOCK(I),I=1,NPBLOCK)
            % *     WRITE(12,*)' IOPARM = ',(IOPARM(I),I=1,NPARM)
            % *     WRITE(12,*)' PARM   = ',(PARM(I),I=1,NPARM)
            % ********************************************************
            
            % C     CALL FDG (' no flow ')  SUBROUTINE FOR COMPUTING IMPULSE RESPONSE
            % C   >>>> NOTE :  THIS SUBROUTINE PERFORMS DECAY CORRECTION ALSO
            % C                if PBLOCK(10) NOT EQUAL 0
            
            QS = this.FDGNF(NCA, TIMBLD, RATES, PBLOCK(10));
            
            % c      write (12,*) ' response '
            % c      write (12,3131) TIMBLD(11),QS(11)
            % c      write (12,3131) TIMBLD(101),QS(101)
            % c      write (12,3131)TIMBLD(1001),QS(1001)
            % c      write (12,3131)TIMBLD(3001),QS(3001)
            % 3131   format (f9.2,f12.6)
            
            % **************DEBUG****************
            % *     WRITE(12,*)(QS(I),I=1,NPETI)
            % ***********************************
            
            % C   NOTE CONVOLUTION ASSUME EQUALLY SPACES INTERVALS OF TIME
            % C   WITH INTERVAL OF OUTDEL/2 FOR INPUT AND OUTDEL FOR OUTPUT
            % C   AND FIRST POINT AT T =0
            
            NH = NCA/2 +1;
            Q = this.JMCONV(NH,OUTDEL,BLDINT,QS);
            
            % C  TRUE Q = VB [Q(i) + BLOOD(I)]
            
            for i=1:nh
                Q(i) = BLDVOL*(Q(i) + BLDINT(2*i-1));
            end
            
            % C   SHifT PET TISSUE ACTIVITY CURVE INSTEAD OF BLOOD CURVE
            % C   SHifT CAN BE EITHER NEGATIVE OR POSITIVE ALTHOUGH IT
            % C   SHOULD BE POSITIVE FOR MOST CASES

            TSHifT = PBLOCK(9);
            if (~(TSHifT<0.))
                
                for I=1:NPOINT
                    TSS =  REALT1(I) + TSHifT;
                    J = TSS/OUTDEL +1;
                    TEMP= (J-1)*OUTDEL;
                    RATIO = (TSS-TEMP)/OUTDEL;
                    Q1 = (1.0-RATIO)*Q(J) + RATIO*Q(J+1);
                    TE = REALT2(I) + TSHifT;
                    K = TE/OUTDEL +1;
                    TEMP = (K-1)*OUTDEL;
                    RATIO = (TE-TEMP)/OUTDEL;
                    Q2 = (1.0-RATIO)*Q(K) + RATIO*Q(K+1);
                    TISACT(I) = (Q2-Q1);
                end
                return
            end
            
            for I = 1:NPOINT
                TSS = REALT1(I)+TSHifT;
                if (~(TSS>=0.))
                    TSS = TSS + OUTDEL;
                    if (TSS<=0.)
                        Q1 = 0.;
                        @continue430;
                    else
                        Q1 = Q(2)* (TSS)/OUTDEL;
                        @continue430;
                    end
                end
                J = TSS/OUTDEL +1;
                TEMP = (J-1)*OUTDEL;
                RATIO = (TSS-TEMP)/OUTDEL;
                Q1 = Q(J) *(1.0-RATIO) + RATIO*Q(J+1);
            end
            
            % ******************DEBUG****************
            % *     WRITE(12,*)(Q(I),I=1,NPETI)
            % ***************************************
            
            % ***************** DEBUG INSTRUCTIONS ******************
            % *
            % *      WRITE(12,*)' FUNC endING'
            % *      WRITE(12,*)' TISACT = ',(TISACT(I),I=1,NPOINT)
            % *
            % *******************************************************
            
            return  
            
            function continue430
                TE = REALT2(I) + TSHifT;
                if (~(TE>=0.))
                    TE = TE + OUTDEL;
                    if (TE<=0)
                        Q2 =0.;
                        TISACT(I) = (Q2-Q1);
                        return
                    else
                        Q2 = Q(2)*(TE/OUTDEL);
                        TISACT(I) = (Q2-Q1);
                        return
                    end
                end
                J = TE/OUTDEL +1;
                TEMP = (J-1)*OUTDEL;
                RATIO = (TE-TEMP)/OUTDEL;
                Q2 = (1.0-RATIO)*Q(J) + RATIO*Q(J+1);
                TISACT(I) = (Q2-Q1);
            end
        end
               
        % c  *********************************************************
        % c    compute response for model with q1(t) = Vb Ca(t);
        % c    i. e., ignore effects of flow
        % C    COMPUTE ONLY FOR COMPARTMENTS 2-3
        % C    ****  Q1 MUST BE ADDED IN CALLING PROGRAM and multiply
        % C    all terms (sum) by Vb  ******
        % C    JOANNE MARKHAM
        % C    JULY 1991
        % c
        % c      April 1997
        % c      add checks for parameter values = 0 so
        % c     program can be used for simplier models
        % c     perform preliminary computation in double precision
        
        function qt = FDGNF(~, n, t, rate, DECAY)
            %        double precision  xk21, xk02, xk32, xk23, xk22, sa, sb
            %        double precision  temp,e1,e2 ,cona,conb
            %        dimension t(2),rate(6),qt(2)
            %        data luerr/6/
            xk21 = rate(2);
            xk02 = rate(3);
            xk32 = rate(4);
            xk23 = rate(5);
            xk22 = xk02 + xk32;
            if (xk21 == 0.) then
                fprintf(' ERROR:  k21 (K1) = 0');
                error('mlpowers:markham:luerr6', ' ERROR:  k21 (K1) = 0');
            end
            
            if (xk02==0.); goto300; end
            if (xk32==0.); goto400; end
            if (xk23== 0.); goto500; end
            
            sa = xk22 +xk23;
            sb = xk02 * xk23;
            temp = dsqrt (sa*sa -4.0*sb);
            e1 = 0.5*(sa + temp);
            e2 = 0.5*(sa - temp);
            cona = xk21/(e1-e2);
            conb = xk32 + xk23;
            con1 = cona*(e1-conb);
            con2 = cona*(conb -e2);
            se1  =e1;
            se2  = e2;
            % c       write (6,*) ' e1, e2 ', se1, se2
            % c	write (6,*) ' con1, con2 ', con1, con2
            for i=1:n
                tt = t(i);
                exa = exptrap(-se2 *tt);
                exb = exptrap(-se1* tt);
                qt(i) = con1*exb + con2*exa;
            end            
            @correctDecay;
            
            function goto300
                % c  model has one/tow compartments that is sink for
                % c  input  -- solution is integral of input *k21
                % c
                for i300 = 1:n
                    qt(i300) = xk21;
                end
                @correctDecay;
            end
            
            function goto400
                % c    model consist of one compartment with outflow
                % c
                for i400=1:n
                    qt(i400) = xk21 *exp(-xk22*t(i400));
                end
                @correctDecay;
            end
            
            function goto500
                % c  model  consists of 2 compartments without
                % c   back transfer between them;
                % c
                cona = xk21/xk22;
                for i500 = 1:n
                    qt(i500) = cona *(xk32 + xk02 *exp(-xk22*t(i500)));
                end
                @correctDecay;
            end
            
            function correctDecay
                if (DECAY==0.); return; end
                RC = ALOG(2.0)/DECAY;
                for I=1:N
                    qt(I) = qt(I) *EXP(-RC*T(I));
                end
                return
            end
        end
        
        function Q = JMCONV(this, NH, OUTDEL, BLDINT, QS) %#ok<STOUT,INUSD>
        end        
    end
    
	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

