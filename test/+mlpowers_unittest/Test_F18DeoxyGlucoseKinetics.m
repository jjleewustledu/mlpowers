classdef Test_F18DeoxyGlucoseKinetics < matlab.unittest.TestCase
	%% TEST_F18DEOXYGLUCOSEKINETICS 

	%  Usage:  >> results = run(mlpowers_unittest.Test_F18DeoxyGlucoseKinetics)
 	%          >> result  = run(mlpowers_unittest.Test_F18DeoxyGlucoseKinetics, 'test_godo')
 	%  See also:  file:///Applications/Developer/MATLAB_R2014b.app/help/matlab/matlab-unit-test-framework.html

	%  $Revision$
 	%  was created 21-Jan-2016 16:55:57
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/test/+mlkinetics_unittest.
 	%% It was developed on Matlab 9.0.0.307022 (R2016a) Prerelease for MACI64.
 	

	properties
 		registry
        sessionData
        studyd
 		testObj
 	end

	methods (Test)
        function test_fdgnf(this)
            cd(this.sessionData.sessionPath);
            tsc = mlpet.TSC.import(this.sessionData.tsc_fqfn);
            dta = mlpet.DTA.loadSessionData(this.sessionData);
            dta.times = tsc.times;
            dta.specificActivity = ...
                [37843 846432 512595 286891 254030 236368 229265 222161 215057 207806 200136 192438 184739 177041 169343 162996 313551 304664 295778 287103 279519 272097 264676 258005 494812 472467 454668 438539 422351 405678 388896 378160 382128 385435 373685 357657 341442 324318 307674 298748 434666 434666 434666 434666] ./ ...
                [30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 60 60 60 60 60 60 60 60 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 180 180 180 180];
            dta.specificActivity = mlpowers.F18DeoxyGlucoseKinetics.wb2plasma( ...
                dta.specificActivity, this.sessionData.hct, dta.times);
            this = mlpowers.F18DeoxyGlucoseKinetics(this.sessionData, 'mask', [], ...
                'dta', dta);
            this = this.updateSummary;
            fprintf('%s\n', char(this.logging));
            this.plot;
        end
		function test_doBayes(this)
            this = mlpowers.F18DeoxyGlucoseKinetics.doBayes(this.sessionData);            
            this.verifyEqual(this.summary.kmin, [2.0750 0.12 0.11595 0.015307], 'RelTol', 1e-1);
            this.verifyEqual(this.summary.chi, 1.0197, 'RelTol', 2e-1);
            this.verifyEqual(this.summary.Kd, 7.0549, 'RelTol', 3e-1);
            disp(this);
        end
		function test_godos(this)
            iter = this.studyd.createIteratorForSessionData;
            for n = 1:23
                try
                    this.sessionData = iter.next;
                    cd(this.sessionData.sessionPath);
                    this = mlpowers.F18DeoxyGlucoseKinetics.godoQuietly(this.sessionData);
                    fprintf('%s\n', this.sessionData.sessionFolder);
                    this.errprintf('k1', this.k1, this.sessionData.k1);
                    this.errprintf('k2', this.k2, this.sessionData.k2);
                    this.errprintf('k3', this.k3, this.sessionData.k3);
                    this.errprintf('k4', this.k4, this.sessionData.k4);
                catch
                end
            end
        end
        function errprintf(~, lbl, v, vexpect)
            fprintf('%s:  err -> %g, value -> %g, expected -> %g\n', ...
                lbl, (v - vexpect)/vexpect, v, vexpect);
        end
        function test_plots(this)
            iter = this.studyd.createIteratorForSessionData;
            for n = 1:23
                try
                    this.sessionData = iter.next;
                    cd(this.sessionData.sessionPath);
                    this = mlpowers.F18DeoxyGlucoseKinetics(this.sessionData, 'mask', []);
                    this.plot;
                catch
                end
            end
            %this = this.updateSummary;
            %fprintf('%s\n', char(this.logging));
        end
        function test_dta(this)
            cd(this.sessionData.sessionPath);
            this = mlpowers.F18DeoxyGlucoseKinetics(this.sessionData, 'mask', []);
            dta  = this.dta;
            dtaNyq = this.arterialNyquist;
            figure;
            plot(dta.times, dta.specificActivity, dtaNyq.times, dtaNyq.specificActivity);
        end
        function test_tsc(this)
            cd(this.sessionData.sessionPath);
            this = mlpowers.F18DeoxyGlucoseKinetics(this.sessionData, 'mask', []);
            tsc  = this.tsc;
            tscNyq = this.scannerNyquist;
            figure;
            plot(tsc.times, tsc.specificActivity, tscNyq.times, tscNyq.specificActivity);
        end
        function test_slide(this)
            studyd = mlpipeline.StudyDataSingletons.instance('test_powers');
            sessDat = studyd.sessionData('studyData', studyd, 'sessionPath', pwd);            
            cd(sessDat.sessionPath);
            
            import mlpet.*;
            t    = [0:0.1*pi:1.9*pi 2*pi:0.2*pi:3.8*pi 4*pi:0.5*pi:5.5*pi 6*pi];
            this = mlpowers.F18DeoxyGlucoseKinetics({ t }, { sin(t) });
            figure;
            hold on;
            plot(t, sin(t));
            plot(t, this.slide(sin(t), t, 2*pi));
            plot(t, this.slide(sin(t), t, -2*pi));
        end
    end

 	methods (TestClassSetup)
		function setupF18DeoxyGlucoseKinetics(this)
 			import mlpowers.*;
            this.studyd = mlpowers.StudyDataSingleton.instance('initialize');
            iter = this.studyd.createIteratorForSessionData;
            this.sessionData = iter.next;
 		end
	end

 	methods (TestMethodSetup)
		function setupF18DeoxyGlucoseKineticsTest(this)
 			this.testObj = this.testObj_;
 		end
	end

	properties (Access = 'private')
 		testObj_
 	end

	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

