classdef F18DeoxyGlucoseKinetics < mlkinetics.AbstractF18DeoxyGlucoseKinetics
	%% F18DEOXYGLUCOSEKINETICS  

	%  $Revision$
 	%  was created 21-Jan-2016 16:55:56
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.307022 (R2016a) Prerelease for MACI64.  Copyright 2017 John Joowon Lee.
 	

	properties
        LC = 0.64 % Powers, et al., JCBFM 31(5) 1223-1228, 2011.
        %LC = 1 % Huang 1980
        notes = ''
        xLabel = 'times/s'
        yLabel = 'activity'
    end
    
    methods (Static)
        function this = godo(sessd)
            cd(sessd.sessionPath);
            this = mlpowers.F18DeoxyGlucoseKinetics(sessd, 'mask', []);
            [this,lg] = this.doItsBayes;
            fprintf('%s\n', char(lg));
        end
        function this = godoQuietly(sessd)
            cd(sessd.sessionPath);
            this = mlpowers.F18DeoxyGlucoseKinetics(sessd, 'mask', []);
            this = this.doItsBayesQuietly;
        end

        function this = simulateMcmc(Aa, fu, k1, k2, k3, k4, t, u0, v1, mapParams)
            import mlpowers.*;
            qpet = F18DeoxyGlucoseKinetics.qpet(Aa, fu, k1, k2, k3, k4, t, v1);
            qpet = F18DeoxyGlucoseKinetics.pchip(t, qpet, t, u0);
            dta_ = struct('times', t, 'specificActivity', Aa);
            tsc_ = struct('times', t, 'specificActivity', qpet);
            this = F18DeoxyGlucoseKinetics(sessd, 'hct', 40, 'dta', dta_, 'tsc', tsc_);
            this.mapParams = mapParams;
            [this,lg] = this.doItsBayes;
            fprintf('%s\n', char(lg));
        end
    end
    
	methods
 		function this = F18DeoxyGlucoseKinetics(varargin)
 			%% F18DEOXYGLUCOSEKINETICS
 			%  Usage:  this = F18DeoxyGlucoseKinetics() 			
 			
 			this = this@mlkinetics.AbstractF18DeoxyGlucoseKinetics(varargin{:});
            this.v1 = this.sessionData.v1;
            this.k1 = this.sessionData.k1;
            this.k2 = this.sessionData.k2;
            this.k3 = this.sessionData.k3;
            this.k4 = this.sessionData.k4;            
        end
        
        function this = prepareScannerData(this)
            tsc = mlpet.TSC.import(this.sessionData.tsc_fqfn);
            this.tsc_ = tsc;
        end
        function this = prepareAifData(this)
            dta = mlpet.DTA.loadSessionData(this.sessionData);
            dta.scannerData = this.tsc; %% KLUDGE
            this.dta_ = dta;
        end   
        function this = simulateItsMcmc(this)
            this = mlpowers.F18DeoxyGlucoseKinetics.simulateMcmc( ...
                   this.arterialNyquist.specificActivity, ...
                   this.fu, this.k1, this.k2, this.k3, this.k4, ...
                   this.arterialNyquist.times, this.u0, this.v1, this.mapParams);
        end
        
 	end 
    
	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

