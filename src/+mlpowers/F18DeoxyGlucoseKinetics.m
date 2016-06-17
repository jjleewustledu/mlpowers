classdef F18DeoxyGlucoseKinetics < mlkinetics.AbstractKinetics & mlkinetics.F18
	%% F18DEOXYGLUCOSEKINETICS  

	%  $Revision$
 	%  was created 21-Jan-2016 16:55:56
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.307022 (R2016a) Prerelease for MACI64.
 	

	properties
        fu = 2 % FUDGE
        % Joanne Markham used the notation K_1 = V_B*k_{21}, rate from compartment 1 to 2.
        % Mean values from Powers xlsx "Final Normals WB PET PVC & ETS"
        k1 = 3.946/60
        k2 = 0.3093/60
        k3 = 0.1862/60
        k4 = 0.01382/60
        u0 = 4.038 % for tscCounts
        v1 = 0.0190
        
        sk1 = 1.254/60
        sk2 = 0.4505/60
        sk3 = 0.1093/60
        sk4 = 0.004525/60
        
        sessionData
        Ca     
        xLabel = 'times/s'
        yLabel = 'activity'
        notes
        
        dta
        tsc
    end
    
    properties (Dependent)
        baseTitle
        detailedTitle
        mapParams 
        parameters
    end
    
    methods %% GET
        function bt = get.baseTitle(this)
            if (isempty(this.sessionData))
                bt = sprintf('%s %s', class(this), pwd);
                return
            end
            bt = sprintf('%s %s', class(this), this.sessionData.sessionFolder);
        end
        function dt = get.detailedTitle(this)
            dt = sprintf('%s\nfu %g, k1 %g, k2 %g, k3 %g, k4 %g, u0 %g, v1 %g\n%S', ...
                         this.baseTitle, ...
                         this.fu, this.k1, this.k2, this.k3, this.k4, this.u0, this.v1, this.notes);
        end
        function m  = get.mapParams(this)
            m = containers.Map;
            N = 5;
            
            % From Powers xlsx "Final Normals WB PET PVC & ETS"
            m('fu') = struct('fixed', 0, 'min', 1e-2,                              'mean', this.fu, 'max', 1e2);  
            m('k1') = struct('fixed', 1, 'min', max(1.4951/60    - N*this.sk1, 0), 'mean', this.k1, 'max',   6.6234/60   + N*this.sk1);
            m('k2') = struct('fixed', 1, 'min', max(0.04517/60   - N*this.sk2, 0), 'mean', this.k2, 'max',   1.7332/60   + N*this.sk2);
            m('k3') = struct('fixed', 1, 'min', max(0.05827/60   - N*this.sk3, 0), 'mean', this.k3, 'max',   0.41084/60  + N*this.sk3);
            m('k4') = struct('fixed', 1, 'min', max(0.0040048/60 - N*this.sk4, 0), 'mean', this.k4, 'max',   0.017819/60 + N*this.sk4);
            m('u0') = struct('fixed', 0, 'min', 0,                                 'mean', this.u0, 'max', 100);  
            m('v1') = struct('fixed', 1, 'min', 0.01,                              'mean', this.v1, 'max',   0.1);  
        end
        function p  = get.parameters(this)
            p   = [this.finalParams('fu'), this.finalParams('k1'), this.finalParams('k2'), ...
                   this.finalParams('k3'), this.finalParams('k4'), this.finalParams('u0'), this.finalParams('v1')]; 
        end
    end
    
    methods (Static)
        function [this,kmin,k1k3overk2k3] = runYi(Ca, t, qpet, notes)
            assert(isnumeric(Ca));
            assert(isnumeric(t));
            assert(isnumeric(qpet));
            assert(length(Ca) == length(t) && length(t) == length(qpet));
            
            this           = mlpowers.F18DeoxyGlucoseKinetics({t}, {qpet});
            Ca(Ca < 0)     = 0;
            this.Ca        = Ca;
            this.notes     = notes;
            this.showPlots = true;
            this           = this.estimateParameters;
            this.plotTimeSamples
            
            kmin         = 60*[this.k1 this.k2 this.k3 this.k4];
            k1k3overk2k3 = kmin(1)*kmin(3)/(kmin(2) + kmin(3));
            fprintf('\n[k_1 ... k_4] / min^{-1} -> %s\n', mat2str(kmin));
            fprintf('frac{k_1 k_3}{k_2 + k_3} / min^{-1} -> %s\n', mat2str(k1k3overk2k3));
        end
        function [this,kmin,k1k3overk2k3] = runPowers(sessDat)
            import mlpet.*;
            cd(sessDat.sessionPath);
            %sessDat.fslmerge_t;
            dta = DTA.loadSessionData(sessDat);
            tsc = TSC.import(sessDat.tsc_fqfn);
            
            tic
            [t,z1,z2]      = mlpowers.F18DeoxyGlucoseKinetics.interpolateAll(dta.times, dta.becquerels, tsc.times, tsc.becquerels);
            this           = mlpowers.F18DeoxyGlucoseKinetics({ t }, { z2 });
            this.dta       = dta;
            this.tsc       = tsc;
            this.v1        = sessDat.v1;
            this.k1        = sessDat.k1;
            this.k2        = sessDat.k2;
            this.k3        = sessDat.k3;
            this.k4        = sessDat.k4;
            this.Ca        = z1;
            this.showAnnealing = false;
            this.showBeta  = false;
            this.showPlots = true;
            this           = this.estimateParameters;
            this.plot;
            
            kmin         = 60*[this.k1 this.k2 this.k3 this.k4];
            k1k3overk2k3 = kmin(1)*kmin(3)/(kmin(2) + kmin(3));
            fprintf('\n%s is working in %s\n', mfilename, sessDat.sessionPath);
            fprintf('[k_1 ... k_4] / min^{-1} -> %s\n', mat2str(kmin));
            fprintf('chi = frac{k_1 k_3}{k_2 + k_3} / min^{-1} -> %s\n', mat2str(k1k3overk2k3));
            fprintf('Kd = K_1 = V_B k1 / (mL min^{-1} (100g)^{-1}) -> %s\n', mat2str(100*this.v1*kmin(1)));
            fprintf('CMRglu/[glu] = V_B chi / mL min^{-1} (100g)^{-1} -> %s\n', mat2str((this.v1/0.0105)*k1k3overk2k3));
            toc
            fprintf('\n');
        end
        function [t,z1,z2] = interpolateAll(x1, y1, x2, y2)
            dt = min([timeDifferences(x1) timeDifferences(x2)]) / 2;
            xInf = min([x1 x2]);
            xSup = max([x1 x2]);
            
            t  = xInf:dt:xSup;
            z1 = pchip(x1,y1,t);
            z2 = pchip(x2,y2,t);

            function timeDiffs = timeDifferences(times)
                timeDiffs = times(2:end) - times(1:end-1);
            end
        end
        function outputs = loopChpc(N)
            assert(isnumeric(N));
            studyDat = mlpipeline.StudyDataSingletons.instance('powers');            
            iter = studyDat.createIteratorForSessionData;            
            outputs = cell(1,N);            
            sessDats = cell(1,N);
            n = 0;
            while (iter.hasNext && n < N)
                try
                    n = n + 1;
                    sessDats{n} = iter.next;
                    fprintf('%s:  n->%i, %s\n', mfilename, n, sessDats{n}.sessionPath);
                catch ME
                    handwarning(ME);
                end
            end
                    
            parfor p = 1:N                
                [outputs{p}.fdgk,outputs{p}.kmin,outputs{p}.k1k3overk2k3] = mlpowers.F18DeoxyGlucoseKinetics.runPowers(sessDats{p});
                %studyDat = mlpipeline.StudyDataSingletons.instance('powers');
                %studyDat.saveWorkspace(sessDats{p}.sessionPath); 
            end
        end
        function [outputs,studyDat] = loopSessionsLocally
            studyDat = mlpipeline.StudyDataSingletons.instance('powers');            
            iter = studyDat.createIteratorForSessionData;            
            outputs = {};
            
            while (iter.hasNext)
                try
                    next = iter.next;
                    %studyDat.diaryOn(next.sessionPath);
                    [o.fdgk,o.kmin,o.k1k3overk2k3] = mlpowers.F18DeoxyGlucoseKinetics.runPowers(next);
                    outputs = [outputs o];
                    saveFigures(fullfile(next.sessionPath, 'fig', ''));
                    %studyDat.saveWorkspace(next.sessionPath);
                    %studyDat.diaryOff;
                catch ME
                    handwarning(ME);
                end
            end
        end
        function alpha_ = a(k2, k3, k4)
            k234   = k2 + k3 + k4;
            alpha_ = k234 - sqrt(k234^2 - 4*k2*k4);
            alpha_ = alpha_/2;
        end
        function beta_  = b(k2, k3, k4)
            k234  = k2 + k3 + k4;
            beta_ = k234 + sqrt(k234^2 - 4*k2*k4);
            beta_ = beta_/2;
        end
        function q      = q2(Ca, k1, a, b, k4, t)
            scale = k1/(b - a);
            q = scale * conv((k4 - a)*exp(-a*t) + (b - k4)*exp(-b*t), Ca);
            q = q(1:length(t));
        end
        function q      = q3(Ca, k1, a, b, k3, t)
            scale = k3*k1/(b - a);
            q = scale * conv(exp(-a*t) - exp(-b*t), Ca);
            q = q(1:length(t));
        end
        function q      = qpet(Ca, fu, k1, k2, k3, k4, t, u0, v1)
            import mlpowers.*;
            Ca = fu*v1*Ca;
            a  = F18DeoxyGlucoseKinetics.a(k2, k3, k4);
            b  = F18DeoxyGlucoseKinetics.b(k2, k3, k4);
            q  = F18DeoxyGlucoseKinetics.q2(Ca, k1, a, b, k4, t) + ...
                 F18DeoxyGlucoseKinetics.q3(Ca, k1, a, b, k3, t) + ...
                 Ca;
            q  = F18DeoxyGlucoseKinetics.slide(q, t, u0); 
        end
        function this   = simulateMcmc(Ca, fu, k1, k2, k3, k4, t, u0, v1, mapParams)
            import mlpowers.*;
            qpet = F18DeoxyGlucoseKinetics.qpet(Ca, fu, k1, k2, k3, k4, t, u0, v1);
            this = F18DeoxyGlucoseKinetics({t}, {qpet});
            this.Ca = Ca;
            this.showAnnealing = true;
            this.showBeta = true;
            this.showPlots = true;
            this = this.estimateParameters(mapParams) %#ok<NOPRT>
            this.plotTimeSamples;
        end
    end
    
	methods
 		function this = F18DeoxyGlucoseKinetics(varargin)
 			%% F18DEOXYGLUCOSEKINETICS
 			%  Usage:  this = F18DeoxyGlucoseKinetics() 			
 			
 			this = this@mlkinetics.AbstractKinetics(varargin{:});
            this.expectedBestFitParams_ = ...
                [this.fu this.k1 this.k2 this.k3 this.k4 this.u0 this.v1]';
        end
        
        function this = simulateItsMcmc(this)
            this = mlpowers.F18DeoxyGlucoseKinetics.simulateMcmc( ...
                   this.Ca, this.fu, this.k1, this.k2, this.k3, this.k4, this.times{1}, this.u0, this.v1, this.mapParams);
        end
        function a    = itsA(this)
            a = mlpowers.F18DeoxyGlucoseKinetics.a(this.k2, this.k3, this.k4);
        end
        function b    = itsB(this)
            b = mlpowers.F18DeoxyGlucoseKinetics.b(this.k2, this.k3, this.k4);
        end
        function q2   = itsQ2(this)
            q2 = mlpowers.F18DeoxyGlucoseKinetics.q2(this.Ca, this.k1, this.itsA, this.itsB, this.k4, this.times{1});
        end
        function q3   = itsQ3(this)
            q3 = mlpowers.F18DeoxyGlucoseKinetics.q3(this.Ca, this.k1, this.itsA, this.itsB, this.k3, this.times{1});
        end
        function qpet = itsQpet(this)
            qpet = mlpowers.F18DeoxyGlucoseKinetics.qpet( ...
                this.Ca, this.fu, this.k1, this.k2, this.k3, this.k4, this.times{1}, this.u0, this.v1);
        end
        function this = estimateParameters(this, varargin)
            ip = inputParser;
            addOptional(ip, 'mapParams', this.mapParams, @(x) isa(x, 'containers.Map'));
            parse(ip, varargin{:});
            
            this = this.runMcmc(ip.Results.mapParams, {'fu' 'k1' 'k2' 'k3' 'k4' 'u0' 'v1'});
        end
        function ed   = estimateDataFast(this, fu, k1, k2, k3, k4, u0, v1)
            ed{1} = this.qpet(this.Ca, fu, k1, k2, k3, k4, this.times{1}, u0, v1);
        end
        function ps   = adjustParams(this, ps)
            theParams = this.theParameters;
            if (ps(theParams.paramsIndices('k4')) > ps(theParams.paramsIndices('k3')))
                tmp                               = ps(theParams.paramsIndices('k3'));
                ps(theParams.paramsIndices('k3')) = ps(theParams.paramsIndices('k4'));
                ps(theParams.paramsIndices('k4')) = tmp;
            end
        end
        
        function plot(this, varargin)
            figure;
            max_Ca    = max(this.Ca);
            max_data1 = max([max(this.dependentData{1}) max(this.itsQpet)]);
            plot(this.dta.times, this.dta.becquerels/max_Ca, ':o',  ...
                 this.times{1},  this.itsQpet       /max_data1, ...
                 this.tsc.times, this.tsc.becquerels/max_data1, ':s', varargin{:});
            legend('data Ca', 'Bayesian qpet', 'data qpet');  
            title(this.detailedTitle, 'Interpreter', 'none');
            xlabel(this.xLabel);
            ylabel(sprintf('%s; rescaled by %g, %g', this.yLabel,  max_Ca, max_data1));
        end
        function plotTimeSamples(this, varargin)
            figure;
            max_Ca    = max(this.Ca);
            max_data1 = max(max(this.dependentData{1}), max(this.itsQpet));
            plot(1:length(this.times{1}), this.Ca              /max_Ca, ':o',  ...
                 1:length(this.times{1}), this.itsQpet         /max_data1, ...
                 1:length(this.times{1}), this.dependentData{1}/max_data1, ':s', varargin{:});
            legend('data Ca', 'Bayesian qpet', 'data qpet');  
            title(this.detailedTitle, 'Interpreter', 'none');
            xlabel('time sample index (cardinal)');
            ylabel(sprintf('%s; rescaled by %g, %g', this.yLabel,  max_Ca, max_data1));
        end
        function plotParVars(this, par, vars)
            assert(lstrfind(par, properties(this)));
            assert(isnumeric(vars));
            switch (par)
                case 'k1'
                    for v = 1:length(vars)
                        args{v} = { vars(v) this.k2 this.k3 this.k4 this.u0 this.v1}; 
                    end
                case 'k2'
                    for v = 1:length(vars)
                        args{v} = { this.k1 vars(v) this.k3 this.k4 this.u0 this.v1}; 
                    end
                case 'k3'
                    for v = 1:length(vars)
                        args{v} = { this.k1 this.k2 vars(v) this.k4 this.u0  this.v1}; 
                    end
                case 'k4'
                    for v = 1:length(vars)
                        args{v} = { this.k1 this.k2 this.k3 vars(v) this.u0 this.v1}; 
                    end
                case 'u0'
                    for v = 1:length(vars)
                        args{v} = { this.k1 this.k2 this.k3 this.k4 vars(v) this.v1};
                    end
                case 'v1'
                    for v = 1:length(vars)
                        args{v} = { this.k1 this.k2 this.k3 this.k4 this.u0 vars(v)}; 
                    end
            end
            this.plotParArgs(par, args, vars);
        end
 	end 
    
    %% PRIVATE
    
    methods (Access = 'private')
        function plotParArgs(this, par, args, vars)
            assert(lstrfind(par, properties('mlpowers.F18DeoxyGlucoseKinetics')));
            assert(iscell(args));
            assert(isnumeric(vars));
            figure
            hold on
            plot(0:length(this.Ca)-1, this.Ca, 'o')
            for v = 1:length(args)
                argsv = args{v};
                plot(0:length(this.Ca)-1, ...
                     mlpowers.F18DeoxyGlucoseKinetics.qpet( ...
                         this.Ca, argsv{1}, argsv{2}, argsv{3}, argsv{4}, this.times{1}, argsv{5}, argsv{6}));
            end
            plot(0:length(this.Ca)-1, this.dependentData{1}, 'LineWidth', 2);
            title(sprintf('k1 %g, k2 %g, k3 %g, k4 %g, u0 %g, v1 %g', ...
                          argsv{1}, argsv{2}, argsv{3}, argsv{4}, argsv{5}, argsv{6}));
            legend(['idaif' ...
                    cellfun(@(x) sprintf('%s = %g', par, x), num2cell(vars), 'UniformOutput', false) ...
                    'WB']);
            xlabel('time sampling index');
            ylabel(this.yLabel);
        end
    end
    
	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

