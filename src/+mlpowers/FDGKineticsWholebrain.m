classdef FDGKineticsWholebrain < mlpowers.F18DeoxyGlucoseKinetics
	%% FDGKINETICSWHOLEBRAIN  

	%  $Revision$
 	%  was created 17-Feb-2017 07:19:33
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.1.0.441655 (R2016b) for MACI64.  Copyright 2017 John Joowon Lee.
 	

	properties (Constant)
        REUSE_APARCASEG = true
 		REUSE_BRAINMASK = true
        HCTS = [35.7 33.6 33.4 34.5 41.4; ...
                31.3 32.5 34.6 34.5 37.2];
 	end

	methods
 		function this = FDGKineticsWholebrain(varargin)
 			%% FDGKINETICSWHOLEBRAIN
 			%  Usage:  this = FDGKineticsWholebrain()

 			this = this@mlpowers.F18DeoxyGlucoseKinetics(varargin{:});
            this.sessionData.parcellation = 'wholebrain';
 		end
    end 

    methods (Static)
        function jobs = godoChpcPart(varargin)
            diary on   
            
            import mlpowers.*;
            ip = inputParser;
            addOptional(ip, 'dirToolArg', 'HYGLY2*', @ischar);
            addOptional(ip, 'vs', 1:2, @isnumeric);
            addParameter(ip, 'hcts', FDGKineticsWholebrain.HCTS, @isnumeric); 
            parse(ip, varargin{:});                     
            
            studyd = StudyData;
            pwd0   = pushd(studyd.subjectsDir);
            dth    = mlsystem.DirTool(ip.Results.dirToolArg);
            hcts   = ip.Results.hcts;
            jobs   = {};
            c      = myparcluster;
            for d = 1:length(dth.dns)
                datobj.sessionFolder = dth.dns{d};
                for v = ip.Results.vs
                    datobj.vnumber = v;
                    datobj.hct = hcts(v,d);
                    try
                        pwd1 = pushd(fullfile(dth.dns{d}, sprintf('V%i', v), ''));
                        %CHPC.pushToChpc(datobj);
                        j = c.batch(@mlpowers.FDGKineticsWholebrain.godo3, 1, {datobj});
                        jobs = [jobs j]; %#ok<AGROW>
                        popd(pwd1);
                    catch ME
                        dispwarning(ME);                        
                    end
                end
            end
            popd(pwd0);
            
            diary off
        end
        function sessions = godoWilliam
            tic 
            
            import mlpowers.*;
            studyd   = StudyDataSingleton.instance('initialize');
            pwd0     = pushd(studyd.subjectsDir); 
            sessions = {};            
            iter     = studyd.createIteratorForSessionData;
            v        = 0;
            while (iter.hasNext)
                v = v + 1;
                try
                    sessions{v} = FDGKineticsWholebrain.godo2(iter.next); %#ok<AGROW>
                catch ME
                    handwarning(ME);
                end
            end            
            popd(pwd0);
            
            toc
        end
        function goPlotOnWilliam
            import mlpowers.*;
            studyd = StudyDataSingleton.instance('initialize');
            pwd0   = pushd(studyd.subjectsDir);
            iter   = studyd.createIteratorForSessionData;
            while (iter.hasNext)
                try
                    sessd = iter.next;
                    FDGKineticsWholebrain.godoPlots(sessd);
                catch ME
                    handwarning(ME);
                end
            end            
            popd(pwd0);
        end
        function goWritetable 
            import mlpowers.*;
            studyd = StudyDataSingleton.instance('initialize');
            pwd0   = pushd(studyd.subjectsDir);
            iter   = studyd.createIteratorForSessionData;
            fqfp   = fullfile(pwd0, sprintf('mlpowers_FDGKineticsWholebrain_goWritetable_%s', datestr(now, 30)));
            v      = 0;
            while (iter.hasNext)
                v = v + 1;
                try
                    sessd = iter.next;
                    pwd1 = pushd(sessd.sessionPath);
                    this = FDGKineticsWholebrain.load('mlpowers_FDGKineticsWholebrain_wholebrain.mat');
                    this.writetable('fqfp', fqfp, 'Range', sprintf('A%i:U%i', v+2, v+2), 'writeHeader', 1==v);
                    popd(pwd1);
                catch ME
                    handwarning(ME);
                end
            end            
            popd(pwd0);
        end
        function summary = godo2(sessd)
            try
                import mlpowers.*;
                pwd0 = pushd(sessd.sessionPath);
                this = FDGKineticsWholebrain(sessd, 'mask', []);
                summary.(sessd.sessionFolder) = this.doItsBayes;
                fprintf('FDGKineticsWholebrain.godo2:  returned from doItsBayes\n');
                popd(pwd0);
            catch ME
                dispwarning(ME);
            end
        end
        function state = godo(sessd)
            try
                import mlpowers.*;
                [m,sessd] = FDGKineticsWholebrain.godoMasks(sessd);
                assert(isdir(sessd.vLocation));
                pwd0 = pushd(sessd.vLocation);
                this = FDGKineticsWholebrain(sessd, 'mask', m);
                state = this.doItsBayes;
                popd(pwd0);
            catch ME
                handwarning(ME);
            end
        end
        function godoPlots(sessd)
            try
                import mlpowers.*;
                assert(isdir(sessd.vLocation));
                pwd0 = pushd(sessd.vLocation);
                this = FDGKineticsWholebrain.load( ...
                    fullfile(sessd.vLocation, sprintf('mlpowers_FDGKineticsWholebrain_%s', sessd.parcellation)), 'this');
                this.plot;
                saveFigures(sprintf('fig_%s_wholebrain', strrep(class(this), '.','_')));
                popd(pwd0);
            catch ME
                handwarning(ME);
            end
        end
        function [m, sessd,ct4rb] = godoMasks(sessd)
            assert(isa(sessd, 'mlpowers.SessionData'));
            try
                import mlpowers.*;
                assert(isdir(sessd.vLocation));
                pwd0 = pushd(sessd.vLocation);
                [~,msktn] = FDGKineticsWholebrain.mskt(sessd);
                [~,ct4rb] = FDGKineticsWholebrain.brainmaskBinarized(sessd, msktn);                
                m = FDGKineticsWholebrain.aparcAsegBinarized(sessd, ct4rb);
                sessd.selectedMask = [m.fqfp '.4dfp.ifh'];
                popd(pwd0);
            catch ME
                handwarning(ME);
            end
        end 
        
        function [m,n] = mskt(sessd)
            import mlfourdfp.*;
            f = [sessd.tracerResolved1('typ','fqfp') '_sumt'];
            f1 = mybasename(FourdfpVisitor.ensureSafeFileprefix(f));
            if (lexist([f1 '_mskt.4dfp.ifh'], 'file') && lexist([f1 '_msktNorm.4dfp.ifh'], 'file'))
                m = mlfourd.ImagingContext([f1 '_mskt.4dfp.ifh']);
                n = mlfourd.ImagingContext([f1 '_msktNorm.4dfp.ifh']);
                return
            end
            
            lns_4dfp(f, f1);
            
            ct4rb = CompositeT4ResolveBuilder('sessionData', sessd);
            ct4rb.msktgenImg(f1);          
            m = mlfourd.ImagingContext([f1 '_mskt.4dfp.ifh']);
            n = m.numericalNiftid;
            n.img = n.img/n.dipmax;
            n.fileprefix = [f1 '_msktNorm'];
            n.filesuffix = '.4dfp.ifh';
            n.save;
            n = mlfourd.ImagingContext(n);
        end
        function [b,ct4rb] = brainmaskBinarized(sessd, msktNorm)
            fdgSumt = mlpet.PETImagingContext(sessd.tracerResolvedSumt1('typ','fqfn'));
            if (~lexist([sessd.tracerResolvedSumt1('typ','fp') '_brain.4dfp.ifh'], 'file'))
                fnii = fdgSumt.numericalNiftid;
                msktNorm = mlfourd.ImagingContext(msktNorm);
                mnii = msktNorm.numericalNiftid;
                fnii = fnii.*mnii;
                fdgSumt = mlpet.PETImagingContext(fnii);
                fdgSumt.filepath = pwd;
                fdgSumt.fileprefix = [sessd.tracerResolvedSumt1('typ','fp') '_brain'];
                fdgSumt.filesuffix = '.4dfp.ifh';
                fdgSumt.save;
            end
            
            brainmask = mlfourd.ImagingContext(sessd.brainmask);
            if (~lexist('brainmask.4dfp.ifh', 'file'))
                brainmask.fourdfp;
                brainmask.filepath = pwd;
                brainmask.save;
                if (lexist('brainmask.nii')); gzip('brainmask.nii'); end
            end
            
            ct4rb = mlfourdfp.CompositeT4ResolveBuilder( ...
                'sessionData', sessd, ...
                'theImages', {fdgSumt.fileprefix brainmask.fileprefix});
            if (mlpowers.FDGKineticsWholebrain.REUSE_BRAINMASK && ...
                lexist(['brainmaskBinarizeBlended_' ct4rb.resolveTag '.4dfp.ifh'], 'file'))
                b = mlpet.PETImagingContext(['brainmaskBinarizeBlended_' ct4rb.resolveTag '.4dfp.ifh']);
                return
            end
            ct4rb = ct4rb.resolve;
            b = ct4rb.product{2};
            b.numericalNiftid;
            b.saveas(['brainmask_' ct4rb.resolveTag '.4dfp.ifh']);
            b = b.binarizeBlended;
            b.saveas(['brainmaskBinarizeBlended_' ct4rb.resolveTag '.4dfp.ifh']);
        end
        function aa = aparcAsegBinarized(sessd, ct4rb)
            if (mlpowers.FDGKineticsWholebrain.REUSE_APARCASEG && ...
                lexist('aparcAsegBinarized_op_fdg.4dfp.ifh', 'file'))
                aa = mlpet.PETImagingContext('aparcAsegBinarized_op_fdg.4dfp.ifh');
                return
            end
            
            aa = sessd.aparcAseg('typ', 'mgz');
            aa = sessd.mri_convert(aa, 'aparcAseg.nii.gz');
            aa = mybasename(aa);
            sessd.nifti_4dfp_4(aa);
            aa = ct4rb.t4img_4dfp( ...
                sessd.brainmask('typ','fp'), aa, 'opts', '-n');
            aa = mlpet.PETImagingContext([aa '.4dfp.ifh']);
            nn = aa.numericalNiftid;
            nn.saveas(['aparcAseg_' ct4rb.resolveTag '.4dfp.ifh']);
            nn = nn.binarized; % set threshold to intensity floor
            nn.saveas(['aparcAsegBinarized_' ct4rb.resolveTag '.4dfp.ifh']);
            aa = mlfourd.ImagingContext(nn);
        end
        
        function teardown(sessd)
        end
    end
    
	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

