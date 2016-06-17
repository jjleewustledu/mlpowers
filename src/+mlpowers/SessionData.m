classdef SessionData < mlpet.SessionData
	%% SESSIONDATA  

	%  $Revision$
 	%  was created 31-Mar-2016 18:56:06
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.341360 (R2016a) for MACI64.
 	

	properties (Dependent)
        aparcA2009sAseg_fqfn
        brain_fqfn
        mpr_fqfn
        orig_fqfn
        pet_fqfns
        petfov_fqfn
        T1_fqfn
        wmparc_fqfn
        
        dta_fqfn
        mask_fqfn
        pet_fqfn
        tsc_fqfn
        
        v1
        k1
        k2
        k3
        k4
    end
    
    methods %% GET 
        function g = get.aparcA2009sAseg_fqfn(this)
            g = fullfile(this.mriPath, 'aparc.a2009s+aseg.mgz');
            if (2 ~= exist(g, 'file'))
                g = '';
                return
            end
        end
        function g = get.brain_fqfn(this)
            g = fullfile(this.mriPath, 'brain.mgz');
        end
        function g = get.mpr_fqfn(this)
            g = fullfile(this.fslPath, this.studyData_.mpr_fn(this));
            g = this.ensureNIFTI_GZ(g);
            if (2 ~= exist(g, 'file'))
                g = '';
                return
            end
        end
        function g = get.orig_fqfn(this)
            g = fullfile(this.mriPath, 'orig.mgz');
        end
        function g = get.pet_fqfns(this)
            fqfns = { this.fdg_fqfn this.ho_fqfn this.oc_fqfn this.oo_fqfn };
            g = {};
            for f = 1:length(fqfns)
                if (2 == exist(fqfns{f}, 'file'))
                    g = [g fqfns{f}];
                end
            end
        end
        function g = get.petfov_fqfn(this)
            g = fullfile(this.petPath, this.studyData_.petfov_fn(this.tag));
            g = this.ensureNIFTI_GZ(g);
            if (2 ~= exist(g, 'file'))
                g = '';
                return
            end
        end
        function g = get.T1_fqfn(this)
            g = fullfile(this.mriPath, 'T1.mgz');
        end
        function g = get.wmparc_fqfn(this)
            g = fullfile(this.mriPath, 'wmparc.mgz');
        end        
        
        function f = get.dta_fqfn(this)
            f = fullfile(this.petPath, [this.pnumber 'fdg1.dta']);
        end
        function f = get.mask_fqfn(this) %#ok<MANU>
            f = '';
        end
        function f = get.pet_fqfn(this)
            f = this.fdg_fqfn;
        end
        function f = get.tsc_fqfn(this)
            f = fullfile(this.petPath, [this.pnumber 'wb_fdg_pvc.tsc']);
        end
        function v = get.v1(this)
            v = this.pnum2v1_(this.pnumber);
        end
        function v = get.k1(this)
            v = this.pnum2k1_(this.pnumber);
        end
        function v = get.k2(this)
            v = this.pnum2k2_(this.pnumber);
        end
        function v = get.k3(this)
            v = this.pnum2k3_(this.pnumber);
        end
        function v = get.k4(this)
            v = this.pnum2k4_(this.pnumber);
        end
    end

	methods 
        function f = fdg_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petPath, sprintf('%sfdg%s%s', this.sessionFolder, this.tag, ip.Results.tag));
        end
        function f = ho_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petPath, sprintf('%sho%i%s%s', this.sessionFolder, this.snumber, this.tag, ip.Results.tag));
        end
        function f = oc_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petPath, sprintf('%soc%i%s%s', this.sessionFolder, this.snumber, this.tag, ip.Results.tag));
        end
        function f = oo_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petPath, sprintf('%soo%i%s%s', this.sessionFolder, this.snumber, this.tag, ip.Results.tag));
        end
        
        function g = aparcA2009sAseg(this)
            g = mlmr.MRImagingContext(this.aparcA2009sAseg_fqfn);
        end
        function g = brain(this)
            g = mlmr.MRImagingContext(this.brain_fqfn);
        end
        function g = fdg(this)
            import mlpet.*;
            if (lexist(this.fdg_fqfn('_resolved')))
                g = PETImagingContext(this.fdg_fqfn('_resolved'));
                return
            end
            g = this.flipAndCropImaging(PETImagingContext(this.fdg_fqfn));
        end
        function g = ho(this)
            import mlpet.*;
            if (lexist(this.ho_fqfn('_resolved')))
                g = PETImagingContext(this.ho_fqfn('_resolved'));
                return
            end
            g = this.flipAndCropImaging(PETImagingContext(this.ho_fqfn));
        end
        function g = mpr(this)
            g = this.flipAndCropImaging(mlmr.MRImagingContext(this.mpr_fqfn));
        end
        function g = oc(this)
            import mlpet.*;
            if (lexist(this.oc_fqfn('_resolved')))
                g = PETImagingContext(this.oc_fqfn('_resolved'));
                return
            end
            g = this.flipAndCropImaging(PETImagingContext(this.oc_fqfn));
        end
        function g = oo(this)
            import mlpet.*;
            if (lexist(this.oo_fqfn('_resolved')))
                g = PETImagingContext(this.oo_fqfn('_resolved'));
                return
            end
            g = this.flipAndCropImaging(PETImagingContext(this.oo_fqfn));
        end
        function g = orig(this)
            g = mlmr.MRImagingContext(this.orig_fqfn);
        end
        function g = petAtlas(this)
            g = mlpet.PETImagingContext(this.pet_fqfns);
            g = g.atlas;
        end
        function g = petfov(this)
            g = mlfourd.ImagingContext(this.petfov_fqfn);
        end      
        function p = petPointSpread(~)
            %% PETPOINTSPREAD
            %  The fwhh at 1cm from axis was measured by:
            %  Delso, Fuerst Jackoby, et al.  Performance Measurements of the Siemens mMR Integrated Whole-Body PET/MR
            %  Scanner.  J Nucl Med 2011; 52:1?9.
            
            p = mlpet.PETRegistry.instance.petPointSpread;
        end
        function g = T1(this)
            g = mlmr.MRImagingContext(this.T1_fqfn);
        end
        function g = wmparc(this)
            g = mlmr.MRImagingContext(this.wmparc_fqfn);
        end                
		  
 		function this = SessionData(varargin)
 			%% SESSIONDATA
 			%  Usage:  this = SessionData()

 			this = this@mlpet.SessionData(varargin{:});
            
            %% KLUDGE for v1
            pnums = {5999	6004	6012	6021	6042	6047	6146	6154	6169	6173	6189	6248	6253	6320	6324	6352	6354	6359	6374	6381	6389	6517	6606};
            pnums = cellfun(@(x) ['p' num2str(x)], pnums, 'UniformOutput', false);
            v1s   = num2cell(0.0105 * ...
                [3.397 4.182 4.134 3.588 3.678 3.484 3.619 4.445 3.038 4.34 3.899 4.019 4.459 3.766 3.43 3.189 3.151 3.275 3.55 3.18 3.33 3.944 1.901]);
            k1s   = num2cell((1/60) * ...
                [2.075	1.9777	2.0735	2.2165	2.252	1.6816	2.4214	1.4951	1.9786	1.6972	2.0609	3.1338	1.9936	2.417	2.2332	1.707	2.527	3.3827	6.6234	5.4782	1.9928	1.6265	3.9461]);
            k2s   = num2cell((1/60) * ...
                [0.12	0.19819	0.148844	0.14499	0.20832	0.1296	0.25279	0.1457	0.16015	0.096899	0.13304	0.37079	0.16917	0.25217	0.11482	0.17539	0.23162	0.46677	1.7003	1.7332	0.083581	0.04517	0.30926]);
            k3s   = num2cell((1/60) * ...
                [0.11595	0.11174	0.15101	0.05827	0.13991	0.13035	0.2219	0.113	0.19051	0.13337	0.18272	0.14293	0.10435	0.11566	0.10961	0.2177	0.13216	0.23739	0.41084	0.55035	0.12549	0.076241	0.18617]);
            k4s   = num2cell((1/60) * ...
                [0.015307	0.010438	0.012118	0.0076479	0.014497	0.016777	0.012545	0.016886	0.012516	0.017819	0.012528	0.0095034	0.0040048	0.0055588	0.016185	0.0097882	0.0068367	0.015909	0.0093596	0.013618	0.023537	0.0084547	0.013817]);

            this.pnum2v1_ = containers.Map(pnums, v1s);
            this.pnum2k1_ = containers.Map(pnums, k1s);
            this.pnum2k2_ = containers.Map(pnums, k2s);
            this.pnum2k3_ = containers.Map(pnums, k3s);
            this.pnum2k4_ = containers.Map(pnums, k4s);
        end
    end 
    
    properties (Access = private)
        pnum2v1_
        pnum2k1_
        pnum2k2_
        pnum2k3_
        pnum2k4_
    end
    
	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

