classdef SessionData < mlpet.SessionData
	%% SESSIONDATA  

	%  $Revision$
 	%  was created 31-Mar-2016 18:56:06
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.341360 (R2016a) for MACI64.
 	

    properties 
        filetypeExt = '.nii.gz'
    end
    
	properties (Dependent)
        petBlur
        v1
        k1
        k2
        k3
        k4
        hct
    end
    
    methods %% GET 
        function g = get.petBlur(~)
            g = mlpet.PETRegistry.instance.petPointSpread;
            g = mean(g)*ones(1,3);
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
        function v = get.hct(this)
            v = this.pnum2hct_(this.pnumber);
        end
    end

	methods
 		function this = SessionData(varargin)
 			%% SESSIONDATA
 			%  @param [param-name, param-value[, ...]]
            %         'ac'          is logical
            %         'rnumber'     is numeric
            %         'sessionPath' is a path to the session data
            %         'studyData'   is a mlpipeline.StudyDataHandle
            %         'snumber'     is numeric
            %         'tracer'      is char
            %         'vnumber'     is numeric
            %         'tag'         is appended to the fileprefix

 			this = this@mlpet.SessionData(varargin{:});
            this.attenuationCorrected_ = true;
            
            %% KLUDGE for v1
            pnums = {5999	6004	6012	6021	6042	6047	6146	6154	6169	6173	6189	6248	6253	6320	6324	6352	6354	6359	6374	6381	6389	6517	6606};
            pnums = cellfun(@(x) ['p' num2str(x)], pnums, 'UniformOutput', false);
            %0.0105 *
            v1s   = num2cell(0.01* ...
                [3.397 4.182 4.134 3.588 3.678 3.484 3.619 4.445 3.038 4.34 3.899 4.019 4.459 3.766 3.43 3.189 3.151 3.275 3.55 3.18 3.33 3.944 1.901]);
            k1s   = num2cell((1/60) * ...
                [2.075	1.9777	2.0735	2.2165	2.252	1.6816	2.4214	1.4951	1.9786	1.6972	2.0609	3.1338	1.9936	2.417	2.2332	1.707	2.527	3.3827	6.6234	5.4782	1.9928	1.6265	3.9461]);
            k2s   = num2cell((1/60) * ...
                [0.12	0.19819	0.148844	0.14499	0.20832	0.1296	0.25279	0.1457	0.16015	0.096899	0.13304	0.37079	0.16917	0.25217	0.11482	0.17539	0.23162	0.46677	1.7003	1.7332	0.083581	0.04517	0.30926]);
            k3s   = num2cell((1/60) * ...
                [0.11595	0.11174	0.15101	0.05827	0.13991	0.13035	0.2219	0.113	0.19051	0.13337	0.18272	0.14293	0.10435	0.11566	0.10961	0.2177	0.13216	0.23739	0.41084	0.55035	0.12549	0.076241	0.18617]);
            k4s   = num2cell((1/60) * ...
                [0.015307	0.010438	0.012118	0.0076479	0.014497	0.016777	0.012545	0.016886	0.012516	0.017819	0.012528	0.0095034	0.0040048	0.0055588	0.016185	0.0097882	0.0068367	0.015909	0.0093596	0.013618	0.023537	0.0084547	0.013817]);
            hcts  = num2cell(...
                [41.5 37.5 36.5 33.5 34.5 42.5 40.3 42.0 35.5 36.0 34.5 36.5 35.0 41.0 37.0 32.0 38.9 36.0 41.5 41.5 38.0 41.0 41.0]);
            
            this.pnum2v1_ = containers.Map(pnums, v1s);
            this.pnum2k1_ = containers.Map(pnums, k1s);
            this.pnum2k2_ = containers.Map(pnums, k2s);
            this.pnum2k3_ = containers.Map(pnums, k3s);
            this.pnum2k4_ = containers.Map(pnums, k4s);
            this.pnum2hct_ = containers.Map(pnums, hcts);
            
            this.parcellation = 'wholebrain';
        end
        
        %% IMRData
        
        function g = mpr(this)
            g = this.flipAndCropImaging(mlmr.MRImagingContext(this.mpr_fqfn));
        end
                
        %% IPETData
		  
        function loc = petLocation(this, varargin)
            loc = fullfile(this.vLocation(varargin{:}), 'pet', '');
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
        function p = petPointSpread(~)
            p = mlpet.PETRegistry.instance.petPointSpread;
        end
    end 
    
    %% PRIVATE
    
    properties (Access = private)
        pnum2v1_
        pnum2k1_
        pnum2k2_
        pnum2k3_
        pnum2k4_
        pnum2hct_
    end
    
    %% DEPRECATED, HIDDEN
    
    methods (Hidden)
        function a = aparcAsegBinarized(~, varargin)
            a = [];
        end
        function f = dta_fqfn(this)
            f = fullfile(this.petLocation, [this.pnumber 'fdg1.dta']);
        end
        function f = fdg_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petLocation, sprintf('%sfdg%s%s', this.sessionFolder, this.tag, ip.Results.tag));
        end
        function f = ho_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petLocation, sprintf('%sho%i%s%s', this.sessionFolder, this.snumber, this.tag, ip.Results.tag));
        end
        function f = mpr_fqfn(this) %#ok<STOUT,MANU>
            error('mlpowers:notImplemented', 'SessionData.mpr_fqfn');
        end
        function f = oc_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petLocation, sprintf('%soc%i%s%s', this.sessionFolder, this.snumber, this.tag, ip.Results.tag));
        end
        function f = oo_fqfn(this, varargin)
            ip = inputParser;
            addOptional(ip, 'tag', '', @ischar);
            parse(ip, varargin{:})
            
            f = this.fullfile(this.petLocation, sprintf('%soo%i%s%s', this.sessionFolder, this.snumber, this.tag, ip.Results.tag));
        end 
        function f = tsc_fqfn(this)
            f = fullfile(this.petLocation, [this.pnumber 'wb_fdg_pvc.tsc']);
        end
    end
    
	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

