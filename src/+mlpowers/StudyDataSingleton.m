classdef StudyDataSingleton < mlpipeline.StudyDataSingleton
	%% STUDYDATASINGLETON  

	%  $Revision$
 	%  was created 31-Mar-2016 17:35:26
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.341360 (R2016a) for MACI64.
 	

    methods (Static)
        function this  = instance(varargin)
            persistent instance_         
            if (~isempty(varargin))
                instance_ = [];
            end          
            if (isempty(instance_))
                instance_ = mlpowers.StudyDataSingleton(varargin{:});
            end
            this = instance_;
        end
        function [s,r] = fslmerge_fslchfiletype(prefix)
            folder = sprintf('%s_frames', prefix);
            assert(lexist(folder, 'file'));
            pwd0 = pwd;
            cd(folder);
            
            fprintf('mlpowers.StudyDataSingleton.fslmerge_fslchfiletype is working in %s.\n', pwd);
            [s,r] = mlbash(sprintf('fslmerge -t %s %s_*.hdr', prefix, prefix)); %#ok<ASGLU>
            [s,r] = mlbash(sprintf('fslchfiletype NIFTI_GZ %s.hdr', prefix));
            
            cd(pwd0);
        end
        function d     = subjectsDir
            d = fullfile(getenv('POWERS'), 'np497', 'jjlee', '');
        end
    end
    
    methods
        function        register(this, varargin)
            %% REGISTER this class' persistent instance with mlpipeline.StudyDataSingletons
            %  using the latter class' register methods.
            %  @param key is any registration key stored by mlpipeline.StudyDataSingletons; default 'derdeyn'.
            
            ip = inputParser;
            addOptional(ip, 'key', 'powers', @ischar);
            parse(ip, varargin{:});
            mlpipeline.StudyDataSingletons.register(ip.Results.key, this);
        end
        function this = replaceSessionData(this, varargin)
            %% REPLACESESSIONDATA
            %  @param [parameter name,  parameter value, ...] as expected by mlpowers.SessionData are optional;
            %  'studyData' and this are always internally supplied.
            %  @returns this.

            this.sessionDataComposite_ = mlpatterns.CellComposite({ ...
                mlpowers.SessionData('studyData', this, varargin{:})});
        end
        function sess = sessionData(this, varargin)
            %% SESSIONDATA
            %  @param [parameter name,  parameter value, ...] as expected by mlpowers.SessionData are optional;
            %  'studyData' and this are always internally supplied.
            %  @returns for empty param:  mlpatterns.CellComposite object or it's first element when singleton, 
            %  which are instances of mlpowers.SessionData.
            %  @returns for non-empty param:  instance of mlpowers.SessionData corresponding to supplied params.
            
            if (isempty(varargin))
                sess = this.sessionDataComposite_;
                if (1 == length(sess))
                    sess = sess.get(1);
                end
                return
            end
            sess = mlpowers.SessionData('studyData', this, varargin{:});
        end
        function f    = subjectsDirFqdns(this)
            dt = mlsystem.DirTools(this.subjectsDir);
            f = {};
            for di = 1:length(dt.dns)
                e = regexp(dt.dns{di}, 'M\d{3}', 'match');
                if (~isempty(e))
                    f = [f dt.fqdns(di)]; %#ok<AGROW>
                end
            end
        end 
    end
    
    %% PROTECTED
    
	methods (Access = protected)
 		function this = StudyDataSingleton(varargin)
 			this = this@mlpipeline.StudyDataSingleton(varargin{:});
        end
        function this = assignSessionDataCompositeFromPaths(this, varargin)
            if (isempty(this.sessionDataComposite_))
                for v = 1:length(varargin)
                    if (ischar(varargin{v}) && isdir(varargin{v}))                    
                        this.sessionDataComposite_ = ...
                            this.sessionDataComposite_.add( ...
                                mlpowers.SessionData('studyData', this, 'sessionPath', varargin{v}));
                    end
                end
            end
        end
    end
    
    %% DEPRECATED, HIDDEN
    
    methods (Hidden)          
        function fn = fdg_fn(~, sessDat, varargin)
            ip = inputParser;
            addOptional(ip, 'suff', '', @ischar);
            parse(ip, varargin{:})  
            fn = sprintf('%sfdg1.4dfp.nii.gz', sessDat.sessionFolder);
        end
        function fn = ho_fn(~, sessDat, varargin)
            ip = inputParser;
            addOptional(ip, 'suff', '', @ischar);
            parse(ip, varargin{:})
            fn = sprintf('%sho%i%s.4dfp.nii.gz', sessDat.sessionFolder, sessDat.snumber, ip.Results.suff);
        end
        function fn = mpr_fn(~, sessDat, varargin)
            ip = inputParser;
            addOptional(ip, 'suff', '', @ischar);
            parse(ip, varargin{:})
            fn = sprintf('%s_mprage%s.nii.gz', sessDat.sessionFolder, ip.Results.suff);
        end
        function fn = oc_fn(~, sessDat, varargin)
            ip = inputParser;
            addOptional(ip, 'suff', '', @ischar);
            parse(ip, varargin{:})
            fn = sprintf('%soc%i%s.4dfp.nii.gz', sessDat.sessionFolder, sessDat.snumber, ip.Results.suff);
        end
        function fn = oo_fn(~, sessDat, varargin)
            ip = inputParser;
            addOptional(ip, 'suff', '', @ischar);
            parse(ip, varargin{:})
            fn = sprintf('%soo%i%s.4dfp.nii.gz', sessDat.sessionFolder, sessDat.snumber, ip.Results.suff);
        end 
    end

	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

