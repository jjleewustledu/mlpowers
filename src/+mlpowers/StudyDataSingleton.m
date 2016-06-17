classdef StudyDataSingleton < mlpipeline.StudyDataSingleton
	%% STUDYDATASINGLETON  

	%  $Revision$
 	%  was created 31-Mar-2016 17:35:26
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.341360 (R2016a) for MACI64.
 	

	properties (SetAccess = protected)
        powersTrunk = fullfile(getenv('POWERS'), '')
    end
    
	properties (Dependent)
        subjectsDir
    end
    
    methods %% GET
        function g = get.subjectsDir(this)
            g = fullfile(this.powersTrunk, 'np497', 'jjlee', '');
        end
    end

    methods (Static)
        function this = instance(qualifier)
            persistent instance_            
            if (exist('qualifier','var'))
                assert(ischar(qualifier));
                if (strcmp(qualifier, 'initialize'))
                    instance_ = [];
                end
            end            
            if (isempty(instance_))
                instance_ = mlpowers.StudyDataSingleton();
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
        function        register(varargin)
            %% REGISTER
            %  @param []:  if this class' persistent instance
            %  has not been registered, it will be registered via instance() call to the ctor; if it
            %  has already been registered, it will not be re-registered.
            %  @param ['initialize']:  any registrations made by the ctor will be repeated.
            
            mlpowers.StudyDataSingleton.instance(varargin{:});
        end
    end
    
    methods
        function loc  = loggingLocation(this, varargin)
            ip = inputParser;
            addParameter(ip, 'type', 'path', @(x) this.isLocationType(x));
            parse(ip, varargin{:});
            
            switch (ip.Results.type)
                case 'folder'
                    [~,loc] = fileparts(this.subjectsDir);
                case 'path'
                    loc = this.subjectsDir;
                otherwise
                    error('mlpipeline:insufficientSwitchCases', ...
                          'StudyDataSingleton.loggingLocation.ip.Results.type->%s not recognized', ip.Results.type);
            end
        end
        function sess = sessionData(this, varargin)
            %% SESSIONDATA
            %  @param parameter names and values expected by mlpowers.SessionData;
            %  'studyData' and this are implicitly supplied.
            %  @returns mlpowers.SessionData object
            
            sess = mlpowers.SessionData('studyData', this, varargin{:});
        end 
        
        function f = fslFolder(~)
            f = 'fsl';
        end
        function f = hdrinfoFolder(~)
            f = 'pet';
        end
        function f = mriFolder(~)
            f = 'mri';
        end
        function f = petFolder(~)
            f = 'pet';
        end
        
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
        function fn = petfov_fn(~, sessDat, varargin)
            ip = inputParser;
            addOptional(ip, 'suff', '', @ischar);
            parse(ip, varargin{:})
            fn = sprintf('%s_mprage%i_brainmask.nii.gz', sessDat.sessionFolder, sessDat.snumber, ip.Results.suff);
        end
    end
    
    %% PROTECTED
    
	methods (Access = protected)
 		function this = StudyDataSingleton(varargin)
 			this = this@mlpipeline.StudyDataSingleton(varargin{:});
            
            dt = mlsystem.DirTools(this.subjectsDir);
            fqdns = {};
            for di = 1:length(dt.dns)
                if (strcmp(dt.dns{di}(1),   'p'))
                    fqdns = [fqdns dt.fqdns(di)];
                end
            end
            this.sessionDataComposite_ = ...
                mlpatterns.CellComposite( ...
                    cellfun(@(x) mlpowers.SessionData('studyData', this, 'sessionPath', x), ...
                    fqdns, 'UniformOutput', false));            
            this.registerThis;
        end
        function registerThis(this)
            mlpipeline.StudyDataSingletons.register('powers', this);
        end
    end  

	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

