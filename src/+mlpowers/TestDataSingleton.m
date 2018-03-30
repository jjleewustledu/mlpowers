classdef TestDataSingleton < mlpowers.StudyDataSingleton
	%% TESTDATASINGLETON  

	%  $Revision$
 	%  was created 31-Mar-2016 17:36:14
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 9.0.0.341360 (R2016a) for MACI64.
 	
    
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
                instance_ = mlpowers.TestDataSingleton();
            end
            this = instance_;
        end
        function        register(varargin)
            %% REGISTER
            %  @param []:  if this class' persistent instance
            %  has not been registered, it will be registered via instance() call to the ctor; if it
            %  has already been registered, it will not be re-registered.
            %  @param ['initialize']:  any registrations made by the ctor will be repeated.
            
            mlpowers.TestDataSingleton.instance(varargin{:});
        end
    end  

    %% PROTECTED
    
	methods (Access = protected)
 		function this = TestDataSingleton(varargin)
 			this = this@mlpowers.StudyDataSingleton(varargin{:});
            
            this.powersTrunk = fullfile(getenv('UNITTESTS'), 'powers', '');
            
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
            mlpipeline.StudyDataSingletons.register('test_powers', this);
        end
 	end 

	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

