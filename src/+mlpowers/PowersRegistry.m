classdef PowersRegistry < mlpatterns.Singleton
	%% PowersRegistry 

	%  $Revision$
 	%  was created 15-Oct-2015 16:31:41
 	%  by jjlee,
 	%  last modified $LastChangedDate$
 	%  and checked into repository /Users/jjlee/Local/src/mlcvl/mlpowers/src/+mlpowers.
 	%% It was developed on Matlab 8.5.0.197613 (R2015a) for MACI64.
 	
    properties
        sessionRegexp = '\S*(?<pnum>M\d+)\S*'
        scanIndexRegexp = 'scan(?<sid>\d)'
        sessionNamePattern = 'M*'
        regionLabels = {'amygdala' 'hippocampus' 'large-hypothalamus' 'mpfc' 'thalamus'}
        gluTxlsxFileprefix = 'GluT de novo 2015aug11.xlsx'
    end
    
    properties (Dependent)
        subjectsDir
        testSubjectPath
        gluTxlsxFqfilename
    end
    
    methods %% GET
        function x = get.subjectsDir(~)
            x = fullfile(getenv('ARBELAEZ'), 'GluT', '');
        end
        function x = get.testSubjectPath(~)
            x = fullfile(getenv('MLUNIT_TEST_PATH'), 'Arbelaez', 'GluT', 'p7991_JJL', '');
        end
        function x = get.gluTxlsxFqfilename(this)
            x = fullfile(this.subjectsDir, this.gluTxlsxFileprefix);
        end
    end
    
    methods (Static)
        function this = instance(qualifier)
            %% INSTANCE uses string qualifiers to implement registry behavior that
            %  requires access to the persistent uniqueInstance
            persistent uniqueInstance
            
            if (exist('qualifier','var') && ischar(qualifier))
                if (strcmp(qualifier, 'initialize'))
                    uniqueInstance = [];
                end
            end
            
            if (isempty(uniqueInstance))
                this = mlpowers.PowersRegistry();
                uniqueInstance = this;
            else
                this = uniqueInstance;
            end
        end
    end  
    
    methods         
        function pnum = str2pnum(this, str)
            r = regexp(str, this.sessionRegexp, 'names');
            pnum = r.pnum;
        end
        function sidx = str2sidx(this, str)
            r = regexp(str, this.scanIndexRegexp, 'names');
            sidx = str2double(r.sid);
        end
        function s = getGluTShifts(this, si, pnum)
            if (1 == si)
                map = containers.Map(this.gluTShiftKeys_, this.scan1Values_);
            else
                map = containers.Map(this.gluTShiftKeys_, this.scan2Values_);
            end
            if (~lstrfind(pnum, this.gluTShiftKeys_))
                error('mlpowers:mapKeyNotFound', 'PowersRegistry.getGluTShifts: scan->%i, pnum->%s', si, pnum);
            end
            s = map(pnum);
        end
        function s = getKinetics4T0(this, si, pnum)
            if (1 == si)
                map = containers.Map(this.gluTShiftKeys_, this.scan1Kinetics4T0_);
            else
                map = containers.Map(this.gluTShiftKeys_, this.scan2Kinetics4T0_);
            end
            if (~lstrfind(pnum, this.gluTShiftKeys_))
                error('mlpowers:mapKeyNotFound', 'PowersRegistry.getGluTShifts: scan->%i, pnum->%s', si, pnum);
            end
            s = map(pnum);
        end
        function f = regressFHerscToVideen(~, f)
            %% REGRESSFHERSCTOVIDEEN uses regressions on NP755 cases to scale Kety-Schmidt-Herscovitch flows to Videen flows.
            %  All flows should have units of 1/s.
            %  Usage:  f = this.regressFHerscToVideen(f)
            
            assert(all(f < 1));
            f = 6000 * f / 1.05; % to mL/min/100 g
            f = 0.3789*f + 19.24;
            f = 1.05 * f / 6000; % to 1/s
        end
        function f = regressFVideenToHersc(~, f)
            %% REGRESSFVIDEENTOHERSC uses regressions on NP755 cases to scale Videen flows to Kety-Schmidt-Herscovitch.
            %  All flows should have units of mL/min/100g.
            %  Usage:  f = this.regressFHerscToVideen(f)
            
            assert(all(f > 20));
            f = 2.6392*(f - 19.24);
        end
    end
    
    %% PRIVATE
    
    properties (Constant, Access = 'private')        
        gluTShiftKeys_ = { ...
            'p7861'    'p7873'    'p7879'    'p7891'    'p7901'    'p7926' ...
            'p7935'    'p7954'    'p7956'    'p7979'    'p7991'    'p7996' ...
            'p8015'    'p8018'    'p8024'    'p8039'               'p8047' };
        scan1Values_ = [ ...
            -34 -43 -24 -35 -43 -36    -33 -30 -38 -25 -50 -43    -43 -42 -30 -44 -33];
        scan2Values_ = [ ...
            -38 -30 -19 -22 -38 -23    -25 -31 -30 -21 -45 -25    -33 -24 -26 -25 -18];
        scan1Kinetics4T0_ = [ ...
             30  21  17  35  30  19     11  14  14  18  23  12     24   9  15   7  14];
        scan2Kinetics4T0_ = [ ...
             33  17  27  12   5   7      9  16  18  12  35  14     16   2  17   5   6];
    end
    
	methods (Access = 'private')		  
 		function this = PowersRegistry(varargin)
 			this = this@mlpatterns.Singleton(varargin{:});
 		end
    end 

	%  Created with Newcl by John J. Lee after newfcn by Frank Gonzalez-Morphy
 end

