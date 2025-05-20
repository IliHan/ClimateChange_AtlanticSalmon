%%%%%%%%%%%%%%%%% Function to compute boxplots and percent change in discharge warmest months (Figure 4 and S6) %%%%%%%%%%%%%

function compare_drought_discharge_subplots_twoPeriods_AveragedModels()

    base_path = "F:\"; 
    river_folders = {
        '0-AuxMelezes_DataGrass', '0-Moisie_DataGrass', '0-Natash_DataGrass', '0-Godbout_DataGrass', ...
        '0-Huile_DataGrass', '0-Jupiter_DataGrass', ...
        '0-Reid_DataGrass', '0-StLewis_DataGrass','0-Gilbert_DataGrass',  ...
        '0-Conne_DataGrass', '0-Highland_DataGrass', ...
        '0-Bear_DataGrass', '0-Carruthers_DataGrass', '0-Wilmot_DataGrass', '0-West_DataGrass', ...
        '0-Restigouche_DataGrass', '0-Miramichi_DataGrass', '0-MiramichiNW_DataGrass', '0-Matapedia_DataGrass', '0-Upsalquitch_DataGrass', ...  
        '0-Casca_DataGrass', '0-Pcasca_DataGrass', '0-Nouvelle_DataGrass', '0-Bonaventure_DataGrass', '0-SteAnne_DataGrass', '0-Dartmouth_DataGrass', ...
        '0-SteMarg_DataGrass', '0-Ouelle_DataGrass', '0-Gouffre_DataGrass', ...
        '0-Sackville_DataGrass', '0-Havre_DataGrass', '0-Margaree_DataGrass', ...
        '0-Ducktrap_DataGrass', '0-Sheepscot_DataGrass', '0-Narragagus_DataGrass'
    };

    % Model fields & names
    model_fields = ["M_CanESM5","M_CMCC_ESM2","M_MPI_ESM1_2_HR", ...
                    "M_MPI_ESM1_2_LR","M_NorESM2_LM","M_NorESM2_MM"];
    model_names  = ["CanESM5","CMCC-ESM2","MPI-ESM1-2-HR", ...
                    "MPI-ESM1-2-LR","NorESM2-LM","NorESM2-MM"];

    % Historical (ERA5)
    era5_period = "P_1979_01_01_2020_12_31";
    era5_time   = datetime(1979,1,1) : days(1) : datetime(2020,12,31);

    % FUTURE #1 =>  2020–2060, we want 2030–2060
    future_period_1 = "P_2020_01_01_2060_12_31";
    mbcn_time_1      = datetime(2020,1,1) : days(1) : datetime(2060,12,31);
    start_sub1       = datetime(2030,1,1);
    end_sub1         = datetime(2060,12,31);

    % FUTURE #2 =>  2061–2099, we want 2061–2090
    future_period_2 = "P_2061_01_01_2099_12_31";
    mbcn_time_2      = datetime(2061,1,1) : days(1) : datetime(2090,12,31);
    start_sub2       = datetime(2061,1,1);
    end_sub2         = datetime(2090,12,31);

    % Two scenarios side by side:
    scenarios = ["ssp370","ssp585"];

    % Drought month(s) => August
    drought_months = [7 8 9];

    % Output directory
    output_dir = fullfile(base_path,"Drought_Discharge_Analysis");
    if ~exist(output_dir,'dir'), mkdir(output_dir); end

    %% 1) LOAD & EXTRACT ERA5
    results = struct();
    for r = 1:numel(river_folders)
        river_folder = river_folders{r};
        river_name   = strrep(strrep(river_folder,'0-',''),'_DataGrass','');

        % Path to data
        mat_path = fullfile(base_path, river_folder, ...
            sprintf('0-Pyceq_%s',river_name), 'meteo','simulations_CPQ_CPWT.mat');
        if ~isfile(mat_path)
            warning('Missing file for %s. Skipping.', river_name);
            continue;
        end
        load(mat_path,'all_simulations');

        % Check ERA5
        if ~isfield(all_simulations,'ERA5') || ...
           ~isfield(all_simulations.ERA5, era5_period) || ...
           ~isfield(all_simulations.ERA5.(era5_period),'discharge')
            warning('ERA5 missing for %s. Skipping.', river_name);
            continue;
        end

        % ERA5 for August
        era5_dis  = all_simulations.ERA5.(era5_period).discharge;
        idx_era5  = ismember(month(era5_time), drought_months);
        era5_vals = era5_dis(idx_era5);
        era5_yrs  = year(era5_time(idx_era5));
        uniq_era5_yrs = unique(era5_yrs);

        era5_means = NaN(numel(uniq_era5_yrs),1);
        for yy = 1:numel(uniq_era5_yrs)
            y_ = uniq_era5_yrs(yy);
            era5_means(yy) = mean(era5_vals(era5_yrs==y_));
        end

        results(r).river_name   = river_name;
        results(r).era5_drought = era5_means;

        if ~isfield(all_simulations,'MBCn'), continue; end

        % Initialize scenario sub-structs
        for s = 1:numel(scenarios)
            scn = scenarios(s);
            results(r).(scn) = struct('near_future',[],'far_future',[]);
        end

        %% 2) For each scenario => gather near/far future yearly means (model-averaged)
        for s = 1:numel(scenarios)
            scn = scenarios(s);

            % Prepare arrays to store final year-by-year means (averaging all models)
            nearFutVals = [];  % 1 value per year => average of 6 models
            farFutVals  = [];

            % List of years
            all_years_1 = (year(start_sub1)) : (year(end_sub1));  % 2030..2060
            all_years_2 = (year(start_sub2)) : (year(end_sub2));  % 2061..2090

            % NEAR FUTURE
            for y_ = all_years_1
                modelValsThisYear = [];
                for m = 1:numel(model_fields)
                    mf = model_fields(m);
                    if ~isfield(all_simulations.MBCn, future_period_1), continue; end
                    if ~isfield(all_simulations.MBCn.(future_period_1), mf), continue; end
                    if ~isfield(all_simulations.MBCn.(future_period_1).(mf), scn), continue; end

                    broad_1 = all_simulations.MBCn.(future_period_1).(mf).(scn).discharge;
                    t1      = mbcn_time_1;

                    idx_y = (year(t1)==y_) & (month(t1)==8);
                    if ~any(idx_y), continue; end
                    theseVals = broad_1(idx_y);
                    if all(isnan(theseVals)), continue; end

                    modelValsThisYear(end+1) = mean(theseVals,'omitnan');
                end
                if ~isempty(modelValsThisYear)
                    nearFutVals(end+1) = mean(modelValsThisYear,'omitnan');
                end
            end

            % FAR FUTURE
            for y_ = all_years_2
                modelValsThisYear = [];
                for m = 1:numel(model_fields)
                    mf = model_fields(m);
                    if ~isfield(all_simulations.MBCn, future_period_2), continue; end
                    if ~isfield(all_simulations.MBCn.(future_period_2), mf), continue; end
                    if ~isfield(all_simulations.MBCn.(future_period_2).(mf), scn), continue; end

                    broad_2 = all_simulations.MBCn.(future_period_2).(mf).(scn).discharge;
                    t2      = mbcn_time_2;

                    idx_y = (year(t2)==y_) & (month(t2)==8);
                    if ~any(idx_y), continue; end
                    theseVals = broad_2(idx_y);
                    if all(isnan(theseVals)), continue; end

                    modelValsThisYear(end+1) = mean(theseVals,'omitnan');
                end
                if ~isempty(modelValsThisYear)
                    farFutVals(end+1) = mean(modelValsThisYear,'omitnan');
                end
            end

            % Store final distributions
            results(r).(scn).near_future = nearFutVals(:);
            results(r).(scn).far_future  = farFutVals(:);
        end
    end

    %% FILTER valid rivers
    valid_idx   = arrayfun(@(x) isfield(x,'era5_drought'), results);
    valid_res   = results(valid_idx);
    num_rivers  = numel(valid_res);
    if num_rivers==0
        disp('No valid rivers found...'); return;
    end

    % Subplot layout
    num_cols = 5;
    num_rows = ceil(num_rivers / num_cols);

    %% =========== BOX PLOT: ERA5 vs [Near-Future for 2 scenarios] vs [Far-Future for 2 scenarios] ===========

    fig_box = figure('Name','Multi-Scenario Discharge Boxplots','Position',get(0,'Screensize'));

    for r = 1:num_rivers
        subplot(num_rows,num_cols,r);
        hold on;

        river_name = valid_res(r).river_name;

        % 1) ERA5 distribution
        era5_data = valid_res(r).era5_drought(:);
        era5_clean = era5_data(~isoutlier(era5_data,'quartiles'));

        % 2) For each scenario => near & far
        near_370 = valid_res(r).("ssp370").near_future;
        far_370  = valid_res(r).("ssp370").far_future;
        near_585 = valid_res(r).("ssp585").near_future;
        far_585  = valid_res(r).("ssp585").far_future;

        near_370_clean = near_370(~isoutlier(near_370,'quartiles'));
        far_370_clean  = far_370(~isoutlier(far_370,'quartiles'));
        near_585_clean = near_585(~isoutlier(near_585,'quartiles'));
        far_585_clean  = far_585(~isoutlier(far_585,'quartiles'));

        % Combine into 1 array + group factor
        X_all = [];
        G     = [];

        % ERA5
        X_all = [X_all; era5_clean];
        G     = [G; repmat({'ERA5'}, numel(era5_clean),1)];

        % Near-370
        X_all = [X_all; near_370_clean];
        G     = [G; repmat({'S1'}, numel(near_370_clean),1)];

        % Near-585
        X_all = [X_all; near_585_clean];
        G     = [G; repmat({'S3'}, numel(near_585_clean),1)];

        % Far-370
        X_all = [X_all; far_370_clean];
        G     = [G; repmat({'S2'}, numel(far_370_clean),1)];

        % Far-585
        X_all = [X_all; far_585_clean];
        G     = [G; repmat({'S4'}, numel(far_585_clean),1)];

        if isempty(X_all)
            title([river_name ' (no data)'],'Interpreter','none');
            hold off; continue;
        end

        % groupOrder = {'ERA5','Near-370','Near-585','Far-370','Far-585'};
        groupOrder = {'ERA5','S1','S3','S2','S4'};
        boxplot(X_all, G, 'GroupOrder',groupOrder, 'Symbol','');

        % Adjust Y-limits
        mnVal = min(X_all);
        mxVal = max(X_all);
        if mxVal>mnVal
            rng_ = mxVal - mnVal;
            ylim([mnVal-0.05*rng_, mxVal+0.05*rng_]);
        else
            ylim([mnVal-1, mnVal+1]);
        end

        title(river_name,'Interpreter','none');
        ylabel('(m^3/s)');
        grid on;
        hold off;
    end

    sgtitle('Jul-Aug Interannual Mean');
    saveas(fig_box, fullfile(output_dir,'AllRivers_BoxPlot_TwoScenarios.png'));

    %% ========== PERCENT-CHANGE FIGURE (Near-370, Near-585, Far-370, Far-585) ==========

    fig_pct = figure('Name','Percent Change (Two Scenarios)','Position',get(0,'Screensize'));

    for r = 1:num_rivers
        subplot(num_rows,num_cols,r);
        hold on;

        river_name = valid_res(r).river_name;

        % ERA5
        era5_data  = valid_res(r).era5_drought(:);
        era5_clean = era5_data(~isoutlier(era5_data,'quartiles'));
        mean_era5  = mean(era5_clean,'omitnan');
        if isempty(era5_clean) || isnan(mean_era5)
            title([river_name ' (no ERA5)'],'Interpreter','none');
            hold off; continue;
        end

        % ssp370: near & far
        near_370 = valid_res(r).("ssp370").near_future;
        far_370  = valid_res(r).("ssp370").far_future;
        near_370_clean = near_370(~isoutlier(near_370,'quartiles'));
        far_370_clean  = far_370(~isoutlier(far_370,'quartiles'));
        mean_near370   = mean(near_370_clean,'omitnan');
        mean_far370    = mean(far_370_clean,'omitnan');
        pct_near370    = 100*(mean_near370 - mean_era5)/mean_era5;
        pct_far370     = 100*(mean_far370  - mean_era5)/mean_era5;

        % ssp585: near & far
        near_585 = valid_res(r).("ssp585").near_future;
        far_585  = valid_res(r).("ssp585").far_future;
        near_585_clean = near_585(~isoutlier(near_585,'quartiles'));
        far_585_clean  = far_585(~isoutlier(far_585,'quartiles'));
        mean_near585   = mean(near_585_clean,'omitnan');
        mean_far585    = mean(far_585_clean,'omitnan');
        pct_near585    = 100*(mean_near585 - mean_era5)/mean_era5;
        pct_far585     = 100*(mean_far585  - mean_era5)/mean_era5;

        pc_array = [pct_near370, pct_near585, pct_far370, pct_far585]; 
        bh = bar(pc_array,'FaceColor','flat');

        % % color each bar individually
        % bh.CData(1,:) = [0.2, 0.4, 0.8];    % near-370 => blue
        % bh.CData(2,:) = [0.2, 0.7, 0.5];    % near-585 => greenish
        % bh.CData(3,:) = [0.85, 0.33, 0.1];  % far-370  => orange
        % bh.CData(4,:) = [0.8, 0.2, 0.7];    % far-585  => magenta

        % Graduated colors (yellow to red)
        % Define start and end colors
        start_color = [1, 1, 0];  % Yellow
        end_color   = [1, 0, 0];  % Red
        num_bars    = 4;          % Total bars: Near-370, Near-585, Far-370, Far-585

        % Interpolate colors for each bar
        gradient_colors = zeros(num_bars, 3);
        for b = 1:num_bars
            gradient_colors(b, :) = start_color + (end_color - start_color) * ((b - 1) / (num_bars - 1));
        end
    
        % Apply the interpolated colors to the bars
        for b = 1:num_bars
            bh.CData(b, :) = gradient_colors(b, :);
        end

        set(gca,'XTick',1:4, ...
            'XTickLabel',{'S1','S3','S2','S4'});
            % 'XTickLabel',{'Near-370','Near-585','Far-370','Far-585'});
        ylabel('%');
        title(river_name,'Interpreter','none');
        grid on;

        % Y-limit
        mnVal = min(pc_array);
        mxVal = max(pc_array);
        if mxVal>mnVal
            rng_ = mxVal - mnVal;
            ylim([mnVal - 0.1*rng_, mxVal + 0.1*rng_]);
        else
            ylim([mnVal-1, mnVal+1]);
        end

        hold off;
    end

    sgtitle('Jul-Aug Interannual Daily Mean % Change from baseline period');
    saveas(fig_pct, fullfile(output_dir,'PctChange_NearFar_TwoScenarios.png'));

    disp('Completed. Boxplots & Percent-change with TWO scenarios (ssp370 & ssp585).');
end
