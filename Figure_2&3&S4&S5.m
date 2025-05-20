
% List of river folders
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

% Define base paths
base_path      = "F:\";  % adjust as needed
lib_path       = fullfile(base_path, "1-Codes_libs\libs");
mexfile_path   = fullfile(base_path, "1-Codes_libs\binaryfiles");
addpath(genpath(lib_path));
addpath(mexfile_path);

% Model names and fields
model_names   = ["CanESM5", "CMCC-ESM2", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "NorESM2-LM", "NorESM2-MM"];
model_fields  = ["M_CanESM5", "M_CMCC_ESM2", "M_MPI_ESM1_2_HR", "M_MPI_ESM1_2_LR", "M_NorESM2_LM", "M_NorESM2_MM"];

% Periods and scenarios
% period        = "P_2061_01_01_2099_12_31";
period        = "P_2020_01_01_2060_12_31";
scenarios     = ["ssp370", "ssp585"];

% Time vectors
era5_time     = datetime(1979,1,1):caldays(1):datetime(2020,12,31);
% mbcn_time     = datetime(2061,1,1):caldays(1):datetime(2090,12,31);
mbcn_time = datetime(2030, 1, 1):caldays(1):datetime(2060, 12, 31);
era5_period   = "P_1979_01_01_2020_12_31";

% Layout for subplots
num_rivers    = numel(river_folders);
num_cols      = 5;
num_rows      = ceil(num_rivers/num_cols);

% Output directory
plots_dir = fullfile(base_path, 'Combined_Plots');
if ~exist(plots_dir,'dir')
    mkdir(plots_dir);
end

for s = 1:numel(scenarios)
    scenario = scenarios(s);

    %% Discharge Probability of Exceedance
    figure_discharge_po = figure('Name', sprintf('Discharge P of Exceedance (%s)',scenario), ...
                                 'Position', get(0,'Screensize'));
    tiledlayout(num_rows, num_cols, 'TileSpacing','compact','Padding','compact');

    model_handles_discharge_po = [];
    model_labels = [model_names, 'ERA5'];

    for i = 1:num_rivers
        nexttile;
        hold on;

        % Load data
        river_folder = river_folders{i};
        river_name   = strrep(strrep(river_folder,'0-',''),'_DataGrass','');
        river_path   = fullfile(base_path,river_folder, ['0-Pyceq_' river_name]);
        save_path    = fullfile(river_path,'meteo','simulations_CPQ_CPWT.mat');
        if ~exist(save_path,'file')
            warning('Missing data for %s, skipping.',river_name);
            continue;
        end
        load(save_path,'all_simulations');

        % Plot MBCn models
        for j = 1:numel(model_fields)
            fld = model_fields(j);
            if isfield(all_simulations.MBCn.(period).(fld).(scenario),'discharge')
                dq = all_simulations.MBCn.(period).(fld).(scenario).discharge;
                june_oct = (month(mbcn_time)>=6 & month(mbcn_time)<=9);
                dq = dq(june_oct);
                [f,x] = ecdf(dq); f = 1 - f;
                x = [0; x]; f = [1; f];
                h = plot(x, f*100, 'LineWidth',1);
                if i==1
                    model_handles_discharge_po(end+1) = h;
                end
            end
        end

        % Plot ERA5
        dq_e = all_simulations.ERA5.(era5_period).discharge;
        june_oct = (month(era5_time)>=6 & month(era5_time)<=9);
        dq_e = dq_e(june_oct);
        [f_e,x_e] = ecdf(dq_e); f_e = 1 - f_e;
        x_e = [0; x_e]; f_e = [1; f_e];
        hE = plot(x_e, f_e*100, 'k-','LineWidth',2);
        if i==1
            model_handles_discharge_po(end+1) = hE;
        end

        title(river_name,'Interpreter','none','FontSize',8,'FontWeight','normal');
        ylim([0 100]);
        grid on;
        ax = gca; ax.FontSize = 14;

        % Y-label only on first column
        if mod(i-1,num_cols)==0
            ylabel('(%)','FontSize',16);
        end
        % X-label only on bottom row
        if i > (num_rows-1)*num_cols
            xlabel('(m^3/s)','FontSize',16);
        end

        hold off;
    end

    % Legend outside
    figure(figure_discharge_po);
    % lg = legend(model_handles_discharge_po, model_labels, ...
    %             'Interpreter','none','FontSize',12, ...
    %             'Orientation','horizontal','Location','eastoutside');
    saveas(gcf, fullfile(plots_dir, sprintf('All_Discharge_2030-2060_PoE_%s.png',scenario)));
    close(gcf);


    %% Temperature Probability of Exceedance
    figure_temperature_po = figure('Name', sprintf('Temperature P of Exceedance (%s)',scenario), ...
                                   'Position', get(0,'Screensize'));
    tiledlayout(num_rows, num_cols, 'TileSpacing','compact','Padding','compact');

    model_handles_temperature_po = [];
    temp_add_labels = {'Lowest P(T>20°C)','Highest P(T>20°C)'};
    exceed_probs = zeros(numel(model_fields),1);

    for i = 1:num_rivers
        nexttile;
        hold on;

        river_folder = river_folders{i};
        river_name   = strrep(strrep(river_folder,'0-',''),'_DataGrass','');
        river_path   = fullfile(base_path,river_folder,['0-Pyceq_' river_name]);
        save_path    = fullfile(river_path,'meteo','simulations_CPQ_CPWT.mat');
        if ~exist(save_path,'file'), warning('Missing %s',river_name); continue; end
        load(save_path,'all_simulations');

        for j = 1:numel(model_fields)
            fld = model_fields(j);
            if isfield(all_simulations.MBCn.(period).(fld).(scenario),'temperature')
                tq = all_simulations.MBCn.(period).(fld).(scenario).temperature;
                june_oct = (month(mbcn_time)>=6 & month(mbcn_time)<=9);
                tq = tq(june_oct);
                [f,x] = ecdf(tq); f = 1 - f;
                x = [0; x]; f = [1; f];
                h = plot(x, f*100, 'LineWidth',1);
                if i==1
                    model_handles_temperature_po(end+1) = h;
                end
                exceed_probs(j) = mean(tq > 20)*100;
            end
        end

        % ERA5
        tq_e = all_simulations.ERA5.(era5_period).temperature;
        june_oct = (month(era5_time)>=6 & month(era5_time)<=9);
        tq_e = tq_e(june_oct);
        [f_e,x_e] = ecdf(tq_e); f_e = 1 - f_e;
        x_e = [0; x_e]; f_e = [1; f_e];
        hE = plot(x_e, f_e*100, 'k-','LineWidth',2);
        if i==1
            model_handles_temperature_po(end+1) = hE;
        end

        % Min/Max P(T>20)
        minP = min(exceed_probs);
        maxP = max(exceed_probs);
        hmin = yline(minP,'r--','LineWidth',1);
        hmax = yline(maxP,'b--','LineWidth',1);
        hmin.Label = sprintf('%.1f%%',minP);
        hmax.Label = sprintf('%.1f%%',maxP);
        hmin.FontSize = 12;
        hmax.FontSize = 12;
        if i==1
            model_handles_temperature_po = [model_handles_temperature_po hmin hmax];
            model_labels_temperature = [model_labels, temp_add_labels];
        end

        title(river_name,'Interpreter','none','FontSize',8,'FontWeight','normal');
        xlim([5 inf]);
        ylim([0 100]);
        grid on;
        ax = gca; ax.FontSize = 14;

        if mod(i-1,num_cols)==0
            ylabel('(%)','FontSize',16);
        end
        if i > (num_rows-1)*num_cols
            xlabel('(°C)','FontSize',16);
        end

        hold off;
    end

    figure(figure_temperature_po);
    % lg = legend(model_handles_temperature_po, model_labels_temperature, ...
    %             'Interpreter','none','FontSize',12, ...
    %             'Orientation','horizontal','Location','eastoutside');
    saveas(gcf, fullfile(plots_dir, sprintf('All_Temperature_2030-2060_PoE_%s.png',scenario)));
    close(gcf);
end
