close all
clear all
addpath('C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode/Matlab-Library-2016-master/Matlab-Library-2016-master/colormaps/cbrewer/cbrewer'); % https://uk.mathworks.com/matlabcentral/fileexchange/34087-cbrewer-colorbrewer-schemes-for-matlab
addpath('C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode/m_map1.4/m_map'); % https://www.eoas.ubc.ca/~rich/map.html
addpath('C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode/raacampbell-shadedErrorBar-aa6d919');
addpath('C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode/textLoc') %https://www.mathworks.com/matlabcentral/fileexchange/17151-textloc

% Set colors for plots
ireds = cbrewer('seq','Reds',9);
set1 = cbrewer('qual','Set1',9);
set2 = cbrewer('qual','Set2',8);
CT1 = cbrewer('seq','YlOrRd',9);

% Change this to the directory in which the data is stored
main_folder = 'C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode';

% Load lake information file
ifid = fopen([main_folder '/river_characteristics.csv']);
metad = textscan(ifid, repmat('%f',1,4), 'headerlines', 1, 'TreatAsEmpty', 'NA', 'delimiter', ',');
fclose(ifid);
iglwd = metad{1};
ilat = metad{2};
ilon = metad{3};
imeandepth = metad{4};

% Climate models
clim_mods = {'M_CanESM5_MeanIntensityCum', 'M_CMCC_ESM2_MeanIntensityCum', 'M_MPI_ESM1_2_HR_MeanIntensityCum', 'M_MPI_ESM1_2_LR_MeanIntensityCum', 'M_NorESM2_LM_MeanIntensityCum', 'M_NorESM2_MM_MeanIntensityCum'};

% Figure setup
fh = figure(1);
set(fh, 'color', 'white', 'Units', 'Inches', 'Position', [0,0,7,9], 'PaperUnits', 'Inches', 'PaperSize', [7,9]);

len_years = 1979:2099;
idx_hist = find(len_years >= 1979 & len_years <= 2020); % WAS 2005
idx_fut = find(len_years >= 2061 & len_years <= 2099);
iscen = {'historical', 'ssp370', 'ssp585'};
% ccols = {[0,0,0], set1(2,:), set1(5,:), set1(1,:)};
ccols = {[0,0,0], [0 0.4470 0.7410], [1 0 0], set1(1,:)};


hist_mean2 = nan(length(iglwd), length(iscen));
hist_dur2 = nan(length(iglwd), length(iscen));
hist_tot2 = nan(length(iglwd), length(iscen));
fut_mean2 = nan(length(iglwd), length(iscen));
fut_dur2 = nan(length(iglwd), length(iscen));
fut_tot2 = nan(length(iglwd), length(iscen));

for i = 1:length(iscen)
    glob_mean2 = nan(length(len_years), length(clim_mods));
    glob_dur2 = nan(length(len_years), length(clim_mods));
    glob_tot2 = nan(length(len_years), length(clim_mods));
    hist_mean = nan(length(iglwd), length(clim_mods));
    hist_dur = nan(length(iglwd), length(clim_mods));
    hist_tot = nan(length(iglwd), length(clim_mods));
    fut_mean = nan(length(iglwd), length(clim_mods));
    fut_dur = nan(length(iglwd), length(clim_mods));
    fut_tot = nan(length(iglwd), length(clim_mods));
    for j = 1:length(clim_mods)
        % Pre-allocate arrays
        glob_mean = nan(length(len_years), length(iglwd));
        glob_dur = nan(length(len_years), length(iglwd));
        glob_tot = nan(length(len_years), length(iglwd));
        for ii = 1:length(iglwd)
            if strcmp(iscen{i}, 'historical')
                fid = fopen([main_folder '/' clim_mods{j} '/globo_rhw_' num2str(iglwd(ii)) '_ssp370.csv']);
                dd = textscan(fid, [repmat('%f', 1, 4) '%s%s' repmat('%f', 1, 16)], 'TreatAsEmpty', 'NA', 'delimiter', ',', 'headerlines', 1);
                fclose(fid);
            else
                fid = fopen([main_folder '/' clim_mods{j} '/globo_rhw_' num2str(iglwd(ii)) '_' iscen{i} '.csv']);
                dd = textscan(fid, repmat('%f', 1, 22), 'TreatAsEmpty', 'NA', 'delimiter', ',', 'headerlines', 1);
                fclose(fid);
            end
            uyear = dd{1};
            ann_mean = dd{2};
            ann_dur = dd{3};
            ann_tot = dd{4};

            if strcmp(iscen{i}, 'historical')
                ann_mean(uyear > 2020) = nan; % was 2005
                ann_dur(uyear > 2020) = nan; % was 2005
                ann_tot(uyear > 2020) = nan; % was 2005
            end
            if strcmp(iscen{i}, 'ssp370') || strcmp(iscen{i}, 'ssp585')
                ann_mean(uyear < 2020) = nan; % was 2006
                ann_dur(uyear < 2020) = nan; % was 2006
                ann_tot(uyear < 2020) = nan; % was 2006
            end

            % Store results for time series
            glob_mean(:, ii) = ann_mean;
            glob_dur(:, ii) = ann_dur;
            glob_tot(:, ii) = ann_tot;

            % Historic and future
            hist_mean(ii, j) = nanmean(ann_mean(idx_hist));
            hist_dur(ii, j) = nanmean(ann_dur(idx_hist));
            hist_tot(ii, j) = nanmean(ann_tot(idx_hist));
            fut_mean(ii, j) = nanmean(ann_mean(idx_fut));
            fut_dur(ii, j) = nanmean(ann_dur(idx_fut));
            fut_tot(ii, j) = nanmean(ann_tot(idx_fut));
        end
        glob_mean2(:, j) = nanmean(glob_mean, 2);
        glob_dur2(:, j) = nanmean(glob_dur, 2);
        glob_tot2(:, j) = nanmean(glob_tot, 2);
    end


    glob_mean3 = nanmean(glob_mean2, 2);
    glob_mean3b = nanstd(glob_mean2, [], 2);
    glob_mean3c = nanmin(glob_mean2, [], 2); %min de tout les modeles
    glob_mean3d = nanmax(glob_mean2, [], 2);

    glob_dur3 = nanmean(glob_dur2, 2);
    glob_dur3b = nanstd(glob_dur2, [], 2);
    glob_dur3c = nanmin(glob_dur2, [], 2);
    glob_dur3d = nanmax(glob_dur2, [], 2);

    glob_tot3 = nanmean(glob_tot2, 2);
    glob_tot3b = nanstd(glob_tot2, [], 2);
    glob_tot3c = nanmin(glob_tot2, [], 2);
    glob_tot3d = nanmax(glob_tot2, [], 2);

    hist_mean2(:, i) = nanmean(hist_mean, 2);
    hist_dur2(:, i) = nanmean(hist_dur, 2);
    hist_tot2(:, i) = nanmean(hist_tot, 2);
    fut_mean2(:, i) = nanmean(fut_mean, 2);
    fut_dur2(:, i) = nanmean(fut_dur, 2);
    fut_tot2(:, i) = nanmean(fut_tot, 2);

    % Define the x-values (years) once
    x_values = 1979:2099;

    % Plot the first subplot (Mean)
    ax1 = subplot(3,2,1);

    % Plot the shaded error bar
    shadedErrorBar(x_values, glob_mean3, glob_mean3b, 'lineProps', {'color', ccols{i}, 'linewidth', 1.2}, 'transparent', true);
    hold on;

    % Plot the minimum and maximum lines with a different linestyle
    plot(x_values, glob_mean3c, ':', 'color', [ccols{i}, 0.8]);
    plot(x_values, glob_mean3d, ':', 'color', [ccols{i}, 0.8]);
    hold on;

    % Add the label if this is the last scenario being plotted
    if i == length(iscen)
        textLoc('a', 'NorthWest', 'fontname', 'Arial', 'fontsize', 8, 'fontweight', 'bold');
    end

    % Plot the second subplot (Duration)
    ax2 = subplot(3,2,3);

    % Plot the shaded error bar
    shadedErrorBar(x_values, glob_dur3, glob_dur3b, 'lineProps', {'color', ccols{i}, 'linewidth', 1.2}, 'transparent', true);
    hold on;

    % Plot the minimum and maximum lines with a different linestyle
    plot(x_values, glob_dur3c, ':', 'color', [ccols{i}, 0.8]);
    plot(x_values, glob_dur3d, ':', 'color', [ccols{i}, 0.8]);
    hold on;

    % Add the label if this is the last scenario being plotted
    if i == length(iscen)
        textLoc('c', 'NorthWest', 'fontname', 'Arial', 'fontsize', 8, 'fontweight', 'bold');
    end

    % Plot the third subplot (Total Duration)
    ax7 = subplot(3,2,5);

    % Plot the shaded error bar
    shadedErrorBar(x_values, glob_tot3, glob_tot3b, 'lineProps', {'color', ccols{i}, 'linewidth', 1.2}, 'transparent', true);
    hold on;
    plot(x_values, glob_tot3c, ':', 'color', [ccols{i}, 0.8]);
    plot(x_values, glob_tot3d, ':', 'color', [ccols{i}, 0.8]);
    hold on;

    if i == length(iscen)
        textLoc('e', 'NorthWest', 'fontname', 'Arial', 'fontsize', 8, 'fontweight', 'bold');
    end
end

% Adjust subplot positions and labels
ylabel(ax1, 'Average intensity (^{o}C)', 'fontname', 'Arial', 'fontsize', 7);
set(ax1, 'position', [0.08, 0.72, 0.4, 0.25], 'XMinorTick', 'on', 'YMinorTick', 'on', 'XTick', [1980, 2000, 2020, 2040, 2060, 2080, 2100], 'fontsize', 7);

ylabel(ax2, 'Average duration (days)', 'fontname', 'Arial', 'fontsize', 7);
set(ax2, 'position', [0.08, 0.4, 0.4, 0.25], 'XMinorTick', 'on', 'YMinorTick', 'on', 'XTick', [1980, 2000, 2020, 2040, 2060, 2080, 2100], 'fontsize', 7);

ylabel(ax7, 'Total duration (days)', 'fontname', 'Arial', 'fontsize', 7);
set(ax7, 'position', [0.08, 0.08, 0.4, 0.25], 'XMinorTick', 'on', 'YMinorTick', 'on', 'XTick', [1980, 2000, 2020, 2040, 2060, 2080, 2100], 'fontsize', 7);

axes(ax1)
h = zeros(3,1);
for ii = 1:3
    h(ii) = plot(nan, nan, '-', 'color', ccols{ii}, 'visible', 'on', 'linewidth', 1);
end
[ll, icons, plots, txt] = legend(h, {'Historical', 'SSP370', 'SSP585'}, 'FontName', 'Arial', 'fontsize', 7, 'AutoUpdate', 'off');
legend boxoff
set(ll, 'position', [0.08, 0.6, 0.15, 0.03], 'fontname', 'Arial', 'fontsize', 7);

% Map plots
addpath('C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode/cmocean')

% Plot for ax3
ax3 = subplot(3,2,2);
m_proj('lambert', 'lon', [-72 -52], 'lat', [42 62]);
m_coast('patch', [.6 .6 .6], 'edgecolor', [.5 .5 .5]);
% m_gshhs_f('patch',[.7 .7 .7]);
m_gshhs('hr');              % Intermediate resolution rivers
% m_gshhs('hb2');              % High resolution province/border
m_grid('linestyle', 'none', 'tickdir', 'out', 'linewidth', 0.2, 'fontsize', 6, 'backcolor',[.2 .65 1]);
hold on;

% Plot data points directly
m_scatter(ilon, ilat, 8, fut_mean2(:, 3), 'filled'); %, 'MarkerEdgeColor','k');

% Determine dynamic color axis limits based on the data
cmin_ax3 = min(fut_mean2(:, 3));
cmax_ax3 = max(fut_mean2(:, 3));
if cmin_ax3 == cmax_ax3
    cmax_ax3 = cmin_ax3 + 1;
end
caxis(ax3, [cmin_ax3 cmax_ax3]);

% Adjust colormap
colormap(ax3, cmocean('-solar'));
mm = m_coast();
mm.Color = 'k';
c1 = colorbar('TickLength', 0.001, 'Location', 'southoutside');
ylabel(c1, 'Average intensity (^{o}C)', 'fontname', 'Arial', 'fontsize', 7);
textLoc('b', 'North', 'fontname', 'Arial', 'fontsize', 8, 'fontweight', 'bold');
set(c1, 'YLim', [cmin_ax3 cmax_ax3], 'fontname', 'Arial', 'fontsize', 7);
set(ax3, 'position', [0.55, 0.74, 0.4, 0.25]);
set(c1, 'position', [0.62, 0.70, 0.25, 0.015]);

% Plot for ax4
ax4 = subplot(3,2,4);
m_proj('lambert', 'lon', [-72 -52], 'lat', [42 62]);
m_coast('patch', [.6 .6 .6], 'edgecolor', [.5 .5 .5]);
% m_gshhs_i('patch',[.7 .7 .7]);
m_gshhs('hr');              % Intermediate resolution rivers
% m_gshhs('hb2');              % High resolution province/border
m_grid('linestyle', 'none', 'tickdir', 'out', 'linewidth', 0.2, 'fontsize', 6, 'backcolor',[.2 .65 1]);
hold on;

% Plot data points directly
m_scatter(ilon, ilat, 8, fut_dur2(:, 3), 'filled'); %, 'MarkerEdgeColor','w');

% Determine dynamic color axis limits based on the data
cmin_ax4 = min(fut_dur2(:, 3));
cmax_ax4 = max(fut_dur2(:, 3));
if cmax_ax4 == 0
    cmax_ax4 = 1;
end
caxis(ax4, [cmin_ax4 cmax_ax4]);

% Adjust colormap
colormap(ax4, cmocean('-solar'));
mm = m_coast();
mm.Color = 'k';
c2 = colorbar('TickLength', 0.001, 'Location', 'southoutside');
ylabel(c2, 'Average duration (days)', 'fontname', 'Arial', 'fontsize', 7);
textLoc('d', 'North', 'fontname', 'Arial', 'fontsize', 8, 'fontweight', 'bold');
set(c2, 'YLim', [cmin_ax4 cmax_ax4], 'fontname', 'Arial', 'fontsize', 7);
set(ax4, 'position', [0.55, 0.41, 0.4, 0.25]);
set(c2, 'position', [0.62, 0.37, 0.25, 0.015]);

% Plot for ax8
ax8 = subplot(3,2,6);
m_proj('lambert', 'lon', [-72 -52], 'lat', [42 62]);
m_coast('patch', [.6 .6 .6], 'edgecolor', [.5 .5 .5]);
% m_gshhs_l('patch',[.7 .7 .7]); % High/low/interm resolution (h,i,l)
m_gshhs('hr4');              % High resolution rivers
% m_gshhs('hb2');              % High resolution rivers
m_grid('linestyle', 'none', 'tickdir', 'out', 'linewidth', 0.2, 'fontsize', 6, 'backcolor',[.2 .65 1]); %'backcolor',[.2 .65 1] for ocean color
hold on;

% Plot data points directly
h1=m_scatter(ilon, ilat, 8, fut_tot2(:, 3), 'filled'); %, 'MarkerEdgeColor','w');
m_scatter(ilon, ilat, 8, fut_tot2(:, 3), 'filled'); %, 'MarkerEdgeColor','w');
legend(h1,'Tw stations','location','northeast');

% Determine dynamic color axis limits based on the data
cmin_ax8 = min(fut_tot2(:, 3)); % Or 0 if we want to colortablette to start at 0
cmax_ax8 = max(fut_tot2(:, 3));
if cmax_ax8 == 0
    cmax_ax8 = 1;
end
caxis(ax8, [cmin_ax8 cmax_ax8]);

% Adjust colormap
colormap(ax8, cmocean('-solar'));
% colormap(ax8, cmocean('balance','pivot',0));
mm = m_coast();
mm.Color = 'k';
c3 = colorbar('TickLength', 0.001, 'Location', 'southoutside');
ylabel(c3, 'Total duration (days)', 'fontname', 'Arial', 'fontsize', 7);
textLoc('f', 'North', 'fontname', 'Arial', 'fontsize', 8, 'fontweight', 'bold');
set(c3, 'YLim', [cmin_ax8 cmax_ax8], 'fontname', 'Arial', 'fontsize', 7);
set(ax8, 'position', [0.55, 0.08, 0.4, 0.25]);
set(c3, 'position', [0.62, 0.04, 0.25, 0.015]);
