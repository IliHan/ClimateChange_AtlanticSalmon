## ClimateChange_AtlanticSalmon
[![DOI](https://zenodo.org/badge/986552679.svg)](https://doi.org/10.5281/zenodo.15468304)

Supplementary Materials (Times series + Figures_Codes)

Discharge and water temperature simulations were generated using CEQUEAU, a deterministic semi-distributed hydrological and thermal model. CEQUEAU was applied to 35 Atlantic salmon rivers across northeastern Canada and the northeastern United States to simulate daily mean discharge and water temperature under both historical (1979–2020) and future (2030–2100) climate conditions.
Future projections were driven by six General Circulation Models (GCMs) from the CMIP6 ensemble, including:
CanESM5, CMCC-ESM2, MPI-ESM1-2-HR, MPI-ESM1-2-LR, NorESM2-LM, and NorESM2-MM, under two shared socioeconomic pathways (SSP3-7.0 and SSP5-8.5).
Future simulations were based on two CMIP6 climate scenarios: SSP3-7.0 (intermediate) and SSP5-8.5 (pessimistic).

Data Organization:
-  Time series CSV files are available in individual folders named after each river.
-  For each river, the CSV files include:
    -  globo_rhw_'rivername'_ssp370(585) for water temperature.
    -  globo_rhw_Q_'rivername'_ssp370(585) for discharge.
      
Figure Reproducibility:
-  Figures 2, 3, S4, and S5
    Run Figures_2&3&S4&S5.r. This script uses the above .csv files for each river and scenario to reproduce the probability of exceedance plots.
-  Figures 4 and S6
    Run Figures_4&S6.r. This script generates boxplots representing discharge and percent change across historical and future periods.
-  Figure 5 and Riverine Heatwave Statistics
    Run Figure_5_DataGen.r to generate the underlying data, including mean intensity and cumulative metrics stored in files named: M_'ClimateChangeModel'_MeanIntensityCum
    Rivers are numbered 1 to 35 (order defined in Figure_5.r).
    Run Figure_5.r to produce Figure 5 panels showing the temporal evolution of heatwave intensity and duration across the study region.
-  Figures 6 and 7 (Best Single Covariate Models)
    Run the following scripts in order:
    Identify_SummerRH.r – Identifies heatwaves across time periods and scenarios.
    Correlation.r – Performs correlation analysis between climate indices (e.g., AO, NAO, SOI) and heatwave frequency.
    Figure_6&7.r – Produces Figures 6 and 7 with parametric bootstrapping for the best single-covariate models.
-  Figures 8 and 9 (Best Multi-Covariate Models)
    Run the same first two scripts (Identify_SummerRH.r and Correlation.r), followed by:
    Figure_8&9.r – Generates Figures 8 and 9 showing best multi-covariate model performance.
