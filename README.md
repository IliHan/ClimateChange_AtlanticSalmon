# ClimateChange_AtlanticSalmon
[![DOI](https://zenodo.org/badge/986552679.svg)](https://doi.org/10.5281/zenodo.15468304)

This repo contains all the script relate to the following open access article : Hani et al., 2025b, Past and Future Hydrological and Water Temperature Shifts in Atlantic Salmon Rivers in Northeastern America (1979-2100)

In a nutshell, it provides the simulated discharge and Tw time series (1979-2100) in 35 Atlantic salmon rivers across northeastern Canada and the northeastern United States. Future projections were driven by six General Circulation Models (GCMs) from the CMIP6 ensemble, including: CanESM5, CMCC-ESM2, MPI-ESM1-2-HR, MPI-ESM1-2-LR, NorESM2-LM, and NorESM2-MM, under two shared socioeconomic pathways (SSP3-7.0 and SSP5-8.5). We also provide the modeling of Riverine heatwave frequency using a nonstationary frequency analysis modelling approach. A parametric bootstrapping is applied giving a fair appraisal of uncertainties

# Software requirements
Matlab, R and Rstudio (user friendly GUI).

# Data Organization:
Time series CSV files are available in individual folders named after each river.
For each river, the CSV files include:
    globo_rhw_'rivername'_ssp370(585) for water temperature.
    globo_rhw_Q_'rivername'_ssp370(585) for discharge.
      
# Figure Reproducibility:
Riverine Heatwave Characteristics (Figure 5):
    Run Figure_5_DataGen.r to generate the underlying data, including mean intensity and cumulative metrics stored in files named: M_'ClimateChangeModel'_MeanIntensityCum
    Rivers are numbered 1 to 35 (order defined in Figure_5.r).
    Run Figure_5.r to produce Figure 5 panels showing the temporal evolution of heatwave intensity and duration across the study region.
Nonstationary Models (Figures 6 and 7; Figures 8 and 9):
    Run the following scripts in order:
    Identify_SummerRH.r – Identifies heatwaves across time periods and scenarios.
    Correlation.r – Performs correlation analysis between climate indices (e.g., AO, NAO, SOI) and heatwave events.
    Figure_6&7.r – Produces Figures 6 and 7 with parametric bootstrapping for the best single-covariate models.
    Figure_8&9.r – Generates Figures 8 and 9 showing best multi-covariate model performance.
