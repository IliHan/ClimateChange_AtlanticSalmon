# ClimateChange_AtlanticSalmon

[![DOI](https://zenodo.org/badge/986552679.svg)](https://doi.org/10.5281/zenodo.15468304)

This repository contains all scripts related to the following open-access article:  
**Hani et al., 2025b**  
*Past and Future Hydrological and Water Temperature Shifts in Atlantic Salmon Rivers in Northeastern America (1979–2100)*

## Summary

This dataset provides simulated daily discharge and water temperature (Tw) time series from **1979 to 2100** for **35 Atlantic salmon rivers** across northeastern Canada and the northeastern United States.

Future projections are based on six General Circulation Models (GCMs) from the CMIP6 ensemble:

- CanESM5  
- CMCC-ESM2  
- MPI-ESM1-2-HR  
- MPI-ESM1-2-LR  
- NorESM2-LM  
- NorESM2-MM  

Two Shared Socioeconomic Pathways are considered:

- **SSP3-7.0**  
- **SSP5-8.5**

In addition, the dataset includes **modeled riverine heatwave frequencies** derived using a **nonstationary frequency analysis** approach.  
**Parametric bootstrapping** was applied to provide uncertainty estimates.

---

## Software Requirements

- **MATLAB**  
- **R**  
- **RStudio** (recommended for user-friendly GUI)

---

## Data Organization

Each river has its own folder, named after the river. Inside each folder:

- `globo_rhw_<rivername>_ssp370.csv` or `ssp585.csv` → *Water temperature*
- `globo_rhw_Q_<rivername>_ssp370.csv` or `ssp585.csv` → *Discharge*

---

## Figure Reproducibility

### Riverine Heatwave Characteristics (Figure 5)

- **In R**:  
  Run `Figure_5_DataGen.r` to generate mean intensity and cumulative heatwave metrics.  
  Output files:  
  - `M_<ClimateChangeModel>_MeanIntensityCum`  
  Rivers are numbered from 1 to 35 (see `Figure_5.r` for the order).

- **In MATLAB**:  
  Run `Figure_5.m` to generate the Figure 5 panels showing temporal evolution of riverine heatwave intensity and duration.

---

### Nonstationary Models (Figures 6–9)

**In R, run the following scripts in order:**

1. `Identify_SummerRH.r`  
   - Identifies heatwaves across scenarios and periods.

2. `Correlation.r`  
   - Performs correlation analysis between heatwave frequency and climate indices (AO, NAO, SOI).

3. `Figure_6&7.r`  
   - Generates Figures 6 and 7 for the **best single-covariate models** with parametric bootstrapping.

4. `Figure_8&9.r`  
   - Generates Figures 8 and 9 for the **best multi-covariate models**.

---

Let me know if you'd like to include links to datasets, contact info, or licensing info.
