# Dendrometers_DifferencesInGrowthOfUnderstoryAndCanopyTrees

Scripts used to calculate the datasets underlying the study  
**“Growth patterns and climate sensitivity differ for understory and canopy trees.”**

The processed dendrometer data and meteorological records ready for analysis are available on **Zenodo** (DOI to be added).

---

## Project Structure

### `Datasets/`
Contains **examples of the primary data** used in the project.  
The full primary datasets used in the study are available on Zenodo.

- `Dendrometer_data/` – Dendrometer records for the study sites (stored in site-specific folders); each year is stored in a separate spreadsheet.
  - `Boubin/` – Dendrometer data for the Boubín site  
  - `Eustaska/` – Dendrometer data for the Eustaška site  
  - `Ranspurk/` – Dendrometer data for the Ranšpurk site  
  - `Zofin/` – Dendrometer data for the Žofín site  

- `Hemiphoto/` – Hemispherical photographs taken above (or close to) the apex of juvenile trees.
  - `Boubin/` – Hemiphotos for the Boubín site  
  - `Eustaska/` – Hemiphotos for the Eustaška site  
  - `Ranspurk/` – Hemiphotos for the Ranšpurk site  
  - `Zofin/` – Hemiphotos for the Žofín site  

- `Junk_data/` – Examples of data quality assessment for each *tree × year* combination (indicating whether a given record is suitable for analysis, including DOY values defining the beginning and end of the analysed period).

- `Metadata/` – Metadata for individual trees and study sites.

- `Meteo_data/` – Examples of meteorological records for the study sites (stored in site-specific folders); each year is stored in a separate spreadsheet.
  - `Boubin/` – Meteorological records for the Boubín site  
  - `Eustaska/` – Meteorological records for the Eustaška site  
  - `Ranspurk/` – Meteorological records for the Ranšpurk site  
  - `Zofin/` – Meteorological records for the Žofín site  

- `Meteo_data_understory/` – Examples of meteorological records (air temperature and humidity) measured in the understory; each year is stored in a separate spreadsheet.

- `SWP_data/` – Examples of Soil Water Potential (SWP) datasets measured at the study sites; each year is stored in a separate spreadsheet.
  - `Boubin/` – SWP records for the Boubín site  
  - `Eustaska/` – SWP records for the Eustaška site  
  - `Ranspurk/` – SWP records for the Ranšpurk site  
  - `Zofin/` – SWP records for the Žofín site  

---

### `Datasets_recalculated/`
Contains **examples of processed datasets** used for analysis, based on the example input data.

Due to the limited number of trees in the example datasets, results of the fitted models are not included. However, all dendrometer and meteorological data can be fully recalculated using the complete datasets available on Zenodo. The functionality of all scripts was checked prior to publication.

- `Calculated_models/` – Folder prepared for `.rds` files containing GLM and GLMM models.
  - `base_models/` – Models based on standard meteorological variables measured at meteorological stations  
  - `understory_models/` – Models based on meteorological variables measured in the understory (paired sensors)
- `Hemiphoto_results/` – Spreadsheets with estimates of canopy cover above the apex of understory trees.
- `Recalculated_datasets/` – Datasets generated using the scripts described below.
- `Recalculated_stationclimate/` – Recalculated climate datasets generated using the scripts described below.

---

### `Functions_Data_preparation/`
Scripts used for data preparation and evaluation of model quality.

### `Functions_figure_making_and_data_analysis/`
Scripts used to generate figures included in the study and to perform analyses related to figure content.

### `Outputs/`
Main outputs used in the associated manuscript.

---

## Dendrometer and Climate Data

Part of the data used in this study were previously published in:

- **Kašpar J et al. (2024)**  
  *The effects of solar radiation on daily and seasonal stem increment of canopy trees in European temperate old-growth forests.*  
  *New Phytologist* **230**, 662–673.

- **Kašpar J et al. (2024)**  
  *Dataset to paper “The effects of solar radiation on daily and seasonal stem increment of canopy trees in European temperate old-growth forests”.*  
  (Version 1.0) [Data set]. Zenodo.  
  https://doi.org/10.5281/zenodo.11127186

The dataset with recalculated data used in the present analysis is available on Zenodo:

- **Kašpar J et al. (2026)**  
  *Dataset to paper “Growth patterns and climate sensitivity differ for understory and canopy trees”.*  
  (Version 1.0) [Data set]. Zenodo.  
  **DOI: to be added**

