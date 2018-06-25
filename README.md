# DrylandAg
Listed below are scripts associated with analyses and figures for manuscript on Hawaii Island dryland agriculture and seasonality.  For now, please cite (for now) Kagawa-Viviani A.K., N.K. Lincoln, S. Quintus, M.P. Lucas, T.W. Giambelluca. (in review) Spatial patterns of seasonal crop production suggest coordination within and across dryland agricultural systems of Hawaiʻi Island. Ecology and Society.

## [FieldSystemCovariates_Polygons_GraphsFin.R](https://github.com/akkagawa/DrylandAg/blob/master/FieldSystemCovariates_Polygons_GraphsFin.R)
This script extracts values of gridded climate data for known locations and generates visualizations including Figures 2, 3, A2 in the manuscript.

## [FieldSystemCovariates_CombinedAnalysesFin.R](https://github.com/akkagawa/DrylandAg/blob/master/FieldSystemCovariates_CombinedAnalysesFin.R)
This script, associated with [FieldSystemCovariates_Polygons_GraphsFin.R](https://github.com/akkagawa/DrylandAg/blob/master/FieldSystemCovariates_Polygons_GraphsFin.R), 
1) generates rasters for seasonality metrics of aridity
2) explores intra-annual shifting of pre-defined "optimal" growing conditions
3) generates Figures 4, 7 in the manuscript

References: 
1. Feng X, Porporato A, Rodriguez-Iturbe I (2013) Changes in rainfall seasonality in the tropics. Nature Clim Change 3:811-815. [doi: 10.1038/nclimate1907](https://www.nature.com/articles/nclimate1907)
2. Valenzuela, H., S. Fukuda, and A. Arakaki (1994) Sweetpotato Production Guides for Hawaii. Pages 1-10. Hawaii Institute of Tropical Agriculture and Human Resources. http://hdl.handle.net/10125/5497

## Sensitivity analysis (author: MP Lucas)
### [MPL_tair_rf_cond_loop_dryag_area_sensitivity.R](https://github.com/akkagawa/DrylandAg/blob/master/MPL_tair_rf_cond_loop_dryag_area_sensitivity.R)
### [MPL_tair_airdity_cond_loop_dryag_area_sensitivity.R](https://github.com/akkagawa/DrylandAg/blob/master/MPL_tair_airdity_cond_loop_dryag_area_sensitivity.R)
Sensitivity analysis calculating percent area meeting various combinations of rainfall/aridity and air temp values. This code takes monthly rainfall (mm)/aridity & tair (C) rasters and reclasses multiple combinations of these two varibles on a monthly basis to define a crop-able (cultivable) area, then combines all 12 months to calculate annual % of dry ag system crop-able, as defined by >=1 month of crop-able area. 

## [FieldSystemThresholding3.R](https://github.com/akkagawa/DrylandAg/blob/master/FieldSystemThresholding3.R)
This script relies on output (rf_tair_per_crop_FINAL_fine.csv, arid_tair_per_crop_FINAL_fine.csv) generated by MP Lucas with scripts [MPL_tair_rf_cond_loop_dryag_area_sensitivity.R](https://github.com/akkagawa/DrylandAg/blob/master/MPL_tair_rf_cond_loop_dryag_area_sensitivity.R) and [MPL_tair_airdity_cond_loop_dryag_area_sensitivity.R](https://github.com/akkagawa/DrylandAg/blob/master/MPL_tair_airdity_cond_loop_dryag_area_sensitivity.R) respectively.  This code generate plotly interactive graphs assessing temperature x rainfall or temperature x aridity combinations that allow for the field system "cultivability" to be visualized (Figures A3.1 and A3.2).  We used this to define a common threshold across the three systems for sake of visualizing the shifting seasonal envelope (for Figures 5, 6, 7).
