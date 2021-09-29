# MCD43 Vegetation Indicies

These codes can achieve the following:

* Calculate Vegetation Indices NDVI, EVI, NIRv, and LSWI and save files in original spatiotemporal resolution (MODIS_HDF_to_TIFF_VIs.R)
* Reorganize output files by year (Annual_Folders.R)
* Aggregate temporally to 8-day or monthly time series (Aggregate_Daily_VIs.R)
* Aggregate spatially to user defined spatial resolution
* Package the resultant data into NC files for storage and sharing (nc_by_year.R)

## Description

This code is originally designed to work with [MCD43C4 v006](https://lpdaac.usgs.gov/products/mcd43c4v006/) surface reflectance data, which has a daily temporal resolution. However, it can be adapted and changed slightly to work with other MODIS products. The code currently supports the calculation of EVI, NDVI, NIRv, and LSWI.

## Workflow

The workflow and codes are listed above in order. It might be necessary to correct the extent on some output files after running MODIS_HDF_to_TIFF_VIs.R. For some reason, the extent on about 10% of the files were not corrected during the processing with this code - which may have had something to do with parallel processing. Use fix_extent.R to identify the files with improper extent and overwrite them in place. To double check it goes smoothly before overwriting, use fix_extent_into_new_dir.R and copy, paste, and overwrite files manually if the new files look good.

## Masks

User can define a snow and qc threshold to use when extracting the MCD43C4 data. The code also employs a land cover mask from MCD12 to ensure only land pixels are included in the output. Fork the code to change filters and techniques when processing other datasets, as needed.

A zero tolerance for snow cover was selected per Walther et. al. (2016). We also found that there is very little data in the tropics if the QC filter was set lower than 3 (0 = best and 5 = worst). Thus, we elected to use a QC filter of 3.

## Projection

Output data is projected into WGS84 and also the extent is set to -180, 180, 90, and 90.

## Aggregation

The current version supports temporal aggregation to 8-day and monthly resolution. If there are not enough days to fill the entire period, then it is skipped automatically. In this way, the code can be run on partial years or be used in a more operational format - we do not need an entire year's worth of data to run the code.

Spatial aggregation can be done to any user defined spatial resolution.

## Notes

* The "Error in (function (x)  : attempt to apply non-function" message should be safely ignored. See https://github.com/rspatial/terra/issues/30.

* Code has been updated to use the terra package, which is a replacement for the raster package. Terra has the ability to extract HDF4 subdatasets directly into memory using sds(), whereas the old workflow was to first write them out to tif using gdal_translate(). Speed is greatly improved.
* The NC file output is basic and not CF compliant, but is sufficient. See my python code for producing CF-compliant NC files.

## Authors

Russell Doughty, PhD - russell.doughty@ou.edu

## Potential Future Improvements

* Work fix_extent code into MODIS_HDF_to_TIFF_VIs.R to ensure proper extent on output.

## Citations

Walther, S., Voigt, M., Thum, T., Gonsamo, A., Zhang, Y., Köhler, P., Jung, M., Varlagin, A. and Guanter, L., 2016. Satellite chlorophyll fluorescence measurements reveal large‐scale decoupling of photosynthesis and greenness dynamics in boreal evergreen forests. Global change biology, 22(9), pp.2979-2996.

https://github.com/rspatial/terra

