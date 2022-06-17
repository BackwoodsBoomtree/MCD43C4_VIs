# MCD43 Vegetation Indices

These codes can achieve the following:

* Calculate Vegetation Indices NDVI, EVI, NIRv, and LSWI, and also Red and NIR bands, and save files in original spatio-temporal resolution (MODIS_HDF_to_TIFF_VIs.R)
* Reorganize output files by year (Annual_Folders.R)
* Aggregate temporally to 8-day or monthly time series (Aggregate_Daily_VIs.R)
* Aggregate spatially to user defined spatial resolution
* Package the resultant data into NC files for storage and sharing (nc_by_year.R)
* Added Daily_nc_to_monthly.R and Aggregate_nc_spatial.R for aggregation of the nc files directly

## Description

This code is originally designed to work with [MCD43C4 v006](https://lpdaac.usgs.gov/products/mcd43c4v006/) surface reflectance data, which has a daily temporal resolution. However, it can be adapted and changed slightly to work with other MODIS products. The code currently supports the calculation of EVI, NDVI, NIRv, and LSWI, and can also be used to grid Red and NIR bands.

## Workflow

* It is important to try and not exceed the RAM by setting the number of cores too high. Otherwise, it will output temporary files to disc, and some funky stuff can occur. For instance, files might be skipped, have 0 byte size, the extent can be wrong, or the error below can occur. If these occur, then use the missing_files() function and the fix_extent.R code as described below.

The workflow and codes are listed above in order. It might be necessary to correct the extent on some output files after running MODIS_HDF_to_TIFF_VIs.R. When running parallel with too many cores, the extent of the files may be incorrect. Use fix_extent.R to identify the files with improper extent and overwrite them in place. To double check it goes smoothly before overwriting, use fix_extent_into_new_dir.R and copy, paste, and overwrite files manually if the new files look good.

## Masks

User can define a snow and qc threshold to use when extracting the MCD43C4 data. The code also employs a land cover mask from MCD12 to ensure only land pixels are included in the output. Fork the code to change filters and techniques when processing other datasets, as needed.

A zero tolerance for snow cover was selected per Walther et. al. (2016). We also found that there is very little data in the tropics if the QC filter was set lower than 3 (0 = best and 5 = worst). Thus, we elected to use a QC filter of 3.

## Geographic Coordinate System

Output data is projected into WGS84 and also the extent is set to -180, 180, 90, and 90.

## Aggregation

The current version supports temporal aggregation to 8-day and monthly resolution. If there are not enough days to fill the entire period, then it is skipped automatically. In this way, the code can be run on partial years or be used in a more operational format - we do not need an entire year's worth of data to run the code.

Spatial aggregation can be done to any user defined spatial resolution.

## Notes

* There might be 0 byte output files (zero size), or missing files all together due to running out of RAM when number of cores is too high. The missing_files() function can be used to identify missing and zero byte files and run the calcs again for those files.
* The "Error in (function (x)  : attempt to apply non-function" message should be safely ignored. See https://github.com/rspatial/terra/issues/30.
* Code has been updated to use the terra package, which is a replacement for the raster package. Terra has the ability to extract HDF4 subdatasets directly into memory using sds(), whereas the old workflow was to first write them out to tif using gdal_translate(). Speed is greatly improved.
* The NC file output is basic and not CF compliant, but is sufficient. See my python code for producing CF-compliant NC files.
* For scripts with parallel processing, temporary folders are created for each process and then deleted. Otherwise, the disk space will fill up because the standard temporary directory is not deleted until the R session is closed.

## Authors

Russell Doughty, PhD - russell.doughty@ou.edu

## Potential Future Improvements

* None.

## Citations

Walther, S., Voigt, M., Thum, T., Gonsamo, A., Zhang, Y., Köhler, P., Jung, M., Varlagin, A. and Guanter, L., 2016. Satellite chlorophyll fluorescence measurements reveal large‐scale decoupling of photosynthesis and greenness dynamics in boreal evergreen forests. Global change biology, 22(9), pp.2979-2996.

https://github.com/rspatial/terra
