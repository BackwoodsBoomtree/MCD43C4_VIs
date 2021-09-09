# MODIS_HDF_to_TIFF_VIs

Calculate Vegetation Indices from MODIS data

This code can achieve the following:

* Convert MODIS HDF data to TIFF files
* Calculate Vegetation Indices NDVI, EVI, NIRv, and LSWI
* Aggregate temporally to 8-day or monthly time series
* Aggregate spatially to user defined resolution
* Package the resultant data into NC files for storage and sharing

## Description

This code is originally designed to work with [MCD43C4 v006](https://lpdaac.usgs.gov/products/mcd43c4v006/) surface reflectance data, which has a daily temporal resolution. However, it can be adapted and changed slightly to work with other MODIS products. The code currently supports the calculation of EVI, NDVI, NIRv, and LSWI.

Built into the workflow of this code is the reprojection of the resultant VI rasters into WGS84 and their masking with a land cover mask built from MCD12.

## Notes

Most modis data is version 4 HDF, which does not have native support in ncdf4 and other packages. ncdf4 could be built and deployed, but I had difficulty with this and found it easier to just convert HDF layers I need to tiff. This workflow requires extra processing time and is not ideal, but is most convenient.

This workflow uses the MODIS land cover data product to mask out non-land pixels.

The NC file output is basic and not CF compliant, but is sufficient. See my python code for producing CF-compliant NC files.

The extent and spatial info for the tif output of the bands from the hdf are funky,and we use gdal_translate function to pull them into tifs. So we apply the masking and reprojection and correction of the extent when calculating VIs.

## Authors

Russell Doughty, PhD - russell.doughty@ou.edu

## Potential Future Improvements

* Add land mask to 8-day function.

