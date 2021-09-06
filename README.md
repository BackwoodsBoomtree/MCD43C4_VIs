# MODIS_HDF_to_TIFF_VIs

Make TIFFs and Vegetation Indices from MODIS data

## Description

This code is originally designed to work with [MCD43C4 v006](https://lpdaac.usgs.gov/products/mcd43c4v006/) surface reflectance data, which has a daily temporal resolution.

This code can achieve the following:

* Convert MODIS HDF data to TIFF files
* Calculate Vegetation Indices NDVI, EVI, NIRv, and LSWI
# 

Most modis data is version 4 HDF, which does not have native support in ncdf4 and other packages. ncdf4 could be built and deployed, but I had difficulty with this and found it easier to just
# convert HDF layers I need to tiff.
# 

## Getting Started

### Dependencies

* Describe any prerequisites, libraries, OS version, etc., needed before installing program.
* ex. Windows 10

### Installing

* How/where to download your program
* Any modifications needed to be made to files/folders

### Executing program

* How to run the program
* Step-by-step bullets
```
code blocks for commands
```

## Help

Any advise for common problems or issues.
```
command to run if program contains helper info
```

## Authors

Contributors names and contact info

ex. Dominique Pizzie  
ex. [@DomPizzie](https://twitter.com/dompizzie)

## Version History

* 0.2
    * Various bug fixes and optimizations
    * See [commit change]() or See [release history]()
* 0.1
    * Initial Release

## License

This project is licensed under the [NAME HERE] License - see the LICENSE.md file for details

## Acknowledgments

Inspiration, code snippets, etc.
* [awesome-readme](https://github.com/matiassingers/awesome-readme)
* [PurpleBooth](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2)
* [dbader](https://github.com/dbader/readme-template)
* [zenorocha](https://gist.github.com/zenorocha/4526327)
* [fvcproductions](https://gist.github.com/fvcproductions/1bfc2d4aecb01a834b46)
