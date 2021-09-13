
library(gdalUtils)
library(tools)
library(raster)
library(ncdf4)
library(filesstrings)
library(rslurm)

hdf_input          <- "/scratch/boomtree/MCD43C4/original"
band_output        <- "/scratch/boomtree/MCD43C4/reprocessed/tif"
vi_original_output <- "/scratch/boomtree/MCD43C4/reprocessed/tif/Daily/0.05"
band_list          <- c(1, 2, 3, 6) # Don't change (RED, NIR, BLUE, SWIR)
vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI")
land_mask          <- "/home/boomtree/land_mask/Land_Ocean_0.05deg_WGS84.tif"


hdf_to_vis    <- function (filename, band_dir, vi_dir, bands, vis, mask) {
  
  start <- Sys.time()
  
  ############### Extract tifs from HDFs ####################
  
  sds           <- get_subdatasets(filename) # Get subdatasets
  file_basename <- file_path_sans_ext(basename(filename))
  file_num      <- 1
  
  print(paste0("Working on ", file_basename, " starting at ", start))
  
  for (band in bands) {

    band_filename <- paste0(band_dir, "/b", band, "/", file_basename, "_b", band, ".tif")
    
    # Extract raster for given band
    gdal_translate(sds[band], band_filename)
    
    # Create list of band file names for VI processing and deletion
    if (file_num == 1) {
      list_band_filenames <- band_filename
      
    } else {
      list_band_filenames <- c(list_band_filenames, band_filename)
    }
    
    file_num <- file_num + 1
  }
   
  ############### Calculate Vegetation Indices ####################
  
  # Build template raster and extent for projecting resultant rasters
  template_raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, ncols = 7200, nrows = 3600, crs = "+proj=longlat +datum=WGS84")
  ext             <- extent(-180, 180, -90, 90)
  
  # Import mask
  l_mask <- raster(mask)

  # Load Rasters
  b1   <- raster(list_band_filenames[1])
  b2   <- raster(list_band_filenames[2])
  b3   <- raster(list_band_filenames[3])
  b6   <- raster(list_band_filenames[4])

  # Reflectance is 0 to 1
  b1[b1 < 0] <- NA
  b1[b1 > 1] <- NA
  b2[b2 < 0] <- NA
  b2[b2 > 1] <- NA
  b3[b3 < 0] <- NA
  b3[b3 > 1] <- NA
  b6[b6 < 0] <- NA
  b6[b6 > 1] <- NA
  
  # Basename for saving output
  file_out <- substr(file_path_sans_ext(basename(list_band_filenames[1])), 1, nchar(file_path_sans_ext(basename(list_band_filenames[1]))) - 17)
  
  if ("EVI" %in% vis) {
    
    # Calculate VI and exclude bad values
    evi          <- 2.5 * (b2 - b1) / (b2 + (6 * b1) - (7.5 * b3) + 1)
    evi[evi < 0] <- NA
    evi[evi > 1] <- NA
    
    # Set extent, project, and mask resultant raster
    extent(evi) <- ext
    evi         <- setExtent(evi, ext)
    evi         <- projectRaster(evi, template_raster)
    evi         <- mask(evi, l_mask, maskvalue = 0) # Mask by land
    
    writeRaster(evi, paste0(vi_dir, "/EVI/", file_out, "_EVI.tif"), overwrite = TRUE)
  }
  
  if ("NDVI" %in% vis) {
    
    # Calculate VI and exclude bad values
    ndvi           <- (b2 - b1) / (b2 + b1)
    ndvi[ndvi < 0] <- NA
    ndvi[ndvi > 1] <- NA
    
    # Set extent and project resultant raster
    extent(ndvi) <- ext
    ndvi         <- setExtent(ndvi, ext)
    ndvi         <- projectRaster(ndvi, template_raster)
    ndvi         <- mask(ndvi, l_mask, maskvalue = 0) # Mask by land
    
    writeRaster(ndvi, paste0(vi_dir, "/NDVI/", file_out, "_NDVI.tif"), overwrite = TRUE)
  }
  
  if ("NIRv" %in% vis) {
    
    # Calculate VI and exclude bad values
    nirv           <- ((b2 - b1) / (b2 + b1)) * b2
    nirv[nirv < 0] <- NA
    nirv[nirv > 1] <- NA
    
    # Set extent and project resultant raster
    extent(nirv) <- ext
    nirv         <- setExtent(nirv, ext)
    nirv         <- projectRaster(nirv, template_raster)
    nirv         <- mask(nirv, l_mask, maskvalue = 0) # Mask by land
    
    writeRaster(nirv, paste0(vi_dir, "/NIRv/", file_out, "_NIRv.tif"), overwrite = TRUE)
  }
  
  if ("LSWI" %in% vis) {
    
    # Calculate VI and exclude bad values
    lswi <- (b2 - b6) / (b2 + b6)
    lswi[nirv < 0] <- NA
    lswi[lswi > 1] <- NA
    
    # Set extent and project resultant raster
    extent(lswi) <- ext
    lswi         <- setExtent(lswi, ext)
    lswi         <- projectRaster(lswi, template_raster)
    lswi         <- mask(lswi, l_mask, maskvalue = 0) # Mask by land
    
    writeRaster(lswi, paste0(vi_dir, "/LSWI/", file_out, "_LSWI.tif"), overwrite = TRUE)
  }
  
  # Delete temporary band files
  for (f in 1:length(list_band_filenames)) {
    if (file.exists(list_band_filenames[f])) {
      file.remove(list_band_filenames[f])
    }
  }
  
  print(paste0("Done with ", file_basename, ". Time difference in minutes: ", round(difftime(Sys.time(), start, units = "mins"), 2)))
  
}


# Create temp dirs to store tif for each band if !exist
for (band in band_list) {
  if (!dir.exists(paste0(band_output, "/b", band))) {
    dir.create(paste0(band_output, "/b", band), recursive = TRUE)
  }
}

# Create dirs for each VI if !exist
for (vi in vi_list) {
  if (!dir.exists(paste0(vi_original_output, "/", vi))) {
    dir.create(paste0(vi_original_output, "/", vi), recursive = TRUE)
  }
}

# Create dataframe of function arguments for slurm_apply()
hdf_list      <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
pars          <- data.frame(filename = hdf_list)

# Queue up the job
sjob <- slurm_apply(hdf_to_vis, pars, jobname = 'calc_VIs', submit = TRUE, cpus_per_node = 20,
                    band_dir = band_output, vi_dir = vi_original_output, bands = band_list, vis = vi_list, mask = land_mask)
