
library(gdalUtils)
library(tools)
library(raster)
library(parallel)
# library(rslurm)

#### LOCAL ####

hdf_input          <- "/mnt/c/Russell/Temp/MCD43C4/original"
tmp_output         <- "/mnt/g/Temp/MCD43C4/tif"
vi_original_output <- "/mnt/g/MCD43C4/tif/Daily/0.05"
band_list          <- c(1, 2, 3, 6) # Don't change (RED, NIR, BLUE, SWIR)
vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI")
qc_filter          <- 3 # Worst quality allowed into the data (0 = best, 5 = worst for MDC43C4)
snow_filter        <- 0 # in percent (0 is no snow and excludes all pixels with any snow; 100 is no filter) 
land_mask          <- "/mnt/c/Russell/Git/land_mask/Land_Ocean_0.05deg_WGS84.tif"

#### SERVER ####

# hdf_input          <- "/scratch/boomtree/MCD43C4/original"
# band_output        <- "/scratch/boomtree/MCD43C4/reprocessed/tif"
# vi_original_output <- "/scratch/boomtree/MCD43C4/reprocessed/tif/Daily/0.05"
# band_list          <- c(1, 2, 3, 6) # Don't change (RED, NIR, BLUE, SWIR)
# vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI")
# land_mask          <- "/home/boomtree/land_mask/Land_Ocean_0.05deg_WGS84.tif"

hdf_to_vis    <- function (filename, tmp_dir, vi_dir, bands, vis, qc, snow, mask) {
  
  start <- Sys.time() # Start clock for timing
  
  # Build template raster and extent for projecting resultant rasters
  template_raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, ncols = 7200, nrows = 3600, crs = "+proj=longlat +datum=WGS84")
  
  # Create temp dirs for bands, QC mask, snow mask, and VIs
  for (band in bands) {
    if (!dir.exists(paste0(tmp_dir, "/b", band))) {
      dir.create(paste0(tmp_dir, "/b", band), recursive = TRUE)
    }
  }
  
  if (!dir.exists(paste0(tmp_dir, "/qc_mask"))) {
    dir.create(paste0(tmp_dir, "/qc_mask"), recursive = TRUE)
  }
  
  if (!dir.exists(paste0(tmp_dir, "/snow_mask"))) {
    dir.create(paste0(tmp_dir, "/snow_mask"), recursive = TRUE)
  }
  
  for (vi in vis) {
    if (!dir.exists(paste0(vi_dir, "/", vi))) {
      dir.create(paste0(vi_dir, "/", vi), recursive = TRUE)
    }
  }
  
  ############### Extract tifs from HDFs ####################
  
  sds           <- get_subdatasets(filename) # Get subdatasets
  file_basename <- file_path_sans_ext(basename(filename))
  file_num      <- 1
  
  print(paste0("Working on ", file_basename, " starting at ", start))
  
  #### BANDS ####
  
  for (band in bands) {
    
    band_filename <- paste0(tmp_dir, "/b", band, "/", file_basename, "_b", band, ".tif")
    
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

  #### MASKS ####
  
  qc_filename   <- paste0(tmp_dir, "/qc_mask/", file_basename, "_qc.tif")
  snow_filename <- paste0(tmp_dir, "/snow_mask/", file_basename, "_snow.tif")
  
  # Extract raster for QC and Snow layers
  gdal_translate(sds[8], qc_filename)
  gdal_translate(sds[11], snow_filename)
  
  # Import masks
  qc_mask <- raster(as.matrix(raster(qc_filename)), template = template_raster)
  s_mask  <- raster(as.matrix(raster(snow_filename)), template = template_raster)
  l_mask  <- raster(mask)
  
  # Make QC and Snow masks using input thresholds
  qc_mask[qc_mask > qc] <- 6
  s_mask[s_mask > snow] <- 101
  
  
  ############### Calculate Vegetation Indices ####################
  
  # Load Rasters
  b1   <- raster(list_band_filenames[1])
  b2   <- raster(list_band_filenames[2])
  b3   <- raster(list_band_filenames[3])
  b6   <- raster(list_band_filenames[4])
  
  # Basename for saving output
  file_out <- substr(file_path_sans_ext(basename(list_band_filenames[1])), 1, nchar(file_path_sans_ext(basename(list_band_filenames[1]))) - 17)
  
  if ("LSWI" %in% vis) {
    
    # Calculate VI and set to raster
    index <- raster(as.matrix((b2 - b6) / (b2 + b6)), template = template_raster)
    
    # Filter and scale to shrink output file size
    index[index > 1] <- NA
    index <- round(index, 4)
    index <- index * 1000
    
    # Masks
    index <- mask(index, qc_mask, maskvalue = 6)  # QC
    index <- mask(index, s_mask, maskvalue = 101) # Snow
    index <- mask(index, l_mask, maskvalue = 0)   # Land
    
    writeRaster(index, paste0(vi_dir, "/LSWI/", file_out, "_LSWI.tif"), overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
  }
  
  if ("EVI" %in% vis) {
    
    # Calculate VI and set to raster
    index <- raster(as.matrix(2.5 * (b2 - b1) / (b2 + (6 * b1) - (7.5 * b3) + 1)), template = template_raster)
    
    # Filter and scale to shrink output file size
    index[index < 0] <- NA
    index[index > 1] <- NA
    index <- round(index, 4)
    index <- index * 1000
    
    # Masks
    index <- mask(index, qc_mask, maskvalue = 6)  # QC
    index <- mask(index, s_mask, maskvalue = 101) # Snow
    index <- mask(index, l_mask, maskvalue = 0)   # Land
    
    writeRaster(index, paste0(vi_dir, "/EVI/", file_out, "_EVI.tif"), overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
  }
  
  if ("NDVI" %in% vis) {
    
    # Calculate VI and set to raster
    index <- raster(as.matrix((b2 - b1) / (b2 + b1)), template = template_raster)
    
    # Filter and scale to shrink output file size
    index[index < 0] <- NA
    index[index > 1] <- NA
    index <- round(index, 4)
    index <- index * 1000
    
    # Masks
    index <- mask(index, qc_mask, maskvalue = 6)  # QC
    index <- mask(index, s_mask, maskvalue = 101) # Snow
    index <- mask(index, l_mask, maskvalue = 0)   # Land
    
    writeRaster(index, paste0(vi_dir, "/NDVI/", file_out, "_NDVI.tif"), overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
  }
  
  if ("NIRv" %in% vis) {
    
    # Calculate VI and set to raster
    index <- raster(as.matrix((b2 - b1) / (b2 + b1) * b2), template = template_raster)
    
    # Filter and scale to shrink output file size
    index[index < 0] <- NA
    index[index > 1] <- NA
    index <- round(index, 4)
    index <- index * 1000
    
    # Masks
    index <- mask(index, qc_mask, maskvalue = 6)  # QC
    index <- mask(index, s_mask, maskvalue = 101) # Snow
    index <- mask(index, l_mask, maskvalue = 0)   # Land
    
    writeRaster(index, paste0(vi_dir, "/NIRv/", file_out, "_NIRv.tif"), overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
  }
  
  #### Remove all temporary files ####
  
  ### List of tmp files from raster package
  tmp_file_list <- list.files(paste0(tempdir(), "/raster"), full.names = TRUE, recursive = TRUE)

  # Isolate PID from temporary raster files and make sure it matches exactly
  pid           <- as.character(Sys.getpid()) # Filter list by processor ID
  tmp_file_list <- tmp_file_list[grepl(paste0("\\<", pid, "\\>"), substr(basename(tmp_file_list), 25, nchar(basename(tmp_file_list)) -10))]
  
  # Remove bands, masks, raster, and R tmp files files
  unlink(c(tmp_dir, tmp_file_list, tempfile()), recursive = TRUE)

  print(paste0("Done with ", file_basename, ". Time difference in minutes: ", round(difftime(Sys.time(), start, units = "mins"), 2)))

}

######## FOR Running locally ##########

hdf_list <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
mclapply(hdf_list, hdf_to_vis, mc.cores = 16, mc.preschedule = FALSE,
         tmp_dir = tmp_output, vi_dir = vi_original_output, bands = band_list, vis = vi_list, qc = qc_filter, snow = snow_filter, mask = land_mask)


######## FOR SLURM ##########

# # Create dataframe of function arguments for slurm_apply()
# hdf_list      <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
# pars          <- data.frame(filename = hdf_list)
# 
# # Queue up the job
# sjob <- slurm_apply(hdf_to_vis, pars, jobname = 'calc_VIs', submit = TRUE, cpus_per_node = 20, nodes = 1,
#                     tmp_dir = band_output, vi_dir = vi_original_output, bands = band_list, vis = vi_list, mask = land_mask)