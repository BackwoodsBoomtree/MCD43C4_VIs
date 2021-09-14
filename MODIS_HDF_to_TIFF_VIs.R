
library(gdalUtils)
library(tools)
library(raster)
library(ncdf4)
library(filesstrings)
library(parallel)
# library(rslurm)

#### LOCAL ####

hdf_input          <- "/mnt/c/Russell/Temp/MCD43C4/original"
band_output        <- "/mnt/g/Temp/MCD43C4/tif"
vi_original_output <- "/mnt/g/MCD43C4/tif/Daily/0.05"
band_list          <- c(1, 2, 3, 6) # Don't change (RED, NIR, BLUE, SWIR)
# vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI")
vi_list            <- c("EVI")
land_mask          <- "/mnt/c/Russell/Git/land_mask/Land_Ocean_0.05deg_WGS84.tif"

#### SERVER ####

# hdf_input          <- "/scratch/boomtree/MCD43C4/original"
# band_output        <- "/scratch/boomtree/MCD43C4/reprocessed/tif"
# vi_original_output <- "/scratch/boomtree/MCD43C4/reprocessed/tif/Daily/0.05"
# band_list          <- c(1, 2, 3, 6) # Don't change (RED, NIR, BLUE, SWIR)
# vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI")
# land_mask          <- "/home/boomtree/land_mask/Land_Ocean_0.05deg_WGS84.tif"

hdf_to_vis    <- function (filename, band_dir, vi_dir, bands, vis, mask) {
  
  start <- Sys.time()
  
  # new_tmpdir <- paste0("/tmp/Rtmp", Sys.getpid())
  # 
  # print(paste0("new directory is ", new_tmpdir))
  # 
  # Sys.setenv(TMPDIR = new_tmpdir)
  
  # Create temp dirs to store tif for each band if !exist
  for (band in bands) {
    if (!dir.exists(paste0(band_dir, "/b", band))) {
      dir.create(paste0(band_dir, "/b", band), recursive = TRUE)
    }
  }
  
  # Create dirs for each VI if !exist
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
  
  for (band in bands) {
    
    band_filename <- paste0(band_dir, "/b", band, "/", file_basename, "_b", band, ".tif")
    
    # Extract raster for given band
    gdal_translate(sds[band], band_filename)
    
    # Scale and truncate for saving space when output
    # raster_fixed <- raster(band_filename)
    # raster_fixed[raster_fixed > 1] <- NA
    # raster_fixed[raster_fixed < 0] <- NA
    # raster_fixed <- round(raster_fixed, 4)
    # raster_fixed <- raster_fixed * 1000
    # writeRaster(raster_fixed, band_filename, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
    
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

  # Import mask
  l_mask <- raster(mask)
  
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
    
    # Land Mask
    index  <- mask(index, l_mask, maskvalue = 0) # Mask by land
    
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
    
    # Land Mask
    index  <- mask(index, l_mask, maskvalue = 0) # Mask by land
    
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
    
    # Land Mask
    index  <- mask(index, l_mask, maskvalue = 0) # Mask by land
    
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
    
    # Land Mask
    index  <- mask(index, l_mask, maskvalue = 0) # Mask by land
    
    writeRaster(index, paste0(vi_dir, "/NIRv/", file_out, "_NIRv.tif"), overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
  }
  
  # Delete temporary band files
  for (f in 1:length(list_band_filenames)) {
    if (file.exists(list_band_filenames[f])) {
      file.remove(list_band_filenames[f])
    }
  }
  
  ### Remove temporary files from raster package
  tmp_file_list <- list.files(paste0(tempdir(), "/raster"), full.names = TRUE, recursive = TRUE)

  # Filter list by processor ID
  pid <- as.character(Sys.getpid())
  
  # Isolate PID from temporary raster files and make sure it matches exactly
  tmp_file_list <- tmp_file_list[grepl(paste0("\\<", pid, "\\>"), substr(basename(tmp_file_list), 25, nchar(basename(tmp_file_list)) -10))]
  
  # Remove raster package temporary files
  unlink(tmp_file_list)

  # Remove R temp file if any
  unlink(tempfile())
  
  print(paste0("Done with ", file_basename, ". Time difference in minutes: ", round(difftime(Sys.time(), start, units = "mins"), 2)))
  
}

######## FOR Running locally ##########

hdf_list <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
mclapply(hdf_list, hdf_to_vis, mc.cores = 14, mc.preschedule = FALSE,
         band_dir = band_output, vi_dir = vi_original_output, bands = band_list, vis = vi_list, mask = land_mask)


######## FOR SLURM ##########

# # Create dataframe of function arguments for slurm_apply()
# hdf_list      <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
# pars          <- data.frame(filename = hdf_list)
# 
# # Queue up the job
# sjob <- slurm_apply(hdf_to_vis, pars, jobname = 'calc_VIs', submit = TRUE, cpus_per_node = 20, nodes = 1,
#                     band_dir = band_output, vi_dir = vi_original_output, bands = band_list, vis = vi_list, mask = land_mask)