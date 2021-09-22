
library(terra)
library(parallel)
# library(rslurm)

terraOptions(memfrac = 0.8) # Fraction of memory to allow terra

#### Input variables ####

tmpdir             <- "/mnt/c/Rwork"
hdf_input          <- "/mnt/c/Russell/Temp/MCD43C4/original"
vi_dir             <- "/mnt/g/MCD43C4/tif/Daily/0.05"
vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI")
qc_filter          <- c(4, 5) # Flags to exclude (0 = best, 5 = worst for MDC43C4)
snow_filter        <- 0 # in percent (0 is no snow and excludes all pixels with any snow; 100 is no filter) 
land_mask          <- "/mnt/c/Russell/Git/land_mask/Land_Ocean_0.05deg_Clark1866.tif"


#### Functions ####

calc_evi   <- function(b1, b2, b3) {
  index            <- 2.5 * (b2 - b1) / (b2 + 6 * b1 - 7.5 * b3 + 1)
  index[index > 1] <- NA
  index[index < 0] <- NA
  names(index)     <- "EVI"
  index <- round(index, digits = 4) * 10000
}
calc_ndvi  <- function(b1, b2) {
  index            <- (b2 - b1) / (b2 + b1)
  index[index > 1] <- NA
  index[index < 0] <- NA
  names(index)     <- "NDVI"
  index <- round(index, digits = 4) * 10000
}
calc_nirv  <- function(b1, b2) {
  index            <- (b2 - b1) / (b2 + b1) * b2
  index[index > 1] <- NA
  index[index < 0] <- NA
  names(index)     <- "NIRv"
  index <- round(index, digits = 4) * 10000
}
calc_lswi  <- function(b2, b6) {
  index             <- (b2 - b6) / (b2 + b6)
  index[index > 1]  <- NA
  index[index < -1] <- NA
  names(index)      <- "LSWI"
  index <- round(index, digits = 4) * 10000
}
mask_all   <- function(index, data_cube, qc_filter, snow_filter, land_mask) {

  index <- mask(index, data_cube[[8]], maskvalues = qc_filter)
  index <- mask(index, land_mask, maskvalues = 0)

  if (snow_filter == 0) {
    index <- mask(index, data_cube[[11]], maskvalues = 0, inverse = TRUE)
  } else {
    s_mask <- data_cube[[11]]
    s_mask[s_mask > snow_filter] <- 101
    index <- mask(index, s_mask, maskvalues = 101)
  }
  return(index)
}
proj_wgs84 <- function(index) {
  
  index      <- terra::project(index, "+proj=longlat +datum=WGS84")
}
tmp_create <- function(tmpdir) {
  
  p_tmp_dir <- paste0(tmpdir, "/", as.character(Sys.getpid())) # Process ID

  if (!dir.exists(p_tmp_dir)) {
    dir.create(p_tmp_dir, recursive = TRUE)
  }
  
  terraOptions(tempdir = p_tmp_dir)
}
tmp_remove <- function(tmpdir) {
  
  p_tmp_dir <- paste0(tmpdir, "/", as.character(Sys.getpid())) # Process ID
  unlink(p_tmp_dir, recursive = TRUE)
}
save_vis   <- function(filename, vi_dir, vi_list) {
  
  start <- Sys.time() # Start clock for timing
  file_out <- file_out <- substr(basename(filename), 1, nchar(basename(filename)) - 18)
  print(paste0("Working on ", file_out, " starting at ", start))
  
  tmp_create(tmpdir)
  
  cube <- sds(filename)
  
  # Import land mask and make sure extent matches the cube
  land_mask      <- rast(land_mask)
  orig_extent    <- ext(cube)
  ext(land_mask) <- orig_extent

  if ("EVI" %in% vi_list) {
    index      <- calc_evi(cube[[1]], cube[[2]], cube[[3]])
    index      <- mask_all(index, cube, qc_filter, snow_filter, land_mask)
    ext(index) <- c(-180, 180, -90, 90)
    index      <- proj_wgs84(index)
    writeRaster(index, paste0(vi_dir, "/EVI/", file_out, ".EVI.tif"), overwrite = TRUE, datatype = 'INT4S', NAflag = -9999)
  }

  if ("NDVI" %in% vi_list) {
    index      <- calc_ndvi(cube[[1]], cube[[2]])
    index      <- mask_all(index, cube, qc_filter, snow_filter, land_mask)
    ext(index) <- c(-180, 180, -90, 90)
    index      <- proj_wgs84(index)
    writeRaster(index, paste0(vi_dir, "/NDVI/", file_out, ".NDVI.tif"), overwrite = TRUE, datatype = 'INT4S', NAflag = -9999)
  }
  
  if ("NIRv" %in% vi_list) {
    index      <- calc_nirv(cube[[1]], cube[[2]])
    index      <- mask_all(index, cube, qc_filter, snow_filter, land_mask)
    ext(index) <- c(-180, 180, -90, 90)
    index      <- proj_wgs84(index)
    writeRaster(index, paste0(vi_dir, "/NIRv/", file_out, ".NIRv.tif"), overwrite = TRUE, datatype = 'INT4S', NAflag = -9999)
  }

  if ("LSWI" %in% vi_list) {
    index      <- calc_lswi(cube[[2]], cube[[6]])
    index      <- mask_all(index, cube, qc_filter, snow_filter, land_mask)
    ext(index) <- c(-180, 180, -90, 90)
    index      <- proj_wgs84(index)
    writeRaster(index, paste0(vi_dir, "/LSWI/", file_out, ".LSWI.tif"), overwrite = TRUE, datatype = 'INT4S', NAflag = -9999)
  }
  
  tmp_remove(tmpdir)

  #### For the old raster package ####
  # ### List of tmp files from raster package
  # tmp_file_list <- list.files(paste0(tempdir(), "/raster"), full.names = TRUE, recursive = TRUE)
  # print("tmp file list")
  # print(tmp_file_list)
  # 
  # # Isolate PID from temporary raster files and make sure it matches exactly
  # pid           <- as.character(Sys.getpid()) # Filter list by processor ID
  # tmp_file_list <- tmp_file_list[grepl(paste0("\\<", pid, "\\>"), substr(basename(tmp_file_list), 25, nchar(basename(tmp_file_list)) -10))]
  # print(pid)
  # print(tmp_file_list)
  # 
  # # Remove raster and R tmp files
  # unlink(c(tmp_file_list, tempfile()))

  print(paste0("Done with ", file_out, ". Time difference in minutes: ", round(difftime(Sys.time(), start, units = "mins"), 2)))

}

######## FOR Running locally ##########

for (vi in vi_list) {
  if (!dir.exists(paste0(vi_dir, "/", vi))) {
    dir.create(paste0(vi_dir, "/", vi), recursive = TRUE)
  }
}

hdf_list <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
mclapply(hdf_list, save_vis, mc.preschedule = FALSE, mc.cores = 5, vi_dir = vi_dir, vi_list = vi_list)


######## FOR SLURM ##########

# # Create dataframe of function arguments for slurm_apply()
# hdf_list      <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
# pars          <- data.frame(filename = hdf_list)
# 
# # Queue up the job
# sjob <- slurm_apply(hdf_to_vis, pars, jobname = 'calc_VIs', submit = TRUE, cpus_per_node = 20, nodes = 1,
#                     tmp_dir = band_output, vi_dir = vi_original_output, bands = band_list, vis = vi_list, mask = land_mask)