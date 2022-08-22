library(terra)
library(rslurm)

#### Input variables ####

hdf_input          <- "/ourdisk/hpc/geocarb/data_share/MCD43C4/v061/original"
vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
vi_dir             <- "/ourdisk/hpc/geocarb/data_share/MCD43C4/v061/tif/daily/0.05"
# qc_filter          <- c(4, 5) # Flags to exclude (0 = best, 5 = worst for MDC43C4)
qc_filter          <- NA
snow_filter        <- 0 # in percent (0 is no snow and excludes all pixels with any snow; 100 is no filter) 
land_mask          <- "/ourdisk/hpc/geocarb/data_share/land_mask/Land_Ocean_0.05deg_Clark1866.tif"

# Create output dirs
for (vi in vi_list) {
  if (!dir.exists(paste0(vi_dir, "/", vi))) {
    dir.create(paste0(vi_dir, "/", vi), recursive = TRUE)
    print(paste0("Created ", vi_dir, "/", vi))
  }
}

# Create dataframe of function arguments for slurm_apply()
hdf_list <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
for (i in 1:length(vi_list)) {
  vi_long <- data.frame(filename = hdf_list, vi = rep(vi_list[i], length(hdf_list)))
  if (i == 1) {
    pars <- vi_long
  } else {
    pars <- rbind(pars, vi_long)
  }
}

#### Functions ####
save_vis     <- function(filename, vi, vi_dir, qc_filter, snow_filter, land_mask) {
  
  # FUNCTIONS ##
  calc_evi     <- function(b1, b2, b3) {
    index            <- 2.5 * (b2 - b1) / (b2 + 6 * b1 - 7.5 * b3 + 1)
    index[index > 1] <- NA
    index[index < 0] <- NA
    names(index)     <- "EVI"
    index            <- round(index, digits = 4) * 10000
    gc()
    return(index)
  }
  calc_ndvi    <- function(b1, b2) {
    index            <- (b2 - b1) / (b2 + b1)
    index[index > 1] <- NA
    index[index < 0] <- NA
    names(index)     <- "NDVI"
    index            <- round(index, digits = 4) * 10000
    gc()
    return(index)
  }
  calc_nirv    <- function(b1, b2) {
    index            <- (b2 - b1) / (b2 + b1) * b2
    index[index > 1] <- NA
    index[index < 0] <- NA
    names(index)     <- "NIRv"
    index            <- round(index, digits = 4) * 10000
    gc()
    return(index)
  }
  calc_lswi    <- function(b2, b6) {
    index             <- (b2 - b6) / (b2 + b6)
    index[index > 1]  <- NA
    index[index < -1] <- NA
    names(index)      <- "LSWI"
    index             <- round(index, digits = 4) * 10000
    gc()
    return(index)
  }
  calc_red     <- function(b1) {
    index             <- b1
    names(index)      <- "RED"
    index             <- round(index, digits = 4) * 10000
    gc()
    return(index)
  }
  calc_nir     <- function(b2) {
    index             <- b2
    names(index)      <- "NIR"
    index             <- round(index, digits = 4) * 10000
    gc()
    return(index)
  }
  mask_all     <- function(index, data_cube, qc_filter, snow_filter, land_mask) {
    
    if (!is.na(qc_filter)) {
      index <- mask(index, data_cube[[8]], maskvalues = qc_filter)
    }
    
    index <- mask(index, land_mask, maskvalues = 0)
    
    if (snow_filter == 0) {
      index <- mask(index, data_cube[[11]], maskvalues = 0, inverse = TRUE)
    } else {
      s_mask                       <- data_cube[[11]]
      s_mask[s_mask > snow_filter] <- 101
      index                        <- mask(index, s_mask, maskvalues = 101)
    }
    gc()
    return(index)
  }
  
  file_out      <- substr(basename(filename), 1, nchar(basename(filename)) - 18)
  file_out      <- paste0(file_out, ".", vi, ".tif")
  out_path_name <- paste0(vi_dir, "/", vi, "/", file_out)

  if (file.exists(out_path_name)) {
    stop(paste0("Error: file exists. Quitting. File: ", out_path_name))
    quit("no")
  }
  
  cube <- sds(filename)
  
  # Import land mask and make sure extent matches the cube
  land_mask      <- rast(land_mask)
  orig_extent    <- ext(cube)
  ext(land_mask) <- orig_extent
  
  if (vi == "EVI") {
    index      <- calc_evi(cube[[1]], cube[[2]], cube[[3]])
  } else if (vi == "NDVI") {
    index      <- calc_ndvi(cube[[1]], cube[[2]])
  } else if (vi == "NIRv") {
    index      <- calc_nirv(cube[[1]], cube[[2]])
  } else if (vi == "LSWI") {
    index      <- calc_lswi(cube[[2]], cube[[6]])
  } else if (vi == "RED") {
    index      <- calc_red(cube[[1]])
  } else if (vi == "NIR") {
    index      <- calc_nir(cube[[2]])
  } else {
    stop(paste0(vi, " is not allowed. Must be EVI, NDVI, NIRv, LSWI, RED, or NIR. Quiting."))
    quit("no")
  }
  index      <- mask_all(index, cube, qc_filter, snow_filter, land_mask)
  ext(index) <- c(-180, 180, -90, 90)
  index <- project(index, "+proj=longlat +datum=WGS84")
  gc()
  writeRaster(index, out_path_name, overwrite = TRUE, datatype = 'INT4S', NAflag = -9999)
  
}

######## FOR SLURM ##########

# Run the job
sjob <- slurm_apply(save_vis, pars, vi_dir = vi_dir, qc_filter = qc_filter, 
                    snow_filter = snow_filter, land_mask = land_mask,
                    jobname = 'calc_VIs', submit = TRUE, nodes = 1, cpus_per_node = 10,
                    preschedule_cores = FALSE,
                    slurm_options = list(partition = "normal", time = "6:00:00"))

# get_job_status(sjob)[2]
# cleanup_files(sjob)