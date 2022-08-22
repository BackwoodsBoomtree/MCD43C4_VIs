library(terra)
library(rslurm)

# terraOptions(memfrac = 0.8) # Fraction of memory to allow terra

#### Input variables ####

hdf_input          <- "/ourdisk/hpc/geocarb/data_share/MCD43C4/v061/original"
vi_list            <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
vi_dir             <- "/ourdisk/hpc/geocarb/data_share/MCD43C4/v061/tif/daily/0.05"
# tmp_dir            <- "/ourdisk/hpc/geocarb/boomtree/tmp"
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
  # tmp_create   <- function(tmp_dir) {
    
  #   p_tmp_dir <- paste0(tmp_dir, "/", as.character(Sys.getpid())) # Process ID
    
  #   if (!dir.exists(p_tmp_dir)) {
  #     dir.create(p_tmp_dir, recursive = TRUE)
  #   }
    
  #   terraOptions(tempdir = p_tmp_dir)
  # }
  # tmp_remove   <- function(tmp_dir) {
    
  #   p_tmp_dir <- paste0(tmp_dir, "/", as.character(Sys.getpid())) # Process ID
  #   unlink(p_tmp_dir, recursive = TRUE)
  # }


  # start <- Sys.time() # Start clock for timing
  file_out      <- substr(basename(filename), 1, nchar(basename(filename)) - 18)
  file_out      <- paste0(file_out, ".", vi, ".tif")
  out_path_name <- paste0(vi_dir, "/", vi, "/", file_out)

  if (file.exists(out_path_name)) {
    stop(paste0("Error: file exists. Quitting. File: ", out_path_name))
    quit("no")
  }
  
  # tmp_create(tmp_dir)
  
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

  # tmp_remove(tmp_dir)
  
  # print(paste0("Done with ", file_out, ". Time difference in minutes: ", round(difftime(Sys.time(), start, units = "mins"), 2)))
  
}
# missing_list <- function(hdf_input, vi_dir){
  
#   hdf_list <- list.files(hdf_input, pattern = "*.hdf$", full.names = FALSE, recursive = TRUE)
  
#   tif_list <- list.files(vi_dir, pattern = "*.tif$", full.names = FALSE, recursive = TRUE)
  
#   missing_files <- c()
#   for (i in hdf_list) {
    
#     file_out <- substr(basename(i), 1, nchar(basename(i)) - 22)
#     pos <- charmatch(file_out, tif_list)
    
#     if (is.na(pos)) {
#       missing_files <- c(missing_files, paste0(hdf_input, "/", i))
#     }
#   }
  
#   tif_list_full <- list.files(vi_dir, pattern = "*.tif$", full.names = TRUE, recursive = TRUE)
#   hdf_list_full <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
  
#   for (i in tif_list_full) {
#     if (file.info(i)$size == 0) {
      
#       file_out <- substr(basename(i), 1, nchar(basename(i)) - 13)
#       pos      <- charmatch(file_out, hdf_list)
      
#       missing_files <- c(missing_files, hdf_list_full[pos])
#     }
#   }
#   return(missing_files)
# }

######## FOR Running locally ##########

# # hdf_list <- missing_list(hdf_input, "/mnt/g/MCD43C4/tif/Daily/0.05/NIRv")
# hdf_list <- list.files(hdf_input, pattern = "*.hdf$", full.names = TRUE, recursive = TRUE)
# 
# # Must use mc.preschedule = F
# for (vi in vi_list) {
#   mclapply(hdf_list, save_vis, mc.cores = 6, mc.preschedule = FALSE, vi_dir = vi_dir, vi = vi)
# }
# 
# # Delete tempdir
# unlink(tmp_dir, recursive = TRUE)

######## FOR SLURM ##########

# Run the job
sjob <- slurm_apply(save_vis, pars, vi_dir = vi_dir, qc_filter = qc_filter, 
                    snow_filter = snow_filter, land_mask = land_mask,
                    jobname = 'calc_VIs', submit = TRUE, nodes = 1, cpus_per_node = 1,
                    slurm_options = list(partition = "geocarb_plus"))

# get_job_status(sjob)[2]
# cleanup_files(sjob)