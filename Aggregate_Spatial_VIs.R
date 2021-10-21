
library(terra)
library(parallel)

fine_input    <- "/mnt/g/MCD43C4/tif/Monthly/0.05"
coarse_output <- "/mnt/g/MCD43C4/tif/Monthly/0.20"
vi_list       <- c("EVI", "NDVI", "NIRv", "LSWI")
spatial_res   <- 0.20 # in degrees

agg_tiff <- function (vi, in_dir, out_dir, s_res) {
  
  sub_dir_list <- list.dirs(paste0(in_dir, "/", vi), recursive = FALSE)
  print(paste0("Starting ", vi, ". Input dir is ", in_dir, "/", vi))
  
  for (s in 1:length(sub_dir_list)) {
    
    temp_output_dir <- paste0(out_dir, "/", vi, "/", basename(sub_dir_list[s]))
    print(paste0("Output dir is: ", temp_output_dir))
    
    if (!dir.exists(temp_output_dir)) { # Create output dirs for each year
      dir.create(temp_output_dir, recursive = TRUE)
    }
    
    sub_dir_files <- list.files(sub_dir_list[s], full.names = TRUE, pattern = "*.tif$")
    basename(substr(sub_dir_files[s], 1, nchar(sub_dir_files[s])-4))
    
    for (h in 1:length(sub_dir_files)){
      
      out_name <- basename(substr(sub_dir_files[h], 1, nchar(sub_dir_files[h])-4))
      out_name <- paste0(temp_output_dir, "/", out_name, ".", s_res, ".deg.tif")
      
      vi_out <- rast(sub_dir_files[h])
      vi_out <- aggregate(vi_out, fact = (s_res / 0.05), fun = mean, na.rm = TRUE)
      
      writeRaster(vi_out, out_name, overwrite = TRUE, NAflag = -9999 , datatype = 'INT4S')
      
      print(paste0("Saved file to: ", temp_output_dir, "/", out_name))
    }
  }
}

# Dedicate each VI instance to a core
mclapply(vi_list, agg_tiff, mc.preschedule = TRUE, mc.cores = 4, in_dir = fine_input, out_dir = coarse_output, s_res = spatial_res)