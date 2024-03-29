
library(terra)
library(parallel)

tif_input    <- "/mnt/g/MCD43C4/tif/monthly/0.05"
nc_output    <- "/mnt/g/MCD43C4/nc_annual/monthly/0.05"
vi_list      <- c("NIRv", "RED", "NIR")
time_res     <- "Monthly"
spatial_res  <- "0.05"

# Creates nc file for each year
nc_by_vi_year <- function (in_dir, out_dir, vis, t_res, s_res) {
  
  for (i in 1:length(vis)) {
  
    year_dirs <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
  
    for (j in 1:length(year_dirs)) {
      
      # Create output dir
      output_dir <- paste(out_dir, vis[i], sep = "/")
      
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
  
      print(paste0("Placing files from ", year_dirs[j], " into .nc file."))
      print(paste0("Output dir is: ", output_dir))
      
      file_list <- list.files(year_dirs[j], full.names = TRUE, pattern = "*.tif$")
  
      vi_stack <- rast(file_list)
  
      out_name <- substr(basename(file_list[1]), 1, 13)
      out_name <- paste(out_name, vis[i], t_res, s_res, "nc", sep = ".")
      out_name <- paste0(output_dir, "/", out_name)
      l_name   <- paste0(t_res, " MCD43C4 v006 ", vis[i])
      writeCDF(vi_stack, filename = out_name, varname = vis[i], longname = l_name, overwrite = TRUE, compression = 4, missval = -9999)
      print(paste0("Saved file to: ", out_name))
    }
  }
}


nc_by_vi_year(tif_input, nc_output, vi_list, time_res, spatial_res)