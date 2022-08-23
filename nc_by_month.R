library(terra)
library(parallel)
library(ncdf4)
library(lubridate)

tif_input    <- "/mnt/g/MCD43C4/v061/tif/daily/0.05"
nc_output    <- "/mnt/g/MCD43C4/v061/nc/daily/0.05"
vi_list      <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
time_res     <- "daily"
spatial_res  <- "0.05"
data_version <- "MCD43C4 v061"

# Creates nc file for each year
nc_by_vi_month <- function (in_dir, out_dir, vis, t_res, s_res) {
  
  for (i in 1:length(vis)) {
  
    year_dirs <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
    
    # Create output dir
    output_dir <- paste(out_dir, vis[i], sep = "/")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      message(paste0("Output dir created ", output_dir))
    }
  
    for (j in 1:length(year_dirs)) {
      
      file_list_year <- list.files(year_dirs[j], full.names = TRUE, pattern = "*.tif$")
      
      if (leap_year(basename(year_dirs[j])) == TRUE) {
        month_list_df <- file_list_year[1:31]
        month_list_df <- cbind(month_list_df, c(file_list_year[32:60], NA, NA))
        month_list_df <- cbind(month_list_df, file_list_year[61:91])
        month_list_df <- cbind(month_list_df, c(file_list_year[92:121], NA))
        month_list_df <- cbind(month_list_df, file_list_year[122:152])
        month_list_df <- cbind(month_list_df, c(file_list_year[153:182], NA))
        month_list_df <- cbind(month_list_df, file_list_year[183:213])
        month_list_df <- cbind(month_list_df, file_list_year[214:244])
        month_list_df <- cbind(month_list_df, c(file_list_year[245:274], NA))
        month_list_df <- cbind(month_list_df, file_list_year[275:305])
        month_list_df <- cbind(month_list_df, c(file_list_year[306:335], NA))
        month_list_df <- cbind(month_list_df, file_list_year[336:366])
      } else {
        month_list_df <- file_list_year[1:31]
        month_list_df <- cbind(month_list_df, c(file_list_year[32:59], NA, NA, NA))
        month_list_df <- cbind(month_list_df, file_list_year[60:90])
        month_list_df <- cbind(month_list_df, c(file_list_year[91:120], NA))
        month_list_df <- cbind(month_list_df, file_list_year[121:151])
        month_list_df <- cbind(month_list_df, c(file_list_year[152:181], NA))
        month_list_df <- cbind(month_list_df, file_list_year[182:212])
        month_list_df <- cbind(month_list_df, file_list_year[213:243])
        month_list_df <- cbind(month_list_df, c(file_list_year[244:273], NA))
        month_list_df <- cbind(month_list_df, file_list_year[274:304])
        month_list_df <- cbind(month_list_df, c(file_list_year[305:334], NA))
        month_list_df <- cbind(month_list_df, file_list_year[335:365])
      }
      
      for (m in 1:12) {
        message(paste0("Placing files from ", year_dirs[j], " month ", m, " into .nc file."))
        
        # Remove NA from file list
        files    <- month_list_df[,m][!is.na(month_list_df[,m])]
        vi_stack <- rast(files)
        
        out_name <- substr(basename(files[1]), 1, 13)
        out_name <- paste(out_name, sprintf("%02d", m), vis[i], t_res, s_res, "nc", sep = ".")
        out_name <- paste0(output_dir, "/", out_name)
        l_name   <- paste0(t_res, " ", data_version, " ", vis[i])
        writeCDF(vi_stack, filename = out_name, varname = vis[i], longname = l_name, 
                 overwrite = TRUE, compression = 4, missval = -9999)
        message(paste0("Saved file to: ", out_name))
      }
    }
  }
}


nc_by_vi_month(tif_input, nc_output, vi_list, time_res, spatial_res)