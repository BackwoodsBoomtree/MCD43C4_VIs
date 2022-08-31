library(terra)
library(parallel)
library(ncdf4)

tif_input    <- "/mnt/g/MCD43C4/v061/tif/daily/0.05"
nc_output    <- "/mnt/g/MCD43C4/v061/nc/daily/0.05"
vi_list      <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
time_res     <- "daily"
spatial_res  <- "0.05"
data_version <- "MCD43C4 v061"

leap      <- function (year) {
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}
get_dates <- function (file_list) {
  for (i in 1:length(file_list)) {
    file_year <- as.numeric(substr(basename(file_list[i]), 10, 13))
    file_doy  <- as.numeric(substr(basename(file_list[i]), 14, 16))
    date      <- as.Date(file_doy - 1, origin = paste0(file_year, "-01-01"))
    if (i == 1) {
      date_list <- date
    } else {
      date_list <- c(date_list, date)
    }
  }
  return(date_list)
}

# Creates nc file for each year
nc_by_vi_month <- function (vi, in_dir, out_dir, t_res, s_res, d_ver) {
  
  year_dirs <- list.dirs(paste0(in_dir, "/", vi), recursive = FALSE)
  
  # Create output dir
  output_dir <- paste(out_dir, vi, sep = "/")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(paste0("Output dir created ", output_dir))
  }

  for (j in 1:length(year_dirs)) {
    
    # Get list of files and dates from those file names
    file_list_year <- list.files(year_dirs[j], full.names = TRUE, pattern = "*.tif$")
    date_list      <- get_dates(file_list_year)
    year           <- as.numeric(basename(year_dirs[j]))
    
    # Create data frame with column for each month and fill with files we will use
    all_dates <- seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by="days")
    df_files <- data.frame(matrix(ncol = 12, nrow = 31))
    
    for (k in 1:12){
      if (k < 10) {
        month <- paste0("0", k)
      } else {
        month <- as.character(k)
      }
      
      sub_dates <- subset(all_dates, format.Date(all_dates, "%m") == month)
      sub_files <- c()
      
      for (l in 1:length(sub_dates)) {
        
        check_file <- file_list_year[grepl(sub_dates[l], date_list)]
        
        if (length(check_file) != 0) {
          sub_files <- c(sub_files, check_file)
        } else {
          sub_files <- c(sub_files, NA)
        }
      }
      
      # Force length to 31
      if (length(sub_files) < 31) {
        sub_files <- sub_files[1:31]
      }
      
      if (k == 1) {
        df_files <- cbind(sub_files)
      } else {
        df_files <- cbind(df_files, sub_files)
      }
    }
    
    for (m in 1:12) {
      
      # Remove NA from file list
      files      <- df_files[,m][!is.na(df_files[,m])]
      file_dates <- get_dates(files)
      file_dates <- as.POSIXlt(file_dates)
      
      # Output name
      out_name <- substr(basename(files[1]), 1, 13)
      out_name <- paste(out_name, sprintf("%02d", m), vi, t_res, s_res, "nc", sep = ".")
      out_name <- paste0(output_dir, "/", out_name)
      
      # Check if file exists already
      if (file.exists(out_name)) {
        message(paste0("Quitting. File exists: ", out_name))
      } else {
        
        message(paste0("Placing files from ", year_dirs[j], " month ", m, " for ", vi, " into .nc file."))
        vi_stack       <- rast(files)
        time(vi_stack) <- file_dates
        
        l_name   <- paste0(t_res, " ", data_version, " ", vi)
        writeCDF(vi_stack, filename = out_name, varname = vi, longname = l_name, 
                 overwrite = TRUE, compression = 4, missval = -9999)
        message(paste0("Saved file to: ", out_name))
      }
    }
  }
}

# Must use mc.preschedule = F
mclapply(vi_list, nc_by_vi_month, mc.cores = 1, mc.preschedule = FALSE, in_dir = tif_input, out_dir = nc_output, 
         t_res = time_res, s_res = spatial_res, d_ver = data_version)