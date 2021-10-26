
library(terra)
library(parallel)

terraOptions(memfrac = 0.8) # Fraction of memory to allow terra

tmpdir       <- "/mnt/c/Rwork"
daily_vi_dir <- "/mnt/g/MCD43C4/tif/Daily/0.05"
output_dir   <- "/mnt/g/MCD43C4/tif/8-day/0.05"
vi_list      <- c("EVI", "NDVI", "NIRv", "LSWI")

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
check_leap <- function(year) {
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
to_8day    <- function(vi, in_dir, out_dir, tmpdir) {

  tmp_create(tmpdir)
  
  sub_dir_list <- list.dirs(paste0(in_dir, "/", vi), recursive = FALSE)
  print(paste0("Starting ", vi, ". Input dir is ", in_dir, "/", vi))

  for (s in 1:length(sub_dir_list)) {

    start <- Sys.time() # Start clock for timing
    
    year <- as.numeric(basename(sub_dir_list[s]))
    print(paste0("Starting from year ", year, ". Start time is ", start))
    
    output_dir <- paste0(out_dir, "/", vi, "/", year)
    print(paste0("Output dir is: ", output_dir))
    
    if (!dir.exists(output_dir)) { # Create output dirs for each year
      dir.create(output_dir, recursive = TRUE)
    }

    sub_dir_files <- list.files(paste0(in_dir, "/", vi, "/", year), full.names = TRUE, pattern = "*.tif$")
    
    if (year != 2000 && year != 2001) {

      for (h in seq(1, length(sub_dir_files), 8)) {
  
        print(paste0("Starting 8-day mean with DOY ", h, " for ", year))
        
        file_list_8day <- sub_dir_files[h : (h + 7)]
        file_list_8day <- file_list_8day[!is.na(file_list_8day)]
  
        if (h != 361 && length(file_list_8day) == 8) {
  
          vi_stack <- rast(file_list_8day)
          flag     <- TRUE
  
        } else if (h == 361 && (length(sub_dir_files) == 365 || length(sub_dir_files == 366))) {
          
          vi_stack <- rast(file_list_8day)
          flag     <- TRUE
          
        } else if (h != 361 && length(file_list_8day) != 8) {
          
          print(paste0("Skipping DOY ", h, " for year ", year, " due to insufficient number of files."))
          flag     <- FALSE
        }
        
        # Mean up the rasters and get output file name
        if (flag == TRUE) {
          
          out_name  <- substr(basename(sub_dir_files[h]), 1, 16) # Get first 16 characters of filename
          out_name  <- paste0(output_dir, "/", out_name, ".", vi, ".8day.tif")
          
          vi_out   <- mean(vi_stack, na.rm = TRUE)
  
          writeRaster(vi_out, filename = out_name, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
          print(paste0("Saved file to: ", out_name))
        }
      }
    } else if (year == 2000) {
       
      # Only 2 files for the first 8 days, so dump them
      sub_dir_files <- tail(sub_dir_files, -2)
      
      doy_index <- seq(57, 365, 8)
      
      for (h in 1:length(doy_index)) {
        
        print(paste0("Starting 8-day mean with DOY ", doy_index[h], " for ", year))
        
        file_list_8day <- sub_dir_files[h : (h + 7)]
        file_list_8day <- file_list_8day[!is.na(file_list_8day)]
        
        vi_stack <- rast(file_list_8day)
        
        out_name  <- substr(basename(sub_dir_files[h]), 1, 16) # Get first 16 characters of filename
        out_name  <- paste0(output_dir, "/", out_name, ".", vi, ".8day.tif")
        
        vi_out   <- mean(vi_stack, na.rm = TRUE)
        
        writeRaster(vi_out, filename = out_name, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
        print(paste0("Saved file to: ", out_name))
      }
    } else if (year == 2001) {
      
      for (h in seq(1, 168, 8)) {
        
        print(paste0("Starting 8-day mean with DOY ", h, " for ", year))
        
        file_list_8day <- sub_dir_files[h : (h + 7)]
        file_list_8day <- file_list_8day[!is.na(file_list_8day)]
        
        vi_stack <- rast(file_list_8day)
        
        out_name  <- substr(basename(sub_dir_files[h]), 1, 16) # Get first 16 characters of filename
        out_name  <- paste0(output_dir, "/", out_name, ".", vi, ".8day.tif")
        
        vi_out   <- mean(vi_stack, na.rm = TRUE)
        
        writeRaster(vi_out, filename = out_name, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
        print(paste0("Saved file to: ", out_name))
      }
      
      # Write raster for 8-day mean that is missing 2 files
      print("Starting 8-day mean with DOY 73 for year 2001, for which MODIS has 2 missing days.")
      
      file_list_8day <- sub_dir_files[173 : 178]
      
      vi_stack <- rast(file_list_8day)
      
      out_name  <- substr(basename(sub_dir_files[h]), 1, 16) # Get first 16 characters of filename
      out_name  <- paste0(output_dir, "/", out_name, ".", vi, ".8day.tif")
      
      vi_out   <- mean(vi_stack, na.rm = TRUE)
      
      writeRaster(vi_out, filename = out_name, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
      print(paste0("Saved file to: ", out_name))
      
      # Write the rest of the files
      for (h in seq(177, 365, 8)) {
        
        print(paste0("Starting 8-day mean with DOY ", h, " for ", year))
        
        file_list_8day <- sub_dir_files[(h - 2) : (h - 2 + 7)]
        file_list_8day <- file_list_8day[!is.na(file_list_8day)]
        
        vi_stack <- rast(file_list_8day)
        
        out_name  <- substr(basename(sub_dir_files[(h - 2)]), 1, 16) # Get first 16 characters of filename
        out_name  <- paste0(output_dir, "/", out_name, ".", vi, ".8day.tif")
        
        vi_out   <- mean(vi_stack, na.rm = TRUE)
        
        writeRaster(vi_out, filename = out_name, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
        print(paste0("Saved file to: ", out_name))
      }
      
    }
      
  }
  tmp_remove(tmpdir)
}
to_month   <- function (vi, in_dir, out_dir, tmpdir) {

  tmp_create(tmpdir)
  
  sub_dir_list <- list.dirs(paste0(in_dir, "/", vi), recursive = FALSE)

  for (s in 1:length(sub_dir_list)) {
    
    start <- Sys.time() # Start clock for timing

    year <- as.numeric(basename(sub_dir_list[s]))
    print(paste0("Starting from ", in_dir, "/", vi, "/", year, ". Start time is ", start))
    
    output_dir <- paste0(out_dir, "/", vi, "/", year)
    print(paste0("Output dir is: ", output_dir))

    if (!dir.exists(output_dir)) { # Create output dir
      dir.create(output_dir, recursive = TRUE)
    }

    sub_dir_files <- list.files(sub_dir_list[s], full.names = TRUE, pattern = "*.tif$")

    leap <- check_leap(year)

    print(paste0("Year ", year, " is a leap year: ", leap))

    doy <- 1

    for (m in 1:12) {

      if (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) {

        month_file_list <- sub_dir_files[doy : (doy + 30)]
        month_file_list <- month_file_list[!is.na(month_file_list)]

        if (length(month_file_list) == 31) {

          print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 30))
          vi_stack <- rast(month_file_list)
          doy <- doy + 31
          flag <- TRUE # write out the file?

        } else {

          print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
          flag <- FALSE # write out the file?

        }

      } else if (m == 4 || m == 6 || m == 9 || m == 11) {

        month_file_list <- sub_dir_files[doy : (doy + 29)]
        month_file_list <- month_file_list[!is.na(month_file_list)]

        if (length(month_file_list) == 30) {

          print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 29))
          vi_stack <- rast(month_file_list)
          doy <- doy + 30
          flag <- TRUE # write out the file?

        } else {

          print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
          flag <- FALSE # write out the file?

        }

      } else if (m == 2) {

        if (leap == FALSE) {

          month_file_list <- sub_dir_files[doy : (doy + 27)]
          month_file_list <- month_file_list[!is.na(month_file_list)]

          if (length(month_file_list) == 28) {

            print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 27))
            print("Not a leap year! Feb has 28 days.")
            vi_stack <- rast(month_file_list)
            doy <- doy + 28
            flag <- TRUE # write out the file?

          } else {

            print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
            flag <- FALSE # write out the file?
          }

        } else if (leap == TRUE) {

          month_file_list <- sub_dir_files[doy : (doy + 28)]
          month_file_list <- month_file_list[!is.na(month_file_list)]

          if (length(month_file_list) == 29) {

            print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 28))
            print("Hello leap year! Feb has 29 days.")
            vi_stack <- rast(month_file_list)
            doy <- doy + 29
            flag <- TRUE # write out the file?

          } else {

            print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
            flag <- FALSE # write out the file?
          }
        }
      }
      
      if (flag == TRUE) {
        
        month_num <- sprintf("%02.0f", m)
        out_name  <- substr(basename(sub_dir_files[1]), 1, 13) # Get first 13 characters of filename
        out_name  <- paste0(output_dir, "/", out_name, ".", month_num, ".", vi, ".Monthly.tif")
        
        vi_out <- mean(vi_stack, na.rm = TRUE)
        
        writeRaster(vi_out, filename = out_name, overwrite = TRUE, NAflag = -9999, datatype = 'INT4S')
        
        print(paste0("Saved file to: ", out_name))
      }
    }
    
    print(paste0("Done with ", in_dir, "/", vi, "/", year, ". Time difference in minutes: ", round(difftime(Sys.time(), start, units = "mins"), 2)))
  }
  
  tmp_remove(tmpdir)
}

# Dedicate each VI instance to a core
# mclapply(vi_list, to_month, mc.cores = 4, in_dir = daily_vi_dir, out_dir = output_dir, tmpdir = tmpdir)
mclapply(vi_list, to_8day, mc.cores = 4, mc.preschedule = FALSE, in_dir = daily_vi_dir, out_dir = output_dir, tmpdir = tmpdir)