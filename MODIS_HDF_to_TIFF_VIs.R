
library(gdalUtils)
library(tools)
library(raster)
library(ncdf4)
library(filesstrings)

hdf_input        <- "C:/Russell/Temp/MCD43C4/original"
tif_output       <- "G:/Temp/MCD43C4/reprocessed/tif"
vi_output        <- "G:/Temp/MCD43C4/reprocessed/tif/VIs"
temp_agg_output  <- "G:/Temp/MCD43C4/reprocessed/tif/Monthly/VIs"
nc_output        <- "G:/Temp/MCD43C4/reprocessed/nc"
agg_tiff_output  <- "G:/Temp/MCD43C4/reprocessed/tif/1-deg/Monthly/VIs"
band_list        <- c(1, 2, 3, 6)
vi_list          <- c("NDVI", "EVI", "NIRv", "LSWI")
land_mask        <- "C:/Russell/Git/land_mask/Land_Ocean_0.05deg_Clark1866.tif"
tiff_res         <- 1.0
year_list        <- c(2019:2021)


convert_hdf    <- function (in_dir, out_dir, bands) {
  
  file_list <- list.files(in_dir, full.names = TRUE, pattern = "*.hdf$", recursive = TRUE)
  
  print(paste0("I am converting ", length(file_list), " HDF files to TIFF for ", length(band_list), " bands."))
  
  # Create subdirs for each band if !exist
  for (b in band_list) {
    if (!dir.exists(paste0(out_dir, "/b", b))) {
      dir.create(paste0(out_dir, "/b", b), recursive = TRUE)
    }
  }
  
  for (f in 1:length(file_list)) {
    
    print(paste0("I am converting ", file_list[f], "."))
    
    sds <- get_subdatasets(file_list[f]) # Get subdatasets
    file_name <- file_path_sans_ext(basename(file_list[f]))
    
    for (band in band_list) {
      
      dir_band <- paste0(out_dir, "/b", band)
      gdal_translate(sds[band], paste0(dir_band, "/", file_name, "_b", band, ".tif"))
    }
  }
}
calc_VIs       <- function (in_dir, out_dir, vis) {
  
  # Create subdirs for each VI if !exist
  for (vi in vis) {
    if (!dir.exists(paste0(out_dir, "/", vi))) {
      dir.create(paste0(out_dir, "/", vi), recursive = TRUE)
    }
  }
  
  if ("NDVI" %in% vis && "EVI" %in% vis && "NIRv" %in% vis && "LSWI" %in% vis) {
    b1_files <- list.files(paste0(in_dir, "/b1"), full.names = TRUE, pattern = "*.tif$")
    b2_files <- list.files(paste0(in_dir, "/b2"), full.names = TRUE, pattern = "*.tif$")
    b3_files <- list.files(paste0(in_dir, "/b3"), full.names = TRUE, pattern = "*.tif$")
    b6_files <- list.files(paste0(in_dir, "/b6"), full.names = TRUE, pattern = "*.tif$")
    
    for (f in 1:length(b1_files)) {
      
      print(paste0("I am working on file number ", f, " of ", length(b1_files), " files."))
      
      b1   <- raster(b1_files[f])
      b2   <- raster(b2_files[f])
      b3   <- raster(b3_files[f])
      b6   <- raster(b6_files[f])
      b1[b1 < 0] <- NA
      b1[b1 > 1] <- NA
      b2[b2 < 0] <- NA
      b2[b2 > 1] <- NA
      b3[b3 < 0] <- NA
      b3[b3 > 1] <- NA
      b6[b6 < 0] <- NA
      b6[b6 > 1] <- NA
      
      ndvi <- (b2 - b1) / (b2 + b1)
      evi  <- 2.5 * (b2 - b1) / (b2 + (6 * b1) - (7.5 * b3) + 1)
      nirv <- ndvi * b2
      lswi <- (b2 - b6) / (b2 + b6)
      
      file_out <- substr(file_path_sans_ext(basename(b1_files[f])), 1, nchar(file_path_sans_ext(basename(b1_files[f]))) - 17)
      
      writeRaster(ndvi, paste0(out_dir, "/NDVI/", file_out, "_NDVI.tif"), overwrite = TRUE)
      writeRaster(evi, paste0(out_dir, "/EVI/", file_out, "_EVI.tif"), overwrite = TRUE)
      writeRaster(nirv, paste0(out_dir, "/NIRv/", file_out, "_NIRv.tif"), overwrite = TRUE)
      writeRaster(lswi, paste0(out_dir, "/LSWI/", file_out, "_LSWI.tif"), overwrite = TRUE)
    }
  }
  
  # Starting code for checking list for VIs and making them
  # if ("NDVI" %in% vi_list) {
  #   b1_files <- list.files(paste0(in_dir, "/b1"), full.names = TRUE, pattern = "*.tif$")
  #   b2_files <- list.files(paste0(in_dir, "/b2"), full.names = TRUE, pattern = "*.tif$")
  #   
  #   for (f in 1:2) {
  #     b1   <- raster(b1_files[f])
  #     b2   <- raster(b2_files[f])
  #     b1[b1 < 0] <- NA
  #     b1[b1 > 1] <- NA
  #     b2[b2 < 0] <- NA
  #     b2[b2 > 1] <- NA
  #     ndvi <- (b2 - b1) / (b2 + b1)
  #     
  #     file_out <- substr(file_path_sans_ext(basename(b1_files[f])), 1, nchar(file_path_sans_ext(basename(b1_files[f]))) - 17)
  #     writeRaster(ndvi, paste0(out_dir, "/NDVI/", file_out, "_NDVI.tif"), overwrite = TRUE)
  #   }
  # } else if ("EVI" %in% vi_list) {
  #   b1_files <- list.files(paste0(in_dir, "/b1"), full.names = TRUE, pattern = "*.tif$")
  #   b2_files <- list.files(paste0(in_dir, "/b2"), full.names = TRUE, pattern = "*.tif$")
  #   b3_files <- list.files(paste0(in_dir, "/b3"), full.names = TRUE, pattern = "*.tif$")
  #   
  #   for (f in 1:2) {
  #     b1  <- raster(b1_files[f])
  #     b2  <- raster(b2_files[f])
  #     b3  <- raster(b3_files[f])
  #     b1[b1 < 0] <- NA
  #     b1[b1 > 1] <- NA
  #     b2[b2 < 0] <- NA
  #     b2[b2 > 1] <- NA
  #     b3[b3 < 0] <- NA
  #     b3[b3 > 1] <- NA
  #     evi <- 2.5 * (b2 - b1) / (b2 + (6 * b1) - (7.5 * b3) + 1)
  #     
  #     file_out <- substr(file_path_sans_ext(basename(b1_files[f])), 1, nchar(file_path_sans_ext(basename(b1_files[f]))) - 17)
  #     writeRaster(evi, paste0(out_dir, "/EVI/", file_out, "_EVI.tif", overwrite = TRUE))
  #   } 
  # } else if ("NIRv" %in% vi_list) {
  #   b1_files <- list.files(paste0(in_dir, "/b1"), full.names = TRUE, pattern = "*.tif$")
  #   b2_files <- list.files(paste0(in_dir, "/b2"), full.names = TRUE, pattern = "*.tif$")
  #   b3_files <- list.files(paste0(in_dir, "/b3"), full.names = TRUE, pattern = "*.tif$")
  #   
  #   for (f in 1:2) {
  #     b1   <- raster(b1_files[f])
  #     b2   <- raster(b2_files[f])
  #     b1[b1 < 0] <- NA
  #     b1[b1 > 1] <- NA
  #     b2[b2 < 0] <- NA
  #     b2[b2 > 1] <- NA
  #     nirv <- ((b2 - b1) / (b2 + b1)) * b2
  #     
  #     file_out <- substr(file_path_sans_ext(basename(b1_files[f])), 1, nchar(file_path_sans_ext(basename(b1_files[f]))) - 17)
  #     writeRaster(nirv, paste0(out_dir, "/NIRv/", file_out, "_NIRv.tif", overwrite = TRUE))
  #   }
  # } else if ("LSWI" %in% vi_list) {
  #   b2_files <- list.files(paste0(in_dir, "/b2"), full.names = TRUE, pattern = "*.tif$")
  #   b6_files <- list.files(paste0(in_dir, "/b6"), full.names = TRUE, pattern = "*.tif$")
  #   
  #   for (f in 1:2) {
  #     b2   <- raster(b2_files[f])
  #     b6   <- raster(b6_files[f])
  #     b2[b2 < 0] <- NA
  #     b2[b2 > 1] <- NA
  #     b6[b6 < 0] <- NA
  #     b6[b6 > 1] <- NA
  #     lswi <- (b2 - b6) / (b2 + b6)
  #     
  #     file_out <- substr(file_path_sans_ext(basename(b1_files[f])), 1, nchar(file_path_sans_ext(basename(b1_files[f]))) - 17)
  #     writeRaster(lswi, paste0(out_dir, "/LSWI/", file_out, "_LSWI.tif", overwrite = TRUE))
  #   }
  # }
}
annual_folders <- function (in_dir, vis, years) {
  
  for (vi in 1:length(vis)) {
    
    vi_file_list <- list.files(paste0(in_dir, "/", vis[vi]), full.names = TRUE, pattern = "*.tif$", recursive = TRUE)

    for (y in 1:length(years)) {
      
      year_dir <- paste0(in_dir, "/", vis[vi], "/", years[y])

      if (!dir.exists(year_dir)) { # Create output dirs for each year
        dir.create(year_dir, recursive = TRUE)
      }
      
      year_file_list <- vi_file_list[grep(paste0("A", years[y]), vi_file_list)] # Year is preceded by A in the file names
      
      for (f in 1:length(year_file_list)) {
        file.move(year_file_list[f], year_dir)
      }
    }
  }
}
check_leap     <- function (year) {
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
to_8day        <- function (in_dir, out_dir, vis) {
  
  # Empty raster for template
  template_raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, ncols = 7200, nrows = 3600, crs = "+proj=longlat +datum=WGS84")
  
  for (i in 1:length(vis)) {
    
    sub_dir_list <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
    print(paste0("Starting ", vis[i], ". Input dir is ", in_dir, "/", vis[i]))
    
    for (s in 1:length(sub_dir_list)) {
      
      temp_output_dir <- paste0(out_dir, "/", vis[i], "/", basename(sub_dir_list[s]))
      print(paste0("Output dir is: ", temp_output_dir))
      
      if (!dir.exists(temp_output_dir)) { # Create output dirs for each year
        dir.create(temp_output_dir, recursive = TRUE)
      }
      
      sub_dir_files <- list.files(sub_dir_list[s], full.names = TRUE, pattern = "*.tif$")
      
      for (h in seq(1, length(sub_dir_files), 8)){
        
        print(paste0("Running 8-day mean starting with DOY ", h))
        
        if (h != 361) { 
          
          vi_stack <- stack(sub_dir_files[h:h + 7])
          
          # Set extent of MCD rasters
          ext <- extent(-180, 180, -90, 90)
          extent(vi_stack) <- ext
          vi_stack <- setExtent(vi_stack, ext)
          
          # Reproject
          vi_stack <- projectRaster(vi_stack, template_raster)
          
          # Get rid of negative values (LSWI can be negative)
          if (vis[i] != "LSWI") {
            # Valid values are >= 0, so set all negative to NA (right=F will not set 0 to NA)
            m <- matrix(c(-Inf, 0, NA), ncol = 3, byrow = T)
            vi_stack <- reclassify(vi_stack, m, right = F)
            print("Got rid of negative values; VI is not LSWI")
          } else {
            print("Kept all values; VI is LSWI")
          }
          
          vi_out <- mean(vi_stack, na.rm = T)
          
          out_name <- file_path_sans_ext(basename(sub_dir_files[h]))
          writeRaster(vi_out, paste0(temp_output_dir, "/", out_name, "_8-day.tif"), overwrite = TRUE, NAflag = -9999)
          
        } else {
          
          remaining_files <- length(sub_dir_files) - h
          vi_stack <- stack(sub_dir_files[h:h + remaining_files])
          
          # Set extent of MCD rasters
          ext <- extent(-180, 180, -90, 90)
          extent(vi_stack) <- ext
          vi_stack <- setExtent(vi_stack, ext)
          
          # Reproject
          vi_stack <- projectRaster(vi_stack, template_raster)
          
          # Valid values are >= 0, so set all negative to NA (right=F will not set 0 to NA)
          m <- matrix(c(-Inf, 0, NA), ncol = 3, byrow = T)
          vi_stack <- reclassify(vi_stack, m, right = F)
          
          vi_out <- mean(vi_stack, na.rm = T)
          
          out_name <- file_path_sans_ext(basename(sub_dir_files[h]))
          writeRaster(vi_out, paste0(temp_output_dir, "/", out_name, "_8-day.tif"), overwrite = TRUE, NAflag = -9999)
        }
        print(paste0("Saved file to: ", temp_output_dir, "/", out_name, "_8-day.tif"))
      }
    }
  }
}
to_month       <- function (in_dir, out_dir, vis, mask) {
  
  # Empty raster for template
  template_raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, ncols = 7200, nrows = 3600, crs = "+proj=longlat +datum=WGS84")
  land_mask       <- raster(mask)
  
  # Set extent of mask
  ext <- extent(-180, 180, -90, 90)
  extent(land_mask) <- ext
  land_mask <- setExtent(land_mask, ext)
  
  # Reproject MCD rasters and mask
  land_mask <- projectRaster(land_mask, template_raster)
  
  for (i in 1:length(vis)) {
    
    sub_dir_list <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
    print(paste0("Starting ", vis[i], ". Input dir is ", in_dir, "/", vis[i]))
    
    for (s in 1:length(sub_dir_list)) {
      
      year <- as.numeric(basename(sub_dir_list[s]))
      
      temp_output_dir <- paste0(out_dir, "/", vis[i], "/", year)
      print(paste0("Output dir is: ", temp_output_dir))
      
      if (!dir.exists(temp_output_dir)) { # Create output dirs for each year
        dir.create(temp_output_dir, recursive = TRUE)
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
            vi_stack <- stack(month_file_list)
            doy <- doy + 31
            
          } else {
            
            print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
            
          }
          
        } else if (m == 4 || m == 6 || m == 9 || m == 11) {
          
          month_file_list <- sub_dir_files[doy : (doy + 29)]
          month_file_list <- month_file_list[!is.na(month_file_list)]
          
          if (length(month_file_list) == 30) {
          
            print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 29))
            vi_stack <- stack(month_file_list)
            doy <- doy + 30
            
          } else {
            
            print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
            
          }
          
        } else if (m == 2) {
          
          if (leap == FALSE) {
            
            month_file_list <- sub_dir_files[doy : (doy + 27)]
            month_file_list <- month_file_list[!is.na(month_file_list)]
            
            if (length(month_file_list) == 28) {
            
              print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 27))
              print("Not a leap year! Feb has 28 days.")
              vi_stack <- stack(month_file_list)
              doy <- doy + 28
              
            } else {
              
              print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
              
            }
            
          } else if (leap == TRUE) {
            
            month_file_list <- sub_dir_files[doy : (doy + 28)]
            month_file_list <- month_file_list[!is.na(month_file_list)]
            
            if (length(month_file_list) == 29) {
            
              print(paste0("Month is ", m, " and the files are for DOYs ", doy, " to ", doy + 28))
              print("Hello leap year! Feb has 29 days.")
              vi_stack <- stack(month_file_list)
              doy <- doy + 29
              
            } else {
              
              print(paste0("Skipping month ", m, " for year ", year, " due to insufficient number of files, where n = ", length(month_file_list)))
              
            }
          }
        }
        
        # Set extent of MCD rasters and mask
        extent(vi_stack)  <- ext
        vi_stack  <- setExtent(vi_stack, ext)
        
        # Reproject MCD rasters and mask
        vi_stack  <- projectRaster(vi_stack, template_raster)
        
        # Get rid of negative values (LSWI can be negative)
        if (vis[i] != "LSWI") {
          # Valid values are >= 0, so set all negative to NA (right = F will not set 0 to NA)
          mat <- matrix(c(-Inf, 0, NA), ncol = 3, byrow = T)
          vi_stack <- reclassify(vi_stack, mat, right = F)
          print("Got rid of negative values; VI is not LSWI")
        } else {
          print("Kept all values; VI is LSWI")
        }
        
        vi_out <- mean(vi_stack, na.rm = T) # Monthly mean
        vi_out <- mask(vi_out, land_mask, maskvalue = 0) # Mask by land
        
        month_num <- sprintf("%02.0f", m)
        out_name  <- substr(basename(sub_dir_files[1]), 1, 13) # Get first 13 characters of filename
        print(paste0("Saving file to: ", temp_output_dir, "/", out_name, month_num, "_", vis[i], "_Monthly.tif"))
        writeRaster(vi_out, paste0(temp_output_dir, "/", out_name, month_num, "_", vis[i], "_Monthly.tif"), overwrite = TRUE, NAflag = -9999)
      }
    }
  }
}
aggregate_0.20 <- function (in_dir, out_dir) {
  
  if (!dir.exists(out_dir)) { # Create output dirs for each year
    dir.create(out_dir, recursive = TRUE)
  }
  
  file_list <- list.files(in_dir, full.names = TRUE, pattern = "*.nc$", recursive = TRUE)
  
  for (f in 1:length(file_list)) {
    
    out_name <- file_path_sans_ext(basename(file_list[f]))
    out_name <- paste0(out_name, "_1.0-deg.nc")
    
    vi_stack <- stack(file_list[f])
    vi_stack <- aggregate(vi_stack, fact = 20, fun = mean, na.rm = TRUE)
    
    out_var <- file_path_sans_ext(basename(file_list[f]))
    out_var <- substr(out_var, 19, nchar(out_var) - 6)
    
    writeRaster(vi_stack, paste0(out_dir, "/", out_name), overwrite = TRUE, format = "CDF", NAflag = -9999, datatype = 'FLT4S', compression = 4, varname = out_var, 
                longname = paste0("8-day MCD43C4 ", out_var), xname = "Longitude",  yname = "Latitude", zname = "Time (DOY)", zunit = "8-days")
  }
}
aggregate_tiff <- function (in_dir, out_dir, res, vis) {
  
  for (i in 1:length(vis)) {
    
    sub_dir_list <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
    print(paste0("Starting ", vis[i], ". Input dir is ", in_dir, "/", vis[i]))
    
    for (s in 1:length(sub_dir_list)) {
      
      temp_output_dir <- paste0(out_dir, "/", vis[i], "/", basename(sub_dir_list[s]))
      print(paste0("Output dir is: ", temp_output_dir))
      
      if (!dir.exists(temp_output_dir)) { # Create output dirs for each year
        dir.create(temp_output_dir, recursive = TRUE)
      }
      
      sub_dir_files <- list.files(sub_dir_list[s], full.names = TRUE, pattern = "*.tif$")
      
      for (h in 1:length(sub_dir_files)){
        
        out_name <- file_path_sans_ext(basename(sub_dir_files[h]))
        out_name <- paste0(out_name, "_", res, "-deg.tif")
        
        vi <- raster(sub_dir_files[h])
        vi <- aggregate(vi, fact = (res / 0.05), fun = mean, na.rm = TRUE)
        
        writeRaster(vi, paste0(temp_output_dir, "/", out_name), overwrite = TRUE)
        
        print(paste0("Saved file to: ", temp_output_dir, "/", out_name))
      }
    }
  }
}
nc_by_vi_year  <- function (in_dir, out_dir, res, vis) {
  
  for (i in 1:length(vis)) {
    
    sub_dir_list <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
    
    for (s in 1:length(sub_dir_list)) {
      
      print(paste0("Starting to put ", vis[i], " files into .nc from the year ", basename(sub_dir_list[s])))
      sub_dir_files <- list.files(sub_dir_list[s], full.names = TRUE, pattern = "*.tif$")
      
      temp_output_dir <- paste0(out_dir, "/", res, "/", vis[i])
      print(paste0("Output dir is: ", temp_output_dir))
      
      if (!dir.exists(temp_output_dir)) { # Create output dirs for each year
        dir.create(temp_output_dir, recursive = TRUE)
      }
      
      vi_stack <- stack(sub_dir_files)
      
      # For some reason, some large values in a few pixels for EVI; kick them out
      if (vis[i] != "LSWI") {
        # old way
        # m_pos <- matrix(c(1, Inf, NA), ncol = 3, byrow = T)
        # vi_stack <- reclassify(vi_stack, m_pos)
        
        vi_stack <- clamp(vi_stack, lower = 0, upper = 1, useValues = FALSE) # F sets values outside threshold to NA
        print("Got rid of values > 1; VI is not LSWI")
      }
      
      vi_stack <- round(vi_stack, digits = 4)
      
      out_name <- substr(basename(sub_dir_files[1]), 1, 13)
      out_name <- paste0(out_name, ".006_", vis[i], "_Monthly.nc")
      writeRaster(vi_stack, paste0(temp_output_dir, "/", out_name), overwrite = TRUE, format = "CDF", NAflag = -9999, datatype = 'FLT4S', compression = 4, varname = vis[i], 
                  longname = paste0("Monthly MCD43C4 v006 ", vis[i]), xname = "Longitude",  yname = "Latitude", zname = "Time", zunit = "Month")
      print(paste0("Saved file to: ", temp_output_dir, "/", out_name))
    }
  }
}

# convert_hdf(hdf_input, tif_output, band_list)

# calc_VIs(tif_output, vi_output, vi_list)

# annual_folders(vi_output, vi_list, year_list)

# to_8day(vi_output, temp_agg_output, vi_list)

to_month(vi_output, temp_agg_output, vi_list, land_mask)

# aggregate_0.20(nc_output, agg_output)

# aggregate_tiff(temp_agg_output, agg_tiff_output, tiff_res, vi_list)

# nc_by_vi_year(agg_tiff_output, nc_output, tiff_res, vi_list)
