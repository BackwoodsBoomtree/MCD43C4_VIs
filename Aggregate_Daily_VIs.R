
library(gdalUtils)
library(tools)
library(raster)
library(ncdf4)
library(filesstrings)
library(rslurm)



temp_agg_output    <- "G:/Temp/MCD43C4/reprocessed/tif/Monthly/VIs"
nc_output          <- "G:/Temp/MCD43C4/reprocessed/nc"
agg_tiff_output    <- "G:/Temp/MCD43C4/reprocessed/tif/1-deg/Monthly/VIs"

tiff_res           <- 1.0
year_list          <- c(2019:2021)


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