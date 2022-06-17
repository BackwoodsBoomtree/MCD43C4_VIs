
library(terra)
library(parallel)

tif_input    <- "/mnt/g/MCD43C4/tif/monthly/0.05"
nc_output    <- "/mnt/g/MCD43C4/nc/monthly/0.05"
time_res     <- "Monthly"
spatial_res  <- "0.05"

nc_by_vi_year <- function (in_dir, out_dir, t_res, s_res) {
  
  year_dirs <- list.dirs(in_dir, recursive = FALSE)

  for (i in 1:length(year_dirs)) {
    
    # Build filename including two parent dirs
    parts      <- unlist(strsplit(year_dirs[i], .Platform$file.sep))
    vi         <- parts[length(parts) - 1]
    year       <- parts[length(parts)]
    output_dir <- paste(out_dir, vi, year, sep = "/")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    print(paste0("Placing files from ", year_dirs[i], " into .nc file."))
    print(paste0("Output dir is: ", output_dir))
    
    file_list <- list.files(year_dirs[i], full.names = TRUE, pattern = "*.tif$")

    vi_stack <- sds(file_list)

    out_name <- substr(basename(file_list[1]), 1, 13)
    out_name <- paste(out_name, vi, t_res, s_res, "nc", sep = ".")
    out_name <- paste0(output_dir, "/", out_name)
    l_name   <- paste0(t_res, " MCD43C4 v006 ", vi)
    writeCDF(vi_stack, filename = out_name, overwrite = TRUE)
    print(paste0("Saved file to: ", out_name))
  }
}


vi_dirs      <- list.dirs(tif_input, recursive = FALSE)

# Dedicate each VI instance to a core
mclapply(vi_dirs, nc_by_vi_year, mc.cores = 1, out_dir = nc_output, t_res = time_res, s_res = spatial_res)