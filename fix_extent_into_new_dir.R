library(terra)

all_daily_files <- list.files("/mnt/g/MCD43C4/tif/Daily/0.05", full.names = TRUE, pattern = "*.tif$", recursive = TRUE)
new_dir         <- "/mnt/g/MCD43C4/tif/new_Daily/0.05"

if (!dir.exists(new_dir)) {
  dir.create(new_dir, recursive = TRUE)
}

n <- 0

for (i in 1:length(all_daily_files)) {
  
  index <- rast(all_daily_files[i])
  
  if (ext(index) != ext(c(-180, 180, -90, 90))) {
    
    new_raster <- rast(vals = values(index), xmin = -180, xmax = 180, ymin = -90, ymax = 90, ncols = 7200, nrows = 3600, crs = "+proj=longlat +datum=WGS84")
    
    # Build filename including two parent dirs
    parts <- unlist(strsplit(all_daily_files[i], .Platform$file.sep))
    
    out_dir <- paste0(new_dir, "/", parts[length(parts) - 2], "/", parts[length(parts) - 1])
    
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    
    outname <- paste0(out_dir, "/", basename(all_daily_files[i]))
    
    writeRaster(new_raster, outname, overwrite = TRUE, datatype = 'INT4S', NAflag = -9999)
    
    n <- n +1
    print("Fixed file.")
  }
}

print(paste0("Fixed extent for ", n, " files."))
