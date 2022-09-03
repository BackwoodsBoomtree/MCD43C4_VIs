
library(filesstrings)

vi_dir    <- "/mnt/g/MCD43C4/v061/tif/daily/0.05"
vi_list   <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
year_list <- c(2005:2006, 2009:2011)

annual_folders <- function (in_dir, vis, years) {

  for (vi in 1:length(vis)) {
    
    message(paste0("Working on ", vis[vi]))

    vi_file_list <- list.files(paste0(in_dir, "/", vis[vi]), full.names = TRUE, pattern = "*.tif$", recursive = TRUE)

    for (y in 1:length(years)) {

      year_dir <- paste0(in_dir, "/", vis[vi], "/", years[y])

      if (!dir.exists(year_dir)) { # Create output dirs for each year
        dir.create(year_dir, recursive = TRUE)
        message(paste0("Created ", year_dir))
      }

      year_file_list <- vi_file_list[grep(paste0("A", years[y]), vi_file_list)] # Year is preceded by A in the file names

      for (f in 1:length(year_file_list)) {
        move_files(year_file_list[f], year_dir)
      }
      message("I have moved all files for ", vis[vi], " ", years[y])
    }
  }
  message("Move complete.")
}

annual_folders(vi_dir, vi_list, year_list)