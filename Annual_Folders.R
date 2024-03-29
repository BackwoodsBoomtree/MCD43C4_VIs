
library(filesstrings)

vi_dir    <- "/mnt/g/MCD43C4/tif/Daily/0.05"
vi_list   <- c("NIRv", "RED", "NIR")
year_list <- c(2019:2020)

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
        move_files(year_file_list[f], year_dir)
      }
    }
  }
}

annual_folders(vi_dir, vi_list, year_list)