library(terra)

in_dir  <- "G:/MCD43C4/nc/daily/0.05"
vi_list <- c("EVI", "NDVI", "NIRv", "LSWI")
out_dir <- "G:/MCD43C4/nc/monthly/0.05"

for(vi in vi_list) {
  
  out_d <- paste0(out_dir, "/", vi)
  
  if (!dir.exists(out_d)) {
    dir.create(out_d, recursive = TRUE)
  }
  
  file_list <- list.files(paste0(in_dir, "/", vi), full.names = TRUE, pattern = "*.nc")
  
  for(i in 1:length(file_list)) {
    
    daily   <- rast(file_list[i])
    monthly <- mean(daily, na.rm = TRUE)
    
    out_name <- basename(file_list[i])
    out_name <- gsub("Daily", "Monthly", out_name)
    out_name <- paste0(out_dir, "/", vi, "/", out_name)
    
    writeCDF(monthly, filename = out_name, varname = vi, unit = "", compression = 4, missval = -9999, overwrite = TRUE)
    print(paste0("Saved file to: ", out_name))
    
  }
}



