library(terra)

in_dir       <- "G:/MCD43C4/monthly/0.05"
vi_list      <- c("EVI", "NDVI", "NIRv", "LSWI")
out_dir      <- "G:/MCD43C4/nc/monthly/1deg"
in_res       <- 0.05
out_res      <- 1.0 # in degrees
in_res_name  <- "0.05.nc"
out_res_name <- "1deg.nc"

for(vi in vi_list) {
  
  out_d <- paste0(out_dir, "/", vi)
  
  if (!dir.exists(out_d)) {
    dir.create(out_d, recursive = TRUE)
  }
  
  file_list <- list.files(paste0(in_dir, "/", vi), full.names = TRUE, pattern = "*.nc")
  
  for(i in 1:length(file_list)) {
    
    infile  <- rast(file_list[i])
    outfile <- aggregate(infile, fact = (out_res / in_res),  fun = "mean", na.rm = TRUE)
    
    out_name <- basename(file_list[i])
    out_name <- gsub(in_res_name, out_res_name, out_name)
    out_name <- paste0(out_dir, "/", vi, "/", out_name)
    
    writeCDF(outfile, filename = out_name, varname = vi, unit = "", compression = 4, missval = -9999, overwrite = TRUE)
    print(paste0("Saved file to: ", out_name))
    
  }
}