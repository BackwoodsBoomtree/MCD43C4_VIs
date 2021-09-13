



# nc_by_vi_year  <- function (in_dir, out_dir, res, vis) {
#   
#   for (i in 1:length(vis)) {
#     
#     sub_dir_list <- list.dirs(paste0(in_dir, "/", vis[i]), recursive = FALSE)
#     
#     for (s in 1:length(sub_dir_list)) {
#       
#       print(paste0("Starting to put ", vis[i], " files into .nc from the year ", basename(sub_dir_list[s])))
#       sub_dir_files <- list.files(sub_dir_list[s], full.names = TRUE, pattern = "*.tif$")
#       
#       temp_output_dir <- paste0(out_dir, "/", res, "/", vis[i])
#       print(paste0("Output dir is: ", temp_output_dir))
#       
#       if (!dir.exists(temp_output_dir)) { # Create output dirs for each year
#         dir.create(temp_output_dir, recursive = TRUE)
#       }
#       
#       vi_stack <- stack(sub_dir_files)
#       
#       # For some reason, some large values in a few pixels for EVI; kick them out
#       if (vis[i] != "LSWI") {
#         # old way
#         # m_pos <- matrix(c(1, Inf, NA), ncol = 3, byrow = T)
#         # vi_stack <- reclassify(vi_stack, m_pos)
#         
#         vi_stack <- clamp(vi_stack, lower = 0, upper = 1, useValues = FALSE) # F sets values outside threshold to NA
#         print("Got rid of values > 1; VI is not LSWI")
#       }
#       
#       vi_stack <- round(vi_stack, digits = 4)
#       
#       out_name <- substr(basename(sub_dir_files[1]), 1, 13)
#       out_name <- paste0(out_name, ".006_", vis[i], "_Monthly.nc")
#       writeRaster(vi_stack, paste0(temp_output_dir, "/", out_name), overwrite = TRUE, format = "CDF", NAflag = -9999, datatype = 'FLT4S', compression = 4, varname = vis[i], 
#                   longname = paste0("Monthly MCD43C4 v006 ", vis[i]), xname = "Longitude",  yname = "Latitude", zname = "Time", zunit = "Month")
#       print(paste0("Saved file to: ", temp_output_dir, "/", out_name))
#     }
#   }
# }
