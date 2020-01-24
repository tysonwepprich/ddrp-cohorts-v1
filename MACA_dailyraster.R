# check out ento: ssh ento -l tyson -p 732
# shell call to copy macaV2 files (each 1.8G)
# scp -P 732 tyson@ento:/home/macav2metdata/IPSL_rcp85/macav2metdata_tas*_2046_2050_CONUS_daily.nc /home/tyson/REPO/photovoltinism/data/maca

library(raster)
library(ncdf4)
library(stringr)

# longitude in degrees EAST
# time is days since 1900-01-01 00:00:00
# temperature in K? -273.15

# macafile <- "/home/macav2metdata/netcdf/macav2metdata_tasmax_HadGEM2-CC365_r1i1p1_rcp45_2016_2020_CONUS_daily.nc"


# Function to split 5-year data into daily tmin/tmax rasters for use in lifecycle model
SplitMACA <- function(macafile, targetdir){
  ras <- brick(macafile, 
               varname = "air_temperature", lvar = 3, level = 4)
  
  # # if you have permission to write, you could make directories for each year here
  # yrs <- gregexpr(pattern = "[0-9]{4}", text = macafile)
  # yr1 <- as.numeric(substr(macafile, start = yrs[[1]][1], stop = yrs[[1]][1] + 3))
  # yr2 <- as.numeric(substr(macafile, start = yrs[[1]][2], stop = yrs[[1]][2] + 3))
  var <- sub(pattern = "as", replacement = "", x = stringr::str_split_fixed(macafile, pattern = "_", n = 3)[, 2], fixed = TRUE)
  # 
  # for (y in yr1:yr2){
  #   dir.create(paste(targetdir, y, sep = "/"))
  # }
  
  fname <- stringr::str_split_fixed(macafile, pattern = "_", 9)
  varname <- paste(var, fname[,3], fname[,5], "4kmD1", sep = "_")

  # loop every day and write new daily rasters
  # shift longitude values and Kelvin temperature scale for MACA to match other raster data
  for (d in 1:nlayers(ras)){
    dras <- shift(ras[[d]], dx = -360) - 273.15
    date <- gsub(pattern = "[^0-9]", replacement = "", x = names(dras))
    yr <- substr(date, start = 1, stop = 4)
    newname <- paste0("MACAV2_", varname, "_", date, ".grd")
    
    writeRaster(x = dras, filename = paste(targetdir, yr, newname, sep = "/"), format = "raster", overwrite = TRUE)
  }
}

# t <- system.time({
  # SplitMACA(macafile, "/home/macav2metdata")
# })

