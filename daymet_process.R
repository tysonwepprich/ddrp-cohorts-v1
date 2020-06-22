# process daymet
library(raster)
# library(maps)
# library(maptools)
library(dplyr)
library(ncdf4)
library(foreach)
library(doParallel)
library(stringr)

# Steps
# 1. Continent brick per year
# 2. Process in parallel by day
# 3. projectRaster by day slice and write daily file

# Worked, except didn't crop to rectangle after reprojecting            
# e_2 <- extent(-131, -52, 24, 55); out2 <- crop(outr, e_2)

new_dir <- "daymet_na4k"

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

agg_res <- 4
fs <- list.files("orders", pattern = '.nc4', full.names = TRUE, recursive = TRUE)
fs <- fs[grep(pattern = "tmin", x = fs, fixed = TRUE)]

outfiles <- foreach(f = 1:length(fs),
                    .packages= c("raster", "stringr"),
                    .inorder = FALSE)%:%
  foreach(day = 1:365,
          .packages= c("raster", "stringr"),
          .inorder = FALSE)%dopar%{
            
            fl <- fs[f]
            newname <- stringr::str_split_fixed(string = fl, pattern = "_", n = 5)[,5]
            varname <- stringr::str_split_fixed(string = newname, pattern = "_", n = 3)[,1]
            yr <- stringr::str_split_fixed(string = newname, pattern = "_", n = 3)[,2]
            
            ifelse(!dir.exists(file.path(new_dir, yr)), dir.create(file.path(new_dir, yr), recursive = TRUE), FALSE)
            
            
            newname <- stringr::str_split_fixed(string = newname, pattern = "_", n = 3)[,1:2]
            # for CONUSPLUS
            e_2 <- extent(-131, -63, 24, 55)
            e_1 <- extent(-3146358, 4689822, -2035046, 2219754)
            
            # # for LOCO high resolution
            # e_2 <- extent(-120, -110, 28, 42)
            # e_1 <- extent(-2500000, 0, -2000000, 500000)
            
            fslice <- brick(fl, varname = varname, level = 1)[[day]]
            proj4string(fslice) <- CRS("+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +lat_2=60 +datum=WGS84 +units=m")
            
            # reproject to prism CRS
            prism_res <- 25.875 / 621 # divided by 4 for Daymet 1 x 1
            newproj <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
            
            agg <- aggregate(crop(fslice, e_1), fact=agg_res, fun=mean, expand=FALSE, na.rm=TRUE)

            outr <- projectRaster(agg, res = prism_res, crs = newproj, method="bilinear")
            
            out2 <- crop(outr, e_2)
            
            dat <- gsub(pattern = "X", replacement = "", x = names(fslice), fixed = TRUE) 
            dat <- gsub(pattern = ".", replacement = "", x = dat, fixed = TRUE) 
            
            fname <-  paste0("/home/tyson/REPO/ddrp-cohorts-v1/", new_dir, "/", newname[2], "/DAYMET_", varname, "_stable_4kmD1_", dat, ".grd")
            writeRaster(out2, filename = fname, overwrite = TRUE)
            
            removeTmpFiles(h=0.1)
          }

if(exists("cl")){
  stopCluster(cl)
}


# check files
fs <- list.files("daymet_na4k", pattern = '.grd', full.names = TRUE, recursive = TRUE)

