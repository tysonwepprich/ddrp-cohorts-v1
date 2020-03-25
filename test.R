# download state/province boundaries for mapping
if (!file.exists("src/ref/ne_10m_admin_1_states_provinces_lakes/ne_10m_admin_1_states_provinces_lakes.dbf")){
  download.file(file.path('http://www.naturalearthdata.com/http/',
                          'www.naturalearthdata.com/download/10m/cultural',
                          'ne_10m_admin_1_states_provinces_lakes.zip'), 
                f <- tempfile())
  unzip(f, exdir = "src/ref/ne_10m_admin_1_states_provinces_lakes")
  rm(f)
}

newproj <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

region <- rgdal::readOGR(paste("src/ref", "ne_10m_admin_1_states_provinces_lakes", sep = "/"), 'ne_10m_admin_1_states_provinces_lakes', encoding='UTF-8')
region <-sp::spTransform(region, CRS(proj4string(ras)))
region <- crop(region, ras)


reg.points = fortify(region, region="name_en")
reg.df = left_join(reg.points, region@data, by = c("id" = "name_en")) %>% 
  filter(geonunit %in% c("United States of America", "Canada", "Mexico"),
         long < -50)



ras <- raster("../photovoltinism/daymet/2017/DAYMET_tmax_stable_4kmD1_20170101_bil.grd")
crs(ras) <- newproj


reg.df <- readRDS(paste(params_dir, "na_lines.rds", sep = "/"))
df <- ConvDF(ras)

p <- ggplot(reg.df, aes(x = long, y = lat)) + 
  geom_tile(data = df, aes(x = x, y = y, fill = value)) + 
  geom_path(aes(group = group), color = "gray20", lwd = 0.4)
p


dl <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/2017/daymet_v3_tmin_2017_na.nc4"
fn <- stringr::str_split_fixed(dl, pattern = stringr::coll("/"), n = 9)[,9]

download.file(dl, 
              destfile = fn)


fl <- list.files(pattern = ".nc4")
newname <- stringr::str_split_fixed(string = fl, pattern = "_", n = 3)[,3]
newname <- gsub(x = newname, pattern = ".nc4", replacement = ".grd", fixed = TRUE)
varname <- stringr::str_split_fixed(string = newname, pattern = "_", n = 3)[,1]
e <- extent(-142, -52, 24, 60)

fslice <- brick(fl, varname = varname, level = 1)[[1]]

# reproject to prism CRS
prism_res <- 25.875 / 621
newproj <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
dayproj <- "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +lat_2=60 +datum=WGS84 +units=m"

crs(fslice) <- dayproj
# outr <- raster::aggregate(fslice, fact = 4, fun = mean)
outr <- crop(projectRaster(fslice, res = prism_res, crs = newproj, method="bilinear"), e)
outr2 <- projectRaster(fslice, crs="+proj=longlat +datum=WGS84")


r <- brick("daymet_v3_tmin_2017_na.nc4", varname = "tmin", level = 1)
outr <- brick(fname)
df2 <- ConvDF(outr)

region <- rgdal::readOGR(paste("src/ref", "ne_10m_admin_1_states_provinces_lakes", sep = "/"), 'ne_10m_admin_1_states_provinces_lakes', encoding='UTF-8')
region <-sp::spTransform(region, CRSobj = crs(outr))
reg.points = fortify(region, region="name_en")
reg.df = left_join(reg.points, region@data, by = c("id" = "name_en")) %>% 
  filter(geonunit %in% c("United States of America", "Canada", "Mexico"),
         long < -50)

p <- ggplot(reg.df, aes(x = long, y = lat)) + 
  geom_tile(data = df2, aes(x = x, y = y, fill = value)) + 
  geom_path(aes(group = group), color = "gray20", lwd = 0.4)
p
