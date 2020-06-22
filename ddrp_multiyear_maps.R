# Multiyear maps

# Load the required packages
pkgs <- c("doParallel", "plyr", "dplyr", "foreach", "ggplot2", "ggthemes", 
          "lubridate", "mapdata", "mgsub", "optparse", "parallel",
          "purrr", "RColorBrewer", "rgdal", "raster", "readr", "sp", "stringr", 
          "tidyr", "tictoc", "tools", "viridis")
ld_pkgs <- lapply(pkgs, library, lib.loc = "/usr/lib64/R/library/", character.only = TRUE)
source("DDRP_cohorts_v1_funcs.R")
params_dir <- "/home/tyson/REPO/ddrp-cohorts-v1/spp_params/" # tyson's GRUB
REGION <- Assign_extent("LOCO") # Bounding box
theme_set(theme_bw(base_size = 12) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())) 
sites <- read.csv("sitecoord.csv", header = TRUE) %>% 
  arrange(site)


# Create list of raster brick files 
Diap_wtd_fls <- list.files(path = "DDRP_results", pattern = "all_weighted.tif", full.names = TRUE, recursive = TRUE)
Diap_wtd_fls <- Diap_wtd_fls[grep(pattern = "DCA_", x = Diap_wtd_fls, fixed = TRUE)]


# FullGen plots are the same regardless of photoperiod response
maptype <-  "FullGen"
index <- 1
fls_10yr <- Diap_wtd_fls[grep(pattern = "FullGen", x = Diap_wtd_fls, fixed = TRUE)]
fls_10yr <- fls_10yr[grep(pattern = "BigBend", x = fls_10yr, fixed = TRUE)]

brks <- lapply(X = fls_10yr, FUN = function(x) brick(x)[[6]]) # for bimonthly maps, take last result
rankbrk <- lapply(brks, FUN = function(x) mean(getValues(x), na.rm = TRUE))

for (quant in c("min", "mean", "max")){
  # for (quant in c("2010", "2018")){
  #   if (quant == "2010"){
  #     brk <- stack(brks[[1]])
  #     yr <- paste("2010 (coolest)")
  #   } 
  #   if (quant == "2018"){
  #     brk <- stack(brks[[8]])
  #     yr <- paste("2018 (warmest)")
  #   }
  if (quant == "min"){
    brk <- min(stack(brks), na.rm = TRUE)
    yr <- paste("2010-2019 (minimum)")
  }
  if (quant == "mean"){
    brk <- mean(stack(brks), na.rm = TRUE)
    yr <- paste("2010-2019 (average)")
  }
  if (quant == "max"){
    brk <- max(stack(brks), na.rm = TRUE)
    yr <- paste("2010-2019 (maximum)")
  }
  
  df <- ConvDF(brk)
  nam <- "FullGen"
  titl <- paste(case_when(nam == "AttVolt" ~ "Attempted voltinism",
                          nam == "FullGen" ~ "Potential voltinism",
                          nam == "Diapause" ~ "Chose to diapause",
                          nam == "Mismatch" ~ "Voltinism mismatch"), "for", yr, sep = " ")
  subtitl <- "No photoperiod response"
  
  reg.df <- readRDS(paste(params_dir, "na_lines.rds", sep = "/"))
  sites$shp <- "Other"
  
  if(nam %in% c("AttVolt", "FullGen")){
    # too many generations is overwhelming on map 
    # TODO: add maxgen+ as highest discrete generation value
    
    plotmax <- max(df$value / 1000)
    if (plotmax >= 10){
      df$value[df$value >= 10000] <- 10000
      df$value <- as.factor(round(df$value / 1000))
      levels(df$value)[length(levels(df$value))] <- 
        paste0(levels(df$value)[length(levels(df$value))], " or more")
      plotmax <- 10
    }else{
      df$value <- as.factor(round(df$value / 1000))
    }
    
    
    p <- ggplot(reg.df, aes(x = long, y = lat)) + 
      geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
      geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
      coord_quickmap(xlim = c(-118, -111), 
                     ylim = c(30, 40), expand = FALSE) +
      scale_fill_viridis(discrete = TRUE, name = "Generations", 
                         begin = 0, end = plotmax/10) +
      geom_point(data = sites, aes(x = lon, y = lat), color = "black", size = 4, shape = 1) +
      ggtitle(titl, subtitle = subtitl) +
      xlab("Longitude") +
      ylab("Latitude") +
      guides(colour=FALSE)
    ggsave(p,file=paste0(nam,"_", quant, ".png"), height = 6, width = 6,
           units = c('in'), dpi = 300) 

  }
}
  
  


fls_all <- expand.grid(maptype = c("AttVolt", "Diapause", "Mismatch"),
                       site = c("BigBend", "Blythe", "Cibola", "Delta", "Imperial", "Lovell", "Princess", "StGeorge"))

for (index in 1:nrow(fls_all)){
  
  fls_10yr <- Diap_wtd_fls[grep(pattern = fls_all$maptype[index], x = Diap_wtd_fls, fixed = TRUE)]
  fls_10yr <- fls_10yr[grep(pattern = fls_all$site[index], x = fls_10yr, fixed = TRUE)]
  sitename <- sites$site[grep(pattern = str_sub(fls_all$site[index], start = 1L, end = 2L), x = sites$site, fixed = TRUE)]
  
  brks <- lapply(X = fls_10yr, FUN = function(x) brick(x)[[6]]) # for bimonthly maps, take last result
  rankbrk <- lapply(brks, FUN = function(x) mean(getValues(x), na.rm = TRUE))
  
  # for (quant in c("min", "mean", "max")){
  #   if (quant == "min"){
  #     brk <- min(stack(brks), na.rm = TRUE)
  #     yr <- paste("2010-2019 (minimum)")
  #   }
  #   if (quant == "mean"){
  #     brk <- mean(stack(brks), na.rm = TRUE)
  #     yr <- paste("2010-2019 (average)")
  #   }
  #   if (quant == "max"){
  #     brk <- max(stack(brks), na.rm = TRUE)
  #     yr <- paste("2010-2019 (maximum)")
  #   }
    for (quant in c("2010", "2018")){
      if (quant == "2010"){
        brk <- stack(brks[[1]])
        yr <- paste("2010 (coolest)")
      } 
      if (quant == "2018"){
        brk <- stack(brks[[8]])
        yr <- paste("2018 (warmest)")
      }
    
    df <- ConvDF(brk)
    nam <- fls_all$maptype[index]
    titl <- paste(case_when(nam == "AttVolt" ~ "Attempted voltinism",
                      nam == "FullGen" ~ "Potential voltinism",
                      nam == "Diapause" ~ "Chose to diapause",
                      nam == "Mismatch" ~ "Voltinism mismatch"), "for", yr, sep = " ")
    subtitl <- paste0(as.character(sitename), " photoperiod response (in red)")
    
    reg.df <- readRDS(paste(params_dir, "na_lines.rds", sep = "/"))
    sites$shp <- "Other"
    sites$shp[sites$site == sitename] <- sitename
    
    if(nam %in% c("AttVolt", "FullGen")){
      # too many generations is overwhelming on map 
      # TODO: add maxgen+ as highest discrete generation value
      
      plotmax <- max(df$value / 1000)
      
      # if (plotmax <= 3){
        # df$value <- as.factor(round(df$value / 1000 / .25) * .25)
      # }
      # if (plotmax > 3 & plotmax < 5){
        # df$value <- as.factor(round(df$value / 1000 / .5) * .5)
      # }
      # if (plotmax >= 5){
      #   df$value[df$value >= 5000] <- 5000
      #   df$value <- as.factor(round(df$value / 1000 / .5) * .5)
      #   levels(df$value)[length(levels(df$value))] <- 
      #     paste0(levels(df$value)[length(levels(df$value))], " or more")
      #   plotmax <- 5
      # }
      if (plotmax >= 10){
        df$value[df$value >= 10000] <- 10000
        df$value <- as.factor(round(df$value / 1000))
        levels(df$value)[length(levels(df$value))] <- 
          paste0(levels(df$value)[length(levels(df$value))], " or more")
        plotmax <- 10
      }else{
        df$value <- as.factor(round(df$value / 1000))
      }
      
      
      p <- ggplot(reg.df, aes(x = long, y = lat)) + 
        geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
        geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
        coord_quickmap(xlim = c(-118, -111), 
                       ylim = c(30, 40), expand = FALSE) +
        scale_fill_viridis(discrete = TRUE, name = "Generations", 
                           begin = 0, end = plotmax/10) +
        geom_point(data = sites, aes(x = lon, y = lat, color = shp), size = 4, shape = 1) +
        scale_color_manual(values=c("red", "black")) +
        ggtitle(titl, subtitle = subtitl) +
        xlab("Longitude") +
        ylab("Latitude") +
        guides(colour=FALSE)
      ggsave(p,file=paste0(sitename, "_", nam,"_", quant, ".png"), height = 6, width = 6,
             units = c('in'), dpi = 300) 
    } 
    
    if(nam == "Diapause"){
      # TODO Diapause map doesn't match lost generations because of pre-diap?
      df$value <- df$value / 10 # Percent

      p <- ggplot(reg.df, aes(x = long, y = lat)) + 
        geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
        geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
        coord_quickmap(xlim = c(-118, -111), 
                       ylim = c(30, 40), expand = FALSE) +
        scale_fill_viridis(discrete = FALSE, name = "% in Diapause", 
                           begin = 0, end = 1) +
        geom_point(data = sites, aes(x = lon, y = lat, color = shp), size = 4, shape = 1) +
        scale_color_manual(values=c("red", "black")) +
        ggtitle(titl, subtitle = subtitl) +
        xlab("Longitude") +
        ylab("Latitude") +
        guides(colour=FALSE)
      ggsave(p, file=paste0(sitename, "_", nam,"_", quant, ".png"), height = 6, width = 6,
             units = c('in'), dpi = 300) 
      
    } 
    
    if(nam == "Mismatch"){
      minmm <- round(min(df$value/1000), 1) - .1
      df$value <- df$value / 1000
      if(minmm < -5){
        mmbrk <- c(minmm, -5, -4, -3, -2, -1, 0, .25, .5, .75, 1)
        df$value <- cut(df$value, mmbrk)
        cols <- setNames(c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", 
                           "#c7eae5", "#80cdc1", "#35978f", "#01665e"), 
                         levels(df$value))
      }else{
        mmbrk <- c(-5, -4, -3, -2, -1, 0, .25, .5, .75, 1)
        df$value <- cut(df$value, mmbrk)
        cols <- setNames(c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", 
                           "#c7eae5", "#80cdc1", "#35978f", "#01665e"), 
                         levels(df$value))
      }
      
      p <- ggplot(reg.df, aes(x = long, y = lat)) + 
        geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
        geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
        coord_quickmap(xlim = c(-118, -111), 
                       ylim = c(30, 40), expand = FALSE) +
        scale_fill_manual(values = cols, name = "Mismatch") +
        geom_point(data = sites, aes(x = lon, y = lat, color = shp), size = 4, shape = 1) +
        scale_color_manual(values=c("red", "black")) +
        ggtitle(titl, subtitle = subtitl) +
        xlab("Longitude") +
        ylab("Latitude") +
        guides(colour=FALSE)
      ggsave(p,file=paste0(sitename, "_", nam,"_", quant, ".png"), height = 6, width = 6,
             units = c('in'), dpi = 300) 
    }
  }
}



# Suggestion from Fritzi: mismatch maps but with % within range of good matches (-1-0.25?)
mm_cohorts <- list.files(path = "DDRP_results", pattern = "Mismatch_cohort", full.names = TRUE, recursive = TRUE)
mm_cohorts <- mm_cohorts[grep(pattern = "DCA_", x = mm_cohorts, fixed = TRUE)]

for (index in 1:nrow(sites)){
  
  fls_10yr <- Diap_wtd_fls[grep(pattern = fls_all$maptype[index], x = Diap_wtd_fls, fixed = TRUE)]
  fls_10yr <- fls_10yr[grep(pattern = fls_all$site[index], x = fls_10yr, fixed = TRUE)]
  sitename <- sites$site[grep(pattern = str_sub(fls_all$site[index], start = 1L, end = 2L), x = sites$site, fixed = TRUE)]
  
  brks <- lapply(X = fls_10yr, FUN = function(x) brick(x)[[6]]) # for bimonthly maps, take last result
  rankbrk <- lapply(brks, FUN = function(x) mean(getValues(x), na.rm = TRUE))
  
  # for (quant in c("min", "mean", "max")){
  #   if (quant == "min"){
  #     brk <- min(stack(brks), na.rm = TRUE)
  #     yr <- paste("2010-2019 (minimum)")
  #   }
  #   if (quant == "mean"){
  #     brk <- mean(stack(brks), na.rm = TRUE)
  #     yr <- paste("2010-2019 (average)")
  #   }
  #   if (quant == "max"){
  #     brk <- max(stack(brks), na.rm = TRUE)
  #     yr <- paste("2010-2019 (maximum)")
  #   }
  for (quant in c("2010", "2018")){
    if (quant == "2010"){
      brk <- stack(brks[[1]])
      yr <- paste("2010 (coolest)")
    } 
    if (quant == "2018"){
      brk <- stack(brks[[8]])
      yr <- paste("2018 (warmest)")
    }
    
    df <- ConvDF(brk)
    nam <- fls_all$maptype[index]
    titl <- paste(case_when(nam == "AttVolt" ~ "Attempted voltinism",
                            nam == "FullGen" ~ "Potential voltinism",
                            nam == "Diapause" ~ "Chose to diapause",
                            nam == "Mismatch" ~ "Voltinism mismatch"), "for", yr, sep = " ")
    subtitl <- paste0(as.character(sitename), " photoperiod response (in red)")
    
    reg.df <- readRDS(paste(params_dir, "na_lines.rds", sep = "/"))
    sites$shp <- "Other"
    sites$shp[sites$site == sitename] <- sitename
    
    if(nam %in% c("AttVolt", "FullGen")){
      # too many generations is overwhelming on map 
      # TODO: add maxgen+ as highest discrete generation value
      
      plotmax <- max(df$value / 1000)
      
      # if (plotmax <= 3){
      # df$value <- as.factor(round(df$value / 1000 / .25) * .25)
      # }
      # if (plotmax > 3 & plotmax < 5){
      # df$value <- as.factor(round(df$value / 1000 / .5) * .5)
      # }
      # if (plotmax >= 5){
      #   df$value[df$value >= 5000] <- 5000
      #   df$value <- as.factor(round(df$value / 1000 / .5) * .5)
      #   levels(df$value)[length(levels(df$value))] <- 
      #     paste0(levels(df$value)[length(levels(df$value))], " or more")
      #   plotmax <- 5
      # }
      if (plotmax >= 10){
        df$value[df$value >= 10000] <- 10000
        df$value <- as.factor(round(df$value / 1000))
        levels(df$value)[length(levels(df$value))] <- 
          paste0(levels(df$value)[length(levels(df$value))], " or more")
        plotmax <- 10
      }else{
        df$value <- as.factor(round(df$value / 1000))
      }
      
      
      p <- ggplot(reg.df, aes(x = long, y = lat)) + 
        geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
        geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
        coord_quickmap(xlim = c(-118, -111), 
                       ylim = c(30, 40), expand = FALSE) +
        scale_fill_viridis(discrete = TRUE, name = "Generations", 
                           begin = 0, end = plotmax/10) +
        geom_point(data = sites, aes(x = lon, y = lat, color = shp), size = 4, shape = 1) +
        scale_color_manual(values=c("red", "black")) +
        ggtitle(titl, subtitle = subtitl) +
        xlab("Longitude") +
        ylab("Latitude") +
        guides(colour=FALSE)
      ggsave(p,file=paste0(sitename, "_", nam,"_", quant, ".png"), height = 6, width = 6,
             units = c('in'), dpi = 300) 
    } 
    
    if(nam == "Diapause"){
      # TODO Diapause map doesn't match lost generations because of pre-diap?
      df$value <- df$value / 10 # Percent
      
      p <- ggplot(reg.df, aes(x = long, y = lat)) + 
        geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
        geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
        coord_quickmap(xlim = c(-118, -111), 
                       ylim = c(30, 40), expand = FALSE) +
        scale_fill_viridis(discrete = FALSE, name = "% in Diapause", 
                           begin = 0, end = 1) +
        geom_point(data = sites, aes(x = lon, y = lat, color = shp), size = 4, shape = 1) +
        scale_color_manual(values=c("red", "black")) +
        ggtitle(titl, subtitle = subtitl) +
        xlab("Longitude") +
        ylab("Latitude") +
        guides(colour=FALSE)
      ggsave(p, file=paste0(sitename, "_", nam,"_", quant, ".png"), height = 6, width = 6,
             units = c('in'), dpi = 300) 
      
    } 
    
    if(nam == "Mismatch"){
      minmm <- round(min(df$value/1000), 1) - .1
      df$value <- df$value / 1000
      if(minmm < -5){
        mmbrk <- c(minmm, -5, -4, -3, -2, -1, 0, .25, .5, .75, 1)
        df$value <- cut(df$value, mmbrk)
        cols <- setNames(c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", 
                           "#c7eae5", "#80cdc1", "#35978f", "#01665e"), 
                         levels(df$value))
      }else{
        mmbrk <- c(-5, -4, -3, -2, -1, 0, .25, .5, .75, 1)
        df$value <- cut(df$value, mmbrk)
        cols <- setNames(c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", 
                           "#c7eae5", "#80cdc1", "#35978f", "#01665e"), 
                         levels(df$value))
      }
      
      p <- ggplot(reg.df, aes(x = long, y = lat)) + 
        geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
        geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
        coord_quickmap(xlim = c(-118, -111), 
                       ylim = c(30, 40), expand = FALSE) +
        scale_fill_manual(values = cols, name = "Mismatch") +
        geom_point(data = sites, aes(x = lon, y = lat, color = shp), size = 4, shape = 1) +
        scale_color_manual(values=c("red", "black")) +
        ggtitle(titl, subtitle = subtitl) +
        xlab("Longitude") +
        ylab("Latitude") +
        guides(colour=FALSE)
      ggsave(p,file=paste0(sitename, "_", nam,"_", quant, ".png"), height = 6, width = 6,
             units = c('in'), dpi = 300) 
    }
  }
}

