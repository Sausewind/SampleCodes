comnpute_SPI_raster <-function(){
  
  library(terra)
  library(ncdf4)
  library(metR)
  library(SCI)
  library(dplyr)
  library(stars)
  library(ncmeta)
  library(abind)
  library(cubelyr)
  library(runner)
  library(lmomco)
  library(fitdistrplus)
  library(lubridate)
  library(xlsx)
  library(colorRamps)
  library(stringr)
  library(RColorBrewer)
  library(raster)
  
  #Read ncdf data and store it as SpatRaster
  
  folder <- "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Data/HISTORICAL/HISTORICAL"
  files <- list.files(folder, pattern= ".nc",full.names = TRUE)

  nc_rain <-rast(nrows=354, ncols = 475)
  
  for (file in files){
    rain_layer<- rast(file, lyrs="Rain")
    nc_rain<- c(nc_rain,rain_layer)
  }
  lon_layer<- rast(file, lyrs="Longitude")
  lat_layer <-rast(file, lyrs="Latitude")
  
  lon<-values(lon_layer)
  lat<-values(lat_layer)
  
  ext(nc_rain) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(nc_rain)<-"EPSG:4326"
  
  nc_pet <-rast(nrows=354, ncols = 475)
  
  for (file in files){
    pet_layer<- rast(file, lyrs="PotentialEvapotranspiration")
    nc_pet<- c(nc_pet,pet_layer)
  }
  lon_layer<- rast(file, lyrs="Longitude")
  lat_layer <-rast(file, lyrs="Latitude")
  
  lon<-values(lon_layer)
  lat<-values(lat_layer)
  
  ext(nc_pet) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(nc_pet)<-"EPSG:4326"
  
  #subtract PET from rain
  
  precpet<-nc_rain-nc_pet
  
  
  # convert SpatRaster to stars object
  
  nc_stars_ppet <-st_as_stars(precpet, ignore_file = TRUE, as_attributes=all(terra::is.factor(precpet)))


  # calculate SPEI parameters based on historical data

  Para_func <-function(x) {if (all(is.na(x))) {rep(NA, times = 456)} 
    else {fitSCI(x, first.mon = 1, time.scale = 3, distr = "pe3", p0=TRUE, p0.center.mass = TRUE, start.fun.fix=TRUE)}}
  
  SPEI3.Pe3.fix.para <-st_apply(nc_stars_ppet, 1:2, Para_func, PROGRESS=TRUE)
  
  #Read 126 scenario data and store it as SpatRaster
  
  folder.126 <- "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Data/SSP126_IPSL_CM6A_LR/SSP126_IPSL_CM6A_LR"
  files.126 <- list.files(folder.126, pattern= ".nc",full.names = TRUE)
  
  nc_rain.126 <-rast(nrows=354, ncols = 475)
  
  for (file in files.126){
    rain_layer.126<- rast(file, lyrs="Rain")
    nc_rain.126<- c(nc_rain.126,rain_layer.126)
  }
  lon_layer<- rast(file, lyrs="Longitude")
  lat_layer <-rast(file, lyrs="Latitude")
  
  lon<-values(lon_layer)
  lat<-values(lat_layer)
  
  ext(nc_rain.126) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(nc_rain.126)<-"EPSG:4326"
  
  nc_pet.126 <-rast(nrows=354, ncols = 475)
  
  for (file in files.126){
    pet_layer.126<- rast(file, lyrs="PotentialEvapotranspiration")
    nc_pet.126<- c(nc_pet.126,pet_layer.126)
  }
  lon_layer<- rast(file, lyrs="Longitude")
  lat_layer <-rast(file, lyrs="Latitude")
  
  lon<-values(lon_layer)
  lat<-values(lat_layer)
  
  ext(nc_pet.126) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(nc_pet.126)<-"EPSG:4326"
  
  #subtract PET from rain
  
  precpet.126<-nc_rain.126-nc_pet.126
  
  
  # convert SpatRaster to stars object
  
  nc_stars_ppet.126 <-st_as_stars(precpet.126, ignore_file = TRUE, as_attributes=all(terra::is.factor(precpet.126)))
  
  # calculate SPEI3 for the 126 scenario
  
  (nc_stars_ppet_126.hist<-c(nc_stars_ppet.126, nc_stars_ppet, along=3))
  
  SPI_func <-function(x) {if (all(is.na(x))) {rep(NA, times = 600)} 
    else {transformSCI(x[1:600],first.mon=1, sci.limit= 3, obj=fitSCI(x[601:1056], first.mon = 1, time.scale = 3, distr = "pe3", p0=TRUE, p0.center.mass = FALSE, start.fun.fix=TRUE))}}
  
  SPEI.Pe3.SSP126.VU.FALSE <-st_apply(nc_stars_ppet_126.hist, 1:2, SPI_func, PROGRESS=TRUE)
  
  #Read 585 scenario data and store it as SpatRaster
  
  folder.585 <- "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Data/SSP585_IPSL_CM6A_LR"
  files.585 <- list.files(folder.585, pattern= ".nc",full.names = TRUE)
  
  nc_rain.585 <-rast(nrows=354, ncols = 475)
  
  for (file in files.585){
    rain_layer.585<- rast(file, lyrs="Rain")
    nc_rain.585<- c(nc_rain.585,rain_layer.585)
  }
  lon_layer<- rast(file, lyrs="Longitude")
  lat_layer <-rast(file, lyrs="Latitude")
  
  lon<-values(lon_layer)
  lat<-values(lat_layer)
  
  ext(nc_rain.585) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(nc_rain.585)<-"EPSG:4326"
  
  nc_pet.585 <-rast(nrows=354, ncols = 475)
  
  for (file in files.585){
    pet_layer.585<- rast(file, lyrs="PotentialEvapotranspiration")
    nc_pet.585<- c(nc_pet.585,pet_layer.585)
  }
  lon_layer<- rast(file, lyrs="Longitude")
  lat_layer <-rast(file, lyrs="Latitude")
  
  lon<-values(lon_layer)
  lat<-values(lat_layer)
  
  ext(nc_pet.585) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(nc_pet.585)<-"EPSG:4326"
  
  #subtract PET from rain
  
  precpet.585<-nc_rain.585-nc_pet.585
  
  
  # convert SpatRaster to stars object
  
  nc_stars_ppet.585 <-st_as_stars(precpet.585, ignore_file = TRUE, as_attributes=all(terra::is.factor(precpet.585)))
  
  # convert SpatRaster to stars object
  
  nc_stars_ppet.585 <-st_as_stars(precpet.585, ignore_file = TRUE, as_attributes=all(terra::is.factor(precpet.585)))
  
  # calculate SPEI3 for the 585 scenario
  
  (nc_stars_ppet_585.hist<-c(nc_stars_ppet.585, nc_stars_ppet, along=3))
  
  SPI_func <-function(x) {if (all(is.na(x))) {rep(NA, times = 600)} 
    else {transformSCI(x[1:600],first.mon=1, sci.limit= 3, obj=fitSCI(x[601:1056], first.mon = 1, time.scale = 3, distr = "pe3", p0=TRUE, p0.center.mass = FALSE, start.fun.fix=TRUE))}}
  
  SPEI.Pe3.SSP585.VU.FALSE <-st_apply(nc_stars_ppet_585.hist, 1:2, SPI_func, PROGRESS=TRUE)
  

#export monthly SPEI3 data in .tif format

for (i in 1:600){
  output.file <-str_replace(files.126[i],"Data/SSP126_IPSL_CM6A_LR/SSP126_IPSL_CM6A_LR" ,"Results/VU_Methodology/SPEI3/SSP126")
  output.file <-str_replace(output.file, ".nc",".tif")
  write_stars(SPEI.Pe3.SSP126.VU[,i,,], output.file)
}
  
  for (i in 1:600){
    output.file <-str_replace(files.585[i],"Data/SSP585_IPSL_CM6A_LR" ,"Results/VU_Methodology/SPEI3/SSP585")
    output.file <-str_replace(output.file, ".nc",".tif")
    write_stars(SPEI.Pe3.SSP585.VU[,i,,], output.file)
  }

#plot maps
  
  start <-seq(1,600, 12)
  end<-seq(12,612,12)
  year<-seq(2051,2100,1)
  
  for (i in 1:50){
    png(paste0("/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/VU_Methodology/SPEI3/SSP126/SPEI3_",year[i],".png"))
    colours <-brewer.pal(8, "RdYlGn") 
    plot(SPEI.Pe3.SSP126.VU[,start[i]:end[i],,], col=colours, main=months.list, breaks="kmeans")
    dev.off()
  }
  
  for (i in 1:50){
    png(paste0("/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/VU_Methodology/SPEI3/SSP585/SPEI3_",year[i],".png"))
    colours <-brewer.pal(8, "RdYlGn") 
    plot(SPEI.Pe3.SSP585.VU[,start[i]:end[i],,], col=colours, main=months.list, breaks="kmeans")
    dev.off()
  }
  
  # calculate aridity index
  
  AI <--0.9*(nc_rain/nc_pet)-0.8
  
  AI_stars <-st_as_stars(AI, ignore_file = TRUE, as_attributes=all(terra::is.factor(AI)))
  
  AI_mean <-st_apply(AI_stars, 1:2, mean,PROGRESS=TRUE)
  
  SPEI.Pe3.SSP126.VU.dmap<-aperm(SPEI.Pe3.SSP126.VU.FALSE,c(2,3,1))
  
  SPEI.Pe3.SSP126.VU.dmap <- split(SPEI.Pe3.SSP126.VU.dmap)
  
  SPEI.Pe3.SSP126.VU.dmap$AI_mean <-AI_mean
  
  SPEI.Pe3.SSP126.VU.AI<-merge(SPEI.Pe3.SSP126.VU.dmap)
  
  SPEI.Pe3.SSP585.VU.dmap<-aperm(SPEI.Pe3.SSP585.VU.FALSE,c(2,3,1))
  
  SPEI.Pe3.SSP585.VU.dmap <- split(SPEI.Pe3.SSP585.VU.dmap)
  
  SPEI.Pe3.SSP585.VU.dmap$AI_mean <-AI_mean
  
  SPEI.Pe3.SSP585.VU.AI<-merge(SPEI.Pe3.SSP585.VU.dmap)
  
  SPEI3.Pe3.fix.dmap<-aperm(SPEI3.Pe3.fix.FALSE,c(2,3,1))
  
  SPEI3.Pe3.fix.dmap <- split(SPEI3.Pe3.fix.dmap)
  
  SPEI3.Pe3.fix.dmap$AI_mean <-AI_mean
  
  SPEI3.Pe3.fix.AI<-merge(SPEI3.Pe3.fix.dmap)
  
  
### apply the aridity index and calculate annual drought maps 
  
  June <-seq(6,600,12)
  Oct <-seq(10,600,12)
  June.hist <-seq(6,456,12)
  Oct.hist <-seq(10,456,12)
  annual.droughts.126<-AI_mean
  annual.droughts.585<-AI_mean
  annual.droughts<-AI_mean
  
  
  for (i in 1:50){
    start <-June[i]
    print(start)
    end<-Oct[i]
    
    drought_func <-function(x) { 
      with(rle(x[start:end] < x[601]), sum(lengths[values] >= 1))
    }
    
    drought <-st_apply(SPEI.Pe3.SSP126.VU.AI, 1:2, drought_func, PROGRESS=TRUE)
    
    annual.droughts.126 <-c(annual.droughts.126, drought)
  }
  annual.droughts.126.final.SPEI3_1month<-merge(annual.droughts.126)
  
  annual.droughts.126.final.SPEI3.1month.rcl <-cut(annual.droughts.126.final.SPEI3_1month, c(-Inf,0,Inf) )
  annual.droughts.126.final.SPEI3.3months.rcl <-cut(annual.droughts.126.final.SPEI3, c(-Inf,0,Inf) )
  
  for (i in 1:50){
    start <-June[i]
    print(start)
    end<-Oct[i]
    
    drought_func <-function(x) { 
      with(rle(x[start:end] < x[601]), sum(lengths[values] >= 1))
    }
    
    drought <-st_apply(SPEI.Pe3.SSP585.VU.AI, 1:2, drought_func, PROGRESS=TRUE)
    
    annual.droughts.585 <-c(annual.droughts.585, drought)
  }
  annual.droughts.585.final.SPEI3_1month<-merge(annual.droughts.585)
 
  annual.droughts.585.final.SPEI3.1month.rcl <-cut(annual.droughts.585.final.SPEI3_1month, c(-Inf,0,Inf) )
  annual.droughts.585.final.SPEI3.3months.rcl <-cut(annual.droughts.585.final.SPEI3, c(-Inf,0,Inf) )
  
  
  for (i in 1:38){
    start <-June.hist[i]
    print(start)
    end<-Oct.hist[i]
    
    drought_func_hist <-function(x) { 
      with(rle(x[start:end] < x[457]), sum(lengths[values] >= 1))
    }
    
    drought <-st_apply(SPEI3.Pe3.fix.AI, 1:2, drought_func_hist, PROGRESS=TRUE)
    
    annual.droughts <-c(annual.droughts, drought)
  }
  annual.droughts.hist.final.SPEI3_1month<-merge(annual.droughts)
  
  annual.droughts.hist.final.SPEI3.1month.rcl <-cut(annual.droughts.hist.final.SPEI3_1month, c(-Inf,0,Inf) )
  annual.droughts.hist.final.SPEI3.3months.rcl <-cut(annual.droughts.hist.final.SPEI3, c(-Inf,0,Inf) )
  
  
  #export data
  
  name<-seq(1,600,12)
  name.list.126<-files.126[name]
  
  for (i in 2:51){
    output.file.126 <-str_replace(name.list.126[i-1],"Data/SSP126_IPSL_CM6A_LR/SSP126_IPSL_CM6A_LR/DroughtMaps_" ,"Results/Standardized_Indices/SPEI_Annual_Maps/3_months_126_rn/")
    output.file.126 <-str_replace(output.file.126, "01.nc",".tif")
    write_stars(annual.droughts.126.final.SPEI3.3months.rcl[,,,i], output.file.126, type= "UInt16")
  }
  
  name.list.585<-files.585[name]
  
  for (i in 2:51){
    output.file.585 <-str_replace(name.list.585[i-1],"Data/SSP585_IPSL_CM6A_LR/DroughtMaps_" ,"Results/Standardized_Indices/SPEI_Annual_Maps/3_months_585_rn/")
    output.file.585 <-str_replace(output.file.585, "01.nc",".tif")
    write_stars(annual.droughts.585.final.SPEI3.3months.rcl[,,,i], output.file.585)
  }
  
  name<-seq(1,456,12)
  name.list<-files[name]
  
  for (i in 2:39){
    output.file <-str_replace(name.list[i-1],"Data/HISTORICAL/HISTORICAL/DroughtMaps_" ,"Results/Standardized_Indices/SPEI_Annual_Maps/3_months_hist_rn/")
    output.file <-str_replace(output.file, "01.nc",".tif")
    write_stars(annual.droughts.hist.final.SPEI3.3months.rcl[,,,i], output.file, type= "UInt16")
  }
  
  ### calculate the mean number of events per year
  events.year.126 <-st_apply(annual.droughts.126.final.SPEI3.3months.rcl[,,,2:51], 1:2, function(x) (sum(x)/51)*100,PROGRESS=TRUE)
  events.year.585 <-st_apply(annual.droughts.585.final.SPEI3.3months.rcl[,,,2:51], 1:2, function(x) (sum(x)/51)*100,PROGRESS=TRUE)
  events.year.hist <-st_apply(annual.droughts.hist.final.SPEI3.3months.rcl[,,,2:39], 1:2, function(x) (sum(x)/39)*100,PROGRESS=TRUE)

  ### export annual probability maps
  write_stars(events.year.hist , "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/Standardized_Indices/Annual_Probability/3_months/SPEI3_p3months_drought.tif")
  write_stars(events.year.126 , "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/Standardized_Indices/Annual_Probability/3_months/SPEI3_126_p3months_drought.tif")
  write_stars(events.year.585 , "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/Standardized_Indices/Annual_Probability/3_months/SPEI3_585_p3months_drought.tif")
  
  
  ### calculate mean SPEI values per year
  
  June <-seq(6,600,12)
  July <-seq(7,600,12)
  Aug <-seq(8,600,12)
  Sep <-seq(9,600,12)
  Oct <-seq(10,600,12)
  season <-c(June,July,Aug,Sep,Oct)
  
  June.hist <-seq(6,456,12)
  July.hist <-seq(7,456,12)
  Aug.hist <-seq(8,456,12)
  Sep.hist <-seq(9,456,12)
  Oct.hist <-seq(10,456,12)
  season.hist <-c(June.hist,July.hist,Aug.hist,Sep.hist,Oct.hist)
  
  SPEI3.Pe3.fix.season<-SPEI3.Pe3.fix[,season.hist,,]
  SPEI3.Pe3.fix.mean <-st_apply(SPEI3.Pe3.fix.season, 2:3, mean,na.rm=TRUE, PROGRESS=TRUE)
  write_stars(SPEI3.Pe3.fix.mean, "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/Standardized_Indices/SPEI3_mean.tif")
  
  SPEI.Pe3.SSP126.VU.season<-SPEI.Pe3.SSP126.VU[,season,,]
  SPEI.Pe3.SSP126.VU.mean <-st_apply(SPEI.Pe3.SSP126.VU.season, 2:3, mean,na.rm=TRUE, PROGRESS=TRUE)
  write_stars(SPEI.Pe3.SSP126.VU.mean, "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/Standardized_Indices/SPEI3_SSP126_mean.tif")
  
  SPEI.Pe3.SSP585.VU.season<-SPEI.Pe3.SSP585.VU[,season,,]
  SPEI.Pe3.SSP585.VU.mean <-st_apply(SPEI.Pe3.SSP585.VU.season, 2:3, mean,na.rm=TRUE, PROGRESS=TRUE)
  write_stars(SPEI.Pe3.SSP585.VU.mean, "/Users/susanne.haas/OneDrive - World Food Programme/Documents/Countries/NIGER/UNDRR/Results/Standardized_Indices/SPEI3_SSP585_mean.tif")
  
  ### calculate base for WCI
  
  wci_base <-st_apply(nc_stars_ppet, 1:2, sum, PROGRESS=TRUE)
  wci_base.126 <-st_apply(nc_stars_ppet.126, 1:2, sum, PROGRESS=TRUE)
  wci_base.585 <-st_apply(nc_stars_ppet.585, 1:2, sum, PROGRESS=TRUE)
  
  
  }
  
