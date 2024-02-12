rainfall_analysis <- function(dir = "C:/Users/Susanne.Haas/Documents/RAIN_2023/NIGER") {
  
  library(readxl)
  library(dplyr)
  library(reshape)
  library(lattice)
  library(writexl)
  library(viridis)
  library(data.table)
  library(strex)
  library(ggplot2)
  library(tidyr)
  library(ggcorrplot)

  
  theme_set(theme_bw())
  
  zeroes_rfe2<-vector()
  obs_rfe2<-vector()
  pzero_rfe2<-vector()
  
  zeroes_arc2<-vector()
  obs_arc2<-vector()
  pzero_arc2<-vector()
  
  zeroes_rfe2_old<-vector()
  obs_rfe2_old<-vector()
  pzero_rfe2_old<-vector()
  
  zeroes_arc2_old<-vector()
  obs_arc2_old<-vector()
  pzero_arc2_old<-vector()

  median_rfe2<-vector()
  mean_rfe2<-vector()
  sd_rfe2<-vector()
  
  median_arc2<-vector()
  mean_arc2<-vector()
  sd_arc2<-vector()

  median_rfe2_old<-vector()
  mean_rfe2_old<-vector()
  sd_rfe2_old<-vector()
  
  median_arc2_old<-vector()
  mean_arc2_old<-vector()
  sd_arc2_old<-vector()
  
  qlist<-list()
  data<-vector()
  
  rfe2_arc2<-vector()
  rfe2_chirp2<-vector()
  rfe2_chirps2<-vector()
  rfe2_tamsat <-vector()
  
  arc2_old_chirp2<-vector()
  arc2_old_chirps2<-vector()
  arc2_old_tamsat<-vector()
  
  chirp2_chirps2<-vector()
  chirp2_tamsat<-vector()
  chirps2_tamsat<-vector()
  
  rfe2_old_arc2_old<-vector()
  rfe2_old_chirp2<-vector()
  rfe2_old_chirps2<-vector()
  rfe2_old_tamsat<-vector()
  
  arc2_chirp2 <-vector()
  arc2_chirps2 <-vector()
  arc2_tamsat <-vector()
  
  ###setwd(dir)
  
  ### read data 
  
  RFE2_Pixels <- read_excel("RFE2_Pixels.xlsx")
  ARC2_Pixels <- read_excel("ARC2_Pixels.xlsx")
  CHIRP2_Pixels <- read_excel("CHIRP2_Pixels.xlsx")
  CHIRPS2_Pixels <- read_excel("CHIRPS2_Pixels.xlsx")
  TAMSAT_Pixels <- read_excel("TAMSAT_Pixels.xlsx")
  RFE2_Pixels_Old <- read_excel("RFE2_Pixels_Old.xlsx")
  ARC2_Pixels_Old <- read_excel("ARC2_Pixels_Old.xlsx")
  
  RFE2_National<- read_excel("RFE2_National.xlsx")
  RFE2_National_Old<- read_excel("RFE2_National_Old.xlsx")
  ARC2_National<- read_excel("ARC2_National.xlsx")
  ARC2_National_Old <- read_excel("ARC2_National_Old.xlsx")
  
  ### reformat data
  
  ## remove unnecessary columns
  
  CHIRP2_Pixels <-subset(CHIRP2_Pixels, select= -c(LonLatLookup, Lat, Lon))
  CHIRPS2_Pixels <-subset(CHIRPS2_Pixels, select= -c(LonLatLookup, Lat, Lon))
  TAMSAT_Pixels <-subset(TAMSAT_Pixels, select= -c(LonLatLookup, Lat, Lon))
  ARC2_Pixels <-subset(ARC2_Pixels, select= -c(LonLatLookup, Lat, Lon))
  RFE2_Pixels <-subset(RFE2_Pixels, select= -c(LonLatLookup, Lat, Lon))
  ARC2_Pixels_Old <-subset(ARC2_Pixels_Old, select= -c(LonLatLookup, Lat, Lon))
  RFE2_Pixels_Old <-subset(RFE2_Pixels_Old, select= -c(LonLatLookup, Lat, Lon))
  
  RFE2_National <-subset(RFE2_National, select= -c(ADM0_CODE,ADM0_NAME,CONTINENT, REGION,Shape_Leng, Shape_Area, LAST_UPDAT))
  RFE2_National_Old <-subset(RFE2_National_Old, select= -c(ADM0_CODE,ADM0_NAME,CONTINENT, REGION,Shape_Leng, Shape_Area, LAST_UPDAT))
  ARC2_National <-subset(ARC2_National, select= -c(ADM0_CODE,ADM0_NAME,CONTINENT, REGION,Shape_Leng, Shape_Area, LAST_UPDAT))
  ARC2_National_Old <-subset(ARC2_National_Old, select= -c(ADM0_CODE,ADM0_NAME,CONTINENT, REGION,Shape_Leng, Shape_Area, LAST_UPDAT))
  
  ## transpose data
  
  t_CHIRP2 <- transpose(CHIRP2_Pixels)
  rownames(t_CHIRP2) <- colnames(CHIRP2_Pixels)
  Year <-str_first_number(rownames(t_CHIRP2))
  Dek <-str_nth_number(rownames(t_CHIRP2), n=2)
  Season <-Year
  CHIRP2_complete <-cbind(Year,Dek,Season,t_CHIRP2)
  
  t_CHIRPS2 <- transpose(CHIRPS2_Pixels)
  rownames(t_CHIRPS2) <- colnames(CHIRPS2_Pixels)
  Year <-str_first_number(rownames(t_CHIRPS2))
  Dek <-str_nth_number(rownames(t_CHIRPS2), n=2)
  Season <-Year
  CHIRPS2_complete <-cbind(Year,Dek,Season,t_CHIRPS2)
  
  t_TAMSAT <- transpose(TAMSAT_Pixels)
  rownames(t_TAMSAT) <- colnames(TAMSAT_Pixels)
  Year <-str_first_number(rownames(t_TAMSAT))
  Dek <-str_nth_number(rownames(t_TAMSAT), n=2)
  Season <-Year
  TAMSAT_complete <-cbind(Year,Dek,Season,t_TAMSAT)
  
  t_ARC2 <- transpose(ARC2_Pixels)
  rownames(t_ARC2) <- colnames(ARC2_Pixels)
  Year <-str_first_number(rownames(t_ARC2))
  Dek <-str_nth_number(rownames(t_ARC2), n=2)
  Season <-Year
  ARC2_complete <-cbind(Year,Dek,Season,t_ARC2)
  
  t_RFE2 <- transpose(RFE2_Pixels)
  rownames(t_RFE2) <- colnames(RFE2_Pixels)
  Year <-str_first_number(rownames(t_RFE2))
  Dek <-str_nth_number(rownames(t_RFE2), n=2)
  Season <-Year
  RFE2_complete <-cbind(Year,Dek,Season,t_RFE2)
  
  t_ARC2_old <- transpose(ARC2_Pixels_Old)
  rownames(t_ARC2_old) <- colnames(ARC2_Pixels_Old)
  Year <-str_first_number(rownames(t_ARC2_old))
  Dek <-str_nth_number(rownames(t_ARC2_old), n=2)
  Season <-Year
  ARC2_complete_old <-cbind(Year,Dek,Season,t_ARC2_old)
  
  t_RFE2_old <- transpose(RFE2_Pixels_Old)
  rownames(t_RFE2_old) <- colnames(RFE2_Pixels_Old)
  Year <-str_first_number(rownames(t_RFE2_old))
  Dek <-str_nth_number(rownames(t_RFE2_old), n=2)
  Season <-Year
  RFE2_complete_old <-cbind(Year,Dek,Season,t_RFE2_old)
  
  t_RFE2_National <- transpose(RFE2_National)
  rownames(t_RFE2_National) <- colnames(RFE2_National)
  Year <-str_first_number(rownames(t_RFE2_National))
  Dek <-str_nth_number(rownames(t_RFE2_National), n=2)
  Day<-ifelse(Dek %% 3==1,1,ifelse(Dek%%3==2,11,21))
  Month <-ceiling(Dek/3)
  Label <-paste0(Year,"-",Month,"-",Day)
  Date <-as.Date(Label,format="%Y-%m-%d")
  t_RFE2_National$dset <- c(rep("reprocessed", length( t_RFE2_National)))
  RFE2_National_complete <-cbind(Date,t_RFE2_National)
  
  t_RFE2_National_Old <- transpose(RFE2_National_Old)
  rownames(t_RFE2_National_Old) <- colnames(RFE2_National_Old)
  Year <-str_first_number(rownames(t_RFE2_National_Old))
  Dek <-str_nth_number(rownames(t_RFE2_National_Old), n=2)
  Day<-ifelse(Dek %% 3==1,1,ifelse(Dek%%3==2,11,21))
  Month <-ceiling(Dek/3)
  Label <-paste0(Year,"-",Month,"-",Day)
  Date <-as.Date(Label,format="%Y-%m-%d")
  t_RFE2_National_Old$dset <- c(rep("erroneous", length(t_RFE2_National_Old)))
  RFE2_National_complete_old <-cbind(Date,t_RFE2_National_Old)
  

  t_ARC2_National <- transpose(ARC2_National)
  rownames(t_ARC2_National) <- colnames(ARC2_National)
  Year <-str_first_number(rownames(t_ARC2_National))
  Dek <-str_nth_number(rownames(t_ARC2_National), n=2)
  Day<-ifelse(Dek %% 3==1,1,ifelse(Dek%%3==2,11,21))
  Month <-ceiling(Dek/3)
  Label <-paste0(Year,"-",Month,"-",Day)
  Date <-as.Date(Label,format="%Y-%m-%d")
  t_ARC2_National$dset <- c(rep("reprocessed", length( t_ARC2_National)))
  ARC2_National_complete <-cbind(Date,t_ARC2_National)
  
  
  t_ARC2_National_Old <- transpose(ARC2_National_Old)
  rownames(t_ARC2_National_Old) <- colnames(ARC2_National_Old)
  Year <-str_first_number(rownames(t_ARC2_National_Old))
  Dek <-str_nth_number(rownames(t_ARC2_National_Old), n=2)
  Day<-ifelse(Dek %% 3==1,1,ifelse(Dek%%3==2,11,21))
  Month <-ceiling(Dek/3)
  Label <-paste0(Year,"-",Month,"-",Day)
  Date <-as.Date(Label,format="%Y-%m-%d")
  t_ARC2_National_Old$dset <- c(rep("erroneous", length( t_ARC2_National_Old)))
  ARC2_National_complete_old <-cbind(Date,t_ARC2_National_Old)
  
  
  RFE2_datasets_National<-rbind(RFE2_National_complete,RFE2_National_complete_old)
  ARC2_datasets_National<-rbind(ARC2_National_complete,ARC2_National_complete_old)

  
  ## extract relevant data for agricultural season
  
  rfe2 <-filter(RFE2_complete, Dek>12& Dek< 31 & Year<2023)
  arc2 <-filter(ARC2_complete, Dek>12& Dek< 31 & Year<2023)
  chirp2 <-filter(CHIRP2_complete, Dek>12 & Dek< 31 & Year<2023 )
  chirps2 <-filter(CHIRPS2_complete, Dek>12 & Dek< 31 & Year<2023)
  tamsat <-filter(TAMSAT_complete, Dek>12 & Dek< 31 & Year<2023)
  rfe2_old <-filter(RFE2_complete_old, Dek>12& Dek< 31 & Year<2023)
  arc2_old <-filter(ARC2_complete_old, Dek>12& Dek< 31 & Year<2023)
  
  ## extract relevant data for sowing window
  
  rfe2_sw <-filter(RFE2_complete, Dek>12& Dek< 20 & Year<2023)
  arc2_sw <-filter(ARC2_complete, Dek>12& Dek< 20 & Year<2023)
  rfe2_old_sw <-filter(RFE2_complete_old, Dek>12& Dek< 20 & Year<2023)
  arc2_old_sw <-filter(ARC2_complete_old, Dek>12& Dek< 20 & Year<2023)
  
  ## add column to identify reprocessed and erroneous dataset
  
  rfe2$dset <- c(rep("reprocessed", length(rfe2$Dek)))
  arc2$dset <- c(rep("reprocessed", length(arc2$Dek)))
  rfe2_old$dset <- c(rep("erroneous", length(rfe2_old$Dek)))
  arc2_old$dset <- c(rep("erroneous", length(arc2_old$Dek)))

  rfe2_sw$dset <- c(rep("reprocessed", length(rfe2_sw$Dek)))
  arc2_sw$dset <- c(rep("reprocessed", length(arc2_sw$Dek)))
  rfe2_old_sw$dset <- c(rep("erroneous", length(rfe2_old_sw$Dek)))
  arc2_old_sw$dset <- c(rep("erroneous", length(arc2_old_sw$Dek)))
  
  ## melt the imported data
  
  rfe2_shape <-melt(rfe2,id.vars = c("Year","Dek","Season","dset"))
  arc2_shape <-melt(arc2,id.vars = c("Year","Dek","Season", "dset"))
  chirp2_shape <-melt(chirp2,id.vars = c("Year","Dek","Season"))
  chirps2_shape <-melt(chirps2,id.vars = c("Year","Dek","Season"))
  tamsat_shape <-melt(tamsat,id.vars = c("Year","Dek","Season"))
  rfe2_shape_old <-melt(rfe2_old,id.vars = c("Year","Dek","Season", "dset"))
  arc2_shape_old <-melt(arc2_old,id.vars = c("Year","Dek","Season","dset"))
  
  rfe2_shape_sw <-melt(rfe2_sw,id.vars = c("Year","Dek","Season","dset"))
  arc2_shape_sw <-melt(arc2_sw,id.vars = c("Year","Dek","Season", "dset"))
  rfe2_shape_old_sw <-melt(rfe2_old_sw,id.vars = c("Year","Dek","Season", "dset"))
  arc2_shape_old_sw <-melt(arc2_old_sw,id.vars = c("Year","Dek","Season","dset"))
   
  ## combine the erroneous and reprocessed ARC2/RFE2 datasets
  
  rfe2_shape_merge <-rbind(rfe2_shape, rfe2_shape_old)
  arc2_shape_merge <-rbind(arc2_shape, arc2_shape_old)
  
  ## convert Season to FACTOR in order to facilitate the creation of related plots
  
  rfe2_shape_merge$Season <- as.factor(rfe2_shape_merge$Season)
  arc2_shape_merge$Season <- as.factor(arc2_shape_merge$Season)
  
  ## create vector that includes the different seasons
  
  seasons <- unique(rfe2_shape$Season)
  
  ### create line graphs at admin level 0 
  
  RFE2_datasets_National$dset<-as.factor(RFE2_datasets_National$dset)
  
  ggplot(RFE2_datasets_National, aes(x=Date, y=V1)) + geom_line(aes(color = dset)) +  scale_color_manual(values= c("indianred2", "cyan4"), name="",labels=c("Discarded Dataset", "Reprocessed Dataset")) + labs(title="RFE2 dekadal rainfall amounts (2001-2023)", x="", y="Rainfall amounts (mm)") + theme(legend.position="bottom", axis.title.y= element_text(size = 13), legend.text = element_text(size = 13),text=element_text(size=13))
  ggsave("RFE2_National_Comparison.png", width=12, height=7.5, units=c("in"))
  
  ARC2_datasets_National$dset<-as.factor(ARC2_datasets_National$dset)
  
  ggplot(ARC2_datasets_National, aes(x=Date, y=V1)) + geom_line(aes(color = dset)) + scale_color_manual(values= c("indianred2", "cyan4"), name="",labels=c("Discarded Dataset", "Reprocessed Dataset"))  + labs(title="ARC2 dekadal rainfall amounts (2001-2023)", x="",y="Rainfall amounts (mm)") + theme(legend.position="bottom", axis.title.y = element_text(size = 13), legend.text= element_text(size = 13), text=element_text(size=13))
  ggsave("ARC2_National_Comparison.png", width=12, height=7.5, units=c("in"))
  
  ### create line graphs at admin level 0 for the period of divergence
  
  
  ggplot(RFE2_datasets_National, aes(x=Date, y=V1)) + geom_line(aes(color = dset)) +  scale_color_manual(values= c("indianred2", "cyan4"), name="",labels=c("Discarded Dataset", "Reprocessed Dataset")) + labs(title="RFE2 dekadal rainfall amounts (2020-2023)", x="", y="Rainfall amounts (mm)") + theme(legend.position="bottom", axis.title.y= element_text(size = 13), text=element_text(size=13), legend.text = element_text(size = 13)) + scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2023-05-01")))
  ggsave("RFE2_National_Comparison_divergence.png", width=12, height=7.5, units=c("in"))
  
  ggplot(ARC2_datasets_National, aes(x=Date, y=V1)) + geom_line(aes(color = dset)) +  scale_color_manual(values= c("indianred2", "cyan4"), name="",labels=c("Discarded Dataset", "Reprocessed Dataset")) + labs(title="ARC2 dekadal rainfall amounts (2020-2023)", x="", y="Rainfall amounts (mm)") + theme(legend.position="bottom", axis.title.y= element_text(size = 13), text=element_text(size=13), legend.text = element_text(size = 13)) + scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2023-05-01")))
  ggsave("ARC2_National_Comparison_divergence.png", width=12, height=7.5, units=c("in"))
  
  
  ### create and print boxplots
  
  ggplot(rfe2_shape_merge, aes(x=Season, y=value, fill=dset)) + geom_boxplot() +  labs(title="RFE2 rainfall amounts", x="", y="Rainfall amount (mm)") + theme(text=element_text(size=13), axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), legend.text = element_text(size = 13), legend.position="bottom",legend.title = element_blank())+ scale_fill_discrete(labels= c("Discarded Dataset","Reprocessed Dataset")) 
  ggsave("RFE2_combined.png", width=12, height=7.5, units=c("in"))
  
  ggplot(arc2_shape_merge, aes(x=Season, y=value, fill=dset)) + geom_boxplot() +  labs(title="ARC2 rainfall amounts", x="", y="Rainfall amount (mm)") + theme(text=element_text(size=13), axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), legend.text = element_text(size = 13), legend.position="bottom",legend.title = element_blank())+ scale_fill_discrete(labels= c("Discarded Dataset","Reprocessed Dataset")) 
  ggsave("ARC2_combined.png", width=12, height=7.5, units=c("in"))
  
  
  ### calculate and print percentage of zeroes
  
  for (i in 1:22){
    zeroes_rfe2[i] <-nrow(filter(rfe2_shape, Season == seasons[i] & value==0))
    obs_rfe2[i]<-nrow(filter(rfe2_shape, Season == seasons[i]))
    pzero_rfe2[i]<-zeroes_rfe2[i]/obs_rfe2[i]
    
    zeroes_arc2[i] <-nrow(filter(arc2_shape, Season == seasons[i] & value==0))
    obs_arc2[i]<-nrow(filter(arc2_shape, Season == seasons[i]))
    pzero_arc2[i]<-zeroes_arc2[i]/obs_arc2[i]
    
    zeroes_rfe2_old[i] <-nrow(filter(rfe2_shape_old, Season == seasons[i] & value==0))
    obs_rfe2_old[i]<-nrow(filter(rfe2_shape_old, Season == seasons[i]))
    pzero_rfe2_old[i]<-zeroes_rfe2_old[i]/obs_rfe2_old[i]
    
    zeroes_arc2_old[i] <-nrow(filter(arc2_shape_old, Season == seasons[i] & value==0))
    obs_arc2_old[i]<-nrow(filter(arc2_shape_old, Season == seasons[i]))
    pzero_arc2_old[i]<-zeroes_arc2_old[i]/obs_arc2_old[i]
  }
  
  pzero_df <-data.frame(seasons, pzero_rfe2_old, pzero_rfe2, pzero_arc2_old, pzero_arc2)
  colnames(pzero_df)<-c("Season","RFE2 Discarded","RFE2 Reprocessed","ARC2 Discarded","ARC2 Reprocessed")
  write_xlsx(pzero_df,"Percentage_zeros.xlsx")
  

  ### calculate descriptive statistics
  
  for (i in 1:22){
    data<-filter(rfe2_shape, Season == seasons[i])
    median_rfe2[i] <-median(data$value)
    mean_rfe2[i] <-mean(data$value)
    sd_rfe2[i] <-sd(data$value)
  
    data<-filter(arc2_shape, Season == seasons[i])
    median_arc2[i] <-median(data$value)
    mean_arc2[i] <-mean(data$value)
    sd_arc2[i] <-sd(data$value)
    
    data<-filter(rfe2_shape_old, Season == seasons[i])
    median_rfe2_old[i] <-median(data$value)
    mean_rfe2_old[i] <-mean(data$value)
    sd_rfe2_old[i] <-sd(data$value)
    
    data<-filter(arc2_shape_old, Season == seasons[i])
    median_arc2_old[i] <-median(data$value)
    mean_arc2_old[i] <-mean(data$value)
    sd_arc2_old[i] <-sd(data$value)
    
  }
  
  stats_df_rfe <-data.frame(seasons, median_rfe2, mean_rfe2, sd_rfe2, median_rfe2_old, mean_rfe2_old, sd_rfe2_old)
  colnames(stats_df_rfe)<-c("Season","RFE2 Median","RFE2 Mean","RFE2 Std", "RFE2* Median","RFE2* Mean","RFE2* Std")
  write_xlsx(stats_df_rfe,"DStats_RFE2.xlsx")
  
  stats_df_arc <-data.frame(seasons, median_arc2, mean_arc2, sd_arc2, median_arc2_old, mean_arc2_old, sd_arc2_old)
  colnames(stats_df_arc)<-c("Season","ARC2* Median","ARC2* Mean","ARC2* Std", "ARC2** Median","ARC2** Mean","ARC2**Std")
  write_xlsx(stats_df_arc, "DStats_ARC2.xlsx")
  
  stats_df_rfe <-data.frame(seasons, median_rfe2, mean_rfe2, sd_rfe2, median_rfe2_old, mean_rfe2_old, sd_rfe2_old, median_arc2, mean_arc2, sd_arc2, median_arc2_old, mean_arc2_old, sd_arc2_old)
  colnames(stats_df_rfe)<-c("Season","RFE2 Median","RFE2 Mean","RFE2 Std", "RFE2* Median","RFE2* Mean","RFE2* Std","ARC2 Median","ARC2 Mean","ARC2 Std", "ARC2* Median","ARC2* Mean","ARC2*Std")
  write_xlsx(stats_df_rfe,"DStats.xlsx")
  
  ### calculate and print quantile plots for agricultural season
  
  colour <-mako(20)

  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
 
  for (i in 1:22){
    data<-filter(rfe2_shape, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="RFE2 rainfall amounts: reprocessed dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("RFE2_Quantiles.png", width=9, height=5.67, units=c("in"))
  
  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(rfe2_shape_old, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="RFE2 rainfall amounts: discarded dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("RFE2_Quantiles_old.png", width=9, height=5.67, units=c("in"))

  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(arc2_shape, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="ARC2 rainfall amounts: reprocessed dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("ARC2_Quantiles.png", width=9, height=5.67, units=c("in"))
  
  
  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(arc2_shape_old, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="ARC2 rainfall amounts: discarded dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("ARC2_Quantiles_old.png", width=9, height=5.67, units=c("in"))
  
  ### calculate and print quantile plots for sowing window
  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(rfe2_shape_sw, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="RFE2 rainfall amounts during the sowing period: reprocessed dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("RFE2_Quantiles_SW.png", width=9, height=5.67, units=c("in"))
  
  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(rfe2_shape_old_sw, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="RFE2 rainfall amounts during the sowing period: discarded dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("RFE2_Quantiles_old_SW.png", width=9, height=5.67, units=c("in"))
  
  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(arc2_shape_sw, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="ARC2 rainfall amounts during the sowing period: reprocessed dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("ARC2_Quantiles_SW.png", width=9, height=5.67, units=c("in"))
  
  
  quantile_df <-data.frame(matrix(ncol =length(seasons), nrow = length(seq(0,1,by=0.001))))
  colnames(quantile_df)<-seasons
  quantile_df$seq <-seq(0, 1, by=.001)
  
  for (i in 1:22){
    data<-filter(arc2_shape_old_sw, Season == seasons[i])
    quantile_df[,i] <-quantile(data$value,seq(0, 1, by=.001))
  }
  
  g_quantile_df <-gather(quantile_df, key = "Season", value = "value", -seq)
  
  ggplot(g_quantile_df, aes(x=seq, y=value)) + geom_line(aes(color = Season, linetype= Season)) + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="ARC2 rainfall amounts during the sowing period: discarded dataset", x="Quantiles", y="Rainfall amount (mm)") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("ARC2_Quantiles_old_SW.png", width=9, height=5.67, units=c("in"))
  
  ### calculate and print empirical distribution function
  
  rfe2_shape_factor<-rfe2_shape
  arc2_shape_factor<-arc2_shape
  rfe2_shape_old_factor<-rfe2_shape_old
  arc2_shape_old_factor<-arc2_shape_old
  
  rfe2_shape_factor$Season <-as.factor(rfe2_shape$Season)
  arc2_shape_factor$Season <-as.factor(arc2_shape$Season)
  rfe2_shape_old_factor$Season <-as.factor(rfe2_shape_old$Season)
  arc2_shape_old_factor$Season <-as.factor(arc2_shape_old$Season)
  
  ggplot(rfe2_shape_factor, aes(x=value, color=Season, linetype=Season)) +  stat_ecdf() + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="Empirical cumulative distribution for RFE2 rainfall amounts: reprocessed dataset", x="Rainfall amount (mm)", y="Percent") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("RFE2_ECDF.png", width=9, height=5.67, units=c("in"))
  
  ggplot(rfe2_shape_old_factor, aes(x=value, color=Season, linetype=Season)) +  stat_ecdf() + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="Empirical cumulative distribution for RFE2 rainfall amounts: discarded dataset", x="Rainfall amount (mm)", y="Percent") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("RFE2_ECDF_old.png", width=9, height=5.67, units=c("in"))
  
  ggplot(arc2_shape_factor, aes(x=value, color=Season, linetype=Season)) +  stat_ecdf() + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="Empirical cumulative distribution for ARC2 rainfall amounts: reprocessed dataset", x="Rainfall amount (mm)", y="Percent") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("ARC2_ECDF.png", width=9, height=5.67, units=c("in"))
  
  ggplot(arc2_shape_old_factor, aes(x=value, color=Season, linetype=Season)) +  stat_ecdf() + scale_colour_manual(values = c(colour[1:19], "red", "red2", "red3"))+ scale_linetype_manual(values = c(rep(1,times=19),2,2,2))+ labs(title="Empirical cumulative distribution for ARC2 rainfall amounts: discarded dataset", x="Rainfall amount (mm)", y="Percent") + theme(axis.text = element_text(size = 13), axis.title.y = element_text(size = 13), axis.title.x = element_text(size =13),legend.title=element_text(size=13),legend.text = element_text(size = 13))
  ggsave("ARC2_ECDF_old.png", width=9, height=5.67, units=c("in"))
  
  ### Calculate correlations between datasets
  
  for (i in 1:22){
  rfe2_s<-filter(rfe2_shape, Season == seasons[i])
  arc2_s<-filter(arc2_shape, Season == seasons[i])
  chirp2_s<-filter(chirp2_shape, Season == seasons[i])
  chirps2_s<-filter(chirps2_shape, Season == seasons[i])
  tamsat_s<-filter(tamsat_shape, Season == seasons[i])
  rfe2_s_old<-filter(rfe2_shape_old, Season == seasons[i])
  arc2_s_old<-filter(arc2_shape_old, Season == seasons[i])
  
  rfe2_arc2[i]<-cor(rfe2_s$value, arc2_s$value)
  rfe2_chirp2[i]<-cor(rfe2_s$value, chirp2_s$value)
  rfe2_chirps2[i]<-cor(rfe2_s$value, chirps2_s$value)
  rfe2_tamsat[i]<-cor(rfe2_s$value, tamsat_s$value)
  
  rfe2_old_arc2_old[i]<-cor(rfe2_s_old$value, arc2_s_old$value)
  rfe2_old_chirp2[i]<-cor(rfe2_s_old$value, chirp2_s$value)
  rfe2_old_chirps2[i]<-cor(rfe2_s_old$value, chirps2_s$value)
  rfe2_old_tamsat[i]<-cor(rfe2_s_old$value, tamsat_s$value)
  
  arc2_chirp2[i]<-cor(arc2_s$value, chirp2_s$value)
  arc2_chirps2[i]<-cor(arc2_s$value, chirps2_s$value)
  arc2_tamsat[i]<-cor(arc2_s$value, tamsat_s$value)
  
  arc2_old_chirp2[i]<-cor(arc2_s$value, chirp2_s$value)
  arc2_old_chirps2[i]<-cor(arc2_s$value, chirps2_s$value)
  arc2_old_tamsat[i]<-cor(arc2_s$value, tamsat_s$value)
  
  chirp2_chirps2[i]<-cor(chirp2_s$value, chirps2_s$value)
  chirp2_tamsat[i]<-cor(chirp2_s$value, tamsat_s$value)
  
  chirps2_tamsat[i]<-cor(chirps2_s$value, tamsat_s$value)
  }
  cor_df_reproduced<-data.frame(seasons, rfe2_arc2, rfe2_chirp2, rfe2_chirps2, rfe2_tamsat, arc2_chirp2, arc2_chirps2, arc2_tamsat)
  colnames(cor_df_reproduced)<-c("Season", "RFE2-ARC2","RFE2-CHIRP2","RFE2-CHIRPS2","RFE2-TAMSAT","ARC2-CHIRP2","ARC2-CHIRPS2", "ARC2-TAMSAT")
  
  cor_df_erroneous<-data.frame(seasons, rfe2_old_arc2_old, rfe2_old_chirp2, rfe2_old_chirps2, rfe2_old_tamsat, arc2_old_chirp2, arc2_old_chirps2, arc2_old_tamsat)
  colnames(cor_df_erroneous)<-c("Season", "RFE2-ARC2","RFE2-CHIRP2","RFE2-CHIRPS2","RFE2-TAMSAT","ARC2-CHIRP2","ARC2-CHIRPS2", "ARC2-TAMSAT")
  
  cor_df <-data.frame(seasons, rfe2_arc2, rfe2_chirp2, rfe2_chirps2, rfe2_tamsat, arc2_chirp2, arc2_chirps2, arc2_tamsat, rfe2_old_arc2_old, rfe2_old_chirp2, rfe2_old_chirps2, rfe2_old_tamsat, arc2_old_chirp2, arc2_old_chirps2, arc2_old_tamsat)
  colnames(cor_df)<-c("Season", "RFE2-ARC2","RFE2-CHIRP2","RFE2-CHIRPS2","RFE2-TAMSAT","ARC2-CHIRP2","ARC2-CHIRPS2", "ARC2-TAMSAT", "RFE2*-ARC2*","RFE2*-CHIRP2","RFE2*-CHIRPS2","RFE2*-TAMSAT","ARC2*-CHIRP2","ARC2*-CHIRPS2", "ARC2*-TAMSAT")
  
  write_xlsx(cor_df, "Correlations.xlsx")
 
}
  
  