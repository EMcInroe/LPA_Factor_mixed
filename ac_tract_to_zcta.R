library(data.table)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(plyr)
library(ggmap)
library(zipcodeR)
library(tigris)
library(sf)
library(viridis)

## import data sets
ac_data<-fread("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/dataverse_files/US_metro_ac_prob-1.csv", header=TRUE)
data_set<-fread("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/zip_cbsa_an2.csv",header=TRUE)
## import relationship file from US Census
tract_to_zcta<-fread("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/tab20_zcta520_tract20_natl.txt",sep="|")

## Join ac data with relationship file
full_df<-left_join(ac_data,tract_to_zcta, by=c("TRACT_GEOID"="GEOID_TRACT_20"),multiple = "all")
## omit observations with no ac data
no_NA_df<-na.omit(full_df)

hist(no_NA_df$ac_prob)
summary(no_NA_df$ac_prob)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1457  0.9163  0.9732  0.9249  0.9910  0.9989 

## group census tracts by ZCTAs, mean of tract data used for ZCTA data 
ZCTA_df<-no_NA_df %>% group_by(GEOID_ZCTA5_20) %>% summarise(ac_prob = mean(ac_prob), cbsa_perc_rank_ac_prob=mean(cbsa_perc_rank_ac_prob))

colnames(ZCTA_df)[1]<-"ZCTA"

## join ac_data to rest of data set
new_data_set<-left_join(data_set, ZCTA_df,by = "ZCTA")
hist(new_data_set$ac_prob)
boxplot(new_data_set$ac_prob)
summary(new_data_set$ac_prob)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.2269  0.9324  0.9754  0.9339  0.9916  0.9986     506 
plot(new_data_set$ac_prob~new_data_set$ann_an_rate100k)
# Distribution does not appear to change when the data is aggregated by ZCTA. 


#Investigate missing values
na_data_set<-subset(new_data_set, is.na(ac_prob))     ## 506 observations with no ac data
na_data_set$ZIP<-as.character(na_data_set$ZIP)
na_data_set$ZIP[1:61]<-sapply(na_data_set$ZIP[1:61], FUN = function(x) paste0("0",x))
na_CBSA<-unique(na_data_set$CBSA)                     ## those observations are found in 80 different CBSAs


zip<-as.character(unique(na_data_set$ZIP))
zip[1:61]<-sapply(zip[1:61], FUN = function(x) paste0("0",x))
zipcode<-reverse_zipcode(zip)[,c(1,7,8,9,13)]
#population size included, cannot find source of the data. Census? https://zipcoder.39n.io/articles/zipcodeR.html

df_zip<-left_join(na_data_set, zipcode, by = c("ZIP" = "zipcode"))

write.csv(df_zip,"C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/na_zip_ac.csv")

## Map of zipcodes that are missing ac data
states<-states(cb=TRUE)

max(df_zip$lat)
min(df_zip$lat)
max(df_zip$lng)
min(df_zip$lng)

my_breaks<-c(2000, 5000, 10000, 20000)
ggplot(states) + geom_sf() + theme_void() + 
  geom_point(data = df_zip, mapping = aes(x=lng, y=lat, color = population)) + 
  coord_sf(xlim=c(-125, -65), ylim = c(20,50)) +
  scale_color_gradientn(colours=rev(magma(6)),
                        trans = "log",
                        breaks = my_breaks, labels = my_breaks)

ggplot(states) + geom_sf() + theme_void() + 
  geom_point(data = df_zip, mapping = aes(x=lng, y=lat, color = population)) + 
  coord_sf(xlim=c(-125, -65), ylim = c(20,50)) +
  scale_color_gradientn(colours=rev(turbo(6)))


ggsave("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/na_zip_ac_map.png")

#library(tidycensus)
#library(mapview)


##### Check Crosswalk ###########
## Get shapefiles for ZCTA and census tract for small area. Check the boundaries
#similar to following example
#all_zctas = get_acs(
#  geography = "zcta",
#  variables = "B19013_001",
#  year      = 2021,
#  geometry  = TRUE)

#filtered_zctas = filter(all_zctas, GEOID %in% get_zctas_by_county(6075))

#mapview(filtered_zctas, zcol = "estimate")