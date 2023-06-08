library(tidycensus)
library(dplyr)
library(purrr)

zz_rr<-readRDS("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/zip_rr_an_results.RDS")
View(zz_rr)

## create data frame with zipcode, CBSA, and the outcome variable ann_an_rate100K
zz_rr_zip<-zz_rr[,c(1:3,18)]
rm(zz_rr)

#Use this API key when pulling Census/ACS data for the first time. No need to run it again after the first time
# census_api_key("e7333da19a029d8d06fa158a1029da0b4dbc767b", install=TRUE,overwrite=T)
# readRenviron("~/.Renviron")

v17<-load_variables(2017,"acs5")
# Available 5yr ACS 2005-2009, 2006-2010, 2007-2011, 2008-2012, 2009-2013, 2010-2014, 2011-2015, 2012-2016, 2013-2017, 2014-2018, 2015-2019

y = 2017 
y2 = 2012
y3 = 2009

###################################################################################################
################## ASC 5 year Estimates Unless otherwise specified ################################
###################################################################################################

#population >65
total_Pop_all <- "B01001_001" # Total Population (Male + Female)
total_Pop_65vars <- paste0("B01001_0",c(20:25,44:49)) # Total Population over 65 (Male + Female)

total_pop<-get_acs(geography = "zcta",total_Pop_all,year=y)[,c("GEOID","estimate")] %>%
  rename(total_pop=estimate)
total_pop_y2<-get_acs(geography = "zcta",total_Pop_all,year=y2)[,c("GEOID","estimate")] %>%
  rename(total_pop=estimate)

total_pop_y2$GEOID<-substring(total_pop_y2$GEOID,3)

total_pop_65 <- get_acs(geography = "zcta",total_Pop_65vars,year=y) %>%
  group_by(GEOID) %>% summarise(total_pop_65=sum(estimate,na.rm=T))

total_pop_65_y2 <- get_acs(geography = "zcta",total_Pop_65vars,year=y2) %>%
  group_by(GEOID) %>% summarise(total_pop_65=sum(estimate,na.rm=T))

total_pop_65_y2$GEOID<-substring(total_pop_65_y2$GEOID,3)

pop_density<-get_acs(geography = "zcta", "B01003_001",year=y)[c("GEOID","estimate")] %>%
  rename(pop_density = estimate)

############################Living Alone ################################
#########################################################################

#Tenure status (owner vs renter)
ownhome65_vars <- c(paste0("B25007_00",c(9)),paste0("B25007_0",c(10:11))) # Number >= 65 years that are home owners
ownhome_vars <- "B25007_002" # Total number of home owners
renter65_vars <- paste0("B25007_0",c(19:21)) # Number >= 65 years that are renters
renter_vars <- "B25007_012" # Total number of renters

owner <- get_acs(geography = "zcta",ownhome_vars,year=y) %>%
  group_by(GEOID) %>% summarise(owner=sum(estimate,na.rm=T))
renter <- get_acs(geography = "zcta",renter_vars,year=y) %>%
  group_by(GEOID) %>% summarise(renter=sum(estimate,na.rm=T))

#Live alone
live_alone_owner65_vars <- "B25011_021" # Number >= 65 years, home owners, that live alone.
live_alone_owner_vars <- "B25011_018" # Total, homeowners living alone
not_alone_owner65_vars <- "B25011_025" # Number >= 65 years, home owners, that do NOT live alone.
not_alone_owner_vars <- "B25011_022" # Number >= total home owners, that do NOT live alone.
live_alone_renter65_vars <- "B25011_045" # Number >= 65 years, renters, that live alone.
live_alone_renter_vars <- "B25011_042" # Number >= total,renters living alone
not_alone_renter65_vars <- "B25011_049" # Number >= 65 years, renters, that do NOT live alone.
not_alone_renter_vars <- "B25011_046" # Number >= total renters, that do NOT live alone.
live_alone_tot65_vars<-paste0("B25011_0",c(21,45)) # Number >= 65 years that live alone (owner + renter)
live_alone_tot_vars<-paste0("B25011_0",c(18, 42)) # Number >= live alone (owner + renter)
not_alone_tot65_vars<-paste0("B25011_0",c(25,49)) # Number >= 65 years that do NOT live alone (owner + renter)
not_alone_tot_vars<-paste0("B25011_0",c(22,46)) # Number >= do NOT live alone (owner + renter)

live_alone_owner<-get_acs(geography = "zcta",live_alone_owner_vars,year=y)[,c("GEOID","estimate")] %>%
  rename(live_alone_owner=estimate)

not_alone_owner<-get_acs(geography = "zcta",not_alone_owner_vars,year=y)[,c("GEOID","estimate")] %>%
  rename(not_alone_owner=estimate)
#%>%  rename(not_alone_owner=estimate)
live_alone_renter<-get_acs(geography = "zcta",live_alone_renter_vars,year=y)[,c("GEOID","estimate")] %>%
  rename(live_alone_renter=estimate)
not_alone_renter<-get_acs(geography = "zcta",not_alone_renter_vars,year=y)[,c("GEOID","estimate")]%>%
  rename(not_alone_renter=estimate)

live_alone_tot <- get_acs(geography = "zcta",live_alone_tot_vars,year=y) %>%
  group_by(GEOID) %>% summarise(live_alone_tot=sum(estimate,na.rm=T))
not_alone_tot <- get_acs(geography = "zcta",not_alone_tot_vars,year=y) %>%
  group_by(GEOID) %>% summarise(not_alone_tot=sum(estimate,na.rm=T))
live_alone_tot65<-get_acs(geography = "zcta",live_alone_tot65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(live_alone_tot65=sum(estimate,na.rm=T))
not_alone_tot65<-get_acs(geography = "zcta",not_alone_tot65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(not_alone_tot65=sum(estimate,na.rm=T))

####################### Poverty #####################################
#####################################################################

total_Pop_poverty <- paste0("B17001_001") # Total Population for which poverty was determined
above_pov65_vars <- paste0("B17001_0",c(44:45,58:59)) # Number >=65 at or above poverty level
above_pov_vars <-"B17001_031" # total number above poverty level
below_pov65_vars <- paste0("B17001_0",c(15:16,29:30)) # Number >=65 below poverty level 
below_pov_vars <- "B17001_002"

#2013-2017
total_pop_pov <- get_acs(geography = "zcta",total_Pop_poverty,year=y)[,c("GEOID","estimate")]  %>%
  rename(total_pop_pov=estimate)
above_pov <- get_acs(geography = "zcta",above_pov_vars,year=y) %>%
  group_by(GEOID) %>% summarise(above_pov=sum(estimate,na.rm=T))
below_pov <- get_acs(geography = "zcta",below_pov_vars,year=y) %>%
  group_by(GEOID) %>% summarise(below_pov=sum(estimate,na.rm=T))
above_pov65<- get_acs(geography = "zcta",above_pov65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(above_pov65=sum(estimate,na.rm=T))
below_pov65<- get_acs(geography = "zcta",below_pov65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(below_pov65=sum(estimate,na.rm=T))

#2008-2012
total_pop_povy2 <- get_acs(geography = "zcta",total_Pop_poverty,year=y2)[,c("GEOID","estimate")]  %>%
  rename(total_pop_pov=estimate)
 above_povy2 <- get_acs(geography = "zcta",above_pov_vars,year=y2) %>%
  group_by(GEOID) %>% summarise(above_pov=sum(estimate,na.rm=T))
below_povy2 <- get_acs(geography = "zcta",below_pov_vars,year=y2) %>%
  group_by(GEOID) %>% summarise(below_pov=sum(estimate,na.rm=T))
above_pov65_y2<- get_acs(geography = "zcta",above_pov65_vars,year=y2) %>%
  group_by(GEOID) %>% summarise(above_pov65_y2=sum(estimate,na.rm=T))
below_pov65_y2<- get_acs(geography = "zcta",below_pov65_vars,year=y2) %>%
  group_by(GEOID) %>% summarise(below_pov65_y2=sum(estimate,na.rm=T))

total_pop_povy2$GEOID<-substring(total_pop_povy2$GEOID,3)
above_povy2$GEOID<-substring(above_povy2$GEOID,3)
below_povy2$GEOID<-substring(below_povy2$GEOID,3)
above_pov65_y2$GEOID<-substring(above_pov65_y2$GEOID,3)
below_pov65_y2$GEOID<-substring(below_pov65_y2$GEOID,3)

#2005-2009 ??????

## calculate Total population below poverty = below_pov/(above_pov+below_pov)

####################Health Insurance################################
####################################################################
total_Pop_HI <- paste0("B27001_001") # Total Population for which health insurance coverage was determined
with_HI_vars <- c(paste0("B27001_00",c(4,7)),paste0("B27001_0",c(10,13,16,19,22,25,28,32,35,38,41,44,47,50,53,56))) # Total with insurance
no_HI_vars <- c(paste0("B27001_00",c(5,8)),paste0("B27001_0",c(11,14,17,20,23,26,29,33,36,42,45,48,51,54,57))) # Total without insurance 

#2013-2017
total_pop_HI <- get_acs(geography = "zcta",total_Pop_HI,year=y)[,c("GEOID","estimate")]  %>%
  rename(total_pop_HI=estimate)
with_HI <- get_acs(geography = "zcta",with_HI_vars,year=y) %>%
  group_by(GEOID) %>% summarise(with_HI=sum(estimate,na.rm=T))
no_HI <- get_acs(geography = "zcta",no_HI_vars,year=y) %>%
  group_by(GEOID) %>% summarise(no_HI=sum(estimate,na.rm=T))

#2008-2012
#total_pop_HI12 <- get_acs(geography = "zcta",total_Pop_HI,year=y2)[,c("GEOID","estimate")]  %>%
#  rename(total_pop_HI=estimate)
#with_HI12 <- get_acs(geography = "zcta",with_HI_vars,year=y2) %>%
#  group_by(GEOID) %>% summarise(with_HI12=sum(estimate,na.rm=T))
#no_HI12 <- get_acs(geography = "zcta",no_HI_vars,year=y2) %>%
#  group_by(GEOID) %>% summarise(no_HI12=sum(estimate,na.rm=T))
####################Race/Ethnicity##################################
####################################################################
## All Ages
black_vars<-"B01001B_001"
hispanic_vars<-"B01001I_001"
white_vars<-"B01001A_001"             # Total White 
white_nothis_vars<-"B01001H_001"      # Total White, not Hispanic or Latino
asian_vars<-"B01001D_001"
mixed_race_vars<-"B01001G_001"       #Two or more races
nat_amer_vars<-"B01001C_001"
pacific_ils_vars<-"B01001E_001"
other_vars<-"B01001F_001"
other_combined_vars<-c("B01001C_001","B01001E_001","B01001F_001","B01001G_001","B01001D_001") #Native American, Pacific Islander,mixed, and Other combined due to low percentages

## 65 and older
black65_vars <- paste0("B01001B_0",c(14:16,29:31)) # Number of >=65 years and black individuals 
hispanic65_vars <- paste0("B01001I_0",c(14:16,29:31)) # Number of >=65 years and Hispanic and Latino individuals
white_nothis65_vars <- paste0("B01001H_0",c(14:16,29:31)) # Number of >=65 years and white, not Hispanic or Latino individuals
white65_vars <- paste0("B01001A_0", c(14:16,29:31)) # Number of >=65 years and while
asian65_vars<-paste0("B01001D_0", c(14:16,29:31))# Number of >=65 years and Asian individuals
mixed_race65_vars<-paste0("B01001G_0",c(14:16,29:31))# Number of >=65 years and two or more races 
other65_vars<-paste0("B01001F_0", c(14:16,29:31))# Number of >=65 years and other race individuals
nat_amer65_vars<-paste0("B01001C_0", c(14:16,29:31))# Number of >=65 years and American Indian and Alaskan Native individuals
pacific_ils65_vars<-paste0("B01001E_0", c(14:16,29:31))# Number of >=65 years and Native Hawaiian and other pacific islander individuals
other_combined65_vars<-c(paste0("B01001F_0", c(14:16,29:31)),paste0("B01001E_0", c(14:16,29:31)),paste0("B01001C_0", c(14:16,29:31)))
# Number of >=65 years and other race individuals combine with Native American and Pacific Islander due to low percentages in groups


hispanic <- get_acs(geography = "zcta",hispanic_vars,year=y) %>%
  group_by(GEOID) %>% summarise(hispanic=sum(estimate,na.rm=T))
white <- get_acs(geography = "zcta",white_vars,year=y) %>%
  group_by(GEOID) %>% summarise(white=sum(estimate,na.rm=T))
white_nothis<-get_acs(geography = "zcta",white_nothis_vars,year=y) %>%
  group_by(GEOID) %>% summarise(white_nothis=sum(estimate,na.rm=T))
black <- get_acs(geography = "zcta",black_vars,year=y) %>%
  group_by(GEOID) %>% summarise(black=sum(estimate,na.rm=T))
asian<-get_acs(geography = "zcta",asian_vars,year=y) %>%
  group_by(GEOID) %>% summarise(asian=sum(estimate,na.rm=T))
nat_amer<-get_acs(geography = "zcta",nat_amer_vars,year=y) %>%
  group_by(GEOID) %>% summarise(nat_amer=sum(estimate,na.rm=T))
pacific_ils<-get_acs(geography = "zcta",pacific_ils_vars,year=y) %>%
  group_by(GEOID) %>% summarise(pacific_ils=sum(estimate,na.rm=T))
mixed<-get_acs(geography = "zcta",mixed_race_vars,year=y) %>%
  group_by(GEOID) %>% summarise(mixed=sum(estimate,na.rm=T))
other<-get_acs(geography = "zcta",other_vars,year=y) %>%
  group_by(GEOID) %>% summarise(other=sum(estimate,na.rm=T))
other_combined<-get_acs(geography = "zcta",other_combined_vars,year=y) %>%
  group_by(GEOID) %>% summarise(other_combined=sum(estimate,na.rm=T))

### Race and Ethnicity of population >=65 
hispanic65 <- get_acs(geography = "zcta",hispanic65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(hispanic65=sum(estimate,na.rm=T))
white65 <- get_acs(geography = "zcta",white65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(white65=sum(estimate,na.rm=T))
white_nothis65<-get_acs(geography = "zcta",white_nothis65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(white_nothis65=sum(estimate,na.rm=T))
black65 <- get_acs(geography = "zcta",black65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(black65=sum(estimate,na.rm=T))
asian65<-get_acs(geography = "zcta",asian65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(asian65=sum(estimate,na.rm=T))
nat_amer65<-get_acs(geography = "zcta",nat_amer65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(nat_amer65=sum(estimate,na.rm=T))
pacific_ils65<-get_acs(geography = "zcta",pacific_ils65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(pacific_ils65=sum(estimate,na.rm=T))
mixed65<-get_acs(geography = "zcta",mixed_race65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(mixed65=sum(estimate,na.rm=T))
other65<-get_acs(geography = "zcta",other65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(other65=sum(estimate,na.rm=T))
other_combined65<-get_acs(geography = "zcta",other_combined65_vars,year=y) %>%
  group_by(GEOID) %>% summarise(other_combined65=sum(estimate,na.rm=T))
################### Median Property Values ############################
####################################################################
median_value<-get_acs(geography = "zcta", "B25077_001", year = y) %>%
  group_by(GEOID) %>% summarise(median_value=sum(estimate,na.rm=T))
#####################  Median Income ###############################
####################################################################
median_income<-get_acs(geography = "zcta", "B20004_001", year = y) %>%
  group_by(GEOID) %>% summarise(median_income=sum(estimate,na.rm=T))
###################Public Transportation ###########################
####################################################################
total_Pop_transport <- "B08006_001"# Total Population for which Transportation to Work was determined
Public_Transport_vars <- "B08006_008" # Total number public transportation to work


#2013-2017
total_pop_transport <- get_acs(geography = "zcta",total_Pop_transport,year=y)[,c("GEOID","estimate")]  %>%
  rename(total_pop_transport=estimate)
public_transport <- get_acs(geography = "zcta",Public_Transport_vars,year=y) %>%
  group_by(GEOID) %>% summarise(public_transport=sum(estimate,na.rm=T))

###################Education (No College) ###########################
####################################################################
total_pop_edu<-get_acs(geography = "zcta","B07009_001",year=y)[,c("GEOID","estimate")]  %>%
  rename(total_pop_edu=estimate)

education_vars<-paste0("B07009_00", c(2,3))
edu_no_college<-get_acs(geography = "zcta",education_vars,year=y)[,c("GEOID","estimate")]  %>%
  rename(edu_no_college=estimate)

################## Calculate Percentages ##########################
###################################################################
zcta_data <- list(total_pop,total_pop_65_y2, total_pop_y2,pop_density,total_pop_65,
                  owner,renter,live_alone_tot,not_alone_tot,total_pop_pov,
                  live_alone_tot65,above_pov, below_pov, total_pop_povy2,above_povy2,
                  below_povy2,above_pov65,below_pov65,above_pov65_y2,below_pov65_y2,
                  total_pop_HI,with_HI,no_HI,hispanic,white,white_nothis,black,asian,
                  nat_amer,pacific_ils,mixed,other,other_combined,hispanic65,white65,
                  white_nothis65,black65,asian65,nat_amer65,pacific_ils65,mixed65,
                  other65,other_combined65,public_transport,total_pop_transport,
                  median_value,edu_no_college,total_pop_edu,median_income)
                  

zcta_data2<-zcta_data %>% reduce(full_join, by='GEOID')

####Percentages calculated using counts
zcta_data3 <-  zcta_data2 %>% mutate(total_pop=total_pop,
                                     pop_density=pop_density,
                                     total_pop_y2=total_pop_y2,
                                     total_pop_65=total_pop_65,
                                     total_pop_65_y2=total_pop_65_y2,
                                     per_over65=total_pop_65/total_pop*100,
                                     per_own_home=owner/(owner+renter)*100,
                                     per_rent_home=renter/(owner+renter)*100,
                                     per_live_alone=live_alone_tot/(live_alone_tot+not_alone_tot)*100,
                                     per_live_alone65=live_alone_tot65/(live_alone_tot+not_alone_tot)*100,
                                     per_live_alone_65=live_alone_tot65/(live_alone_tot65+not_alone_tot65)*100,
                                     per_pov=below_pov/(below_pov+above_pov)*100,        #add above 65 pov
                                     per_pov_y2=below_povy2/(below_povy2+above_povy2),   #add above 65 pov yr 2
                                     per_HI=with_HI/(with_HI+no_HI)*100,
                                     #per_white = white/total_pop*100,
                                     per_white_nothis = white_nothis/total_pop*100,
                                     per_black=black/total_pop*100,
                                     per_hispanic=hispanic/total_pop*100,
                                     #per_asian=asian/total_pop*100,
                                     #per_mixed=mixed/total_pop*100,
                                     #per_other=other/total_pop*100,
                                     #per_nat_amer=nat_amer/total_pop*100,
                                     #per_pacific_ils=pacific_ils/total_pop*100,
                                     per_other_combined=other_combined/total_pop*100,
                                     #per_white65 = white65/total_pop_65*100,
                                     #per_white_nothis65 = white_nothis65/total_pop_65*100,
                                     #per_black65=black65/total_pop_65*100,
                                     #per_hispanic65=hispanic65/total_pop_65*100,
                                     #per_asian65=asian65/total_pop_65*100,
                                     #per_mixed65=mixed65/total_pop_65*100,
                                     #per_other65=other65/total_pop_65*100,
                                     #per_nat_amer65=nat_amer65/total_pop_65*100,
                                     #per_pacific_ils65=pacific_ils65/total_pop_65*100,
                                     #per_other_combined65=other_combined65/total_pop_65*100,
                                     per_public_trans=public_transport/total_pop_transport*100,
                                     median_value=median_value,
                                     no_college=edu_no_college/total_pop_edu*100,
                                     median_income)
#zcta_data3<-zcta_data3[,-c(3,16:18,22:24)]

## Connect Zips to ZCTAs
zip_info <- readRDS("C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/CBSA_ZIPs_ZCTA_Proj_CCHealth.rds")
zip_info2<-zip_info[,c(1,2,5:7)]
rm(zip_info)

# Limit dataframe to ZCTAs of interest
colnames(zcta_data3)[1]<-"ZCTA"
zips <- as.data.frame(merge(zip_info2, zcta_data3,by="ZCTA"))

names(zips)
str(zips)
zips$CBSA<-as.character(zips$CBSA)

zips<-zips %>% mutate_at(40:53,round,2)

zip_cbsa_an<-left_join(zips, zz_rr_zip,by = join_by(ZIP, CBSA, CBSA.Title))
zip_cbsa_an<-subset(zip_cbsa_an, !is.na(zip_cbsa_an$ann_an_rate100k))


zip_cbsa_an2<-na.omit(zip_cbsa_an)
zip_cbsa_an2<-zip_cbsa_an2[,-c(7:39,41:42)]


write.csv(zip_cbsa_an2,"C:/Users/emcinroe/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Climate_Data_Exc/zip_cbsa_an2.csv")

cbsa_an<-zip_cbsa_an2 %>% group_by(CBSA)