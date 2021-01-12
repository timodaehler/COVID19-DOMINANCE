# This code assembles several variables to generate a 6.5 year panel from January 2014-June 2020. This script is copmletely autonomous, so 
# it can be run without needing the other scripts to calculate the revised paper tables and regressions, etc. 
# The way that I assemble the panel is simple. First, I import CDS data as "cds_five" and subset it for the 30 countries and 6.5 years. 
# This gives me the dataframe of roughly 30 x 365 x 6.5 = 70,000+ observations to which I will later attach all other variables through joins. 
# This panel is then used to run regressions at the end of the script. 
# In detail: 
# 
# First, I Import cds_five to have the basic observations and cds data ready. I do this first because CDS is the explained variable
# so it should also be at the left-hand side of the dataframe for ease of overview. 

# Second, I manipulate the CDS data so that I get the necessary lagged log changes as an explanatory variable. I also create the global CDS
# factor immediately. The regional CDS factors are generated in a later step. 

# Third, after the CDS manipulations, I join VIX and Brent data to the CDS dataframe and manipualte them so that I can use  lagged log changes as explanatory variables. 

# Fourth, I import the main dataset of the 1st version of the paper "Oxford_V1" to obtain many potential explanatory variables related to epidemiology. Keep in mind that
# Oxford_V1 data is not for the period between 2014 and 2019 but only starts in 2020. Thus, after joining Oxford_V1 to the main CDS dataset, I need to fill up the values 
# of the period before early 2020 with zeros. As an example, the variable "Oxford_V1$Total_Cases_Country" gives for a given day and country the number of total COVID infections
# up to this day. Obviously this should be zero for the period 2014 to 2019 as there were no cases before the outbreak of Corona in 2020 in China. 

# Fifth, now I import additional time series such as the oil dependence, sovereign wealth funds, international reserves, etc. 

# Sixth, I need to generate the regional factors. This time around I need to have them weighted by GDP but alo by trade-shares. See further down in the script how I do this. 
# After creating those, I need to join the regional factor data to the CDS dataset. 

# Keep in mind that throughtout this script, I try to call the final dataset that I am using for the regressions "panel_for_revised_paper". So this is the main outputset of the
# first part of the script. 


# loading packages --------------------------------------------------------
#I don't want scientific notation for my values, so I specify this below. 
options(scipen = 999)
# Load libraries

library(sf)
library(readr)
library(mapview)
library(ggplot2)
library(tidyverse)
library(rlang)
library(reshape)
library(rgdal)
library(lubridate)
library(plotly)
library(patchwork)
library(ggforce)
library(gridExtra)
library(htmltools)
library(data.table)
library(webshot)
library(coronavirus)
library(runner)
library(zoo)
library(DataCombine)
library(fastDummies)
library(car)
library(heatmaply)
library(htmlwidgets)
library(summarytools)
library(glmnet)
library(caret)
library(mlbench)
library(psych)
library(plm)
library(lmtest)
library(quantmod)
library(leaflet)
library(corrplot)
library(fBasics)
library(stargazer)
library(tseries)
library(vars)
library(dplyr)
library(gghighlight)

library(readxl)
library(data.table)
library(dplyr)
library(writexl)
library(plyr)
library(visdat)
--------------------------------------------------------------------------

# read in cds ddata -------------------------------------------------------
cds_five <- read_excel("Data/CDS.xlsx", sheet = "5yrCDS")
countries <- read_excel("Data/laender.xlsx", sheet = "EM")
# You can create safety copies so that when my mac does not have the excel files downloaded you can still load it
# safetycopy_cds_five <- cds_five
# cds_five <- safetycopy_cds_five

# define 30 EM countries ---------------------------------------------------
# subsetting for only EM countries and for the right time frame
em_countries <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"   ]
cds_five <- cds_five[which(cds_five$Date>="2013-12-30" & cds_five$Date<="2020-07-01"   ),]
em_cds <- cds_five[, c("Date", em_countries$COUNTRY5yrCDS )]
# Make the wide panel long
em_cds <- as.data.frame(gather(em_cds, "Country", "CDS", -"Date")) 
# logging the CDS levels
em_cds <- em_cds %>%
  mutate(log.CDS = log(CDS) ) 
# lagging the logged levels
em_cds <- ddply( 
  em_cds, .(Country), transform,
  # This assumes that the data is sorted
  lagged.log.CDS = c( NA, log.CDS[-length(log.CDS)] ) )
# taking the difference of the previous and the current lagged logs
em_cds <- em_cds %>%
  mutate(changes.log.CDS = log.CDS-lagged.log.CDS ) 
# lagging the differences
em_cds <- ddply( 
  em_cds, .(Country), transform,
  # This assumes that the data is sorted
  lagged.changes.log.CDS = c( NA, changes.log.CDS[-length(log.CDS)] ) )
# I created "neededforJoins" to have a date-country dataframe to subset and join other variables
neededforJoins <- em_cds[, c("Date", "Country")]
# removing the first two observations which were only necessary to generate first-differences
em_cds <- em_cds[!(em_cds$Date==as.Date("2013-12-30")), ]
em_cds <- em_cds[!(em_cds$Date==as.Date("2013-12-31")), ]



# Define the panel --------------------------------------------------------
# I call this the "panel_for_revised_paper"
panel_for_revised_paper <- em_cds



# Generate the global CDS variable ----------------------------------------
# import the GDP-weights, which are based on GDP. 
GDP2019USDMIL_D <- read_excel("Data/GDP.xlsx", sheet = "GLOBAL")
total <- sum(GDP2019USDMIL_D$GDP2019MIL)
GDP2019USDMIL_D <- GDP2019USDMIL_D %>% mutate(weight = GDP2019MIL/ total  )
# import the cds data for developed markets
cds_five <- read_excel("Data/CDS.xlsx", sheet = "5yrCDS")
cds_five <- cds_five[which(cds_five$Date>="2013-12-31" & cds_five$Date<="2020-07-01"   ),]
non_em_countries <- countries[ which(countries$EM_dummy5yr !=1), "COUNTRY5yrCDS"   ]
nonem_cds<-cds_five[, c("Date", non_em_countries$COUNTRY5yrCDS )]
# difference it
d_nemds<-apply(log(nonem_cds[,-1]),2,diff)
d_nemds <- as.data.frame(d_nemds)
# weighting the log CDS changes and creating a dataframe of global CDS changes
d_nemds <- as.data.frame(d_nemds[,])
weighted_matrix <- d_nemds*GDP2019USDMIL_D$weight
glo_cds <- as.data.frame(rowMeans(weighted_matrix) )
glo_cds$Date <- unique(em_cds$Date)
glo_cds <- glo_cds[, c(2,1)]
colnames(glo_cds) <- c("Date", "GDPweightedglobalCDSlogchanges")
# This code below allows us to see the weights of the US, Europe, and Japan of the GDP weighted global log CDSchanges
GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "US"), ]
US_Weight <- GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "US"), ]
# Weight of Japan in global factor
GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "Japan"), ]
Japan_Weight <- GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "Japan"), ]
# Weight of Eurozone in global factor
1-(US_Weight$weight + Japan_Weight$weight)
EZ_Weight <- 1-(US_Weight$weight + Japan_Weight$weight)
# Joining the global CDS changes (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, glo_cds, by = c("Date" = "Date")  )

# Safetycopy 
# safetypanelwithglobalCDS <- panel_for_revised_paper
# panel_for_revised_paper <- safetypanelwithglobalCDS



# Adding the VIX data to the panel ----------------------------------------
# importing the VIX data
VIX <- as.data.frame(read_excel("Data/vix.xlsx", sheet = "VIX"))
VIX <- VIX %>% arrange(Date) 
VIX <- VIX[which(VIX$Date>="2013-12-30" & VIX$Date<="2020-07-01"   ),]
VIX <- left_join(neededforJoins, VIX, by = c("Date" = "Date")  )
# logging the VIX levels
VIX <- VIX %>%
  mutate(log.VIX = log(VIX) ) 
# lagging the logged levels
VIX <- ddply( 
  VIX, .(Country), transform,
  # This assumes that the data is sorted
  lagged.log.VIX = c( NA, log.VIX[-length(log.VIX)] ) )
# taking the difference of the previous and the current lagged logs
VIX <- VIX %>%
  mutate(changes.log.VIX = log.VIX-lagged.log.VIX ) 
# lagging the differences
VIX <- ddply( 
  VIX, .(Country), transform,
  # This assumes that the data is sorted
  lagged.changes.log.VIX = c( NA, changes.log.VIX[-length(log.VIX)] ) )
# Joining the VIX changes (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, VIX, by = c("Country" = "Country", "Date" = "Date")  )

# Safetycopy 
# safetywithVIX <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithVIX



# Adding the BRENT data to the panel ---------------------------------------
# importing the Brent data
BRENT <- as.data.frame(read_excel("Data/brent.xlsx", sheet = "BRENT"))
BRENT <- BRENT %>% arrange(Date) 
BRENT <- BRENT[which(BRENT$Date>="2013-12-30" & BRENT$Date<="2020-07-01"   ),]
BRENT <- left_join(neededforJoins, BRENT, by = c("Date" = "Date")  )
# logging the BRENT levels
BRENT <- BRENT %>%
  mutate(log.BRENT = log(BRENT) ) 
# lagging the logged levels
BRENT <- ddply( 
  BRENT, .(Country), transform,
  # This assumes that the data is sorted
  lagged.log.BRENT = c( NA, log.BRENT[-length(log.BRENT)] ) )
# taking the difference of the previous and the current lagged logs
BRENT <- BRENT %>%
  mutate(changes.log.BRENT = log.BRENT-lagged.log.BRENT ) 
# lagging the differences
BRENT <- ddply( 
  BRENT, .(Country), transform,
  # This assumes that the data is sorted
  lagged.changes.log.BRENT = c( NA, changes.log.BRENT[-length(log.BRENT)] ) )
# Joining the BRENT changes (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, BRENT, by = c("Country" = "Country", "Date" = "Date")  )

# Safetycopy 
# safetywithBRENT <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithBRENT




# Importing the policy, economic, and epidemiological variables -----------
# All these variables were generated with the script "Sensitivity Analysis.R" for the first edition of the paper. 
# Thus, all I need to do here is import the excel sheet that was the other script's output. 
Oxford_V1 <- read_excel("Data/Oxford_V1_augmented_5.1.2021.xlsx") 
oxford <- Oxford_V1

# Manipulating some variables
colnames(oxford)[which(colnames(oxford) == "Total_Cases_Country")] <- "Total_Case"
colnames(oxford)[which(colnames(oxford) == "Total_Deceased_Country")] <- "Total_Death"
colnames(oxford)[which(colnames(oxford) == "New_Confirmed_Country")] <- "New_Case"
colnames(oxford)[which(colnames(oxford) == "New_Total_Deceased_Country")] <- "New_Death"
oxford$Total_Case_Rate <- oxford$Total_Case/oxford$Population*1000000 ### Total case rate per one million people
oxford$Total_Mortality_Rate <- oxford$Total_Death/oxford$Population*1000000 ### Total mortality rate per one million people
oxford$New_Case_Rate <- oxford$New_Case/oxford$Population*1000000 ### New case rate per one million people
oxford$New_Mortality_Rate <- oxford$New_Death/oxford$Population*1000000 ### New mortality rate per one million people

# Reordering
oxford <- oxford[order(oxford$Date),]
oxford <- oxford[order(oxford$COUNTRY),]

# Generating new variables
oxford <- oxford %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(Total_Mortality_Rate_Growth = (Total_Mortality_Rate - Lag(Total_Mortality_Rate,1))/Lag(Total_Mortality_Rate,1))
oxford <- oxford %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(New_Mortality_Rate_Growth = (New_Mortality_Rate - Lag(New_Mortality_Rate,1))/Lag(New_Mortality_Rate,1))
oxford <- oxford %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(Total_Case_Rate_Growth = (Total_Case_Rate - Lag(Total_Case_Rate,1))/Lag(Total_Case_Rate,1))
oxford <- oxford %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(New_Case_Rate_Growth = (New_Case_Rate - Lag(New_Case_Rate,1))/Lag(New_Case_Rate,1))

# Now I can join the Oxford_V1 database that contain many of the explanatory variables to the panel_for_revised_paper 
panel_for_revised_paper <- left_join(panel_for_revised_paper, oxford, by = c("Country" = "COUNTRY", "Date" = "Date")  )

copy <- panel_for_revised_paper  <- copy
# view(colnames(panel_for_revised_paper))
# view(colnames(panel_for_revised_paper))

# In order for this to work, I need to make the data numeric first.
panel_for_revised_paper$Fiscal_Response_Dummy <- as.numeric(panel_for_revised_paper$Fiscal_Response_Dummy)
panel_for_revised_paper$Monetary_Response_Dummy <- as.numeric(panel_for_revised_paper$Monetary_Response_Dummy)
panel_for_revised_paper$Macroprudential_Dummy <- as.numeric(panel_for_revised_paper$Macroprudential_Dummy)
panel_for_revised_paper$Loan_or_Credit_Dummy <- as.numeric(panel_for_revised_paper$Loan_or_Credit_Dummy)
# Now I fill up the missing values in the early dates for all those columns for which I know this makes sense
for(i in c(19:22, 24:30, 33:54, 87:102)  ) {       # for-loop over columns
  panel_for_revised_paper[panel_for_revised_paper$Date==as.Date("2014-01-01") , i] <- 0
  panel_for_revised_paper <- panel_for_revised_paper  %>% dplyr::group_by(Country) %>% fill(i) }
# Now I subset for variables that I need and toss out the rest. 
panel_for_revised_paper <- panel_for_revised_paper[, c(1:26, 31:32, 51:52, 60:62, 87:102)]
# Now I fill up the missing values in the early dates for all those columns for which I know that they stay constant across time, for example latitude
for(i in c(23, 27:28)  ) {       
  panel_for_revised_paper <- panel_for_revised_paper %>% fill(i, .direction = "up") }
# This worked well so far. What we see is that there's some NAs in "driving", "walking", and "transit". I'll fill those up now
panel_for_revised_paper <- panel_for_revised_paper %>%   dplyr::group_by(Country) %>% fill(transit, .direction = "up")
panel_for_revised_paper <- panel_for_revised_paper %>%   dplyr::group_by(Country) %>% fill(driving, .direction = "up")
panel_for_revised_paper <- panel_for_revised_paper %>%   dplyr::group_by(Country) %>% fill(walking, .direction = "up")

# Safetycopy 
# safetywithfilledupvalues <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithfilledupvalues



# Import google mobility stringency ---------------------------------------
# Importing
mobility_google <- read.csv("Data/Global_Mobility_Report.csv") # This thing is large so I keep a copy here: copyofmobility_google <- mobility_google
# Subsetting for the right 30 EM countries. It turns out China is missing, but that's fine. In fact, having data for 29 countries is much better than for the Apple mobility data. 
mobility_google <- mobility_google[mobility_google$country_region %in% listofneededcountries, ] 
# Subsetting for data on the country as a hole
mobility_google <- mobility_google[mobility_google$sub_region_1 == "", ]
mobility_google <- mobility_google[mobility_google$metro_area == "", ]
# Getting the date in the right format
mobility_google$Date <- ymd(mobility_google$date, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)
# Subset for the right dates for our panel
mobility_google <- mobility_google[mobility_google$Date < as.Date("2020-07-01"), ]
# Subset for the right variables/columns
mobility_google <- mobility_google[, c("Date", "country_region", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline" )]
# Now I can join the mobility_google database to the panel_for_revised_paper 
panel_for_revised_paper <- left_join(panel_for_revised_paper, mobility_google, by = c("Country" = "country_region", "Date" = "Date")  )
# Since the first day for which google provides data is 2020-02-15, I set values for 2020-02-14 to zero and fill up the early stages of the time series with zeros, too
for(i in c(50:55)  ) {       
  panel_for_revised_paper[panel_for_revised_paper$Date==as.Date("2020-02-14") & panel_for_revised_paper$Country != "China", i] <- 0
  panel_for_revised_paper <- panel_for_revised_paper  %>% dplyr::group_by(Country) %>% fill(i, .direction = "up") }
# I was trying to fill up the missing google_mobility data for China with apple mobility data but that is also not available for China. 

# Safetycopy 
# safetywithmobility_google <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithmobility_google



# Import government stringency --------------------------------------------
# Since the code in "weightedCDS.R" script does not do the right data manipulation for the countries (i.e. it does not look at a country's aggregate stringency but instead
# looks at the provincial ones for cases like Brazil and China, I am reimporting it here to do the necessary manipualtion myself)
# I have to download it from the internet
Government_Response <- read.csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")
# Then I create a copy just so that I can avoid future downloads
# Download.Government_Response <- Government_Response
# Government_Response <- Download.Government_Response
# I select the columns that I need
Government_Response <- Government_Response[,c(1:6, 40)]
# First, I convert the date to a proper format.
Government_Response$Date <- ymd(Government_Response$Date, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), truncated = 0)
# Now I rename the column "CountryName" to "Country" to have it standardized across datasets
colnames(Government_Response)[1] <- "Country"
# Now I have to rename Czechia as the name of this country are different across datasets
Government_Response$Country[Government_Response$Country == "Czech Republic"] <- "Czechia"
# Now I subset so that only the country aggregates remain, rather than the states within a country, for example Brazil 
Government_Response <- Government_Response[Government_Response$Jurisdiction=="NAT_TOTAL", ]
# Now I subset for only those variables that I need again
Government_Response <- Government_Response[,c("Country", "Date", "StringencyIndex")]
# Now I can join the Government_Response data to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, Government_Response, by = c("Country" = "Country", "Date" = "Date")  )

# Now I can create the Strinencyindex growth variable and name it SI_Growth. First, I set the NAs to zero. I checked at the end of the panel to make sure I can do so without problems
panel_for_revised_paper$StringencyIndex[is.na(panel_for_revised_paper$StringencyIndex)] <- 0
# Then I create the growth number
panel_for_revised_paper <- panel_for_revised_paper %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(SI_Growth = (StringencyIndex - Lag(StringencyIndex,1))/Lag(StringencyIndex,1))
# The first number is a NA since it does not have a divisor. So I set it to zero
panel_for_revised_paper$SI_Growth[is.na(panel_for_revised_paper$SI_Growth)] <- 0
# The remaining problem is that there are some SI_Growth terms that are infinite values because of division by zero. I leave them in now.
# Maybe when we do the regressions, we should exclude those values. 

# Safetycopy 
# safetywithgovernmentstringency <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithgovernmentstringency



# Adding additional variables such as oil income effects ------------------
# Importing the net oil exports as a share of GDP
net_oil_exp_of_GDP <- as.data.frame(read_excel("Data/OilshareofGDP.xlsx", sheet = "R") )
# For some reason, I need to change the country names to do the merge. I will set them back later. 
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "Dominican Republic"] <- "DominicanRepublic"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "Saudi Arabia"] <- "SaudiArabia"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "South Africa"] <- "SouthAfrica"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "Sri Lanka"] <- "SriLanka"
# Joining it to the panel
panel_for_revised_paper <- left_join(panel_for_revised_paper, net_oil_exp_of_GDP, by = c("Country" = "Country")  )
vis_dat(panel_for_revised_paper, warn_large_data = F)
# Now I change the names back again
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "DominicanRepublic"] <- "Dominican Republic"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "SaudiArabia"] <- "Saudi Arabia"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "SouthAfrica"] <- "South Africa"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "SriLanka"] <- "Sri Lanka"
# Creating the oil income effect variable by multiplying the net oil exports as a share of GDP with log changes in Brent prices
panel_for_revised_paper <- panel_for_revised_paper %>% mutate(oil_income_price_effect =  net_oil_exp_of_GDP*changes.log.BRENT) 

# Safetycopy 
# safetywithoilincome <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithoilincome



# Adding international reserves to the panel ------------------------------
# Importing Reserves
Reserves <- gather(as.data.frame(read_excel("Data/International_Liquidity.xlsx", sheet = "R") ), "Date", "Reserves_in_millions", -"Country"   )
# Getting the right Date format
Reserves$Date <- as.Date(as.numeric(Reserves$Date),  origin = "1899-12-30")
# Multiplying values by 1 million to get normal number rather than million scale
Reserves$Reserves_in_millions <- as.numeric(Reserves$Reserves_in_millions)
Reserves <- Reserves %>% mutate(Reserves = Reserves_in_millions *1000000  )
# Subsetting for only those 30 EMs for which we need data
Reserves <- Reserves[Reserves$Country %in% listofneededcountries, ]
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, Reserves, by = c("Country" = "Country", "Date" = "Date")  )
# Filling up the missing values of the month from the End-of-Month-value
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Reserves, .direction = "up")
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Reserves_in_millions, .direction = "up")
# This worked well. The only country for which there is no Data is Peru after March 2020. But I just project these values forward into the future.
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Reserves)
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Reserves_in_millions)

# Safetycopy 
# safetywithreserves <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithreserves



# Adding sovereign wealth fund to the data --------------------------------
# Importing data on SWF
SWF <- read_excel("Data/SWF.xlsx", sheet = "R")
# Replace funds with NA in AuM with zero
SWF$AuM_bn[SWF$AuM_bn == "NA"]  <- 0
# Changing AuM to numeric
SWF$AuM_bn <- as.numeric(SWF$AuM_bn)
# Changin AuM from billion to USD
SWF <- SWF %>% mutate(AuM = AuM_bn * 10^9)
# Drop Funds which aren't SWF, i.e. PPF
SWF <- subset(SWF, Type=="SWF" )
# Defining sample countries so that I can subset the SWF dataset 
countries <- read_excel("Data/laender.xlsx", sheet = "EM")
allCountriesSWF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
colnames(allCountriesSWF) <- "COUNTRY"
# Subsetting countries with SWF that are in sample
SWF <- SWF[SWF$COUNTRY %in% allCountriesSWF$COUNTRY,  ]
# Summing funds per country to get country total SWF
SWF_Countryaggregates <- aggregate(SWF$AuM, by=list(Category=SWF$COUNTRY), FUN=sum)
# Renaming columns
colnames(SWF_Countryaggregates) <- c("Country", "SWF_2019_AuMinUSD")
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, SWF_Countryaggregates, by = c("Country" = "Country")  )
# Filling up the missing values for those countries with no sovereign wealth fund with zeros
panel_for_revised_paper$SWF_2019_AuMinUSD[is.na(panel_for_revised_paper$SWF_2019_AuMinUSD)] <- 0
# Checking if the data looks complete
vis_dat(panel_for_revised_paper, warn_large_data = F)

# Safetycopy 
# safetywithSWF <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithSWF



# Adding GDP to the panel -------------------------------------------------
# Importing GDP
GDP_Worldbank <- gather(as.data.frame(read_excel("Data/GDP_WorldBank.xlsx", sheet = "R") ), "Date", "Annual_GDP", -"Country"   )
# Getting the right date format
GDP_Worldbank$Date <- as.Date(GDP_Worldbank$Date)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, GDP_Worldbank, by = c("Country" = "Country", "Date" = "Date")  )
# Since GDP data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the GDP data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Annual_GDP, .direction = "up") 
vis_dat(panel_for_revised_paper, warn_large_data = F)

# Safetycopy 
# safetywithGDP <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithGDP



# Adding foreign currency share of external debt to panel ------------------------
# Importing External debt in foreign currency, % of total
Externaldebtforeigncurrencyshareoftotalexternaldebt <- gather(as.data.frame(read_excel("Data/External debt in foreign currency, % of total.xlsx", sheet = "R") ), "Date", "External_debt_foreign_currency_share_of_total_external_debt", -"Country"   )
# Getting the right date format
Externaldebtforeigncurrencyshareoftotalexternaldebt$Date <- as.Date(Externaldebtforeigncurrencyshareoftotalexternaldebt$Date)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, Externaldebtforeigncurrencyshareoftotalexternaldebt, by = c("Country" = "Country", "Date" = "Date")  )
# Since external debt data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the external debt data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(External_debt_foreign_currency_share_of_total_external_debt, .direction = "up") 

# Safetycopy 
# safetywithforeigncurrencyshareofexternaldebt <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithforeigncurrencyshareofexternaldebt



# Adding external debt GDP share to panel -------------------------------------------
# Data for Bahrain and Qatar is not avaialble!!
# Importing external debt GDP share to panel
TotalexternaldebtasshareofGDP <- gather(as.data.frame(read_excel("Data/Total external debt stocks, % of GDP.xlsx", sheet = "R") ), "Date", "Total_external_debt_as_share_of_GDP", -"Country"   )
# Getting the right date format
TotalexternaldebtasshareofGDP$Date <- as.Date(TotalexternaldebtasshareofGDP$Date)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, TotalexternaldebtasshareofGDP, by = c("Country" = "Country", "Date" = "Date")  )
# Since external debt data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the external debt data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Total_external_debt_as_share_of_GDP, .direction = "up") 

# Safetycopy 
# safetywithexternalDebt <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithexternalDebt



# Adding China debt data to panel -----------------------------------------
# Data for many countries not available. For those that it is available, it ends in 2017. As this is possibly a slow moving variable, I extrapolated it into the future.
# Importing China debt data
DebtToChina <- as.data.frame(read_excel("Data/HRT _ China Debt Stock Database_April2020.xlsx", sheet = "R") )
# Getting the right date format
DebtToChina$Date <- as.Date(DebtToChina$Date)
# Joining the data to panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, DebtToChina, by = c("Country" = "Country", "Date" = "Date")  )
# Since external debt data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the external debt data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(Debt_to_China_as_share_of_GDP, .direction = "up") 
panel_for_revised_paper <- panel_for_revised_paper %>% dplyr::group_by(Country) %>% fill(ChinaDebt_USD, .direction = "up") 

# xxxx maybe I need to do these later
# panel_for_revised_paper <- panel_for_revised_paper %>% fill(Debt_to_China_as_share_of_GDP) 
# panel_for_revised_paper <- panel_for_revised_paper %>% fill(ChinaDebt_USD) 

# Safetycopy 
# safetywithChinaDebt <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithChinaDebt





# Creating the trade-shares -----------------------------------------------
# Define the 30 countries to be imported 
my_data.frame <- as.data.frame(unique(panel_for_revised_paper$Country) )
# Import each country's excel sheet into a list element
mybiglist <- list()
for(i in 1:nrow(my_data.frame) ) {   
  NameofExporter <- my_data.frame[i,1]
  name <- NameofExporter
  tmp <- as.data.frame(read_excel(paste0("Data/",NameofExporter,".xlsx"), sheet = "Partner-Timeseries") )
  mybiglist[[name]] <- tmp
}
# Stack each list element into one dataframe.
ExportData  <- NULL
for(i in 1:nrow(my_data.frame) ) { 
  name <- my_data.frame[i,1]
  tmp <- as.data.frame(mybiglist[[i]] )
  ExportData <- dplyr::bind_rows(ExportData, tmp)
}
# Standardize the country names so that it matches with the country names in panel_for_revised_paper
ExportData$`Reporter Name`[ExportData$`Reporter Name` == "Czech Republic"] <- "Czechia"
ExportData$`Reporter Name`[ExportData$`Reporter Name` == "Russian Federation"] <- "Russia"
ExportData$`Reporter Name`[ExportData$`Reporter Name` == "Egypt, Arab Rep."] <- "Egypt"
ExportData$`Partner Name`[ExportData$`Partner Name` == "Czech Republic"] <- "Czechia"
ExportData$`Partner Name`[ExportData$`Partner Name` == "Russian Federation"] <- "Russia"
ExportData$`Partner Name`[ExportData$`Partner Name` == "Egypt, Arab Rep."] <- "Egypt"

#Now I can subset the "Partner Name" for the 30 countries which we need data for. 
# This leaves for every exporting country its import partners within the 30 EM
allCountriesSWF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
colnames(allCountriesSWF) <- "Country"
ExportData <- ExportData[ExportData$`Partner Name` %in% allCountriesSWF$Country,  ]
table(ExportData$`Reporter Name`)
# This looks somewhat odd. Every country should only have 29 partners, not 30. 
# Thus, I manually check the excel sheets for those countries for which the country shows up
# as its own partner. In all cases, the actual numbers are zero, i.e. Brazil rightly exports 0 to Brazil. 
# However, it is still vexing that these entries are in there, so I delete them by subsetting for observations
# where the reporting country and the partner country are not the dame.
ExportData <- subset(ExportData,`Reporter Name` != `Partner Name`)
# This worked well
table(ExportData$`Reporter Name`) 

# I rename the columns
colnames(ExportData) <- c("Exporter", "Partner", "Tradeflow", "Indicator", "Exports2014", "Exports2015", "Exports2016", "Exports2017", "Exports2018")
# Now I check if we have any NAs
summary(ExportData) # It appears that the least-sparse export column is for 2016
# view(subset(ExportData, is.na(ExportData$Exports2016) )) # It appears that those country pairs where data is missing are negligeable. I set them equal to zero
ExportData$Exports2016[is.na(ExportData$Exports2016)] <- 0

# view(ExportData[ExportData$Exporter=="Argentina", c("Exporter", "Partner", "Exports2016") ] )
# aggregate(ExportData$Exports2016, by=list(Category=ExportData$Exporter), FUN=sum)

# Now I sum up all the shars of a country's partners for 2016. 
Sumofexportshares <- aggregate(Exports2016 ~ Exporter, ExportData, sum)
colnames(Sumofexportshares) <- c("Exporter", "SumofExportsharsof2016")
# Now I add the sums to the dataset
ExportData <- left_join(ExportData, Sumofexportshares, by = c("Exporter" = "Exporter")  ) 
# Now I divide a parntner's share by the total of the partners' shares
# This gives me the weight for each partner which is proportional to that partner's share of trade
# compared to all trade with the 29 partners.
ExportData <- ExportData %>% mutate(Shareoftotalexports2016 = (Exports2016 / SumofExportsharsof2016) )
# summary(ExportData)
# a <-order(ExportData$Shareoftotalexports, decreasing =T)
# view(ExportData[a,])
# Now I subset for the needed data:
ExportData <- ExportData[, c("Exporter", "Partner", "Shareoftotalexports2016")]
# aggregate(Shareoftotalexports ~ Exporter, ExportData, sum) # works


# Now I start creating the weighted log CDS changes by multiplying the weights with the log CDS changes of all partners on a daily basis
my_data.frame <- as.data.frame(unique(panel_for_revised_paper$Country) )
mynewlist <-  list()
for(i in 1:nrow(my_data.frame) ) {   
  NameofExporter <- my_data.frame[i,1]
  testpanel <- panel_for_revised_paper[panel_for_revised_paper$Country != NameofExporter, c("Date", "Country", "changes.log.CDS")]
  CDS <- as.data.frame(spread(testpanel, Country, changes.log.CDS))
  Weights <- spread(ExportData[ExportData$Exporter == NameofExporter , ], Partner, Shareoftotalexports2016   )
  # Weights <- as.data.frame(lapply(Weights, rep, 2373))
  temp <- rowSums(data.frame(  mapply(`*`,CDS[, -c(1)], Weights[, -c(1)]  )    ) )
  mynewlist[[NameofExporter]] <- temp
}

# Create a safetycopy
# outputofloop <-mynewlist
# mynewlist <- outputofloop

# Stack each list element into one dataframe.
WeightedCDSdata  <- NULL
for(i in 1:nrow(my_data.frame) ) { 
  name <- my_data.frame[i,1]
  tmp <- as.data.frame(mynewlist[[i]] )
  WeightedCDSdata <- dplyr::bind_rows(WeightedCDSdata, tmp)  }
# Rename the column of the dataframe so that it then looks good when appended to paneo_for_revised_paper
colnames(WeightedCDSdata) <- "trade_share_weighted_log_CDS_changes_of_29_peers"
# copyblabla <- panel_for_revised_paper 
panel_for_revised_paper <- cbind(panel_for_revised_paper, WeightedCDSdata)

# view(panel_for_revised_paper)
vis_dat(panel_for_revised_paper, warn_large_data = F)

# Create a safetycopy
# safetywithTradeWeightedCDSdata <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithTradeWeightedCDSdata



# Add GDP weighted regional CDS log changes to panel ----------------------
# import and subset for countries
countries <- read_excel("Data/laender.xlsx", sheet = "EM")
em_countries <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"   ]
# import CDS data and subset for the right time and countries
cds_five <- read_excel("Data/CDS.xlsx", sheet = "5yrCDS")
cds_five <- cds_five[which(cds_five$Date>="2013-12-31" & cds_five$Date<="2020-07-01"   ),]
em_cds <- cds_five[, c("Date", em_countries$COUNTRY5yrCDS )]

# difference it
d_emcds<-apply(log(em_cds[,-1]),2,diff)
d_emcds <- as.data.frame(d_emcds)
dim(em_cds)
dim(d_emcds)

# Importing GDP 2019 data for all emerging markets
GDP2019USDMIL <- read_excel("Data/GDP.xlsx", sheet = "EM")

#Importing geographic classification of emerging markets
geographic_classification <- read_excel("Data/geographic_classification.xlsx", sheet = "COARSE_GRANULATION")

# Getting the names of the geographic groups
regions <- colnames(geographic_classification)

# Subsetting individual geographic groups
Africa <- as.data.frame( drop_na(geographic_classification[,1] ) )
names(Africa)[1] <- "COUNTRY"
CentralAsia <- as.data.frame( drop_na(geographic_classification[,2]) )
names(CentralAsia)[1] <- "COUNTRY"
EastAsia <- as.data.frame( drop_na(geographic_classification[,3]) )
names(EastAsia)[1] <- "COUNTRY"
Europe <- as.data.frame( drop_na(geographic_classification[,4]) )
names(Europe)[1] <- "COUNTRY"
LATAM <- as.data.frame( drop_na(geographic_classification[,5]) )
names(LATAM)[1] <- "COUNTRY"
MiddleEast <- as.data.frame( drop_na(geographic_classification[, 6] ) )
names(MiddleEast)[1] <- "COUNTRY"
SouthAsia <- as.data.frame( drop_na(geographic_classification[,7] ) )
names(SouthAsia)[1] <- "COUNTRY"

# Adding GDP data to countries in groups
Africa <- merge(Africa, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)
CentralAsia <- merge(CentralAsia, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)
EastAsia <- merge(EastAsia, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)
Europe <- merge(Europe, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)
LATAM <- merge(LATAM, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)
MiddleEast <- merge(MiddleEast, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)
SouthAsia <- merge(SouthAsia, GDP2019USDMIL, by = c("COUNTRY"), all.x = TRUE)

# str(as.data.frame(drop_na(geographic_classification[,1] ) ) )

# sum of others, i.e. sum of GDP for each country's regional peers
for(i in 1:(nrow(Africa)) ) {
  Africa[i,"sumofothers"] <-  sum(Africa[-i,]$GDP2019MIL) }

for(i in 1:(nrow(CentralAsia)) ) {
  CentralAsia[i,"sumofothers"] <-  sum(CentralAsia[-i,]$GDP2019MIL) }

for(i in 1:(nrow(EastAsia)) ) {
  EastAsia[i,"sumofothers"] <-  sum(EastAsia[-i,]$GDP2019MIL) }

for(i in 1:(nrow(Europe)) ) {
  Europe[i,"sumofothers"] <-  sum(Europe[-i,]$GDP2019MIL) }

for(i in 1:(nrow(LATAM)) ) {
  LATAM[i,"sumofothers"] <-  sum(LATAM[-i,]$GDP2019MIL) }

for(i in 1:(nrow(MiddleEast)) ) {
  MiddleEast[i,"sumofothers"] <-  sum(MiddleEast[-i,]$GDP2019MIL) }

for(i in 1:(nrow(SouthAsia)) ) {
  SouthAsia[i,"sumofothers"] <-  sum(SouthAsia[-i,]$GDP2019MIL) }

# Adding additional country columns
Countries_Africa <- Africa$COUNTRY
Matrix_Africa <- matrix(NA, nrow = nrow(Africa), ncol = nrow(Africa))
colnames(Matrix_Africa) <- Countries_Africa
Africa <- cbind(Africa, Matrix_Africa )

Countries_CentralAsia <- CentralAsia$COUNTRY
Matrix_CentralAsia <- matrix(NA, nrow = nrow(CentralAsia), ncol = nrow(CentralAsia))
colnames(Matrix_CentralAsia) <- Countries_CentralAsia
CentralAsia <- cbind(CentralAsia, Matrix_CentralAsia )

Countries_EastAsia <- EastAsia$COUNTRY
Matrix_EastAsia <- matrix(NA, nrow = nrow(EastAsia), ncol = nrow(EastAsia))
colnames(Matrix_EastAsia) <- Countries_EastAsia
EastAsia <- cbind(EastAsia, Matrix_EastAsia )

Countries_Europe <- Europe$COUNTRY
Matrix_Europe <- matrix(NA, nrow = nrow(Europe), ncol = nrow(Europe))
colnames(Matrix_Europe) <- Countries_Europe
Europe <- cbind(Europe, Matrix_Europe )

Countries_LATAM <- LATAM$COUNTRY
Matrix_LATAM <- matrix(NA, nrow = nrow(LATAM), ncol = nrow(LATAM))
colnames(Matrix_LATAM) <- Countries_LATAM
LATAM <- cbind(LATAM, Matrix_LATAM )

Countries_MiddleEast <- MiddleEast$COUNTRY
Matrix_MiddleEast <- matrix(NA, nrow = nrow(MiddleEast), ncol = nrow(MiddleEast))
colnames(Matrix_MiddleEast) <- Countries_MiddleEast
MiddleEast <- cbind(MiddleEast, Matrix_MiddleEast )

Countries_SouthAsia <- SouthAsia$COUNTRY
Matrix_SouthAsia <- matrix(NA, nrow = nrow(SouthAsia), ncol = nrow(SouthAsia))
colnames(Matrix_SouthAsia) <- Countries_SouthAsia
SouthAsia <- cbind(SouthAsia, Matrix_SouthAsia )

# Creating country specific weights 
for(i in 4:(ncol(Africa)) ) {
  for(j in 1:nrow(Africa)) {
    
    Africa[j, i ] <- Africa[i-3, "GDP2019MIL"]   / Africa[j,"sumofothers"] }
}

for(i in 4:(ncol(CentralAsia)) ) {
  for(j in 1:nrow(CentralAsia)) {
    
    CentralAsia[j, i ] <- CentralAsia[i-3, "GDP2019MIL"]   / CentralAsia[j,"sumofothers"] }
}

for(i in 4:(ncol(EastAsia)) ) {
  for(j in 1:nrow(EastAsia)) {
    
    EastAsia[j, i ] <- EastAsia[i-3, "GDP2019MIL"]   / EastAsia[j,"sumofothers"] }
}

for(i in 4:(ncol(Europe)) ) {
  for(j in 1:nrow(Europe)) {
    
    Europe[j, i ] <- Europe[i-3, "GDP2019MIL"]   / Europe[j,"sumofothers"] }
}

for(i in 4:(ncol(LATAM)) ) {
  for(j in 1:nrow(LATAM)) {
    
    LATAM[j, i ] <- LATAM[i-3, "GDP2019MIL"]   / LATAM[j,"sumofothers"] }
}

for(i in 4:(ncol(MiddleEast)) ) {
  for(j in 1:nrow(MiddleEast)) {
    
    MiddleEast[j, i ] <- MiddleEast[i-3, "GDP2019MIL"]   / MiddleEast[j,"sumofothers"] }
}

for(i in 4:(ncol(SouthAsia)) ) {
  for(j in 1:nrow(SouthAsia)) {
    
    SouthAsia[j, i ] <- SouthAsia[i-3, "GDP2019MIL"]   / SouthAsia[j,"sumofothers"] }
}

# Adding NAs on the diagonal
for(i in 4:(ncol(Africa)) ) {
  Africa[i-3, i ] <- NA
}

for(i in 4:(ncol(CentralAsia)) ) {
  CentralAsia[i-3, i ] <- NA
}

for(i in 4:(ncol(EastAsia)) ) {
  EastAsia[i-3, i ] <- NA
}

for(i in 4:(ncol(LATAM)) ) {
  LATAM[i-3, i ] <- NA
}

for(i in 4:(ncol(LATAM)) ) {
  LATAM[i-3, i ] <- NA
}

for(i in 4:(ncol(MiddleEast)) ) {
  MiddleEast[i-3, i ] <- NA
}

for(i in 4:(ncol(SouthAsia)) ) {
  SouthAsia[i-3, i ] <- NA
}


Africa <- Africa[,-(2:3)]
CentralAsia <- CentralAsia[,-(2:3)]
EastAsia <- EastAsia[,-(2:3)]
Europe <- Europe[,-(2:3)]
LATAM <- LATAM[,-(2:3)]
MiddleEast <- MiddleEast[,-(2:3)]
SouthAsia <- SouthAsia[,-(2:3)]

Africa
CentralAsia
EastAsia
Europe
LATAM
MiddleEast
SouthAsia



regions
Africa$COUNTRY

Egypt_affiliate_countries <- Africa$COUNTRY[!Africa$COUNTRY %in% "Egypt"]
Egypt_affiliate_countries

Egypt_weights <- subset(Africa, COUNTRY == "Egypt")
Egypt_weights 

Egypt_weights <- Egypt_weights[,Egypt_affiliate_countries]
Egypt_weights

sum(Egypt_weights) # just a check

# Subset for affiliates countries
Egypt_affiliates_matrix <- d_emcds[, Egypt_affiliate_countries]

# Change to a dataframe so that the multiplication works
Egypt_affiliates_matrix <- data.frame(Egypt_affiliates_matrix)

Egypt <- rowSums(data.frame(mapply(`*`,Egypt_affiliates_matrix,Egypt_weights)) )
Egypt


Ghana_affiliate_countries <- Africa$COUNTRY[!Africa$COUNTRY %in% "Ghana"]
Ghana_affiliate_countries

Ghana_weights <- subset(Africa, COUNTRY == "Ghana")
Ghana_weights 

Ghana_weights <- Ghana_weights[,Ghana_affiliate_countries]
Ghana_weights

sum(Ghana_weights) # just a check

# Subset for affiliates countries
Ghana_affiliates_matrix <- d_emcds[, Ghana_affiliate_countries]

# Change to a dataframe so that the multiplication works
Ghana_affiliates_matrix <- data.frame(Ghana_affiliates_matrix)

Ghana <- rowSums(data.frame(mapply(`*`,Ghana_affiliates_matrix,Ghana_weights)) )
Ghana


South.Africa_affiliate_countries <- Africa$COUNTRY[!Africa$COUNTRY %in% "South Africa"]
South.Africa_affiliate_countries

South.Africa_weights <- subset(Africa, COUNTRY == "South Africa")
South.Africa_weights 

South.Africa_weights <- South.Africa_weights[,South.Africa_affiliate_countries]
South.Africa_weights

sum(South.Africa_weights) # just a check

# Subset for affiliates countries
South.Africa_affiliates_matrix <- d_emcds[, South.Africa_affiliate_countries]

# Change to a dataframe so that the multiplication works
South.Africa_affiliates_matrix <- data.frame(South.Africa_affiliates_matrix)

South.Africa <- rowSums(data.frame(mapply(`*`,South.Africa_affiliates_matrix,South.Africa_weights)) )
South.Africa


regions
CentralAsia$COUNTRY

Kazakhstan_affiliate_countries <- CentralAsia$COUNTRY[!CentralAsia $COUNTRY %in% "Kazakhstan"]
Kazakhstan_affiliate_countries

Kazakhstan_weights <- subset(CentralAsia, COUNTRY == "Kazakhstan")
Kazakhstan_weights 

Kazakhstan_weights <- Kazakhstan_weights[,Kazakhstan_affiliate_countries]
Kazakhstan_weights

sum(Kazakhstan_weights) # just a check

# Subset for affiliates countries
Kazakhstan_affiliates_matrix <- d_emcds[, Kazakhstan_affiliate_countries]

# Change to a dataframe so that the multiplication works
Kazakhstan_affiliates_matrix <- data.frame(Kazakhstan_affiliates_matrix)

Kazakhstan <- rowSums(data.frame(mapply(`*`,Kazakhstan_affiliates_matrix,Kazakhstan_weights)) )
Kazakhstan


Russia_affiliate_countries <- CentralAsia$COUNTRY[!CentralAsia $COUNTRY %in% "Russia"]
Russia_affiliate_countries

Russia_weights <- subset(CentralAsia, COUNTRY == "Russia")
Russia_weights 

Russia_weights <- Russia_weights[,Russia_affiliate_countries]
Russia_weights

sum(Russia_weights) # just a check

# Subset for affiliates countries
Russia_affiliates_matrix <- d_emcds[, Russia_affiliate_countries]

# Change to a dataframe so that the multiplication works
Russia_affiliates_matrix <- data.frame(Russia_affiliates_matrix)

Russia <- rowSums(data.frame(mapply(`*`,Russia_affiliates_matrix,Russia_weights)) )
Russia


regions
EastAsia$COUNTRY

China_affiliate_countries <- EastAsia$COUNTRY[!EastAsia $COUNTRY %in% "China"]
China_affiliate_countries

China_weights <- subset(EastAsia, COUNTRY == "China")
China_weights 

China_weights <- China_weights[,China_affiliate_countries]
China_weights

sum(China_weights) # just a check

# Subset for affiliates countries
China_affiliates_matrix <- d_emcds[, China_affiliate_countries]

# Change to a dataframe so that the multiplication works
China_affiliates_matrix <- data.frame(China_affiliates_matrix)

China <- rowSums(data.frame(mapply(`*`,China_affiliates_matrix,China_weights)) )
China



Indonesia_affiliate_countries <- EastAsia$COUNTRY[!EastAsia $COUNTRY %in% "Indonesia"]
Indonesia_affiliate_countries

Indonesia_weights <- subset(EastAsia, COUNTRY == "Indonesia")
Indonesia_weights 

Indonesia_weights <- Indonesia_weights[,Indonesia_affiliate_countries]
Indonesia_weights

sum(Indonesia_weights) # just a check

# Subset for affiliates countries
Indonesia_affiliates_matrix <- d_emcds[, Indonesia_affiliate_countries]

# Change to a dataframe so that the multiplication works
Indonesia_affiliates_matrix <- data.frame(Indonesia_affiliates_matrix)

Indonesia <- rowSums(data.frame(mapply(`*`,Indonesia_affiliates_matrix,Indonesia_weights)) )
Indonesia



Malaysia_affiliate_countries <- EastAsia$COUNTRY[!EastAsia $COUNTRY %in% "Malaysia"]
Malaysia_affiliate_countries

Malaysia_weights <- subset(EastAsia, COUNTRY == "Malaysia")
Malaysia_weights 

Malaysia_weights <- Malaysia_weights[,Malaysia_affiliate_countries]
Malaysia_weights

sum(Malaysia_weights) # just a check

# Subset for affiliates countries
Malaysia_affiliates_matrix <- d_emcds[, Malaysia_affiliate_countries]

# Change to a dataframe so that the multiplication works
Malaysia_affiliates_matrix <- data.frame(Malaysia_affiliates_matrix)

Malaysia <- rowSums(data.frame(mapply(`*`,Malaysia_affiliates_matrix,Malaysia_weights)) )
Malaysia



Philippines_affiliate_countries <- EastAsia$COUNTRY[!EastAsia $COUNTRY %in% "Philippines"]
Philippines_affiliate_countries

Philippines_weights <- subset(EastAsia, COUNTRY == "Philippines")
Philippines_weights 

Philippines_weights <- Philippines_weights[,Philippines_affiliate_countries]
Philippines_weights

sum(Philippines_weights) # just a check

# Subset for affiliates countries
Philippines_affiliates_matrix <- d_emcds[, Philippines_affiliate_countries]

# Change to a dataframe so that the multiplication works
Philippines_affiliates_matrix <- data.frame(Philippines_affiliates_matrix)

Philippines <- rowSums(data.frame(mapply(`*`,Philippines_affiliates_matrix,Philippines_weights)) )
Philippines


Thailand_affiliate_countries <- EastAsia$COUNTRY[!EastAsia $COUNTRY %in% "Thailand"]
Thailand_affiliate_countries

Thailand_weights <- subset(EastAsia, COUNTRY == "Thailand")
Thailand_weights 

Thailand_weights <- Thailand_weights[,Thailand_affiliate_countries]
Thailand_weights

sum(Thailand_weights) # just a check

# Subset for affiliates countries
Thailand_affiliates_matrix <- d_emcds[, Thailand_affiliate_countries]

# Change to a dataframe so that the multiplication works
Thailand_affiliates_matrix <- data.frame(Thailand_affiliates_matrix)

Thailand <- rowSums(data.frame(mapply(`*`,Thailand_affiliates_matrix,Thailand_weights)) )
Thailand


regions
Europe$COUNTRY

Czechia_affiliate_countries <- Europe$COUNTRY[!Europe $COUNTRY %in% "Czechia"]
Czechia_affiliate_countries

Czechia_weights <- subset(Europe, COUNTRY == "Czechia")
Czechia_weights 

Czechia_weights <- Czechia_weights[,Czechia_affiliate_countries]
Czechia_weights

sum(Czechia_weights) # just a check

# Subset for affiliates countries
Czechia_affiliates_matrix <- d_emcds[, Czechia_affiliate_countries]

# Change to a dataframe so that the multiplication works
Czechia_affiliates_matrix <- data.frame(Czechia_affiliates_matrix)

Czechia <- rowSums(data.frame(mapply(`*`,Czechia_affiliates_matrix,Czechia_weights)) )
Czechia


Hungary_affiliate_countries <- Europe$COUNTRY[!Europe $COUNTRY %in% "Hungary"]
Hungary_affiliate_countries

Hungary_weights <- subset(Europe, COUNTRY == "Hungary")
Hungary_weights 

Hungary_weights <- Hungary_weights[,Hungary_affiliate_countries]
Hungary_weights

sum(Hungary_weights) # just a check

# Subset for affiliates countries
Hungary_affiliates_matrix <- d_emcds[, Hungary_affiliate_countries]

# Change to a dataframe so that the multiplication works
Hungary_affiliates_matrix <- data.frame(Hungary_affiliates_matrix)

Hungary <- rowSums(data.frame(mapply(`*`,Hungary_affiliates_matrix,Hungary_weights)) )
Hungary


Poland_affiliate_countries <- Europe$COUNTRY[!Europe $COUNTRY %in% "Poland"]
Poland_affiliate_countries

Poland_weights <- subset(Europe, COUNTRY == "Poland")
Poland_weights 

Poland_weights <- Poland_weights[,Poland_affiliate_countries]
Poland_weights

sum(Poland_weights) # just a check

# Subset for affiliates countries
Poland_affiliates_matrix <- d_emcds[, Poland_affiliate_countries]

# Change to a dataframe so that the multiplication works
Poland_affiliates_matrix <- data.frame(Poland_affiliates_matrix)

Poland <- rowSums(data.frame(mapply(`*`,Poland_affiliates_matrix,Poland_weights)) )
Poland


Romania_affiliate_countries <- Europe$COUNTRY[!Europe $COUNTRY %in% "Romania"]
Romania_affiliate_countries

Romania_weights <- subset(Europe, COUNTRY == "Romania")
Romania_weights 

Romania_weights <- Romania_weights[,Romania_affiliate_countries]
Romania_weights

sum(Romania_weights) # just a check

# Subset for affiliates countries
Romania_affiliates_matrix <- d_emcds[, Romania_affiliate_countries]

# Change to a dataframe so that the multiplication works
Romania_affiliates_matrix <- data.frame(Romania_affiliates_matrix)

Romania <- rowSums(data.frame(mapply(`*`,Romania_affiliates_matrix,Romania_weights)) )
Romania


Turkey_affiliate_countries <- Europe$COUNTRY[!Europe $COUNTRY %in% "Turkey"]
Turkey_affiliate_countries

Turkey_weights <- subset(Europe, COUNTRY == "Turkey")
Turkey_weights 

Turkey_weights <- Turkey_weights[,Turkey_affiliate_countries]
Turkey_weights

sum(Turkey_weights) # just a check

# Subset for affiliates countries
Turkey_affiliates_matrix <- d_emcds[, Turkey_affiliate_countries]

# Change to a dataframe so that the multiplication works
Turkey_affiliates_matrix <- data.frame(Turkey_affiliates_matrix)

Turkey <- rowSums(data.frame(mapply(`*`,Turkey_affiliates_matrix,Turkey_weights)) )
Turkey


Ukraine_affiliate_countries <- Europe$COUNTRY[!Europe $COUNTRY %in% "Ukraine"]
Ukraine_affiliate_countries

Ukraine_weights <- subset(Europe, COUNTRY == "Ukraine")
Ukraine_weights 

Ukraine_weights <- Ukraine_weights[,Ukraine_affiliate_countries]
Ukraine_weights

sum(Ukraine_weights) # just a check

# Subset for affiliates countries
Ukraine_affiliates_matrix <- d_emcds[, Ukraine_affiliate_countries]

# Change to a dataframe so that the multiplication works
Ukraine_affiliates_matrix <- data.frame(Ukraine_affiliates_matrix)

Ukraine <- rowSums(data.frame(mapply(`*`,Ukraine_affiliates_matrix,Ukraine_weights)) )
Ukraine



regions
LATAM$COUNTRY

Argentina_affiliate_countries <- LATAM$COUNTRY[!LATAM $COUNTRY %in% "Argentina"]
Argentina_affiliate_countries

Argentina_weights <- subset(LATAM, COUNTRY == "Argentina")
Argentina_weights 

Argentina_weights <- Argentina_weights[,Argentina_affiliate_countries]
Argentina_weights

sum(Argentina_weights) # just a check

# Subset for affiliates countries
Argentina_affiliates_matrix <- d_emcds[, Argentina_affiliate_countries]

# Change to a dataframe so that the multiplication works
Argentina_affiliates_matrix <- data.frame(Argentina_affiliates_matrix)

Argentina <- rowSums(data.frame(mapply(`*`,Argentina_affiliates_matrix,Argentina_weights)) )
Argentina


Brazil_affiliate_countries <- LATAM$COUNTRY[!LATAM $COUNTRY %in% "Brazil"]
Brazil_affiliate_countries

Brazil_weights <- subset(LATAM, COUNTRY == "Brazil")
Brazil_weights 

Brazil_weights <- Brazil_weights[,Brazil_affiliate_countries]
Brazil_weights

sum(Brazil_weights) # just a check

# Subset for affiliates countries
Brazil_affiliates_matrix <- d_emcds[, Brazil_affiliate_countries]

# Change to a dataframe so that the multiplication works
Brazil_affiliates_matrix <- data.frame(Brazil_affiliates_matrix)

Brazil <- rowSums(data.frame(mapply(`*`,Brazil_affiliates_matrix,Brazil_weights)) )
Brazil


Chile_affiliate_countries <- LATAM$COUNTRY[!LATAM $COUNTRY %in% "Chile"]
Chile_affiliate_countries

Chile_weights <- subset(LATAM, COUNTRY == "Chile")
Chile_weights 

Chile_weights <- Chile_weights[,Chile_affiliate_countries]
Chile_weights

sum(Chile_weights) # just a check

# Subset for affiliates countries
Chile_affiliates_matrix <- d_emcds[, Chile_affiliate_countries]

# Change to a dataframe so that the multiplication works
Chile_affiliates_matrix <- data.frame(Chile_affiliates_matrix)

Chile <- rowSums(data.frame(mapply(`*`,Chile_affiliates_matrix,Chile_weights)) )
Chile


Colombia_affiliate_countries <- LATAM$COUNTRY[!LATAM $COUNTRY %in% "Colombia"]
Colombia_affiliate_countries

Colombia_weights <- subset(LATAM, COUNTRY == "Colombia")
Colombia_weights 

Colombia_weights <- Colombia_weights[,Colombia_affiliate_countries]
Colombia_weights

sum(Colombia_weights) # just a check

# Subset for affiliates countries
Colombia_affiliates_matrix <- d_emcds[, Colombia_affiliate_countries]

# Change to a dataframe so that the multiplication works
Colombia_affiliates_matrix <- data.frame(Colombia_affiliates_matrix)

Colombia <- rowSums(data.frame(mapply(`*`,Colombia_affiliates_matrix,Colombia_weights)) )
Colombia


Dominican.Republic_affiliate_countries <- LATAM$COUNTRY[!LATAM$COUNTRY %in% "Dominican Republic"]
Dominican.Republic_affiliate_countries

Dominican.Republic_weights <- subset(LATAM, COUNTRY == "Dominican Republic")
Dominican.Republic_weights 

Dominican.Republic_weights <- Dominican.Republic_weights[,Dominican.Republic_affiliate_countries]
Dominican.Republic_weights

sum(Dominican.Republic_weights) # just a check

# Subset for affiliates countries
Dominican.Republic_affiliates_matrix <- d_emcds[, Dominican.Republic_affiliate_countries]

# Change to a dataframe so that the multiplication works
Dominican.Republic_affiliates_matrix <- data.frame(Dominican.Republic_affiliates_matrix)

Dominican.Republic <- rowSums(data.frame(mapply(`*`,Dominican.Republic_affiliates_matrix,Dominican.Republic_weights)) )
Dominican.Republic


Mexico_affiliate_countries <- LATAM$COUNTRY[!LATAM$COUNTRY %in% "Mexico"]
Mexico_affiliate_countries

Mexico_weights <- subset(LATAM, COUNTRY == "Mexico")
Mexico_weights 

Mexico_weights <- Mexico_weights[,Mexico_affiliate_countries]
Mexico_weights

sum(Mexico_weights) # just a check

# Subset for affiliates countries
Mexico_affiliates_matrix <- d_emcds[, Mexico_affiliate_countries]

# Change to a dataframe so that the multiplication works
Mexico_affiliates_matrix <- data.frame(Mexico_affiliates_matrix)

Mexico <- rowSums(data.frame(mapply(`*`,Mexico_affiliates_matrix,Mexico_weights)) )
Mexico


Panama_affiliate_countries <- LATAM$COUNTRY[!LATAM$COUNTRY %in% "Panama"]
Panama_affiliate_countries

Panama_weights <- subset(LATAM, COUNTRY == "Panama")
Panama_weights 

Panama_weights <- Panama_weights[,Panama_affiliate_countries]
Panama_weights

sum(Panama_weights) # just a check

# Subset for affiliates countries
Panama_affiliates_matrix <- d_emcds[, Panama_affiliate_countries]

# Change to a dataframe so that the multiplication works
Panama_affiliates_matrix <- data.frame(Panama_affiliates_matrix)

Panama <- rowSums(data.frame(mapply(`*`,Panama_affiliates_matrix,Panama_weights)) )
Panama


Peru_affiliate_countries <- LATAM$COUNTRY[!LATAM$COUNTRY %in% "Peru"]
Peru_affiliate_countries

Peru_weights <- subset(LATAM, COUNTRY == "Peru")
Peru_weights 

Peru_weights <- Peru_weights[,Peru_affiliate_countries]
Peru_weights

sum(Peru_weights) # just a check

# Subset for affiliates countries
Peru_affiliates_matrix <- d_emcds[, Peru_affiliate_countries]

# Change to a dataframe so that the multiplication works
Peru_affiliates_matrix <- data.frame(Peru_affiliates_matrix)

Peru <- rowSums(data.frame(mapply(`*`,Peru_affiliates_matrix,Peru_weights)) )
Peru

Uruguay_affiliate_countries <- LATAM$COUNTRY[!LATAM$COUNTRY %in% "Uruguay"]
Uruguay_affiliate_countries

Uruguay_weights <- subset(LATAM, COUNTRY == "Uruguay")
Uruguay_weights 

Uruguay_weights <- Uruguay_weights[,Uruguay_affiliate_countries]
Uruguay_weights

sum(Uruguay_weights) # just a check

# Subset for affiliates countries
Uruguay_affiliates_matrix <- d_emcds[, Uruguay_affiliate_countries]

# Change to a dataframe so that the multiplication works
Uruguay_affiliates_matrix <- data.frame(Uruguay_affiliates_matrix)

Uruguay <- rowSums(data.frame(mapply(`*`,Uruguay_affiliates_matrix,Uruguay_weights)) )
Uruguay

regions
MiddleEast$COUNTRY

Bahrain_affiliate_countries <- MiddleEast$COUNTRY[!MiddleEast$COUNTRY %in% "Bahrain"]
Bahrain_affiliate_countries

Bahrain_weights <- subset(MiddleEast, COUNTRY == "Bahrain")
Bahrain_weights 

Bahrain_weights <- Bahrain_weights[,Bahrain_affiliate_countries]
Bahrain_weights

sum(Bahrain_weights) # just a check

# Subset for affiliates countries
Bahrain_affiliates_matrix <- d_emcds[, Bahrain_affiliate_countries]

# Change to a dataframe so that the multiplication works
Bahrain_affiliates_matrix <- data.frame(Bahrain_affiliates_matrix)

Bahrain <- rowSums(data.frame(mapply(`*`,Bahrain_affiliates_matrix,Bahrain_weights)) )
Bahrain


Qatar_affiliate_countries <- MiddleEast$COUNTRY[!MiddleEast$COUNTRY %in% "Qatar"]
Qatar_affiliate_countries

Qatar_weights <- subset(MiddleEast, COUNTRY == "Qatar")
Qatar_weights 

Qatar_weights <- Qatar_weights[,Qatar_affiliate_countries]
Qatar_weights

sum(Qatar_weights) # just a check

# Subset for affiliates countries
Qatar_affiliates_matrix <- d_emcds[, Qatar_affiliate_countries]

# Change to a dataframe so that the multiplication works
Qatar_affiliates_matrix <- data.frame(Qatar_affiliates_matrix)

Qatar <- rowSums(data.frame(mapply(`*`,Qatar_affiliates_matrix,Qatar_weights)) )
Qatar


Saudi.Arabia_affiliate_countries <- MiddleEast$COUNTRY[!MiddleEast$COUNTRY %in% "Saudi Arabia"]
Saudi.Arabia_affiliate_countries

Saudi.Arabia_weights <- subset(MiddleEast, COUNTRY == "Saudi Arabia")
Saudi.Arabia_weights 

Saudi.Arabia_weights <- Saudi.Arabia_weights[,Saudi.Arabia_affiliate_countries]
Saudi.Arabia_weights

sum(Saudi.Arabia_weights) # just a check

# Subset for affiliates countries
Saudi.Arabia_affiliates_matrix <- d_emcds[, Saudi.Arabia_affiliate_countries]

# Change to a dataframe so that the multiplication works
Saudi.Arabia_affiliates_matrix <- data.frame(Saudi.Arabia_affiliates_matrix)

Saudi.Arabia <- rowSums(data.frame(mapply(`*`,Saudi.Arabia_affiliates_matrix,Saudi.Arabia_weights)) )
Saudi.Arabia



regions
SouthAsia$COUNTRY

India_affiliate_countries <- SouthAsia$COUNTRY[!SouthAsia$COUNTRY %in% "India"]
India_affiliate_countries

India_weights <- subset(SouthAsia, COUNTRY == "India")
India_weights 

India_weights <- India_weights[,India_affiliate_countries]
India_weights

sum(India_weights) # just a check

# Subset for affiliates countries
India_affiliates_matrix <- d_emcds[, India_affiliate_countries]

# Change to a dataframe so that the multiplication works
India_affiliates_matrix <- data.frame(India_affiliates_matrix)

India <- rowSums(data.frame(mapply(`*`,India_affiliates_matrix,India_weights)) )
India


Sri.Lanka_affiliate_countries <- SouthAsia$COUNTRY[!SouthAsia$COUNTRY %in% "Sri Lanka"]
Sri.Lanka_affiliate_countries

Sri.Lanka_weights <- subset(SouthAsia, COUNTRY == "Sri Lanka")
Sri.Lanka_weights 

Sri.Lanka_weights <- Sri.Lanka_weights[,Sri.Lanka_affiliate_countries]
Sri.Lanka_weights

sum(Sri.Lanka_weights) # just a check

# Subset for affiliates countries
Sri.Lanka_affiliates_matrix <- d_emcds[, Sri.Lanka_affiliate_countries]

# Change to a dataframe so that the multiplication works
Sri.Lanka_affiliates_matrix <- data.frame(Sri.Lanka_affiliates_matrix)

Sri.Lanka <- rowSums(data.frame(mapply(`*`,Sri.Lanka_affiliates_matrix,Sri.Lanka_weights)) )
Sri.Lanka



# This was the old way before we started weighting things
# em_fac<-matrix(NA,nrow=nrow(d_emcds),ncol=ncol(d_emcds)) # create an EM common factor excluding country i
# for(i in 1:ncol(em_fac)){
#   em_fac[,i]<-rowMeans(d_emcds[,-i])
# }

# This is the new way when we start weighting things
# First I create an empty matrix of 30 countries X the number of differenced log CDS spreads
em_fac<-matrix(NA,nrow=nrow(d_emcds),ncol=ncol(d_emcds)) 

# Then I rename the column names to keep track of the individual columns
colnames(em_fac) <- colnames(d_emcds)

# Then I fill up all the columns with the right data
Africa$COUNTRY
em_fac[, "Egypt"] <- Egypt
em_fac[, "Ghana"] <- Ghana
em_fac[, "South Africa"] <- South.Africa

CentralAsia$COUNTRY
em_fac[, "Kazakhstan"] <- Kazakhstan
em_fac[, "Russia"] <- Russia

EastAsia$COUNTRY
em_fac[, "China"] <- China
em_fac[, "Indonesia"] <- Indonesia
em_fac[, "Malaysia"] <- Malaysia
em_fac[, "Philippines"] <- Philippines
em_fac[, "Thailand"] <- Thailand

Europe$COUNTRY
em_fac[, "Czechia"] <- Czechia
em_fac[, "Hungary"] <- Hungary
em_fac[, "Poland"] <- Poland
em_fac[, "Romania"] <- Romania
em_fac[, "Turkey"] <- Turkey
em_fac[, "Ukraine"] <- Ukraine

LATAM$COUNTRY
em_fac[, "Argentina"] <- Argentina
em_fac[, "Brazil"] <- Brazil
em_fac[, "Chile"] <- Chile
em_fac[, "Colombia"] <- Colombia
em_fac[, "Dominican Republic"] <- Dominican.Republic
em_fac[, "Mexico"] <- Mexico
em_fac[, "Panama"] <- Panama
em_fac[, "Peru"] <- Peru
em_fac[, "Uruguay"] <- Uruguay

MiddleEast$COUNTRY
em_fac[, "Bahrain"] <- Bahrain
em_fac[, "Qatar"] <- Qatar
em_fac[, "Saudi Arabia"] <- Saudi.Arabia

SouthAsia$COUNTRY
em_fac[, "India"] <- India
em_fac[, "Sri Lanka"] <- Sri.Lanka

# Now that I am done with the steps above, I need to create a long panel of them and then match them to the panel_for_revised_paper
Date <- as.data.frame(unique(panel_for_revised_paper$Date) )
colnames(Date) <- "Date"
em_fac <- cbind(Date, em_fac )
em_fac <- gather(em_fac, "Country", "GDPweightedregionalpeersCDSlogchanges", -"Date")

# Join it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, em_fac, by = c("Date" = "Date", "Country" = "Country")  )

# Create a safetycopy
# safetywithGDPweightedpeersregionalCDS <- panel_for_revised_paper
# panel_for_revised_paper <- safetywithGDPweightedpeersregionalCDS




# Adding the PMI data -----------------------------------------------------



vis_dat(panel_for_revised_paper, warn_large_data = F)




