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
# checking the previous work 
str(em_cds)
# view(em_cds)
# view(em_cds[em_cds$Country=="Uruguay",])
head(em_cds)
tail(em_cds)
str(unique(em_cds$Date))

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
# view(panel_for_revised_paper)
# view(panel_for_revised_paper[panel_for_revised_paper$Country=="Uruguay",])

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
# view(VIX[VIX$Country=="China",])
# head(VIX)
# str(VIX)
# view(VIX)
# Joining the VIX changes (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, VIX, by = c("Country" = "Country", "Date" = "Date")  )
head(panel_for_revised_paper)

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
# view(BRENT[BRENT$Country=="Uruguay",])
# Joining the BRENT changes (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, BRENT, by = c("Country" = "Country", "Date" = "Date")  )
head(panel_for_revised_paper)

# Here I create a safetycopy of all the previous steps so far so that when I mess around with panel_for_revised_paper I could always 
# reset to this point. 
# asdfyerjylckj  <- panel_for_revised_paper
panel_for_revised_paper <- asdfyerjylckj



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
# view(colnames(panel_for_revised_paper))
# view(colnames(panel_for_revised_paper2))
# Now I fill up the missing values in the early dates for all those columns for which I know this makes sense
for(i in c(19:22, 24:30, 33:54, 87:102)  ) {       # for-loop over columns
  panel_for_revised_paper[panel_for_revised_paper$Date==as.Date("2014-01-01") , i] <- 0
  panel_for_revised_paper <- panel_for_revised_paper %>% fill(i) }
# Now I fill up the missing values in the early dates for all those columns for which I know that they stay constant across time, for example latitude
for(i in c(23, 31:32, 55:56)  ) {       # for-loop over columns
  # panel_for_revised_paper[panel_for_revised_paper$Date==as.Date("2014-01-01") , i] <- 0
  panel_for_revised_paper <- panel_for_revised_paper %>% fill(i, .direction = "up") }
# Now I subset for variables that I need and toss out the rest. 
panel_for_revised_paper <- panel_for_revised_paper[, c(1:26, 31:32, 51:52, 60:62, 87:102)]
str(panel_for_revised_paper)





# # With this comand, I can see where we still have missing data. This seems to be the case for transit, walking, and driving, which is not a problem. 
# vis_dat(panel_for_revised_paper, warn_large_data = F)
# view(colnames(panel_for_revised_paper))

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
# Now I can join the Oxford_V1 database that contain many of the explanatory variables to the 
panel_for_revised_paper <- left_join(panel_for_revised_paper, Government_Response, by = c("Country" = "Country", "Date" = "Date")  )
# Creating a safety copy
# safetycopynumber534857 <- panel_for_revised_paper    
# panel_for_revised_paper <- safetycopynumber534857

# Now I can create the Strinencyindex growth as SI_Growth
# First, I set the NAs to zero
panel_for_revised_paper$StringencyIndex[is.na(panel_for_revised_paper$StringencyIndex)] <- 0
# Then I create the growth number
panel_for_revised_paper <- panel_for_revised_paper %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(SI_Growth = (StringencyIndex - Lag(StringencyIndex,1))/Lag(StringencyIndex,1))
# The first number is a NA since it does not have a divisor. So I set it to zero
panel_for_revised_paper$SI_Growth[is.na(panel_for_revised_paper$SI_Growth)] <- 0
# There are a few SI_Growth observations that are inf as the divisor is zero. I don't know what to do about it. 
table(panel_for_revised_paper$SI_Growth)
# panel_for_revised_paper$SI_Growth[is.infinite(panel_for_revised_paper$SI_Growth)] <- 0
# table(panel_for_revised_paper$SI_Growth)
# view(panel_for_revised_paper[panel_for_revised_paper$Country=="Argentina", ])
# safetycopynumber5yxcewercc <- panel_for_revised_paper    
# panel_for_revised_paper <- safetycopynumber5yxcewercc


# For some reason, I need to change the country names to do the merge. I will set them back later. 
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "Dominican Republic"] <- "DominicanRepublic"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "Saudi Arabia"] <- "SaudiArabia"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "South Africa"] <- "SouthAfrica"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "Sri Lanka"] <- "SriLanka"

# Adding additional variables such as oil income effects ------------------
# Importing the net oil exports as a share of GDP
net_oil_exp_of_GDP <- as.data.frame(read_excel("Data/OilshareofGDP.xlsx", sheet = "R") )

# Joining it to the panel
panel_for_revised_paper <- left_join(panel_for_revised_paper, net_oil_exp_of_GDP, by = c("Country" = "Country")  )

# Creating the oil income effect variable by multiplying the net oil exports as a share of GDP with log changes in Brent prices
panel_for_revised_paper <- panel_for_revised_paper %>% mutate(oil_income_price_effect =  net_oil_exp_of_GDP*changes.log.BRENT) 

# Now I change the names back again
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "DominicanRepublic"] <- "Dominican Republic"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "SaudiArabia"] <- "Saudi Arabia"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "SouthAfrica"] <- "South Africa"
panel_for_revised_paper$Country[panel_for_revised_paper$Country == "SriLanka"] <- "Sri Lanka"
unique(panel_for_revised_paper$Country)
vis_dat(panel_for_revised_paper, warn_large_data = F)
colnames(panel_for_revised_paper)

# safetycopy
ycqwe3yeyadas <- panel_for_revised_paper
panel_for_revised_paper <- ycqwe3yeyadas 




# Adding international reserves to the panel ------------------------------
# Importing Reserves
Reserves <- gather(as.data.frame(read_excel("Data/International_Liquidity.xlsx", sheet = "R") ), "Date", "Reserves_in_millions", -"Country"   )
# Getting the right Date format
Reserves$Date <- as.numeric(Reserves$Date)
Reserves$Date <- as.Date(Reserves$Date, origin = "1899-12-30")
# Multiplying values by 1 million to ger normal number rather than million scale
Reserves$Reserves_in_millions <- as.numeric(Reserves$Reserves_in_millions)
Reserves <- Reserves %>% mutate(Reserves = Reserves_in_millions *1000000  )
# Subsetting for only those 30 EMs for which we need data
Reserves <- Reserves[Reserves$Country %in% listofneededcountries, ]
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, Reserves, by = c("Country" = "Country", "Date" = "Date")  )
# Filling up the missing values of the month from the End-of-Month-value
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Reserves, .direction = "up")
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Reserves_in_millions, .direction = "up")
# This worked well. The only country for which there is no Data is Peru after March 2020. But I just project these values forward into the future. 
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Reserves)
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Reserves_in_millions)
# view(panel_for_revised_paper[panel_for_revised_paper$Country=="China", c("Date", "Country", "Reserves")])
# str(panel_for_revised_paper)
# summary(panel_for_revised_paper$Reserves)
# view(panel_for_revised_paper[is.na(panel_for_revised_paper$Reserves),])
# listofneededcountries <- unique(panel_for_revised_paper$Country)
# Reserves <- Reserves[Reserves$Country %in% listofneededcountries, ]
# unique(Reserves$Country)  == unique(panel_for_revised_paper$Country)




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
colnames(SWF_Countryaggregates) <- c("Country", "SWF_2019_AuMinUSD")
SWF_Countryaggregates
unique(SWF_Countryaggregates$Country)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, SWF_Countryaggregates, by = c("Country" = "Country")  )
# Filling up the missing values for those countries with no sovereign wealth fund with zeros
panel_for_revised_paper$SWF_2019_AuMinUSD[is.na(panel_for_revised_paper$SWF_2019_AuMinUSD)] <- 0
# Checking if the data looks complete
vis_dat(panel_for_revised_paper, warn_large_data = F)
# Creating a safetycopy
# alxlqllxrrrt <- panel_for_revised_paper
panel_for_revised_paper <- alxlqllxrrrt





# Adding GDP to the panel -------------------------------------------------
# Importing GDP
GDP_Worldbank <- gather(as.data.frame(read_excel("Data/GDP_WorldBank.xlsx", sheet = "R") ), "Date", "Annual_GDP", -"Country"   )
# Getting the right date format
GDP_Worldbank$Date <- as.Date(GDP_Worldbank$Date)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, GDP_Worldbank, by = c("Country" = "Country", "Date" = "Date")  )
# Since GDP data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the GDP data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Annual_GDP, .direction = "up") 
# vis_dat(panel_for_revised_paper, warn_large_data = F)
# table(panel_for_revised_paper$GDP)



# Adding foreign currency share of external debt to panel ------------------------
# Importing External debt in foreign currency, % of total
Externaldebtforeigncurrencyshareoftotalexternaldebt <- gather(as.data.frame(read_excel("Data/External debt in foreign currency, % of total.xlsx", sheet = "R") ), "Date", "External_debt_foreign_currency_share_of_total_external_debt", -"Country"   )
# Getting the right date format
Externaldebtforeigncurrencyshareoftotalexternaldebt$Date <- as.Date(Externaldebtforeigncurrencyshareoftotalexternaldebt$Date)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, Externaldebtforeigncurrencyshareoftotalexternaldebt, by = c("Country" = "Country", "Date" = "Date")  )
# Since external debt data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the external debt data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% fill(External_debt_foreign_currency_share_of_total_external_debt, .direction = "up") 
# vis_dat(panel_for_revised_papertest, warn_large_data = F)
# table(panel_for_revised_papertest$Externaldebtforeigncurrencyshareoftotalexternaldebt)
# countries <- read_excel("Data/laender.xlsx", sheet = "EM")
# allCountriesSWF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
# colnames(allCountriesSWF) <- "COUNTRY"
# # Subsetting countries with external debt that are in sample
# Externaldebtforeigncurrencyshareoftotalexternaldebt <- Externaldebtforeigncurrencyshareoftotalexternaldebt[Externaldebtforeigncurrencyshareoftotalexternaldebt$Country %in% allCountriesSWF$COUNTRY,  ]
# unique(Externaldebtforeigncurrencyshareoftotalexternaldebt$Country)
# table((panel_for_revised_papertest[is.na(panel_for_revised_papertest$Externaldebtforeigncurrencyshareoftotalexternaldebt),c("Externaldebtforeigncurrencyshareoftotalexternaldebt","Date","Country")])$Country)



# Adding external debt GDP share to panel -------------------------------------------
# Data for Bahrain and Qatar is not avaialble!!
# Importing external debt GDP share to panel
TotalexternaldebtasshareofGDP <- gather(as.data.frame(read_excel("Data/Total external debt stocks, % of GDP.xlsx", sheet = "R") ), "Date", "Total_external_debt_as_share_of_GDP", -"Country"   )
# Getting the right date format
TotalexternaldebtasshareofGDP$Date <- as.Date(TotalexternaldebtasshareofGDP$Date)
# Joining it to the panel_for_revised_paper
panel_for_revised_paper <- left_join(panel_for_revised_paper, TotalexternaldebtasshareofGDP, by = c("Country" = "Country", "Date" = "Date")  )
# Since external debt data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the external debt data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Total_external_debt_as_share_of_GDP, .direction = "up") 
# vis_dat(panel_for_revised_paper, warn_large_data = F)
# table(panel_for_revised_paper$TotalexternaldebtasshareofGDP)
# safetcopy24542 <- panel_for_revised_paper
panel_for_revised_paper <- safetcopy24542



# Adding China debt data to panel -----------------------------------------
# Data for many countries not available. For those that it is available, it ends in 2017. As this is possibly a slow moving variable, I extrapolated it into the future.
# Importing China debt data
DebtToChina <- as.data.frame(read_excel("Data/HRT _ China Debt Stock Database_April2020.xlsx", sheet = "R") )
# Getting the right date format
DebtToChina$Date <- as.Date(DebtToChina$Date)
str(DebtToChina)
panel_for_revised_paper <- left_join(panel_for_revised_paper, DebtToChina, by = c("Country" = "Country", "Date" = "Date")  )
# Since external debt data is always year end data, I fill up the empty observations of each year with the data on the 31.12.xxxx of that year. That is, for every day of for example 2014, the external debt data is the end of year 2014 data.
panel_for_revised_paper <- panel_for_revised_paper %>% fill(Debt_to_China_as_share_of_GDP, .direction = "up") 
# panel_for_revised_paper <- panel_for_revised_paper %>% fill(Debt_to_China_as_share_of_GDP) 
panel_for_revised_paper <- panel_for_revised_paper %>% fill(ChinaDebt_USD, .direction = "up") 
# panel_for_revised_paper <- panel_for_revised_paper %>% fill(ChinaDebt_USD) 
# vis_dat(panel_for_revised_paper, warn_large_data = F)
# view(panel_for_revised_paper[panel_for_revised_paper$Country == "Uruguay",c("Country", "Date", "Debt_to_China_as_share_of_GDP", "ChinaDebt_USD")])
# tail(panel_for_revised_paper)
vis_dat(panel_for_revised_paper, warn_large_data = F)
view(unique(panel_for_revised_paper$Country))







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
view(ExportData)
table(ExportData$`Reporter Name`)
# This looks somewhat odd. Every country should only have 29 partners, not 30. 
# Thus, I manually check the excel sheets for those countries for which the country shows up
# as its own partner. In all cases, the actual numbers are zero, i.e. Brazil rightly exports 0 to Brazil. 
# However, it is still vexing that these entries are in there, so I delete them by subsetting for observations
# where the reporting country and the partner country are not the dame.
ExportData <- subset(ExportData,`Reporter Name` != `Partner Name`)
table(ExportData$`Reporter Name`) # This worked well

# I rename the columns
colnames(ExportData) <- c("Exporter", "Partner", "Tradeflow", "Indicator", "Exports2014", "Exports2015", "Exports2016", "Exports2017", "Exports2018")
# Now I check if we have any NAs
summary(ExportData) # It appears that the least sparse expor column is for 2016
# view(subset(ExportData, is.na(ExportData$Exports2016) )) # It appears that those country pairs where data is missing are negligeable. I set them equal to zero
ExportData$Exports2016[is.na(ExportData$Exports2016)] <- 0

# view(ExportData[ExportData$Exporter=="Argentina", c("Exporter", "Partner", "Exports2016") ] )
# aggregate(ExportData$Exports2016, by=list(Category=ExportData$Exporter), FUN=sum)

# Now I sum up all the shars of a country's partners for 2016. 
Sumofexportshares <- aggregate(Exports2016 ~ Exporter, ExportData, sum)
colnames(Sumofexportshares) <- c("Exporter", "SumofExportsharsof2016")

ExportData <- left_join(ExportData, Sumofexportshares, by = c("Exporter" = "Exporter")  ) 

ExportData <- ExportData %>%
mutate(Shareoftotalexports = (Exports2016 / SumofExportsharsof2016) )
view(ExportData)


ExportData <- ExportData[, c("Exporter", "Partner", "Shareoftotalexports")]
# aggregate(Shareoftotalexports ~ Exporter, ExportData, sum) # works






mybiglist <- list()
for(i in 1:nrow(my_data.frame) ) {   
NameofExporter <- my_data.frame[i,1]
name <- NameofExporter
tmp <- as.data.frame(read_excel(paste0("Data/",NameofExporter,".xlsx"), sheet = "Partner-Timeseries") )
mybiglist[[name]] <- tmp
}

ExportData  <- NULL
for(i in 1:nrow(my_data.frame) ) { 
  name <- my_data.frame[i,1]
  tmp <- as.data.frame(mybiglist[[i]] )
  ExportData <- dplyr::bind_rows(ExportData, tmp)
}

view(ExportData)





ExportData
y <- data.frame()
for(i in 1:nrow(my_data.frame) ) {   
name <- my_data.frame[i,1]
rbind(y, as.data.frame(mybiglist$my_data.frame[i,1]))
}






Argentina <- as.data.frame(mybiglist$Argentina)


adfasdfadf <- rbind()


mybiglist2 <- data.frame()
for(i in 2:nrow(my_data.frame) ) {   
  NameofExporter <- my_data.frame[i,1]
  # name <- NameofExporter
  tmp <- as.data.frame(mybiglist$NameofExporter )
  mybiglist2 <- dplyr::bind_rows(tmp)
}





str(mybiglist2)

Argentina <- as.data.frame(mybiglist[1])

for(i in 1:nrow(my_data.frame) ) {   
  NameofExporter <- my_data.frame[i,1]
  name <- NameofExporter
  tmp <- as.data.frame(mybiglist[i])
f <- rbind(Argentina,tmp)
}


dplyr::bind_rows(as.data.frame(mybiglist[1]), as.data.frame(mybiglist[2]))


df1 <- data.frame(a = c(1:5), b = c(6:10))
df2 <- data.frame(a = c(11:15), b = c(16:20), c = LETTERS[1:5])
dplyr::bind_rows(df1, df2)




f <- rbind(as.data.frame(mybiglist[1]), as.data.frame(mybiglist[2]) )


str(mybiglist)




Bahrain <- mybiglist$Bahrain  
str(Bahrain)





view(unique(panel_for_revised_paper$Country))



# Import the dataset
TradeShare <- as.data.frame(read_excel("Data/Argentina.xlsx", sheet = "Partner-Timeseries") )

# Import the dataset
TradeShare <- as.data.frame(read_excel("Data/Argentina.xlsx", sheet = "Partner-Timeseries") )
# Rename the partner names to standardize them
TradeShare$`Partner Name`[TradeShare$`Partner Name` == "Czech Republic"] <- "Czechia"
TradeShare$`Partner Name`[TradeShare$`Partner Name` == "Russian Federation"] <- "Russia"
TradeShare$`Partner Name`[TradeShare$`Partner Name` == "Egypt, Arab Rep."] <- "Egypt"


# Subset for the 30-1=29 partners because a country cannot be it's own partner. 
allCountriesSWF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
colnames(allCountriesSWF) <- "Country"
TradeShare <- TradeShare[TradeShare$`Partner Name` %in% allCountriesSWF$Country,  ]

paste(1:12)  


# unique(Externaldebtforeigncurrencyshareoftotalexternaldebt$Country)
# table((panel_for_revised_papertest[is.na(panel_for_revised_papertest$Externaldebtforeigncurrencyshareoftotalexternaldebt),c("Externaldebtforeigncurrencyshareoftotalexternaldebt","Date","Country")])$Country)











































countries <- read_excel("Data/laender.xlsx", sheet = "EM")
allCountriesSWF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
colnames(allCountriesSWF) <- "COUNTRY"
# Subsetting countries with external debt share that are in sample
DebtToChina <- DebtToChina[DebtToChina$Country %in% allCountriesSWF$COUNTRY,  ]
unique(DebtToChina$Country)
table((panel_for_revised_paper[is.na(panel_for_revised_paper$Debt_to_China_as_share_of_GDP),c("Debt_to_China_as_share_of_GDP","Date","Country")])$Country)
unique((panel_for_revised_paper[!is.na(panel_for_revised_paper$Debt_to_China_as_share_of_GDP),c("Debt_to_China_as_share_of_GDP","Date","Country")])$Country)


vis_dat(panel_for_revised_paper, warn_large_data = F)
# table(panel_for_revised_papertest$GDP)

panel_for_revised_paper[panel_for_revised_paper$Date==as.Date("2019-12-30"),c("GDP","Date","Country")]
table((panel_for_revised_paper[!is.na(panel_for_revised_paper$GDP),c("GDP","Date","Country")])$Country)

# Defining sample countries so that I can subset the SWF dataset 
countries <- read_excel("Data/laender.xlsx", sheet = "EM")
allCountriesSWF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
colnames(allCountriesSWF) <- "COUNTRY"
# Subsetting countries with SWF that are in sample
GDP_Worldbank <- GDP_Worldbank[GDP_Worldbank$Country %in% allCountriesSWF$COUNTRY,  ]
# Summing funds per country to get country total SWF
SWF_Countryaggregates <- aggregate(SWF$AuM, by=list(Category=SWF$COUNTRY), FUN=sum)
colnames(SWF_Countryaggregates) <- c("Country", "SWF_2019_AuMinUSD")
SWF_Countryaggregates
unique(GDP_Worldbank$Country)

import <-gather(as.data.frame( read_excel("2_Data/testimport.xlsx", col_names = TRUE) ), "Year", "GDPpc", -"Country Name")
paneladdition <- read_excel("Data/paneladdition.xlsx")
view(paneladdition)




# listofneededcountries <- unique(panel_for_revised_paper$Country)
# 
# 
# Government_Responseasdfasdf <- Government_Response[Government_Response$Country %in% listofneededcountries, ]
# 
# unique(Government_Responseasdfasdf$Country)
# unique(Government_Response$Country)
# 
# view(Government_Response$StringencyIndex)

# Oxford_V1$StringencyIndex[is.na(Oxford_V1$StringencyIndex)] <- 0
# Oxford_V1 <- Oxford_V1 %>%
#   dplyr::group_by(COUNTRY) %>%
#   dplyr::mutate(SI_Growth = (StringencyIndex - Lag(StringencyIndex,1))/Lag(StringencyIndex,1))
# Oxford_V1$SI_Growth[is.na(Oxford_V1$SI_Growth)] <- 0
# 
# 
# 
# view(Oxford_V1[, c("Date", "COUNTRY", "StringencyIndex", "SI_Growth")][is.infinite(Oxford_V1$SI_Growth),])
# 
# view(Government_Responses[, c("Date", "CountryName", "StringencyIndex")][Government_Responses$CountryName == "Brazil",])
# colnames(Government_Responses)
# 
# oxford$Country <- as.character(oxford$Country)
# 
# # xxx I have to add zeros here for the code to work 
# oxford$driving[is.na(oxford$driving)] <- 0
# oxford$SI_Growth[is.na(oxford$SI_Growth)] <- 0
# oxford$SI_Growth[is.na(oxford$SI_Growth)] <- 0
# oxford$SI_Growth[is.infinite(oxford$SI_Growth)] <- 0




# Saving a safety copy so that I canreset the panel if it gets lost along the way
# asdflkycppppoweijjiadf <- panel_for_revised_paper
panel_for_revised_paper <- asdflkycppppoweijjiadf

colnames(panel_for_revised_paper)





paneladdition <- read_excel("Data/paneladdition.xlsx")





SI_Growth Dummy_Fiscal_Country Dummy_Monetary_ECB Dummy_Monetary_Fed         IR










# paneladdition <- read_excel("Data/paneladdition.xlsx") # adding variables such as the oil, IMF suppot dummy, remittances, RFI, etc.
# panel <- merge(panel, paneladdition, by = c("Country", "Date"), all.x = TRUE )
# View(panel)
view(colnames(panel_for_revised_paper))



view(panel_for_revised_paper[is.na(panel_for_revised_paper$transit),])
a <- panel_for_revised_paper[!is.na(panel_for_revised_paper$transit),]
unique(a$Country)
b <- panel_for_revised_paper[!is.na(panel_for_revised_paper$walking),]
unique(b$Country)
c <- panel_for_revised_paper[!is.na(panel_for_revised_paper$driving),]
unique(c$Country)















view(colnames(panel_for_revised_paper))
# Now I can look at a few portions of the data to check if it looks good.
# view(head(panel_for_revised_paper))
# view(colnames(panel_for_revised_paper))
# view(head(panel_for_revised_paper))
# view(panel_for_revised_paper[panel_for_revised_paper$Country == "China", ])
# tail(panel_for_revised_paper)
# view(panel_for_revised_paper[panel_for_revised_paper$Date == as.Date("2020-04-16"), ])






d_nemds$Date <- unique(em_cds$Date)
d_nemds <- d_nemds[, c(21, 1:20)]

nonem_cds <- gather(d_nemds, "Country", "CDSlogchanges", -"Date")


# prepare data for regression ---------------------------------------------
# Importing GDP 2019 data for developed markets
GDP2019USDMIL_D <- read_excel("Data/GDP.xlsx", sheet = "GLOBAL")
total <- sum(GDP2019USDMIL_D$GDP2019MIL)
GDP2019USDMIL_D <- GDP2019USDMIL_D %>% mutate(weight = GDP2019MIL/ total  )

###Prepare data for regression
em_cds2<- em_cds
# xxx nonem_cds2 <- nonem_cds

# em_cds_test <- em_cds[which(em_cds$Date>="2020-06-20" & cds_five$Date<="2020-07-01"   ),]
# d_emcds_test<-apply(log(em_cds_test[,-1]),2,diff) #log differences of EM spreads
# View(em_cds_test)
# View(d_emcds_test)

d_emcds<-apply(log(em_cds2[,-1]),2,diff) #log differences of EM spreads
d_nemds<-apply(log(nonem_cds[,-1]),2,diff) # log differences of non-EM spreads



# This was the old way before we starting weighting things
# glo_cds <- as.data.frame(rowMeans(d_nemds) )# create a global CDS factor excluding EM
# I changed this on 11. September 2020 as I realized I had not weighted the global factor accordingly. 



# This is the new way when we start weighting things
d_nemds <- as.data.frame(d_nemds[,])
weighted_matrix <- d_nemds*GDP2019USDMIL_D$weight
glo_cds <- as.data.frame(rowMeans(weighted_matrix) )

# Weight of US in global factor
GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "US"), ]
US_Weight <- GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "US"), ]

# Weight of Japan in global factor
GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "Japan"), ]
Japan_Weight <- GDP2019USDMIL_D[which(GDP2019USDMIL_D$COUNTRY == "Japan"), ]

# Weight of Eurozone in global factor
1-(US_Weight$weight + Japan_Weight$weight)
EZ_Weight <- 1-(US_Weight$weight + Japan_Weight$weight)

# glo_cds_weighted <- 
a <- as.data.frame(c("Germany", "France"), c(100,40) )
countryexample <- c("Germany", "France")
countrynumbers <- c(100,40)
country.roster <- data.frame(countryexample, countrynumbers)

em_fac<-matrix(NA,nrow=nrow(d_emcds),ncol=ncol(d_emcds)) # create an EM common factor excluding country i
for(i in 1:ncol(em_fac)){
  em_fac[,i]<-rowMeans(d_emcds[,-i])
}

for(i in 1:ncol(em_fac)){
  em_fac[,i]<-rowMeans(d_emcds[,-i])
}




















myJoin$Rating[myJoin$Month=="1989-12-31"] <- "xoxoxo"
myJoin <- myJoin %>% fill(Rating)
myJoin <- myJoin %>% fill(Home)

# Removing first value
myJoin <- myJoin[!(myJoin$Month=="1989-12-31"),]

# 5) Removing all observations with no rating value 
myJoin <- myJoin[!(myJoin$Rating=="xoxoxo"),]




em_cds$value.lag1<-NA
levs<-levels(as.factor(em_cds$Country))
levs
for (i in 1:length(levs)) {
  temper<- subset(em_cds,Country==as.numeric(levs[i]))
  temper<- rbind(NA,temper[-nrow(temper),])  
  em_cds$value.lag1[df$Country==as.numeric(as.character(levs[i]))]<- temper
}



d <- data.frame( 
  User = rep( LETTERS[1:3], each=10 ),
  Date = seq.Date( Sys.Date(), length=30, by="day" ),
  Value = rep(1:10, 3)
)
library(plyr)
d <- ddply( 
  d, .(User), transform,
  # This assumes that the data is sorted
  Value.new = c( NA, Value[-length(Value)] ) 
)



df$value.lag1<-NA
levs<-levels(as.factor(df$User))
levs
for (i in 1:length(levs)) {
  temper<- subset(df,User==as.numeric(levs[i]))
  temper<- rbind(NA,temper[-nrow(temper),])  
  df$value.lag1[df$User==as.numeric(as.character(levs[i]))]<- temper
}

library(dplyr)
tlag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

em_cds <- em_cds %>% group_by(Country) %>% mutate(value_lagged = tlag(log.CDS, 1, time = Date))



em_cds <- em_cds %>% group_by(Country) %>% mutate(log.CDSlag = lag(log.CDS, order_by =Date) )
         

view(em_cds[em_cds$Country=="Argentina",])
str(em_cds)
view(em_cds)
     

result <- do.call("rbind", by(A, A$person, f))
result



df<-pdata.frame(em_cds,index=c("id","date"))  


em_cds <- em_cds %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(diff.CDS.lagged = lag(log.CDS, n = 2) )

str(em_cds)
view(em_cds)


d_emcds<-apply(log(em_cds2[,-1]),2,diff) #log differences of EM spreads


explanatoryVariable <- em_cds %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(GDPpc.12ma = rollapply(data = log.CDS, 
                                width = 12, 
                                FUN = mean, 
                                align = "left", 
                                fill = NA, 
                                na.rm = T)) %>%
  mutate(GDPpc.12ma.lagged = lag(GDPpc.12ma, n=12) )
view(explanatoryVariable)





em_cds$Date <- as.Date(em_cds$Date)



em_cds %>%
  mutate(log.CDS = log(CDS.level) ) 



em_cds <- em_cds %>%
  group_by(Country, Date) %>%
  mutate(log.CDS.lagged = lag(log.CDS) )

str(lag(em_cds$log.CDS))
str(em_cds$log.CDS)

view(em_cds)

explanatoryVariable <- import %>%
  group_by(Country) %>%
  arrange(Country, Month) %>%
  mutate(GDPpc.12ma = rollapply(data = GDPpc, 
                                width = 12, 
                                FUN = mean, 
                                align = "left", 
                                fill = NA, 
                                na.rm = T)) %>%
  mutate(GDPpc.12ma.lagged = lag(GDPpc.12ma, n=12) )


em_cds <- em_cds %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(GDPpc.12ma = rollapply(data = GDPpc, 
                                width = 12, 
                                FUN = mean, 
                                align = "left", 
                                fill = NA, 
                                na.rm = T)) %>%
  mutate(GDPpc.12ma.lagged = lag(GDPpc.12ma, n=12) )



explanatoryVariable <- import %>%
  group_by(Country) %>%
  arrange(Country, Month) %>%
  mutate(GDPpc.12ma = rollapply(data = GDPpc, 
                                width = 12, 
                                FUN = mean, 
                                align = "left", 
                                fill = NA, 
                                na.rm = T)) %>%
  mutate(GDPpc.12ma.lagged = lag(GDPpc.12ma, n=12) )


# ###### I don't think this is necessary
# Oxford_V1 <- read_excel("Data/Oxford_V1.xlsx")
# # safetyofthis <- Oxford_V1
# # Oxford_V1 <- safetyofthis
# Oxford_addition <- read_excel("Data/Oxford_V1_dummyts_additions.xlsx")
# # safetyofthat <- Oxford_addition
# # Oxford_addition <- safetyofthat
# Oxford_V1 <- as_tibble(merge(Oxford_V1, Oxford_addition, by=c("COUNTRY", "Date")))
# Oxford_V1 <- as.data.frame(Oxford_V1)
# # safetyoftheother <- Oxford_V1
# # Oxford_V1 <- safetyoftheother
# # View(Oxford_V1)
# # I don't think this is necessary up to here. 

