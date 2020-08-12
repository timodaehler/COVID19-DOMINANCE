# README ------------------------------------------------------------------
# Author: Timo Daehler, daehler@usc.edu
# Date of last update: 31 July 2020
# Inputs: -
# Outputs: -
# Other relevant notes: If you do not wish to run the code, please see the final output (graphs and datasets) provided in the Github repository. Make sure to check the relevant README files for the contents of the folder.
# =========================================================================.



# install packages --------------------------------------------------------
# Installing the relevant packages.
# install.packages("sf")
# install.packages("readr")
# install.packages("tmap") 
# install.packages("leaflet") 
# install.packages("mapview") 
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("rlang")
# install.packages("reshape")
# install.packages("rgdal")
# install.packages("lubridate")
# install.packages("plotly")
# install.packages("patchwork")
# install.packages("ggforce")
# install.packages("gridExtra")
# install.packages("htmltools")
# install.packages("data.table")
# install.packages("webshot")
# webshot::install_phantomjs()
# install.packages("runner")
# install.packages("zoo")
# install.packages("devtools")
# devtools::install_version("latticeExtra", version="0.6-28")
# install.packages("Hmisc")
# install.packages("DataCombine")
# install.packages("fastDummies")
# install.packages("heatmaply")
# install.packages("glmnet")
# install.packages("caret")
# install.packages("summarytools")
# install.packages("remote")
# remotes::install_github('rapporter/pander')
# install.packages("mlbench")
# install.packages("psych")
# install.packages("lmtest")
# install.packages("quantmod")
# install.packages("corrplot")
# install.packages("fBasics")
# install.packages("stargazer")
# install.packages("tseries")
# install.packages("vars")
# install.packages("gghighlight")
# =========================================================================.



# load libraries ----------------------------------------------------------
# In subsequent rounds of analysis, we will be integrating new libraries (we do not do so here to avoid conflicts between packages).
# library(data.table)
# library(quandl)
# library(readxl)
# library(zoo)
# library(xts)
# library(ggplot2)
# library(reshape2)
# library(gridExtra)
# library(grid)
# library(tseries)

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
# =========================================================================.



# set number format -------------------------------------------------------
#I don't want scientific notation for my values, so I specify this below. 
options(scipen = 999)
`%notin%` <- Negate(`%in%`)
# =========================================================================.

library(readxl)
cds_five <- read_excel("Data/CDS.xlsx", sheet = "5yrCDS")
countries <- read_excel("Data/laender.xlsx", sheet = "EM")

em_countries <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"   ]
non_em_countries <- countries[ which(countries$EM_dummy5yr !=1), "COUNTRY5yrCDS"   ]
cds_five <- cds_five[which(cds_five$Date>="2014-01-01" & cds_five$Date<="2020-07-01"   ),]

em_cds <- cds_five[, c("Date", em_countries$COUNTRY5yrCDS )]
nonem_cds<-cds_five[, c("Date", non_em_countries$COUNTRY5yrCDS )]


# countriesinoxforddataset <- as_tibble(unique(Oxford_V1$COUNTRY))
countrieswithcdsdata <- em_countries
# countrieswithoutcds <- countriesinoxforddataset[countriesinoxforddataset$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
# countrieswithoutcds



# importing COVID data ----------------------------------------------------
# Note that initial confirmed cases and deaths are recorded as cumulative sums in the Hopkins dataset. We will generate new (daily and weekly) case and death data.
## CLEANING
# Import and quickly cleaning our data. We will call the import our initial confirmed data (Wide). 
Initial_Confirmed_Wide <- read_csv("Data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# We need to know the number of columns for the next step in which we make the wide set long
tempcolnumber <- ncol(Initial_Confirmed_Wide)

## Creating our Temporary and World data files.
# Our data is in wide format. Here, I translate it to long format.
Confirmed_Long <- pivot_longer(Initial_Confirmed_Wide, cols = c(5:all_of(tempcolnumber)), names_to = "Dates", values_to = "Confirmed_Cases")

# Now, I rename my column for country in the long dataset.
Confirmed_Long$COUNTRY <- Confirmed_Long$`Country/Region`

# I can also convert the dates into a more usable string format with lubridate.
Confirmed_Long$Date <- mdy(Confirmed_Long$Dates, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"),
                           truncated = 0)
order(Confirmed_Long$Date)

# Now, I repeat these steps for the deceased data. 
Initial_Deceased_Wide <- read_csv("Data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# We need to know the number of columns for the next step in which we make the wide set long
tempcolnumber <- ncol(Initial_Deceased_Wide)

# We continue with the reshape.
Deceased_Long <- pivot_longer(Initial_Deceased_Wide, cols = c(5:all_of(tempcolnumber)), names_to = "Dates", values_to = "Deceased")
require(reshape)
Deceased_Long$COUNTRY <- Deceased_Long$`Country/Region`
Deceased_Long$Date <- mdy(Deceased_Long$Dates, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"),
                          truncated = 0)
order(Deceased_Long$Date)

# I then merge the cleaned deceased and confirmed datasets.
Merged_Data <- cbind(Confirmed_Long, Deceased_Long)
colnames(Merged_Data)

# I only keep the variables we are interested in.
myvars <- c("COUNTRY", "Province/State", "Lat", "Long", "Dates", "Confirmed_Cases", "Date", "Deceased")

# I rename this file as our temporary data.
Temporary_Data_Country <- Merged_Data[myvars]
Temporary_Data_Country$Date <- as.Date(Temporary_Data_Country$Date) # XXX I added this. Maybe it is not necessary

# We might also want a dataset with the total number of global confirmed cases and deaths per day.
# I first have to deatch plyr for it to group and summarise correctly. 
# detach(package:plyr)    
library(dplyr)
World_Data <- Temporary_Data_Country %>% group_by(Date) %>%
  summarise(country='World', Global_Confirmed_Cases = sum(Confirmed_Cases, na.rm=T),
            Global_Deceased = sum(Deceased, na.rm=T))  %>% ungroup()
World_Data$Death_Rate <- ((World_Data$Global_Deceased/World_Data$Global_Confirmed_Cases) * 100)

##  Collapsing and calculating relevant variables in the temporary data.
# Here, I subset the data so as to properly collapse by country.
myvarstemporary <- c("COUNTRY", "Date", "Confirmed_Cases", "Deceased")

Temporary_Data_Country <- Temporary_Data_Country[myvarstemporary]

# Sicherheitskopie <- Temporary_Data_Country
# Temporary_Data_Country <- Sicherheitskopie 

Temporary_Data_Country <- Temporary_Data_Country %>%
  group_by(COUNTRY, Date) %>% summarise(Total_Cases_Country = sum(Confirmed_Cases), Total_Deceased_Country = sum(Deceased))

# I next want to calculate the increase in deaths and confirmed cases per day.
Temporary_Data_Country %>%
  arrange(COUNTRY, Date)

number <- nrow(Temporary_Data_Country)
First_Day <- min(Temporary_Data_Country$Date)

Temporary_Data_Country$New_Confirmed_Country <- ifelse(Temporary_Data_Country$Date == First_Day, NA, Temporary_Data_Country$Total_Cases_Country - dplyr::lag(Temporary_Data_Country$Total_Cases_Country, number=1))

Temporary_Data_Country$New_Total_Deceased_Country <- ifelse(Temporary_Data_Country$Date == First_Day, NA, Temporary_Data_Country$Total_Deceased_Country - dplyr::lag(Temporary_Data_Country$Total_Deceased_Country, number=1))

# There are a few values for which we have negative new deaths and new cases. Intuitively, this shouldn't make sense. This is actually a problem which steps from the COVID data. For example, Iceland has some weird values in the initial dataframe. Although deaths should represent cumulative sums, if we observe the data from 3/15 - 4/05, we note that the number of recorded deaths drops from 5 to 0, then increases back to six as the month progresses. This is a well-documented problem which stems from the original Github data: https://github.com/CSSEGISandData/COVID-19/issues/2379; https://github.com/CSSEGISandData/COVID-19/issues/2165. We can either exclude data points which show a decrease in cumulative deaths (which I haven't done here); or wait for the data to be updated. Because we only drop a few observations from the analysis, I choose to exclude them, and proceed as normal. Note that when we take global sums, this will affect our cumulative totals (as we have excluded observations for certain nations on certain days).
Temporary_Data_Country <- Temporary_Data_Country[which(Temporary_Data_Country$New_Confirmed_Country >= 0),]
Temporary_Data_Country <- Temporary_Data_Country[which(Temporary_Data_Country$New_Total_Deceased_Country >= 0),]
# =========================================================================.



# add population data -----------------------------------------------------
# Now, I calculate the total mortality rate per capita, new mortality rate per capita, death rate per capita, and confirmed cases per capita (using data from the United Nations on global demographics for the merge). 
UN_Population <- read_csv("data/UN_Population_Data.csv")

UN_Population$COUNTRY <- UN_Population$Location

UN_Population[25, "COUNTRY"] <- "Bolivia"
UN_Population[30, "COUNTRY"] <- "Brunei"
UN_Population[43, "COUNTRY"] <- "Taiwan*"
UN_Population[46, "COUNTRY"] <- "Congo (Brazzaville)"
UN_Population[54, "COUNTRY"] <-"Cote d'Ivoire"
UN_Population[56, "COUNTRY"] <- "Congo (Kinshasa)"
UN_Population[98, "COUNTRY"] <- "Iran"
UN_Population[112, "COUNTRY"] <- "Laos"
UN_Population[138, "COUNTRY"] <- "Burma"
UN_Population[162, "COUNTRY"] <- "Korea, South"
UN_Population[163, "COUNTRY"] <- "Moldova"
UN_Population[165, "COUNTRY"] <- "Russia"
UN_Population[196, "COUNTRY"] <- "Syria"
UN_Population[212, "COUNTRY"] <- "Tanzania"
UN_Population[213, "COUNTRY"] <- "US"
UN_Population[217, "COUNTRY"] <- "Venezuela"
UN_Population[218, "COUNTRY"] <- "Vietnam"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(UN_Population$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

# I subset the file to only include the nations in our final data.
UN_Population <- subset(UN_Population, is.element(UN_Population$COUNTRY, Temporary_Data_Country$COUNTRY))

# Lastly, I join the two dataframes. Note, we lose some observations using inner join (Western Sahara, West Bank and Gaza, MS Zaandam, Kosovo, Diamond Princess, and the Central African Republic.). However, we are not interested in these nations for the purposes of our sample.
Temporary_Data_Country <- inner_join(Temporary_Data_Country, UN_Population)

# This allows me to calculate our variable of interest: the mortality rate per capita (for both total deaths, and new deaths).
Temporary_Data_Country$Total_Mortality_Rate_Per_Capita = (Temporary_Data_Country$Total_Deceased_Country / Temporary_Data_Country$Population)

Temporary_Data_Country$New_Mortality_Rate_Per_Capita = (Temporary_Data_Country$New_Total_Deceased_Country / Temporary_Data_Country$Population)

# We might also be interested in cases per capita.
Temporary_Data_Country$Total_Cases_Country_Per_Capita = (Temporary_Data_Country$Total_Cases_Country / Temporary_Data_Country$Population)

# Lastly, I want to create a rolling average of the total and new mortality rates per capita, new confirmed cases, and new deaths. This is a bit complicated, so bear with me.
# First, I order the data by country and date.
Temporary_Data_Country <- Temporary_Data_Country[order(Temporary_Data_Country$COUNTRY, Temporary_Data_Country$Date),]

# I then construct a seven-day rolling average of confirmed cases, deaths, and both types of mortality rates by country.
Temporary_Data_Country <- Temporary_Data_Country %>%
  group_by(COUNTRY) %>%
  mutate(rolling_average_confirmed = frollmean(New_Confirmed_Country, 7))
Temporary_Data_Country <- Temporary_Data_Country %>%
  group_by(COUNTRY) %>%
  mutate(rolling_average_deceased= frollmean(New_Total_Deceased_Country, 7))
Temporary_Data_Country <- Temporary_Data_Country %>%
  group_by(COUNTRY) %>%
  mutate(total_rolling_average_mortality = frollmean(Total_Mortality_Rate_Per_Capita, 7))
Temporary_Data_Country <- Temporary_Data_Country %>%
  group_by(COUNTRY) %>%
  mutate(new_rolling_average_mortality = frollmean(New_Mortality_Rate_Per_Capita, 7))

# The seven-day rolling average works well, but the algorithm only gives an output after the first week (as we need seven prior values for the sake of our calculation). Ideally, we would want these missing values to have their own rolling averages (i.e. a rolling average of two days for the second date in our dataset; or a rolling average of five days for the fifth date). To do this, I first create a new dataframe with our missing values from the initial seven-day rolling average.
Missing_Mean_Confirmed <- Temporary_Data_Country[which(is.na(Temporary_Data_Country$rolling_average_confirmed)), ]
Missing_Mean_Total_Deceased_Country <- Temporary_Data_Country[which(is.na(Temporary_Data_Country$rolling_average_deceased)), ]
Missing_Mean_Total_Mortality <- Temporary_Data_Country[which(is.na(Temporary_Data_Country$total_rolling_average_mortality)), ]
Missing_Mean_New_Mortality <- Temporary_Data_Country[which(is.na(Temporary_Data_Country$new_rolling_average_mortality)), ]

# Next, I calculate a "new" rolling average for each of our nations, using the roll apply function. The syntax is complicated; but essentially this command allows the user the freedom to vary rolling averages by different "windows" of dates (i.e. three-day average vs. six-day average).
Missing_Mean_Confirmed <- Missing_Mean_Confirmed %>% 
  group_by(COUNTRY) %>%
  mutate(rolling_average_confirmed2 = rollapply(New_Confirmed_Country, 6, mean, na.rm = TRUE, fill = NA, align = 'right', partial=TRUE))
Missing_Mean_Total_Deceased_Country <- Missing_Mean_Total_Deceased_Country %>% 
  group_by(COUNTRY) %>%
  mutate(rolling_average_deceased2 = rollapply(New_Total_Deceased_Country, 6, mean, na.rm = TRUE, fill = NA, align = 'right', partial=TRUE))
Missing_Mean_Total_Mortality <- Missing_Mean_Total_Mortality %>% 
  group_by(COUNTRY) %>%
  mutate(total_rolling_average_mortality2 = rollapply(Total_Mortality_Rate_Per_Capita, 6, mean, na.rm = TRUE, fill = NA, align = 'right', partial=TRUE))
Missing_Mean_New_Mortality <- Missing_Mean_New_Mortality %>% 
  group_by(COUNTRY) %>%
  mutate(new_rolling_average_mortality2 = rollapply(New_Mortality_Rate_Per_Capita, 6, mean, na.rm = TRUE, fill = NA, align = 'right', partial=TRUE))

# We now want to merge this data back to our temporary file.
Temporary_Data_Country <- merge(Temporary_Data_Country, Missing_Mean_Confirmed, by = c("COUNTRY", "Date"), all=TRUE)
Temporary_Data_Country <- merge(Temporary_Data_Country, Missing_Mean_Total_Deceased_Country, by = c("COUNTRY", "Date"), all=TRUE)
Temporary_Data_Country <- merge(Temporary_Data_Country, Missing_Mean_Total_Mortality, by = c("COUNTRY", "Date"), all=TRUE)
Temporary_Data_Country <- merge(Temporary_Data_Country, Missing_Mean_New_Mortality, by = c("COUNTRY", "Date"), all=TRUE)

# Lastly, we can replace the missing values in our temporary file (for the confirmed rolling averages prior to one week) with the newly calculated "partial" rolling averages. We then replace all missing values with zero (the only missing values which remain are for our first day, when naturally no new cases will be recorded, or, in the case of our rolling average for the mortality rate, for dates in which we don't yet have a recorded case).
Temporary_Data_Country$rolling_average_confirmed.x[is.na(Temporary_Data_Country$rolling_average_confirmed.x)] <- Temporary_Data_Country$rolling_average_confirmed2[is.na(Temporary_Data_Country$rolling_average_confirmed.x)]
Temporary_Data_Country$rolling_average_confirmed.x[is.na(Temporary_Data_Country$rolling_average_confirmed.x)] <- 0
Temporary_Data_Country$rolling_average_confirmed <- Temporary_Data_Country$rolling_average_confirmed.x
Temporary_Data_Country$rolling_average_deceased.x[is.na(Temporary_Data_Country$rolling_average_deceased.x)] <- Temporary_Data_Country$rolling_average_deceased2[is.na(Temporary_Data_Country$rolling_average_deceased.x)]
Temporary_Data_Country$rolling_average_deceased.x[is.na(Temporary_Data_Country$rolling_average_deceased.x)] <- 0
Temporary_Data_Country$rolling_average_deceased <- Temporary_Data_Country$rolling_average_deceased.x
Temporary_Data_Country$total_rolling_average_mortality.x[is.na(Temporary_Data_Country$total_rolling_average_mortality.x)] <- Temporary_Data_Country$total_rolling_average_mortality2[is.na(Temporary_Data_Country$total_rolling_average_mortality.x)]
Temporary_Data_Country$total_rolling_average_mortality.x[is.na(Temporary_Data_Country$total_rolling_average_mortality.x)] <- 0
Temporary_Data_Country$total_rolling_average_mortality <- Temporary_Data_Country$total_rolling_average_mortality.x
Temporary_Data_Country$new_rolling_average_mortality.x[is.na(Temporary_Data_Country$new_rolling_average_mortality.x)] <- Temporary_Data_Country$new_rolling_average_mortality2[is.na(Temporary_Data_Country$new_rolling_average_mortality.x)]
Temporary_Data_Country$new_rolling_average_mortality.x[is.na(Temporary_Data_Country$new_rolling_average_mortality.x)] <- 0
Temporary_Data_Country$new_rolling_average_mortality <- Temporary_Data_Country$new_rolling_average_mortality.x

# I only keep columns we care about (which are not repetitive).
Temporary_Data_Country$Total_Cases_Country <- Temporary_Data_Country$Total_Cases_Country.x
Temporary_Data_Country$Total_Deceased_Country <- Temporary_Data_Country$Total_Deceased_Country.x
Temporary_Data_Country$New_Confirmed_Country <- Temporary_Data_Country$New_Confirmed_Country.x
Temporary_Data_Country$New_Total_Deceased_Country <- Temporary_Data_Country$New_Total_Deceased_Country.x
Temporary_Data_Country$Death_Rate <- Temporary_Data_Country$Death_Rate.x
Temporary_Data_Country$Total_Mortality_Rate_Per_Capita <- Temporary_Data_Country$Total_Mortality_Rate_Per_Capita.x
Temporary_Data_Country$New_Mortality_Rate_Per_Capita <- Temporary_Data_Country$New_Mortality_Rate_Per_Capita.x
Temporary_Data_Country$Location <- Temporary_Data_Country$Location.x
Temporary_Data_Country$Population <- Temporary_Data_Country$Population.x
Temporary_Data_Country$Total_Cases_Country_Per_Capita <- Temporary_Data_Country$Total_Cases_Country_Per_Capita.x
Temporary_Data_Country$rolling_average_confirmed <- Temporary_Data_Country$rolling_average_confirmed.x
Temporary_Data_Country$rolling_average_deceased <- Temporary_Data_Country$rolling_average_deceased.x
Temporary_Data_Country$total_rolling_average_mortality <- Temporary_Data_Country$total_rolling_average_mortality.x
Temporary_Data_Country$new_rolling_average_mortality <- Temporary_Data_Country$new_rolling_average_mortality.x

colnames(Temporary_Data_Country)
myvarstemp <- c("COUNTRY", "Date", "Total_Cases_Country", "Total_Deceased_Country", "New_Confirmed_Country", "New_Total_Deceased_Country", "Population", "Total_Mortality_Rate_Per_Capita", "New_Mortality_Rate_Per_Capita", "Total_Cases_Country_Per_Capita", "rolling_average_confirmed", "rolling_average_deceased", "total_rolling_average_mortality", "new_rolling_average_mortality")
Temporary_Data_Country <- Temporary_Data_Country[myvarstemp]

colnames(Temporary_Data_Country)
# =========================================================================.



# add coordinates data ----------------------------------------------------
# Here, I add coordinates for each nation in the dataset. When we collapse the COVID data to the country level, we lose our coordinate data. I now merge a dataset on global coordinates by country from: https://developers.google.com/public-data/docs/canonical/countries_csv. Note that I edited country names to match the data in the CSV itself.
Country_Coordinates <- read.csv("Data/Country_Coordinates.csv")

Country_Coordinates$COUNTRY <- Country_Coordinates$Country
Country_Coordinates$latitude <- Country_Coordinates$Latitude 
Country_Coordinates$longitude <- Country_Coordinates$Longitude

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(Country_Coordinates$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

colnames(Country_Coordinates)
myvarscoords <- c("COUNTRY", "latitude", "longitude")
Country_Coordinates <- Country_Coordinates[myvarscoords]

# Lastly, I merge the two dataframes.
Temporary_Data_Country <- merge(Temporary_Data_Country, Country_Coordinates, by = c("COUNTRY"), all=TRUE)
# =========================================================================.



# label and save temporary data -------------------------------------------
# I load the following two libraries in order to label our variables.
library(lattice)
detach(package:Hmisc)
library(Hmisc)

label(Temporary_Data_Country$Total_Cases_Country) <- "Cumulative Sum of Confirmed Cases by Country (John Hopkins)"
label(Temporary_Data_Country$Total_Deceased_Country) <- "Cumulative Sum of Deaths by Country (John Hopkins)"
label(Temporary_Data_Country$New_Confirmed_Country) <- "Daily Increase in Confirmed Cases (John Hopkins)"
label(Temporary_Data_Country$New_Total_Deceased_Country) <- "Daily Increase in Deaths (John Hopkins)"
label(Temporary_Data_Country$Total_Mortality_Rate_Per_Capita) <- "Total Deaths by Population (Total_Deceased_Country/Population)"
label(Temporary_Data_Country$New_Mortality_Rate_Per_Capita) <- "New Deaths by Population (New_Total_Deceased_Country/Population)"
label(Temporary_Data_Country$Total_Cases_Country_Per_Capita) <- "Total Cases by Population (Total_Cases_Country/Population)"
label(Temporary_Data_Country$rolling_average_confirmed) <- "Seven Day Rolling Average of New Confirmed Cases by Country (with the exception of Days 1-7)"
label(Temporary_Data_Country$rolling_average_deceased) <- "Seven Day Rolling Average of New Deaths by Country (with the exception of Days 1-7)"
label(Temporary_Data_Country$total_rolling_average_mortality) <- "Seven Day Rolling Average of Total Case Mortality Rate by Country (with the exception of Days 1-7)"
label(Temporary_Data_Country$new_rolling_average_mortality) <- "Seven Day Rolling Average of New Case Mortality Rate by Country (with the exception of Days 1-7)"
# =========================================================================.




# add government responses data -------------------------------------------
# Merging our government data and temporary data to create a final dataframe.
# Now, I want to merge our temporary data with the Oxford dataset on global policy responses. Please see a description of the variables here: https://www.bsg.ox.ac.uk/sites/default/files/2020-04/BSG-WP-2020-032-v5.0_0.pdf. 
# The source is: Hale, Thomas, Anna Petherick, Toby Phillips, Samuel Webster. “Variation in Government Responses to COVID-19” Version 5.0. Blavatnik School of Government Working Paper. April 28, 2020. Available: www.bsg.ox.ac.uk/covidtracker.
# See https://www.bsg.ox.ac.uk/sites/default/files/Calculation%20and%20presentation%20of%20the%20Stringency%20Index.pdf for information on how the stringency index was calculated.
Government_Responses <- read.csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")
colnames(Government_Responses)
# If the link on the web is changed, you can alternatively import a version of the CSV from 14 July 2020 from the document "Government_Responses.csv" in the data folder on the github through the command below
# Government_Responses <- read_csv("Data/Government_Responses.csv")

# First, I convert the date to a proper format.
Government_Responses$Date <- ymd(Government_Responses$Date, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"),
                                 truncated = 0)
order(Government_Responses$Date)

# As the download dota is updated everyday, I have to subset the dataframe here so that it corresponds to the dates of the other dataframes.
Government_Responses <- subset(Government_Responses, Date < as.Date("2020-07-17"))

# Then, I correct country and variable names.
Government_Responses$COUNTRY <- Government_Responses$CountryName
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Cape Verde", "Cabo Verde", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Congo", "Congo (Brazzaville)", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Democratic Republic of Congo", "Congo (Kinshasa)", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Myanmar", "Burma", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Czech Republic", "Czechia", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Swaziland", "Eswatini", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Krygz Republic", "Kyrgyzstan", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "South Korea", "Korea, South", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Macedonia", "North Macedonia", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Slovak Republic", "Slovakia", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Macedonia", "North Macedonia", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Taiwan", "Taiwan*", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "Timor", "Timor-Leste", Government_Responses$COUNTRY)
Government_Responses$COUNTRY <- ifelse(Government_Responses$COUNTRY == "United States", "US", Government_Responses$COUNTRY)

Government_Responses <- Government_Responses[order(Government_Responses$CountryName, Government_Responses$Date),]
# XXX had to make it a tibble to make it the same
# Government_Responses <- as_tibble(Government_Responses)

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(Government_Responses$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems


colnames(Government_Responses)

# XXX I'm gonna mark these ass comments because they eliminate certain columns that seem to be needed
# Government_Responses$C1_School.closing <- Government_Responses$`C1_School closing`
# Government_Responses$C2_Workplace.closing <- Government_Responses$`C2_Workplace closing`
# Government_Responses$C3_Cancel.public.events <- Government_Responses$`C3_Cancel public events`
# Government_Responses$C4_Restrictions.on.gatherings <- Government_Responses$`C4_Restrictions on gatherings`
# Government_Responses$C5_Close.public.transport <- Government_Responses$`C5_Close public transport`
# Government_Responses$C6_Stay.at.home.requirements <- Government_Responses$`C6_Stay at home requirements`
# Government_Responses$C7_Restrictions.on.internal.movement <- Government_Responses$`C7_Restrictions on internal movement`
# Government_Responses$C8_International.travel.controls <- Government_Responses$`C8_International travel controls`
# Government_Responses$E1_Income.support <- Government_Responses$`E1_Income support`
# Government_Responses$E2_Debt.contract.relief <- Government_Responses$`E2_Debt/contract relief`
# Government_Responses$E3_Fiscal.measures <- Government_Responses$`E3_Fiscal measures`
# Government_Responses$E4_International.support <- Government_Responses$`E4_International support`
# Government_Responses$H1_Public.information.campaigns <- Government_Responses$`H1_Public information campaigns`
# Government_Responses$H2_Testing.policy <- Government_Responses$`H2_Testing policy`
# Government_Responses$H3_Contact.tracing <- Government_Responses$`H3_Contact tracing`
# Government_Responses$H4_Emergency.investment.in.healthcare <- Government_Responses$`H4_Emergency investment in healthcare`
# Government_Responses$H5_Investment.in.vaccines <- Government_Responses$`H5_Investment in vaccines`

# XXX here something starts looking different

# I now subset the data. PLEASE NOTE the observations change in this merge. Oxford began collecting data before Hopkins started tracking cases and deaths. For simplicity, I use the merged data from before to visualize the confirmed cases and deaths (as these dates are filtered correctly).
Government_Responses <- subset(Government_Responses, is.element(Government_Responses$COUNTRY, Temporary_Data_Country$COUNTRY))

# Note that we have two sets of variables for confirmed cases and deaths. The first, I label "Oxford_Cases" and "Oxford_Deaths". The second (from John Hopkins) I label "Total_Cases_Country" and "Total_Deceased_Country". At a later date, we might want to cross-reference these values. Note that Oxford does not provide calculations for province/state; however it is highly likely they used the (same) John Hopkins data.
Government_Responses$Oxford_Cases <- Government_Responses$ConfirmedCases
Government_Responses$Oxford_Deaths <- Government_Responses$ConfirmedDeaths


# Once more, I only keep the variables we are interested in. I elect not to keep the notes... these can be re-added if preferred. E4_International.support also has almost no values above 0, so it is dropped. 

colnames(Government_Responses)
myvarsgovernment <- c("COUNTRY", "Date", "C1_School.closing", "C1_Flag", "C2_Workplace.closing", "C2_Flag", "C3_Cancel.public.events", "C3_Flag", "C4_Restrictions.on.gatherings", "C4_Flag", "C5_Close.public.transport", "C5_Flag", "C6_Stay.at.home.requirements", "C6_Flag", "C7_Restrictions.on.internal.movement", "C7_Flag", "C8_International.travel.controls", "E1_Income.support", "E1_Flag", "E2_Debt.contract.relief", "E3_Fiscal.measures", "H1_Public.information.campaigns", "H1_Flag", "H2_Testing.policy", "H3_Contact.tracing", "H4_Emergency.investment.in.healthcare", "H5_Investment.in.vaccines", "M1_Wildcard", "StringencyIndex", "Oxford_Cases", "Oxford_Deaths", "GovernmentResponseIndex", "ContainmentHealthIndex")

# XXX here's an issue witht he variables
Government_Responses <- Government_Responses[myvarsgovernment]

Government_Responses <- Government_Responses[order(Government_Responses$COUNTRY, Government_Responses$Date),]

Temporary_Data_Country <- Temporary_Data_Country[order(Temporary_Data_Country$COUNTRY, Temporary_Data_Country$Date),]

# I then merge our data.
Final_Data_Country <- merge(Temporary_Data_Country, Government_Responses, by = c("COUNTRY", "Date"), all=TRUE)
# =========================================================================.




# add lagged government response variables --------------------------------
# Next, I specify a set of lagged government response variables for use in our regression later (one-week, two-week, and three-week lags). First, I filter the data and create a lag function for each week (these functions are also used for generating the lags in our testing data).
Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# This chunk of code is quite extensive. In our regressions, we use the "lag" function to generate lags for our key variables, rather than using the generated lagged variables below. I simply include them in order to visualize the dataframe with lags incorporated (as the lag function will simply lag existing variables in analysis; not generate new ones to explore). Using this function also allows us to observe which nations do not have enough observations to generate specified lags.




# one week ----------------------------------------------------------------
Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C1_School.closing", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_School_Closing_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_School_Closing_One_Week <- as.factor(Final_Data_Country$Lagged_School_Closing_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C1_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_School_Closing_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_School_Closing_General_One_Week <- as.factor(Final_Data_Country$Lagged_School_Closing_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C2_Workplace.closing", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Workplace_Closing_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Workplace_Closing_One_Week <- as.factor(Final_Data_Country$Lagged_Workplace_Closing_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C2_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Workplace_Closing_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Workplace_Closing_General_One_Week <- as.factor(Final_Data_Country$Lagged_Workplace_Closing_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C3_Cancel.public.events", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Events_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Events_One_Week <- as.factor(Final_Data_Country$Lagged_Public_Events_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C3_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Events_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Events_General_One_Week <- as.factor(Final_Data_Country$Lagged_Public_Events_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C4_Restrictions.on.gatherings", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Gatherings_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Gatherings_One_Week <- as.factor(Final_Data_Country$Lagged_Gatherings_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C4_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Gatherings_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Gatherings_General_One_Week <- as.factor(Final_Data_Country$Lagged_Gatherings_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C5_Close.public.transport", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Transport_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Transport_One_Week <- as.factor(Final_Data_Country$Lagged_Public_Transport_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C5_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Transport_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Transport_General_One_Week <- as.factor(Final_Data_Country$Lagged_Public_Transport_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C6_Stay.at.home.requirements", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Lockdown_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Lockdown_One_Week <- as.factor(Final_Data_Country$Lagged_Lockdown_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C6_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Lockdown_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Lockdown_General_One_Week <- as.factor(Final_Data_Country$Lagged_Lockdown_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C7_Restrictions.on.internal.movement", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Internal_Movement_One_Week", keepInvalid =   
          TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Internal_Movement_One_Week <- as.factor(Final_Data_Country$Lagged_Internal_Movement_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C7_Flag",slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Internal_Movement_General_One_Week", keepInvalid = TRUE, reminder =  FALSE)

Final_Data_Country$Lagged_Internal_Movement_General_One_Week <- as.factor(Final_Data_Country$Lagged_Internal_Movement_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C8_International.travel.controls", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_International_Travel_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_International_Travel_One_Week <- as.factor(Final_Data_Country$Lagged_International_Travel_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E1_Income.support", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Income_Support_One_Week", keepInvalid =   
          TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Income_Support_One_Week <- as.factor(Final_Data_Country$Lagged_Income_Support_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E1_Flag",slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Income_Support_General_One_Week", keepInvalid = TRUE, reminder =  FALSE)

Final_Data_Country$Lagged_Income_Support_General_One_Week <- as.factor(Final_Data_Country$Lagged_Income_Support_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E2_Debt.contract.relief", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Debt_Relief_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Debt_Relief_One_Week <- as.factor(Final_Data_Country$Lagged_Debt_Relief_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E3_Fiscal.measures", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Fiscal_Measures_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H1_Public.information.campaigns", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Campaign_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Campaign_One_Week <- as.factor(Final_Data_Country$Lagged_Campaign_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H1_Flag", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Campaign_General_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Campaign_General_One_Week <- as.factor(Final_Data_Country$Lagged_Campaign_General_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H2_Testing.policy", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Testing_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Testing_One_Week <- as.factor(Final_Data_Country$Lagged_Testing_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H3_Contact.tracing", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Tracing_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Tracing_One_Week <- as.factor(Final_Data_Country$Lagged_Tracing_One_Week)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H4_Emergency.investment.in.healthcare", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Health_Care_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H5_Investment.in.vaccines", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Health_Care_One_Week", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "StringencyIndex", TimeVar = "Date", slideBy = -7, GroupVar = "COUNTRY", NewVar = "Lagged_Stringency_Index_One_Week", keepInvalid 
        = TRUE, reminder = FALSE)
# =========================================================================.



# two weeks ---------------------------------------------------------------
Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C1_School.closing", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_School_Closing_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_School_Closing_Two_Weeks <- as.factor(Final_Data_Country$Lagged_School_Closing_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C1_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_School_Closing_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_School_Closing_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_School_Closing_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C2_Workplace.closing", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Workplace_Closing_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Workplace_Closing_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Workplace_Closing_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C2_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Workplace_Closing_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Workplace_Closing_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Workplace_Closing_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C3_Cancel.public.events", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Events_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Events_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Events_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C3_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Events_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Events_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Events_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C4_Restrictions.on.gatherings", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Gatherings_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Gatherings_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Gatherings_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C4_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Gatherings_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Gatherings_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Gatherings_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C5_Close.public.transport", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Transport_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Transport_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Transport_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C5_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Transport_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Transport_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Transport_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C6_Stay.at.home.requirements", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Lockdown_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Lockdown_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Lockdown_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C6_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Lockdown_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Lockdown_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Lockdown_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C7_Restrictions.on.internal.movement", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Internal_Movement_Two_Weeks", keepInvalid =   
          TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Internal_Movement_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Internal_Movement_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C7_Flag",slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Internal_Movement_General_Two_Weeks", keepInvalid = TRUE, reminder =  FALSE)

Final_Data_Country$Lagged_Internal_Movement_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Internal_Movement_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C8_International.travel.controls", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_International_Travel_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_International_Travel_Two_Weeks <- as.factor(Final_Data_Country$Lagged_International_Travel_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E1_Income.support", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Income_Support_Two_Weeks", keepInvalid =   
          TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Income_Support_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Income_Support_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E1_Flag",slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Income_Support_General_Two_Weeks", keepInvalid = TRUE, reminder =  FALSE)

Final_Data_Country$Lagged_Income_Support_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Income_Support_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E2_Debt.contract.relief", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Debt_Relief_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Debt_Relief_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Debt_Relief_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E3_Fiscal.measures", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Fiscal_Measures_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H1_Public.information.campaigns", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Campaign_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Campaign_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Campaign_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H1_Flag", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Campaign_General_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Campaign_General_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Campaign_General_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H2_Testing.policy", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Testing_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Testing_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Testing_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H3_Contact.tracing", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Tracing_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Tracing_Two_Weeks <- as.factor(Final_Data_Country$Lagged_Tracing_Two_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H4_Emergency.investment.in.healthcare", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Health_Care_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H5_Investment.in.vaccines", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Health_Care_Two_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "StringencyIndex", TimeVar = "Date", slideBy = -14, GroupVar = "COUNTRY", NewVar = "Lagged_Stringency_Index_Two_Weeks", keepInvalid 
        = TRUE, reminder = FALSE)
# =========================================================================.



# three week --------------------------------------------------------------
Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C1_School.closing", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_School_Closing_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_School_Closing_Three_Weeks <- as.factor(Final_Data_Country$Lagged_School_Closing_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C1_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_School_Closing_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_School_Closing_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_School_Closing_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C2_Workplace.closing", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Workplace_Closing_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Workplace_Closing_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Workplace_Closing_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C2_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Workplace_Closing_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Workplace_Closing_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Workplace_Closing_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C3_Cancel.public.events", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Events_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Events_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Events_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C3_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Events_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Events_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Events_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C4_Restrictions.on.gatherings", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Gatherings_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Gatherings_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Gatherings_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C4_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Gatherings_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Gatherings_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Gatherings_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C5_Close.public.transport", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Transport_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Transport_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Transport_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C5_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Public_Transport_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Public_Transport_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Public_Transport_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C6_Stay.at.home.requirements", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Lockdown_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Lockdown_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Lockdown_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C6_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Lockdown_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Lockdown_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Lockdown_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C7_Restrictions.on.internal.movement", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Internal_Movement_Three_Weeks", keepInvalid =   
          TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Internal_Movement_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Internal_Movement_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C7_Flag",slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Internal_Movement_General_Three_Weeks", keepInvalid = TRUE, reminder =  FALSE)

Final_Data_Country$Lagged_Internal_Movement_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Internal_Movement_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "C8_International.travel.controls", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_International_Travel_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_International_Travel_Three_Weeks <- as.factor(Final_Data_Country$Lagged_International_Travel_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E1_Income.support", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Income_Support_Three_Weeks", keepInvalid =   
          TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Income_Support_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Income_Support_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E1_Flag",slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Income_Support_General_Three_Weeks", keepInvalid = TRUE, reminder =  FALSE)

Final_Data_Country$Lagged_Income_Support_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Income_Support_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E2_Debt.contract.relief", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Debt_Relief_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Debt_Relief_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Debt_Relief_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "E3_Fiscal.measures", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Fiscal_Measures_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H1_Public.information.campaigns", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Campaign_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Campaign_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Campaign_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H1_Flag", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Campaign_General_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Campaign_General_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Campaign_General_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H2_Testing.policy", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Testing_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Testing_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Testing_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H3_Contact.tracing", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Tracing_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country$Lagged_Tracing_Three_Weeks <- as.factor(Final_Data_Country$Lagged_Tracing_Three_Weeks)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H4_Emergency.investment.in.healthcare", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Health_Care_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "H5_Investment.in.vaccines", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Health_Care_Three_Weeks", keepInvalid = TRUE, reminder = FALSE)

Final_Data_Country <- Final_Data_Country %>%
  dplyr::group_by(COUNTRY) %>%
  slide(Var = "StringencyIndex", TimeVar = "Date", slideBy = -21, GroupVar = "COUNTRY", NewVar = "Lagged_Stringency_Index_Three_Weeks", keepInvalid 
        = TRUE, reminder = FALSE)
# =========================================================================.



# add coordinates data ----------------------------------------------------
# Here, I add coordinates for each nation in the dataset.
Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

# I am using the same Google coordinate data from: https://developers.google.com/public-data/docs/canonical/countries_csv. Note that I edited country names to match the data in the CSV itself (I only kept coordinates for nations in our dataframe). 
Country_Coordinates <- read.csv("data/Country_Coordinates.csv")

Country_Coordinates$COUNTRY <- Country_Coordinates$Country

colnames(Country_Coordinates)
myvarscoords <- c("COUNTRY", "Latitude", "Longitude")
Country_Coordinates <- Country_Coordinates[myvarscoords]

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(Country_Coordinates$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems




# I subset the file to only include the nations in our final data.
Country_Coordinates <- subset(Country_Coordinates, is.element(Country_Coordinates$COUNTRY, Final_Data_Country$COUNTRY))

# Lastly, I merge the two dataframes.
Final_Data_Country <- merge(Final_Data_Country, Country_Coordinates, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add proportion of 65+ people --------------------------------------------
# Now, I add socioeconomic controls from the World Development Indicators.
Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

# First, I add the proportion of the national population above 65 from: https://data.worldbank.org/indicator/SP.POP.65UP.TO.ZS. 
pop65yo <- read_csv("Data/pop65yo.csv")

pop65yo$COUNTRY <- pop65yo$country
pop65yo$prop65 <- pop65yo$value

# Next, I correct the country names.
pop65yo[19, "COUNTRY"] <- "Bahamas"
pop65yo[26, "COUNTRY"] <- "Brunei"
pop65yo[37, "COUNTRY"] <- "Congo (Kinshasa)"
pop65yo[38, "COUNTRY"] <- "Congo (Brazzaville)"
pop65yo[46, "COUNTRY"] <- "Czechia"
pop65yo[53, "COUNTRY"] <- "Egypt"
pop65yo[66, "COUNTRY"] <- "Gambia"
pop65yo[82, "COUNTRY"] <- "Iran"
pop65yo[95, "COUNTRY"] <- "Korea, South"
pop65yo[92, "COUNTRY"] <- "Kyrgyzstan"
pop65yo[97, "COUNTRY"] <- "Laos"
pop65yo[101, "COUNTRY"] <- "Saint Lucia"
pop65yo[116, "COUNTRY"] <- "Burma"
pop65yo[148, "COUNTRY"] <- "Russia"
pop65yo[162, "COUNTRY"] <- "Slovakia"
pop65yo[167, "COUNTRY"] <- "Syria"
pop65yo[182, "COUNTRY"] <- "US"
pop65yo[184, "COUNTRY"] <- "Saint Vincent and the Grenadines"
pop65yo[185, "COUNTRY"] <- "Venezuela"
pop65yo[186, "COUNTRY"] <- "Virgin Islands"
pop65yo[190, "COUNTRY"] <- "Yemen"
pop65yo[5, "COUNTRY"] <- "United Arab Emirates"
pop65yo[62, "COUNTRY"] <- "United Kingdom"
pop65yo[132, "COUNTRY"] <- "New Zealand"
pop65yo[150, "COUNTRY"] <- "Saudi Arabia"
pop65yo[191, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(pop65yo$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

# subsetting
pop65yo <- subset(pop65yo, is.element(pop65yo$COUNTRY, Final_Data_Country$COUNTRY))

# I only keep the columns I care about.
colnames(pop65yo)
myvarspop65yo <- c("prop65", "COUNTRY")

pop65yo <- pop65yo[myvarspop65yo]

pop65yo <- pop65yo[order(pop65yo$COUNTRY, pop65yo$prop65),]

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

Final_Data_Country <- merge(Final_Data_Country, pop65yo, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add urban population data -----------------------------------------------
# Now, I add another world development indicator for the proportion of the population which is urban: https://data.worldbank.org/indicator/SP.URB.TOTL.in.zs. 
urbanpp <- read_csv("Data/urbanpp.csv")

# Next, I correct the country names.
urbanpp[21, "COUNTRY"] <- "Bahamas"
urbanpp[29, "COUNTRY"] <- "Brunei"
urbanpp[40, "COUNTRY"] <- "Congo (Kinshasa)"
urbanpp[41, "COUNTRY"] <- "Congo (Brazzaville)"
urbanpp[50, "COUNTRY"] <- "Czechia"
urbanpp[58, "COUNTRY"] <- "Egypt"
urbanpp[73, "COUNTRY"] <- "Gambia"
urbanpp[91, "COUNTRY"] <- "Iran"
urbanpp[105, "COUNTRY"] <- "Korea, South"
urbanpp[101, "COUNTRY"] <- "Kyrgyzstan"
urbanpp[107, "COUNTRY"] <- "Laos"
urbanpp[111, "COUNTRY"] <- "Saint Lucia"
urbanpp[129, "COUNTRY"] <- "Burma"
urbanpp[164, "COUNTRY"] <- "Russia"
urbanpp[179, "COUNTRY"] <- "Slovakia"
urbanpp[185, "COUNTRY"] <- "Syria"
urbanpp[202, "COUNTRY"] <- "US"
urbanpp[204, "COUNTRY"] <- "Saint Vincent and the Grenadines"
urbanpp[205, "COUNTRY"] <- "Venezuela"
urbanpp[206, "COUNTRY"] <- "Virgin Islands"
urbanpp[211, "COUNTRY"] <- "Yemen"
urbanpp[6, "COUNTRY"] <- "United Arab Emirates"
urbanpp[68, "COUNTRY"] <- "United Kingdom"
urbanpp[147, "COUNTRY"] <- "New Zealand"
urbanpp[166, "COUNTRY"] <- "Saudi Arabia"
urbanpp[212, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(urbanpp$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

urbanpp <- subset(urbanpp, is.element(urbanpp$COUNTRY, Final_Data_Country$COUNTRY))

# I only keep the columns I care about.
colnames(urbanpp)
myvarsurbanpp <- c("propurban", "COUNTRY")

urbanpp <- urbanpp[myvarsurbanpp]

Final_Data_Country <- merge(Final_Data_Country, urbanpp, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add population density data ---------------------------------------------
# Now, I add the population density indicator. Note the latest year available for this data is 2018. 
popdensity <- read.csv("Data/popdensity.csv")

popdensity$COUNTRY <- popdensity$Country.Name

# Next, I correct the country names.
popdensity[22, "COUNTRY"] <- "Bahamas"
popdensity[30, "COUNTRY"] <- "Brunei"
popdensity[42, "COUNTRY"] <- "Congo (Kinshasa)"
popdensity[43, "COUNTRY"] <- "Congo (Brazzaville)"
popdensity[53, "COUNTRY"] <- "Czechia"
popdensity[66, "COUNTRY"] <- "Egypt"
popdensity[85, "COUNTRY"] <- "Gambia"
popdensity[111, "COUNTRY"] <- "Iran"
popdensity[125, "COUNTRY"] <- "Korea, South"
popdensity[121, "COUNTRY"] <- "Kyrgyzstan"
popdensity[128, "COUNTRY"] <- "Laos"
popdensity[132, "COUNTRY"] <- "Saint Lucia"
popdensity[159, "COUNTRY"] <- "Burma"
popdensity[201, "COUNTRY"] <- "Russia"
popdensity[220, "COUNTRY"] <- "Slovakia"
popdensity[226, "COUNTRY"] <- "Syria"
popdensity[250, "COUNTRY"] <- "US"
popdensity[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
popdensity[253, "COUNTRY"] <- "Venezuela"
popdensity[255, "COUNTRY"] <- "Virgin Islands"
popdensity[261, "COUNTRY"] <- "Yemen"
popdensity[7, "COUNTRY"] <- "United Arab Emirates"
popdensity[80, "COUNTRY"] <- "United Kingdom"
popdensity[179, "COUNTRY"] <- "New Zealand"
popdensity[204, "COUNTRY"] <- "Saudi Arabia"
popdensity[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(popdensity$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

popdensity <- subset(popdensity, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(popdensity)
myvarspopdensity <- c("popdensity", "COUNTRY")

popdensity <- popdensity[myvarspopdensity]

popdensity <- popdensity[order(popdensity$COUNTRY, popdensity$popdensity),]

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

Final_Data_Country <- merge(Final_Data_Country, popdensity, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]  
# =========================================================================.



# adding mobility data ----------------------------------------------------
# Merging the mobility data
Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

# Here, I integrate the Apple Mobility data. 
mobility <- read.csv("https://covid19-static.cdn-apple.com/covid19-mobility-data/2012HotfixDev11/v3/en-us/applemobilitytrends-2020-07-12.csv")
# You can also download it from online by changing the date in the following command
# mobility <- read.csv("Data/applemobilitytrends.csv")
# mobility <- read.csv("https://covid19-static.cdn-apple.com/covid19-mobility-data/2012HotfixDev11/v3/en-us/applemobilitytrends-2020-07-12.csv")

# First, we convert from wide to long format.
mobility <- pivot_longer(mobility, cols = starts_with("X"), names_to = "Dates")

# Next, we reformat the dates.
mobility$Dates<-sub("X", "", mobility$Dates)
mobility$Date <- ymd(mobility$Dates, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"),
                     truncated = 0)

# Renaming the "key" variable XXXX. no longer necessary I think
# mobility$Date <- mobility$Dates

# I only keep the variables we care about. I also filter for only countries (we can re-add cities at a later period).
mobility <- mobility %>%
  dplyr:: filter(mobility$geo_type == "country/region")

colnames(mobility)
myvarsmobility <- c("region", "transportation_type", "Date", "value")

mobility <- mobility[myvarsmobility]

mobility$COUNTRY <- mobility$region

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(mobility$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutmobilitydata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutmobilitydata

# Now, I re-convert from long to wide format.
mobility <- pivot_wider(mobility, id_cols = c("COUNTRY", "Date", "transportation_type"), names_from = "transportation_type", values_from = c("value"))

mobility <- subset(mobility, is.element(mobility$COUNTRY, Final_Data_Country$COUNTRY))

Final_Data_Country <- merge(Final_Data_Country, mobility, by = c("COUNTRY", "Date"), all=TRUE)
# =========================================================================.



# adding arrivals data ----------------------------------------------------
# Now, I add our controls for cross-country travel.
Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

# First, I add information on the number of tourist arrivals (2018 is l.y.a.). Run this in the console
arrivals <- read.csv("Data/arrivals.csv")

arrivals$COUNTRY <- arrivals$Country.Name

# Next, I correct the country names.
arrivals[22, "COUNTRY"] <- "Bahamas"
arrivals[30, "COUNTRY"] <- "Brunei"
arrivals[42, "COUNTRY"] <- "Congo (Kinshasa)"
arrivals[43, "COUNTRY"] <- "Congo (Brazzaville)"
arrivals[53, "COUNTRY"] <- "Czechia"
arrivals[66, "COUNTRY"] <- "Egypt"
arrivals[85, "COUNTRY"] <- "Gambia"
arrivals[111, "COUNTRY"] <- "Iran"
arrivals[125, "COUNTRY"] <- "Korea, South"
arrivals[121, "COUNTRY"] <- "Kyrgyzstan"
arrivals[128, "COUNTRY"] <- "Laos"
arrivals[132, "COUNTRY"] <- "Saint Lucia"
arrivals[159, "COUNTRY"] <- "Burma"
arrivals[201, "COUNTRY"] <- "Russia"
arrivals[220, "COUNTRY"] <- "Slovakia"
arrivals[226, "COUNTRY"] <- "Syria"
arrivals[250, "COUNTRY"] <- "US"
arrivals[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
arrivals[253, "COUNTRY"] <- "Venezuela"
arrivals[255, "COUNTRY"] <- "Virgin Islands"
arrivals[261, "COUNTRY"] <- "Yemen"
arrivals[7, "COUNTRY"] <- "United Arab Emirates"
arrivals[80, "COUNTRY"] <- "United Kingdom"
arrivals[179, "COUNTRY"] <- "New Zealand"
arrivals[204, "COUNTRY"] <- "Saudi Arabia"
arrivals[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(arrivals$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutarrivalsdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutarrivalsdata
# xxx but some countries have NA

arrivals <- subset(arrivals, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(arrivals)
myvarsarrivals <- c("arrivals", "COUNTRY")

arrivals <- arrivals[myvarsarrivals]

Final_Data_Country <- merge(Final_Data_Country, arrivals, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# adding tourism departures data ------------------------------------------
# Next, I add information on tourism departures (2018 is l.y.a.). 
departures <- read.csv("Data/departures.csv")

departures$COUNTRY <- departures$Country.Name

# Next, I correct the country names.
departures[22, "COUNTRY"] <- "Bahamas"
departures[30, "COUNTRY"] <- "Brunei"
departures[42, "COUNTRY"] <- "Congo (Kinshasa)"
departures[43, "COUNTRY"] <- "Congo (Brazzaville)"
departures[53, "COUNTRY"] <- "Czechia"
departures[66, "COUNTRY"] <- "Egypt"
departures[85, "COUNTRY"] <- "Gambia"
departures[111, "COUNTRY"] <- "Iran"
departures[125, "COUNTRY"] <- "Korea, South"
departures[121, "COUNTRY"] <- "Kyrgyzstan"
departures[128, "COUNTRY"] <- "Laos"
departures[132, "COUNTRY"] <- "Saint Lucia"
departures[159, "COUNTRY"] <- "Burma"
departures[201, "COUNTRY"] <- "Russia"
departures[220, "COUNTRY"] <- "Slovakia"
departures[226, "COUNTRY"] <- "Syria"
departures[250, "COUNTRY"] <- "US"
departures[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
departures[253, "COUNTRY"] <- "Venezuela"
departures[255, "COUNTRY"] <- "Virgin Islands"
departures[261, "COUNTRY"] <- "Yemen"
departures[7, "COUNTRY"] <- "United Arab Emirates"
departures[80, "COUNTRY"] <- "United Kingdom"
departures[179, "COUNTRY"] <- "New Zealand"
departures[204, "COUNTRY"] <- "Saudi Arabia"
departures[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(departures$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutdeparturesdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutdeparturesdata
# Some countries have NAs

departures <- subset(departures, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(departures)
myvarsdepartures <- c("departures", "COUNTRY")

departures <- departures[myvarsdepartures]

Final_Data_Country <- merge(Final_Data_Country, departures, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add vulnerable employment data ------------------------------------------
# Now, I add additional country-specific characteristics gathered from the World Bank. All data is from 2018, unless noted.
Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

# First, I add the percentage of vulnerable employment by nation from: https://data.worldbank.org/indicator/SL.EMP.VULN.ZS?view=chart 
vulnerable_employment <- read.csv("Data/vulnerable_employment.csv")

# Next, I correct the country names.
vulnerable_employment[22, "COUNTRY"] <- "Bahamas"
vulnerable_employment[30, "COUNTRY"] <- "Brunei"
vulnerable_employment[42, "COUNTRY"] <- "Congo (Kinshasa)"
vulnerable_employment[43, "COUNTRY"] <- "Congo (Brazzaville)"
vulnerable_employment[53, "COUNTRY"] <- "Czechia"
vulnerable_employment[66, "COUNTRY"] <- "Egypt"
vulnerable_employment[85, "COUNTRY"] <- "Gambia"
vulnerable_employment[111, "COUNTRY"] <- "Iran"
vulnerable_employment[125, "COUNTRY"] <- "Korea, South"
vulnerable_employment[121, "COUNTRY"] <- "Kyrgyzstan"
vulnerable_employment[128, "COUNTRY"] <- "Laos"
vulnerable_employment[132, "COUNTRY"] <- "Saint Lucia"
vulnerable_employment[159, "COUNTRY"] <- "Burma"
vulnerable_employment[201, "COUNTRY"] <- "Russia"
vulnerable_employment[220, "COUNTRY"] <- "Slovakia"
vulnerable_employment[226, "COUNTRY"] <- "Syria"
vulnerable_employment[250, "COUNTRY"] <- "US"
vulnerable_employment[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
vulnerable_employment[253, "COUNTRY"] <- "Venezuela"
vulnerable_employment[255, "COUNTRY"] <- "Virgin Islands"
vulnerable_employment[261, "COUNTRY"] <- "Yemen"
vulnerable_employment[7, "COUNTRY"] <- "United Arab Emirates"
vulnerable_employment[80, "COUNTRY"] <- "United Kingdom"
vulnerable_employment[179, "COUNTRY"] <- "New Zealand"
vulnerable_employment[204, "COUNTRY"] <- "Saudi Arabia"
vulnerable_employment[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(vulnerable_employment$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutvulnerableemploymentdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutvulnerableemploymentdata
# Some countries have NAs

vulnerable_employment <- subset(vulnerable_employment, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(vulnerable_employment)
myvarsvulnerable_employment <- c("vul_emp", "COUNTRY")

vulnerable_employment <- vulnerable_employment[myvarsvulnerable_employment]

Final_Data_Country <- merge(Final_Data_Country, vulnerable_employment, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add GNI per capita data -------------------------------------------------
# Next, I add GNI per capita from: https://data.worldbank.org/indicator/NY.GNP.PCAP.CD?view=chart. 
GNI <- read.csv("Data/GNI.csv")

# Next, I correct the country names.
GNI[22, "COUNTRY"] <- "Bahamas"
GNI[30, "COUNTRY"] <- "Brunei"
GNI[42, "COUNTRY"] <- "Congo (Kinshasa)"
GNI[43, "COUNTRY"] <- "Congo (Brazzaville)"
GNI[53, "COUNTRY"] <- "Czechia"
GNI[66, "COUNTRY"] <- "Egypt"
GNI[85, "COUNTRY"] <- "Gambia"
GNI[111, "COUNTRY"] <- "Iran"
GNI[125, "COUNTRY"] <- "Korea, South"
GNI[121, "COUNTRY"] <- "Kyrgyzstan"
GNI[128, "COUNTRY"] <- "Laos"
GNI[132, "COUNTRY"] <- "Saint Lucia"
GNI[159, "COUNTRY"] <- "Burma"
GNI[201, "COUNTRY"] <- "Russia"
GNI[220, "COUNTRY"] <- "Slovakia"
GNI[226, "COUNTRY"] <- "Syria"
GNI[250, "COUNTRY"] <- "US"
GNI[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
GNI[253, "COUNTRY"] <- "Venezuela"
GNI[255, "COUNTRY"] <- "Virgin Islands"
GNI[261, "COUNTRY"] <- "Yemen"
GNI[7, "COUNTRY"] <- "United Arab Emirates"
GNI[80, "COUNTRY"] <- "United Kingdom"
GNI[179, "COUNTRY"] <- "New Zealand"
GNI[204, "COUNTRY"] <- "Saudi Arabia"
GNI[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(GNI$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutGNIdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutGNIdata
# Some countries have NAs

GNI <- subset(GNI, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(GNI)
myvarsGNI <- c("GNI", "COUNTRY")

GNI <- GNI[myvarsGNI]

Final_Data_Country <- merge(Final_Data_Country, GNI, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add health expenditures data --------------------------------------------
# Next, I control for current health expenditures (l.y.a. is 2017): https://data.worldbank.org/indicator/SH.XPD.CHEX.PC.CD?view=chart. 
health_pc <- read.csv("Data/health_pc.csv")

# Next, I correct the country names.
health_pc[22, "COUNTRY"] <- "Bahamas"
health_pc[30, "COUNTRY"] <- "Brunei"
health_pc[42, "COUNTRY"] <- "Congo (Kinshasa)"
health_pc[43, "COUNTRY"] <- "Congo (Brazzaville)"
health_pc[53, "COUNTRY"] <- "Czechia"
health_pc[66, "COUNTRY"] <- "Egypt"
health_pc[85, "COUNTRY"] <- "Gambia"
health_pc[111, "COUNTRY"] <- "Iran"
health_pc[125, "COUNTRY"] <- "Korea, South"
health_pc[121, "COUNTRY"] <- "Kyrgyzstan"
health_pc[128, "COUNTRY"] <- "Laos"
health_pc[132, "COUNTRY"] <- "Saint Lucia"
health_pc[159, "COUNTRY"] <- "Burma"
health_pc[201, "COUNTRY"] <- "Russia"
health_pc[220, "COUNTRY"] <- "Slovakia"
health_pc[226, "COUNTRY"] <- "Syria"
health_pc[250, "COUNTRY"] <- "US"
health_pc[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
health_pc[253, "COUNTRY"] <- "Venezuela"
health_pc[255, "COUNTRY"] <- "Virgin Islands"
health_pc[261, "COUNTRY"] <- "Yemen"
health_pc[7, "COUNTRY"] <- "United Arab Emirates"
health_pc[80, "COUNTRY"] <- "United Kingdom"
health_pc[179, "COUNTRY"] <- "New Zealand"
health_pc[204, "COUNTRY"] <- "Saudi Arabia"
health_pc[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(health_pc$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithouthealth_ocdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithouthealth_ocdata
# Some countries have NAs

health_pc <- subset(health_pc, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(health_pc)
myvarshealth_pc <- c("health_exp", "COUNTRY")

health_pc <- health_pc[myvarshealth_pc]

Final_Data_Country <- merge(Final_Data_Country, health_pc, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# add air pollution data --------------------------------------------------
# I also add data on air pollution by nation from: https://data.worldbank.org/indicator/EN.ATM.PM25.MC.M3?view=chart. 
pollution <- read.csv("Data/pollution.csv")

# Next, I correct the country names.
pollution[22, "COUNTRY"] <- "Bahamas"
pollution[30, "COUNTRY"] <- "Brunei"
pollution[42, "COUNTRY"] <- "Congo (Kinshasa)"
pollution[43, "COUNTRY"] <- "Congo (Brazzaville)"
pollution[53, "COUNTRY"] <- "Czechia"
pollution[66, "COUNTRY"] <- "Egypt"
pollution[85, "COUNTRY"] <- "Gambia"
pollution[111, "COUNTRY"] <- "Iran"
pollution[125, "COUNTRY"] <- "Korea, South"
pollution[121, "COUNTRY"] <- "Kyrgyzstan"
pollution[128, "COUNTRY"] <- "Laos"
pollution[132, "COUNTRY"] <- "Saint Lucia"
pollution[159, "COUNTRY"] <- "Burma"
pollution[201, "COUNTRY"] <- "Russia"
pollution[220, "COUNTRY"] <- "Slovakia"
pollution[226, "COUNTRY"] <- "Syria"
pollution[250, "COUNTRY"] <- "US"
pollution[252, "COUNTRY"] <- "Saint Vincent and the Grenadines"
pollution[253, "COUNTRY"] <- "Venezuela"
pollution[255, "COUNTRY"] <- "Virgin Islands"
pollution[261, "COUNTRY"] <- "Yemen"
pollution[7, "COUNTRY"] <- "United Arab Emirates"
pollution[80, "COUNTRY"] <- "United Kingdom"
pollution[179, "COUNTRY"] <- "New Zealand"
pollution[204, "COUNTRY"] <- "Saudi Arabia"
pollution[262, "COUNTRY"] <- "South Africa"

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(pollution$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithouthealth_ocdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithouthealth_ocdata
# Some countries have NAs

pollution <- subset(pollution, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(pollution)
myvarspollution <- c("pollution", "COUNTRY")

pollution <- pollution[myvarspollution]

Final_Data_Country <- merge(Final_Data_Country, pollution, by = c("COUNTRY"), all=TRUE)
# =========================================================================.



# add democracy data ------------------------------------------------------
# Here, I add democracy indicators from Freedom House: https://freedomhouse.org/countries/freedom-world/scores and the Economist: https://www.eiu.com/topic/democracy-index?&zid=democracyindex2019&utm_source=blog&utm_medium=blog&utm_name=democracyindex2019&utm_term=democracyindex2019&utm_content=top_link. 
democracy <- read.csv("Data/democracy.csv")

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(democracy$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutdemocracydata<- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutdemocracydata

democracy <- subset(democracy, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(democracy)
myvarsdemocracy <- c("EUI_democracy", "freedom_house", "COUNTRY")

democracy <- democracy[myvarsdemocracy]

Final_Data_Country <- merge(Final_Data_Country, democracy, by = c("COUNTRY"), all=TRUE)
# =========================================================================.



# add cellular subscriptions data -----------------------------------------
# Here, I add data on the number of cellular subscriptions by nation from: https://data.worldbank.org/indicator/IT.CEL.SETS.P2?start=196.
cellular <- read.csv("Data/cellular.csv")

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(cellular$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutcellulardata<- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutcellulardata

cellular <- subset(cellular, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(cellular)
myvarscellular <- c("cellular_sub", "COUNTRY")

cellular <- cellular[myvarscellular]

Final_Data_Country <- merge(Final_Data_Country, cellular, by = c("COUNTRY"), all=TRUE)
# =========================================================================.



# add military data -------------------------------------------------------
# Now, I add military data from: https://correlatesofwar.org/data-sets/national-material-capabilities. Note the l.y.a. is 2012.
military <- read.csv("Data/military.csv")

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(military$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutmilitarydata<- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutmilitarydata

military <- subset(military, COUNTRY %in% Final_Data_Country$COUNTRY)

# I only keep the columns I care about.
colnames(military)
myvarsmilitary <- c("milex", "milper", "irst", "pec", "cinc", "COUNTRY")

military <- military[myvarsmilitary]

Final_Data_Country <- merge(Final_Data_Country, military, by = c("COUNTRY"), all=TRUE)
# =========================================================================.




# add prior deaths data ---------------------------------------------------
# Adding data on prior deaths from diseases.
# I now add data on diseases (aggregated from 2015-2018) from the WHO ICD10: https://www.who.int/classifications/icd/icdonlineversions/en/. 
diseases <- read.csv("data/ICD_Deaths_Final.csv")

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(diseases$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutdiseasesdata<- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutdiseasesdata

diseases <- subset(diseases, COUNTRY %in% Final_Data_Country$COUNTRY)

Final_Data_Country <- merge(Final_Data_Country, diseases, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.



# label variables ---------------------------------------------------------
# Performing final edits and saving the dataframe (plus filtering the sample).
# Labelling our variables.
label(Final_Data_Country$C1_School.closing) <- "Record Closings of Schools and Universities (0 - No Measures; 1 - Recommended Closing; 2 - Required Closing (some levels); 3 - Required Closing (all levels))"
label(Final_Data_Country$C1_Flag) <- "Targeted vs. General School Closings (0 - Targeted; 1 - General)"
label(Final_Data_Country$C2_Workplace.closing) <- "Record Closings of Workplaces (0 - No Measures; 1 - Recommended Closing; 2 - Required Closing (some sectors); 3 - Required Closing (all but essential workers))"
label(Final_Data_Country$C2_Flag) <- "Targeted vs. General Workplace Closings (0 - Targeted; 1 - General)"
label(Final_Data_Country$C3_Cancel.public.events) <- "Record Closings of Public Events (0 - No Measures; 1 - Recommended Cancelling; 2 - Required Cancelling)"
label(Final_Data_Country$C3_Flag) <- "Targeted vs. General Public Event Closings (0 - Targeted; 1 - General)"
label(Final_Data_Country$C4_Restrictions.on.gatherings) <- "Restrictions on Public Gatherings (0 - No Measures; 1 - Restrictions on Gatherings of 1000 + people; 2 - Restrictions on Gatherings of 100-1000 people; 3 - Restrictions on Gatherings of 10-100 people; 4 - Restrictions on Gatherings of < 10 people)"
label(Final_Data_Country$C4_Flag) <- "Targeted vs. General Public Gathering Restrictions (0 - Targeted; 1 - General)"
label(Final_Data_Country$C5_Close.public.transport) <- "Record Closings of Public Transport (0 - No Measures; 1 - Recommended Closing (means of transport reduced); 2 - Required Closing (prohibit public transit operations))"
label(Final_Data_Country$C5_Flag) <- "Targeted vs. General Public Transport Closings (0 - Targeted; 1 - General)"
label(Final_Data_Country$C6_Stay.at.home.requirements) <- "Lockdown Measures (0 - No Measures; 1 - Recommendeded; 2 - Lax Lockdown (allows for exercise, shopping, and essential trips); 3 - Strict Lockdown)"
label(Final_Data_Country$C6_Flag) <- "Targeted vs. General Lockdown (0 - Targeted; 1 - General)"
label(Final_Data_Country$C7_Restrictions.on.internal.movement) <- "Record Restrictions on Internal Movement (0 - No Measures; 1 - Recommend Movement Restriction (reduce means of transport); 2 - Restrict Movement (prohibit internal movement))"
label(Final_Data_Country$C7_Flag) <- "Targeted vs. General Restrictions on Internal Movement (0 - Targeted; 1 - General)"
label(Final_Data_Country$C8_International.travel.controls) <- "Record Restrictions on International Travel (0 - No Measures; 1 - Screening; 2 - Quarantine on High-Risk Regions; 3 - Ban on High-Risk Regions)"
label(Final_Data_Country$E1_Income.support) <- "Income Support (0 - None; 1 - Govt. Funds < 50% of Lost Salary; 2 - Govt. Funds > 50% of Lost Salary)"
label(Final_Data_Country$E1_Flag) <- "Targeted vs. General Income Support"
label(Final_Data_Country$E2_Debt.contract.relief) <- "Debt Relief (0 - None; 1 - Narrow Relief (specific kinds of contracts); 2 - Broad Relief)"
label(Final_Data_Country$E3_Fiscal.measures) <- "Value of Fiscal Stimuli, Including Spending or Tax Cuts (in USD)"
label(Final_Data_Country$H1_Public.information.campaigns) <- "Record Presence of Public Information Campaigns (0 - No; 1 - Public Officials Urge Caution; 2 - Coordinated Public Information Campaign)"
label(Final_Data_Country$H1_Flag) <- "Targeted vs. General Public Information Campaigns (0 - Targeted; 1 - General)"
label(Final_Data_Country$H2_Testing.policy) <- "Who Can Get Tested (0 - No Testing Policy; 1 - Testing for those with a) Symptoms AND b) Meet Given Criteria (key workers; admitted to hospital; came into contact with known patient; returned from overseas); 2 - Testing of anyone showing symptoms; 3 - Open Public Testing ('drive-through testing')"
label(Final_Data_Country$H3_Contact.tracing) <- "Contact Tracing (0 - No Contact Tracing; 1 - Limited Contact Tracing; 2 - Comprehensive Contact Tracing)"
label(Final_Data_Country$H4_Emergency.investment.in.healthcare) <- "Value of New Short-Term Spending on Health (in USD)"
label(Final_Data_Country$H5_Investment.in.vaccines) <- "Value of Investment (in USD)"
label(Final_Data_Country$StringencyIndex) <- "Index for Government Stringency (calculated using C1-S8 + H1)"
label(Final_Data_Country$Oxford_Cases) <- "Cumulative Sum of Confirmed Cases by Country (Oxford)"
label(Final_Data_Country$Oxford_Deaths) <- "Cumulative Sum of Deaths by Country (Oxford)"
label(Final_Data_Country$prop65) <- "Proportion of the Population Above Age 65"
label(Final_Data_Country$propurban) <- "Proportion of Population in Urban Areas"
label(Final_Data_Country$popdensity) <- "People per 100 Sq. Km. of Land Area"
label(Final_Data_Country$arrivals) <- "Number of Tourist Arrivals"
label(Final_Data_Country$departures) <- "Number of Tourist Departures"
label(Final_Data_Country$vul_emp) <- "Percentage of Population in Vulnerable Employment"
label(Final_Data_Country$GNI) <- "GNI per capita (current USD - Atlas Method) "
label(Final_Data_Country$health_exp) <- "Health Expenditure Per Capita"
label(Final_Data_Country$pollution) <- "PM2.5 Air Pollution (micrograms per cubic meter)"
# These next two generate errors XXX
label(Final_Data_Country$EUI_democracy) <- "Democracy scores from the Economist Intelligence Unit"
label(Final_Data_Country$freedom_house) <- "Democracy scores from Freedom House"
label(Final_Data_Country$milex) <- "Military expenditures (thousands of GBP)"
label(Final_Data_Country$milper) <- "Military personnel (thousands of soldiers)"
label(Final_Data_Country$irst) <- "Iron and steel production (thousands of ton)"
label(Final_Data_Country$pec) <- "Primary energy consumption (thousands of coal-ton equivalents)"
label(Final_Data_Country$cinc) <- "Composite Index of National Capability score"

# Because the country names in Final_Data_Country are not according to the same standard as ISO country names (for example "Philippines" instead of "Philippines (the)"), I need to export the file below to do some manual Excel adjustment work to make sure that the names of the countries in the "SAMPLE_LISTS" sheet are conform with the country names in Final_Country_Data
laender <- as.data.frame(unique(Final_Data_Country$COUNTRY))
# write_xlsx(laender,"Data/laender.xlsx")
# write_xlsx(em_countries, "Data/EMcountries.xlsx")
library(writexl)
library(readxl)
# =========================================================================.




# define sample -----------------------------------------------------------
# I restrict the analysis to Emerging Market countries, from a list pulled from here: https://www.ishares.com/us/products/239572/ishares-jp-morgan-usd-emerging-markets-bond-etf for EMBI (plus India and Thailand for EMBI+2) and here https://www.ishares.com/us/products/239528/ishares-emerging-markets-local-currency-bond-etf for LEMB
EMBI <- read_excel("Data/SAMPLE_LISTS.xlsx", sheet = "EMBI")
EMBI2 <- read_excel("Data/SAMPLE_LISTS.xlsx", sheet = "EMBI2")
LEMB <- read_excel("Data/SAMPLE_LISTS.xlsx", sheet = "LEMB")
uniquecountries <- unique(rbind(EMBI, EMBI2, LEMB))
# =========================================================================.



# Creating a safety copy of Final_Data_Country so that I don't have to rerun the entire previous code in case I subset the wrong way. 
# safetyFinal_Data_Country <- Final_Data_Country
# str(safetyFinal_Data_Country)
Final_Data_Country <- subset(Final_Data_Country, COUNTRY %in% uniquecountries$Country)
# test <- subset(Final_Data_Country, COUNTRY %in% EMBI$Country)
# subset(Final_Data_Country, (COUNTRY == "Austria" | COUNTRY == "Belgium" | COUNTRY == "Cyprus" | COUNTRY == "Estonia" | COUNTRY == "Finland" | COUNTRY == "France" | COUNTRY == "Germany" | COUNTRY == "Greece" | COUNTRY == "Ireland" | COUNTRY == "Italy" | COUNTRY == "Latvia" | COUNTRY == "Lithuania" | COUNTRY == "Luxembourg" | COUNTRY == "Malta" | COUNTRY == "Netherlands" | COUNTRY == "Portugal" | COUNTRY == "Slovakia" | COUNTRY == "Slovenia" | COUNTRY == "Spain"))


# test <- subset(Final_Data_Country, COUNTRY %in% EMBI$Country)
# anothertest <- subset(Final_Data_Country, COUNTRY %in% EMBI2$Country)
# anothertest2 <- subset(Final_Data_Country, COUNTRY %in% LEMB$Country)
# 
# test <- subset(Final_Data_Country$COUNTRY %in% mycountriesoffocus$EMBI_Countries)
# test <- subset(mycountriesoffocus, EMBI_Countries %in% Final_Data_Country$COUNTRY)
# test2 <- subset(Final_Data_Country, COUNTRY %in% mycountriesoffocus$EMBI_Countries )
# unique(test2$COUNTRY)
# laender
# test2 <- subset(Final_Data_Country, )



# Final_Data_Country <- subset(Final_Data_Country, (COUNTRY == "Austria" | COUNTRY == "Belgium" | COUNTRY == "Cyprus" | COUNTRY == "Estonia" | COUNTRY == "Finland" | COUNTRY == "France" | COUNTRY == "Germany" | COUNTRY == "Greece" | COUNTRY == "Ireland" | COUNTRY == "Italy" | COUNTRY == "Latvia" | COUNTRY == "Lithuania" | COUNTRY == "Luxembourg" | COUNTRY == "Malta" | COUNTRY == "Netherlands" | COUNTRY == "Portugal" | COUNTRY == "Slovakia" | COUNTRY == "Slovenia" | COUNTRY == "Spain"))



# add government indicators -----------------------------------------------
# Adding data on government indicators.
# I now add data on government indicators from: https://info.worldbank.org/governance/wgi/Home/Documents. 
govt <- read.csv("Data/govt.csv")

# Checking for completeness of added data source
countriesinnewdataset <- as_tibble(unique(govt$COUNTRY))
countriesintemporaryset <- as_tibble(unique(Temporary_Data_Country$COUNTRY))
countries_not_in_new_data_set <- countriesintemporaryset[countriesintemporaryset$value %notin% countriesinnewdataset$value,]
countries_not_in_new_data_set

problems <- countries_not_in_new_data_set[countries_not_in_new_data_set$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
problems 

realproblems <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %in% problems$value,]
realproblems

countrieswithoutgovtdata <- countrieswithcdsdata[countrieswithcdsdata$COUNTRY5yrCDS %notin% countriesinnewdataset$value,]
countrieswithoutgovtdata

govt <- subset(govt, COUNTRY %in% Final_Data_Country$COUNTRY)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]

Final_Data_Country <- merge(Final_Data_Country, govt, by = c("COUNTRY"), all=TRUE)

Final_Data_Country <- Final_Data_Country[order(Final_Data_Country$COUNTRY, Final_Data_Country$Date),]
# =========================================================================.

unique(Final_Data_Country$COUNTRY)

# saving Oxford_V1  --------------------------------------------------------
Oxford_V1 <- Final_Data_Country
View(Oxford_V1)
# Once the dataset "Oxford_V1" is created, a few variables still have to be added. However, these variables are hand-coded. 
# Thus, I first export/save the dataset "Oxford_V1" and then manually add the variable in the exported excel sheet. 
# Upon hand-coding, I then re-import the augmented sheet. 
# =========================================================================.



# IMPORTANT ---------------------------------------------------------------
# 1. step: export dataframe
# write_xlsx(Oxford_V1,"Data/Oxford_V1.xlsx")
# 2. step: handcoding FED, ECB etc. dummy time series and saving it as "Oxford_V1_dummyts_additions.xlsx"
# 3. step: importing additional dummy ts
# Oxford_addition <- read_excel("Data/Oxford_V1_dummyts_additions.xlsx")
# 4. step: merging Oxford_V1 with Oxford_addition and calling it mergedOxford
# Oxford_V1 <- as_tibble(merge(Oxford_V1, Oxford_addition, by=c("COUNTRY", "Date")))
# View(Oxford_V1)
# 5. step: For future reference, I am gong to export the augmented dataset once so I have it again when I need it:
# write_xlsx(Oxford_V1,"Data/Oxford_V1_augmented_3_8_2020.xlsx")
# If I ever wanted to reimport the dataset instead of performing every step so far just use the line below
# Oxford_V1 <- read_excel("Data/Oxford_V1_augmented_3_8_2020.xlsx")
# =========================================================================.



# Now, I calculate deaths per million.
library(data.table)
library(dplyr)
library(writexl)
library(readxl)



# add population data in million ------------------------------------------
# To calculate deaths per million, news sources divide the number of cumulative (total) deaths by the population size, measured in millions. Luxembourg and Malta have less than a million inhabitants, and thus I do not calculate deaths per million for these nations. Data was pulled from: https://www.statista.com/statistics/1104709/coronavirus-deaths-worldwide-per-million-inhabitants/.
Mill_Pop <- read_excel("Data/Mill_Pop.xlsx")

Oxford_V1 <- merge(Oxford_V1, Mill_Pop, by = c("COUNTRY"), all=TRUE)

Oxford_V1 <- Oxford_V1[order(Oxford_V1$COUNTRY, Oxford_V1$Date),]

Oxford_V1$New_Deaths_Per_Million = (Oxford_V1$New_Total_Deceased_Country / Oxford_V1$PopMil)

Oxford_V1 <- Oxford_V1[order(Oxford_V1$COUNTRY, Oxford_V1$Date),]

Oxford_V1$Total_Deaths_Per_Million = (Oxford_V1$Total_Deceased_Country / Oxford_V1$PopMil)
# =========================================================================.




# visualizations ----------------------------------------------------------
# =========================================================================.


# load libraries ----------------------------------------------------------
library(lubridate)
library(zoo)
library(quantmod)
library(fBasics)
library(tseries)
library(sandwich)
library(lmtest)
library(lattice)
library(xtable)
library(vars)
library(plyr)
library(gridExtra)
library(corrplot)
library(ggplot2)
library(reshape2)
library(data.table)
library(rvest)
library(foreign)
library(dplyr)
library(plm)
library(stringr)
library(stargazer)
library(survival)
library(ggfortify)
library(plotly)
library(sf)
library(readr)
library(mapview)
library(ggplot2)
library(tidyverse)
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
# XXX here's a problem with summarytools
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
library(haven)
# =========================================================================.



# remove factor variables for government responses ------------------------
Oxford_V1<-Oxford_V1 %>% dplyr::select(-contains("Flag"))
Oxford_V1<-Oxford_V1 %>% dplyr::select(-contains("Lagged"))
Oxford_V1$latitude <- NULL
Oxford_V1$longitude <- NULL



# Days since 100 deaths
Days_Since_100_Deceased <- Oxford_V1 %>%
  dplyr:: filter(Total_Deceased_Country >= 100)

Days_Since_100_Deceased <- Days_Since_100_Deceased %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(Days = row_number(COUNTRY))

Plot_10 <- plot_ly(Days_Since_100_Deceased, x=~Days, y=~Total_Deceased_Country) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Days Since 100 Deaths - Emerging Markets")
Plot_10
# =========================================================================.



# 7-day moving average graphs ---------------------------------------------
# Next, I construct a graph illustrating the seven-day rolling average of new deaths, new cases, and the mortality rate per capita. This is a version of the fourth Bloomberg plot from the above link. You will need to slide to the right (to March 2020) to see variation in these plots (or simply check the output folder). 
# Please note that Hopkins revised their estimates of deaths in Spain downwards on May 27th. We do not include dates for which the cumulative sum of deaths decreases (as this is not intuitive), and hence there are some gaps in the visualizations.
Oxford_V1 <- Oxford_V1[order(Oxford_V1$COUNTRY, Oxford_V1$Date),]

Plot_13 <- plot_ly(Oxford_V1, x=~Date, y=~rolling_average_confirmed) %>% add_lines(linetype = ~COUNTRY) %>% layout(title="Rolling Average of Confirmed Cases")
Plot_13

Plot_14 <- plot_ly(Oxford_V1, x=~Date, y=~rolling_average_deceased) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Rolling Average of Deaths")
Plot_14

Plot_15 <- plot_ly(Oxford_V1, x=~Date, y=~total_rolling_average_mortality) %>% add_lines(linetype = ~COUNTRY) %>% layout(title="Total Rolling Average of Mortality Rate per Capita")
Plot_15

Plot_16 <- plot_ly(Oxford_V1, x=~Date, y=~new_rolling_average_mortality) %>% add_lines(linetype = ~COUNTRY) %>% layout(title="New Rolling Average of Mortality Rate per Capita")
Plot_16

# Here, I plot the global confirmed cases over our time period.
exponential <- function(x) exp(x)
exponential_function <- ggplot(data = data.frame(x=c(0,10)), mapping=aes(x=x)) + stat_function(fun = exponential)
exponential_function

# Here I subset only for data from 22-1-2020 to end of June XXX
#str(World_Data$Date)
# World_Data$Date <- as.Date(World_Data$Date)
# World_Data <- subset(World_Data, Date < as.Date("2020-06-30") )

Confirmed <- ggplot(data = subset(World_Data, Date < as.Date("2020-06-30")) , aes(x = Date, y = Global_Confirmed_Cases)) + geom_bar(stat = "identity", fill = "red") +
  labs(title = "Global Confirmed Cases - COVID19",
       subtitle = "January 22nd, 2020 - June 30th, 2020",
       x = "Date", y = "Confirmed Cases")
Plot_17 <- ggplotly(Confirmed)
Plot_17

# Now, I plot the global deaths over our time period.
Deaths <- ggplot(data = subset(World_Data, Date < as.Date("2020-06-30")), aes(x = Date, y = Global_Deceased)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Global Deaths - COVID19",
       subtitle = "January 22nd, 2020 - June 30th, 2020",
       x = "Date", y = "Deaths")
Plot_18 <- ggplotly(Deaths)
Plot_18 

# Now, I plot the change in the global death rate across the time period.
Death_Rate <- ggplot(data = subset(World_Data, Date < as.Date("2020-06-30")), aes(x=Date)) +
  geom_line(aes(y=Death_Rate, colour='Daily')) +
  xlab('') + ylab('Death Rate (%)') + labs(title='Change in Death Rate (%)', subtitle = "January 22nd, 2020 - June 30th, 2020") +
  theme(legend.position='bottom', legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.key.size=unit(0.5, 'cm'),
        axis.text.x=element_text(angle=45, hjust=1))
Plot_21 <- ggplotly(Death_Rate)
Plot_21

# Now, I extend this analysis to generate the number of confirmed cases and deaths by country.
Plot_23 <- plot_ly(data = subset(Oxford_V1, Date < as.Date("2020-06-30")), x=~Date, y=~Total_Cases_Country) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Confirmed Cases Across Countries")
Plot_23

Plot_24 <- plot_ly(data = subset(Oxford_V1, Date < as.Date("2020-06-30")), x=~Date, y=~Total_Deceased_Country) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Deaths Across Countries")
Plot_24

Plot_25 <- plot_ly(data = subset(Oxford_V1, Date < as.Date("2020-06-30")), x=~Date, y=~Total_Mortality_Rate_Per_Capita) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Total Mortality Rate Across Countries")
Plot_25

# This was the original time frame
# Plot_26 <- plot_ly(data = subset(Oxford_V1, Date < as.Date("2020-06-30")), x=~Date, y=~New_Mortality_Rate_Per_Capita) %>%
#   add_lines(linetype = ~COUNTRY) %>%
#   layout(title="New Mortality Rate Across Countries")
# Plot_26

# I subset for March to June
Plot_26 <- plot_ly(data = subset(Oxford_V1, Date < as.Date("2020-06-30") & Date >= as.Date("2020-04-01") ), x=~Date, y=~New_Mortality_Rate_Per_Capita) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="COVID-19 mortality rate curves, by country") # New Mortality Rate Across Countries
Plot_26
# orca(Plot_26, "Plots/Weighted/Figure4a.pdf")
# =========================================================================.



# weekly growth rate visualizations ---------------------------------------
# Here, I filter by first deaths per country.
First_Death <- Oxford_V1 %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::filter(Total_Deceased_Country > 0)

# Now, I calculate the week-by-week growth rate in the rolling average of the mortality rate. This is equivalent to the log[total_total_rolling_average_mortality_(t) – total_total_rolling_average_mortality_(t-7)]. First, I create a variable for weeks by country. This tells us in what week of the year each nation had their first death.
First_Death$week_num = lubridate::week(ymd(First_Death$Date))

# Next, I generate week-over-week mortality growth rates. Should be (current week - previous week)/previous week. I use two distinct formulas for this calculation.
First_Death <- First_Death %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(total_mortality_growth = log((total_rolling_average_mortality/Lag(total_rolling_average_mortality,7))))

First_Death <- First_Death %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(total_mortality_growth_b <- (total_rolling_average_mortality-Lag(total_rolling_average_mortality,7))/Lag(total_rolling_average_mortality,7))

First_Death$total_mortality_growth_b  <- log(First_Death$`... <- NULL`,10)

First_Death <- First_Death %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(new_mortality_growth <- log((new_rolling_average_mortality/Lag(new_rolling_average_mortality,7)))) 

First_Death$new_mortality_growth  <- First_Death$`... <- NULL`

First_Death <- First_Death %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::mutate(new_mortality_growth_b <- (new_rolling_average_mortality-Lag(new_rolling_average_mortality,7))/Lag(new_rolling_average_mortality,7))

First_Death$new_mortality_growth_b  <- log(First_Death$`... <- NULL`,10)
# =========================================================================.



# rolling average visualizations ------------------------------------------
# The following code allows us to visualize the rolling average of the mortality rate by country.
Plot_27A <- plot_ly(data  = subset(First_Death, Date < as.Date("2020-06-30") & Date >= as.Date("2020-04-01") ), x=~Date, y=~total_rolling_average_mortality) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="COVID-19 deaths per million, by country")
Plot_27A
# orca(Plot_27A, "Plots/Weighted/Figure4b.pdf")

Plot_28 <- Plot_27A %>% layout(yaxis = list(type = "log"))
Plot_28

Plot_27B <- plot_ly(First_Death, x=~Date, y=~new_rolling_average_mortality) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="New Rolling Average of Mortality Rate Over Time - COVID19")
Plot_27B

# Here, I add a facet wrap for each nation in our sample.
Plot_28B <- ggplot(data = First_Death, aes(Date, log(total_rolling_average_mortality))) + geom_line(size=.7) + facet_wrap(~COUNTRY, switch="x", ncol = 7) + theme_bw() + theme(axis.title.y=element_blank(),
                                                                                                                                                                               axis.text.y=element_blank(),
                                                                                                                                                                               axis.text.x=element_blank(),
                                                                                                                                                                               axis.ticks.x=element_blank(),
                                                                                                                                                                               axis.title.x = element_blank(),
                                                                                                                                                                               strip.background = element_rect(color="white", fill="white"))
Plot_28B

Plot_28C <- ggplot(data = First_Death, aes(x = Date, y = new_rolling_average_mortality, group = COUNTRY)) + geom_line() + theme_bw() + theme(legend.title = element_blank(), axis.text.y=element_blank(), axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("Rolling Average New Mortality")
Plot_28C


### Generating Figures 1.1, 1.2 and 1.3. xxx
library(gghighlight)

Plot_28D <- ggplot(data = First_Death, aes(x = Date, y = new_rolling_average_mortality * 100, color = COUNTRY)) + geom_line() + theme_bw() + theme(axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("New Mortality Rate (%)")
Plot_28D
# ggsave("Plots/Figure4a.pdf")

# Here I am subsetting for the top and bottom 5 mortality countries per the end of April 
First_Death_April30 <- subset(First_Death, Date == as.Date("2020-04-30"))

First_Death_April30decreasing <- arrange(First_Death_April30, desc(new_rolling_average_mortality) )
Top5_Mortality_Countries <- First_Death_April30decreasing[1:5, "COUNTRY"]

First_Death_April30increasing <- arrange(First_Death_April30, new_rolling_average_mortality )
Bottom5_Mortality_Countries <- First_Death_April30increasing[1:5, "COUNTRY"]

First_Death_High <- First_Death %>%
  dplyr::filter(COUNTRY %in% Top5_Mortality_Countries$COUNTRY)
Plot_28D.1 <- ggplot(data = First_Death_High, aes(x = Date, y = new_rolling_average_mortality * 100, color = COUNTRY)) + geom_line() + theme_bw() + theme(axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("New Mortality Rate (%)")
Plot_28D.1

First_Death_Medium <- First_Death %>%
  dplyr::filter( !(     (COUNTRY %in% Top5_Mortality_Countries$COUNTRY)  | (COUNTRY %in% Bottom5_Mortality_Countries$COUNTRY)  )        )
Plot_28D.2 <- ggplot(data = First_Death_Medium, aes(x = Date, y = new_rolling_average_mortality * 100, color = COUNTRY)) + geom_line() + theme_bw() + theme(axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("New Mortality Rate (%)")
Plot_28D.2

First_Death_Low <- First_Death %>%
  dplyr::filter(COUNTRY %in% Bottom5_Mortality_Countries$COUNTRY)
Plot_28D.3 <- ggplot(data = First_Death_Low, aes(x = Date, y = new_rolling_average_mortality * 100, color = COUNTRY)) + geom_line() + theme_bw() + theme(axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("New Mortality Rate (%)")
Plot_28D.3

Plot_28E <- ggplot(data = First_Death, aes(x = Date, y = log(total_rolling_average_mortality), color = COUNTRY)) + geom_line() + theme_bw() + theme(legend.title = element_blank(), legend.position = "none", axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("Logged Cumulative Mortality Rate")
Plot_28E
# =========================================================================.




# add all country chart ---------------------------------------------------
countriez <- unique(Oxford_V1$COUNTRY)
country_plots<-list()

for(i in 1:length(countriez)) {
  country_plots[[i]] <- ggplot(data=subset(Oxford_V1,COUNTRY==countriez[i]), aes_string(x="Date",y="new_rolling_average_mortality"))+geom_line(size=1)+theme_bw()+ylab("")+xlab(countriez[i])+theme(axis.text=element_blank())
}

Plot28F <- do.call("grid.arrange", c(country_plots))
Plot28F

Oxford_Death <- Oxford_V1 %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::filter(Total_Deceased_Country > 0)

Plot_28G <- ggplot(data = Oxford_Death, aes(x = Date, y = Total_Deaths_Per_Million, color = COUNTRY)) + geom_line() + theme_bw() + theme( axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("Total Deaths Per Million")
Plot_28G
# ggsave("Plots/Figure4b.pdf")

Plot_28G.1 <- plot_ly(Oxford_Death, x=~Date, y=~Total_Deaths_Per_Million) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Total Total_Deaths_Per_Million")
Plot_28G.1
# =========================================================================.



# subset top and bottom 5 countries ---------------------------------------
# Here I am subsetting for the top and bottom 5 mortality countries per the end of April 
Oxford_Death_April30 <- subset(Oxford_Death, Date == as.Date("2020-04-30"))

Oxford_Death_April30decreasing <- arrange(Oxford_Death_April30, desc(Total_Deaths_Per_Million) )
Top5_Oxford_Death_Countries <- Oxford_Death_April30decreasing[1:5, "COUNTRY"]

Oxford_Death_April30increasing <- arrange(Oxford_Death_April30, Total_Deaths_Per_Million )
Bottom5_Oxford_Death_Countries <- First_Death_April30increasing[1:5, "COUNTRY"]
# =========================================================================.

First_Death_High <- First_Death %>%
  dplyr::filter(COUNTRY %in% Top5_Mortality_Countries$COUNTRY)
Plot_28D.1 <- ggplot(data = First_Death_High, aes(x = Date, y = new_rolling_average_mortality * 100, color = COUNTRY)) + geom_line() + theme_bw() + theme(axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("New Mortality Rate (%)")
Plot_28D.1

Oxford_Death_High <- Oxford_V1 %>%
  dplyr::filter(COUNTRY %in% Top5_Oxford_Death_Countries$COUNTRY)
Plot_28H <- ggplot(data = Oxford_Death_High, aes(x = Date, y = Total_Deaths_Per_Million, color = COUNTRY)) + geom_line() + theme_bw() + theme( axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("Total Deaths Per Million")
Plot_28H

Oxford_Death_Medium <- Oxford_V1 %>%
  dplyr::filter( !(     (COUNTRY %in% Top5_Oxford_Death_Countries$COUNTRY)  | (COUNTRY %in% Bottom5_Oxford_Death_Countries$COUNTRY)  )        )
Plot_28I <- ggplot(data = Oxford_Death_Medium, aes(x = Date, y = Total_Deaths_Per_Million, color = COUNTRY)) + geom_line() + theme_bw() + theme( axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("Total Deaths Per Million")
Plot_28I

Oxford_Death_Low <- Oxford_V1 %>%
  dplyr::filter(COUNTRY %in% Bottom5_Oxford_Death_Countries$COUNTRY)
Plot_28J <- ggplot(data = Oxford_Death_Low, aes(x = Date, y = Total_Deaths_Per_Million, color = COUNTRY)) + geom_line() + theme_bw() + theme( axis.title.y=element_text(size=9), axis.title.x=element_blank()) + ylab("Total Deaths Per Million")
Plot_28J

# Next, I create initial visualizations of the mortality growth rates by country.
Plot_29 <- plot_ly(First_Death, x=~Date, y=~total_mortality_growth) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Weekly Growth Rate in Total Rolling Average Mortality per Capita - COVID19")
Plot_29

Plot_30 <- plot_ly(First_Death, x=~Date, y=~total_mortality_growth_b) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Alternate Weekly Growth Rate in Total Rolling Average Mortality per Capita - COVID19")
Plot_30

Plot_31 <- plot_ly(First_Death, x=~Date, y=~new_mortality_growth) %>%
  add_lines(linetype = ~COUNTRY) %>%
  layout(title="Weekly Growth Rate in New Rolling Average Mortality per Capita - COVID19")
Plot_31





# generate summary statistics ---------------------------------------------
# First, we generate extensive summary statistics for each variable with describe in the Hmisc function.
Hmisc::describe(Oxford_V1)
# n, nmiss, unique, mean, 5, 10, 25, 50, 75, 90, 95th percentiles

# XXX this does not work
# Next, I generate shareable outputs, using the summarytools package. Set st_options(use.x11 = FALSE).
# install.packages("summarytools")

# This whole thing here below does not work. xxx 
# saved_x11_option <- st_options("use.x11")
# st_options(use.x11 = TRUE)
# dfSummary(Oxford_V1, plain.ascii = FALSE, style = "grid", 
#           graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")
# view(dfSummary(Oxford_V1))



# For the complete output, please open the summary in the html link.
# =========================================================================.



# create correlation heat map ---------------------------------------------
# Now, I generate a correlation heat map, using a subset of variables from the final dataset.
corrmapvarlist <- c("total_rolling_average_mortality", "Total_Cases_Country", "Total_Deceased_Country", "New_Confirmed_Country", "New_Total_Deceased_Country", "Population", "rolling_average_confirmed", "rolling_average_deceased", "C1_School.closing", "C2_Workplace.closing", "C3_Cancel.public.events", "C4_Restrictions.on.gatherings", "C5_Close.public.transport", "C6_Stay.at.home.requirements", "C7_Restrictions.on.internal.movement", "C8_International.travel.controls", "E1_Income.support", "E2_Debt.contract.relief", "E3_Fiscal.measures", "H1_Public.information.campaigns", "H2_Testing.policy", "H3_Contact.tracing", "H4_Emergency.investment.in.healthcare", "H5_Investment.in.vaccines", "StringencyIndex", "prop65", "propurban", "driving", "walking")
corrmapvars <- Oxford_V1[corrmapvarlist]

# Here, I rename the variables to make them easier to read on the map.
corrmapvars <- corrmapvars %>% dplyr::rename(mortality_ra = total_rolling_average_mortality)
corrmapvars <- corrmapvars %>% dplyr::rename(confirmed_ra = rolling_average_confirmed)
corrmapvars <- corrmapvars %>% dplyr::rename(deceased_ra = rolling_average_deceased)
corrmapvars <- corrmapvars %>% dplyr::rename(cases = Total_Cases_Country)
corrmapvars <- corrmapvars %>% dplyr::rename(deaths = Total_Deceased_Country)
corrmapvars <- corrmapvars %>% dplyr::rename(new_cases = New_Confirmed_Country)
corrmapvars <- corrmapvars %>% dplyr::rename(new_deaths = New_Total_Deceased_Country)
corrmapvars <- corrmapvars %>% dplyr::rename(school_closing = C1_School.closing)
corrmapvars <- corrmapvars %>% dplyr::rename(work_closing = C2_Workplace.closing)
corrmapvars <- corrmapvars %>% dplyr::rename(public_event_closing = C3_Cancel.public.events)
corrmapvars <- corrmapvars %>% dplyr::rename(gathering_restrictions = C4_Restrictions.on.gatherings)
corrmapvars <- corrmapvars %>% dplyr::rename(transport_closing = C5_Close.public.transport)
corrmapvars <- corrmapvars %>% dplyr::rename(lockdown = C6_Stay.at.home.requirements)
corrmapvars <- corrmapvars %>% dplyr::rename(internal_restrictions = C7_Restrictions.on.internal.movement)
corrmapvars <- corrmapvars %>% dplyr::rename(travel_restrictions = C8_International.travel.controls)
corrmapvars <- corrmapvars %>% dplyr::rename(income = E1_Income.support)
corrmapvars <- corrmapvars %>% dplyr::rename(debt = E2_Debt.contract.relief)
corrmapvars <- corrmapvars %>% dplyr::rename(fiscal = E3_Fiscal.measures)
corrmapvars <- corrmapvars %>% dplyr::rename(campaigns = H1_Public.information.campaigns)
corrmapvars <- corrmapvars %>% dplyr::rename(testing = H2_Testing.policy)
corrmapvars <- corrmapvars %>% dplyr::rename(tracing = H3_Contact.tracing)
corrmapvars <- corrmapvars %>% dplyr::rename(health_care = H4_Emergency.investment.in.healthcare)
corrmapvars <- corrmapvars %>% dplyr::rename(vaccine = H5_Investment.in.vaccines)
corrmapvars <- corrmapvars %>% dplyr::rename(stringency_index = StringencyIndex)
corrmapvalues <- round(cor(corrmapvars, use = "pairwise.complete.obs"), 2)

# Here, I get the lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Now, I get the upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  ### Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
corrmapvalues <- reorder_cormat(corrmapvalues)
# Get the Upper Triangle
upper_tri <- get_upper_tri(corrmapvalues) 
# Melt the Correlation Map
melted_corrmap <- reshape2::melt(upper_tri, na.rm = TRUE)

# Generate the Static Correlation Map
ggheatmap <- ggplot(data = melted_corrmap, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
  theme(axis.text.x = element_text(hjust=1))
print(ggheatmap)

# Generate an Interactive Correlation Map (you are able to zoom to specific correlates).
interactive_heatmap <- heatmaply_cor(
  cor(corrmapvalues),
  row_text_angle = 0,
  column_text_angle = 45,
  na.value = "grey50",
  na.rm = TRUE,
  xlab = "Features",
  ylab = "Features",
  k_col = 0,
  k_row = 0,
  fontsize_row = 5,
  fontsize_col = 5,
  plot_method = c("ggplot", "plotly"),
  show_dendrogram = c(FALSE, FALSE),
  key.title = "Pearson Correlations")
interactive_heatmap

# Correlation matrix of government interventions (Table 3B)
cor.policy.dat <- Oxford_V1[,c("C1_School.closing", 
                               "C2_Workplace.closing",
                               "C3_Cancel.public.events",
                               "C4_Restrictions.on.gatherings",
                               "C5_Close.public.transport",            
                               "C6_Stay.at.home.requirements",         
                               "C7_Restrictions.on.internal.movement", 
                               "C8_International.travel.controls",
                               "H1_Public.information.campaigns",
                               "StringencyIndex")]
colnames(cor.policy.dat) <- c("School Closing", 
                              "Workplace Closing",
                              "Cancel Public Events",
                              "Gathering Restrictions",
                              "Close Public Transport",            
                              "Stay at Home",         
                              "Internal Movement Restrictions", 
                              "International Travel Controls",
                              "Public Information Campaigns",
                              "Stringency Index")
cormat<-round(cor(cor.policy.dat,use = "complete.obs"), 2)

# Here, I get the lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Now, I get the upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
melted_cormat <- reshape2::melt(get_upper_tri(cormat), na.rm = TRUE)
# =========================================================================.



# create static correlation map -------------------------------------------
# Generate the Static Correlation Map
ggheatmap <- ggplot(data = melted_cormat, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
  theme(axis.text.x = element_text(hjust=1))+coord_fixed()

Plot_78 <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.75) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text=element_text(size=7),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    legend.title = element_text(size = 7),
    legend.text=element_text(size=7),
    plot.caption = element_text(size = 7, hjust=0))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Print the heatmap
print(Plot_78)
# =========================================================================.




# generate multi-factor model ---------------------------------------------
# Generating the heterogenous multi-factor model.
# =========================================================================.



# loading libraries -------------------------------------------------------
library(lubridate)
library(zoo)
library(quantmod)
library(fBasics)
library(tseries)
library(sandwich)
library(lmtest)
library(lattice)
library(xtable)
library(vars)
library(plyr)
library(gridExtra)
library(corrplot)
library(ggplot2)
library(reshape2)
library(data.table)
library(rvest)
library(plm)
library(stringr)
library(stargazer)
library(ggpubr)
# =========================================================================.



# read in cds ddata -------------------------------------------------------
# cds_five<-read.csv("data/cds_daily_5y.csv",header=T,sep=',')
library(readxl)
cds_five <- read_excel("Data/CDS.xlsx", sheet = "5yrCDS")
countries <- read_excel("Data/laender.xlsx", sheet = "EM")


# define EM countries -----------------------------------------------------
# subsetting for only EM and non-EM countries and for between 2020-06-30 and 2014-01-01 
em_countries <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"   ]
non_em_countries <- countries[ which(countries$EM_dummy5yr !=1), "COUNTRY5yrCDS"   ]
cds_five <- cds_five[which(cds_five$Date>="2014-01-01" & cds_five$Date<="2020-07-01"   ),]
# =========================================================================.



# subset CES for EM and non EM --------------------------------------------
em_cds <- cds_five[, c("Date", em_countries$COUNTRY5yrCDS )]
nonem_cds<-cds_five[, c("Date", non_em_countries$COUNTRY5yrCDS )]
# =========================================================================.

`%notin%` <- Negate(`%in%`)
countriesinoxforddataset <- as_tibble(unique(Oxford_V1$COUNTRY))
countrieswithcdsdata <- em_countries
countrieswithoutcds <- countriesinoxforddataset[countriesinoxforddataset$value %notin% countrieswithcdsdata$COUNTRY5yrCDS,]
countrieswithoutcds

# write_xlsx(countrieswithcdsdata, "Data/countrieswithcdsdata.xlsx")


# visualize individual spreads --------------------------------------------
# plot individual spreads for EM
pdat<-melt(em_cds,id.vars="Date")
countriez<-unique(pdat$variable)
country_plots<-list()

for(i in 1:length(countriez)) {
  country_plots[[i]] <- ggplot(data=subset(pdat,variable==countriez[i]), aes_string(x="Date",y="value"))+geom_line(size=1)+theme_bw()+ylab("")+xlab(countriez[i])
}
EMindividualspreadsroster <- do.call("grid.arrange", c(country_plots))
EMindividualspreadsroster 
# XYZ plot
# Saving the plot
# pdf("Plots/weighted/individualspreads.pdf")
# grid.arrange(EMindividualspreadsroster,nrow=1)
# dev.off() 

# plot individual spreads for non EM
pdat<-melt(nonem_cds,id.vars="Date")
countriez<-unique(pdat$variable)
country_plots<-list()

for(i in 1:length(countriez)) {
  country_plots[[i]] <- ggplot(data=subset(pdat,variable==countriez[i]), aes_string(x="Date",y="value"))+geom_line(size=1)+theme_bw()+ylab("")+xlab(countriez[i])
}
nonEMindividualspreadsroster <- do.call("grid.arrange", c(country_plots))
nonEMindividualspreadsroster
# XYZ plot
# Saving the plot
# jpeg("Plots/individualspreads.jpg", width = 1920, height = 1080)
# do.call("grid.arrange", c(country_plots))
# dev.off()
# =========================================================================.



# Create country weights for CDS data -------------------------------------
# Importing GDP 2019 data for developed markets
GDP2019USDMIL_D <- read_excel("Data/GDP.xlsx", sheet = "GLOBAL")
total <- sum(GDP2019USDMIL_D$GDP2019MIL)
GDP2019USDMIL_D <- GDP2019USDMIL_D %>% mutate(weight = GDP2019MIL/ total  )

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

# sum of others
for(i in 1:(nrow(Africa)) ) {
  Africa[i,"sumofothers"] <-  sum(Africa[-i,]$GDP2019MIL) 
}

for(i in 1:(nrow(CentralAsia)) ) {
  CentralAsia[i,"sumofothers"] <-  sum(CentralAsia[-i,]$GDP2019MIL) 
}

for(i in 1:(nrow(EastAsia)) ) {
  EastAsia[i,"sumofothers"] <-  sum(EastAsia[-i,]$GDP2019MIL) 
}

for(i in 1:(nrow(Europe)) ) {
  Europe[i,"sumofothers"] <-  sum(Europe[-i,]$GDP2019MIL) 
}

for(i in 1:(nrow(LATAM)) ) {
  LATAM[i,"sumofothers"] <-  sum(LATAM[-i,]$GDP2019MIL) 
}

for(i in 1:(nrow(MiddleEast)) ) {
  MiddleEast[i,"sumofothers"] <-  sum(MiddleEast[-i,]$GDP2019MIL) 
}

for(i in 1:(nrow(SouthAsia)) ) {
  SouthAsia[i,"sumofothers"] <-  sum(SouthAsia[-i,]$GDP2019MIL) 
}

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

# =========================================================================.


# prepare log difference data sets ----------------------------------------
###Prepare data for regression
em_cds2<- em_cds

# em_cds_test <- em_cds[which(em_cds$Date>="2020-06-20" & cds_five$Date<="2020-07-01"   ),]
# d_emcds_test<-apply(log(em_cds_test[,-1]),2,diff) #log differences of EM spreads
# View(em_cds_test)
# View(d_emcds_test)

d_emcds<-apply(log(em_cds2[,-1]),2,diff) #log differences of EM spreads
d_nemds<-apply(log(nonem_cds[,-1]),2,diff) # log differences of non-EM spreads


# head(em_cds2)
# d_emcds_test <- log(em_cds2[,-1])
# head(d_emcds_test)
# diff_d_emcds_test <- apply(d_emcds_test, 2, diff)
# head(diff_d_emcds_test)

# This was the old way before we started weighting things
#glo_cds <- rowMeans(d_nemds) 
# =========================================================================.



# global weight vector ----------------------------------------------------
# This is the new way when we start weighting things
glo_cds <- rowSums( t(t(d_nemds)*GDP2019USDMIL_D$weight) )
# =========================================================================.



# Africa weights ----------------------------------------------------------
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
# =========================================================================.





# Central Asia weights ----------------------------------------------------
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
# =========================================================================.




# East Asia weights -------------------------------------------------------
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
# =========================================================================.




# Europe weights ----------------------------------------------------------
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
# =========================================================================.




# LATAM weights -----------------------------------------------------------
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
# =========================================================================.





# Middle East weights -----------------------------------------------------
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
# =========================================================================.





# South Asia weights ------------------------------------------------------
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
# =========================================================================.




# regional weight vector --------------------------------------------------
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
# =========================================================================.



# train test sample split -------------------------------------------------
###training - test sample split
pre.dat<-d_emcds[which(em_cds2$Date[-1]<"2019-06-30"),]
post.dat<-d_emcds[which(em_cds2$Date[-1]>="2019-06-30"),]

glo_cds_pre<-glo_cds[which(em_cds2$Date[-1]<"2019-06-30")]
glo_cds_post<-glo_cds[which(em_cds2$Date[-1]>="2019-06-30")]

em_fac_pre<-em_fac[which(em_cds2$Date[-1]<"2019-06-30"),]
em_fac_post<-em_fac[which(em_cds2$Date[-1]>="2019-06-30"),]

# Same -test sample kit for developed countries
pre.dat.nonem <-d_nemds[which(nonem_cds$Date[-1]<"2019-06-30"),]
post.dat.nonem<-d_nemds[which(nonem_cds$Date[-1]>="2019-06-30"),]

glo_cds_pre.nonem<-glo_cds[which(nonem_cds$Date[-1]<"2019-06-30")]
glo_cds_post.nonem<-glo_cds[which(nonem_cds$Date[-1]>="2019-06-30")]

nonem_fac_pre<-em_fac[which(nonem_cds$Date[-1]<"2019-06-30"),]
nonem_fac_post<-em_fac[which(nonem_cds$Date[-1]>="2019-06-30"),]
# =========================================================================.



# fit the models ----------------------------------------------------------
# Fit the models for developed countries
coefz.nonem<-matrix(NA,ncol=4,nrow=ncol(pre.dat.nonem))
predz.nonem<-matrix(NA,ncol=ncol(pre.dat.nonem),nrow=(nrow(post.dat.nonem)))
rsqz.nonem<-c(0)
for(i in 1:nrow(coefz.nonem)){
  mod<-lm(pre.dat.nonem[,i]~Lag(pre.dat.nonem[,i],1)+glo_cds_pre.nonem+nonem_fac_pre[,i])
  coefz.nonem[i,]<-coef(mod)
  predz.nonem[,i]<-coef(mod)[1]*rep(1,nrow(post.dat.nonem))+coef(mod)[2]*Lag(post.dat.nonem[,i],1)+coef(mod)[3]*glo_cds_post.nonem+coef(mod)[4]*nonem_fac_post[,i]
  rsqz.nonem[i]<-summary(mod)$r.squared
}
colnames(predz.nonem)<-colnames(post.dat.nonem)


# Fit the models for emerging markets
coefz<-matrix(NA,ncol=4,nrow=ncol(pre.dat))
predz<-matrix(NA,ncol=ncol(pre.dat),nrow=(nrow(post.dat)))
rsqz<-c(0)
for(i in 1:nrow(coefz)){
  mod<-lm(pre.dat[,i]~Lag(pre.dat[,i],1)+glo_cds_pre+em_fac_pre[,i])
  coefz[i,]<-coef(mod)
  predz[,i]<-coef(mod)[1]*rep(1,nrow(post.dat))+coef(mod)[2]*Lag(post.dat[,i],1)+coef(mod)[3]*glo_cds_post+coef(mod)[4]*em_fac_post[,i]
  rsqz[i]<-summary(mod)$r.squared
}
colnames(predz)<-colnames(post.dat)

pdat<-data.frame(em_cds2$Date[which(em_cds2$Date>"2019-06-30")], # date
                 rowMeans(post.dat), # EM_Avg
                 rowMeans(post.dat.nonem[,which(colnames(post.dat.nonem) %in%  non_em_countries$COUNTRY5yrCDS  )]), # Developed_Avg
                 rowMeans(post.dat[,which(colnames(post.dat) %in% Top5_Mortality_Countries$COUNTRY  )]), # EM_Avg_COVID
                 rowMeans(post.dat[,which(!colnames(post.dat) %in% Bottom5_Mortality_Countries$COUNTRY )]), # EM_Avg_nCOVID
                 rowMeans(predz), # EM_Pred
                 rowMeans(predz.nonem), # Developed_Pred
                 rowMeans(predz[,which(colnames(post.dat) %in% Top5_Mortality_Countries$COUNTRY   )]), # EM_Pred_COVID
                 rowMeans(predz[,which(!colnames(post.dat) %in% Bottom5_Mortality_Countries$COUNTRY  )]), # EM_Pred_nCOVID 
                 apply(apply(post.dat,2,cumsum),1,sd)) # EM_SD
colnames(pdat)<-c("date","EM_Avg", "Developed_Avg", "EM_Avg_COVID", "EM_Avg_nCOVID", "EM_Pred", "Developed_Pred", "EM_Pred_COVID","EM_Pred_nCOVID","EM_SD")

p<-ggplot(data=pdat[-1,],aes(x=date,y=cumsum(EM_Avg)))+geom_line()+geom_line(aes(y=cumsum(EM_Pred)),linetype=2)+theme_bw()+xlab("")+ylab("Cumulative Change (Log CDS)")+ggtitle("Emerging Markets Average CDS Spreads, Out-of-Sample Period")+geom_vline(xintercept = c(as.numeric(as.Date("2020-03-18")),as.numeric(as.Date("2020-06-04"))),alpha=.5)
p
p2<-ggplot(data=pdat[-1,],aes(x=date,y=EM_SD))+geom_line()+theme_bw()+xlab("")+ylab("Standard Deviation")+ggtitle("Emerging Markets CDS Spreads Dispersion")+geom_vline(xintercept = c(as.numeric(as.Date("2020-03-18")),as.numeric(as.Date("2020-06-04"))),alpha=.5)
p2
p4a<-ggplot(data=pdat[-1,],aes(x=date,y=cumsum(EM_Avg_COVID)))+geom_line()+geom_line(aes(y=cumsum(EM_Avg_nCOVID)),size=1)+theme_bw()+xlab("")+ylab("Cumulative Change (Log CDS)")+ggtitle("High Vs. Low COVID Mortalities: Actual")  + annotate(geom ="text", x = c(as.POSIXct("2020-01-25"),as.POSIXct("2020-01-25")  ), y = c(.4,.1), label = c("Low Mortality", "High Mortality") , fontface=c("bold","plain"))
p4a
p4b<-ggplot(data=pdat[-1,],aes(x=date,y=cumsum(EM_Avg_COVID-EM_Pred_COVID)))+geom_line()+geom_line(aes(y=cumsum(EM_Avg_nCOVID-EM_Pred_nCOVID)),size=1)+theme_bw()+xlab("")+ylab("Cumulative Residuals")+labs(title="High Vs. Low COVID Mortalities: Actual-Fitted")+geom_vline(xintercept=c(as.numeric(as.Date("2020-03-18")),as.numeric(as.Date("2020-06-04"))),alpha=.5)+annotate("text", x = c(as.POSIXct("2020-02-01"),as.POSIXct("2020-02-10")), y = c(.0,-.35), label = c("Low Mortality", "High Mortality") , fontface=c("bold","plain"))
p4b
grid.arrange(p,p2,p4a,p4b,nrow=2) 
# XYZ figure
# pdf("Plots/Weighted/pp2p4ap4b.pdf")
# grid.arrange(p,p2,p4a,p4b,nrow=2)
# dev.off()

p5a<-ggplot(data=pdat[-1,],aes(x=date,y=cumsum(EM_Avg)))+geom_line()+geom_line(aes(y=cumsum(Developed_Avg)),size=1)+theme_bw()+xlab("")+ylab("Cumulative Change (Log CDS)")+ggtitle("Emerging markets Vs. Developed countries: Actual")+annotate("text", x = c(as.POSIXct("2020-01-25"),as.POSIXct("2020-01-05")), y = c(.1,-.5), label = c("Developed", "EM") , fontface=c("bold","plain")) # geom_vline(xintercept=c(c(as.POSIXct("2020-03-18")),as.POSIXct(as.Date("2020-06-04"))),alpha=.5)+ylim(c(-.6,1))+
p5a

p5b<-ggplot(data=pdat[-1,],aes(x=date,y=cumsum(EM_Avg-EM_Pred)))+geom_line()+geom_line(aes(y=cumsum(Developed_Avg-Developed_Pred)),size=1)+theme_bw()+xlab("")+ylab("Cumulative Residuals")+labs(title="Emerging Markets Vs. Developed countries: Actual-Fitted")+annotate("text", x = c(as.POSIXct("2020-01-25"),as.POSIXct("2020-02-10")), y = c(.05,-.1), label = c("Developed", "EM") , fontface=c("bold","plain")) # geom_vline(xintercept=c(as.numeric(as.POSIXct("2020-03-18")),as.numeric(as.POSIXct("2020-06-04"))),alpha=.5)+
p5b
grid.arrange(p5a,p5b,nrow=1)
# XYZ figure
# jpeg("Plots/Weighted/p5ap5b.jpg", width = 1920, height = 1080)
# grid.arrange(p5a,p5b,nrow=1)
# dev.off()

write.csv(data.frame(pdat$date,post.dat),"Data/covid_ez_spreads_weighted.csv")
write.csv(data.frame(pdat$date,predz),"Data/covid_ez_prediction_weighted.csv")

corz<-c(0)
for(i in 1:ncol(post.dat)){
  corz[i]<-cor(post.dat[,i],predz[,i],use="complete.obs")
}

###plot individual spreads. 
pdat<-data.frame(em_cds2$Date[which(em_cds2$Date>"2019-06-30")],post.dat,predz)
colnames(pdat)[1]<-"date"
countriez<-colnames(pdat)[-1]
country_plots<-list()

for(i in 1:(length(countriez)/2)) {
  plotdat<-pdat[-1,c(1,i+1,i+31)]
  plotdat[,-1]<-apply(plotdat[,-1],2,cumsum)
  country_plots[[i]] <- ggplot(data=(plotdat), aes_string(x="date",y=colnames(plotdat)[2]))+geom_line()+theme_bw()+ylab("")+xlab(countriez[i])+geom_line(aes_string(x="date",y=colnames(plotdat)[3]),linetype=2)
}
do.call("grid.arrange", c(country_plots)) 
# jpeg("Plots/Weighted/individualpredvsactual.jpg", width = 1920, height = 1080)
# do.call("grid.arrange", c(country_plots)) 
# dev.off()
# =========================================================================.



# plots vs fundamentals ---------------------------------------------------
fstim<-read.csv("Data/CESI_7.csv",header=T,sep=',')
###keep EM
em_countries

fstim<-fstim[which(fstim$Country %in% c(em_countries$COUNTRY5yrCDS, non_em_countries$COUNTRY5yrCDS) ),]
nrow(fstim)

# fstim<-fstim[-which(fstim$Country %in% c("Malta","Luxembourg")),]
# colnames(fstim)[which(colnames(fstim)=="Slovak Republic")]<-"Slovak_Rep"
fstim<-fstim[,c("Country","fiscal_7")]
###quick plot

p.cds.coefs<-data.frame(coefz)
p.cds.coefs$countries<-colnames(post.dat)
p.cds.coefs.stim<-merge(p.cds.coefs,fstim,by.x="countries",by.y="Country")
p.cds.coefs.pd <-p.cds.coefs

pd <- read_excel("Data/PublicDebttoGDPyearly.xlsx", sheet = "DebttoGDP")

pd <- pd[, em_countries$COUNTRY5yrCDS]
pd2 <- pd
pd3<-colMeans(pd2[,])

# rates<-read.csv("Data/_rMinusg.csv",header=T,sep=',')
# rates$Date<-as.yearqtr((rates$Date))
# rmg<-rates[which(rates$Date >= "2014 Q4"),]
# rmg<-rmg[which(rmg$Date<="2018 Q4"),]
# rmg<-colMeans(rmg[,-1],na.rm=T)
# rmg<-rmg[-length(rmg)]
# names(rmg)<-c("Austria","Belgium","Cyprus","Finland","France","Germany","Greece","Ireland","Italy","Lithuania","Malta","Netherlands","Portugal","Slovak_Rep","Slovenia","Spain")
# rmg<-rmg[-which(names(rmg)=="Malta")]
# rmg<-rmg[p.cds.coefs.pd$countries]


###fundamentals plot 
pdat<-data.frame(pd3,p.cds.coefs.pd)
colnames(pdat)<-c("PD","Intercept","AR","Global","EM","countries")

p<-ggplot(data=pdat,aes(x=PD,y=Global))+geom_point(shape=21,fill="grey",size=3)+theme_bw()+geom_text(aes(label=countries),hjust="inward", vjust="inward")+xlab("Public Debt/GDP (%)")+ylab("Global Beta")+stat_cor(method = "pearson", label.x = 50, label.y = .5) + geom_smooth(method="lm",se=F,color="red",linetype=2,size=.5)
p
# jpeg("Plots/Figure6a.jpg", width = 1920, height = 1080)
# p
# dev.off()

p2<-ggplot(data=pdat,aes(x=PD,y=EM))+geom_point(shape=21,fill="grey",size=3)+theme_bw()+geom_text(aes(label=countries),hjust="inward", vjust="inward")+xlab("Public Debt/GDP (%)")+ylab("Regional Beta")+stat_cor(method = "pearson", label.x = 45, label.y = 1.9) + geom_smooth(method="lm",se=F,color="red",linetype=2,size=.5)
p2
# jpeg("Plots/Figure6b.jpg", width = 1920, height = 1080)
# p2
# dev.off()

grid.arrange(p,p2,nrow=1)## 1000x400
# pdf("Plots/Weighted/Figure6_weighted.pdf")
# grid.arrange(p,p2,nrow=1)
# dev.off()

pdat2<-data.frame(p.cds.coefs.stim)
colnames(pdat2)<-c("Country","Intercept","AR","Global","EM","Fiscal")

p4<-ggplot(data=pdat2,aes(x=Global,y=Fiscal))+geom_point(shape=21,size=3,fill="grey")+theme_bw()+geom_text(aes(label=Country),hjust="inward", vjust="inward")+ylab("COVID Stimulus/GDP (%)")+xlab("Global Beta")+stat_cor(method = "pearson", label.x = -.15, label.y = 20)+geom_smooth(method="lm",se=F,color="red",linetype=2,size=.5)
p4
p5<-ggplot(data=pdat2,aes(x=EM,y=Fiscal))+geom_point(shape=21,size=3,fill="grey")+theme_bw()+geom_text(aes(label=Country),hjust="inward", vjust="inward")+ylab("COVID Stimulus/GDP (%)")+xlab("Regional Beta")+stat_cor(method = "pearson", label.x = 1, label.y = 20)+geom_smooth(method="lm",se=F,color="red",linetype=2,size=.5)
p5
p7<-ggplot(data=pdat2,aes(x=Country,y=Fiscal))+geom_bar(stat="identity")+theme_bw()+theme(axis.text.x=element_text(angle = 60))+xlab("")+ylab("Fiscal Stimulus Announced (% of GDP)")
p7

grid.arrange(p7,p4,p5,nrow=1)## 1000x400
# pdf("Plots/Weighted/Figure7_weighted.pdf")
# grid.arrange(p7,p4,p5,nrow=1)## 1000x400
# dev.off()



pdat3<-data.frame(names(pd3),pd3,colSums(post.dat[1:197,],na.rm=T),colSums(post.dat[1:197,]-predz[1:197,],na.rm=T))
colnames(pdat3)<-c("country","pd","post","covidresid")
p8<-ggplot(data=pdat3,aes(x=pd,y=post))+geom_point(shape=21,fill="grey",size=3)+theme_bw()+stat_cor(method = "pearson", label.x =-.50, label.y = -.5)+geom_text(aes(label=country),hjust="inward",check_overlap = T)+xlab("Public Debt/GDP (%)")+ylab("Realized log CDS Change (2020)")
p8
p9<-ggplot(data=pdat3,aes(x=pd,y=covidresid))+geom_point(shape=21,fill="grey",size=3)+theme_bw()+stat_cor(method = "pearson", label.x =-.50, label.y = -.5)+geom_text(aes(label=country),hjust="inward",check_overlap = T)+xlab("Public Debt/GDP (%)")+ylab("Cumulative COVID Residual")
p9

fstim<-read.csv("Data/CESI_7.csv",header=T,sep=',')

fstim<-fstim[which(fstim$Country %in% c(em_countries$COUNTRY5yrCDS) ),]
fstim<-fstim[,c("Country","fiscal_7")]
nrow(fstim)

post.dat.without <- post.dat[, -30 ]
post.dat.without.redundant <- post.dat.without
ncol(post.dat.without)

post.dat.without <- colSums(post.dat.without[1:197,],na.rm=T)
post.dat.without <- as.data.frame(post.dat.without)
post.dat.without$country <- rownames(post.dat.without)
post.dat.without <- post.dat.without[order(post.dat.without[,2]) , ]
post.dat.without <- post.dat.without[,1]


postminuspredz <- colSums(post.dat.without.redundant[1:197,]-predz[1:197,-30],na.rm=T)
postminuspredz <- as.data.frame(postminuspredz)
postminuspredz$country <- rownames(postminuspredz)
postminuspredz <- postminuspredz[order(postminuspredz[,2]) , ]
postminuspredz <- postminuspredz[,1]


pdat4<-data.frame(fstim,post.dat.without,postminuspredz )
colSums(post.dat.without[1:197,],na.rm=T)  #error 
colnames(pdat4)<-c("country","fstim","post","covidresid")
p10<-ggplot(data=pdat4,aes(y=post,x=fstim))+geom_point(shape=21,fill="grey",size=3)+theme_bw()+stat_cor(method = "pearson", label.x =15, label.y = -.5)+geom_text(aes(label=country),hjust="inward",check_overlap = T)+ylab("Realized log CDS Change (2020)")+xlab("COVID Stimulus/GDP (%)")
p10
# jpeg("Plots/Figure7.jpg", width = 1920, height = 1080)
# grid.arrange(p7,p4,p5,nrow=1)## 1000x400
# dev.off()

p11<-ggplot(data=pdat4,aes(y=covidresid,x=fstim))+geom_point(shape=21,fill="grey",size=3)+theme_bw()+stat_cor(method = "pearson", label.x =15, label.y = -.5)+geom_text(aes(label=country),hjust="inward",check_overlap = T)+ylab("Cumulative COVID Residual")+xlab("COVID Stimulus/GDP (%)")
p11 # XYZ

grid.arrange(p10,p11,nrow=1)
# jpeg("Plots/stimulusvsresiduals.jpg", width = 1920, height = 1080)
# grid.arrange(p10,p11,nrow=1)
# dev.off()
# =========================================================================.



# link COVID residuals to COVID mortality ---------------------------------
# Weining Analysis - Linking COVID Residuals to COVID Mortality
# =========================================================================.



# load libraries ----------------------------------------------------------
library(lubridate)
library(zoo)
library(quantmod)
library(fBasics)
library(tseries)
library(sandwich)
library(lmtest)
library(lattice)
library(xtable)
library(vars)
library(tidyverse)
library(plyr)
library(reshape2)
library(gridExtra)
library(corrplot)
library(ggplot2)
library(reshape2)
library(data.table)
library(rvest)
library(plm)
library(stringr)
library(readxl)
library(stargazer)
# =========================================================================.



# import prediction and actual data ---------------------------------------
cds_5yr_prediction <- read.csv("data/covid_ez_prediction_weighted.csv", header = T, sep=',')
cds_5yr_actual <- read.csv("data/covid_ez_spreads_weighted.csv", header = T, sep = ',')

oxford <- Oxford_V1
oxford <- read_excel("Data/Oxford_V1.xlsx")
oxford <- oxford %>% dplyr::select(-contains("Flag"))
oxford <- oxford %>% dplyr::select(-contains("Lagged"))
colnames(oxford)[which(colnames(oxford) == "Total_Cases_Country")] <- "Total_Case"
colnames(oxford)[which(colnames(oxford) == "Total_Deceased_Country")] <- "Total_Death"
colnames(oxford)[which(colnames(oxford) == "New_Confirmed_Country")] <- "New_Case"
colnames(oxford)[which(colnames(oxford) == "New_Total_Deceased_Country")] <- "New_Death"
oxford$Total_Case_Rate <- oxford$Total_Case/oxford$Population*1000000 ### Total case rate per one million people
oxford$Total_Mortality_Rate <- oxford$Total_Death/oxford$Population*1000000 ### Total mortality rate per one million people
oxford$New_Case_Rate <- oxford$New_Case/oxford$Population*1000000 ### New case rate per one million people
oxford$New_Mortality_Rate <- oxford$New_Death/oxford$Population*1000000 ### New mortality rate per one million people
oxford$Total_Mortality_Rate_Per_Capita <- NULL
oxford$New_Mortality_Rate_Per_Capita <- NULL
oxford$Total_Cases_Country_Per_Capita <- NULL
oxford$rolling_average_confirmed <- NULL
oxford$rolling_average_deceased <- NULL
oxford$total_rolling_average_mortality <- NULL
oxford$new_rolling_average_mortality <- NULL

colnames(oxford)[1] <- "Country"

oxford <- pdata.frame(oxford, index = c("Country", "Date"))
oxford$Date <- as.Date(oxford$Date,"%Y-%m-%d")
#reorder
oxford <- oxford[order(oxford$Date),]
oxford <- oxford[order(oxford$Country),]

oxford <- oxford %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(Total_Mortality_Rate_Growth = (Total_Mortality_Rate - Lag(Total_Mortality_Rate,1))/Lag(Total_Mortality_Rate,1))

oxford <- oxford %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(New_Mortality_Rate_Growth = (New_Mortality_Rate - Lag(New_Mortality_Rate,1))/Lag(New_Mortality_Rate,1))

oxford <- oxford %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(Total_Case_Rate_Growth = (Total_Case_Rate - Lag(Total_Case_Rate,1))/Lag(Total_Case_Rate,1))

oxford <- oxford %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(New_Case_Rate_Growth = (New_Case_Rate - Lag(New_Case_Rate,1))/Lag(New_Case_Rate,1))

# xxx I have to add zeros here for the code to work 
oxford$StringencyIndex[is.na(oxford$StringencyIndex)] <- 0
oxford <- oxford %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(SI_Growth = (StringencyIndex - Lag(StringencyIndex,1))/Lag(StringencyIndex,1))

oxford$Country <- as.character(oxford$Country)

# xxx I have to add zeros here for the code to work 
oxford$driving[is.na(oxford$driving)] <- 0
oxford$SI_Growth[is.na(oxford$SI_Growth)] <- 0
oxford$SI_Growth[is.na(oxford$SI_Growth)] <- 0
oxford$SI_Growth[is.infinite(oxford$SI_Growth)] <- 0

# oxford$Country[which(oxford$Country == "Slovakia")] <- "Slovak_Rep"

cds_5yr_prediction_long <- reshape2::melt(cds_5yr_prediction, id.vars=c("X", "pdat.date"))
colnames(cds_5yr_prediction_long) <- c("X", "Date", "Country", "CDS_5y_Prediction")
cds_5yr_prediction_long$X <- NULL

cds_5yr_actual_long <- reshape2::melt(cds_5yr_actual, id.vars=c("X", "pdat.date"))
colnames(cds_5yr_actual_long) <- c("X", "Date", "Country", "CDS_5y_Actual")
cds_5yr_actual_long$X <- NULL

cds_5yr_merged <- merge(cds_5yr_actual_long, cds_5yr_prediction_long, by=c("Country", "Date"), all.x=TRUE)
cds_5yr_merged$CDS_5y_Residual <- cds_5yr_merged$CDS_5y_Actual - cds_5yr_merged$CDS_5y_Prediction

cds_5yr_merged$Date <- as.Date(cds_5yr_merged$Date,"%Y-%m-%d")
cds_5yr_merged <- cds_5yr_merged[which(cds_5yr_merged$Date < '2020-07-01'),]

cds_5y_actual_EM <- aggregate(CDS_5y_Actual ~ Date, cds_5yr_merged, mean)
cds_5y_prediction_EM <- aggregate(CDS_5y_Prediction ~ Date, cds_5yr_merged, mean)

eval <- c(0)

cds_5yr_EM <- merge(cds_5y_actual_EM, cds_5y_prediction_EM, by = c("Date"), all.x = TRUE)
cds_5yr_EM_preMar <- cds_5yr_EM[which(cds_5yr_EM$Date < '2020-03-01' & cds_5yr_EM$Date >= '2020-01-01'),]
cds_5yr_EM_preMar$residual2 <- (cds_5yr_EM_preMar$CDS_5y_Actual - cds_5yr_EM_preMar$CDS_5y_Predictio)^2  
eval[1] <- mean(cds_5yr_EM_preMar$residual2, na.rm = TRUE)

cds_5yr_EM_postMar <- cds_5yr_EM[which(cds_5yr_EM$Date >= '2020-04-01' & cds_5yr_EM <= '2020-06-30'),]
cds_5yr_EM_postMar$residual2 <- (cds_5yr_EM_postMar$CDS_5y_Actual - cds_5yr_EM_postMar$CDS_5y_Predictio)^2  
eval[2] <- mean(cds_5yr_EM_postMar$residual2, na.rm = TRUE)

cds_5yr_EM_Mar <- cds_5yr_EM[which(cds_5yr_EM$Date >= '2020-03-01' & cds_5yr_EM$Date < '2020-04-01'),]
cds_5yr_EM_Mar$residual2 <- (cds_5yr_EM_Mar$CDS_5y_Actual - cds_5yr_EM_Mar$CDS_5y_Predictio)^2  
eval[3] <- mean(cds_5yr_EM_Mar$residual2, na.rm = TRUE)

p_EM_preMar <- ggplot(dat = cds_5yr_EM_preMar,aes(x=Date,y=CDS_5y_Actual,linetype="Actual"))+geom_line()+geom_line(aes(y=CDS_5y_Prediction,linetype="Fitted"))+theme_bw()+xlab("")+ylab("Daily Change (Log CDS)")+ggtitle("Emerging Markets Average CDS Spreads, Mid Dec 2019 - Feb 2020")+theme(axis.title.y = element_text(size = 10),axis.text = element_text(size = 10),legend.title = element_blank(),plot.title = element_text(size=12),legend.position = c(0.8, 0.8))+ylim(-0.15,0.2)
p_EM_preMar

p_EM_postMar <- ggplot(dat = cds_5yr_EM_postMar,aes(x=Date,y=CDS_5y_Actual,linetype="Actual"))+geom_line()+geom_line(aes(y=CDS_5y_Prediction,linetype="Fitted"))+theme_bw()+xlab("")+ylab("Daily Change (Log CDS)")+ggtitle("Emerging Markets Average CDS Spreads, Apr 2020 - June 2020")+theme(axis.title.y = element_text(size = 10),axis.text = element_text(size = 10),legend.title = element_blank(),plot.title = element_text(size=12),legend.position = c(0.8, 0.8))+ylim(-0.15,0.2)
p_EM_postMar

p_EM_Mar <- ggplot(dat = cds_5yr_EM_Mar,aes(x=Date,y=CDS_5y_Actual,linetype="Actual"))+geom_line()+geom_line(aes(y=CDS_5y_Prediction,linetype="Fitted"))+theme_bw()+xlab("")+ylab("Daily Change (Log CDS)")+ggtitle("Emerging Markets Average CDS Spreads, March 2020")+theme(axis.title.y = element_text(size = 10),axis.text = element_text(size = 10),legend.title = element_blank(),plot.title = element_text(size=12),legend.position = c(0.8, 0.8))+ylim(-0.15,0.2)
p_EM_Mar

p_EM <- grid.arrange(p_EM_preMar, p_EM_Mar, p_EM_postMar, ncol=3,nrow=1)
# XYZ figure
# pdf("Plots/Weighted/figure10weighted.pdf", width = 11.69, height = 8.27 )
# plot(p_EM)
# dev.off()

# =========================================================================.



# create panel ------------------------------------------------------------
# panel <- read_excel("Data/panel.xlsx")

panel <- merge(cds_5yr_merged, oxford, by = c("Country", "Date"), all.x = TRUE)
paneladdition <- read_excel("Data/paneladdition.xlsx") # adding variables such as the oil, IMF suppot dummy, remittances, RFI, etc.

panel <- merge(panel, paneladdition, by = c("Country", "Date"), all.x = TRUE )
View(panel)

panel$Dummy_Fiscal_Country <- as.numeric(panel$Fiscal_Response_Dummy > 0)
# panel$Dummy_Fiscal_EU <- as.numeric(panel$EU_Fiscal_Response_Dummy > 0)
panel$Dummy_Monetary_ECB <- as.numeric(panel$ECB_Announcement > 0)
panel$Dummy_Monetary_Fed <- as.numeric(panel$Fed_Announcement > 0)

panel <- panel[which(panel$Date > '2019-07-01'),]
panel <- panel %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(Cum_CDS_5y_Residual = cumsum(CDS_5y_Residual))

panel_inCOVID <- panel[which(panel$Date >= "2020-03-01" & panel$Date < "2020-04-01"),]

panel_inCOVID <- panel_inCOVID %>% ungroup()

panel_inCOVID <- pdata.frame(panel_inCOVID, index = c("Country", "Date"))

panel_inCOVID$Date <- as.Date(panel_inCOVID$Date,"%Y-%m-%d")

#reorder
panel_inCOVID <- panel_inCOVID[order(panel_inCOVID$Date),]
panel_inCOVID <- panel_inCOVID[order(panel_inCOVID$Country),]
# =========================================================================.



# panel analysis of COVID residuals with additional controls --------------
### Panel analysis: COVID residuals
new_res.mortality.2 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")

se.new_res.mortality.2 <- coeftest(new_res.mortality.2, vcov = vcovHC(new_res.mortality.2, type = "HC1"))

new_res.mortality.3 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth,
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")

se.new_res.mortality.3 <- coeftest(new_res.mortality.3, vcov = vcovHC(new_res.mortality.3, type = "HC1"))

new_res.mortality.4 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth + Lag(Dummy_Fiscal_Country) + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_res.mortality.4 <- coeftest(new_res.mortality.4, vcov = vcovHC(new_res.mortality.4, type = "HC1"))

new_res.mortality.5 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth  + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed) + Lag(China_debt_stock_GDP) + Lag(Dummy_Fiscal_Country_weighted_extdebt) + Lag(RFI_GDP) + Lag(Oil_effect),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_res.mortality.5 <- coeftest(new_res.mortality.5, vcov = vcovHC(new_res.mortality.5, type = "HC1"))

stargazer(digits=4,new_res.mortality.2,new_res.mortality.3,new_res.mortality.4, new_res.mortality.5,
          type="latex",se=list(se.new_res.mortality.2[,2],se.new_res.mortality.3[,2],se.new_res.mortality.4[,2], se.new_res.mortality.5[,2]), out=file.path("Table_inCOVID_panel_output_newRES_mortality.htm"),
          dep.var.labels=c("COVID Residual"), 
          covariate.labels=c("New Mortality Rate", "New Mortality Rate Growth", 
                             "Total Mortality Rate", "Total Mortality Rate Growth",
                             "Mobility", "SI Growth",
                             "Country Fiscal Policy Dummy", "ECB Policy Dummy", "Fed Policy Dummy", 
                             "China debt stock", "Fiscal dummy X ext. debtGDP", "Oil effect"),
          df = FALSE, omit.stat="adj.rsq", 
          notes = c("*,**,*** correspond to 10%, 5% and 1% significance, respectively.","HAC robust standard errors, clustered by country. Time and Country FEs."),
          notes.append=F, notes.align ="l",
          title="COVID-Sample Panel Analysis",add.lines = list(c("Fixed effects?", "Y","Y","Y","Y")))
# =========================================================================.



# panel analysis of COVID spreads changes in march 2020 with additional controls--------
### Panel analysis: CDS spreads changes in 2020 March
new_CDS.mortality.0 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction,
                           method="pooling", effect="twoways",
                           data=panel_inCOVID, na.action="na.exclude")
se.new_CDS.mortality.0 <- coeftest(new_CDS.mortality.0, vcov = vcovHC(new_CDS.mortality.0, type = "HC1"))

new_CDS.mortality.2 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.2 <- coeftest(new_CDS.mortality.2, vcov = vcovHC(new_CDS.mortality.2, type = "HC1"))

new_CDS.mortality.3 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth,
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.3 <- coeftest(new_CDS.mortality.3, vcov = vcovHC(new_CDS.mortality.3, type = "HC1"))

new_CDS.mortality.4 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth + Lag(Dummy_Fiscal_Country) + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.4 <- coeftest(new_CDS.mortality.4, vcov = vcovHC(new_CDS.mortality.4, type = "HC1"))

new_CDS.mortality.5 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth  + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed) + Lag(China_debt_stock_GDP) + Lag(Dummy_Fiscal_Country_weighted_extdebt) + Lag(RFI_GDP) + Lag(Oil_effect),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.5 <- coeftest(new_CDS.mortality.5, vcov = vcovHC(new_CDS.mortality.5, type = "HC1"))


stargazer(digits=4,new_CDS.mortality.0,new_CDS.mortality.2,new_CDS.mortality.3,new_CDS.mortality.4, new_CDS.mortality.5,
          type="latex",se=list(se.new_CDS.mortality.0[,2],se.new_CDS.mortality.2[,2],se.new_CDS.mortality.3[,2],se.new_CDS.mortality.4[,2], se.new_CDS.mortality.5[,2]),out=file.path("Table_inCOVID_panel_output_newCDS_mortality.htm"),
          dep.var.labels=c("Daily CDS Spread Change"),
          covariate.labels=c("Fitted Daily CDS Spread Change", "New Mortality Rate", "New Mortality Rate Growth", 
                             "Total Mortality Rate", "Total Mortality Rate Growth",
                             "Mobility", "SI Growth",
                             "Country Fiscal Policy Dummy", "ECB Policy Dummy", "Fed Policy Dummy", "China debt stock", "Fiscal dummy X ext. debtGDP", "Oil effect"),
          df = FALSE, omit.stat="adj.rsq", 
          notes = c("*,**,*** correspond to 10%, 5% and 1% significance, respectively.","HAC robust standard errors, clustered by country. Time and Country FEs."),
          notes.append=F, notes.align ="l",
          title="COVID-Sample Panel Analysis",add.lines = list(c("Fixed effects?","Y","Y","Y","Y","Y")))
# =========================================================================.







































































# panel analysis of COVID residuals ---------------------------------------
### Panel analysis: COVID residuals
new_res.mortality.2 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")

se.new_res.mortality.2 <- coeftest(new_res.mortality.2, vcov = vcovHC(new_res.mortality.2, type = "HC1"))

new_res.mortality.3 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth,
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")

se.new_res.mortality.3 <- coeftest(new_res.mortality.3, vcov = vcovHC(new_res.mortality.3, type = "HC1"))

new_res.mortality.4 <- plm(CDS_5y_Residual ~ Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth + Lag(Dummy_Fiscal_Country) + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_res.mortality.4 <- coeftest(new_res.mortality.4, vcov = vcovHC(new_res.mortality.4, type = "HC1"))

stargazer(digits=4,new_res.mortality.2,new_res.mortality.3,new_res.mortality.4,
          type="latex",se=list(se.new_res.mortality.2[,2],se.new_res.mortality.3[,2],se.new_res.mortality.4[,2]), out=file.path("Table_inCOVID_panel_output_newRES_mortality.htm"),
          dep.var.labels=c("COVID Residual"),
          covariate.labels=c("New Mortality Rate", "New Mortality Rate Growth", 
                             "Total Mortality Rate", "Total Mortality Rate Growth",
                             "Driving Mobility", "SI Growth",
                             "Country Fiscal Policy Dummy", "ECB Policy Dummy", "Fed Policy Dummy"), df = FALSE, omit.stat="adj.rsq", 
          notes = c("*,**,*** correspond to 10%, 5% and 1% significance, respectively.","HAC robust standard errors, clustered by country. Time and Country FEs."),
          notes.append=F, notes.align ="l",
          title="COVID-Sample Panel Analysis",add.lines = list(c("Fixed effects?", "Y","Y","Y","Y")))
# =========================================================================.



# panel analysis of COVID spreads changes in march 2020 -------------------
### Panel analysis: CDS spreads changes in 2020 March

new_CDS.mortality.0 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction,
                           method="pooling", effect="twoways",
                           data=panel_inCOVID, na.action="na.exclude")
se.new_CDS.mortality.0 <- coeftest(new_CDS.mortality.0, vcov = vcovHC(new_CDS.mortality.0, type = "HC1"))

new_CDS.mortality.2 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.2 <- coeftest(new_CDS.mortality.2, vcov = vcovHC(new_CDS.mortality.2, type = "HC1"))

new_CDS.mortality.3 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth,
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.3 <- coeftest(new_CDS.mortality.3, vcov = vcovHC(new_CDS.mortality.3, type = "HC1"))

new_CDS.mortality.4 <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth + Lag(Dummy_Fiscal_Country) + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed),
                           method="pooling", effect="twoways",
                           data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
se.new_CDS.mortality.4 <- coeftest(new_CDS.mortality.4, vcov = vcovHC(new_CDS.mortality.4, type = "HC1"))


stargazer(digits=4,new_CDS.mortality.0,new_CDS.mortality.2,new_CDS.mortality.3,new_CDS.mortality.4,
          type="latex",se=list(se.new_CDS.mortality.0[,2],se.new_CDS.mortality.2[,2],se.new_CDS.mortality.3[,2],se.new_CDS.mortality.4[,2]),out=file.path("Table_inCOVID_panel_output_newCDS_mortality.htm"),
          dep.var.labels=c("Daily CDS Spread Change"),
          covariate.labels=c("Fitted Daily CDS Spread Change", "New Mortality Rate", "New Mortality Rate Growth", 
                             "Total Mortality Rate", "Total Mortality Rate Growth",
                             "Mobility", "SI Growth",
                             "Country Fiscal Policy Dummy", "ECB Policy Dummy", "Fed Policy Dummy"), df = FALSE, omit.stat="adj.rsq", 
          notes = c("*,**,*** correspond to 10%, 5% and 1% significance, respectively.","HAC robust standard errors, clustered by country. Time and Country FEs."),
          notes.append=F, notes.align ="l",
          title="COVID-Sample Panel Analysis",add.lines = list(c("Fixed effects?","Y","Y","Y","Y","Y")))
# =========================================================================.



# 
# # panel analysis of subsample with mortality data -------------------------
# ### Panel analayis: subsample with observations for which mortality data is available
# new_CDS.mortality.2.small <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth),
#                                  method="pooling", effect="twoways",
#                                  data=panel_inCOVID[which(!is.infinite(-panel_inCOVID$New_Mortality_Rate_Growth) & !is.infinite(-panel_inCOVID$Total_Mortality_Rate_Growth)),], na.action="na.exclude")
# se.new_CDS.mortality.2.small <- coeftest(new_CDS.mortality.2.small, vcov = vcovHC(new_CDS.mortality.2.small, type = "HC1"))
# 
# panel_inCOVID_small <- cbind(as.vector(new_CDS.mortality.2$model[[1]]), attr(new_CDS.mortality.2$model[[1]], "index"))
# panel_inCOVID_small$Date <- as.Date(panel_inCOVID_small$Date,"%Y-%m-%d")
# panel_inCOVID_small <- merge(panel_inCOVID_small, panel_inCOVID, by = c("Country", "Date"), all.x = TRUE)
# 
# new_CDS.mortality.0.small <- plm(CDS_5y_Actual ~ CDS_5y_Prediction,
#                                  method="pooling", effect="twoways",
#                                  data=panel_inCOVID_small, na.action="na.exclude")
# se.new_CDS.mortality.0.small <- coeftest(new_CDS.mortality.0.small, vcov = vcovHC(new_CDS.mortality.0.small, type = "HC1"))
# 
# new_CDS.mortality.3.small <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth,
#                                  method="pooling", effect="twoways",
#                                  data=panel_inCOVID_small, na.action="na.exclude")
# se.new_CDS.mortality.3.small <- coeftest(new_CDS.mortality.3.small, vcov = vcovHC(new_CDS.mortality.3.small, type = "HC1"))
# 
# new_CDS.mortality.4.small <- plm(CDS_5y_Actual ~ CDS_5y_Prediction + Lag(New_Mortality_Rate) + Lag(New_Mortality_Rate_Growth) + Lag(Total_Mortality_Rate) + Lag(Total_Mortality_Rate_Growth) + driving + SI_Growth + Lag(Dummy_Fiscal_Country) + Lag(Dummy_Monetary_ECB) + Lag(Dummy_Monetary_Fed),
#                                  method="pooling", effect="twoways",
#                                  data=panel_inCOVID_small, na.action="na.exclude")
# se.new_CDS.mortality.4.small <- coeftest(new_CDS.mortality.4.small, vcov = vcovHC(new_CDS.mortality.4.small, type = "HC1"))
# 
# stargazer(digits=4,new_CDS.mortality.0.small,new_CDS.mortality.2.small,new_CDS.mortality.3.small,new_CDS.mortality.4.small,
#           type="latex",se=list(se.new_CDS.mortality.0.small[,2],se.new_CDS.mortality.2.small[,2],se.new_CDS.mortality.3.small[,2],se.new_CDS.mortality.4.small[,2]),out=file.path("Table_inCOVID_panel_output_newCDS_mortality_small.htm"),
#           dep.var.labels=c("Daily CDS Spread Change"),
#           covariate.labels=c("Fitted Daily CDS Spread Change", "New Mortality Rate", "New Mortality Rate Growth", 
#                              "Total Mortality Rate", "Total Mortality Rate Growth",
#                              "Mobility", "SI Growth",
#                              "Country Fiscal Policy Dummy", "ECB Policy Dummy", "Fed Policy Dummy"), df = FALSE, omit.stat="adj.rsq", 
#           notes = c("*,**,*** correspond to 10%, 5% and 1% significance, respectively.","HAC robust standard errors, clustered by country. Time and Country FEs."),
#           notes.append=F, notes.align ="l",
#           title="COVID-Sample Panel Analysis",add.lines = list(c("Fixed effects?","Y","Y","Y","Y","Y")))
# # =========================================================================.




# EM aggregate: realized values vs model-implied values -------------------
# EM aggregate: realized values versus model-implied values
panel_inCOVID_CDS_5y_Prediction_New <- cbind(as.vector(new_CDS.mortality.4$model[[1]]-new_CDS.mortality.4$residuals), attr(new_CDS.mortality.4$model[[1]], "index"))
colnames(panel_inCOVID_CDS_5y_Prediction_New)[1] <- c("CDS_5y_Prediction_New")

panel_inCOVID_CDS_5y_Prediction_New$Date <- as.Date(panel_inCOVID_CDS_5y_Prediction_New$Date,"%Y-%m-%d")
panel_inCOVID_small <- merge(panel_inCOVID_CDS_5y_Prediction_New, panel_inCOVID, by = c("Country", "Date"), all.x = TRUE)

panel_inCOVID_EM_prediction <- aggregate(CDS_5y_Prediction ~ Date, panel_inCOVID_small, mean)
panel_inCOVID_EM_actual <- aggregate(CDS_5y_Actual ~ Date, panel_inCOVID_small, mean)
panel_inCOVID_EM_prediction_new <- aggregate(CDS_5y_Prediction_New ~ Date, panel_inCOVID_small, mean)

panel_inCOVID_EM <- merge(panel_inCOVID_EM_prediction_new, panel_inCOVID_EM_prediction, by = c("Date"), all.x = TRUE)
panel_inCOVID_EM <- merge(panel_inCOVID_EM, panel_inCOVID_EM_actual, by = c("Date"), all.x = TRUE)

p_EM_Mar_New <- ggplot(dat = panel_inCOVID_EM,aes(x=Date,y=CDS_5y_Actual,linetype = "Actual"))+geom_line()+geom_line(aes(y=CDS_5y_Prediction, linetype="Fitted by model [1]"))+
  geom_line()+geom_line(aes(y=CDS_5y_Prediction_New,linetype="Fitted by model [4]"))+theme_bw()+xlab("")+ylab("Daily Change (Log CDS)")+
  ggtitle("Emerging markets Average CDS Spreads, March 2020")+
  theme(axis.title.y = element_text(size = 12), axis.text = element_text(size = 10), legend.title = element_blank(),legend.position = c(0.8, 0.8))

p_EM_Mar_New
# jpeg("Plots/Figure11.jpg", width = 1920, height = 1080)
# p_EM_Mar_New
# dev.off()
# =========================================================================.










