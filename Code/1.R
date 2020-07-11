# Importing time series data for all countries to run the first and second stage regression

 
# Important information about code ----
# Author: Timo Daehler, daehler@usc.edu
# Date of last update: 9 July 2020
# Inputs: -
# Outputs: -
# Other relevant notes: -








# packages ----
library(data.table)
library(quandl)
library(readxl)



# Defining countries ------------------------------------------------------
# Import Excel sheet
countriesDF <- read_excel("Data/Country_Names.xlsx")
countryNames <- countriesDF$Name
countryISO2  <- countriesDF$ISO2
countryISO3  <- countriesDF$ISO3






# Load data ----

# *********************************************************************************************
# COVID data 
# The links from which the data is downloaded is dynamically updated every day. 
# The data is from https://data.humdata.org/dataset/coronavirus-covid-19-cases-and-deaths (and not https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases which is an alternative)

# Downloading dataset from the web as a dataframe
covidDF <- read.csv(url("https://docs.google.com/spreadsheets/d/e/2PACX-1vSe-8lf6l_ShJHvd126J-jGti992SUbNLu-kmJfx1IRkvma_r4DHi0bwEW89opArs8ZkSY5G2-Bc1yT/pub?gid=0&single=true&output=csv"))

# Changing format to data.table
covidDT <- as.data.table(covidDF)

# *********************************************************************************************
# *********************************************************************************************


# *********************************************************************************************
# Traffic data
# The links from which the data is downloaded is dynamically updated every day. 
# The data is from https://www.apple.com/covid19/mobility

# Downloading dataset from the web as a dataframe
mobilityDF <- read.csv(url("https://covid19-static.cdn-apple.com/covid19-mobility-data/2011HotfixDev17/v3/en-us/applemobilitytrends-2020-07-07.csv"))

# Changing format to data.table
mobilityDT <- as.data.table(mobilityDF)

"https://covid19-static.cdn-apple.com/covid19-mobility-data/2011HotfixDev17/v3/en-us/applemobilitytrends-2020-07-07.csv"




# Inspect data ----
View(covidDF)







# Format data for analysis ----









# Data manipulation ----








# Modeling first stage ----








# Visualizing model outputs ----











