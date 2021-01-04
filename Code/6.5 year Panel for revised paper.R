# This code assembles several variables to generate a 6.5 year panel from January 2014-June 2020. 
# This panel is then used to run regressions. This code became necessary for the revised version of the paper. 

#I don't want scientific notation for my values, so I specify this below. 
options(scipen = 999)
# Load libraries
library(readxl)
library(plyr)

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



# Now I can add the VIX data to the panel
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


# Joining the VIX changess (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, VIX, by = c("Country" = "Country", "Date" = "Date")  )
head(panel_for_revised_paper)
view(panel_for_revised_paper)
view(panel_for_revised_paper[panel_for_revised_paper$Country=="Uruguay",])


# Now I can add the Brent data to the panel
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
# head(BRENT)
# str(BRENT)
# view(BRENT)

# Joining the BRENT changes (explanatory variable) to the dependent variable
panel_for_revised_paper <- left_join(panel_for_revised_paper, BRENT, by = c("Country" = "Country", "Date" = "Date")  )
# safetcopyofeverythingsofar <- panel_for_revised_paper
head(panel_for_revised_paper)

###### I don't think this is necessary
Oxford_V1 <- read_excel("Data/Oxford_V1.xlsx")
# safetyofthis <- Oxford_V1
# Oxford_V1 <- safetyofthis
Oxford_addition <- read_excel("Data/Oxford_V1_dummyts_additions.xlsx")
# safetyofthat <- Oxford_addition
# Oxford_addition <- safetyofthat
Oxford_V1 <- as_tibble(merge(Oxford_V1, Oxford_addition, by=c("COUNTRY", "Date")))
Oxford_V1 <- as.data.frame(Oxford_V1)
# safetyoftheother <- Oxford_V1
# Oxford_V1 <- safetyoftheother
# View(Oxford_V1)
# I don't think this is necessary up to here. 


# Now, I calculate deaths per million. This is just to complete teh Oxford_V1 dataset
library(data.table)
library(dplyr)
library(writexl)
library(readxl)

Oxford_V1 <- read_excel("Data/Oxford_V1_augmented_4.1.2021.xlsx") # I think I can just use this one

# # add population data in million ------------------------------------------
# # To calculate deaths per million, news sources divide the number of cumulative (total) deaths by the population size, measured in millions. Luxembourg and Malta have less than a million inhabitants, and thus I do not calculate deaths per million for these nations. Data was pulled from: https://www.statista.com/statistics/1104709/coronavirus-deaths-worldwide-per-million-inhabitants/.
# Mill_Pop <- read_excel("Data/Mill_Pop.xlsx")
# Oxford_V1 <- merge(Oxford_V1, Mill_Pop, by = c("COUNTRY"), all=TRUE)
# Oxford_V1 <- Oxford_V1[order(Oxford_V1$COUNTRY, Oxford_V1$Date),]
# Oxford_V1$New_Deaths_Per_Million = (Oxford_V1$New_Total_Deceased_Country / Oxford_V1$PopMil)
# Oxford_V1 <- Oxford_V1[order(Oxford_V1$COUNTRY, Oxford_V1$Date),]
# Oxford_V1$Total_Deaths_Per_Million = (Oxford_V1$Total_Deceased_Country / Oxford_V1$PopMil)
# # safetyofyetanother <- Oxford_V1
# # Oxford_V1 <- safetyofyetanother
# 162 variables

str(Oxford_V1)
# 78 variables








Oxford_V1 <- read_excel("Data/Oxford_V1.xlsx")
oxford <- Oxford_V1

view(oxford[oxford$Date==as.Date("2020-07-11"),])


asdfasdf <- left_join(neededforJoins, oxford, by = c("Country" = "Country", "Date" = "Date")  )


asdfasdf$Total_Case[asdfasdf$Date==as.Date("2013-12-30")] <- "xoxoxo"
asdfasdf$Total_Case[asdfasdf$Date==as.Date("2013-12-31")] <- "0"
asdfasdf <- asdfasdf %>% fill(Total_Case)
head(asdfasdf)
view(asdfasdf)
view(asdfasdf[asdfasdf$Country == "China", ])
tail(asdfasdf)


install.packages("visdat")
library(visdat)
vis_dat(asdfasdf, warn_large_data = F)

oxford

view(oxford[oxford$Country == "China", ])





























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


