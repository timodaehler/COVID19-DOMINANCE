# README ====
# Author: Timo Daehler, daehler@usc.edu
# Date of last update: 20 July 2020
# Inputs: -
# Outputs: -
# Other relevant notes: At the end of coding script "1.R" I didn't need the chunks of code below anymore. I keep them in this file just in case. 
# ==================================================.




# Defining countries in sample ----
# potentialSamplesDF <- read_excel("Data/SAMPLE_LISTS.xlsx", sheet = "SAMPLE_LISTS_R")
# 
# # Choose one of the following sample possibilities and assign it to 
# # "EMBI_ISO_NAME"	
# # "JPM_LEMB_ISO_NAME"	
# # "EMBI2_ISO_NAME"
# # "JPM_EMBI_GD_ISO_NAME"
# # "IMF_ISO_NAME"
# # "1yrCDS_ISO_NAME"
# # "5yrCDS_ISO_NAME"
# # "1yrYield_ISO_NAME"
# # "5yrYield_ISO_NAME"
# 
# sampleColumn <- "JPM_LEMB_ISO_NAME"
# 
# # Subsetting the data frame to the sample vector 
# sampleVector <- potentialSamplesDF[, sampleColumn] 
# # Removing NAs from this vector so that the countries do not get recycled
# sampleCountries <- sampleVector[!is.na(sampleVector)]















# Plot individual CDS spreads ----
# Importing the 5 year CDS data
cds_fiveDF <- read_excel("Data/EIKON_Download.xlsx", sheet = "5yCDS_R")

# Subsetting for the sample countries specified above
cds_fiveDF <- cds_fiveDF[, c("Date", sampleCountries) ]

# Making sure "Date" column has correct date format
cds_fiveDF$Date<-as.Date(cds_fiveDF$Date, "%m/%d/%Y")

# Definint start and end date of pre period
start_date <- "2014-01-01"
end_date <- "2020-06-30"

# Subsetting the right period: training period 
cds_fiveDF <-cds_fiveDF[which(cds_fiveDF$Date >= start_date & cds_fiveDF$Date <= end_date,  ),]

#plot individual spreads
pdat <- melt(cds_fiveDF,id.vars="Date")
countriez <- unique(pdat$variable)
country_plots<-list()

for(i in 1:length(countriez)) {
  country_plots[[i]] <- ggplot(data=subset(pdat,variable==countriez[i]), aes_string(x="Date",y="value"))+geom_line(size=1)+theme_bw()+ylab("")+xlab(countriez[i])
}

do.call("grid.arrange", c(country_plots))






# Plot individual yields  ----
# Importing the 5 year CDS data
yield_fiveDF <- read_excel("Data/EIKON_Download.xlsx", sheet = "5yYield_R")

# It appears that Dom. Rep. and Uruguay need to be removed from Sample
x <- c("Dominican Republic (the)", "Uruguay" )
sampleCountries <- sampleCountries[!sampleCountries %in% x] 

# Subsetting for the sample countries specified above
yield_fiveDF <- yield_fiveDF[, c("Date", sampleCountries) ]

# Making sure "Date" column has correct date format
yield_fiveDF$Date<-as.Date(yield_fiveDF$Date, "%m/%d/%Y")

# Definint start and end date of pre period
start_date <- "2014-01-01"
end_date <- "2020-06-30"

# Subsetting the right period: training period 
yield_fiveDF <- yield_fiveDF[which(yield_fiveDF$Date >= start_date & yield_fiveDF$Date <= end_date,  ),]

#plot individual spreads
pdat <- melt(yield_fiveDF,id.vars="Date")
countriez <- unique(pdat$variable)
country_plots<-list()

for(i in 1:length(countriez)) {
  country_plots[[i]] <- ggplot(data=subset(pdat,variable==countriez[i]), aes_string(x="Date",y="value"))+geom_line(size=1)+theme_bw()+ylab("")+xlab(countriez[i])
}

do.call("grid.arrange", c(country_plots))



###Prepare data for regression: CDS ----
em_cds2 <- cds_fiveDF
nonem_cds <- read_excel("Data/EIKON_Download.xlsx", sheet = "5yCDS_R")
nonem_cds <- nonem_cds[ ,c("Date", "United States of America (the)", "Japan", "Germany") ]

# minimums <- apply(em_cds2,2,min)

d_emcds<-apply(log(em_cds2[,-1]),2,diff) #log differences of EM spreads
d_nemcds<-apply(log(nonem_cds[,-1]),2,diff) # log differences of global spreads

glo_cds<-rowMeans(d_nemcds) # create a global CDS factor excluding EM

em_fac<-matrix(NA,nrow=nrow(d_emcds),ncol=ncol(d_emcds)) # create an EM common factor excluding country i

for(i in 1:ncol(em_fac)){
  em_fac[,i]<-rowMeans(d_emcds[,-i])
}


###training - test sample split
pre.dat<-d_emcds[which(em_cds2$Date[-1]<"2019-06-30"),]
post.dat<-d_emcds[which(em_cds2$Date[-1]>="2019-06-30"),]

glo_cds_pre<-glo_cds[which(em_cds2$Date[-1]<"2019-06-30")]
glo_cds_post<-glo_cds[which(em_cds2$Date[-1]>="2019-06-30")]

em_fac_pre<-em_fac[which(em_cds2$Date[-1]<"2019-06-30"),]
em_fac_post<-em_fac[which(em_cds2$Date[-1]>="2019-06-30"),]




### Fit the models

coefz<-matrix(NA,ncol=4,nrow=ncol(pre.dat))
predz<-matrix(NA,ncol=ncol(pre.dat),nrow=(nrow(post.dat)))
rsqz<-c(0)
for(i in 1:nrow(coefz)){
  mod<-lm(pre.dat[,i]~lag(pre.dat[,i],1)+glo_cds_pre+em_fac_pre[,i])
  coefz[i,]<-coef(mod)
  predz[,i]<-coef(mod)[1]*rep(1,nrow(post.dat))+coef(mod)[2]*Lag(post.dat[,i],1)+coef(mod)[3]*glo_cds_post+coef(mod)[4]*em_fac_post[,i]
  rsqz[i]<-summary(mod)$r.squared
}

colnames(predz)<-colnames(post.dat)










# Load data ----
























# Yield data 
# The data is from Eikon 


# Importing
Yield1YearDF <- read_excel("Data/TR_Download_11.7.2020.xlsx", sheet = "1yYield_R") 

# Changing format to data.table
Yield1YearDT <- as.data.table(Yield1YearDF)

# Creating an XTS object
Yield1YearTS <- xts(x = Yield1YearDT[, -(1:2)], order.by = as.POSIXct(Yield1YearDT$Date)  )


temp <- data.frame(index(Yield1YearDT), stack(as.data.frame(coredata(Yield1YearDT[, -1]) ) )  )



ggplot(temp, aes (x = "Date", y = ) ) + geom_line()

summary(Yield1YearTS)





plot(Yield1YearTS)


# *********************************************************************************************
# *********************************************************************************************

tsobject <- as.xts(Yield1YearDT)

str(Yield1YearDT)

selectionDT <- Yield1YearDT[, 1:2]
ts.plot()
ts <- zoo(selectionDT)

time_series <- ts(selectionDT)


time_series <- xts(x = Yield1YearDT[, -1], order.by = Yield1YearDT[,1]  )


plot(time_series)



newobject <- as.xts(read.table("Data/TR_Download_11.7.2020.xlsx", sheet = "1yYield_R") )

newobject <- read.zoo("Data/TR_Download_11.7.2020.xlsx", sheet = "1yYield_R")






