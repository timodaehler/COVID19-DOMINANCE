


options(scipen = 999)


# Data import -------------------------------------------------------------

# Import data on international reserves and GDP of emerging markets
InternationalReserves <- read_excel("Data/InternationalReserves.xlsx", sheet = "ReserveRatio")

# Create the reserve to GDP ratios
InternationalReserves <- InternationalReserves %>%
mutate(IR_GDP_ratio_Start_2019 = IR_12_2018 / GDP2018, IR_GDP_ratio_February_2020 = IR_2_2020 / GDP2019, IR_GDP_ratio_March_2020 = IR_3_2020 / GDP2019, IR_GDP_ratio_April_2020 = IR_4_2020 / GDP2019 )

# Create the change in ratios between April and February
InternationalReserves <- InternationalReserves %>%
  mutate(Reserve_ratio_change_February_April_2020 = (IR_GDP_ratio_April_2020 - IR_GDP_ratio_February_2020) * 100, 
         Reserve_ratio_change_March_2020 = (IR_GDP_ratio_March_2020 - IR_GDP_ratio_February_2020 ) *100, 
         Reserve_ratio_change_February_April_2020_percent = (IR_GDP_ratio_April_2020 - IR_GDP_ratio_February_2020)/IR_GDP_ratio_February_2020 * 100, 
         Reserve_ratio_change_March_2020_percent = (IR_GDP_ratio_March_2020 - IR_GDP_ratio_February_2020)/IR_GDP_ratio_February_2020 *100 )


# =========================================================================.



# Histogram with 20 bins --------------------------------------------------
h1 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_Start_2019)) + 
  geom_histogram(bins = 20) + 
  xlab("") + 
  ggtitle("Start of January 2019") 

h2 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_February_2020)) + 
  geom_histogram(bins = 20) + 
  xlab("") + 
  ggtitle("End of February 2020") 

h3 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_March_2020)) + 
  geom_histogram(bins = 20) + 
  xlab("") + 
  ggtitle("End of March 2020") 

h4 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_April_2020)) + 
  geom_histogram(bins = 20) + 
  xlab("") + 
  ggtitle("End of April 2020") 

library(cowplot)
# Output
title <- ggdraw() + 
  draw_label(
    "International reserves to GDP ratio, 20 bins",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(h1, h2, h3, h4)
plot_grid(title, plot_row)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("Plots/Sensitivity/forexreserveshistograms20bins.pdf", width = 11.69, height = 8.27 )
# grid.arrange(h1, h2, h3, h4,nrow=2)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()
# grid.arrange(h1, h2, h3, h4,nrow=2)
# =========================================================================.



# Histogram with 3 bins --------------------------------------------------
h1 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_Start_2019)) + 
  geom_histogram(bins = 3) + 
  xlab("") + 
  ggtitle("Start of January 2019") 

h2 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_February_2020)) + 
  geom_histogram(bins = 3) + 
  xlab("") + 
  ggtitle("End of February 2020") 

h3 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_March_2020)) + 
  geom_histogram(bins = 3) + 
  xlab("") + 
  ggtitle("End of March 2020") 

h4 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_April_2020)) + 
  geom_histogram(bins = 3) + 
  xlab("") + 
  ggtitle("End of April 2020") 

# Output
title <- ggdraw() + 
  draw_label(
    "International reserves to GDP ratio, 3 bins",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(h1, h2, h3, h4)
plot_grid(title, plot_row)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("Plots/Sensitivity/forexreserveshistograms3bins.pdf", width = 11.69, height = 8.27 )
# grid.arrange(h1, h2, h3, h4,nrow=2)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()
# grid.arrange(h1, h2, h3, h4,nrow=2)
# =========================================================================.





# Manual bin width --------------------------------------------------------
h1 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_Start_2019)) + 
  geom_histogram(binwidth=0.1) + 
  xlab("") + 
  ggtitle("Start of January 2019") 

h2 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_February_2020)) + 
  geom_histogram(binwidth=0.1) + 
  xlab("") + 
  ggtitle("End of February 2020") 

h3 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_March_2020)) + 
  geom_histogram(binwidth=0.1) + 
  xlab("") + 
  ggtitle("End of March 2020") 

h4 <- ggplot(InternationalReserves, aes(IR_GDP_ratio_April_2020)) + 
  geom_histogram(binwidth=0.1) + 
  xlab("") + 
  ggtitle("End of April 2020") 

# Output
title <- ggdraw() + 
  draw_label(
    "International reserves to GDP ratio, bin width = 0.1",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(h1, h2, h3, h4)
plot_grid(title, plot_row)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("Plots/Sensitivity/forexreserveshistogramssetbinwidth.pdf", width = 11.69, height = 8.27 )
# grid.arrange(h1, h2, h3, h4,nrow=2)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()
# =========================================================================.




# Barplots of IR to GDP ratios --------------------------------------------
p1 <- arrange(InternationalReserves, IR_GDP_ratio_Start_2019 ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(IR_GDP_ratio_Start_2019))) %>%
  ggplot( aes(COUNTRY , IR_GDP_ratio_Start_2019, label = IR_GDP_ratio_Start_2019)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("Start of January 2019, share of 2018 GDP") 
  guides(fill=FALSE) +
  theme_minimal() 

p2 <- arrange(InternationalReserves, IR_GDP_ratio_February_2020 ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(IR_GDP_ratio_February_2020))) %>%
  ggplot( aes(COUNTRY , IR_GDP_ratio_February_2020, label = IR_GDP_ratio_February_2020)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("End of February 2020, share of 2019 GDP") 
  guides(fill=FALSE) +
  theme_minimal() 

p3 <- arrange(InternationalReserves, IR_GDP_ratio_March_2020 ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(IR_GDP_ratio_March_2020))) %>%
  ggplot( aes(COUNTRY , IR_GDP_ratio_March_2020, label = IR_GDP_ratio_March_2020)) + 
  coord_flip() +    
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("End of March 2020, share of 2019 GDP") 
  guides(fill=FALSE) +
  theme_minimal() 
  
p4 <- arrange(InternationalReserves, IR_GDP_ratio_April_2020 ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(IR_GDP_ratio_April_2020))) %>%
  ggplot( aes(COUNTRY , IR_GDP_ratio_April_2020, label = IR_GDP_ratio_April_2020)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("End of April 2020, share of 2019 GDP") 
  guides(fill=FALSE) +
  theme_minimal() 

# Output
title <- ggdraw() + 
  draw_label(
    "International reserve to GDP ratios",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(p1,p2,p3, p4)
plot_grid(title, plot_row)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("Plots/Sensitivity/reserveratioacrosstime.pdf", width = 11.69, height = 8.27 )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()
# grid.arrange(p1,p2,p3,nrow=1)
# =========================================================================.






# Absolute change in reserve ratio across time ----------------------------
p4 <- arrange(InternationalReserves, Reserve_ratio_change_March_2020 ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(Reserve_ratio_change_March_2020))) %>%
  ggplot( aes(COUNTRY , Reserve_ratio_change_March_2020, label = Reserve_ratio_change_March_2020)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("% of 2019 GDP") + 
  guides(fill=FALSE) +
  ggtitle("Change in March 2020") + 
  theme_minimal() 

p5 <- arrange(InternationalReserves, Reserve_ratio_change_February_April_2020 ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(Reserve_ratio_change_February_April_2020))) %>%
  ggplot( aes(COUNTRY , Reserve_ratio_change_February_April_2020, label = Reserve_ratio_change_February_April_2020)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("% of 2019 GDP") + 
  guides(fill=FALSE) +
  ggtitle("Change in March/April 2020") + 
  theme_minimal() 

# Output
title <- ggdraw() + 
  draw_label(
    "Absolute change in international reserve to GDP ratio",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(p4, p5)
plot_grid(title, plot_row)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("Plots/Sensitivity/absolutechange.pdf", width = 11.69, height = 8.27 )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()
# grid.arrange(p4,p5,nrow=1)
# =========================================================================.





# Relative change in reserve ratio across time ----------------------------
p6 <- arrange(InternationalReserves, Reserve_ratio_change_March_2020_percent ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(Reserve_ratio_change_March_2020_percent))) %>%
  ggplot( aes(COUNTRY , Reserve_ratio_change_March_2020_percent, label = Reserve_ratio_change_March_2020_percent)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("%") + 
  guides(fill=FALSE) +
  ggtitle("Change in March 2020") + 
  theme_minimal() 

p7 <- arrange(InternationalReserves, Reserve_ratio_change_February_April_2020_percent ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(Reserve_ratio_change_February_April_2020_percent))) %>%
  ggplot( aes(COUNTRY , Reserve_ratio_change_February_April_2020_percent, label = Reserve_ratio_change_February_April_2020_percent)) + 
  coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("%") + 
  guides(fill=FALSE) +
  ggtitle("Change in March/April 2020") + 
  theme_minimal() 

# Output
title <- ggdraw() + 
  draw_label(
    "Relative change in international reserve to GDP ratio",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_row <- plot_grid(p6, p7)
plot_grid(title, plot_row)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("Plots/Sensitivity/relativechange.pdf", width = 11.69, height = 8.27 )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()
# grid.arrange(p4,p5,nrow=1)
# =========================================================================.



# Corr. coefficients between actual vs fitted values, entire period --------
# Create the individual figures of actual vs fitted of entire period
pdat<-data.frame(em_cds2$Date[which(em_cds2$Date>"2019-06-30")],post.dat,predz)
colnames(pdat)[1]<-"date"
countriez<-colnames(pdat)[-1]
country_lists<-list()
for(i in 1:(length(countriez)/2)) {
  plotdat<-pdat[-1,c(1,i+1,i+31)]
  plotdat[,-1]<-apply(plotdat[,-1],2,cumsum)
  country_plots[[i]] <- ggplot(data=(plotdat), aes_string(x="date",y=colnames(plotdat)[2]))+geom_line()+theme_bw()+ylab("")+xlab(countriez[i])+geom_line(aes_string(x="date",y=colnames(plotdat)[3]),linetype=2)
}

# Save output
pdf("Plots/Sensitivity/actualvsfitted_including_COVID_period.pdf", width = 11.69, height = 8.27 )
do.call("grid.arrange", c(country_plots)) 
dev.off()

# Correlation coefficients of actual vs fitted of entire period
for(i in 1:(length(countriez)/2)) {
  plotdat<-pdat[-1,c(1,i+1,i+31)]
  CountryName <- colnames(plotdat)[2]
  CorrelationCoefficient <- cor(plotdat[,2], plotdat[,3] ) 
  country_lists[[i]] <- c(CountryName, CorrelationCoefficient) }
CountryCorrelationCoefficients <- data.frame(matrix(unlist(country_lists), nrow=30, byrow=T))
colnames(CountryCorrelationCoefficients) <- c("COUNTRY", "Correlation_Coefficient")
arrange(CountryCorrelationCoefficients, Correlation_Coefficient )

p8 <- arrange(CountryCorrelationCoefficients, Correlation_Coefficient ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(Correlation_Coefficient))) %>%
  ggplot( aes(COUNTRY , Correlation_Coefficient, label = Correlation_Coefficient)) + 
  # coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("") + 
  guides(fill=FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
p8

# Save output
pdf("Plots/Sensitivity/correlationcoefficients_actual_vs_fited_entire_period.pdf", width = 11.69, height = 8.27 )
p8
dev.off()

# Set threshold for correlation coefficients
threshold <- 0.25

# Find countries above threshold
CountriesAboveThreshold_EntirePeriod <- CountryCorrelationCoefficients[which(CountryCorrelationCoefficients$Correlation_Coefficient>threshold), ]
CountriesBelowThreshold_EntirePeriod <- CountryCorrelationCoefficients[which(CountryCorrelationCoefficients$Correlation_Coefficient<=threshold), ]
nrow(CountriesAboveThreshold_EntirePeriod)
nrow(CountriesBelowThreshold_EntirePeriod)
View(CountriesBelowThreshold_EntirePeriod)

# not needed
# countriesnotoverlapping <- CountriesBelowThreshold_EntirePeriod[CountriesBelowThreshold_EntirePeriod$COUNTRY %notin% CountriesBelowThreshold_BeforeCovid$COUNTRY,]
# View(countriesnotoverlapping$COUNTRY)
# countriesnotoverlapping2 <- CountriesBelowThreshold_BeforeCovid[CountriesBelowThreshold_BeforeCovid$COUNTRY %notin% CountriesBelowThreshold_EntirePeriod$COUNTRY,]
# View(countriesnotoverlapping2$COUNTRY)




# Corr. coefficients between actual vs fitted values, pre-COVID pe --------
# Create the individual figures of actual vs fitted of period before COVID
colnames(pdat)[1] <- "date"
pdat_before_COVID<-pdat[which(pdat$date < "2020-03-01"),]
countriez<-colnames(pdat_before_COVID)[-1]
country_lists2<-list()
for(i in 1:(length(countriez)/2)) {
  plotdat<-pdat_before_COVID[-1,c(1,i+1,i+31)]
  plotdat[,-1]<-apply(plotdat[,-1],2,cumsum)
  country_lists2[[i]] <- ggplot(data=(plotdat), aes_string(x="date",y=colnames(plotdat)[2]))+geom_line()+theme_bw()+ylab("")+xlab(countriez[i])+geom_line(aes_string(x="date",y=colnames(plotdat)[3]),linetype=2)
}

# Save output
pdf("Plots/Sensitivity/actualvsfitted_before_COVID_period.pdf", width = 11.69, height = 8.27 )
do.call("grid.arrange", c(country_lists2)) 
dev.off()

# Correlation coefficients of actual vs fitted of period before COVID
countriez<-colnames(pdat_before_COVID)[-1]
country_lists2<-list()
for(i in 1:(length(countriez)/2)) {
  plotdat<-pdat_before_COVID[-1,c(1,i+1,i+31)]
  CountryName <- colnames(plotdat)[2]
  CorrelationCoefficient <- cor(plotdat[,2], plotdat[,3] ) 
  country_lists2[[i]] <- c(CountryName, CorrelationCoefficient) }
CountryCorrelationCoefficients_before_COVID <- data.frame(matrix(unlist(country_lists2), nrow=30, byrow=T))
colnames(CountryCorrelationCoefficients_before_COVID) <- c("COUNTRY", "Correlation_Coefficient")
arrange(CountryCorrelationCoefficients_before_COVID, Correlation_Coefficient )

p9 <- arrange(CountryCorrelationCoefficients_before_COVID, Correlation_Coefficient ) %>%
  mutate(COUNTRY = fct_reorder(COUNTRY, desc(Correlation_Coefficient))) %>%
  ggplot( aes(COUNTRY , Correlation_Coefficient, label = Correlation_Coefficient)) + 
  #coord_flip() +
  geom_bar(stat="identity", width=.90) + 
  xlab("") + # Set axis labels
  ylab("") + 
  guides(fill=FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  
p9

# Save output
pdf("Plots/Sensitivity/correlationcoefficients_actual_vs_fited_before_COVID_period.pdf", width = 11.69, height = 8.27 )
p9
dev.off()


# Set threshold for correlation coefficients
threshold <- 0.25

CountriesAboveThreshold_BeforeCovid <- CountryCorrelationCoefficients_before_COVID[which(CountryCorrelationCoefficients_before_COVID$Correlation_Coefficient>threshold), ]
CountriesBelowThreshold_BeforeCovid <- CountryCorrelationCoefficients_before_COVID[which(CountryCorrelationCoefficients_before_COVID$Correlation_Coefficient<threshold), ]
View(CountriesAboveThreshold_BeforeCovid$COUNTRY)
View(CountriesBelowThreshold_BeforeCovid$COUNTRY)
nrow(CountriesAboveThreshold_BeforeCovid)
# =========================================================================.


# Countries to drop -------------------------------------------------------
# From the previous analysis, it appears that 10 of the 30 countries would drop 
# from the sample as they had a correlation coefficient between actual vs fitted
# values in the pre-COVID period of below 0.25. These ten countries are: 
View(CountriesBelowThreshold_BeforeCovid$COUNTRY)
CountriesToDrop <- CountriesBelowThreshold_BeforeCovid$COUNTRY

# The remaning 20 countries for the reduced sample all have a correlation 
# coefficient above 0.25. These countries are 
View(CountriesAboveThreshold_BeforeCovid$COUNTRY)
CountriesToRemain <- CountriesAboveThreshold_BeforeCovid$COUNTRY
# =========================================================================.


# Next step ---------------------------------------------------------------
# Now that we know which countries to drop, the next step is to add the three
# missing explanatory variables to the large panel so that we can run the second
# stage regression. 
# =========================================================================.







# IR panel ----------------------------------------------------------------

# Importing Data on monthly international reserves across time and deleting unnecessary columns
InternationalReservesTS <- read_excel("Data/InternationalReserves.xlsx", sheet = "ReserveTS")
InternationalReservesTS <- InternationalReservesTS[, -c(1,2)]
InternationalReservesTS

# Making dataframe into time seris object
InternationalReservesTS <- xts(InternationalReservesTS, order.by=InternationalReservesTS$DateBeginOfMonth)
str(InternationalReservesTS)
View(InternationalReservesTS)

# Deleting redundant date column 
InternationalReservesTS <- InternationalReservesTS[, -c(1) ]

# Converting monthly data to daily. For example: I use the (end of) February values to create the daily values for 1-31 of March etc.
InternationalReservesTS <- na.locf(merge(InternationalReservesTS, foo=zoo(NA, order.by=seq(start(InternationalReservesTS), end(InternationalReservesTS),
                                               "day",drop=F)))[, ])

# Deleting unnecessary column 
InternationalReservesTS <- InternationalReservesTS[, -c(31) ]

# View if the data is correct
View(InternationalReservesTS)
dim(InternationalReservesTS)

# Here I add an id variable so that I can then later reshape the wide data into a long panel
InternationalReservesTS$id <- seq(from = 1, to = 548, by = 1 )

# Now I make it a dataframe such as to keep the index part of the data
InternationalReservesTS <- data.frame(Date=index(InternationalReservesTS), coredata(InternationalReservesTS))

# Now I melt the data into long form
InternationalReservesTS_long <- reshape2::melt(InternationalReservesTS, id.vars=c("Date", "id"))

# Renaming the variables
colnames(InternationalReservesTS_long) <- c("Date", "id", "COUNTRY", "IR")

# Removing the id variable which is no longer needed
InternationalReservesTS_long <- InternationalReservesTS_long[,  c("Date", "COUNTRY", "IR")]

# Subsetting such as to have the same time frame as the object "panel"
InternationalReservesTS_long <- InternationalReservesTS_long[which(InternationalReservesTS_long$Date < '2020-07-01'),]
InternationalReservesTS_long <- InternationalReservesTS_long[which(InternationalReservesTS_long$Date >= '2019-07-02'),]
dim(InternationalReservesTS_long)

View(InternationalReservesTS_long)
dim(InternationalReservesTS_long)
str(InternationalReservesTS_long)

merged_test <- merge(panel, InternationalReservesTS_long, by.x=c("Country", "Date"), by.y=c("COUNTRY", "Date"), all.x=TRUE)
# xxx here I have to specify the by variable more clearly
# =========================================================================.


# Not needed I think -------------------------------------------------------
# unique(panel$Country)
# dim(panel)
# dim()
# unique(InternationalReservesTS_long$Country)
# InternationalReservesTS_long <- InternationalReservesTS_long[which(InternationalReservesTS_long$Date < '2020-07-01'),]
# InternationalReservesTS_long <- InternationalReservesTS_long[which(InternationalReservesTS_long$Date >= '2019-07-02'),]
# dim(InternationalReservesTS_long)
# unique(panel$Date)
# dim(merged_test)
# View(merged_test)
# =========================================================================.



# IR/GDP ratio panel ------------------------------------------------------

# Importing data on monthly international reserve to GDP ratio and deleting unnecessary columns
InternationalReserveRatioTS <- read_excel("Data/InternationalReserves.xlsx", sheet = "ReserveRatioTS")
InternationalReserveRatioTS <- InternationalReserveRatioTS [, -c(1,2)]
InternationalReserveRatioTS

# Making dataframe into time seris object
InternationalReserveRatioTS <- xts(InternationalReserveRatioTS, order.by=InternationalReserveRatioTS$DateBeginOfMonth)
str(InternationalReserveRatioTS)
View(InternationalReserveRatioTS)

# Deleting redundant column 
InternationalReserveRatioTS <- InternationalReserveRatioTS[, -c(1) ]

# Converting monthly data to daily. For example: I use the (end of) February values to create the daily values for 1-31 of March etc.
InternationalReserveRatioTS <- na.locf(merge(InternationalReserveRatioTS, foo=zoo(NA, order.by=seq(start(InternationalReserveRatioTS), end(InternationalReserveRatioTS),
                                                                                                   "day",drop=F)))[, ])
# Deleting unnecessary column 
InternationalReserveRatioTS <- InternationalReserveRatioTS[, -c(31) ]

# View if the data is correct
View(InternationalReserveRatioTS)
dim(InternationalReserveRatioTS)

# Here I add an id variable so that I can then later reshape the wide data into a long panel
InternationalReserveRatioTS$id <- seq(from = 1, to = 548, by = 1 )

# Now I make it a dataframe such as to keep the index part of the data
InternationalReserveRatioTS <- data.frame(Date=index(InternationalReserveRatioTS), coredata(InternationalReserveRatioTS))

# Now I melt the data into long form
InternationalReserveRatioTS_long <- reshape2::melt(InternationalReserveRatioTS, id.vars=c("Date", "id"))

# Renaming the variables
colnames(InternationalReserveRatioTS_long) <- c("Date", "id", "COUNTRY", "IR/GDP")

# Removing the id variable which is no longer needed
InternationalReserveRatioTS_long <- InternationalReserveRatioTS_long[,  c("Date", "COUNTRY", "IR/GDP")]

# Subsetting such as to have the same time frame as the object "panel"
InternationalReserveRatioTS_long <- InternationalReserveRatioTS_long[which(InternationalReserveRatioTS_long$Date < '2020-07-01'),]
InternationalReserveRatioTS_long <- InternationalReserveRatioTS_long[which(InternationalReserveRatioTS_long$Date >= '2019-07-02'),]
dim(InternationalReserveRatioTS_long)

View(InternationalReserveRatioTS_long)
dim(InternationalReserveRatioTS_long)
str(InternationalReserveRatioTS_long)

merged_test <- merge(panel, InternationalReserveRatioTS_long, by.x=c("Country", "Date"), by.y=c("COUNTRY", "Date"), all.x=TRUE)
# =========================================================================.




# SWF data ----------------------------------------------------------------
# Importing data on SWF
SWF <- read_excel("Data/SWF.xlsx", sheet = "R")
View(SWF)

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
colnames(SWF_Countryaggregates) <- c("COUNTRY", "AuM")
SWF_Countryaggregates

# Here I would do the same but with Aum in bn
# SWF_Countryaggregates_bn <- aggregate(SWF$AuM_bn, by=list(Category=SWF$COUNTRY), FUN=sum)
# colnames(SWF_Countryaggregates_bn) <- c("COUNTRY", "AuM_bn")
# SWF_Countryaggregates_bn

# Merging all countries in sample with countries that have SWF
SWF <- merge(allCountriesSWF, SWF_Countryaggregates, by =  c("COUNTRY"), all=TRUE ) 

# Countries without SWF are NA, so I set them to zero
SWF$AuM[is.na(SWF$AuM)] <- 0

# Import GDP data for 2019
GDP <- read_excel("Data/GDP.xlsx", sheet = "EM_GDP")

# Merging GDP and SWF data
SWF <- merge(SWF, GDP, by =  c("COUNTRY"), all=TRUE ) 

# Creating SWF/GDP ratio
SWF <- SWF %>% mutate(SWF_GDP_RATIO = AuM / GDP)

# Drop intermediate data on GDP and AuM 
SWF <- SWF[, c("COUNTRY", "SWF_GDP_RATIO")]

# Get the data into wide format
SWF_transpose <- as.data.frame(t(as.matrix(SWF)))

# Renaming the columns with the country names
colnames(SWF_transpose) <- SWF_transpose["COUNTRY",]

# Subsetting to obtain only the relevant data
SWF <- SWF_transpose["SWF_GDP_RATIO",]

# Adding a date column
SWF <- SWF %>% mutate(DateBeginOfMonth = "2019-01-01")

# Changeing format to date column
SWF$DateBeginOfMonth <- as.Date(SWF$DateBeginOfMonth)

# Duplicate row
SWF <- rbind(SWF, SWF[rep(1), ]) 

# Changing 
SWF["SWF_GDP_RATIO1", "DateBeginOfMonth"] <- "2020-07-01"

# Making dataframe into time seris object
SWF_TS <- xts(SWF, order.by=SWF$DateBeginOfMonth)
str(SWF_TS)
View(SWF_TS)

# Deleting redudant date column as we have a ts object now
SWF_TS <- SWF_TS[, -31]

# Converting monthly data to daily. For example: I use the (end of) February values to create the daily values for 1-31 of March etc.
SWF_TS <- na.locf(merge(SWF_TS, foo=zoo(NA, order.by=seq(start(SWF_TS), end(SWF_TS),
                                                                                           "day",drop=F)))[, ])

# Deleting unnecessary column 
SWF_TS <- SWF_TS[, ! colnames(SWF_TS) %in% c("foo")]

# View if the data is correct
View(SWF_TS)
dim(SWF_TS)

# Here I add an id variable so that I can then later resphape the wide data into a long panel
SWF_TS$id <- seq(from = 1, to = 549, by = 1 )

# Now I make it a dataframe such as to keep the index part of the data
SWF_TS <- data.frame(Date=index(SWF_TS), coredata(SWF_TS))

# Now I melt the data into long form
SWF_TS_long <- reshape2::melt(SWF_TS, id.vars=c("Date", "id"))

# Renaming the variables
colnames(SWF_TS_long) <- c("Date", "id", "Country", "SWF_GDP_ratio")

# Removing the id variable which is no longer needed
SWF_TS_long <- SWF_TS_long[,  c("Date", "Country", "SWF_GDP_ratio")]

# Subsetting such as to have the same time frame as the object "panel"
SWF_TS_long <- SWF_TS_long[which(SWF_TS_long$Date < '2020-07-01'),]
SWF_TS_long <- SWF_TS_long[which(SWF_TS_long$Date >= '2019-07-02'),]
dim(SWF_TS_long)

View(SWF_TS_long)
dim(SWF_TS_long)
str(SWF_TS_long)

merged_test_3 <- merge(panel, SWF_TS_long, by=c("Country", "Date"), all.x=TRUE)
View(merged_test_3)
# =========================================================================.



# SWF and PPF data ----------------------------------------------------------
# Importing data on SWF and PPF
SWFandPPF <- read_excel("Data/SWF.xlsx", sheet = "R")
View(SWFandPPF)

# Replace funds with NA in AuM with zero
SWFandPPF$AuM_bn[SWFandPPF$AuM_bn == "NA"]  <- 0

# Changing AuM to numeric
SWFandPPF$AuM_bn <- as.numeric(SWFandPPF$AuM_bn)

# Changin AuM from billion to USD
SWFandPPF <- SWFandPPF %>% mutate(AuM = AuM_bn * 10^9)

# # Drop Funds which aren't SWF, i.e. PPF
# SWF <- subset(SWF, Type=="SWF" )

# Defining sample countries so that I can subset the SWF dataset 
countries <- read_excel("Data/laender.xlsx", sheet = "EM")
allCountriesSWFandPPF <- countries[ which(countries$EM_dummy5yr ==1), "COUNTRY5yrCDS"]
colnames(allCountriesSWFandPPF) <- "COUNTRY"

# Subsetting countries with SWF that are in sample
SWFandPPF <- SWFandPPF[SWFandPPF$COUNTRY %in% allCountriesSWFandPPF$COUNTRY,  ]

# Summing funds per country to get country total SWF
SWFandPPF_Countryaggregates <- aggregate(SWFandPPF$AuM, by=list(Category=SWFandPPF$COUNTRY), FUN=sum)
colnames(SWFandPPF_Countryaggregates) <- c("COUNTRY", "AuM")
SWFandPPF_Countryaggregates

# Here I would do the same but with Aum in bn
# SWF_Countryaggregates_bn <- aggregate(SWF$AuM_bn, by=list(Category=SWF$COUNTRY), FUN=sum)
# colnames(SWF_Countryaggregates_bn) <- c("COUNTRY", "AuM_bn")
# SWF_Countryaggregates_bn

# Merging all countries in sample with countries that have SWF and or PPF
SWFandPPF <- merge(allCountriesSWFandPPF, SWFandPPF_Countryaggregates, by =  c("COUNTRY"), all=TRUE ) 

# Countries without SWF are NA, so I set them to zero
SWFandPPF$AuM[is.na(SWFandPPF$AuM)] <- 0

# Import GDP data for 2019
GDP <- read_excel("Data/GDP.xlsx", sheet = "EM_GDP")

# Merging GDP and SWF data
SWFandPPF <- merge(SWFandPPF, GDP, by =  c("COUNTRY"), all=TRUE ) 

# Creating SWF and PPF /GDP ratio
SWFandPPF <- SWFandPPF %>% mutate(SWFandPPF_GDP_RATIO = AuM / GDP)

# Drop intermediate data on GDP and AuM 
SWFandPPF <- SWFandPPF[, c("COUNTRY", "SWFandPPF_GDP_RATIO")]

# Get the data into wide format
SWFandPPF_transpose <- as.data.frame(t(as.matrix(SWFandPPF)))

# Renaming the columns with the country names
colnames(SWFandPPF_transpose) <- SWFandPPF_transpose["COUNTRY",]

# Subsetting to obtain only the relevant data
SWFandPPF <- SWFandPPF_transpose["SWFandPPF_GDP_RATIO",]

# Adding a date column
SWFandPPF <- SWFandPPF %>% mutate(DateBeginOfMonth = "2019-01-01")

# Changeing format to date column
SWFandPPF$DateBeginOfMonth <- as.Date(SWFandPPF$DateBeginOfMonth)

# Duplicate row
SWFandPPF <- rbind(SWFandPPF, SWFandPPF[rep(1), ]) 

# Changing 
SWFandPPF["SWFandPPF_GDP_RATIO1", "DateBeginOfMonth"] <- "2020-07-01"

# Making dataframe into time seris object
SWFandPPF_TS <- xts(SWFandPPF, order.by=SWFandPPF$DateBeginOfMonth)
str(SWFandPPF_TS)
View(SWFandPPF_TS)

# Deleting redudant date column as we have a ts object now
SWFandPPF_TS <- SWFandPPF_TS[, -31]

# Converting monthly data to daily. For example: I use the (end of) February values to create the daily values for 1-31 of March etc.
SWFandPPF_TS <- na.locf(merge(SWFandPPF_TS, foo=zoo(NA, order.by=seq(start(SWFandPPF_TS), end(SWFandPPF_TS),
                                                         "day",drop=F)))[, ])

# Deleting unnecessary column 
SWFandPPF_TS <- SWFandPPF_TS[, ! colnames(SWFandPPF_TS) %in% c("foo")]

# View if the data is correct
View(SWFandPPF_TS)
dim(SWFandPPF_TS)

# Here I add an id variable so that I can then later resphape the wide data into a long panel
SWFandPPF_TS$id <- seq(from = 1, to = 549, by = 1 )

# Now I make it a dataframe such as to keep the index part of the data
SWFandPPF_TS <- data.frame(Date=index(SWFandPPF_TS), coredata(SWFandPPF_TS))

# Now I melt the data into long form
SWFandPPF_TS_long <- reshape2::melt(SWFandPPF_TS, id.vars=c("Date", "id"))

# Renaming the variables
colnames(SWFandPPF_TS_long) <- c("Date", "id", "Country", "SWFandPPF_GDP_ratio")

# Removing the id variable which is no longer needed
SWFandPPF_TS_long <- SWFandPPF_TS_long[,  c("Date", "Country", "SWFandPPF_GDP_ratio")]

# Subsetting such as to have the same time frame as the object "panel"
SWFandPPF_TS_long <- SWFandPPF_TS_long[which(SWFandPPF_TS_long$Date < '2020-07-01'),]
SWFandPPF_TS_long <- SWFandPPF_TS_long[which(SWFandPPF_TS_long$Date >= '2019-07-02'),]
dim(SWFandPPF_TS_long)

View(SWFandPPF_TS_long)
dim(SWFandPPF_TS_long)
str(SWFandPPF_TS_long)

merged_test_3 <- merge(panel, SWFandPPF_TS_long, by=c("Country", "Date"), all.x=TRUE)
View(merged_test_3)
# =========================================================================.





