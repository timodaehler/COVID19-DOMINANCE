The data folder contains all the raw, temporary, and final data files used for analysis. The sample consists of all 34 countries classified as investible emerging markets by different sources [JPM EMBI](https://www.ishares.com/us/products/239572/ishares-jp-morgan-usd-emerging-markets-bond-etf), [JPM LEMB](https://www.ishares.com/us/products/239528/ishares-emerging-markets-local-currency-bond-etf).

*Note that all COVID-related variables (cases, mortality, etc.) are only available from the start of the Hopkins data, on January 23rd, 2020; while the Oxford government data begins on January 1, 2020.*

**Raw Data**

1. Various raw datafiles on [EM CDS spread data](https://github.com/snairdesai/COVID_Dominance/tree/master/spreads_data).  XXXX
2. [Country_Coordinates.csv](https://developers.google.com/public-data/docs/canonical/countries_csv) contains centroid coordinates for each nation in the sample.
3. [GNI.csv](https://data.worldbank.org/indicator/NY.GNP.PCAP.CD?view=chart) controls for GNI per capita (calculated using the Atlas Method).
4. [Government_Responses.csv](https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker) contains policy data pulled from Oxford. A more detailed README file is available in the relevant folder.
5. [ICD_Deaths_Final.csv](https://www.who.int/classifications/icd/icdonlineversions/en/) pulls information on prior deaths from diseases (endocrine, high blood pressure, and respiratory) aggregated across 2015-2018.
6. [UN_Population_Data](https://population.un.org/wpp/Download/Standard/Population/) contains the country population data used to construct mortality rates.
7. [applemobilitytrends.csv](https://www.apple.com/covid19/mobility) details mobility data provided by Apple, which captures changes in walking, driving, and transit levels across nations.
8. [arrivals.csv](https://data.worldbank.org/indicator/ST.INT.ARVL) controls for international tourist arrivals in the l.y.a. (2018).
9. [cellular.csv](https://data.worldbank.org/indicator/IT.CEL.SETS.P2?start=1960) controls for the number of cellular subscriptions by nation.
10. The countryContinent.csv is not used in our analysis, but may be used to generate subplots in the visualizations (i.e. by continent or sub-region, rather than by nation).
11. The democracy.csv file aggregates democracy data from [Freedom House](https://freedomhouse.org/countries/freedom-world/scores) and the [Economist Intelligence Unit](https://www.eiu.com/topic/democracy-index?&zid=democracyindex2019&utm_source=blog&utm_medium=blog&utm_name=democracyindex2019&utm_term=democracyindex2019&utm_content=top_link).
12. [departures.csv](https://data.worldbank.org/indicator/ST.INT.DPRT) controls for international tourist departures in the l.y.a. (2018).
13. Both global_preferences_survey_country.dta and global_preferences_survey_individual.dta were pulled from the [Global Preferences Survey](https://www.briq-institute.org/global-preferences/downloads). In our cross-sectional analysis, these datasets (very roughly) attempt to control for differing levels of patience; trust; and altriusm across nations.
14. [govt.csv](https://info.worldbank.org/governance/wgi/Home/Documents) reports indicators related to nations' political stability, government effectiveness and accountability, rule of law, etc.
15. [health_pc.csv](https://data.worldbank.org/indicator/SH.XPD.CHEX.PC.CD?view=chart) accounts for previous health expenditures in the l.y.a. (2017).
16. [military.csv](https://correlatesofwar.org/data-sets/national-material-capabilities) captures measures of state capacity and force, including military personnel, military expenditures, iron and steel production, and an index of national capabilities. Note, the l.y.a. for this data is 2012.
17. [pollution.csv](https://data.worldbank.org/indicator/EN.ATM.PM25.MC.M3?view=chart) is a dataset of PM2.5 Air Pollution (micrograms per cubic meter).
18. [pop65yo.csv](https://data.worldbank.org/indicator/SP.POP.65UP.TO.ZS) controls for the proportion of the population above the age of 65+ (which we consider elderly).
19. [popdensity.csv](https://data.worldbank.org/indicator/EN.POP.DNST) refers to the density of people per square kilometer of land.
20. [social_protection.csv](https://data.worldbank.org/indicator/IQ.CPA.PROT.XQ?view=chart) is the World Bank CPIA's index for a nation's level of social protection provided for citizens.
21. The [testcap.csv](https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv) file contains data on the test capacity and distribution across nations. Because of the large number of missing values in the data, this initial file is not used in our analysis.
22. The [transparency.csv](https://data.worldbank.org/indicator/IQ.CPA.TRAN.XQ) file contains data from the World Bank CPIA's index on government transparency.
23. [urbanpp.csv](https://data.worldbank.org/indicator/SP.URB.TOTL.in.zs) measures the proportion of the population living in urban areas.
24. [vulnerable_employment.csv](https://data.worldbank.org/indicator/SL.EMP.VULN.ZS?view=chart) captures the proportion of the population's total employment working in occupations considered "vulnerable" to macroeconomic shocks (i.e. working in family-businesses/self-employed).

**Final Data**

1. Oxford_V1 is the primary dataset used for our analysis. It includes the following covariates, which are also described and labelled in the Markdown file (I only describe the most relevant variables below; please the WP or Markdown for a complete breakdown):

    A. Total_Cases_Country --> Cumulative COVID-19 cases recorded for each country, per day (*JHU CSSE*).
    
    B. Total_Deceased_Country --> Cumulative COVID-19 deaths recorded for each country, per day (*JHU CSSE*).
    
    C. New_Confirmed_Country --> New COVID-19 cases recorded for each country, per day (*JHU CSSE*).
    
    D. New_Total_Deceased_Country --> New COVID-19 deaths recorded for each country, per day (*JHU CSSE*).
    
    E. Population --> The population of the country.
    
    F. Total_Mortality_Rate_Per_Capita --> Cumulative mortality rate per capita for each country per day, calculated as    
       Total_Deceased_Country/Population.
       
    G. New_Mortality_Rate_Per_Capita --> New mortality rate per capita for each country per day, calculated as    
       New_Total_Deceased_Country/Population.
       
    H. Total_Cases_Country_Per_Capita --> Cumulative case count per capita for each country per day, calculated as      
       Total_Cases_Country/Population.
       
    I. rolling_average_confirmed --> Seven-day rolling average of New_Confirmed_Country (for observations within the first 
       week, the rolling average is updated per day -- i.e. the rolling average for Day 4 is calculated as the rolling average 
       of new cases for Days 1-3).
       
    J. rolling_average_deceased --> Seven-day rolling average of New_Total_Deceased_Country (for observations within the first 
       week, the rolling average is updated per day -- i.e. the rolling average for Day 4 is calculated as the rolling average 
       of new cases for Days 1-3).
       
    K. total_rolling_average_mortality --> Seven-day rolling average of Total_Mortality_Rate_Per_Capita (for observations
       within the first week, the rolling average is updated per day -- i.e. the rolling average for Day 4 is calculated as the rolling average of new     
       cases for Days 1-3).
       
    L. new_rolling_average_mortality --> Seven-day rolling average of New_Mortality_Rate_Per_Capita (for observations
       within the first week, the rolling average is updated per day -- i.e. the rolling average for Day 4 is calculated as the rolling average of new 
       cases for Days 1-3).
       
    M. The country coordinates of each nation (latitude and longitude).
    
    N. Oxford's Government Response Tracker covariates (from C1_School.closing to the LegacyStringencyIndex). Note Oxford has 
       their own measure for confirmed cases and deaths, which we include in the dataframe (Oxford_Cases and Oxford_Deaths)   
       but do not use for analysis over the Hopkins data.
       
    O. Lagged variables (one week; two week; and three week) created for the government covariates of interest. These columns 
       have simply been created for illustrative purposes; so users can scroll to observe the specific lags implemented. We          use the "Lag" function directly 
       in the regression model to generate these lags, without directly calling these columns.
       
    P. Numerous demographic controls pulled from the World Bank Development Indicators and described above (i.e. prop65; 
       propurban; popdensity; etc.).
       
    Q. Mobility data pulled from Apple (driving; walking; transit).
    
    R. Democracy and institutional data pulled from various sources (i.e. EUI_democracy; freedom_house; cinc; etc.)
    
    S. Prior deaths from high blood pressure, respiratory, and endocrine diseases over the period from 2015-2018, pulled from 
       the World Health Organization's International Classification of Diseases (endocrine_deaths; blood_pressure_deaths;     
       respiratory_deaths).
       
    T.  A number of columns on fiscal and monetary policy announcements. These were collected for individual countries, for the European Central Bank, and for the Federal Reserve. These columns capture whether or not an action or proposal was made by a given nation/institution on a specific date in the sample, but do not control for the size or number of policies on any given day. A row is coded as "1" if the date corresponded with the announcement of at least one key policy. With the exception of the Federal Reserve (whose major announcements related to reductions in the interest rate along with fiscal spending), we restricted our analysis of key fiscal policies to those which provided “millions” or “billions” of local currency units in spending. The extended list of sources used to aggregate this data can be found in the Working Paper.
        
        
    **Note on Policy Announcement Generation**
     
     
     While there are a number of different datasets which attempt to capture the policy announcements and actions of various nations, none is fully comprehensive. As such, the policy announcements used in the Oxford V1 dataset were hard-coded outside of R, by aggregating information from a number of different datasets (listed below). Although we collected information on monetary, macroprudential, and loan policies, we focus on fiscal policy announcements (with the notable exception of the Federal Reserve, for which we capture monetary policy announcements). The methodology is as follows: 
     
     1. We capture these policies across individual countries, the European Central Bank, and the Federal Reserve. Policy announcements were coded as 1, and actions were coded as 2. On days with neither an announcement nor an action, we input 0.
     
     2. These columns captured whether or not an action or proposal was made by a given nation/institution on a specific date in the sample. Thus, we did not control for the size or number of policies on any given day, and only if the date corresponded with the announcement of at least one key policy. 
     
     3. With the exception of the Federal Reserve (whose major announcements related to percent reductions in the interest rate along with spending), we restricted our analysis of key fiscal policies to those which provided “millions” or “billions” of local currency units in spending, based on the descriptions provided in the given sources.
     
     4. A sourced version of the dataset is available (Oxford_V1_Sourced), which color codes the announcements and actions by the dataset we pulled them from.
        The color legend is described below along with the sources of the datasets.
           
  *Sources for Policy Announcements*
  
  A. [Yale COVID-19 Financial Response Tracker](https://som.yale.edu/node/222278) - Colored Red in the Sourced Dataset
  
  B. [Harvard Global Policy Tracker](https://www.hbs.edu/covid-19-business-impact/Insights/Economic-and-Financial-Impacts/Global-Policy-Tracker) - Colored Blue in Sourced Dataset
  
  C. [Bruegel COVID-19 National Dataset](https://www.bruegel.org/publications/datasets/covid-national-dataset/) - Colored Yellow in Sourced Dataset
  
  D. [IMF Policy Responses to COVID](https://www.imf.org/en/Topics/imf-and-covid19/Policy-Responses-to-COVID-19) - Colored Green in Sourced Dataset

  
  
