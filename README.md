# COVID19-DOMINANCE
COVID19 Dominance and EM Debt

*Tracing the relationship between Emerging Market sovereign spreads and COVID-19 mortality dynamics, accounting for fiscal interventions.*

This repo stores input, output, and code for the NBER Working Paper "Pandemic shock to emerging markets: COVID-19 and financial fragility" by [Joshua Aizenman](https://dornsife.usc.edu/cf/econ/econ_faculty_display.cfm?Person_ID=1043595) (University of Southern California and NBER), [Yothin Jinjarak](https://www.wgtn.ac.nz/sef/about/staff/yothin-jinjarak) (Victoria University of Wellington); [Timo Daehler](https://dornsife.usc.edu/poir/students/) (University of Southern California).

The master branch hosts the R Markdown and Knitr documents, as well as folders with the initial data; temporary data; visualizations (interactive and static); and regression outputs.

# DESCRIPTION OF THE FILES

The relevant R Markdown file is "COVID-Dominance.rmd" which is fully reproducible in RStudio or RCloud. Accompanying the markdown is a knit file of the output ("COVID-Dominance.html"). The Python file "EZcovid.ipynb" reproduces Figures 1-3 in the working paper.

# DESCRIPTION OF THE FOLDERS

The "data" folder includes all of the initial and manipulated data used in the analysis. Details on where the data was pulled from can be found in the relevant chunks of the Markdown file, in the text documents of the working paper, and in the associated README file. Credit for the raw data goes to all associated sources, which are cited in the paper.

In addition to the data folder, the repository also hosts the "csse_covid_19_time_series", "government_data", and "spreads_data" folders. The data from these folders is also uploaded to the central data folder, but have been included separately for ease of access. These represent our primary raw data files. The first file contains the COVID data pulled from John Hopkins CSSE. Note that we use data up until our cutoff date of June 30th, 2020. The second file contains the government data pulled from the Oxford Government Response Tracker. The last folder contains raw datafiles used for our projection analysis of Eurozone spreads, pulled from Thomson Reuters.

The "output_png" folder includes all of our static figures. All interative HTML figures can be generated using the Markdown file (or for Figures 1-3 through the Python script), and viewed in the Knit documents.

The "reg_output" folder includes all of the tables from the regression analysis, which have been clearly labelled for ease of access. You will be able to find where the tables are generated in the markdown file by using the search function.

All folders in the repository are accompanied by a detailed README file explaining the contents.

**CONTACT**

Joshua Aizenman

aizenman@usc.edu

Yothin Jinjarak

yothin.jinjarak@vuw.ac.nz

Timo Daehler  

daehler@usc.edu       
