#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "January 16, 2019"
#' ---

#' # Find Dataset
#' 
#' Think about your interests. Following are few examples of potential data sources:
#' 
#' ## General
#' 
#' * [ICPSR](https://www.icpsr.umich.edu/icpsrweb/ICPSR/index.jsp): You may need to create 
#' the account with UC Davis E-mail Address to download data.
#' * [Link List of Public Data](https://github.com/erikgahner/PolData)
#' 
#' ## Comparative
#' 
#' * [Correlates of War](http://www.correlatesofwar.org)
#' * [Polity IV](http://www.systemicpeace.org/inscrdata.html)
#' * [Varieties of Democracy](https://www.v-dem.net/en/)
#' * [Quality of Government](https://qog.pol.gu.se/data)
#' * [World Bank](https://data.worldbank.org)
#' * [IMF](https://www.imf.org/en/Data)
#' * [World Value Survey](http://www.worldvaluessurvey.org/wvs.jsp)
#' * [Comparative Study of Electoral Systems](http://www.cses.org)
#' * [Manifesto Project](https://manifestoproject.wzb.eu)
#' 
#' ## American
#' 
#' * [American National Election Study (ANES)](https://electionstudies.org)
#' * [Cooperative Congressional Election Study (CCES)](https://cces.gov.harvard.edu/pages/welcome-cooperative-congressional-election-study)
#' * [General Social Survey](http://gss.norc.org)
#' * [Correlates of State Policy](http://ippsr.msu.edu/public-policy/correlates-state-policy)
#' * [Legislative Effectiveness](https://thelawmakers.org/data-download)
#' 
#' # Preparing R Environment
#' 


## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Load Relevant Packages (Install if not Installed)
#install.packages("foreign")
# Data Importing
library(foreign) # Stata 12 or Later
library(readstata13) # For Stata 13 Data or Later
library(haven)
library(readr)
# Data Visualization
library(car)
library(lattice)
library(ggplot2)

#'
#' # Practice Loading Data
#' 

## Online Location Quality of Government Data 
# CSV
#+ echo=FALSE
csvloc <- "./data/qog_bas_cs_jan18.csv" # if downloaded locally
#+ eval=FALSE
csvloc <- "http://www.qogdata.pol.gu.se/data/qog_bas_cs_jan18.csv"
# STATA
#+ echo=FALSE
dtaloc <- "./data/qog_bas_cs_jan18.dta" # if downloaded locally
#+ eval=FALSE
dtaloc <- "http://www.qogdata.pol.gu.se/data/qog_bas_cs_jan18.dta"
# SPSS
#+ echo=FALSE
savloc <- "./data/qog_bas_cs_jan18.sav" # if downloaded locally
#+ eval=FALSE
savloc <- "http://www.qogdata.pol.gu.se/data/qog_bas_cs_jan18.sav"

#+
## Basic Import Commands
# CSV (don't require any package)
d1 <- read.csv(csvloc, stringsAsFactors = FALSE)
# Stata 12 or Before (foreign package)
d2a <- read.dta(dtaloc, convert.factors = FALSE)
# Stata 13 or Later (readstata13 package)
d2b <- read.dta13(dtaloc, convert.factors = FALSE)
# SPSS (may not work with some new format)
d3 <- read.spss(savloc, use.value.labels = FALSE, to.data.frame = TRUE)

## More Advanced Import Commands
# CSV (readr package)
d4 <- read_csv(csvloc)
# Stata (haven package)
d5 <- read_stata(dtaloc)
#d5 <- read_dta(dtaloc)
# SPSS (haven package)
d6 <- read_spss(savloc)
#d6 <- read_sav(savloc)

## Check Format Differences
# CSV (base vs readr)
head(d1[,seq(1,5,1)])
class(d1)
head(d4[,seq(1,5,1)])
class(d4)
# Stata (foreign)
head(d2b[,seq(1,5,1)])
attr(d2b,"var.labels")[1:5]
attr(d2b,"val.labels")[1:5]
# Stata (haven)
head(d5[,seq(1,5,1)])
lapply(d5, function(x) attr(x,"label"))[1:5]
# SPSS (foreign)
head(d3[,seq(1,5,1)])
attr(d3,"var.labels")[1:5]
attr(d3,"val.labels")[1:5]
# SPSS (haven)
head(d6[,seq(1,5,1)])
lapply(d6, function(x) attr(x,"label"))[1:5]

#'
#' # Histogram
#'

#+ fig.width=6, fig.height=4
with(d1, hist(icrg_qog, breaks=10, col="gray",
              xlab="Quality of Government (ICRG)"))
#+ fig.width=6, fig.height=4
with(d1, hist(wdi_gdpcapcon2010, breaks="FD",col="gray",
              xlab="GDP per Capita (2010)"))

#'
#' # Scatter Plot
#' 

# Basic Plot
#+ fig.width=6, fig.height=4
X <- d1$wdi_gdpcapcon2010
Y <- d1$icrg_qog
plot(X,Y, main="Quality of Government (ICRG)", xlab="GDP Per Capita", ylab="Butthead")
abline(lm(Y ~ X), col="red1")

# LOWESS scatter plot smoothing
d1x <- na.omit(d1[,c("wdi_gdpcapcon2010","icrg_qog")]) # Eliminate NAs
#+ fig.width=6, fig.height=4
with(d1x, plot(wdi_gdpcapcon2010, icrg_qog))
with(d1x, lines(lowess(wdi_gdpcapcon2010, icrg_qog)))
with(d1x, lines(lowess(wdi_gdpcapcon2010, icrg_qog, f=1/10), col="red1"))

# Using car
#+ fig.width=6, fig.height=4
scatterplot(icrg_qog ~ wdi_gdpcapcon2010, data=d1,
            smooth=list(span=0.6, lwd=3, lwd.var=2))

# Scatter Plot Matrix by QoG, GDP per capita, and Human Capital Index
#+ fig.width=6, fig.height=6
scatterplotMatrix(~ icrg_qog + wdi_gdpcapcon2010 + pwt_hci, data=d1)

# Using ggplot2
#+ fig.width=6, fig.height=4
ggplot(d1, aes(x=wdi_gdpcapcon2010, y=icrg_qog)) + geom_point() + 
  xlab("GDP per Capita") + ylab("Quality of Government") + 
  geom_smooth(method="lm", aes(color="Linear")) + 
  geom_smooth(method="loess", aes(color="LOWESS")) +
  scale_color_manual(name="Smoother", values=c("blue","red")) + 
  theme_bw()

#'
#' # Boxplot
#'

# Single Boxplot
#+ fig.width=6, fig.height=4
Boxplot(~icrg_qog, data=d1)

# By Electoral System
table(d1$iaep_es)
table(d1$iaep_es)/sum(table(d1$iaep_es))
# 1 = Plurarity
# 2 = Majority
# 3 = Proportional Representation
# 4 = Mixed
#+ fig.width=6, fig.height=4
Boxplot(icrg_qog~iaep_es, data=d1, 
        xlab="Electoral System",
        ylab="Quality of Government")

# Transform Value Labels
#+ fig.width=6, fig.height=4
d1$eslab <- factor(d1$iaep_es,
                   levels = seq(1,4,1),
                   labels = c("Plurarity","Majority","PR","Mixed"))
#+ fig.width=6, fig.height=4
Boxplot(icrg_qog~eslab, data=d1, 
        xlab="Electoral System",
        ylab="Quality of Government")

# ggplot2
d1y <- na.omit(d1[,c("icrg_qog","eslab")])
#+ fig.width=6, fig.height=4
ggplot(d1y, aes(x=eslab, y=icrg_qog)) + geom_boxplot() + 
  xlab("GDP per Capita") + ylab("Quality of Government") + 
  theme_bw()

#'
#' # Transformation 
#'

# Logarithm Transformation
d1$loggdp <- log(d1$wdi_gdpcapcon2010)

# Compare Histograms
#+ fig.width=6, fig.height=4
with(d1, hist(wdi_gdpcapcon2010, breaks=10, col="gray"))
#+ fig.width=6, fig.height=4
with(d1, hist(loggdp, breaks="FD",col="gray"))

# Scatter Plot
#+ fig.width=6, fig.height=4, 
scatterplot(icrg_qog ~ loggdp, span=0.6,
            lwd=3, id.n=4, data=d1)

# Scatter Plot Matrix by QoG, GDP per capita, and Logged GDP per Capita
#+ fig.width=6, fig.height=6
scatterplotMatrix(~ icrg_qog + wdi_gdpcapcon2010 + loggdp, data=d1)

# ggplot2
#+ fig.width=6, fig.height=4
ggplot(d1, aes(x=loggdp, y=icrg_qog)) + geom_point() + 
  xlab("GDP per Capita") + ylab("Quality of Government") + 
  geom_smooth(method="lm", aes(color="Linear")) + 
  geom_smooth(method="loess", aes(color="LOWESS")) +
  scale_color_manual(name="Smoother", values=c("blue","red")) + 
  theme_bw()

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
#rmarkdown::render('TA_session_011619.R', 'github_document')
#rmarkdown::render('TA_session_011619.R', 'pdf_document')
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_011619.R', 'html_document')"
# Rscript -e "rmarkdown::render('TA_session_011619.R', 'pdf_document')"
