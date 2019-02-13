#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "February 13, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required Package
library(readstata13)
library(haven)

#'
#' # Practice of Analysis
#'
#' 1. Download **BRITISH GENERAL ELECTION CONSTITUENCY RESULTS 2010-2017, V1.2**
#' from [HERE](https://www.pippanorris.com/data/). (Choose Any Version You Want)
#' 
#' 2. Open Data in R
#' 

# I chose stata version

# Set Data Location (Set on your own)
dataloc <- "D:/BoxSync/Data/UK_votes/data/UK GE 2010_2015_2017 V1_2 (+Brexit vote).dta"

# Load Data
d <- read.dta13(dataloc, convert.factors = FALSE)
# OR
d <- read_dta(dataloc)

#' 
#' 3. Check the codebook (available at the same website).
#' Find following variables in data.
#'   * % of Brexit "Leave" Vote in the District (2016)
#'   * Vote Share of Liberal Democratic Party (2015)
#'   * Proportion of Female (2011)
#'   * Proportion of Whites who are British (2011)
#' 
#' Check the distribution by summary() function.
#'        

summary(d$BREXITLeave)
summary(d$LD15)
summary(d$c11Female)
summary(d$c11EthnicityWhiteBritish)

#' 4. Create a new data.frame including all the above variables. 
#' Also, add following Variable by transoforming relevant variables in the original data.
#' 
#' * Proportion of Younger than Age 30 in the Population of 
#' Age 18 or Older (2011) 
#'

# Initiate Data.Frame with No Columns, but same number of rows as d
dnew <- d[,FALSE]
dnew$leave <- d$BREXITLeave
dnew$LD15 <- d$LD15
dnew$pFEM <- d$c11Female
dnew$pWB <- d$c11EthnicityWhiteBritish
pU18 <- d$c11Age0to4 + d$c11Age10to14 + d$c11Age15 + 
  d$c11Age16to17
p1830 <- d$c11Age18to19 + d$c11Age20to24 + d$c11Age25to29
dnew$pU30 <- (p1830 / (100 - pU18))*100
summary(dnew$pU30)

dim(dnew)

#' 
#' 5. Compare means of Brexit vote proportion by Following two groups
#' * Proportion of Age <30 is larger than 20%
#' * Proportion of Age <30 is 20% or lower
#' Interpretation?
#' 

with(dnew, t.test(leave[pU30>20],leave[pU30<=20]))

#' 
#' 6. Run OLS regression to test the hypothesis that larger the proportion of voters 
#' who are age 30 or younger, lower the proporition of Brexit votes. Run two models:
#' 
#' * Bivariate regression with only one variable
#' * Multiple Regression with control variables
#' 
#' Interpreations?
#' 

# Conventional Way
m1 <- lm(leave ~ pU30, data=dnew)
m2 <- lm(leave ~ pU30 + LD15 + pFEM + pWB, data=dnew)
# Show Summary
summary(m1)
summary(m2)


# You can also compare with NULL Model
m0 <- lm(leave ~ 1, data=dnew)
var.test(m0,m1)



library(texreg)
screenreg(list(m1,m2)) # in R console

#'
#' 7. Now Test conditional hypothesis. For Example
#' 
#' * The effect of Age <30 Proportion of "Leave" Proportion is stronger
#' for districts with lower proportion of British Whites. 
#' 
#' Run OLS Regression. Interpretation?
#'

# Conventional Way
m3 <- lm(leave ~ pU30*pWB + LD15 + pFEM, data=dnew)
# Dichotomous Group (British Whites Proportion 80% or Smaller)
dnew$pWB80 <- (dnew$pWB<80)*1
m4 <- lm(leave ~ pU30*pWB80 + LD15 + pFEM, data=dnew)

# Show Summary
summary(m3)
summary(m4)

screenreg(list(m1,m2,m3,m4)) # in R console

#' 
#' 8. Visualize!
#'

# Conditional Prediction
library(sjPlot)
plot_model(m3, type = "pred", terms = c("pU30", "pWB")) # Not Good
plot_model(m4, type = "pred", terms = c("pU30", "pWB80")) # OK
m2.5 <- lm(leave ~ pU30 + LD15 + pFEM + pWB80, data=dnew)
plot_model(m2.5, type = "pred", terms = c("pU30", "pWB80")) # Check if it's not interacted

# Better Way to Plot Interaction in m3 (Conditional Coefficients)
library(interplot)
interplot(m3,"pU30","pWB") + 
  ylab("pU30 Coefficients") + xlab("British Whites Proportion")

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
#rmarkdown::render('TA_session_021319.R', 'pdf_document')
#rmarkdown::render('TA_session_021319.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_021319.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_021319.R', 'pdf_document')"
