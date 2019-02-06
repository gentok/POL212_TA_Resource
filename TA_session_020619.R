#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "February 6, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required Package
library(foreign)

#'
#' # Practice of Analysis
#'
#' 1. Download 2016 General Social Survey Data in Stata Format 
#' from [HERE](http://gss.norc.org/get-the-data/stata). (See Individual Data Section)
#' 
#' 2. Open Data in R
#' 

# Set Data Location (Set on your own)
dataloc <- "D:/BoxSync/Data/GSS/2016/data/GSS2016.sav"

# Load Data
d <- read.spss(dataloc, to.data.frame=TRUE, 
               use.value.labels = FALSE, use.missings = FALSE)

#' 
#' 3. Check the codebook [HERE](http://www.thearda.com/archive/files/Codebooks/GSS2016_CB.asp).
#' Find following variables in data.
#'   * Preference between Public Access to Government Information and Public Security (govtinfo)
#'   * Safety in respondent's neighborhood
#'   * Respondent's age
#'   * Respondent's gender
#'   * Respondent's marital status
#'   * Number of children
#' 
#' Check the distribution by either table() or summary() function.
#'        

table(d$GOVTINFO)
table(d$NEISAFE)
table(d$AGE)
table(d$SEX)
table(d$MARITAL)
table(d$CHILDS)

#'
#' 4. Build a new data.frame with transformed variables. Transformation includes:
#'  * Drop Missing Values
#'  * Combine the categories with too few N
#'  * Define categorical variables as "factor" and set labels and reference category
#'  * Recode binary variables to have values 1 and 0.
#'

# Initiate Data.Frame with No Columns, but same number of rows as d
dnew <- d[,FALSE]


# Information Access Preference
?ifelse
dnew$GOVTINFO <- ifelse(d$GOVTINFO %in% c(-1,98,99),
                        NA,
                        d$GOVTINFO)
table(dnew$GOVTINFO)
# Neighborhood safety
dnew$NEISAFE <- ifelse(d$NEISAFE %in% c(8,9), 
                       NA, 
                       4-d$NEISAFE)
table(dnew$NEISAFE)
dnew$NEISAFE[d$NEISAFE==4] <- 1
dnew$NEISAFE <- factor(dnew$NEISAFE, 
                       labels=c("Unsafe","Sw Safe","Very Safe"))
table(dnew$NEISAFE)
# Age
dnew$AGE <- ifelse(d$AGE==99,NA,d$AGE)
table(dnew$AGE)
# Female
dnew$FEM <- d$SEX - 1
table(dnew$FEM)
# MARITAL STATUS
dnew$MARITAL <- ifelse(d$MARITAL==9,NA,d$MARITAL)
dnew$MARITAL <- factor(dnew$MARITAL,
                       labels=c("Married","Widowed","Divorced",
                                "Separated","Never Married"))
dnew$MARITAL <- relevel(dnew$MARITAL, ref="Never Married")
table(dnew$MARITAL)
# CHILDS
dnew$CHILDS <- ifelse(d$CHILDS==9,NA,d$CHILDS)
table(dnew$CHILDS)

#' 
#' 5. Run OLS regression to test the hypothesis that the more safe the neghborhood is, 
#' more strongly the respondent support for public access to government information. 
#' Include age, gender, marital status, number of children as control variables. Show result summary.
#' 

# Conventional Way
m <- lm(GOVTINFO ~ NEISAFE + AGE + FEM + MARITAL + CHILDS, data=dnew)
# All remaining variables in data as right-hand-side variables
m <- lm(GOVTINFO~., data=dnew)
# Show Summary
summary(m)

# APSR TABLE
#install.packages("apsrtable")
library(apsrtable)
apsrtable(m)

# I like Texreg
#install.packages("texreg")
library(texreg)
m1 <- lm(GOVTINFO ~ NEISAFE, data=dnew)
m2 <- lm(GOVTINFO ~ ., data=dnew)
screenreg(list(m1,m2)) # in R console
texreg(list(m1,m2)) # in tex

#'
#' 6. Interpret the result. What is the direct interpretation of coefficients? 
#' Is hypothesis supported or not? 
#' What are the potential issues in the analysis?
#'


#' 
#' 7. Try using your own data to run simple regression.
#'




#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
#rmarkdown::render('TA_session_020619.R', 'pdf_document')
#rmarkdown::render('TA_session_020619.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_020619.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_020619.R', 'pdf_document')"
