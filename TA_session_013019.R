#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "January 30, 2019"
#' ---

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required Package
library(ggplot2)

#'
#' # OLS Calculation in R
#' 
#' **Note that in HW, you need to show your work (by typing of hand-writing steps in calcuation), NOT R codes** 
#' 
#' 
#' ## Simple Example of Linear Regression

# Create Data
(d <- data.frame(y = c(2, 1, 4, 10, 8),
                x = c(0, 1, 2, 3,  4) ))
# Plot
(p <- ggplot(d, aes(x,y)) + geom_point(size=3))
 
# Linear Prediction 1 (alpha=-1, beta=3)
(d$yh1 <- (-1 + 3 * d$x))
(p <- p + geom_line(data=d,aes(x=x,y=yh1),size=2,color="red"))

# Linear Prediction 2 (alpha=1, beta=2)
(d$yh2 <- (1 + 2 * d$x))
(p <- p + geom_line(data=d,aes(x=x,y=yh2),size=2,color="blue"))

# Calculate The Sum of Squared Residuals (SSE)
# For yh1
(step1 <- d$yh1 - d$y)
(step2 <- step1^2)
(yh1sse <- sum(step2))
# For yh2
(step1 <- d$yh2 - d$y)
(step2 <- step1^2)
(yh2sse <- sum(step2))
# yh2 has smaller SSE than yh1
yh1sse > yh2sse

#'
#' # Verify OLS Estimates
#' 
#' Practice of Writing Functions
#'

# Calculate Beta Hat (Coefficient)
betah <- function(y,x){ # two arguments, y and x
  n <- length(y) # Number of Cases
  xi.xb <- rep(NA, n) # Xi - Xbar Placeholder
  yi.yb <- rep(NA, n) # yi - ybar Placeholder
  # Calculate xi-xbar and yi-ybar for each case
  for (i in 1:n){
    xi.xb[i] <- x[i] - mean(x)
    yi.yb[i] <- y[i] - mean(y)
  }
  # Calculate Beta Hat
  betah <- sum(xi.xb * yi.yb)/sum(xi.xb^2)
  return(betah)
}

# Calculate Alpha Hat (Intercept)
alphah <- function(y,x,betah) { # three arguments, y, x, betah 
  mean(y) - betah * mean(x)
}

# USE THE EXAMPLE DATA TO CALCULATE BETA HAT and ALPHA HAT
(b_est <- betah(y = d$y, x = d$x))
(a_est <- alphah(y = d$y, x = d$x, betah = b_est))

# Store the estimate in data & plot
yh <- function(y, x){
  b_est <- betah(y, x)
  a_est <- alphah(y, x, b_est)
  return(a_est + b_est * d$x)
}
(d$yhOLS <- yh(d$y, d$x))
(p <- p + geom_line(data=d,aes(x=x,y=yhOLS),size=2,color="green"))

# Residuals Standard Error 
residualSE <- function(y, x){
  df <- length(y) - 2 # Degrees of Freedom 
  cat(paste("Step 0: Degrees of Freedom (DF) = n-p-1 =", df))
  y_est <- yh(y, x)
  step1 <- y - y_est
  cat("Step 1: y - yhat:\n")
  cat(step1)
  cat("\n\n")
  step2 <- step1^2
  cat("Step 2: (y - yhat)^2:\n")
  cat(step2)
  cat("\n\n")
  step3 <- sum(step2)/df 
  cat(paste("Step 3: sum((y - yhat)^2)/(DF):\n", step3, "\n\n"))
  res <- sqrt(step3)
  cat(paste("End: sqrt(sum((y - yhat)^2)/(DF)):\n", res, "\n\n"))
  return(res)
}
residualSE(d$y, d$x)
# OR JUST
sqrt(sum((d$y - yh(d$y,d$x))^2)/(length(d$y)-2))

# Total Sum of Squares (TSS)
(TSS <- sum((d$y-mean(d$y))^2))

# Residual Sum of Squares(RSS)
(RSS <- sum((d$y-yh(d$y, d$x))^2))

# Regression Sum of Squares (RegSS)
# (Also called Explained Sum of Squares ESS)
(RegSS <- sum((yh(d$y, d$x) - mean(d$y))^2))

# Confirm that RegSS + RSS = TSS
RegSS + RSS 
TSS

# Rsquared 
RegSS/TSS
1 - RSS/TSS

# You can also write a function
Rsq <- function(y, x){
  TSS <- sum((y-mean(y))^2)
  RSS <- sum((y-yh(y, x))^2)
  RegSS <- sum((yh(y, x) - mean(y))^2)
  Rsq <- RegSS/TSS
  res <- c(TSS,RSS,RegSS,Rsq)
  names(res) <- c("TSS", "RSS","RegSS|ESS","R squared")
  return(res)
}
Rsq(d$y, d$x)

# Summary Function
sum_est <- function(y, x){
  b_est <- betah(y, x)
  a_est <- alphah(y, x, b_est)
  residualSE <- sqrt(sum((y - yh(y,x))^2)/(length(y)-2))
  coefs <- c(a_est,b_est,residualSE)
  names(coefs) <- c("Intercept","Coefficient", "Residual SE")
  rsqs <- Rsq(y, x)
  res <- list(coefs, rsqs)
  names(res) <- c("Estiamtes", "Goodness of Fit")
  return(res)
}
sum_est(d$y, d$x)

# Check by R's Default Linear Regression Function
summary(lm(y~x, data=d))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
#rmarkdown::render('TA_session_013019.R', 'pdf_document')
#rmarkdown::render('TA_session_013019.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_011619.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_011619.R', 'pdf_document')"
