#' ---
#' title: "POL212 TA Session"
#' author: "Gento Kato"
#' date: "March 6, 2019"
#' ---

#'
#' # Preparation
#'

## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Required Packages
library(ggplot2)
library(gridExtra) # For Advanced Plotting

#'
#' # Data visualization (Using Midterm as an Example)
#'

#' ## Load Data
dloc <- "midtermdata.csv"
d <- read.csv(dloc, stringsAsFactors = FALSE)

#' ## Dependent Variable Distribution

# Draw Histogram
dvdist <- 
  ggplot(d, aes(politicalinterest)) + 
  geom_histogram(fill = "white", # color to fill bars
                 color = "black", # color of lines
                 binwidth=1 # width of bars
                 ) +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + # center the title
  labs(x=NULL, # No xaxis label 
       y="Count",  
       title="Histogram of Political Interest") 

#+ fig.width=6, fig.height=4
dvdist

#+ Save Histogram (As It Is in Plot Window)
ggsave("dvdist.png", dvdist, w=6, h=4)
# w is width, h is height

#' ## Independent Variables

# Age
ivdist1 <- 
  ggplot(d,aes(age)) + 
  geom_histogram(fill="white",color="black",binwidth=5) + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + 
  labs(x=NULL,y=NULL,title="Age")

# Education
# Define Levels of Factor
d$education <- factor(d$education,
                      levels=c("No HS","HS",
                               "Some College","College +"))
ivdist2 <- 
  ggplot(d,aes(education)) + 
  geom_bar(fill="white",color="black") + # Bar Graph (Since Categorical)
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + 
  labs(x=NULL,y=NULL,title="Education")

# Income
ivdist3 <- 
  ggplot(d,aes(income)) + 
  geom_histogram(fill="white",color="black",binwidth=20) + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + 
  labs(x=NULL,y=NULL,title="Income")

# Female

# Create New Variable for Plotting
d$fem_fac <- ifelse(d$female==1,"Female","Male")
d$fem_fac <- factor(d$fem_fac,levels=c("Male","Female"))

ivdist4 <- 
  ggplot(d,aes(fem_fac)) + 
  geom_bar(fill="white",color="black") + # Bar Graph (Since Categorical)
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + 
  labs(x=NULL,y=NULL,title="Gender")

# Treatment

# Create New Variable for Plotting
d$treat_fac <- ifelse(d$treatment==1,"Treated","Control")
d$treat_fac <- factor(d$treat_fac,levels=c("Control","Treated"))

ivdist5 <- 
  ggplot(d,aes(treat_fac)) + 
  geom_bar(fill="white",color="black") + # Bar Graph (Since Categorical)
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + 
  labs(x=NULL,y=NULL,title="Treatment")


# Combine Plots
ivdist <- arrangeGrob(ivdist1,ivdist2,ivdist3,
                      ivdist4,ivdist5,ncol=3)
grid.arrange(ivdist)

# More Advanced
ivdist <- arrangeGrob(ivdist1,ivdist2,ivdist3,ivdist4,ivdist5,
             layout_matrix = rbind(c(1,1,1,2,2,2),  # Plot Only Two Panels in 1st Row
                                   c(3,3,4,4,5,5)), # Plot Three Panels in 2nd Row
             top = textGrob("Distributions of Independent Variables", # Title
                            gp=gpar(fontface="bold")), # Use Bold Font
             left = "Count"
             ) 
#+ fig.width=7, fig.height=4
grid.arrange(ivdist) # Use grid.arrange to plot arrangeGrob

# Save
ggsave("ivdist.png", ivdist, w=7, h=4)

#' ## Transform Variables

# Numeric Education
d$edunum <- as.numeric(d$education)

# Logged Income
d$loginc <- log(d$income)
hist(d$loginc)

#' ## Run OLS

# Without Interaction
m <- lm(politicalinterest ~ treatment + female + age + education + loginc,
        data = d)
# With Numeric Education
mn <- lm(politicalinterest ~ treatment + female + age + edunum + loginc,
        data = d)
# Interacted with Female
mi <- lm(politicalinterest ~ treatment*female + age + education + loginc,
        data = d)

# Show Results Temporarily
summary(m)
summary(mn)
summary(mi)

#' ## Visualize OLS Estimates

# 1st: APSR Table

library(apsrtable)
# Start
apsrtable(m,mn,mi)
# More Advanced (Save File)
cn <- c("(Intercept)","Treatment",
        "Gender (Female)", "Age",
        "Education (High School)",
        "Education (Some College)",
        "Education (College/More)",
        "Income (Log)",
        "Education (4p Scale)",
        "Treatment * Female")
tabout1 <- 
  apsrtable(m,mn,mi,
            coef.names = cn, # Custom Coefficient Names
            digits = 3, # Number of Digits in Output
            coef.rows = 1, # Single Row for Coefficient (Can also be 2)
            Sweave = TRUE # Only Tabular
            ) # Appear in LaTex File
# Save File 
cat(tabout1, file="apsrtable_out.tex")

# 2nd: texreg

library(texreg)

# Default
texreg(list(m,mn,mi))

# Advanced
texreg(list(m,mn,mi),
      file = "texreg_out.tex", # Write in specified file
      custom.coef.names = cn, # Change Coefficient Names
      reorder.coef = c(1,2,3,10,4,5,6,7,9,8), # Change Orde of Coefs 
      single.row = TRUE, # Present coefficients in single row
      booktabs = TRUE, use.packages=FALSE, # Advanced formatting
      table = FALSE # Just present tabular
      )

# Word Format
htmlreg(list(m,mn,mi),
       file = "texreg_out.doc", # Write in specified file
       custom.coef.names = cn, # Change Coefficient Names
       reorder.coef = c(1,2,3,10,4,5,6,7,9,8), # Change Orde of Coefs 
       single.row = TRUE, # Present coefficients in single row
       caption = "OLS Estimates",
       caption.above = TRUE # Put caption above the table
       )

# 3rd: coefficient plot

library(lmtest)
cpd1 <- as.data.frame(cbind(m$coefficients,coefci(m)))
cpd2 <- as.data.frame(cbind(mn$coefficients,coefci(mn)))
cpd3 <- as.data.frame(cbind(mi$coefficients,coefci(mi)))
cpd1$cn <- cn[-c(9,10)]
cpd2$cn <- cn[-c(5,6,7,10)]
cpd3$cn <- cn[-9]
cpd1$mname <- "Model 1"
cpd2$mname <- "Model 2"
cpd3$mname <- "Model 3"
cpd <- rbind(cpd1,cpd2,cpd3)
names(cpd) <- c("cf","lCI","uCI","cn","mname")
# Rownames as Coefficient Names (Order reversed for later purpose)
cpd$cn <- factor(cpd$cn,
                 levels=rev(unique(cpd$cn)))
cp <- ggplot(cpd, aes(x=cn,y=cf)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_point() + 
  geom_errorbar(aes(ymin=lCI,ymax=uCI), width=0.1)+ 
  facet_grid(.~mname) + 
  coord_flip() + theme_bw() + 
  theme(plot.title=element_text(hjust=0.5)) + 
  labs(title="Coefficient Plots",
       x = "Coefficients with 95% CI", y = NULL)

#+ fig.width=7, fig.height=4
cp

# Save
ggsave("cp.png", cp, w=7, h=4)

#' ## Interaction

prof <- data.frame(treatment=c(0,1,0,1),
                   female = c(0,0,1,1),
                   education = "College +",
                   age = median(d$age),
                   loginc = median(d$loginc)
                   )
prof

pred <- predict(mi, newdata=prof, 
        se.fit = TRUE)
pred

prd <- data.frame(est = pred$fit,
                  lCI = pred$fit - 1.96*pred$se.fit,
                  uCI = pred$fit + 1.96*pred$se.fit)
prd$gender <- factor(c("Male","Male","Female","Female"),
                     levels=c("Male","Female"))
prd$treat <- factor(c("Control","Treated","Control","Treated"),
                    levels=c("Control","Treated"))

pri <- ggplot(prd, aes(x=treat, y=est)) + 
  geom_point(aes(color=treat), 
             position=position_dodge(width=0.5), # Jittering location of points
             size = 2 # Size of point 
             ) + 
  geom_errorbar(aes(ymin=lCI,ymax=uCI, color=treat), 
                width=0.1, # wdith of horizontal line
                size = 0.8, # thickness of line
                position=position_dodge(width=0.5))+ 
  facet_grid(.~gender, scales="free_x") + # Split panels by gender
  scale_color_manual(name="Treatment",values=c(1,1)) + # Both Black Lines
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none", # Do NOT SHOW LEGEND
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12, face="bold"),
        strip.text = element_text(size=12, face="bold")) + 
  labs(title="Interaction Effect",
       x = "Treatment Status", y = "Predicted Political Interest with 95%CI")

#+ fig.width=6, fig.height=4
pri

# Save
ggsave("pri.png", pri, w=7, h=4)

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
rmarkdown::render('TA_session_030619.R', 'pdf_document')
rmarkdown::render('TA_session_030619.R', 'github_document', clean=FALSE)
# In Terminal, run:
# Rscript -e "rmarkdown::render('TA_session_030619.R', 'github_document')"
# Rscript -e "rmarkdown::render('TA_session_030619.R', 'pdf_document')"
