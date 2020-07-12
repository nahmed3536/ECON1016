# Gregory Bruich, Ph.D.
# Economics 1016, Harvard University
# Send corrections and suggestions to gbruich@fas.harvard.edu

#Thanks to Jose Ramon Morales Arilla for comments and suggestions.

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

# Let's install a number of useful packages.
# To make things easy, the following snippet of code will download
# and install everything you'll need.
# But for future reference, remember that to install a package
# you only have to type
# > install.packages("<packagename>")
# And then you can load it with
# > library(lib)

packages <- c("haven"
              ,"ggplot2"
              ,"sandwich"
              ,"lmtest")

not_installed <- !packages %in% installed.packages()
if (any(not_installed)) install.packages(packages[not_installed])
lapply(packages,require,character.only=TRUE)
# Now all packages should be installed and loaded!

#Set working directory to be location of data
#Session -> set working directory -> choose working directory

#read in data
#File import data set -> from stata -> browse 
cig <- read_dta("cig_taxes.dta")


#-------------------------------------------------------------------------------
# Data set up
#-------------------------------------------------------------------------------

#Data prep
#Define log cigarette consumption
cig$log_packs_pc <- log(cig$pack_sales)

#Define Arizona indicator
cig$az <- 0
cig$az[which(cig$state=="AZ")] <- 1
summary(cig$az)



#-------------------------------------------------------------------------------
# Graphical analyes
#-------------------------------------------------------------------------------


#Subset data to particular time range
cig_narrow <- subset(cig, year>=1987 & year <= 2000)

#Replicate figure 1
ggplot(cig_narrow, aes(x=year,y=log_packs_pc, 
                       shape= factor(az, labels = c("Rest of U.S.", "Arizona")))) +
  geom_vline(xintercept=1993.5) +
  stat_summary(fun.y = "mean",geom="point") +
  stat_summary(fun.y = "mean",geom="line")  +
  labs(x = "Year", y = "Log Cigarette Consumption", shape = "") +
  theme(legend.position="bottom")
ggsave("plot1.png")


#Compare Arizona to only neighboring states
#California, Nevada, Utah, New Mexico, and Colorado
cig_narrow <- subset(cig_narrow, state == "AZ" 
                     | state == "CA" 
                     | state == "NV" 
                     | state == "UT" 
                     | state == "NM" 
                     | state == "CO")


#Replicate figure 2
ggplot(cig_narrow, 
       aes(x=year,y=log_packs_pc, 
           shape = factor(az, labels = c("Surrounding States", "Arizona")))) +
  geom_vline(xintercept=1993.5) +
  stat_summary(fun.y = "mean",geom="point") +
  stat_summary(fun.y = "mean",geom="line")  +
  labs(x = "Year", y = "Log Cigarette Consumption", shape = "") +
  theme(legend.position="bottom")
ggsave("plot2.png")


#-------------------------------------------------------------------------------
# Regression analysis to quantify what we see in the graphs
#-------------------------------------------------------------------------------


#Regression example to modify

#These regressions are run over the neighboring states
#California, Nevada, Utah, New Mexico, and Colorado

#Generate post 1994 indicator variable
cig_narrow$post <- 0
cig_narrow$post[which(cig_narrow$year >= 1994)] <- 1

#Generate interation term
cig_narrow$dd <- cig_narrow$az*cig_narrow$post

#Subset to years you want
cig_narrow2 <- subset(cig_narrow, year>=1993 & year <= 1995)

#Estimate simple differences in differences
reg1 <- lm(log_packs_pc ~ post + az + dd, data=cig_narrow2)

#Report heteroskedasticity robust standard errors
coeftest(reg1, vcov = vcovHC(reg1, type="HC1"))

#Estimate difference in difference regression extended to multiple control states and many years
reg2 <- lm(log_packs_pc ~ dd + factor(year) + factor(state_fips), 
           data=cig_narrow)
coeftest(reg2, vcov = vcovHC(reg2, type="HC1"))




#-------------------------------------------------------------------------------
# Plot coefficients from non-parametric regression
#-------------------------------------------------------------------------------

cig_final <- subset(cig, year >= 1987 & year <= 2000)

#Estimate regression 3
reg3 <- lm(log_packs_pc ~ factor(year):az + az + factor(year) + factor(state_fips), 
           data=cig_final)

#Clustered standard errors
coeftest(reg3, vcov = vcovCL, cluster = ~ state_fips)


#Create data frame to graph coefficients
years <- 1988:2000

#List the coefficients. We want the ones starting with factor(year)1988:az
reg3$coef

#Store the 66th through 78th coeffiicents
beta <- reg3$coef[66:78]

#Store the standard errors for these as well
se1 <-  sqrt(diag(vcovCL(reg3, cluster = ~ state_fips)))
se <- se1[66:78]

#Create a data frame that combines them
dfgraph = data.frame(years, beta, se)  

#Add 1987 as 0 because it is the excluded year
years <- 1987
beta <- 0
se <- 0
df1987 = data.frame(years, beta, se) 

#Create final data frame to graph with the standard errors
forgraph <- rbind(dfgraph,df1987)
forgraph$ub <- forgraph$beta + (1.96*forgraph$se)
forgraph$lb <- forgraph$beta - (1.96*forgraph$se)

##Draw graph
ggplot(data=forgraph, aes(x=years)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1, color="red") +
  geom_point(aes(y=beta)) + 
  geom_line(aes(y=beta)) +
  labs(title = "",
       y = "Coefficient and 95% CI",
       x = "Year") + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=1993.5) 

#Save graph
ggsave("plot3.png")

