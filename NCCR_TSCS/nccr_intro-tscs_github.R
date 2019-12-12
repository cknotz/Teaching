################################################
################################################
# Example analysis for NCCR TSCS Analysis course
################################################
################################################

# Carlo Knotz
# IDHEAP

# setwd("") <- adjust this

# Credit where credit is due: https://docplayer.net/39110203-Intro-to-analyzing-cross-sectional-time-series-data-in-r-for-students-of-ir-comparative-politics-justin-murphy-november-21-2014.html

library("ggplot2")
library("plm")
library("readstata13")
library("countrycode")
library("readxl")
library("texreg")
library("pcse")
library("dplyr")
library("tseries")
library("xtable")
library("car")

####################
# SIMULATED EXAMPLES
####################

addTaskCallback(function(...) {set.seed(14);TRUE}) # <- this makes the results reproducible

# Well-behaved data

# Simulate data
b <- 50 # coefficient
    a <- 500 # intercept
    n <- 100 # observations

X <- runif(n, 0, 120) # Create a sample of (fixed!) observations on variable X.

Y <- a + b*X + rnorm(n,0,200) # true Data Generating Process

dat1 <- data.frame(X,Y)
    dat1$panel <- "Panel 1"
    dat1$time <- seq(1,10)

X <- runif(n, 0, 75) # Create a sample of (fixed!) observations on variable X, but with greater variation

Y <- a + b*X + rnorm(n,0,200) # true Data Generating Process

dat2 <- data.frame(X,Y)
    dat2$panel <- "Panel 2"
    dat2$time <- seq(1,10)

dat <- rbind(dat1,dat2)
rm(dat1,dat2)

plot1 <- ggplot(data=dat,aes(x=X,y=Y,colour=factor(panel))) +
    geom_point(shape=3,size=2,) +
    geom_smooth(method="lm", se=F,color="black",linetype="dashed") +
    expand_limits(y=0) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
          panel.border = element_rect(fill=NA,size=1,linetype="solid"),
          plot.margin = unit(c(.1,.1,.1,.1), "cm"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    xlab("X") + 
    ylab("Y")

plot1

rm(list=ls()) # <- clearing all 

    
# Problem with pooled model: different intercepts    
    
# Simulate data
b <- 50 # coefficient
    a <- 500 # intercept
    n <- 100 # observations
    
X <- runif(n, 0, 100) # Create a sample of (fixed!) observations on variable X.
    
Y <- a + b*X + rnorm(n,0,50) # true Data Generating Process
    
dat1 <- data.frame(X,Y)
    dat1$panel <- "Panel 1"
    dat1$time <- seq(1,10)
    
X <- runif(n, 0, 100) # Create a sample of (fixed!) observations on variable X, but with greater variation
    
Y <- a + b*X + rnorm(n,0,50) # true Data Generating Process
    
    X <- X+100 # <- we move the data a bit
    
dat2 <- data.frame(X,Y)
    dat2$panel <- "Panel 2"
    dat2$time <- seq(1,10)
    
dat <- rbind(dat1,dat2)
    rm(dat1,dat2)
    
plot2 <- ggplot(data=dat,aes(x=X,y=Y,colour=factor(panel))) +
        geom_point(shape=3,size=1,) +
        geom_smooth(method="lm", se=F,color="black",linetype="dashed") +
        expand_limits(y=0) + 
        theme_bw() + 
        theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
              panel.border = element_rect(fill=NA,size=1,linetype="solid"),
              plot.margin = unit(c(.1,.1,.1,.1), "cm"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")) +
        xlab("X") + 
        ylab("Y")  
plot2

  
# Negative relationship 
    
b <- 5 # coefficient
    a1 <- 500 # intercept
    a2 <- 1500 # second intercept
    n <- 100 # observations
    
X <- runif(n, 0, 100) # Create a sample of (fixed!) observations on variable X.
    
Y <- a1 - b*X + rnorm(n,0,50) # true Data Generating Process
    
dat1 <- data.frame(X,Y)
    dat1$panel <- "Panel 1"
    dat1$time <- seq(1,10)
    
X <- runif(n, 0, 100) # Create a sample of (fixed!) observations on variable X, but with greater variation
    
Y <- a2 - b*X + rnorm(n,0,50) # true Data Generating Process
    
X <- X+500 # <- we move the data a bit
    
dat2 <- data.frame(X,Y)
    dat2$panel <- "Panel 2"
    dat2$time <- seq(1,10)
    
dat <- rbind(dat1,dat2)
    rm(dat1,dat2)
    
plot3 <- ggplot(data=dat,aes(x=X,y=Y,colour=factor(panel))) +
        geom_point(shape=3,size=1,) +
        geom_smooth(method="lm", se=F,color="black",linetype="dashed") +
        expand_limits(y=0) + 
        theme_bw() + 
        theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
              panel.border = element_rect(fill=NA,size=1,linetype="solid"),
              plot.margin = unit(c(.1,.1,.1,.1), "cm"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")) +
        xlab("X") + 
        ylab("Y")  
plot3

    rm(list=ls()) # <- clearing all 
    
    
# Fixed-effects model
    
b <- 10 # coefficient
    a <- 500 # intercept
    n <- 100 # observations
    
X <- runif(n, 0, 500) # Create a sample of (fixed!) observations on variable X.
    
Y <- a + b*X + rnorm(n,0,500) # true Data Generating Process
    
dat1 <- data.frame(X,Y)
    dat1$panel <- "Panel 1"
    dat1$time <- seq(1,10)
    
    X <- runif(n, 0, 500) # Create a sample of (fixed!) observations on variable X, but with greater variation
    
    Y <- a + b*X + rnorm(n,0,500) # true Data Generating Process
    
    X <- X+500 # <- we move the data a bit
    
dat2 <- data.frame(X,Y)
    dat2$panel <- "Panel 2"
    dat2$time <- seq(1,10)
    
dat <- rbind(dat1,dat2)
    rm(dat1,dat2)
    
plot4 <- ggplot(data=dat,aes(x=X,y=Y,colour=factor(panel))) +
        geom_point(shape=3,size=1) +
        geom_smooth(method="lm", se=F, linetype="dashed",aes(group=factor(panel),colour=(factor(panel)))) +
        expand_limits(y=0) + 
        theme_bw() + 
        theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
              panel.border = element_rect(fill=NA,size=1,linetype="solid"),
              plot.margin = unit(c(.1,.1,.1,.1), "cm"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")) +
        xlab("X") + 
        ylab("Y")  
plot4
    
    
# Stationary (autocorrelated) time series
library(forecast)
    
stat <- arima.sim(list(order = c(1,0,0), ar = 0.75),n=1000,rand.gen = rnorm)
    
# Non-stationary/random-walk time series
int <- cumsum(rnorm(n=1000, mean=0)) 
    
ts.plot(int, gpars = list(ylab="Value"))
  abline(h=0, lty="dashed", col="red")

ts.plot(stat, gpars = list(ylab="Value"))
  abline(h=0, lty="dashed", col="red")

detach(package:forecast, unload=T)
  rm(list=ls())
  
# EXAMPLE ANALYSIS
##################
    
# Data management
#################

# Importing migration policy data from IMPIC (Political Rights)

data <- read.csv("impic2016.csv")  
    data <-data[c("cntry","year","AvgS_ImmPol")] # <- removing clutter
    
# Using countrycode to produce full country names
data$cntry <- toupper(data$cntry)
    data$country <- countrycode(data$cntry,"iso2c","country.name")

# Quick and dirty solution to problems with unclear (nf ed cf) or misclassified (er) units: deletion
data <- subset(data,cntry!="ED" & cntry!="ER")
    unique(data$cntry)
    unique(data$country)

# Average immigrant rights <- our main indicator!
data$impol <-1-data$AvgS_ImmPol # reverse scale of variable
    data <- data[c("cntry","country","year","impol")] ## removing clutter

# Descriptives
##############
    
# By-country line graph showing developments within countries
within <- ggplot(data,aes(x=year,y=impol)) +
    geom_line(aes(group=country)) + 
    facet_wrap(~ country) +
    scale_x_continuous(name="Year",limits=c(1980,2010)) +
    scale_y_continuous(name="Immigration policy",limits=c(0,1)) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),panel.spacing = unit(1, "lines"))
within
    rm(within)
    

# Line graph showing development of voting rights across countries over time
mean <- aggregate(data$impol,by=list(data$year),FUN=mean,na.rm=T) # <- first we create a new dataset of annual averages

avrights <- ggplot(mean,aes(x=Group.1)) + # this creates the graph's foundation
    geom_line(aes(y=x)) + # this adds a line for our main indicator
    theme(panel.background = element_rect(fill = "white", colour = "grey50")) + # this renders the graph in simple b/w
    scale_x_continuous(name="Year",limits=c(1980,2010)) + # <-this labels and defines the range of the x-axis
    scale_y_continuous(name="Immigration policy",limits=c(0,1)) # same for the y-axis
avrights
  rm(avrights,mean) # <- removing clutter


# Bar graph showing cross-country differences in voting rights
mean <- aggregate(data$impol, by=list(data$cntry),FUN=mean,na.rm=T)
mean$Group.1 <- factor(mean$Group.1, levels = mean$Group.1[order(mean$x)])
# this sorts the countries according to the strictness of their rights

avrights <- ggplot(mean, aes(x=Group.1,y=x)) +
    geom_bar(stat="identity") +
    theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
    xlab("Country") + ylab("Immigration policy") + ylim(0,1)
avrights
  rm(avrights)


# Bivariate analysis
####################

# Reading in data from Armingeon et al.
cpds<-read.dta13("cpds19.dta",nonint.factors=T,generate.factors = T)
    
# Selecting relevant variables
cpds <- cpds[c("year","country","countryn","iso","gov_left1","sstran","socexp_t_pmp","eu","poco")]

# Merging the two datasets
data <- merge(cpds,data,by=c("country","year"))
    data$socx <-data$socexp_t_pmp
    data$socexp_t_pmp <- NULL
    rm(cpds) # removing more clutter
    data$socx <- data$socx/100 # <- to align scales of DV and IV
    
# -> Hypothesis 1: More generous welfare states associated with more generous voting rights

# Scatterplot
scatter <- aggregate(data$impol,by=list(data$country),FUN=mean,na.rm=T)
    scatter2 <- aggregate(data$socx,by=list(data$country),FUN=mean,na.rm=T)
    scatter2$socx <- scatter2$x 
    scatter2$x <- NULL
    scatter <- merge(scatter,scatter2,by=c("Group.1"))
    rm(scatter2)
    
scatterplot <- ggplot(scatter, aes(x=socx, y=x)) +
    geom_point(size=2,shape=1) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
    ylab("Immigration policy") + xlab("Social security spending (%GDP)") +
    geom_text(aes(label=Group.1),,hjust=0, vjust=0,size=3) +
    geom_smooth(method="lm", se=F,colour="red",linetype="dashed",size=.5) +
    xlim(.15,.30)
scatterplot
    rm(scatterplot)
    
# Regressions
#############

# Indexing data to let R understand the structure of the data <- IMPORTANT!    
datain <- pdata.frame(data, index=c("country","year"), drop.index=TRUE, row.names=TRUE)
    
# Between-effects regression to confirm pattern
lm <- lm(x~socx, data=scatter)
summary(lm)

be <- plm(impol ~ socx, data = datain, model="between",index = c("state","year"))
summary(be)

# Summary
screenreg(list(lm,be))

# Robustness
be <- plm(impol ~ socx, data = datain, model="between",index = c("state","year"))
summary(be)

be1 <- plm(impol ~ socx + eu, data = datain, model="between",index = c("state","year"))
summary(be1)

be2 <- plm(impol ~ socx + gov_left1, data = datain, model="between",index = c("state","year"))
summary(be2)

# Summary
screenreg(list(be,be1,be2),stars=c(.05,.1))

rm(be,be1,be2,scatter,lm)
    rm(datain)

# More data cleaning % prep
###########################

# Creating lagged DV:
lag <- function(x)c(NA, x[1:(length(x)-1)])
   data$l_impol <- unlist(tapply(data$impol, data$country, lag))
   
# We check if dataset is balanced (it is not, as could be seen from the graphical analysis above)
is.pbalanced(data) # <- it is not!
   
# We make it balanced to avoid estimation issues (this you might want to carefully think about in your own analysis!)
data_bal <- make.pbalanced(data,balance.type = "shared.times",index = c("country","year"))
    is.pconsecutive(data_bal)
    
# Dropping missings
data_bal <- data_bal[data_bal$country!="Estonia" & data_bal$country!="Hungary" & data_bal$country!="Slovakia",]


# Testing for non-stationarity
##############################
    
datain <- pdata.frame(data_bal,index = c("country","year"))
    
purtest(datain$impol,test = "hadri", exo = c("trend"),
        lags="AIC", pmax = 5)    

purtest(datain$socx,test = "hadri", exo = c("trend"),
        lags="AIC", pmax = 5)

   
# Both tests indicate that our main variables are likely non-stationary.

#   Technically, we should now either difference the variables (and see if this solves the issue)
#   or use models for non-stationary panel data (Birkel 2014)

# We proceed with the non-transformed data just for the purpose of illustration!
    
# Pooled regression models
##########################

pool1 <- lm(impol~l_impol + socx, data=data_bal)
summary(pool1)

pool1.pcse <- pcse(pool1,groupN=data_bal$countryn, groupT=data_bal$year)
summary(pool1.pcse)

pool2 <- lm(impol~l_impol + socx + eu + gov_left1, data=data_bal)

pool2.pcse <- pcse(pool2,groupN=data_bal$countryn, groupT=data_bal$year)
summary(pool2.pcse)   


# What we are in essence estimating
pooled <- ggplot(data_bal, aes(x=socx, y=impol)) +
    geom_point(size=2,shape=1) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
    ylab("Immigration policy") + xlab("Social security spending (%GDP)") +
    geom_smooth(method="lm", se=F,colour="red",linetype="dashed",size=.5) +
    xlim(.15,.30)

pooled
    rm(pooled)

rm(pool1,pool1.pcse,pool2,pool2.pcse,results)



# Fixed-effects models
######################

# Graphical
means <- aggregate(data_bal$impol,by=list(data_bal$country),FUN=mean, na.rm=TRUE)
    means$mean <- means$x
    means$x <- NULL
    means$country <- means$Group.1
    means$Group.1 <- NULL
    data_bal <- merge(data_bal,means,by=("country"))
    rm(means)
    
data_bal$fediff <- data_bal$impol-data_bal$mean # <- this is the difference between each observation & the country mean

fixed <- ggplot(data_bal,aes(x=year,y=fediff)) +
    geom_line(aes(group=country)) + 
    facet_wrap(~ country) +
    scale_x_continuous(name="Year",limits=c(1980,2010)) +
    scale_y_continuous(name="Immigration policy (difference from country mean)") +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),panel.spacing = unit(1, "lines"))
fixed
    rm(fixed)

# Models
fe1 <- lm(impol~l_impol + socx + factor(countryn), data=data_bal)
summary(fe1)

fe1.pcse <- pcse(fe1,groupN=data_bal$countryn, groupT=data_bal$year)
summary(fe1.pcse)

fe2 <- lm(impol~l_impol + socx + eu + gov_left1 + as.factor(countryn), data=data_bal)
summary(fe2)

fe2.pcse <- pcse(fe2,groupN=data_bal$countryn, groupT=data_bal$year)
summary(fe2.pcse)

# We could also do two-way fixed effects, but this model is very demanding!
fe3 <- lm(impol~l_impol + socx + eu + gov_left1 + as.factor(countryn) + as.factor(year), data=data_bal)
summary(fe3)

fe3.pcse <- pcse(fe3,groupN=data_bal$countryn, groupT=data_bal$year)
summary(fe3.pcse)

rm(fe1,fe1.pcse,fe2,fe2.pcse,fe3,fe3.pcse) 

# First-difference models
#########################

# Compute first differences
diff <- data %>% 
    group_by(data$country) %>%
    mutate(d_impol=impol-lag(impol)) %>%
    mutate(d_socx=socx-lag(socx)) %>%
    mutate(d_eu=eu-lag(eu)) %>%
    mutate(d_gov_left1=gov_left1-lag(gov_left1)) %>%
    ungroup

diff <- diff[c("country","countryn","year","d_impol","d_socx","d_eu","d_gov_left1")]

# Illustrative graph
fd <- ggplot(diff,aes(x=year,y=d_impol)) +
    geom_line(aes(group=country)) + 
    facet_wrap(~ country) +
    scale_x_continuous(name="Year",limits=c(1980,2010)) +
    scale_y_continuous(name="Immigration policy (expressed as first-differences)") +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),panel.spacing = unit(1, "lines"))
fd


# We make it again balanced
diff <- make.pbalanced(diff,balance.type = "shared.times",index = c("country","year"))

# Dropping missings
diff <- diff[diff$country!="Estonia" & diff$country!="Hungary" & diff$country!="Slovakia",]


# Testing for non-stationarity
##############################

diffin <- pdata.frame(diff,index = c("country","year"))

purtest(diffin$d_impol,test = "hadri", exo = c("trend"),
        lags="AIC", pmax = 5) # <- this one looks good

purtest(diffin$d_socx,test = "hadri", exo = c("trend"),
        lags="AIC", pmax = 5) # <- this one not so much; we'll again proceed for the sake of illustration

# Models
df1 <- lm(d_impol~d_socx, data=diff)
    summary(df1)

df1.pcse <- pcse(df1,groupN=diff$countryn, groupT=diff$year)
    summary(df1.pcse)


df2 <- lm(d_impol~d_socx + d_eu + d_gov_left1, data=diff)
    summary(df2)

df2.pcse <- pcse(df2,groupN=diff$countryn, groupT=diff$year)
    summary(df2.pcse)

rm(df1,df1.pcse,df2,df2.pcse,diff,diffin,results)

