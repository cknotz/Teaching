
#############################################################################################################
# Figures for Intro to BA Course "Socio-economic change, immigration, and the future of progressive politics"
#############################################################################################################
# Author: Carlo Knotz

# setwd() # <- Set this

rm(list=ls())

library(ggplot2) # <- install these -- e.g. install.packages("ggplot2") -- if you haven't already
library(texreg)

addTaskCallback(function(...) {set.seed(14);TRUE}) # <- this makes the results reproducible

# Hypothetical example -- effect of education on income
#######################################################

# Simulate data
b <- 50 # coefficient
    a <- 500 # intercept
    n <- 20 # observations
    
X <- runif(n, 0, 25) # Create a sample of (fixed!) observations on variable X.

Y <- a + b*X + rnorm(n,0,200) # true DGP

dat <- data.frame(X,Y)

# The hypothesized relationship
ggplot(data=dat,aes(x=X,y=Y)) +
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
    xlab("Education (X)") + 
    ylab("Income (Y)")

# Our "data"
ggplot(data=dat,aes(x=X,y=Y)) +
    geom_point(shape=3,size=3,) +
    expand_limits(y=0) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,size=1,linetype="solid"),
          plot.margin = unit(c(.1,.1,.1,.1), "cm"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    xlab("Education (years)") + 
    ylab("Income")

# Our data, with the estimated relationship indicated by the fitted line
ggplot(data=dat,aes(x=X,y=Y)) +
    geom_point(shape=3,size=3,) +
    geom_smooth(method="lm", se=F,color="red",linetype="dashed") +
    expand_limits(y=0) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,size=1,linetype="solid"),
          plot.margin = unit(c(.1,.1,.1,.1), "cm"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    xlab("Education (years)") + 
    ylab("Income")

# Regression estimation
mod <- lm(Y~X)
    summary(mod)
    beta <- summary(mod)$coefficients[2,1] # extracting coefficient for X
    alpha <- summary(mod)$coefficients[1,1] # extracting intercept
    beta <- format(round(beta,2), nsmall=2)
    alpha <- format(round(alpha,2), nsmall=2)
    
# The full plot, with the data points, fitted curve and regression coefficients
ggplot(data=dat,aes(x=X,y=Y)) +
        geom_point(shape=3,size=3,) +
        geom_smooth(method="lm", se=F,color="red",linetype="dashed") +
        expand_limits(y=0) + 
        annotate("text", x=3.75,y=350,label=alpha, color="blue") + 
        annotate("text", x=2,y=350,label="alpha_hat = ", color="blue") + 
        annotate("text", x=20,y=1300,label=beta, color="red") + 
        annotate("text", x=18.5,y=1300,label="beta_hat = ", color="red") + 
        theme_bw() + 
        theme(panel.grid.minor = element_blank(),
              panel.border = element_rect(fill=NA,size=1,linetype="solid"),
              plot.margin = unit(c(.1,.1,.1,.1), "cm"),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold")) +
        xlab("Education (years)") + 
        ylab("Income")

screenreg(mod) # <- to display the results in an article-like way

# Dummy  variable regression
############################
# Simulate data
b <- 50 # coefficient
    a <- 500 # intercept
    g <- -350 # random dummy
    n <- 100 # observations


X <- runif(n, 0, 25) # Create a sample of (fixed!) observations on variable X.
    Z <- rbinom(n, size=1, prob=0.5)
    X <- sample(X)
    Y <- sample(Y)

Y <- a + b*X + g*Z+ rnorm(n,0,100) # true DGP
    
dat <- data.frame(X,Z,Y)

ggplot(data=dat,aes(x=X,y=Y,aes(colour=factor(Z)))) +
    geom_point(shape=3,size=3,aes(colour=factor(Z))) +
    geom_smooth(method="lm", se=F,linetype="dashed",aes(group=factor(Z),colour=(factor(Z)))) +
    expand_limits(y=0) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,size=1,linetype="solid"),
          plot.margin = unit(c(.1,.1,.1,.1), "cm"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    xlab(" ") + 
    ylab(" ")

mod <- lm(Y ~ X + Z)
    summary(mod)
    alpha <- summary(mod)$coefficients[1,1] # extracting intercept
    beta <- summary(mod)$coefficients[2,1] # extracting coefficient for X
    gamma <- summary(mod)$coefficients[3,1] # extracting coefficient for dummy Z
    alpha <- format(round(alpha,2), nsmall=2)
    beta <- format(round(beta,2), nsmall=2)
    gamma <- format(round(gamma,2),nsmall=2)
    

screenreg(mod)


# Categorical variable regression
#################################
# Simulate data
b <- 50 # coefficient
a <- 500 # intercept
g <- -150 # random dummy
h <- 250 # second dummy
n <- 100 # observations


X <- runif(n, 0, 25) # Create a sample of (fixed!) observations on variable X.
    W <- rmultinom(n,1,c(.33,.33,.33))
    W <- t(W)
    W_1 <- W[,1] # creating dummy variables
    W_2 <- W[,2]
    W_3 <- W[,3]

Y <- a + b*X + g*W_1 + h*W_2+ rnorm(n,0,100) # true DGP

dat <- data.frame(X,W_1,W_2,Y)
    dat$W <- NA # re-creating original categorical variable
    dat$W[dat$W_1==1] <- 1
        dat$W[dat$W_2==1] <- 2
        dat$W[dat$W_1==0 & dat$W_2==0] <- 0

ggplot(data=dat,aes(x=X,y=Y,colour=factor(W))) +
    geom_point(shape=3,size=3) +
    geom_smooth(method="lm", se=F,linetype="dashed") +
    expand_limits(y=0) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,size=1,linetype="solid"),
          plot.margin = unit(c(.1,.1,.1,.1), "cm"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    xlab(" ") + 
    ylab(" ")

# Regression model
mod <- lm(Y ~ X + W_1 + W_2)

screenreg(mod)
