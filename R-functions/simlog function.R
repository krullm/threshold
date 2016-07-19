##### simlog function 

## Author: Marcos Krull
## Contact: mkrull@vims.edu
## Last update: 07/2016

#### Description
# Generic function to generate three parameters log-logistic datasets 
# for binomial data using Monte Carlo simulations.

#### Usage
# simlog(x=c(0,6.25,12.5,25,50,100),int=0.9,slope=10,ec=log(37.33),rep=3,size=10,n=1000)


#### Arguments
# x       The concentrations not in the log_e scale(without replicates) 
# int     The intercept (or background mortality)
# slope   The slope of the curve 
# ec      The inflection point, or the EC50 (in the log_e -scale)
# rep     The number of replicates per concentration
# size    Number of organisms per treatment
# n       The number of datasets to generate

#### Details

# This function uses a a three parameter log logistic model as described 
# by Ritz (2010)

# P(y_j)=  d/(1+ e^((b(log(x_j)-log(e))))), 		

# where (i) P(y_j ) is the survival probability in the x_j concentration, 
# (ii) b is the slope, 
# (iii) e is the inflection point, or the EC50, and 
# (iv) d is the intercept. 
             
#### Value
# simlog returns a matrix with n datasets (columns). 
# The row names indicates the concentrations.

### References
# Ritz C. 2010. Toward a unified approach to dose-response modeling in ecotoxicology. Environ Toxicol Chem. 29: 220???229.

#### Examples
# b <-simlog(n=1) #creating one dataset
# plot(rownames(b),b) #plotting the dataset

## Run the code bellow to load the function

simlog <- function(x=c(0,6.25,12.5,25,50,100),int=0.9,slope=10,ec=log(37.33),rep=3,size=10,n=1000) {
  x<-rep(x, each=rep)
  y<-int/(1+exp(slope*(log(x) -ec)))
  dat=matrix(ncol=n, nrow=(length(y)))
  for(i in 1: length(y))
  {
    dat[i,]<-(rbinom(n,size, prob<-(y[i])))
  }
  colnames(dat)<-colnames(dat, do.NULL = F, prefix = "dataset")
  rownames(dat)<-x
  return(dat)
}

##### Using the function
a=simlog(n=1) #creating one dataset

