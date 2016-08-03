##### simnec function 

## Author: Marcos Krull
## Contact: mkrull@vims.edu
## Last update: 07/2016

#### Description
# Generic function to generate three parameters no observed effect concentration 
# (NEC) datasets for binomial data using Monte Carlo simulations.

#### Usage
# simnec(x=c(0,0.0625,0.125,0.25,0.5,1),int=0.9,b=10,c=0.2,rep=3,size=10,n=1000)

#### Arguments
# x       The concentrations (without replicates)
# int     The intercept (or background mortality)
# b       The decay rate 
# c       The threshold parameter
# rep     The number of replicates per concentration
# size    Number of organisms per treatment
# n       The number of datasets to generate

#### Details

# This function uses a three parameters NEC model modified from Pires et al. (2002), 
# and used by Fox (2010), to describe the survival probability of organisms 
# exposed to an effluent with the equation

# P(y_j )=int^([-b (x_j - c) I (x_j - c)]),	 	
# I(x_j-c)= (1,x> c ; 0,x <=c)

#where (i) P(y_j ) is the survival probability in the x_j concentration, 
# (ii) l is the intercept, 
# (iii) m is the rate of decay (throughout the paper, m is  referred as a "slope" 
# (iv) c is the threshold parameter, and 
# (v) I(x_j-c) is the indicator function. When x_j is lower or equal to the 
# threshold, the probability of survival is equal to the intercept
  
#### Value
# simnec returns a matrix with n datasets (columns). The row names indicates the concentrations.

### References
# Fox DR. 2010. A Bayesian approach for determining the no effect concentration and hazardous concentration in ecotoxicology. Ecotoxicol Environ Saf 73:123-131.
# Pires AM, Branco JA, Picado A, Mendonça E. 2002. Models for the estimation of a 'no effect concentration'.   Environmetrics 13:15-27


#### Examples
# b<-simnec(n=1) #creating one dataset
# plot(rownames(b),b) #plotting the dataset


## Run the code bellow to load the function

simnec <- function(x=c(0,0.0625,0.125,0.25,0.5,1),int=0.9,b=10,c=0.2,rep=3,size=10,n=1000) {
  x<-rep(x, each=rep)
  y<-numeric()
  for (i in 1:length(x))
  {
    if (x[i] <= c){
      y[i]<-int*exp(-(b*(x[i]-c)*0*(x[i]-c)))}
    else {
      y[i]<-int*exp(-(b*(x[i]-c)))}
  }
  dat=matrix(ncol=n, nrow=(length(y)))
  for(i in 1: length(y))
    {
    dat[i,]<-(rbinom(n,size, prob<-(y[i])))
  }
  colnames(dat)<-colnames(dat, do.NULL = F, prefix = "dataset")
  rownames(dat)<-x
  return(dat)
}

#### Using the function
a=simnec(n=10) #creating 10 datasets
b=simnec(b=3) #creating 1000 datasets with the slope equals to 3)




