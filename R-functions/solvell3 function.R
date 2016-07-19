## solve.ll3 function

## Author: Marcos Krull
## Contact: mkrull@vims.edu
## Last update: 07/2016

#### Description
# Generic function to get the true ECx values from a three parameters log-logistic 
# model. The default y values are set to return the EC5, EC10 and EC50.

#### Usage
# solve.ll3(y=c(0.05,0.10,0.50),int=0.95,slope=4,ec=log(37.33))

#### Arguments
# y       The ECx values to  
# int     The intercept (or background mortality)
# slope   The slope of the curve 
# ec      The inflection point, or the EC50 (in the log_e -scale)

#### Details

# This function return the true ECx values of a a three parameter log logistic 
# model as describedby Ritz (2010)

# P(y_j)=  d/(1+ e^((b(log(x_j)-log(e))))), 		

# where (i) P(y_j ) is the survival probability in the x_j concentration, 
# (ii) b is the slope, 
# (iii) e is the inflection point, or the EC50, and 
# (iv) d is the intercept. 

#### Value
# solve.ll3 returns a vector with the true ECx values of a three parameter
# log-logistic model. 


### References
# Ritz C. 2010. Toward a unified approach to dose-response modeling in ecotoxicology. Environ Toxicol Chem. 29: 220???229.

#### Examples
# solve.ll3(0.25) #returns the true EC25

## Run the code bellow to load the function

solve.ll3<-function(y=c(0.05,0.10,0.50),int=0.95,slope=4,ec=log(37.33)){ 
  y2<-int*(1-y) ## correcting for background mortality
  x<- ec + (log((int-y2)/y2))/slope
  return(exp(x))
}


solve.ll3()# returns the EC5, EC10 and EC50 respectively

