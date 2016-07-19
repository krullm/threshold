## solve.NEC function

## Author: Marcos Krull
## Contact: mkrull@vims.edu
## Last update: 07/2016

#### Description
# Generic function to get the true ECx values from a three parameters NEC 
# model. The default y values are set to return the EC5, EC10 and EC50.

#### Usage
# solve.NEC(y=c(0.05,0.10,0.50),int=0.95,b=4,c=0.2)

#### Arguments
# y       The ECx values to  
# int     The intercept (or background mortality)
# slope   The slope of the curve 
# c       The threshold parameter 

#### Details
# This function return the true ECx values of a a three parameter NEC 
# model as describedby by  Pires et al. (2002) and Fox (2010).
# P(y_j )=int^([-b (x_j - c) I (x_j - c)]),	 	
# I(x_j-c)= (1,x> c ; 0,x <=c)

#where (i) P(y_j ) is the survival probability in the x_j concentration, 
# (ii) l is the intercept, 
# (iii) m is the rate of decay (throughout the paper, m is  referred as a "slope" 
# (iv) c is the threshold parameter, and 
# (v) I(x_j-c) is the indicator function. When x_j is lower or equal to the 
# threshold, the probability of survival is equal to the intercept

#### Value
# solve.NEC returns a vector with the true ECx values of a three parameter
# NEC model. 

### References
# Fox DR. 2010. A Bayesian approach for determining the no effect concentration and hazardous concentration in ecotoxicology. Ecotoxicol Environ Saf 73:123-131.
# Pires AM, Branco JA, Picado A, Mendonça E. 2002. Models for the estimation of a 'no effect concentration'.   Environmetrics 13:15-27

#### Examples
# solve.NEC(0.25) #returns the true EC25

## Run the code bellow to load the function

solve.NEC<-function(y=c(0.05,0.10,0.50),int=0.95,b=4,c=0.2){
  y2<-int*(1-y)
  ((b*c-log(y2/int))/b)
}


solve.NEC()# returns the EC5, EC10 and EC50 respectively

