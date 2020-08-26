##################################################################
##                  PHS SummR 2020: Regression                  ##
##                 Regression Exercise 1 R Code                 ##
##                     Covariance/Correlation                   ##
##################################################################

# CREATING RANDOM VARIABLES

###  R let’s you generate your own random variables from common distribution      
###  functions like the normal, binomial, poisson, and uniform distributions we   
###  discussed in class.                                                          

###  For example, the following code generates 3 realizations/random samples      
###  from a normal distribution with mean 45 (mean) and standard deviation (sd)   
###  19. The function rnorm() takes in these arguments, along with the number     
###  of draws you want (n) and returns n random draws from the normal             
###  distribution you specify.  

rnorm(n = 3, mean = 45, sd = 19)

###  Next, generate a vector of 100 realizations from your favorite               
###  distribution and save the results to a vector (call it whatever you          
###  want!). Then, plot a histograms of all 100 realizations. This should         
###  resemble the distribution you chose to a degree. Feel free to increase the   
###  number of realizations (say from n = 100 to n = 1000) and see what happens   
###  to the histogram.       

##################################################################
# Exercise 1.1: Insert your R code here





##################################################################



# COVARIANCE, CORRELATION, AND INDEPENDENCE 

###  In R we can calculate the correlation and covariance using the cor()and      
###  cov() functions. For example, the code below draws 1000 realizations from    
###  a normal distribution with mean 10 and standard deviation 2, and another     
###  1000 realizations from a normal distribution with mean 8 and standard        
###  deviation 3. You can think of these as data on 1000 individuals on two       
###  variables: x and y. What do you get when you run this code? Why might that   
###  be?  

x <- rnorm(1000, 10, 2)
y <- rnorm(1000, 8, 3)

cor(x, y)
cov(x, y)


###  What happens if we redefine y as a function of x? In other words, what   
###  happens if y and x are related by some given function y = 10 * x? Obtain the       
###  correlation and covariance between these two variables with this new     
###  definition of y.  

##################################################################
# Exercise 1.2: Insert your R code here





##################################################################



###  To generate two random variables with known covariance and/or correlation    
###  we can also simulate a joint distribution, in this case the multivariate     
###  normal, using a function from the MASS package. Recall that a package in R   
###  is just a set of functions and objects that have been compiled into a        
###  single collection. One function from MASS is mvrnorm(), which as it          
###  suggests, returns values from the multi-variate normal distribution. 

###  Use the mvrnorm() function to generate 1000 samples/realizations from the    
###  multivariate normal distribution. Variable  should have mean 10 and          
###  variable  should have mean 15. The variance of  is 30, and the variance of   
###  is 60. Let the covariance between  X and Y  be 40. You can read about the        
###  documentation for this function here, which will tell you how to specify     
###  the arguments mu, Sigma, and n. Also recall that the correlation  between    
###  two variables is given by:   

###  Corr(X,Y) = Cov(X, Y) / [sd(X) * sd(Y)]

###  Where Cov(X,Y) is the covariance, and sd() is the standard deviation for each            
###  variable. The sd() function returns standard deviation values in R. Print    
###  the covariance and correlation matrices between X and Y, and graph the         
###  relationship between these two variables on a 2-dimensional plane. What do   
###  you see?  

##################################################################
# Exercise 1.3: Insert your R code here





##################################################################


###  This scatter plot tells us that X and Y  co-occur with greater frequency       
###  around their means. If we want to visualize the multi-variate probability   
###  distribution, we need to extend our perspective to 3 dimensions. The        
###  plotly package allows us to create really neat interactive plots – check    
###  out the one below!  

library(MASS)
library(plotly)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(widgetframe)

# generate data from the multivariate normal
mvnorm1000 <- 
  mvrnorm(
    n = 1000, 
    mu = c(10, 15), 
    Sigma = 
      matrix(c(30, 40, 40, 60), 
             nrow=2, ncol=2, byrow=FALSE)
          )

# compute density surface
dens <- kde2d(mvnorm1000[,1], mvnorm1000[,2])

# plot object
p <-
  plot_ly(x = dens$x,
          y = dens$y,
          z = dens$z) %>% 
  add_surface()

# place plot object in iframe
frameWidget(p, height = 500, width = '100%', )



















































##################################################################
## Solutions! 
##################################################################


##################################################################
# Exercise 1.1: Insert your R code here
norm100 <- rnorm(n = 100, mean = 1, sd = 0.5)
hist(norm100)
##################################################################



##################################################################
# Exercise 1.2: Insert your R code here
x <- rnorm(1000, 10, 2)
y <- rnorm(1000, 8, 3)

# a scaled y
y <- 10*x

cor(x, y)
cov(x, y)
##################################################################



##################################################################
# Exercise 1.3: Insert your R code here
library(MASS)

mvnorm1000 <- 
  mvrnorm(
    n = 1000, 
    mu = c(10, 15), 
    Sigma = 
      matrix(c(30, 40, 40, 60), 
             nrow=2, ncol=2, byrow=FALSE)
          )
cov(mvnorm1000)
cor(mvnorm1000)

## BONUS PLOT
library(ggplot2)
ggplot(as.data.frame(mvnorm1000)) + 
  geom_point(aes(x=mvnorm1000[,1], y = mvnorm1000[,2])) +  
  theme_bw()
##################################################################


