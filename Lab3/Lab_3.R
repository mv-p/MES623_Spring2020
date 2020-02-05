##Lab3 Scirpt 

##load our packages
library(tidyverse) ##this contains the package ggplot2 which is what we will use to plot our simulation 
set.seed(4200) ##This is for replication purposes. 


#####Uniform Distribution####
## ?runif
## runif(n = numer of samples, min = lower boundary, max = upper boundary)
unif.sim = runif(10000, min = 0, max = 1) 
ggplot() + 
  aes(unif.sim) +
  geom_histogram(aes(y = ..density..),
                  bins= 30, 
                  color = "black", fill = "white") + 
  stat_function(fun = dnorm, color = "red" , args = 
                  list(mean = mean(unif.sim), 
                       sd = sd(unif.sim)))


##Uniform Bernouli Distribution 
U = runif(n = 1, min = 0, max = 1)
U 

##Coin Toss Example 
n = 10000
Uni = runif(n, min = 0, max = 1) 
toss = Uni < 0.5 
a = vector("numeric", n+1) 
avg = vector("numeric", n)
for(i in 2:length(a)){
  a[i] = a[i-1] + toss[i-1] ##because TRUE == 1 and FALSE == 0 when these logics are coerced to numeric ops
  avg[i-1] = a[i]/(i-1) ##this indexing of (i-1) is why we loop from 2:1001
}

ggplot() +
  geom_line(aes(x = 1:n, y = avg), col = "blue") + 
  labs(x = "Number of Throw", y = "Proportion of Heads", title = "Convergence to the Expected Mean")

#####Normal Distribution#####
## ?rnorm
## rnorm(x = number of samples, mean = mean of normal, sd = sd of normal) 

normal.sim = rnorm(10000, mean = 0, sd = 1)

ggplot() +
  aes(x=normal.sim)+
  geom_histogram(aes(y = ..density..), 
                 binwidth = .1, 
                 color = "black", fill = "white") +
    stat_function(fun = dnorm, color = "red", args = 
                    list(mean = mean(normal.sim10000), 
                         sd = sd(normal.sim10000)))


##Monte Carlo Distribution 
mc.func = function(numDice, numSides, targetValue, numTrials){ ##variables
  apply(matrix(sample(1:numSides, numDice*numTrials, replace = TRUE), nrow = numDice), 2, sum) <= targetValue
}

##5 runs 
outcomes = mc.func(2,6,5,5)
mean(outcomes)

##10000 runs
outcomes2 = mc.func(2,6,5,10000)
mean(outcomes2)  


##Exercise

##A
##generate random set of prices 
prices = seq(1,100, 5) 
p_n = length(prices) 
er.prices = rnorm(p_n, mean = prices, sd = 1)

##function 
dem.fun = function(slope, intercept, price){
  q = slope*price + intercept
  return(q)
}

##quantity 
quan = rnorm(p_n, dem.fun(-.4, 50, er.prices), 1) 

##demand 
dem = cbind(er.prices, quan) %>%
  data.frame()

##plot 
dem.curve = 
  ggplot(dem) + 
  geom_point(aes(x = quan, y = er.prices)) + 
  stat_smooth(method = 'lm', aes(x= quan, y = er.prices), col = "red", lty = 2) +
  labs(x = "Quantity", y = "Price", title = "Demand Graph")
dem.curve
##B 

##sample 
samples = sample(er.prices, 2) 

# Create a function to determine the quantity demanded for each price, 
# and label the points on individual graphs
graph.samp.fun = function(prices.samp) 
{
  dem.curve + # the original demand curve
    geom_linerange(aes(x = dem.fun(-.4, 50, prices.samp), 
                       y= NULL, ymin=0, ymax=prices.samp),
                   linetype="dashed")+ # add a vertical line from zero to the quantity
    geom_segment(aes(x=0,xend= dem.fun(-.4, 50, prices),
                     y=prices.samp,yend=prices.samp),
                 linetype="dashed" )+ # add a horizontal line from zero to the price
    annotate(geom="text", x=dem.fun(-.4, 50, prices.samp)-10, 
             y=prices.samp + 5, 
             label=paste("P= $",signif(prices.samp,3),", Q = ", signif(dem.fun(-.4, 50, prices.samp),3)),
             size = 4 , color="black") + # label the point on the graph, use paste to make a nice label
    labs(x= "Quantity", y = "Price ($)") # add an accurate x label
    
}

graph.samp.fun(samples[1]) 
graph.samp.fun(sample[2])





