# use CTRL + ENTER to run each line
####################################################################
# R for Arithmetic:  think of R as a fancy calculator
1+2*3^2       # power first, then multiplication, then addition
(1+2)*3^2     # parentheses force addition before multiplication
((1+2)*3)^2   # nested parentheses

####################################################################
# Assignment versus tests of equality
x <- 2  # assigns the value 2 to variable named x
x = 2   # assigns the value 2 to variable named x
x       # shows the contents of x

x == 2  # checks whether the value of x is equal to 2
x != 3  # checks whether the value of x is NOT equal to 3
x < 3   # checks whether the value of x is less than 3
x > 3   # checks whether the value of x is greater than 3


####################################################################
# Vectors and element-by-element vector operations:
c(1,2,3)  ### Combine Values into a Vector
c(1,2,3) * c(7,6,5)
2 * c(1,2,3)
2 + c(1,2,3)
c(1,2,3) + c(1,2,3,4,5,6,7)  # weird behavior called vector recycling

###Simple plotting of two vectors
x = seq( from = -2 , to = 2 , by = 0.1 )   # Specify vector of x values.
y = x^2                                    # Specify corresponding y values.
plot( x , y , col="Dark Blue" , type="l" , lwd = 4)   # Plot the x,y points as a blue line.

## using functions
mean(x = 1:6) # argument explicitly named
mean(1:6)   # argument implied by position

## write your own function
## take two numbers and return the secondNumber's value
getSecond = function(firstNumber, secondNumber) {
  return(secondNumber)
}

## see autocomplete in action
## navigate cursor to end of below word & press tab
getSe

## predict these outcomes
getSecond(2,1)
getSecond(1,2)
getSecond(firstNumber = 17, secondNumber = 24)
getSecond(secondNumber = 7, 14)
getSecond(26, firstNumber = 22)

###Reading Data Using readr####
# install.packages("readr")
library("readr")
superbowl <- read_csv("./data/dowjones_super.csv")

###extract elements of a dataframe
superbowl$Winner  # best way for extracting columns
superbowl$DowJonesSimpleReturn
superbowl[1,1] # indexing starts at 1, not 0 like some other languages
superbowl[1:2,"Winner"]
superbowl[ , "Winner"]
superbowl$Winner[1:2]

###plotting that data
plot(superbowl$Winner)

###opening a webpage
shell.exec("http://en.wikipedia.org/wiki/Super_Bowl_indicator")

###load ggplot2 package to use fancier graphics
# install.packages("ggplot2")
library(ggplot2)   ##load graphing package
###looking at first 31 superbowls
ggplot(superbowl[1:31,],aes(x = DowJonesSimpleReturn, fill = Winner)) + 
  geom_dotplot(binwidth=0.05) + 
  facet_grid(Winner ~ .) + 
  ylim(0, 6)

###looking at all points
ggplot(superbowl,aes(x = DowJonesSimpleReturn, fill = Winner)) + 
  geom_dotplot(binwidth=0.05) + 
  facet_grid(Winner ~ .) + ylim(0, 6)






