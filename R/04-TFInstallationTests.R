## After installing tensorflow (see pdf), let's follow this script
## to install more packages and test tensorflow

## enable github package installation
install.packages("remotes")

## install dev version of DiagrammeR
remotes::install_github("rich-iannone/DiagrammeR") # need dev version for plate notation

## install up to date pacakge that accompanies our book
remotes::install_github("flyaflya/causact") # update causact package used in book

## install CRAN version of greta
install.packages("greta")

## install CRAN version of bayesplot
install.packages("bayesplot")

# ------------------------------------------------------
## TEST#1:   test installation
library(greta)
library(causact)
## simulate Bernoulli data with 72% prob of success
y <- rbern(n = 100, prob = 0.72) #data

## create prior for theta - prob of succes
theta = uniform(0,1)  #prior

## specify likelihood of data
distribution(y) <- bernoulli(prob = theta)  #likelihood

## create model - list parameters of interest
m <- model(theta)

## get representative sample of posterior distribution
draws <- mcmc(m)
## below line requires bayesplot package if you
## get an error running the below then install.packages("bayesplot")
bayesplot::mcmc_trace(draws)
## if you see plots that look like fuzzy caterpillars
## then your install was successful

# ------------------------------------------------------
## TEST#2:   test installation of CAUSACT package
graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability",label = "theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_plate(descr = "Car Model", label = "x",  #plate labels
            data = carModelDF$carModel,  #where index variable is stored
            nodeLabels = "theta",  #nodes duplicated for each unique data value
            addDataNode = TRUE)  #automate creation of index observations
graph %>% dag_render()
## if you see a picture with three ovals and one rectangle
## then your installation was successful

# ------------------------------------------------------
# CLASS EXERCISE:  Test your ability to do a left_join with these two data frames
library("dplyr")
library("causact")
data("delivDF")
data("prodLineDF")
delivDF  ## shipment information about partID's and quantities shipped
prodLineDF  ## information about product line and prouct category?

#QUESTION1:  Which prodCategory shipped the largest quantity of products?
#QUESTION2:  Hard Question:  How many shipID's included more than one product line?

