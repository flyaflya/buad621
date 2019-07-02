library(tidyverse)
library(causact)
library(greta)

# ----------------------------------------------------------
### A BAYESIAN VIEW OF THE NORMAL DISTRIBUTION
# ----------------------------------------------------------
### looking at the built-in trees dataset
trees %>% dagp_plot() ## can use this to plot any df containing samples

graph = dag_create() %>%
  dag_node("Cherry Tree Height","x",
           rhs = normal(mu,sigma),
           data = trees$Height) 
graph %>% dag_render()

## make sure all unknown arguments on rhs have parents
graph = dag_create() %>%
  dag_node("Cherry Tree Height","x",
           rhs = normal(mu,sigma),
           data = trees$Height) %>%
  dag_node("Exp Height of Tree","mu",
           rhs = normal(50,24.5),  # 95% of all heights within 2 sd of mean
           child = "x")
graph %>% dag_render()

graph2 = graph %>% ## start with previous DAG and add sigma
  dag_node("Tree Height Deviaiton","sigma",
           rhs = uniform(0,50),  ## must be positive
           child = "x")
  
graph2 %>% dag_render()
graph2 %>% dag_greta(mcmc = TRUE, warmup = 400)
drawsDF %>% dagp_plot()  ## notice posterior for sigma not uniform
tidyDrawsDF %>% dagp_plot()

### CLASS EXERCISE:  Use the dag_***() functions from the causact package to create a graphical/observational model of cherry tree heights with the student-t distribution.  Here is the statistical model:
#
#     X ~ Student(nu,mu,sigma)
#     nu ~ Gamma(2,0.1)
#     mu ~ Normal(50,24.5)
#     sigma ~ Uniform (0,50)
#

### CLASS EXERCISE:  You just created two skeleton recipes for cherry tree height.  The first assumed normality.  The second a t-distribuion.  Together, let's add a ridiculous cherry tree height to our data, say a 1,000 foot tree.  Use: c(trees$Height,1000).  Does this change our estimate of mu from the 70-80 foot range?