### DAG Modelling Walkthrough

library(tidyverse)
library(greta)
# Installing causact package
# remotes::install_github("rich-iannone/DiagrammeR")
# remotes::install_github("flyaflya/causact",dependencies = TRUE) ##uncomment to update causact
library(causact)

# DAG shortcuts from CAUSACT PACKAGE

# ---------------------------------------
# Starting the DAG creation process
dag_create()   ## returns a list of data frames to store DAG info
dag_create() %>% dag_node("BernoulliRV")   ## adds a node with the given description
dag_create() %>% dag_node("BernoulliRV") %>% dag_render()   ## makes picture

# ---------------------------------------
# Replicate Homework Question With DAG Shortcuts
# Assume a generative model with Bernoulli data (X) and a uniform prior
# X ~ Bernoulli(theta)
# theta ~ uniform(0,1)
# assume you observe two successes and one failure x_1 = 1, x_2 = 1, x_3 = 0.
# What is posterior probability that theta >= 65%?  (i.e. P(theta>0.65))

dag_create() %>%  # first pass - just get one node showing
  dag_node(descr = "Store Data", label = "x") %>%
  dag_render()

dag_create() %>%  # second pass - add distribution and data (if node is observed)
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = c(1,1,0)) %>%
  dag_render()

## note:  rhs distributions can be any greta distribution
?greta::distributions  # see list here

dag_create() %>%  # third pass - add parent nodes for all rhs arguments 
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), ## rhs does not use quotes
           data = c(1,1,0)) %>%
  dag_node(descr = "Success Probability", label = "theta",  # label needs quotes
           rhs = uniform(0,1)) %>%
  dag_render()

dag_create() %>%  # fourth pass - connect parent to child 
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = c(1,1,0)) %>%
  dag_node(descr = "Success Probability", label = "theta",  
           rhs = uniform(0,1)) %>%  
  dag_edge(from = "theta",
           to = "x") %>% ## ADDED LINE TO CREATE EDGE
  dag_render()

## above is the observational model we want:
# X ~ Bernoulli(theta)
# theta ~ uniform(0,1)
# DATA: c(1,1,0)

# running Bayesian inference - remove dag_render() and save graph object
graph = dag_create() %>%  # fourth pass - connect parent to child 
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = c(1,1,0)) %>%
  dag_node(descr = "Success Probability", label = "theta",  
           rhs = uniform(0,1)) %>%  
  dag_edge(from = "theta",
           to = "x")
# then, pass graph to greta object with mcmc = TRUE
graph %>% dag_render()
graph %>% dag_greta() ## creates greta code
graph %>% dag_greta(mcmc=TRUE) ## runs greta code
# two useful object are created in your environment
drawsDF  ## 1) a wide data frame of representative sample of posterior distribution - useful for posterior computation
tidyDrawsDF   ## 2) a long data frame useful for plotting

# plot first (dagp_plot behaves differently depending on the input dataframe
tidyDrawsDF %>% dagp_plot()  ## eyeball P(theta>0.65)

## typical mistake with unhelpful error: graph %>% dagp_plot()
  
# computation second - get P(theta>0.55) using dplyr recipe
drawsDF %>% 
  mutate(indicatorFlag = ifelse(theta > 0.55,"theta > 0.55","theta <= 0.55")) %>%
  group_by(indicatorFlag) %>%
  summarize(countInstances = n()) %>%
  mutate(percentageOfInstances = countInstances / sum(countInstances))

# fancier and more terse computation - mean of indicator function
# represents the percentage of occurrences
drawsDF %>% 
  mutate(flag = ifelse(theta > 0.55,1,0)) %>%
  pull(flag) %>%
  mean() # this is P(theta>0.55)

#### CLASS EXERCISE ####
# Instead of 2 successes and 1 failure,
# assume 20 successes and 10 failures.
# Modify the model and sample the posterior.
# 1) Eyeball the posterior using tidyDrawsDF %>% dagp_plot()
# 2) Use the new drawsDF to find P(theta > 0.55)

