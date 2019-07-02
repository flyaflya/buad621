library(tidyverse)
library(causact)
library(greta)

# ----------------------------------------------------------
### Posterior Predicitve Check For The Credit Card Model Based On Car Driven
# ----------------------------------------------------------

carModelDF   #show data

### DAG With One Theta
### parantheses make assignment and output rhs
### of the assignment operator
(graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability",label = "theta",
           rhs = beta(2,2),
           child = "y")) %>% dag_render()
graph %>% dag_greta(mcmc = TRUE, warmup = 400) ## get representative sample
## note: do not use the warmup argument in HW

drawsDF %>% dagp_plot()

## POSTERIOR PREDICTIVE CHECK:  See if the posterior model can generate data similar to the data that was observed.
set.seed(123)
(postPredDF = drawsDF %>%
  sample_n(40) %>% # get 40 random values
  # simulate 1,000 data points 
  # (i.e. the same # as in the data)
  # for each of the 40 theta possiblities
  mutate(simNumber = row_number()) %>%
  rowwise() %>% # treat each row as group
  mutate(sampNumSuccess = rbinom(n=1,
                                 size = 1000,
                                 prob = theta)))

## compare posterior predicted successes to actual # of successes
actualSuccessNumber = sum(carModelDF$getCard)

postPredDF %>%
  ggplot() +
  geom_histogram(aes(x = sampNumSuccess),
               fill = "slateblue",
               binwidth = 1) # +
# geom_vline(xintercept = actualSuccessNumber,
#            color = "navyblue",
#            size = 4)
# remove above commenting to see actual successes

# This is a successful generative story, but the
# previous model does not take advantage of 
# car model.  Does the story change by carModel?

## POSTERIOR PREDICTIVE CHECK BY CAR MODEL:  
set.seed(123)

sampDF = carModelDF %>%
  group_by(carModel) %>%
  summarize(numObs = n(),
            numSuccess = sum(getCard)) %>%
  rowwise() %>%
  mutate(predSim = 
           list(rbinom(n = 40,
                       size = numObs,
                       prob = postPredDF$theta)))

## unnest list column to get observations
## and plot
sampDF %>%
  unnest() %>%
  ggplot() +
  geom_histogram(aes(x = predSim),
                 fill = "slateblue",
                 binwidth = 1) +
  facet_wrap(~carModel) +
  geom_vline(data = sampDF,
             aes(xintercept = numSuccess),
            color = "navyblue",
            size = 2)
  
# see http://r4ds.had.co.nz/iteration.html#mapping-over-multiple-arguments for more on pmap



### Now vary your modelling of theta by car model
### we use the plate notation to say
### "create a copy of theta for each unique car model
(graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability",
           label = "theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_plate(descr = "Car Model", label = "x", 
            data = carModelDF$carModel,  
            nodeLabels = "theta", 
            addDataNode = TRUE)) %>% 
  dag_render()
graph %>% dag_greta(mcmc = TRUE, warmup = 400) ## get representative sample
drawsDF %>% dagp_plot() + xlim(0,1)  # visually appealling method of plotting

## POSTERIOR PREDICTIVE CHECK WITH CAR MODEL PLATE:  See if the posterior model can generate data similar to the data that was observed.
set.seed(123)

## connect parameter estimate names to 
## real world names
thetaSamples = drawsDF %>%
    sample_n(40) %>%
  gather() %>%
  mutate(key = case_when(
    key == "theta_JpWrnglr" ~ "Jeep Wrangler",
    key == "theta_KiaForte" ~ "Kia Forte",
    key == "theta_SbrOtbck" ~ "Subaru Outback",
    key == "theta_ToytCrll" ~ "Toyota Corolla",
    TRUE ~ "Unknown Car Model",
  ))

## create tibble to get simulated coefficients in
## the same tibble as the observed data
nestedThetaSamples = thetaSamples %>%
  nest(value, .key = "postSamps") 

## use join to combine
sampDF = carModelDF %>%
  group_by(carModel) %>%
  summarize(numObs = n(),
            numSuccess = sum(getCard)) %>%
  left_join(nestedThetaSamples, by = c("carModel" = "key")) %>%
  unnest() %>%
  rowwise() %>%
  mutate(predSim = rbinom(
    n = 1,
    size = numObs,
    prob = value
  ))

## plot the simulated versus observed
sampDF %>%
  ggplot() +
  geom_histogram(aes(x = predSim),
                 fill = "slateblue",
                 binwidth = 1) +
  facet_wrap(~carModel) +
  geom_vline(data = sampDF,
             aes(xintercept = numSuccess),
             color = "navyblue",
             size = 2)
