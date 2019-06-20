library(tidyverse)
library(causact)
library(greta)

## THE BELOW CODE WILL BE LARNED LATER
## FOR NOW, IT GIVES THE PICTURE OF THE
## GENERATIVE STORY
dag_create() %>%
    dag_node("Total Passengers","y",
             rhs = x1 + x2 + x3) %>%
    dag_node("Pass#1","x1",
             rhs = bernoulli(0.85),
             child = "y") %>%
    dag_node("Pass#2","x2",
             rhs = bernoulli(0.85),
             child = "y") %>%
    dag_node("Pass#3","x3",
             rhs = bernoulli(0.85),
             child = "y") %>% 
  dag_render()


numFlights = 1000 ## number of simulated flights
probShow = 0.85 ## probability of passenger showing up
set.seed(111) ## choose random seed so others can replicate results

# SIMULATE PASSENGER ARRIVALS
pass1 = rbern(n = numFlights, prob = probShow)
pass2 = rbern(n = numFlights, prob = probShow)
pass3 = rbern(n = numFlights, prob = probShow)

# ANALYZE RESULTS
flightDF = tibble(
  simNum = 1:numFlights,
  totalPassengers = pass1 + pass2 + pass3
)

# transform data to give proportion
propDF = flightDF %>% 
  group_by(totalPassengers) %>% 
  summarize(numObserved = n()) %>%
  mutate(proportion = numObserved / sum(numObserved))

# plot data with estimates
ggplot(propDF, aes(x = totalPassengers, y = proportion)) +
  geom_col() +
  geom_text(aes(label = proportion), nudge_y = 0.03, size = 6) +
  theme_minimal(24)






