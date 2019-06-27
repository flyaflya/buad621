# ---------------------------------------
### Exploring the beta distribution
# ---------------------------------------
graph = dag_create() %>%
  dag_node("Beta Distribution1","theta1",
           rhs = beta(0.5,0.5)) %>%
  dag_node("Beta Distribution2","theta2",
           rhs = beta(1,1))  %>%
  dag_node("Beta Distribution2","theta3",
           rhs = beta(2,2)) %>%
  dag_node("Beta Distribution3","theta4",
           rhs = beta(50,100)) %>%
  dag_node("Beta Distribution4","theta5",
           rhs = beta(500,500))
graph %>% dag_render()
## running inference without data simply gives prior distributions
## for simple distributions, we can shrink the "warmup" period for faster results
graph %>% dag_greta(mcmc = TRUE, warmup = 400)  
## plot density functions
drawsDF %>% dagp_plot()  # this is a ggplot, but scales are misleading
drawsDF %>% dagp_plot() + xlim(0,1)# see ZOOMING section of ggplot2 cheat sheat 

### CLASS Question:  Let theta represent the probability of heads on a coin flip.  Which distribution, beta(0.5,0.5), beta(1,1), beta(2,2), beta(50,100) or beta(500,500) best represents your belief in theta given that the coin is randomly chosen from someone's pocket in the class?

### CLASS Question:  Which distribution, beta(0.5,0.5), beta(1,1), beta(2,2), beta(50,100), or beta(500,500) best represents your belief in theta given the coin is purchased from a magic store and you have strong reason to believe that both sides are heads or both sides are tails?

### CLASS Question:
### Given theta4 ~ BETA(50,100), 
### how plausible are values below 40%?  i.e. what 
### is the percentage of posterior draws (i.e. see 
### drawsDF) below 40%? (replace ... below to answer)
drawsDF %>%
  mutate(indicatorFunction = ...) %>%
  summarize(mean(indicatorFunction))

### compare to exact cumulative distribution function
pbeta(q = 0.4,shape1 = 50,shape2 = 100)  ##use more draws to get closer

### CLASS EXERCISE:  IN ANOTHER R-SCRIPT.  Simulate two random variables.  X ~ Beta(1,2) and Y ~ Beta(2,1).  Then use the indicator function on the posterior draws to answer what is the probability that X > Y? (note: this cannot be answered without getting 

