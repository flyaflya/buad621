library(tidyverse)
library(causact)
library(greta)

# ----------------------------------------------------------
### Revisiting the Get Credit Card Model Based On Car Driven
# ----------------------------------------------------------
# data included in the causact package
# 1,000 potential customers, the car they drvie
?carModelDF  ## see help file
carModelDF   #show data

### Use similar DAG
graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability",label = "theta",
           rhs = beta(2,2),
           child = "y")
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_render()
graph %>% dag_greta(mcmc = TRUE, warmup = 400) ## get representative sample
## note: do not use the warmup argument in HW

drawsDF %>% dagp_plot()

### Now vary your modelling of theta by car model
### we use the plate notation to say
### "create a copy of theta for each unique car model
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
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_render()
graph %>% dag_greta(mcmc = TRUE, warmup = 400) ## get representative sample
drawsDF %>% dagp_plot() + xlim(0,1)  # visually appealling method of plotting

### CLASS Question:  Why is the probability interval for the Toyota Corolla more narrow than the interval for the other cars?  Back up your theory with facts from the data.
tidyDrawsDF %>% dagp_plot() 

### CLASS EXERCISE:  IN ANOTHER R-SCRIPT.  Create an indicator function on the posterior draws to answer what is the probability that theta_KiaForte > theta_ToytCrll ? 


