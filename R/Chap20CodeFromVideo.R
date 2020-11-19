library(tidyverse)
library(greta)
library(causact)
theme_set(theme_minimal(16))
## see house sales price data in Ames, IA
houseDF$SalePrice

## see density function approximation
houseDF %>%
  ggplot(aes(x = SalePrice)) +
  geom_density(fill = "purple",
               alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar_format())

## generative DAG (bad) for sale prices
graph = dag_create() %>%
  dag_node("Sale Price","x",
           rhs = normal(mu,sigma),
           data = houseDF$SalePrice) %>%
  dag_node("Exp Sales Price","mu",
           rhs = uniform(100000,250000),
           child = "x") %>%
  dag_node("Std. Dev of Obs. Sales Price","sigma",
           rhs = uniform(10000,200000),
           child = "x") %>%
  dag_plate("Observation","i",
            nodeLabels = "x") 
graph %>%
  dag_render()

# get our posterior
drawsDF = graph %>% dag_greta()

# pull a random draw 
randomDrawDF = drawsDF %>% 
  sample_n(size = 1)

# simulate 1,460 observations using this mu and sigma
simObsDF = tibble(
  simSalePrice = rnorm(n = 1460,
                       mean = randomDrawDF$mu[1],
                       sd = randomDrawDF$sigma[1]),
  simNum = 1,
  dataType = "simulated")

## see density function approximation
# and compare to simulated data density plot
colors = c("simulated" = "cadetblue",
           "observed" = "navyblue")

houseDF %>%
  ggplot(aes(x = SalePrice)) +
  geom_density(aes(color = "observed"),
               alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values = colors) +
  geom_density(data = simObsDF,
               aes(x = simSalePrice,
                   color = dataType))


## now simulate more than one draw
randomDrawDF = drawsDF %>% 
  sample_n(size = 20)

## the trick part is to create 20 simulated densities
## to plot versus the observed density
simObsDF = tibble()

for (i in 1:20) {
  ### create tibble of simulated observations
  tempDF = tibble(
    simSalePrice = rnorm(
      n = 1460,
      mean = randomDrawDF$mu[i],
      sd = randomDrawDF$sigma[i]
    ),
    simNum = i,
    dataType = "simulated"
  )
  simObsDF = bind_rows(simObsDF,tempDF)
}

## now I recreate the spaghetti plot
houseDF %>%
  ggplot(aes(x = SalePrice)) +
  geom_density(aes(color = "observed"),
               alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values = colors) +
  geom_density(data = simObsDF,
               aes(x = simSalePrice,
                   color = dataType,
                   group = simNum))
