## get R-Script from:
##          
## data sources:
## https://ourworldindata.org/happiness-and-life-satisfaction
## https://population.un.org/wpp2019/Download/Standard/Interpolated/
## https://worldhappiness.report/ed/2019/

# preliminaries
library(tidyverse)
theme_set(theme_minimal(16))
library(greta)
library(causact)

# get the data 
happyDF = read_csv("https://raw.githubusercontent.com/flyaflya/buad621/master/data/happy.csv")

# transform for plotting
plotDF = happyDF %>%
  select(country,lifeSatisfaction,GDPperCapita) %>%
  mutate(x_trans = ## centered logarithm of x
           log10(GDPperCapita) - 
           mean(log10(GDPperCapita)))
plotDF ## see inside plotDF

# see initial data
plotDF %>%
  ggplot() +
  geom_point(aes(x = GDPperCapita, y = lifeSatisfaction))

## see transformed data
plotDF %>%
  ggplot() +
  geom_point(aes(x = x_trans, y = lifeSatisfaction))

## FITTING THE TRANSFORMED DATA

## x-trans ranges from -1 to 1 .. approx 2 units
## PRIOR: min value of slope should be 0 ... corresponding
## to no relationship.
## max value should be about 5 for a 10-point
## increase in life satisfaction over the 2 unit
## change in x

## THE GENERATIVE dag

graph = dag_create() %>%
  dag_node("Life Satisfaction","y",
           rhs = normal(mu,sigma),
           data = plotDF$lifeSatisfaction) %>%
  dag_node("Exp Life Satisfaction","mu",
           child = "y",
           rhs = alpha + beta * x_trans) %>% 
  dag_node("Intercept","alpha",
           rhs = normal(5,2),
           child = "mu") %>%
  dag_node("Inter-Country Variation","sigma",
           rhs = gamma(2,1),
           child = "y") %>%
  dag_node("Slope Term","beta",
           rhs = uniform(0,5),
           child = "mu") %>%
  dag_node("Transformed GPD Meas","x_trans",
           data = plotDF$x_trans,
           child = "mu") %>%
  dag_plate("Observation","i",
            nodeLabels = c("x_trans","y","mu")) #x
graph %>% dag_render()

drawsDF = graph %>% dag_greta()

### from posterior to relationship
### between Per Capita GDP (x) and
### Exp Life Satisfaction, mu

### pick a draw of alpha and beta
### save it as a ggplot layer
### collect 20 or so layers as a list for plotting
spaghettiLayerParamsDF = drawsDF %>% 
  slice_sample(n = 20)

getMu = function(alpha,beta,x) {
  x_trans = log10(x) - mean(log10(happyDF$GDPperCapita))
  mu = alpha + beta * x_trans
  return(mu)
}

plot = happyDF %>%  #initialize plot
  ggplot(aes(x = GDPperCapita)) +
  geom_point(aes(x = GDPperCapita,
                 y = lifeSatisfaction)) #establish limits
## add layers to plot
for (i in 1:nrow(spaghettiLayerParamsDF)) {
  alphaParam = spaghettiLayerParamsDF$alpha[i]
  betaParam = spaghettiLayerParamsDF$beta[i]
  plot = plot  +
    stat_function(fun = getMu,
                  args = list(alpha = alphaParam,
                              beta = betaParam),
                  color = "cadetblue",
                  )
}
plot


