# preliminaries
library(tidyverse)
theme_set(theme_minimal(16))
library(greta)
library(causact)

# getting the data
wawaDF = read_csv("https://raw.githubusercontent.com/flyaflya/buad621/master/data/wawa.csv",
                  col_types = c(col_integer(),
                                col_integer(),
                                col_integer()))
wawaDF = wawaDF %>%
  mutate(pctRedeemed = redemptions / offers)
wawaDF

#create a simple mode of redmption probability
simpleGraph = dag_create() %>%
  dag_node("# Redeemed","k",
           rhs = binomial(n, theta),
           data = wawaDF$redemptions) %>%
  dag_node("# of Offers","n",
           data = wawaDF$offers,
           child = "k") %>%
  dag_node("Redemption Probability","theta",
           rhs = beta(2/10,98/10),
           child = "k") %>%
  dag_plate("DistanceKM","x",
            nodeLabels = "theta",
            data = wawaDF$distanceKM,
            addDataNode = TRUE) %>%
  dag_plate("Observations","i",
            nodeLabels = c("n","k","x")) 

simpleGraph %>% dag_render()
simpDrawsDF = simpleGraph %>% dag_greta()
simpDrawsDF %>% dagp_plot()

graph = dag_create() %>%
  dag_node("# Redeemed","k",
           rhs = binomial(n, theta),
           data = wawaDF$redemptions) %>%
  dag_node("# of Offers","n",
           data = wawaDF$offers,
           child = "k") %>%
  dag_node("Redemption Probability","theta",
           rhs = 1 / (1 + exp(-y)),
           child = "k") %>%
  dag_node("Linear Predictor","y",
           rhs = alpha + beta*x,
           child = "theta") %>%
  dag_node("Base Succ Prob Param","alpha",
           rhs = normal(-3,1),
           child = "y") %>%
  dag_node("Distance Slope Param","beta",
           rhs = normal(0,3),
           child = "y") %>%
  dag_node("DistanceKM","x",
           data = wawaDF$distanceKM,
           child = "y") %>%
  dag_plate("Observation","i",
            nodeLabels = c("k","n","theta","y","x"))
graph %>% dag_render()

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

# get posterior for theta given a distance
postTheta = function(distance) {
  tempDF = drawsDF %>% 
    mutate(y = alpha + beta * distance) %>%
    mutate(theta = 1 / (1+exp(-y)))
  return(tempDF$theta)
}

#test it
postTheta(distance = 4)

# and then show as density plot
tibble(x = postTheta(4)) %>% # need df for ggplot
  ggplot() +
  geom_density(aes(x=x), fill = "purple", alpha = 0.5)

# another way to plot posterior that is easy and
# also a little more compact is to pass a representative sample to a stat_geom from ggdist
# install.packages("ggdist")
library("ggdist")
tibble(x = postTheta(4)) %>% # need df for ggplot
  ggplot(aes(x = x, y = 4)) +
  stat_pointinterval(.width = 0.9) + # cred interval
  labs(x = "Success Prob Theta",
       y = "Distance x")

## really we should plot distance on x-axis
## so switch around axes and add some limits
## tso plot can be added to.
tibble(x = postTheta(4)) %>% # need df for ggplot
  ggplot(aes(y = x, x = 4)) +
  stat_pointinterval(.width = 0.9) + # cred interval
  labs(y = "Success Prob Theta",
       x = "Distance x") +
  coord_cartesian(xlim = c(2,10)) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.035))

# use for loop to make data frame for more than
# just 4 kilometers distance - go from 2km to 10km
plotDF = tibble() ## initialize dataframe for plot
for (dist in seq(2,10,by=0.5)) {
  tempDF = tibble(distance = dist,
                  draw = postTheta(dist))
  plotDF = bind_rows(plotDF,tempDF)
}

## now grab plot from above and graph this
# expanded dataset
plotDF %>% # need df for ggplot
  ggplot(aes(y = draw, x = distance)) +
  stat_pointinterval(.width = 0.9) + # cred interval
  labs(y = "Success Prob Theta",
       x = "Distance x") +
  coord_cartesian(xlim = c(2,10)) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,0.04))
