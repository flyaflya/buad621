### This walkthrough closely follows the lecture
### of Richard McElreath.  We will use cherry
### tree heights instead of human heights.
### see this video for more info:
### https://www.youtube.com/watch?v=h5aPo5wXN8E&t=0s

library(tidyverse)
library(greta)
library(causact)

treesDF = trees %>% as_tibble()
treesDF  ## note: girth is tree diameter at 4'6" above ground

dagp_plot(treesDF) ## quick way to plot data frame of samples
treesDF %>% dagp_plot()  ## same as above ... just reminding you of %>%

## let's try to just understand height
treesDF %>%
  ggplot(aes(x=Height)) + geom_dotplot()

## create a model
dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_render()

# ----------------------------------
## add priors for unknown parameters
# ----------------------------------

## add prior for mu
dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_node("Avg Height of Cherry Tree","mu",
           child = "y") %>%
  dag_render()

## let's check a possible prior
## for cherry tree height
graph = dag_create() %>%
  dag_node("Prior for Avg Cherry Tree Height","heightPrior",
           rhs = normal(0,10))
graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE, warmup = 100)
drawsDF %>%
  dagp_plot() +
  geom_vline(xintercept = -10, size = 2) +
  geom_text(aes(x = -11.75,
                y = 0.45,
                label = "Is -10 a plausible cherry tree height?"),
            angle = 90,
            size = 8,
            color = "darkblue")
## what does this prior imply about height?


# try again
graph = dag_create() %>%
  dag_node("Prior for Avg Cherry Tree Height","heightPrior",
           rhs = uniform(6,200))
graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE, warmup = 100)
drawsDF %>%
  dagp_plot() + xlim(0,300)

# let's use it!
graph = dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_node("Avg Height of Cherry Tree","mu",
           child = "y",
           rhs = uniform(6,200)) ##NEW LINE
graph %>% dag_render()

# add a prior for sigma
graph = graph %>%
  dag_node("Deviation From Avg Height","sigma",
           rhs = uniform(0,60))
graph %>% dag_render()

## forgot the edge
graph = graph %>%
  dag_edge(from = "sigma",to = "y")
graph %>% dag_render()

graph %>% dag_greta(mcmc=TRUE, warmup = 400)
drawsDF %>% dagp_plot()

## posterior predictive check - use code from parameter estimation chapter
## use skeleton recipe plus posterior distribution to make fake data
## get three samples from posterior
postPredSamples = drawsDF %>% sample_n(3)  ## just compare one sample to data
postPredSamples

## use samples to generate fake data.
## fake data should be the same length as real data
numObs = nrow(treesDF)

## And Matrix of Posterior Predictive Draws
postCheckMatrix = postPredSamples %>%
  mutate(sampDraws = pmap(list(n = numObs,mean = mu, sd = sigma),rnorm)) %>%  ##difficult function to learn.  Spend time going through http://r4ds.had.co.nz/iteration.html#mapping-over-multiple-arguments
  .$sampDraws %>%
  unlist() %>%
  matrix(ncol = numObs, byrow = TRUE)

bayesplot::ppc_dens_overlay(treesDF$Height, postCheckMatrix)

### change line 96 to sample 30 possible datasets and rerun


### even though our fit is good, we know we have other factors
### that we can include to get a better fit
treesDF %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
### here is a so-called "best fit" ... is this the only plausible line?


### ---------------------------------------------
### we will create a skeleton recipe for drawing lines
### ---------------------------------------------
graph = dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_node("Avg Height of Cherry Tree","mu",
           child = "y",
           rhs = alpha + beta * x) %>%
  dag_node("Deviation From Avg Height","sigma",
           rhs = uniform(0,60),
           child = "y") %>%
graph %>% dag_render()


### add parents
graph2 = graph  %>%
  dag_node("Cherry Tree Girth","x",
           child = "mu",
           data = treesDF$Girth) %>%
  dag_node("Intercept of Linear Predictor","alpha",
           child = "mu") %>%
  dag_node("Height to Girth Slope","beta",
           child = "mu")
graph2 %>% dag_render()


### add plate for clarity
graph3 = graph2 %>%
  dag_plate("Observation","i",
            nodeLabels = c("y","mu","x"))
graph3 %>% dag_render()

## what do you know about height to girth
## its positive
## good dist to use with positive elements is the lognormal
graph4 = dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_node("Avg Height of Cherry Tree","mu",
           child = "y",
           rhs = alpha + beta * x) %>%
  dag_node("Cherry Tree Girth","x",
           child = "mu",
           data = treesDF$Girth) %>%
  dag_node("Deviation From Avg Height","sigma",
           rhs = uniform(0,60),
           child = "y")  %>%
  dag_node("Intercept of Linear Predictor","alpha",
           child = "mu") %>%
  dag_node("Height to Girth Slope","beta",
           child = "mu")  %>%
  dag_plate("Observation","i",
            nodeLabels = c("y","mu","x"))
graph4 %>% dag_render()

### this is our skelton recipe for a line
### we just need to narrow down the list of
### possible alphas and beta values
### let's look at reasonable lines
treesDF %>%
  ggplot(aes(x = Girth, y = Height)) +
  xlim(0,40) + ylim(-50,200) +
  ggtitle("Relationship Between Avg Cherry Tree Height and Girth")

## show lines in Onenote

## let's go with this model of narrowing down plausible lines
graph5 = dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_node("Avg Height of Cherry Tree","mu",
           child = "y",
           rhs = alpha + beta * x) %>%
  dag_node("Cherry Tree Girth","x",
           child = "mu",
           data = treesDF$Girth) %>%
  dag_node("Deviation From Avg Height","sigma",
           rhs = uniform(0,60),
           child = "y")  %>%
  dag_node("Intercept of Linear Predictor","alpha",
           child = "mu",
           rhs = uniform(-200,150)) %>% # new line
  dag_node("Height to Girth Slope","beta",
           child = "mu",
           lognormal(4,1))  %>%  # new line
  dag_plate("Observation","i",
            nodeLabels = c("y","mu","x"))
graph5 %>% dag_render(shortLabel=TRUE)

graph5 %>% dag_render()

### simulate some average lines
priorGraph = dag_create() %>%
  dag_node("Intercept of Linear Predictor","alpha",
           rhs = uniform(-200,150)) %>%
  dag_node("Height to Girth Slope","beta",
           lognormal(4,1))
priorGraph %>% dag_render()
priorGraph %>%
  dag_greta(mcmc=TRUE, warmup = 100)
drawsDF %>% dagp_plot()


tempDF = drawsDF %>% sample_n(20)

treesDF %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(data=tempDF, aes(intercept = alpha, slope = beta), alpha = 0.5, linetype = "dashed")  +
  xlim(0,40) + ylim(-50,200)


### slopes look to big ... try modifying
### simulate some average lines
priorGraph = dag_create() %>%
  dag_node("Intercept of Linear Predictor","alpha",
           rhs = uniform(-200,150)) %>%
  dag_node("Height to Girth Slope","beta",
           lognormal(2,1))
priorGraph %>% dag_render()
priorGraph %>%
  dag_greta(mcmc=TRUE, warmup = 100)
drawsDF %>% dagp_plot()


tempDF = drawsDF %>% sample_n(20)

treesDF %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(data=tempDF, aes(intercept = alpha, slope = beta), alpha = 0.5, linetype = "dashed")  +
  xlim(0,40) + ylim(-50,200)

### that looks better.
### now update your graph with new prior on slope
## run inference using greta
graph6 = dag_create() %>%
  dag_node("Cherry Tree Height","y",
           rhs = normal(mu,sigma),
           data = treesDF$Height) %>%
  dag_node("Avg Height of Cherry Tree","mu",
           child = "y",
           rhs = alpha + beta * x) %>%
  dag_node("Cherry Tree Girth","x",
           child = "mu",
           data = treesDF$Girth) %>%
  dag_node("Deviation From Avg Height","sigma",
           rhs = uniform(0,60),
           child = "y")  %>%
  dag_node("Intercept of Linear Predictor","alpha",
           child = "mu",
           rhs = uniform(-200,150)) %>%
  dag_node("Height to Girth Slope","beta",
           child = "mu",
           lognormal(2,1))  %>%  # new line
  dag_plate("Observation","i",
            nodeLabels = c("y","mu","x"))
graph6 %>% dag_render(shortLabel=TRUE)

graph6 %>% dag_render()

## use greta
graph6 %>%
  dag_greta(mcmc=TRUE)

## posterior
drawsDF %>% dagp_plot()

## posterior predictive check
tempDF = drawsDF %>% sample_n(20)

treesDF %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(data=tempDF, aes(intercept = alpha, slope = beta), alpha = 0.5, linetype = "dashed")  +
  xlim(0,40) + ylim(-50,200)

## zoom in
treesDF %>%
  ggplot(aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(data=tempDF, aes(intercept = alpha, slope = beta), alpha = 0.5, linetype = "dashed")

### read the book to fill in the details (i.e. draw the owl, lol)
