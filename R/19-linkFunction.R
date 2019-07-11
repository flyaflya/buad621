############################################
# R_CODE TO SUPPORT LINK FUNCTION DISCUSSION

## load packages and data
library(causact)
library(tidyverse)
library(greta)
data("gymDF")  ## load into environment
gymDF  ## view the data

## create DAG
## outer parantheses used to output rhs
## after assignment is made (i.e. enable
## assignment + output)
(graph = dag_create() %>%
    dag_node("# of Signups","k",
             rhs = binomial(nTrials,succProb),
             data = gymDF$nSigned) %>%
    dag_node("# of Trials","nTrials",
             data = gymDF$nTrialCustomers,
             child = "k") %>%
    dag_node("Success Probability","succProb",
             rhs = 1 / (1 + exp(-y)),
             child = "k") %>%
    dag_node("Linear Predictor","y",
             rhs = intercept + coeff * x,
             child = "succProb") %>%
    dag_node("Yoga Impact at Gym","coeff",
             rhs = normal(mu_yo,sigma_yo),
             child = "y") %>%
    dag_node("Baseline Success at Gym","intercept",
             rhs = normal(mu_cf,sigma_cf),
             child = "y") %>%
    dag_node("Yoga Stretch","x",
             data = gymDF$yogaStretch,
             child = "y") %>%
    dag_node("Baseline Success - CF","mu_cf",
             child = "intercept",
             rhs = normal(0,10)) %>%
    dag_node("Baseline Variation - CF","sigma_cf",
             child = "intercept",
             rhs = uniform(0,5)) %>%
    dag_node("Yoga Impact - CF","mu_yo",
             child = "coeff",
             rhs = normal(0,2)) %>%
    dag_node("Yoga Imp Variation - CF","sigma_yo",
             child = "coeff",
             rhs = uniform(0,5))
    %>%
    dag_plate("Gym","j",
              data = gymDF$gymID,
              addDataNode = TRUE,
              nodeLabels = c("intercept","coeff")) %>%
    dag_plate("Observation","i",
              nodeLabels = c("nTrials","k","succProb","y","x","j"))
    ) %>% dag_render()


## get posterior
graph %>% dag_greta(mcmc = TRUE)

## convert posterior rv estimates into
## posterior linear predictor(lp) estimates
## for gym#7
drawsDF %>%
  select(intercept_7,coeff_7) %>%
  sample_n(10) %>% ## show on small sample
  mutate(lpNoYoga7 = intercept_7,
           lpYoga7 = intercept_7 + coeff_7)

## convert posterior lp estimates into
## posterior probability estimates
## for gym#7 using inverse logit link function
(postDF = drawsDF %>%
  select(intercept_7,coeff_7) %>%
#  sample_n(10) %>% ## show on small sample
  mutate(lpNoYoga7 = intercept_7,
           lpYoga7 = intercept_7 + coeff_7) %>%
  mutate(succProb7NoYoga = 1 / (1 + exp(-lpNoYoga7)),
           succProb7Yoga = 1 / (1 + exp(-lpYoga7))) %>%
  select(succProb7NoYoga,succProb7Yoga)
)

## Find probability Yoga is helpful
## at gym #7
postDF %>%
  mutate(yogaHelp7 = succProb7Yoga > succProb7NoYoga) %>%
  summarize(pctYogaHelp7 = mean(yogaHelp7))

## Class Exercise - In separate R-script
## Find the probability Yoga is helpful at
## gym #9.  Interpret why the number is different
## than the probability for gym #7 (use below
## ggplot code to aid interpretation)
plotDF = gymDF %>%
  mutate(stretchType = ifelse(yogaStretch == 1, "Yoga Stretch", "Traditional"))
histDataPlot = ggplot(plotDF,
                      aes(x = timePeriod,
                          # map time period to x-axis
                          y = nTrialCustomers,
                          # map trial customers to y-axis
                          fill = stretchType
                          #color = stretchType
                          )) +
  geom_col(width = 0.75,
           aes(y = nSigned)) + # different y-mapping
  facet_wrap(vars(gymID),
             labeller = label_both) + #map gymID to facets +
  theme_minimal(11) +
  theme(plot.caption = element_text(size = 9,
                                    face = "italic"),
        legend.position = "bottom") +
  ylab("# of Trial Customers") +
  labs(caption = "Darker fill represents the # of trial customers who became paying members.")
histDataPlot

####### INTERPRETING POSTERIORS WITH LINK FUNCTIONS
## USE LINK FUNCTION TO MAKE PROBABLISTIC
## STATEMENTS ON A MEANINGFUL SCALE
## FOR EXAMPLE, WE WANT THE POSTERIOR
## DISTRIBUTION FOR THE CONVERSION RATE
## IMPACT OF OFFERING YOGA AT EACH GYM

## NOTICE: graphing coefficient estimates
## yields hard-to-interpret parameter estimates
tidyDrawsDF %>% dagp_plot()


getSuccProbDiff = function(intercept,coefficient) {
  succProbIntOnly = 1 / (1 + exp(-intercept))
  succProbIntAndCoeff = 1 / (1 + exp(-(intercept+coefficient)))
  succProbDiff = succProbIntAndCoeff - succProbIntOnly
  return(succProbDiff)
}

# test function
getSuccProbDiff(intercept = 0,coefficient = 1)
getSuccProbDiff(intercept = -1,coefficient = 1)
getSuccProbDiff(intercept = -999,coefficient = 999)
## CLASS EXERCISE: use below ggplot to describe why
## the below two functions with identical coefficent term
## Function1: getSuccProbDiff(intercept = 0,coefficient = 1)
## Function2: getSuccProbDiff(intercept = -1,coefficient = 1)
## give identical results, but changing the intercept
## to another number gives a different result (see Funct3)
## Function3: getSuccProbDiff(intercept = 1,coefficient = 1)
invLogit = function(x) {1 / (1 + exp(-x))}
ggplot((data.frame(x = c(-5, 5))), aes(x))+
  geom_vline(xintercept = c(0,-1,1,2),
             color = "red",
             lwd = 2) +
  geom_hline(yintercept = 0.5,
             color = "blue",
             lwd = 1.2)  +
  stat_function(fun = invLogit,
                lwd = 4) +
  scale_x_continuous(breaks = seq(-5,5,0.5)) +
  xlab("linear predictor value: alpha + beta * x") +
  ylab("Output of Inverse Link Function (i.e. invLogit)")

#### KEY TAKEAWAY:  NEED TO TRANSFORM POSTERIOR
## TO MORE MEANINGFUL SCALE THAN INT + COEFF

## get data in form to measure impact of yoga
## at each gym without writing every var name
# how to get success prob diff by gym
drawsDF %>%
  sample_n(2) %>%
  select(coeff_1,coeff_2,intercept_1,intercept_2) %>%
  ##deleta above two lines out after digesting code
  mutate(drawNum = row_number()) %>% ## get unique row id
  gather("key","value",-drawNum) %>%
  separate("key",into = c("varName","gymID")) %>%
  spread(varName,value) %>%
  mutate(succProbDiff = getSuccProbDiff(intercept,coeff))

postDF = drawsDF %>%
  select(-contains("mu"), -contains("sigma")) %>%
  mutate(drawNum = row_number()) %>%
  gather("key","value",-drawNum, ) %>%
  separate("key",into = c("varName","gymID")) %>%
  spread(varName,value) %>%
  mutate(succProbDiff = getSuccProbDiff(intercept,coeff)) %>%
  select(drawNum,gymID,succProbDiff) %>%
  mutate(gymID = paste0("gym",gymID)) %>%
  spread(gymID,succProbDiff) %>%
  select(-drawNum)

postDF %>% gather()

(plotDF = postDF %>% gather() %>%
  group_by(key) %>%
  summarize(q05 = quantile(value, probs = 0.05),
            q50 = quantile(value, probs = 0.5), #median
            q95 = quantile(value, probs = 0.95))
)

## Visualize Credible Intervals
plotDF %>%
  ggplot(aes(y = key, yend = key)) +
  geom_segment(aes(x = q05, xend = q95),
               color = "#11114e", size = 4) +
  geom_point(aes(x = q50), color = "purple", size = 4) +
  theme_minimal(26) +
  xlab("Conversion Rate Inc Due to Yoga") +
  ylab("Gym")

## Order axis to be meaningful
(succProbPlot = plotDF %>%
  mutate(key = fct_reorder(key,q50)) %>%
  ggplot(aes(y = key, yend = key)) +
  geom_segment(aes(x = q05, xend = q95),
               color = "#11114e", size = 4) +
  geom_point(aes(x = q50), color = "white", size = 4) +
  theme_minimal(26) +
  xlab("Conversion Rate Inc Due to Yoga") +
  ylab("Gym")
)

## Put results in context
gridExtra::grid.arrange(histDataPlot,succProbPlot, ncol = 2)


## One last thing - we are not restricted to
## marginal distributions of single variables
## can look at relationship between variables
drawsDF %>%
  select(contains("10")) %>%
  ggplot(aes(x=coeff_10,y=intercept_10)) +
  geom_point(alpha = 0.2)
## CLASS QUESTION:  Why is there a
## negative relationship between intercept
## and coeff at gym #9?

## other pair plots can be different
drawsDF %>%
  select(coeff_3,coeff_10) %>%
  ggplot(aes(x=coeff_3,y=coeff_10)) +
  geom_point(alpha = 0.2)
