library(tidyverse)
library(causact)
set.seed(123)

## Step 0:  Define Question Parameters
purchCost = 1
salesPrice = 4

expDemand = 120
stdDevDemand = 20


## UNCERTAINTY MODEL
## Step 1:  Create representative sample of demand
sampleSize = 4000
simDemand = rnorm(sampleSize,expDemand,stdDevDemand)

## ACTION/DECISION SPACE
## Step 2:  Create a vector of possible decisions
actions = seq(80,160,10) # vector 80,90,100,...160

## OBJECTIVE:  typically, a PROFIT or LOSS FUNCTION
## Step 3:  Create a vector of possible decisions
profitFunction = function(price, cost, demand, orderQty) {
  amtSold = pmin(demand,orderQty)  # can not sell more than you have
  revenue = price * amtSold
  expense = cost * orderQty
  profit = revenue - expense
  return(profit)
}

# test function
profitFunction(price = salesPrice,cost = purchCost,demand = 2,orderQty = 1)
profitFunction(price = salesPrice,cost = purchCost,
               demand = 1,orderQty = 2)
profitFunction(price = salesPrice,cost = purchCost,
               demand = c(7,1),orderQty = c(1,7))


## CALCULATE PROFIT/LOSS FOR ALL POSSIBLE OUTCOME/DECISIONS
## Step 4: Get all combinations of the repSample and decision space
df = cross_df(list(demand = simDemand,order = actions))

## Step 5: Calculate the profit for each demand/order combo
profitDF = df %>%
  mutate(profit = profitFunction(price = salesPrice,
                                 cost = purchCost,
                                 demand = demand,
                                 order = order)) #%>%
  # sample_frac() uncomment this line to see variety

## VISUALIZE DECISION OUTCOMES UNDER UNCERTAINTY
## Step 6: Summarize Credible Intervals
profitSummaryDF = profitDF %>%
  select(order,profit) %>%
  group_by(order) %>%
  summarize(q05 = quantile(profit, probs = 0.05), 
            q45 = quantile(profit, probs = 0.45),
            q50 = quantile(profit, probs = 0.5), #median
            expP = mean(profit), #mean
            q55 = quantile(profit, probs = 0.55),
            q95 = quantile(profit, probs = 0.95))

## Step 7: Visualize Credible Intervals
profitSummaryDF %>%  
  ggplot(aes(y = order, yend = order)) +
  geom_segment(aes(x = q05, xend = q95),
               color = "#11114e", size = 4) +
  geom_segment(aes(x = q45, xend = q55),
               color = "#5f9ea0", size = 4) +
  geom_point(aes(x = q50), color = "purple", size = 4) +
  geom_point(aes(x = expP), color = "white", size = 2) +
  theme_minimal(26) +
  xlab("profit") + 
  ylab("Order Decision") + coord_flip()

## Step 8:  Make a decision
## Questions for Discussion
# Q1:  Which order decision has the highest mean profit?
# Q2:  Which order decision has the highest median profit?
# Q3:  Which order decision(s) have a chance at making $400?
# Q4:  Which order decision ensures at least a $275 profit?
# Q5:  Why do we not see the credible interval for ordering 80 units?

## CLASS EXERCISE:  Change the sales price to $400 and the cost to
##                  $350.  Answer the following
# Q1:  Which order decision has the highest mean profit?
# Q2:  Which order decision has the highest median profit?
# Q3:  Which order decision maximizes the 5th percentile?
# Q4:  Which order decision maximizes the 45th percentile?
# Q5:  Which order decision maximizes the 95th percentile?



# more info found here:  
# http://www.statsathome.com/2017/10/12/bayesian-decision-theory-made-ridiculously-simple/