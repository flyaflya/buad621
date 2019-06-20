library(tidyverse)
library(causact)
set.seed(123)

numFlips = 7000
df = data.frame(flipNum = 1:numFlips,
                coinFlip = rbern(n=numFlips,prob=0.5)) %>%
  mutate(headsProportion = cummean(coinFlip))

## show all proportions
ggplot(df, aes(x = flipNum, y = headsProportion)) + 
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0.5, color = "red") + 
  theme_minimal(20) + ylim(0,1) +
  ylab("Cumulative Heads Proportion")

## Class Exercise####
## Experiment with different values of numFlips above
## How many flips of a coin are needed to guarantee a
## representative sample?

## zoom in on 45% to 55%
ggplot(df, aes(x = flipNum, y = headsProportion)) + 
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0.5, color = "red") + 
  theme_minimal(20) + ylim(0.40,0.60) +
  ylab("Cumulative Heads Proportion")
