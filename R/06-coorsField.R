###read in baseball data
## uncomment the below if you need to install these two packages for the first time
#install.packages("ggplot2")
#install.packages("forcats")
library(dplyr)
library(ggplot2)
library(forcats)
### fyi, all above packages can be loaded using library(tidyverse)


###let's see if Coors field is a "run-friendly" baseball park
browseURL("http://en.wikipedia.org/wiki/Coors_Field")


###the following line loads data from the 2010 - 2014 baseball seasons.  The following data gets loaded:
# Date      :     The date the baseball game was played
# Home      :     A three letter code indicating the "home" team
# Visitor   :     A three letter code indicating the "visiting" team
# HomeScore :     # of runs scored by the home team
# VisitorScore :  # of runs scored by the visiting team
###note: If HomeScore > VisitorScore, then the Home team wins the game.
###      If HomeScore < VisitorScore, then the Visitor team wins the game.
load("data/baseball.RData")   ###ensure this file is in your working directory

####Manipulate Data to Match Purpose
baseballData2 = baseballData %>% 
  mutate(totalRuns = HomeScore + VisitorScore) %>% 
  group_by(Home) %>% 
  summarise(avgRuns = mean(totalRuns)) %>% 
  arrange(desc(avgRuns))

### Let's choose our axes to be the two most important content features
### we do this via aesthetic mapping (see cheatsheet)
ggplot(data = baseballData2, aes(x = Home, y = avgRuns))

## mapping points to aesthetic using plot default
## of x = Home and y = avgRund
ggplot(data = baseballData2, aes(x = Home, y = avgRuns)) +
  geom_point()

ggplot(data = baseballData2) + ## mapping just for pts
  geom_point(aes(x = Home, y = avgRuns))  

###Change structure to highlight the differences
ggplot(data = baseballData2, aes(x = Home, y = avgRuns)) +
  geom_col()

###Explore reversing the axes
###the x data is now vertical and the y data horizontal
ggplot(data = baseballData2, aes(x = Home, y = avgRuns)) +
  geom_col() +
  coord_flip()

###Highlight Coors Field - need data first!!
###This is a common recipe.  If you want to map a visual
###element to data, you often have to create the data
baseballData3 = baseballData2 %>% 
  mutate(CoorsField = ifelse(Home == "COL","Coors Field","Other Stadium"))

### then map data to a plot aesthetic
ggplot(data = baseballData3,
       aes(x = Home,y = avgRuns,fill = CoorsField)) +
  geom_col() +
  coord_flip()

###try with reordering vertical axis labels (i.e. x-axis due to coord_flip())
###to highlight difference among teams
### Notice the Home is a factor
### R uses factors to handle categorical variables, 
### variables that have a fixed and known set of possible values. 
### we have avoided these by using stringsAsFactors = FALSE
### when plotting though, it is nice to control the order of levels

## two most important functions
## forcats::fct_reorder()   -  Reordering a factor by another variable
## forcats::relevel()   -  Reordering a factor by another variable
ggplot(data = baseballData3, 
       aes(x = fct_reorder(Home,avgRuns), ##use fct_reorder()
           y = avgRuns, 
           fill = CoorsField)) + 
  geom_col() + 
  coord_flip()

## create new order where CO in middle
newLevelOrder = c("SEA","LAN","NYN","PIT","MIN","MIL","KCA","FLO","CLE","SLN","ANA","OAK","ARI","TOR","BOS","COL","TEX","DET","NYA","BAL","CHA","CHN","HOU","CIN","PHI","WAS","MIA","ATL","TBA","SFN","SDN")

ggplot(data = baseballData3, 
       aes(x = fct_relevel(Home,newLevelOrder), ##use fct_relevel()
           y = avgRuns, 
           fill = CoorsField)) + 
  geom_col() + 
  coord_flip()

###work on formatting... try points instead of bars
ggplot(data = baseballData3, 
       aes(x = fct_reorder(Home,avgRuns), 
           y = avgRuns, 
           fill = CoorsField)) + 
  geom_point() + ## different line
  coord_flip()

###color the points
ggplot(data = baseballData3, 
       aes(x = fct_reorder(Home,avgRuns), 
           y = avgRuns)) + 
  geom_point(aes(color = CoorsField)) + 
  coord_flip()

###make the points bigger
ggplot(data = baseballData3, 
       aes(x = reorder(Home,avgRuns), 
           y = avgRuns)) + 
  geom_point(aes(color = CoorsField), size = 4) + ###size not mapped to data so outside of aes()
  coord_flip()

###map point size to the avgRuns data
ggplot(data = baseballData3, 
       aes(x = reorder(Home,avgRuns), 
           y = avgRuns)) + 
  geom_point(aes(color = CoorsField, size=avgRuns)) + ###size mapped to data so inside of aes()
  coord_flip()

###to take control of how data is mapped to the visual aesthetic
###USE SCALES ... you will need to use google as your friend
## for specific inquiries (e.g. see http://www.cookbook-r.com/Graphs/)
ggplot(data = baseballData3, 
       aes(x = reorder(Home,avgRuns), 
           y = avgRuns)) + 
  geom_point(aes(color = CoorsField, size=avgRuns)) + 
  coord_flip() +
  scale_color_manual(values = c("red","purple")) + # NEW LINE
  scale_size(range = c(1,9), guide = FALSE) # NEW LINE


### save the last plot as an object
myPlot = last_plot()  ##ggplot function to get last plot made

###add thin bars to points
myPlot + 
  geom_col()

###plot points on top of lines by adding point layer after
###geom_col layer.  
myPlot + 
  geom_col() +
    geom_point(aes(color = CoorsField, size=avgRuns))

###start editing smaller details
myPlot + 
  geom_col(fill = "black",color = "black", width = 0.1) +
  geom_point(aes(color = CoorsField, size=avgRuns)) + 
  labs(title = "How Run Friendly is Colorado's Coors Field?",
       x = "Ballpark",
       y = "Avergage Number of Runs Per Game") 

myPlot2 = last_plot()

## try different themes
# install.packages("ggthemes") # Install 
library(ggthemes) # Load
myPlot2 + ## ggplot function to give last plot
  theme_economist() + 
  scale_color_economist()

## others to explore
myPlot2 + theme_stata() + scale_color_stata()
myPlot2 + theme_wsj()+ scale_colour_wsj("colors6")

#### CLASS EXERCISE:  a skeptical investigator claims that
#### Colorado's team is the reason for the high runs in that
#### park.  To investigate this, look at the average runs
#### scored by the visiting team only (i.e. exclude the home team)


