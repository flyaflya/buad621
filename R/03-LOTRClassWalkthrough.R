# install.packages("tidyverse")
library(tidyverse)

## use read_xxx() functionality from readr package (part of tidyverse)
lotr_dat <- read_tsv(file.path("data", "lotr_clean.tsv"), col_types = cols(
  Film = col_character(),
  Chapter = col_character(),
  Character = col_character(),
  Race = col_character(),
  Words = col_integer()  ## this defaults to double otherwise
))

## bring in data about gender in LOTR
females <- c("Galadriel", "Arwen", "Lobelia Sackville-Baggins", "Rosie",
             "Mrs. Bracegirdle", "Eowyn", "Freda", "Rohan Maiden")

## add gender data to the lotr_dat data frame
lotr_dat <- mutate(lotr_dat,
                   Gender = ifelse(Character %in% females, 
                                   "Female", 
                                   "Male"))


## Assuming we are interested in knowing how vocal each race 
## and gender are in each of the movies  
## To study this, we need one observation of word count
## per each possible combination of film/gender/race

## Coding Tip: 
## Enclosing the entire expression in parantheses prints the LHS object
## in addition to creating the new data frame (LHS = "left hand side")

## make a tidy dataframe of Film, Gender, Race, Words
(lotr_tidy <- lotr_dat %>%
    group_by(Film, Gender, Race) %>%
    summarize(Words = sum(Words)))

## the above includes certain combinations, like Female Hobbit
## in the Two Towers ... let's be explicit with our zeroes.
## get all combinations of film/gender/race
(all_combns <- lotr_tidy %>% 
    ungroup() %>%   ###otherwise it does within group expansion
    expand(Film, Gender, Race))

## use left_join to get all data from the left Data Frame
## and combine it with the word count data from the right data frame
lotr_tidy <- all_combns %>% 
  left_join(lotr_tidy) 

## change NA values to 0 and specify sort order
lotr_tidy <- lotr_tidy %>%
  mutate(Words = ifelse(is.na(Words),0,Words)) %>% 
  arrange(Film, Race, Gender)

## CLASS EXERCISE#1:  Do this in a new script window File -> New Script
## Question:  Does a certain race dominate a movie (in terms of words spoken)?  Does the dominant `Race` differ across the movies?

# ----------------------------------------

## CLASS EXERCISE#2:  Do this in a new script window File -> New Script
## MAKE TWO TIDY DATAFRAMES (df'S): 
## #1: df OF FILM/CHARACTER/WORD COUNT OBSERVATIONS
## ANSWER THE FOLLOWING QUESTIONS USING DPLYR CODE:
#     1. WHO ARE THE TOP 5 CHARACTERS BY WORD COUNT FOR EACH MOVIE
#     2. WHO ARE THE TOP 5 CHARACTERS BY WORD COUNT ACROSS ALL MOVIES
## #2: df OF ALL CHARACTERS AND THEIR RACE
##    3. USE LEFT_JOIN AND OTHER DPLYR FUNCTIONS
##       TO FIND THE TOP 5 HOBBITS BY WORD COUNT ACROSS ALL MOVIES

# ----------------------------------------

# RESOURCE:  more info at ### Resources http://r4ds.had.co.nz/tidy-data.html chapter in R for Data Science, by Garrett Grolemund and Hadley Wickham

##### GATHERING DATA
### Assume your data lives in three different files
fship <- read_csv(file.path("data", "The_Fellowship_Of_The_Ring.csv"))
ttow <- read_csv(file.path("data", "The_Two_Towers.csv"))
rking <- read_csv(file.path("data", "The_Return_Of_The_King.csv")) 
rking

## We now have one data frame per film, 
## each with a common set of 4 variables. 
## Step one in tidying this data is to glue 
## them together into one data frame, stacking 
## them up row wise. This is called row binding
lotr_untidy <- bind_rows(fship, ttow, rking)
lotr_untidy  ## notice more than one observation per row
# this might be good for humans to read, but computers 
# like tidy data ... making untidy data for presentation
# or graphical purposes is the last step in your data workflow

## We are still violating one of the fundamental principles of __tidy data__. ## "Word count" is a fundamental variable in our dataset and it's currently ## spread out over two variables, `Female` and `Male`. Conceptually, we need to ## gather up the word counts into a single variable and create a new variable, ## `Gender`, to track whether each count refers to females or males. We use the ## `gather()` function from the tidyr package to do this.

lotr_tidy <- lotr_untidy %>%
  gather(key = 'Gender', value = 'Words', Female, Male)
lotr_tidy
## ```
## 
## Tidy data ... mission accomplished!
## 
## To explain our call to `gather()` above, 
## let's read it from right to left: we 
## took the variables `Female` and `Male` and 
## gathered their *values* into a 
## single new variable `Words`. This forced  
## the creation of a companion variable 
## `Gender`, a *key*, which tells whether a specific 
## value of `Words` came from 
## `Female` or `Male`. 
## All other variables, such as `Film`, 
## remain unchanged and 
## are simply replicated as needed. 

## ---------------------------------------------

## CLASS EXERCISE#3:  Open new script window
## Some word count data is given in these two __untidy__ and gender-specific files:
#       1. [Female.csv](data/Female.csv)
#       2. [Male.csv](data/Male.csv)
#  Use the read_csv function to read both files.
#  Combine the files into a tidy data frame using gather()

## ---------------------------------------------

## SPREAD() is Opposite of GATHER()  ... here are some examples

## rememeber what the tidy data looks like
lotr_tidy

## practicing with spread: let's get one variable per Race
lotr_tidy %>% 
  spread(key = Race, value = Words)

## practicing with spread: let's get one variable per Gender
lotr_tidy %>% 
  spread(key = Gender, value = Words)

## these non-tidy dataframes can make for good tables and
## visualizations as needed.  Humans like them.
## For your computer though, store tidy data.


