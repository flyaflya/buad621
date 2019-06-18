library(tidyverse)
library(readxl) # part of tidyverse, but needs separate library command as it is less used

#########################
### GET RAW DATA ########

## read in data
# attempt 1 - wrong sheet
read_excel("data/christmasTrees.xlsx",sheet = "Data",skip=4)

# attempt 2 - too many rows
read_excel("data/christmasTrees.xlsx",sheet = "Data")

# attempt 3 - almost right
read_excel("data/christmasTrees.xlsx",sheet = "Data",skip=4)

# attempt 4 - get rid of spaces in names ... they can cause trouble
read_excel("data/christmasTrees.xlsx",sheet = "Data",skip=4,.name_repair = "universal")

# attempt 5 - perfect, except year is not named well
read_excel("data/christmasTrees.xlsx",sheet = "Data",skip=4,.name_repair = "universal") %>%
  mutate(year = ...1)

# attempt 6 - treat year as integer to get perfection
read_excel("data/christmasTrees.xlsx",sheet = "Data",skip=4,.name_repair = "universal") %>%
  mutate(year = as.integer(...1)) %>%
  select(year,Real.trees,Fake.trees)

# save attempt 6 to df
treeDF = read_excel("data/christmasTrees.xlsx",sheet = "Data",skip=4,.name_repair = "universal") %>%
  mutate(year = as.integer(...1)) %>%
  select(year,Real.trees,Fake.trees)

###########################
### CLASS EXERCISE ########

### Replace the ??? in the skeleton code below to
### create a dataframe named tidyDF that is a tidy version
### of treeDF.  Use the gather() function and name the key
### treeType and the value treesSoldInMillions

tidyDF = treeDF %>% 
  gather(key = "???",
         value = "???",
         ???,
         ???)



#### this comment should be on line 50

###########################
### Slow Plotting ########

# plot 1 - associate dataframe with ggplot
ggplot(data = tidyDF)

# plot 2 - map year data to x-axis
ggplot(data = tidyDF) +
  aes(x = year)

# plot 3 - map trees Sold data to y-axis
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions)

# plot 4 - map trees Sold data to y-axis
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions)
  
# plot 5 - choose a visual marker to represent data
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point()  ## this is a layer

# plot 6 - map the color attrbute to treeType data
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point() +
  aes(col = treeType)

# plot 7 - add a layer that includes both visual marker (line) and data transformation
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point() +
  aes(col = treeType) +
  geom_smooth()

# plot 8 - change layer arguments to get line and remove uncertainty band
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point() +
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F)

# plot 9 - choose colors to help audience digest data quicker
# hint: use colors() to see list of available colors.
# helpful site: https://www.datamentor.io/r-programming/color/
colors()
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point() +
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = c("darkgreen","green")) 
# scales are used to override default mapping of data to visual attributes like color and size

# plot 10 - be more explicit about scale to make the unnatural color
# associated with the fake trees.  Store colors in a data frame or tibble
treeDataDF = tibble(
  treeType = c("Real.trees","Fake.trees"),
  color = c("darkgreen","magenta"),
  labels = c("Real Trees","Tree Look-A-Likes")  ## add while here
)

# use tibble::deframe to create a "named" vector of colors
namedColorVector = treeDataDF %>% select(treeType,color) %>% deframe()
## yes, the above feels like a cumbersome workflow
## but, controlling colors is important for audience ease

ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) +  ## add aize argument to geom_point
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector)

# plot 11 - create limits on y-axis
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) +
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector) +
  ylim(c(0,35))

# plot 12 - remove legend label
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) +  
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector) +
  ylim(c(0,35)) +
  labs(col = "")

# plot 13 - add title label
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) + 
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector) +
  ylim(c(0,35)) +
  labs(col = "") +
  labs(title = "When will the fake trees be more popular?") +
  labs(subtitle = "Real and fake Christmas trees sold in the US | Data Source: Statista | @EvaMaeRey ")

# plot 14 - use theme to alter the overall look and feel
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) + 
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector) +
  ylim(c(0,35)) +
  labs(col = "") +
  labs(title = "When will the fake trees be more popular?") +
  labs(subtitle = "Real and fake Christmas trees sold in the US | Data Source: Statista | @EvaMaeRey ") +
  theme_minimal(base_size = 14)

# plot 15 - change legend labels using data from treeDataDF 
# by adding argument to scale_color_manual
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) + 
  aes(col = treeType) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector,labels = treeDataDF$labels) +
  ylim(c(0,35)) +
  labs(col = "") +
  labs(title = "When will the fake trees be more popular?") +
  labs(subtitle = "Real and fake Christmas trees sold in the US | Data Source: Statista | @EvaMaeRey ") +
  theme_minimal(base_size = 14)


# plot 16 - getting really picky...
ggplot(data = tidyDF) +
  aes(x = year) +
  aes(y = treesSoldInMillions) +
  geom_point(size = 4) +  
  aes(col = forcats::fct_rev(treeType)) +  ## control factor for reordering legend
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = namedColorVector,labels = treeDataDF$labels) +
  ylim(c(0,35)) +
  labs(col = "") +
  labs(title = "When will the fake trees be more popular?") +
  labs(subtitle = "Real and fake Christmas trees sold in the US | Data Source: Statista | @EvaMaeRey ") +
  theme_minimal(base_size = 14) +
  xlab("YEAR") +
  ylab("Trees Sold (In Millions)")
