## ------------------------------------------------------------------------
## Uncomment the install lines if the package has not
## been installed on your computer.
#install.packages("nycflights13", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
library(nycflights13)
library(dplyr)

### this is just showing the last line of the ppt
starwars[1:10,] %>%
  select(name, species) %>%
  mutate(isDroidFlag = 
           ifelse(species == "Droid", TRUE, FALSE)) %>%
  summarize(countDroids = sum(isDroidFlag, na.rm = TRUE),
            pctDroids = mean(isDroidFlag, na.rm = TRUE))
            

## Now demo dplyr functionality with data from nycflights13
## Show the dataframe in the RStudio envirnoment
flights = flights

## just fyi, there are lots of datasets already in R
data()

## ------------------------------------------------------------------------
filter(flights, day == 1, month == 1)


####{Class Exercise:} Create a new data frame called uaFlights that finds all flights where the carrier was United Airlines.



## ------------------------------------------------------------------------
arrange(flights, year, month, day)

## ------------------------------------------------------------------------
arrange(flights, desc(arr_delay))

## ------------------------------------------------------------------------
# Select columns by name
select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

## ------------------------------------------------------------------------  get unique row values
select(flights, tailnum) %>% distinct()

####{Class Exercise:} In a separate script, create a new data frame called routes that consists of two columns which contain all combinations of flight origin and flight destination in the original dataset.  How many unique routes are there?




## ------------------------------------------------------------------------
flightSpeedDF = select(flights, distance, air_time)
mutate(flightSpeedDF,
  speed = distance / air_time * 60)

## ------------------------------------------------------------------------
summarise(flights,
  delay = mean(dep_delay, na.rm = TRUE))

## ------------------------------------------------------------------------
## creater new dataframe that is organized by groups
destinations = group_by(flights, dest)
destDF = summarise(destinations,
  planes = n_distinct(tailnum), #unique planes
  flights = n() # number of flights
)
destDF

####{Class Exercise:} In a separate script, create a new data fram called sortDestDF that orders (i.e. arranges the destDF dataframe in descending order of popularity(i.e. number of flights from NYC to that destination) to discover the most popular places people from New York City fly to.




## ------------------------------------------------------------------------
lightSpeedDF = select(flights, distance, air_time, dest)
lightSpeedDF2 = mutate(lightSpeedDF,
  speed = distance / air_time * 60)
lightSpeedDF3 = group_by(lightSpeedDF2, dest)
lightSpeedDF4 = summarize(lightSpeedDF3, avgSpeed = mean(speed, na.rm = TRUE))
arrange(lightSpeedDF4, desc(avgSpeed))


##The above is challenging code to read.
## Here is the nice chained version using the chaining
## operator (i.e. %>%, aka pipe, aka "then")
flights %>%
  select(distance, air_time, dest) %>%
  mutate(speed = distance / air_time * 60) %>%
  group_by(dest) %>%
  summarize(avgSpeed = mean(speed, na.rm = TRUE)) %>%
  arrange(desc(avgSpeed))

####{Class Exercise:} In a separate script, use the chaining operator, %>%, to find which of the New York City airports experience the highest average departure delay.

## Back to PPT
