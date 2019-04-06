library(dplyr)
library(ggplot2)

## 1. Filter function ##

df = data.frame(
  color = c("blue", "black", "blue", "blue", "black"),
  value = 1:5
)
filter(df, color == "blue")

#Find all flights to SFO or OAK
filter(flights, dest =="SFO" | dest == "OAK")
filter(flights, dest %in% c("SFO","OAK"))

#In January
filter(flights, date < "2011-02-01")

#Departed between midnight and 5AM
filter(flights, hour >= 0, hour <= 5)
filter(flights, hour >= 0 & hour <= 5)

#Delayed by more than an hour
filter(flights, dep_delay > 60)

#Arrival delay : twice as much as departure delay (lost time during flight)
filter(flights, arr_delay > 2* dep_delay)


## SELECT function ##
select(df, color)
select(df, -color)
?select

#useful functions
#starts_with(), ends_with(), contains()
#matches()
#num_range()
#one_of()
#everything()

#select 2 delay variables
select(flights, arr_delay, dep_delay)
select(flights, arr_delay:dep_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))

## 2. Arrange function ##
arrange(df, color)
arrange(df,desc(color)) #order desc order by color

#order flights by dep date & time
arrange(flights, date, hour, minute)

#which flights were most delayed
arrange(flights, desc(dep_delay))
arrange(flights, desc(arr_delay))

#which flights caught up the most time during the flight
arrange(flights, desc(arr_delay - dep_delay))


## 3. Mutate function ##
mutate(df, double = 2 * value)
mutate(df, double = 2 * value, quadruple = 2 * double)

#compute speed in mph from time (minutes) and distance (miles)
#which flight flew the fastest?

flights = mutate(flights, speed = dist/(time/60))
arrange(flights,desc(speed))
View(arrange(flights,desc(speed)))

#add a new variable that shows how much time was made up or lost in flight
mutate(flights, delta = abs(dep_delay - arr_delay))

#How did I compute hour and minute from dep?
mutate(flights,
       hour = dep %/% 100,
       minute = dep%% 100)

## 4. Group by and Summarize ##
summarise(df, total = sum(value))
summarise(df, total = mean(value))

#compute sum/mean by group
by_color = group_by(df, color)
summarise(by_color, total = sum(value))


#option 1
summarise(by_color, mean = mean(value))

#another way to do this using pipelines

#option 2
meanval = df %>%
  group_by(color) %>%
  summarise(avg = mean(value))
meanval





#Useful "group by" statements for flight data
by_date = group_by(flights, date)
by_hour = group_by(flights, date, hour)
by_plane = group_by(flights, plane)
by_dest = group_by(flights, dest)

#summary functions
#min(x), median(x), max(x), quantile(x, p)
#n(), n_distinct(x), sum(x), mean(x)
#sum(x>10), mean(x>10)
#sd(x), var(x), IQR(x), mad(x)

summarise(filter(by_date),
          med = median(dep_delay, na.rm=TRUE),
          mean = mean(dep_delay, na.rm=TRUE),
          max = max(dep_delay, na.rm=TRUE),
          q90 = quantile(dep_delay, 0.9, na.rm=TRUE))
#na.rm => Exclude missing values
#or just do it this way
summarise(filter(by_date, !is.na(dep_delay)),
          med = median(dep_delay, na.rm=TRUE),
          mean = mean(dep_delay, na.rm=TRUE),
          max = max(dep_delay, na.rm=TRUE),
          q90 = quantile(dep_delay, 0.9),
          q95 = quantile(dep_delay, 0.95),
          delayed = mean(dep_delay > 0),
          delay15 = mean(dep_delay > 15))
#or you can also do this for arr_delay/difference between the two delays

## DATA PIPELINES ##

#Why use it?

#downside of functional interface:
#difficult to read multiple operations

#ex)
hourly_delay = filter(
  summarise(
    group_by(
      filter(
        flights,
        !is.na(dep_delay)
      ),
      date, hour
    ),
    delay = mean(dep_delay),
    n = n()
  ),
  n > 10
)
#hourly_delay

#Solutioin: pipe operator from magrittr
# x %>% f(y) -> f(x,y)

hourly_delay = flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date, hour) %>%
  summarise(delay = mean(dep_delay), n = n()) %>%
  filter(n > 10)

#Hint: pronounce %>% as 'then'.


#Practice
#1. Which destinations have the highest avg delays?

highest_avg_delay = flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay), n = n()) %>%
  filter(n > 10) %>%
  arrange(desc(delay))

highest_avg_delay
#useful: command + shift + p = rerun the last code

# Note: group_by is the unit of interest in the analysis.
  #1. dest, 
  #2. carrier, flight, dest
  #3. 


#2. Which flights (carrier + flight) happen every day? Where do they fly to?

flights %>%
  group_by(carrier,flight,dest) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n == 365)

flights %>%
  group_by(carrier, flight, dest) %>%
  tally(sort = TRUE) %>% #save some typing
  filter(n == 365)

#best solution: since there could be two flights in one day, and no flight the day after.
flights %>%
  group_by(carrier,flight,dest) %>%
  summarise(n = n_distinct(date)) %>%
  arrange(desc(n)) %>%
  filter(n == 365)
  
#3. On average, how do delays (of non-cancelled flights) vary over the course of a day?
#hint: use hour+minute / 60

per_hour = flights %>%
  filter(cancelled == 0) %>%
  mutate(time = hour + minute / 60) %>%
  group_by(time) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE), n = n())

#visualize it
qplot(time, arr_delay, data = per_hour)
#outlier: ones from the previous night.
#hardly any flights leaving around midnight.

#make points proportional to the number of observations
qplot(time, arr_delay, data=per_hour, size=n) + scale_size_area()

#filter it and add other stuff
qplot(time, arr_delay, data = filter(per_hour, n>30), size = n) + scale_size_area()

#use ggplot fn for better visualization
ggplot(filter(per_hour, n>30), aes(time, arr_delay)) +
  geom_vline(xintercept = 5:24, colour = "white", size=2) +
  geom_point()
#delays are cumulative over the course of the day
#weird pattern of drops.

#if you want to leave on time: fly early in the day


## GROUPED mutate/filter ##

#setup
planes <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(plane) %>%
  filter(n() >30)

#group often used with summary BUT there's also
#groupwise transformation, groupwise filter

#Ex) find the z score = (x - mean(x) ) / sd(x)

#z score for each PLANE
planes %>%
  mutate(z_delay =
           (arr_delay - mean(arr_delay)) / sd(arr_delay)) %>%
  filter(z_delay > 5)

#Aggregation function: n inputs -> 1 output
#Window function: n inputs -> n outputs. Considers all data at the same time.
  # - ranking and ordering
  # - offsets: lead & lag
  # - cumulative aggregates
  # - rolling aggregates

#difference betw min_rank(), row_number(), dense_rank() ? dealing with ties.

#For each plane, find the two most delayed flights. Which of the three rank functions is most appropriate?

min_rank(c(1, 1, 2, 3))
dense_rank(c(1, 1, 2, 3)) # doesn't skip the numbers
row_number(c(1, 1, 2, 3)) #ignores ties.

flights %>% group_by(plane) %>% filter(row_number(desc(arr_delay)) == 2)
flights %>% group_by(plane) %>% filter(min_rank(desc(arr_delay)) == 2)
flights %>% group_by(plane) %>% filter(dense_rank(desc(arr_delay)) == 2)

#arrange -> copies the entire dataset. The most expensive operation/"verb".
#try to avoid using arrange with Big data.

#Lead and Lag
#what is the change over time??
#what is the difference between today and yesterday?

daily = flights %>%
  group_by(date) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
#easiest: use "lag"
daily %>% mutate(delay - lag(delay)) #calculates diff between each day
#diff => takes an input of "n" observations and output of "n-1" observations.
#lag => takes an input of length "n" and output of length "n"

#If not ordered by date already
daily %>% mutate(delay - lag(delay), order_by = date)

location = airports %>%
  select(dest=iata, name = airport, lat, long)
location


delay = flights %>%
  group_by(dest) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  arrange(desc(arr_delay))

delay

#use inner join to join to get lan and long

delay = flights %>%
  group_by(dest) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  arrange(desc(arr_delay)) %>%
  left_join(location)
delay

#put it on a map
ggplot(delay, aes(long, lat)) +
  borders("state") +
  geom_point(aes(colour = arr_delay), size = 5, alpha = 0.9) +
  scale_colour_gradient2() +
  coord_quickmap()

delay %>% filter(arr_delay < 8)


#Joins Practice
x = data.frame(
  name = c("John", "Paul", "George", "Ringo", "Stuart", "Pete"),
  instrument = c("guitar", "bass", "guitar", "drums", "bass", "drums")
)

y = data.frame(
  name = c("John", "Paul", "George", "Ringo", "Brian"),
  band = c("TRUE", "TRUE", "TRUE", "TRUE", "FALSE")
)

#Joins
inner_join(x,y)
left_join(x,y)

#doesn't add any columns to x. Gives the rows that match y
semi_join(x,y)

#opposite of semi_join = anti_join. Give all the rows that DON'T have a match in y
anti_join(x,y)

#right join / outer join (Not implemented in dplyr 2014.)
 
#Try combining hourly delay data with weather information

hourly_delay = flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date, hour) %>%
  summarise(delay = mean(dep_delay), n = n()) %>%
  filter(n > 10)

#are delays correlated with weather?
delay_weather = hourly_delay %>% left_join(weather)
delay_weather

#What weather conditions are associated with delays leaving in Houston?

qplot(temp, delay, data = delay_weather)
qplot(dew_point, delay, data = delay_weather)
qplot(humidity, delay, data = delay_weather)
qplot(pressure, delay, data = delay_weather)
qplot(visibility, delay, data = delay_weather)
qplot(wind_dir2, delay, data = delay_weather)
qplot(wind_speed, delay, data = delay_weather)

#boxplots
qplot(conditions, delay, data=delay_weather, geom="boxplot") + coord_flip()
qplot(events,delay, data = delay_weather, geom="boxplot") + coord_flip()

#Are older planes more likely to be delayed?


#do() function = equivalent to ddply() and dlply()
#apply a function.

