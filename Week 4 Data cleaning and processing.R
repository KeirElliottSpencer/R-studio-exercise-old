library(tidyverse)
str(mpg)

filter(mpg, manufacturer=="audi")

mpg


filter(mpg, displ > 2)

filter(mpg, displ >= 2)

filter(mpg, displ > 2 & cyl > 6)

filter(mpg, manufacturer == "audi", year == 1999)
filter(mpg, manufacturer == "audi" & year == 1999)

filter(mpg, manufacturer == "audi" | year == 1999)

filter(mpg, manufacturer != "audi" & year == 1999)

filter(mpg, (manufacturer == "audi" | manufacturer == "chevrolet"), year == 1999)
filter(mpg, (manufacturer %in% c("audi","chevrolet")), year == 1999)

filter(mpg, (manufacturer %in% c("audi","chevrolet")), year == 1999) %>%
  count(manufacturer)

set.seed(34)
sample_frac(mpg, 0.05, replace = TRUE)
sample_n(mpg, 10, replace = TRUE)

rio2016medals <- read.csv("Rio2016.csv")

arrange(rio2016medals, Country)
arrange(rio2016medals, desc(Country))

arrange(rio2016medals, desc(Gold), desc(Silver), desc(Bronze)) %>%
  View()


select(mpg, manufacturer, hwy)

select(mpg, starts_with("d"))

select(mpg, manufacturer, hwy) %>%
  filter(manufacturer=="chevrolet" & hwy >= 20)

select(mpg, manufacturer, hwy) %>%
  filter(manufacturer!="chevrolet" & hwy >= 20)

select(mpg, manufacturer, hwy) %>%
  filter(manufacturer != "chevrolet" & hwy >= 20) %>%
  arrange(desc(manufacturer))

mpg %>% select(manufacturer, hwy) %>%
  filter(manufacturer != "chevrolet" & hwy >= 20) %>%
  arrange(desc(manufacturer))


mutate(rio2016medals, Total=Gold+Silver+Bronze)

summarise(mpg, avg=mean(hwy))

group_by(mpg, year, manufacturer) %>%
  summarise(count=n())

group_by(mpg, manufacturer) %>%
  summarise(count=n())


install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

nycflights13::airlines
nycflights13::airports

flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)
flights2

airlines

flights2 %>%
  left_join(airlines, by = "carrier")


meals <- read.csv("freeschoolmeals.csv")
View(meals)

summary(meals$FSMTaken)

mean(meals$FSMTaken)
mean(meals$FSMTaken, na.rm = TRUE)

actualFSMTaken <- meals$FSMTaken[meals$FSMTaken!=9999]
length(actualFSMTaken)
mean(actualFSMTaken, na.rm = TRUE)


filter(meals, FSMTaken < 9999) %>%
  count()

filter(meals, (FSMTaken < 9999 | is.na(FSMTaken))) %>%
  count()


y <- c(4,5,6,NA)
is.na(y)
y[is.na(y)] <- mean(y, na.rm = TRUE)
y

actualFSMTaken[is.na(actualFSMTaken)] <- floor(mean(actualFSMTaken, na.rm = TRUE)) %>%
  View(actualFSMTaken)


filter(meals, (FSMTaken < 9999 | is.na(FSMTaken))) %>%
  mutate(newFSMTaken=ifelse(is.na(FSMTaken), floor(mean(FSMTaken, na.rm = TRUE)),FSMTaken))












