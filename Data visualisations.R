library(tidyverse)
library(lubridate)
library(nycflights13)
flights


flightsEdited <- flights %>%
  mutate(date=make_date(year,month,day))

flightsEdited %>%
  select(year, month, day, date) %>%
  head

daily <- flightsEdited %>%
  group_by(date) %>%
  summarise(n=n())

head(daily)


ggplot(daily, aes(date, n)) + geom_line()

updatedFlightsEdited <- flightsEdited %>%
  mutate(weekday=wday(date, label=TRUE)) %>%
  mutate(month=month(date, label=TRUE))

head(updatedFlightsEdited)


#######



data("mtcars")
str(mtcars)

mcor <- cor(mtcars)
round(mcor, digits=2)

install.packages("corrplot")
library(corrplot)

corrplot(mcor)


library(GGally)

ggcorr(mtcars)




nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")
head(nba)

nba <- nba[,2:20]

nbaMatrix <- data.matrix(nba)

heatmap(nbaMatrix, Rowv = NA, Colv = NA, col=heat.colors(256), scale = "column", margins = c(5,10))






install.packages("igraph")
library(igraph)




gDirected<- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
gDirected

plot(gDirected)



gUndirected <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed=FALSE)
gUndirected
plot(gUndirected)





nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header = T, as.is = T)
head(nodes)

links<-read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(links)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net
V(net)$media
E(net)

plot(net, edge.arrow.size=.4, vertex.label=V(net)$media, vertex.color=V(net)$media.type)


##########


install.packages("tmap")
library(tmap)
library(rgdal)

















