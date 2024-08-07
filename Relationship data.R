library(WDI)
new_wdi_cache <- WDIcache()

library(igraph)
library(tidyverse)

airports <- read_csv("airports.dat", col_types = "icccccdddccccc")
routes <- read_csv("routes.dat")

nodes <- airports %>%
  select(IATACode, Name, City, Country, longitude, latitude) %>%
  filter(IATACode != "\\N") %>%
  filter(Country == "United Kingdom")

links <- routes %>%
  select(Source, Destination, Airline, Plane) %>%
  filter(Source %in% nodes$IATACode) %>%
  filter(Destination %in% nodes$IATACode)

net <- graph_from_data_frame(d=links, vertices = nodes, directed = T)

library(ggnetwork)

ggplot(net, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour="red") +
  geom_nodes(size=3) + 
  geom_text(aes(x=x, y=y, label=City)) + 
  theme_blank() +
  labs(caption='Airport routes')



education <- WDI(indicator = c("SE.PRM.ENRR", "SE.SEC.ENRR",
"SE.TER.ENRR","SE.SEC.PROG.ZS","SE.PRM.CMPT.ZS"), start = 2014, end = 2014, extra = TRUE, cache = new_wdi_cache)

education <- education[education$region!="Aggregates",]
education<-na.omit(education)

education.features <- education[,7:11]

education.features_scaled <- scale(education.features)
education.distance_matrix <- as.matrix(dist(education.features_scaled))
education.adjacency_matrix <- education.distance_matrix < 1.0

g1 <- graph_from_adjacency_matrix(education.adjacency_matrix, mode = "undirected")
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = "blue") + 
  geom_nodes(size = 5, colour = "grey") + 
  theme_blank() +
  labs(caption='WDI School enrollment and progression datasets')

g1 <- set_vertex_attr(g1, name="Region", value=as.character(education$region))
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = "grey") + 
  geom_nodes(size = 5, aes(colour = Region)) + 
  theme_blank() +
  labs(caption='WDI School enrollment and progression datasets')



new.g1 <- ggnetwork(g1, layout = igraph::layout.random(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = "grey") + 
  geom_nodes(size = 5, aes(colour = Region)) + 
  theme_blank() +
  labs(caption='WDI School enrollment and progression datasets')

new.g1 <- ggnetwork(g1, layout = igraph::layout.fruchterman.reingold(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = "grey") + 
  geom_nodes(size = 5, aes(colour = Region)) + 
  theme_blank() +
  labs(caption='WDI School enrollment and progression datasets')

new.g1 <- ggnetwork(g1, layout = igraph::layout.kamada.kawai(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = "grey") + 
  geom_nodes(size = 5, aes(colour = Region)) + 
  theme_blank() +
  labs(caption='WDI School enrollment and progression datasets')




### dendograms

library(ggdendro)

hc <- hclust(dist(education.features_scaled))
ggdendrogram(hc, rotate = T) + coord_flip()


hc <- hclust(dist(education.features_scaled))
dd.row <- as.dendrogram(hc)
ddata <- dendro_data(dd.row)

ggplot(ddata$segments) + 
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend)) + 
  geom_point(data=ddata$labels, aes(x=x,y=y,colour=education$region[as.numeric(as.character(label))])) +
  theme_dendro() +
  labs(colour="region", caption = 'WDI School enrollment and progression datasets')




library(ggalluvial)

education %>%
  group_by(income, region) %>%
  summarise(count=n()) %>%
  ggplot(aes(y=count, axis1 = income, axis2=region)) + 
  geom_alluvium(aes(fill=region)) +
  geom_stratum() +
  geom_label(stat="stratum", aes(label = after_stat(stratum))) + 
  scale_x_discrete(limits=c("Income", "Region")) + 
  theme(panel.background = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  labs(x="",y="", fill="Region")



data("majors")
ggplot(majors, aes(x=semester, stratum=curriculum, alluvium=student, fill=curriculum, label=curriculum)) +
  geom_stratum() +
  geom_flow(stat="alluvium", colour='darkgrey') + 
  labs(fill="Curriculum") + 
  theme(legend.position = "bottom", panel.background = element_blank())



library(tmap)
library(rgdal)

sheffieldShape <- readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")

head(sheffieldShape@data, n=2)
head(sheffieldShape@polygons, n=1)

qtm(sheffieldShape)

tmap_mode("view")
qtm(sheffieldShape)

qtm(sheffieldShape, fill = NULL)

tmap_mode("plot")


library(readxl)

deprivation2015 <- read_excel("Week8.xlsx", sheet = "ID2015 IDACI & IDAOPI")

View(deprivation2015)
deprivation2015Pop <- deprivation2015


deprivation2015Pop <- deprivation2015 %>%
  select(
    names(deprivation2015Pop)[names(deprivation2015Pop)=="LSOA name (2011)"]<-"LSOA_name",
    names(deprivation2015Pop)[names(deprivation2015Pop)=="LSOA code (2011)"]<-"LSOA_code"
    
    )

