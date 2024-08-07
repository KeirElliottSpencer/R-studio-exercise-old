library(ggplot2)
library(WDI)

new_wdi_cache <- WDIcache()

new_wdi_cache<-WDIcache()
education<-WDI(indicator=c("SE.PRM.ENRR","SE.SEC.ENRR",
                           "SE.TER.ENRR","SE.SEC.PROG.ZS","SE.PRM.CMPT.ZS"),
               start=2014,
               end=2014,
               extra= TRUE,
               cache=new_wdi_cache)


education<-education[education$region!="Aggregates",]
education<-na.omit(education)




pca<-prcomp(education[,7:11])

education.pca <- data.frame(
  country=education$country,
  region=education$region,
  PC1 = pca$x[,1],
  PC2 = pca$x[,2]
)


ggplot(education.pca, aes(PC1, PC2, label=country)) +
  geom_text(size = 3) +
  labs(title="PCA of education data", caption = "World Bank")


pca <- prcomp(education[,7:11], scale=TRUE)
education.pca.scaled <- data.frame(
  country=education$country,
  region=education$region,
  PC1=pca$x[,1],
  PC2=pca$x[,2]
)

ggplot(education.pca.scaled, aes(PC1, PC2, label=country)) + 
  geom_text(size = 3) + 
  labs(title="Scaled PCA of education data", caption = "World Bank")


ggplot(education.pca, aes(PC1, PC2)) +
  geom_point(aes(colour=region),size = 3) +
  labs(title = "PCA of education data", caption = "World Bank", colour="Region")


pca <- prcomp(education[,7:11])
education.loading <- data.frame(
  dimensions = colnames(education)[7:11],
  PC1= pca$rotation[,1],
  PC2= pca$rotation[,2]
)

ggplot(education.loading, aes(PC1, PC2, label=dimensions)) + geom_text(size=3) +
  labs(title="Loading plot of education data", caption="World Bank")


#############

library(MASS)

scaled.data <- scale(education[,7:11])
distance.matrix <- dist(scaled.data)

mds <- isoMDS(distance.matrix)
education.mds <- data.frame(
  country = education$country,
  region = education$region,
  MDS1 = mds$points[,1],
  MDS2 = mds$points[,2]
)

ggplot(education.mds, aes(MDS1, MDS2, label=country)) + 
  geom_text(size = 3) +
  labs(title="MDS of education data", caption="World Bank")


distance.matrix <- dist(scaled.data, "canberra")
mds <- isoMDS(distance.matrix)
education.mds <- data.frame( 
  country=education$country,
  region=education$region,
  MDS1=mds$points[,1],
  MDS2=mds$points[,2]
)

ggplot(education.mds, aes(MDS1, MDS2, label=country)) +
  geom_text(size=3) + 
  labs(title="MDS of education data with Canberra distance measure", caption="World Bank")

#######

install.packages("GGally")
library(GGally)

ggparcoord(education, columns = c(7,8,9,10,11), groupColumn = 12) +
  theme(axis.text.x = element_text(angle = 45), panel.background = element_blank()) +
  geom_vline(xintercept = c(1,2,3,4,5)) +
  labs(colour='Region', x='Variables', y='Scaled values', title='Parallel coordinates of education data', caption='World Bank')



install.packages("devtools")
library(devtools)


devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE, force=TRUE)

library(ggradar)
library(scales)
library(tidyverse)

education$year <- NULL

education %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_region = str_replace_all(region, " ", "_")) %>%
  group_by(new_region) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar()

education %>% mutate_if(is.numeric, rescale) %>%
  mutate(new_region = str_replace_all(region, " ", "_")) %>%
  group_by(new_region) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 3, legend.text.size= 8)

?graddar
















