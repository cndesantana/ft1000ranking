url <- "https://www.ft.com/content/8b37a92b-15e6-4b9c-8427-315a8b5f4332"

library(httr)
library(rvest)
library(tidyverse)
library(leaflet)
library(rworldmap)
library(mapproj)
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)

##### SCRAPING PART

#download data
financial_times <- read_html(url)
table <- financial_times  %>% html_node("table")%>%html_table(fill=TRUE)
table<- table[,1:13]
table <- table[-1001,]
#get the   ranking  of countries
ranking <- table %>% count(Country) %>% ungroup()
ranking$Country[ranking$Country=="UK"] <- "United Kingdom"
ranking$Country[ranking$Country=="Czech Republic"] <- "Czech Rep."


##### MAPPING PART


#get the world map
worldMap <- getMap()
#match  the countries in the world map  with the ones in  the ranking 
indEU <- which(worldMap$NAME%in%ranking$Country)
europeCoords <- lapply(indEU, function(i){
df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
df$country =as.character(worldMap$NAME[i])
colnames(df) <- list("long", "lat", "country")
return(df)
})
europeCoords <- do.call("rbind", europeCoords)
europeCoords$startups <-  ranking$n[match(europeCoords$country,ranking$Country)]

#preparing the map
P <- ggplot() + 
geom_polygon(data = europeCoords, aes(x = long, y = lat, group = country, fill = startups),colour = "black", size = 0.1) +
coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

#P <- P + scale_fill_gradient(name = "# of startups", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")
P <- P + scale_fill_viridis_c(option = "plasma") 
P <- P + theme(
axis.text.x = element_blank(),
axis.text.y = element_blank(), axis.ticks.x = element_blank(),
axis.ticks.y = element_blank(), axis.title = element_blank(),
plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
P <- P + labs(title= "Countries with more startups at FT1000's ranking", fill = "# of startups")
ggplotly(P)
