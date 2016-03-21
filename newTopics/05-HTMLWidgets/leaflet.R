library(leaflet)
pal <- colorQuantile("YlOrRd", NULL, n = 8)
orstationc <- read.csv('~/sol-eng/presentations/grandTourTopics/05-HTMLWidgets/Showcase/orstationc.csv')
leaflet(orstationc) %>% 
  addTiles() %>%
  addCircleMarkers(color = ~pal(tann))

