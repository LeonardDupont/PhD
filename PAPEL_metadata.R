rm(list=ls())
# Library imports
library(gdata)
library(ggplot2)
library(gridExtra)
library(tmap)
library(leaflet)
library(plyr)
library(dplyr)
library(sf)
library(htmlwidgets)

# Let us first import the metadata and take a look at the stats
setwd("./Documents/PhD/PAPEL_related/PAPEL_2020")
metadata = read.csv("metadata_lacs_2020.csv")


#we get lake positions, group them and transform them into geometry points
sectors = unique(metadata$Secteur)
cropped_meta = metadata[,c('Secteur','Zone','Nom','Code','lat','lon','z','S..m2.')]

for (i in 1:length(sectors)){
  toto <- filter(cropped_meta,cropped_meta$Secteur==sectors[i])
  toto$group = i
  if(i==1){lakepos=toto}else(lakepos=rbind(lakepos,toto))}

lakepos = filter(lakepos, lakepos$group < 5)
a = st_as_sf(lakepos, coords=c('lon','lat'), crs=4326,na.fail=F)


#we create a color range for the different valleys
lakecols = colorFactor("viridis", lakepos$Secteur)

#we create popup info in the HTLM version
lakepos = lakepos %>% mutate(
  popup = paste0("<p><strong>", Nom, "</strong></p>","<p><em>Surface = ",S..m2.,"</em></p>","<p><em> Altitude = ", z,"</em></p>" )
)


#Now we create the map
map_lakes <- leaflet() %>% addProviderTiles('OpenTopoMap') %>%
  
  addCircleMarkers(data=a, color=lakecols(lakepos$Secteur), opacity=1, fillOpacity=1, radius=5, label=lakepos$Code, popup=lakepos$popup,     popupOptions = 
               list(maxHeight = 150, maxWidth = 200)) %>%
  
  addLegend('bottomright', pal = lakecols, values = lakepos$Secteur,
          title = "Secteur d'étude",
          opacity = 1)
map_lakes


saveWidget(map_lakes, file="lacs_PAPEL_2020.html", selfcontained = TRUE)
