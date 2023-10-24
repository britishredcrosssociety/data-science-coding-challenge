#######################################################################################
# Function to produce a map of counties to display either % poor health or disability #
#######################################################################################

counties.map.fn <- function(fill.poly, pal.legend, val.legend, title.legend){
  
  map.out <- 
    leaflet(data=counties.dat) %>% 
    addProviderTiles(provider= providers$Esri.WorldImagery,options=tileOptions(minZoom=6 , maxZoom=18), group="Esri World Imagery") %>%
    addTiles(urlTemplate="",group="No basemap")%>%                     
    addTiles(group="OpenStreetMap")%>%
    addLayersControl(baseGroups = c("OpenStreetMap", "Esri World Imagery","No basemap"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    addPolygons(fillColor = fill.poly, weight = 1, color="white",opacity = 1, fillOpacity = 0.8, popup=~label,
                layerId=~utla21_code) %>%
    addLegend(pal=pal.legend, values = ~val.legend, opacity = 0.8, title = title.legend,
              position = "bottomright", layerId="counties.legend")
  map.out
}