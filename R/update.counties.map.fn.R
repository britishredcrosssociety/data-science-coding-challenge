#########################################################################
# Update the county-level map with either % poor health or % disability #
#########################################################################

update.counties.map.fn <- function(start.map, fill.poly, pal.legend, val.legend, title.legend){
  
  map.out <- 
    leafletProxy(start.map, data=counties.dat) %>% 
    clearShapes()%>%
    clearControls()%>%
    addPolygons(fillColor = fill.poly, weight = 1, color="white",opacity = 1, fillOpacity = 0.8, popup=~label,
                layerId=~utla21_code) %>%
    addLegend(pal=pal.legend, values = ~val.legend, opacity = 0.8, title = title.legend,
              position = "bottomright")
  map.out
}