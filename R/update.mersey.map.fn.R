# Function to update the LSOA map depending on whether the opacity slider has been altered,
# the radio button has been clicked, or the covariate has changed

update.mersey.map.fn <- function(fill.poly, pal.legend, val.legend, title.legend){
  map.out <- leafletProxy("mersey", data=lsoa.merseyside.dat) %>% clearShapes() %>% clearControls()%>%
             addPolygons(layerId=~LSOA21NM, fillColor = fill.poly, weight = 1, color="darkgrey",opacity = 1, fillOpacity = input$slideropacity, popup=~label) %>%
             addPolylines(data=utla21_merseyside, col="darkred", weight=2) %>%
             addLegend(pal=pal.legend, values = val.legend, opacity = 0.8, title = title.legend,
                position = "bottomright")
  map.out
}