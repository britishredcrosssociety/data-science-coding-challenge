# Read in packages

pacman::p_load(tidyverse, nomisr, shiny, shinydashboard, shinydashboardPlus, shinyWidgets, plotly, leaflet, sf,DT)

# All data sourced from script preparedata.R, read in automatically when loading the censusApp project and running 
# devtools::load_all()


censusApp <- function(){



###################
#      UI         #
###################

ui <- dashboardPage(skin="green",
                    
  dashboardHeader(title = "Census 2021"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName="info", icon=shiny::icon("circle-info")),
      menuItem("Health maps", tabName = "healthmaps", icon = shiny::icon("globe-europe")),
      menuItem("Explore Merseyside", tabName = "mersey", icon = shiny::icon("chart-line")),
      menuItem("Guessing Health", tabName = "guess", icon = shiny::icon("calculator"))
    )
  ), # End of dashboardSidebar
  
  
  dashboardBody(
    tabItems(
            
      # Tab #1 - Overview of the Shiny app contents
      tabItem(tabName = "info",
        fluidRow(
          tabBox(title=shiny::icon("circle-info"), width=12,
            tabPanel("Health Maps",
                HTML(box1.text)),
            tabPanel("Explore Merseyside",
                HTML(box2.text)),
            tabPanel("Guessing Health",
                HTML(box3.text))
          )
        )
      ), # End of tabItem #1
                  
                  
      # Tab #2 - county maps
      tabItem(tabName = "healthmaps",
        fluidRow(

          box( # Box containing map of counties, displaying either % poor health or  % disabled
            title = "Census 2021 health-related data by County/UA",align='left',width=7,
            radioButtons('healthordis', 'Census variable', choiceNames=c('Self-assessed poor health', 'Disability'), 
            choiceValues=c('health', 'disable'),inline = TRUE),
            leafletOutput("counties", height=500)
          ),
          box( # Box containing a barchart of health status comparisons
            title = "Health rating comparison", width=5,plotOutput("countyplothealth", height=245)
          ),
          box( # Box containing a barchart of disability status comparisons
            title="Disability rating comparison", width=5,plotOutput("countyplotdisability", height=245)
          )
        ) # End of fluidRow
      ), # End of tabItem #2
                 
                   
      # Tab #3 - LSOA maps and scatter plots of % poor health vs covariates
      tabItem(tabName = "mersey",
        fluidRow(

          box( # Box containing scatter plots with dropdown bar for covariate selection and tickbox for smoother
            title = "Covariate comparisons", width=6,
            selectInput("yvar", label="Covariate", choices=covariate.choices, selected = "Overcrowded"),
            checkboxInput("smooth", "Add smoother"),
            plotlyOutput("lsoaplothealth", height=500)),
            box( # Box containing maps of either % poor health of covariate (radio button), with slider for opacity
            title = "Self-assessed poor health by LSOA for Merseyside",align='left',width=6,
            radioButtons('lsoa.display', 'Display variable', choiceNames=c('Self-assessed poor health', 'Selected covariate'), 
                           choiceValues=c('health', 'covar'),inline = TRUE),
            leafletOutput("mersey", height=450),
            sliderInput("slideropacity", "Opacity:", min = 0, max = 1,value = 0.85, step=0.05)
          )
        )
      ), # End of tabItem #3                        
                       
      # Tab #4 - scatter of observed vs fitted and map of residuals using dropdown for covariate selection
      tabItem(tabName = "guess",
        fluidRow(

          box( # Box containing scatterplot, with dropdown for covariate selection
            height=695,title="Covariates", status="success", solidHeader=TRUE,
            plotlyOutput("lmplot"),
            pickerInput("linearmodelinput","Select covariates(s)", choices=covariate.choices, multiple = T, selected=default.covar, options=pickerOptions(dropupAuto=FALSE))
          ),
          box( # Box containing map of residuals
            title="Map of residuals", status="success", solidHeader=TRUE,leafletOutput("residmap")
          ),
          box( # Box containing table of summary statistics
            title="Summary statistics for linear model", status="success", solidHeader=TRUE,
            DT::dataTableOutput("table.lmfit")
          )
        )
      ) # End of tabItem #4
                         
    ) # End of tabItems
  ) # End of dashboardBody
  
) # End of dashboardPage



###################
#    Server       #
###################




server = function(input, output, session){
  

  ###### Start of content for Tab 1 ######
  ###### Map + bar charts ######
  
  # Create a map of poor health % or disability at the county level.
  # Choose which to display using the radio button input$healthordis
  # Uses counties.map.fn() and update.counties.map.fn()  

  output$counties <-renderLeaflet({
    
    # Initial map displays % (adjusted) poor health    
    fill.poly <- ~adjhealth.pal(adjpoorhealth)
    pal.legend <- adjhealth.pal
    val.legend <- ~adjpoorhealth
    title.legend <- "% poor health"

    counties.map.fn(fill.poly, pal.legend,val.legend, title.legend)
    
  }) # End of output.counties
  
  # Update the map with the radio buttons
  
  observeEvent({
    input$healthordis},{ 
      
      if(input$healthordis=="health"){
        fill.poly <- ~adjhealth.pal(adjpoorhealth)
        pal.legend <- adjhealth.pal
        val.legend <- ~adjpoorhealth
        title.legend <- "% poor health"
      }
      
      if(input$healthordis=="disable"){
        fill.poly <- ~adjdisable.pal(adjdisable)
        pal.legend <- adjdisable.pal
        val.legend <- ~adjdisable
        title.legend <- "% disabled"
      }
      
      update.counties.map.fn(start.map="counties",fill.poly, pal.legend,val.legend, title.legend)
        
    }
  )
  
  # Create a plot of health and disability scores, comparing Liverpool to other counties/UAs
  # Start with a default comparison of Liverpool against the lowest scoring county which is Wokingham
  # Use the function ggplot.compare.fn() to compare Liverpool with an inputted county
  
  barplotdata <-  ggplot.compare.fn("E06000041")     
  
  output$countyplothealth <- renderPlot(barplotdata$health.plot)
  
  output$countyplotdisability <- renderPlot(barplotdata$disable.plot)
  
  #Click on county/UA to update the health score plots
  
  observeEvent({
    input$counties_shape_click},{ 
      
      # Only update the plots if the user selects a county other than Liverpool 
      if(input$counties_shape_click$id!="E08000012"){
        barplotdata <-  ggplot.compare.fn(input$counties_shape_click$id)     
      
        output$countyplothealth <- renderPlot(barplotdata$health.plot)
      
        output$countyplotdisability <- renderPlot(barplotdata$disable.plot)
      }
    }
  )
  
  ##### End of content for Tab 2 #######
  
  ########## Start of content for Tab 3 ##################
  # Merseyside map at LSOA level + scatterplots #
  
  
  #SCATTERPLOT
  
  # Get the information for the selected covariate only
  df.scatter.lsoa <- reactive({
    select.var <- DF.covars %>% filter(label== input$yvar) %>% select(var)
    df <- st_drop_geometry(lsoa.merseyside.dat) %>% select(LSOA21NM, poorhealth, as.character(select.var[1]))
    names(df) <- c("LSOA", "poorhealth", "covariate")
    df$LSOA <- factor(df$LSOA)
    out <- list(df=df,lab=input$yvar)
  })
  
  

  output$lsoaplothealth <- renderPlotly(ggplotly(ggplot(df.scatter.lsoa()$df, aes(covariate, poorhealth, label=LSOA))+geom_point()+
                                                   ylim(0,19)+xlab(df.scatter.lsoa()$lab)+ylab("Poor health")+ list(
                                                     if (input$smooth) geom_smooth()), source="plotlylsoahealth"))
  
  
  
  # MAP - initial LSOA map to display % poor health
  
  # First create county labels
  utla_centroid <- st_centroid(utla21_merseyside, byid = TRUE)

  output$mersey <-renderLeaflet({
    
      map.mersey <- leaflet(data=lsoa.merseyside.dat) %>% 
      addProviderTiles(provider= providers$Esri.WorldImagery,options=tileOptions(minZoom=10 , maxZoom=18), group="Esri World Imagery") %>%
      addTiles(urlTemplate="",group="No basemap")%>%                     
      addTiles(group="OpenStreetMap")%>%
      addLayersControl(baseGroups = c("OpenStreetMap", "Esri World Imagery","No basemap"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      addPolygons(layerId=~LSOA21NM, fillColor = ~mersey.pal(poorhealth), weight = 1, color="darkgrey",opacity = 1, fillOpacity = 0.55, popup=~label) %>%
      addPolylines(data=utla21_merseyside, col="darkred", weight=2)%>%
      addLabelOnlyMarkers(data = utla_centroid,label = ~utla21_name,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style=list(color="darkred",`font-weight` = "bold"))) %>%
      addLegend(pal=mersey.pal, values = ~poorhealth, opacity = 0.8, title = "% poor health",
                position = "bottomright") 
      

    map.mersey
    
  }) # End of output$mersey
  
  
  # Redraw the map if one of three events occurs - opacity slider is moved, display changes from health value
  # the covariate value, or the covariate selection is changed.
  # Use update.mersey.map.fn()
  
  update.mersey.map.fn <- function(fill.poly, pal.legend, val.legend, title.legend){
    leafletProxy("mersey", data=lsoa.merseyside.dat) %>% clearShapes() %>% clearControls()%>%
      addPolygons(layerId=~LSOA21NM, fillColor = fill.poly, weight = 1, color="darkgrey",opacity = 1, fillOpacity = input$slideropacity, popup=~label) %>%
      addPolylines(data=utla21_merseyside, col="darkred", weight=2, label=~utla21_name) %>%
      addLabelOnlyMarkers(data = utla_centroid,label = ~utla21_name,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style=list(color="darkred",`font-weight` = "bold"))) %>%
      addLegend(pal=pal.legend, values = val.legend, opacity = 0.8, title = title.legend,
                position = "bottomright")
  }

  observeEvent(list(input$slideropacity,input$lsoa.display, input$yvar),{
    
      if(input$lsoa.display=="health"){
        fill.poly <- ~mersey.pal(poorhealth)
        pal.legend <- mersey.pal
        val.legend <- ~poorhealth
        title.legend <- "% poor health"
      }
      
      if(input$lsoa.display=="covar"){
        DF.cov.select <- DF.covars %>%filter(label==input$yvar)
        covar.bins <- round(seq(DF.cov.select$minval,DF.cov.select$maxval,len=9),2)
        covar.pal <- colorBin("Oranges", bins = covar.bins)

        fill.poly <- ~covar.pal(get(DF.cov.select$var))
        pal.legend <- covar.pal
        val.legend <- ~DF.cov.select$var
        title.legend <- paste0("% ",input$yvar)
      }
    
    update.mersey.map.fn(fill.poly, pal.legend, val.legend, title.legend)

    }
  )
  
  
  
  # If a point on the scatter plot (plotly) has been clicked, highlight the corresponding LSOA on the map      
  
  observe({
    click.plot <- event_data(event = "plotly_click", source = "plotlylsoahealth")
    if(is.null(click.plot)==FALSE){
      sf.highlight <- lsoa.merseyside.dat[click.plot$pointNumber+1,]
      leafletProxy("mersey", data=sf.highlight) %>% clearGroup("highlighted") %>%
        addPolylines(data=sf.highlight, col="red", layerId="highlighted")
    }
  })

  
  ## If an LSOA on the map is selected, highlight the corresponding point on the plot
  observeEvent({
    input$mersey_shape_click},{ 
      
      # Get the choice from the map click  
      lsoa.selected <- input$mersey_shape_click
      output$lsoaplothealth <- renderPlotly(
        ggplotly(ggplot(df.scatter.lsoa()$df, aes(covariate, poorhealth, label=factor(LSOA)))+geom_point()+
                   ylim(0,19)+xlab(df.scatter.lsoa()$lab)+ylab("Poor health")+ list(
                   if (input$smooth) geom_smooth(),
                   if (is.null(lsoa.selected)==FALSE) geom_point(df.scatter.lsoa()$df%>%filter(LSOA==lsoa.selected$id), 
                                                               mapping=aes(covariate, poorhealth),col="red")),
                 source="plotlylsoahealth")
      
      ) # End of output$lsoaplothealth
    } # End of input
  ) # End of observeEvent - mersey_click_shape
  
  ##### End of content for Tab 3 #####
  
  
  
  ##### Start of content for Tab 4 #####
  
 
  # Default linear model
  covar.picked <- DF.covars %>% filter(label %in% default.covar)
  formula.text <- paste0("poorhealth~", paste(covar.picked$var, collapse="+"))
  lm.formul <-  as.formula(formula.text)
  mod.lm <- lm(lm.formul, data=st_drop_geometry(lsoa.merseyside.dat))
  
  # Use input$linearmodelinput to select covariates for linear model
  lm.fitted <- reactive({
    if(is.null(input$linearmodelinput)==TRUE){
      covar.picked <- DF.covars %>% filter(label %in% default.covar)
    }
    if(is.null(input$linearmodelinput)==FALSE){
      covar.picked <- DF.covars %>% filter(label %in% input$linearmodelinput)
    }
    formula.text <- paste0("poorhealth~", paste(covar.picked$var, collapse="+"))
    lm.formul <-  as.formula(formula.text)
    mod.lm <- lm(lm.formul, data=st_drop_geometry(lsoa.merseyside.dat))
    mod.lm
  })

  # Create tibble of observed and fitted values from selected linear model
  df.obs.fitted <- reactive({
    df.fit <- tibble(LSOA = lsoa.merseyside.dat$LSOA21NM, observed=lsoa.merseyside.dat$poorhealth, fitted=fitted(lm.fitted()))
    df.fit
  })
  
  # Get the VIF values for the fitted model and identify covariates with VIF>5
  vif.out <- reactive({  
    if(length(input$linearmodelinput)<=1){vif.output="NA"}
    if(length(input$linearmodelinput)>1){
      vif.vals <- car::vif(lm.fitted())
      vif.gt5 <- which(vif.vals>=5)
      if(length(vif.gt5)>=1){
        DF.covars.vifgt5 <- DF.covars %>% filter(var %in% names(vif.vals[vif.gt5]))
        vif.output <- paste(DF.covars.vifgt5$label, collapse=", ")
      }
      if(length(vif.gt5)==0){vif.output="None"}
    }
    vif.output
  })
  
  # Create plot of observed vs fitted
  output$lmplot <- renderPlotly({ggplotly(ggplot(df.obs.fitted(), aes(x=observed,y=fitted, label=LSOA))+geom_point()+
                                            geom_abline(intercept=0, slope=1, col="darkred", linewidth=1, linetype=2)+
                                            xlab("Observed % poor health")+ylab("Predicted % poor health")+ylim(c(0,18)),
                                            source="plotlymodel") 
                              })
  
  # Create a map of residuals - initially using the default covariate, then overwriting with leafletProxy
  
 
  # Create initial map using default
  output$residmap <-renderLeaflet({
  resid.bins <- round(seq(-12,12,len=12),2)
  resid.pal <- colorBin("RdBu", bins = resid.bins)
  
  # Map default model
  
  lsoa.merseyside.dat$resid <- round(resid(mod.lm),2)
 
  map.model <- leaflet(data=lsoa.merseyside.dat) %>% 
    addProviderTiles(provider= providers$Esri.WorldImagery,options=tileOptions(minZoom=10 , maxZoom=18), group="Esri World Imagery") %>%
    addTiles(urlTemplate="",group="No basemap")%>%                     
    addTiles(group="OpenStreetMap")%>%
    addLayersControl(baseGroups = c("OpenStreetMap", "Esri World Imagery","No basemap"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    addPolygons(layerId=~LSOA21NM, fillColor = ~resid.pal(resid), weight = 1, color="darkgrey",opacity = 1, fillOpacity = 0.85, popup=~model.label) %>%
    addPolylines(data=utla21_merseyside, col="darkred", weight=2)%>%
    addLabelOnlyMarkers(data = utla_centroid,label = ~utla21_name,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style=list(color="darkred",`font-weight` = "bold"))) %>%
    addLegend(pal=resid.pal, values = ~resid, opacity = 0.8, title = "Model residuals",
              position = "bottomright") 
  })
  
  
  # Update map when change the selection of covariates.
  observeEvent({
    input$linearmodelinput},{ 
      resid.bins <- seq(-12,12,len=12)
      resid.pal <- colorBin("RdBu", bins = resid.bins)
      
      lsoa.merseyside.dat$resid <- round(resid(lm.fitted()),2)
      leafletProxy("residmap", data=lsoa.merseyside.dat) %>% clearShapes() %>% 
        addPolygons(layerId=~LSOA21NM, fillColor = ~resid.pal(resid), weight = 1, color="darkgrey",opacity = 1, fillOpacity = 0.85, popup=~model.label)%>% 
        addPolylines(data=utla21_merseyside, col="darkred", weight=2) %>%
        addLabelOnlyMarkers(data = utla_centroid,label = ~utla21_name,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style=list(color="darkred",`font-weight` = "bold")))
       })

  
  # If a point has been clicked on the scatterplot, highlight the corresponding LSOA:      
  
  observe({
    click.model <- event_data(event = "plotly_click", source = "plotlymodel")
    if(is.null(click.model)==FALSE){
      sf.highlight <- lsoa.merseyside.dat[click.model$pointNumber+1,]
      leafletProxy("residmap", data=sf.highlight) %>% clearGroup("highlightmodel") %>%
        addPolylines(data=sf.highlight, col="red", layerId="highlightmodel")
    }
  })
  
  # Create a table of summary statistics
  # Default model first
  mod.rmse <- sqrt(sum((lsoa.merseyside.dat$poorhealth-fitted(mod.lm))^2)/nrow(lsoa.merseyside.dat))
  model.table <- tibble(Statistic = c("R2", "RMSE", "VIF>5"),
                        Value = c(round(summary(mod.lm)$r.squared,2), round(mod.rmse,2),"NA"))
  
  # Render default table
  output$table.lmfit = DT::renderDataTable({datatable(model.table,
                                                               rownames=FALSE, options=list(dom='t', autoWidth=TRUE))})
  
  # Update the table when the model covariates (input$linearmodelinput) changes
  observeEvent({
    input$linearmodelinput},{ 

      mod.rmse <- sqrt(sum((lsoa.merseyside.dat$poorhealth-df.obs.fitted()$fitted)^2)/nrow(lsoa.merseyside.dat))
      
      model.table <- tibble(Statistic = c("R2", "RMSE", "VIF>5"),
                            Value = c(round(summary(lm.fitted())$r.squared,2), round(mod.rmse,2), vif.out()))
      output$table.lmfit = DT::renderDataTable({datatable(model.table,
                                                          rownames=FALSE, options=list(dom='t', autoWidth=TRUE))})
      
    }
  )
  
  ##### End of content for Tab 4 #####
  
} # End of server


# Run the application 
shinyApp(ui = ui, server = server)

}