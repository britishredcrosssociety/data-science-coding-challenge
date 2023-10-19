# Shiny app.
#      ONS Census Data Visualisation.
#      UK Accommodation types and central heating system.
# Version - 1
# Date - 13/10/2023
# Author - A R Vaughan

# load required packages.
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(bbplot)
library(scales)
library(magrittr)
library(sf)
library(leaflet)
library(janitor)

# import cleaned census data
tmp1 <- readRDS("./data/LSOA/census_2021_LSOA_data.rds")
tmp2 <- readRDS("./data/LTLA/census_2021_LTLA_data.rds")
tmp3 <- unique(tmp2$local_authority)
tmp4 <- unique(tmp2$type)
tmp5 <- tmp2 %>% select(type,variable) %>% distinct(.)

# read ONS Local Authority Boundary shapefile
map1 <- read_sf("./data/LA_boundary/LAD_DEC_2021_GB_BFC.shp")
map1 %<>% rename(local_authority=LAD21NM)

# define functions
# function 1 tail data to clip number of shown records
tail_slice <- function(dat,obs){
  
  s1 <- dat[1:obs-1,]
  dat <- s1 %>% rbind(.,dat[obs,] %>%
                        mutate(percentage=sum(dat$percentage[obs:length(dat$percentage)]),
                               value=sum(dat$value[obs:length(dat$value)]),
                               variable="other sources",
                               region=dat$region[1],
                               country=dat$country[1]))
  return(dat)}

# dashboard has 4 pages
# Page 1 Home page, overview and licence
# Page 2 UK/Regional Overview
# Page 3 Local Authority Comparison
# Page 4 Map View and Deep Dive

system.time

# Define the UI
ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "ONS 2021 Census Analysis"),
                    dashboardSidebar(collapsed = T,
                                     sidebarMenu(menuItem("Home", tabName = "page1",
                                                          icon = icon("home")),
                                                 menuItem("UK Overview", tabName = "page2",
                                                          icon = icon("line-chart")),
                                                 menuItem("Local Authority", tabName = "page3",
                                                          icon = icon("line-chart")),
                                                 menuItem("Mapping Overview", tabName = "page4",
                                                          icon = icon("line-chart")))),
                    dashboardBody(tabItems(tabItem("page1",
                                                   column(width = 6,
                                                          align = "left",
                                                          tags$h1("UK Accommodation and Central Heating Data."),
                                                          tags$h2("Supporting Local Retro Fit Design and Coordination."),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2(""),
                                                          tags$h4("In 2021, the UK emitted 426.5 million net tonnes of carbon 
                                                                 dioxide equivalent (Mt CO2e), according to the Department of 
                                                                 Business, Energy & Industrial Strategy, UK Greenhouse Gas 
                                                                 Emissions 2021. Residential emissions contributed 16% of 
                                                                 emissions, with housing heating forming an important.",
                                                                  style="text-align: justify;",),
                                                          tags$h2(""),
                                                          tags$h4("As the UK transitions to NetZero, decarbonisation of home 
                                                                 heating will form an essential step in reducing domestic 
                                                                 greenhouse gas emissions and will also support improved 
                                                                 local air quality and wellbeing by improving fuel poverty 
                                                                 levels.",style="text-align: justify;",),
                                                          tags$h2(""),
                                                          tags$h4("To aid the local focus on local energy efficiency and 
                                                                 retrofit, ONS census data contains important information 
                                                                 concerning central heating technologies and house types 
                                                                 across various spatial scales. The union of this data 
                                                                 can aid in programme development and grant schemes.",
                                                                  style="text-align: justify;",),
                                                          tags$h2(""),
                                                          tags$h4("This Shiny app focuses on showcasing the value of 
                                                                 using ONS census data at different spatial scales 
                                                                 to aid local delivery retrofit.",
                                                                  style="text-align: justify;"),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2("Page 1 UK Overview"),
                                                          tags$h4("The page provides an overview, at a country-wide or 
                                                                  regional scale, of both the types of housing present 
                                                                  in England and Wales and their central heating systems.",
                                                                  style="text-align: justify;"),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2(""),
                                                          tags$h2("Page 2 Local Authority"),
                                                          tags$h4("This page allows the user to choose and compare up to 
                                                                  three local authority areas regarding their housing 
                                                                  classifications and central heating stocks.",
                                                                  style="text-align: justify;"),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2(""),
                                                          tags$h2("Page 3 Mapping Overview"),
                                                          tags$h4("The final shiny page allows the user to understand 
                                                                  which accommodation or central heating systems are 
                                                                  most common across the UK across local authorities.",
                                                                  style="text-align: justify;"),
                                                          tags$h2(""),
                                                          tags$hr(style = "border-top: 1px solid #000000;")),
                                                   column(1,
                                                          tags$img(src = "shiny_image.png", height = 1060/2, width = 1536/2),
                                                          tags$h2("Credits")),
                                                   column(6,
                                                          tags$h4("This R Shiny App uses the UK Government ONS census data. 
                                                          This data is available under the Open Government Licence v3.0."),
                                                          tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
                                                                 "Open Government Licence v3.0."),
                                                          tags$h4("ONS Census data can be downloaded and accesses through 
                                                                  the nomis website."),
                                                          tags$a(href="https://www.nomisweb.co.uk/sources/census_2021",
                                                                 "nomisweb"),
                                                          tags$hr(style = "border-top: 1px solid #000000;"))),
                                           tabItem("page2",
                                                   column(width = 6,
                                                          align = "left",
                                                          tags$h1("UK and Regional Data."),
                                                          tags$h2("Accommodation and Central Heating."),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2(""),
                                                          tags$h4("This tool shows the percentage breakdown by either 
                                                                  Country or Region, for each, accommodation or central 
                                                                  heating system type.",style="text-align: justify;")),
                                                   fluidRow(column(6,selectizeInput("option_p1_1", "Select an option:",
                                                                                    choices = c("region","country"),
                                                                                    selected = "Option 1")),
                                                            column(6,selectizeInput("option_p1_2", "Select an option:",
                                                                                    choices = c("heating","accommodation"),
                                                                                    selected = "Option 1")),
                                                            column(6,sliderInput("option_p1_o1","Percentage Scale:",
                                                                                 min = 0, max = 100,value = c(0, 100)))),
                                                   fluidPage(plotOutput("p1_p1"))),
                                           tabItem("page3",
                                                   column(width = 6,
                                                          align = "left",
                                                          tags$h1("Local Authority Data."),
                                                          tags$h2("Accommodation and Central Heating."),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2(""),
                                                          tags$h4("This tool allows the user to investigate central 
                                                                  heating types further by comparing up to three 
                                                                  local authority areas and visualising the percentage 
                                                                  breakdown.",style="text-align: justify;")),
                                                   fluidRow(column(6,selectizeInput("option_p2_1", "Select an option:",
                                                                                    choices = c(tmp3),
                                                                                    multiple = T,
                                                                                    options = list(
                                                                                      placeholder = 'Please select up to three option',
                                                                                      maxItems = 3))),
                                                            column(6,sliderInput("option_p2_o1", "Number of groups:",
                                                                                 min = 3, max = 8, value = 4))),
                                                   h2("Housing Types"),
                                                   plotOutput("p2_p1"),
                                                   h2("Central Heating Types"),
                                                   plotOutput("p2_p2")),
                                           tabItem("page4",
                                                   column(width = 6,
                                                          align = "left",
                                                          tags$h1("Local Authority Mapping"),
                                                          tags$h2("Accommodation and Central Heating."),
                                                          tags$hr(style = "border-top: 1px solid #000000;"),
                                                          tags$h2(""),
                                                          tags$h4("This tool allows users to investigate specific 
                                                                  Accommodation or Central heating types they would 
                                                                  like to concentrate on. This can help aid where 
                                                                  retrofit programmes should spatially target.",
                                                                  style="text-align: justify;"),
                                                          tags$h2(""),
                                                          tags$h4("A breakdown of the top three local authority areas 
                                                                  is shown for the selected input, with a visual map showing 
                                                                  where these local authorities are in the UK.",
                                                                  style="text-align: justify;")),
                                                   fluidRow(column(width = 6,
                                                                   selectizeInput("option_p3_1", 
                                                                                  label = "Select Accommodation or Heating:", 
                                                                                  selected = NULL, 
                                                                                  choices = c("heating",
                                                                                              "accommodation")),
                                                                   selectizeInput("option_p3_2",
                                                                                  label = "Filter by Type:", 
                                                                                  selected = "",
                                                                                  choices = NULL,
                                                                                  multiple=T)),
                                                            column(width = 6,
                                                                   tags$h4("Please ensure that the 'Filter by Type' entry 
                                                                           box is clear before switching between 
                                                                           accommodation and central heating system 
                                                                           visualisations.",
                                                                           style="text-align: justify;"))),
                                                   fluidRow(column(column = 6,width = 6,
                                                                   plotOutput("p3_p1")),
                                                            column(column = 6,width = 6,
                                                                   leafletOutput("p3_p2")))))))

# Define the server logic
server <- function(input, output, session) {
  
  #server logic for Page 1
  #set plot width and height as reactive to option selected
  plotwidth <- reactive({as.numeric(length(unique(tmp1[[input$option_p1_1]])))})
  plotheight <- reactive({subset(tmp1,type=input$option_p1_2) %>%
      select(variable) %>% distinct() %>% nrow(.) %>% as.numeric(.)})
  
  #Page 2, Plot 1 contained inside observe to ensure plot width and height are reactive
  observe({
    #define plot
    output$p1_p1 <- renderPlot({
      #filter LSOA level data 
      dat <- tmp1 %>% select(input$option_p1_1,type,
                             variable,value) %>%
        filter(type==input$option_p1_2) %>%
        rename(geography=input$option_p1_1) %>%
        drop_na(geography)
      
      #group and summarise LSOA data to selected geography
      dat <- dat %>%
        group_by(geography,variable) %>%
        summarise(across(starts_with("value"), ~sum(.))) %>%
        ungroup()
      
      #calculate percentage breakdowns per group
      dat <- dat %>%
        group_by(geography) %>%
        group_modify(~mutate(., percentage = value/sum(value)*100)) %>%
        ungroup() %>%
        mutate(variable=gsub("_"," ",variable))
      
      #remove data pointa outside slider range, to appear as grey
      dat$percentage[which(dat$percentage<input$option_p1_o1[1])] <- NA
      dat$percentage[which(dat$percentage>input$option_p1_o1[2])] <- NA
      
      #plot 1 accommodation or heating system geography breakdown
      g1 <- dat %>% ggplot(aes(geography,variable,
                               fill=as.numeric(percentage))) +
        geom_tile(colour="white",height=0.6) +
        geom_text(aes(label = round(percentage, 0)),colour="white") +
        scale_fill_viridis_c(option="H",limits=c(0,50),
                             oob=squish) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
        scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
        bbc_style() +
        theme(legend.position = "none",
              axis.title.x = NULL,
              axis.title.y = NULL,
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14,angle = 90, hjust = 1))
      
      #only display plot if input entry exists
      if(length(input$option_p1_1)>=1)
        g1
      
    },height = (plotheight()*30),width = 400+(plotwidth()*100))
  })
  
  #Page 3, Plot 1 - Local Authority Comparison plot for Accommodation Types
  output$p2_p1 <- renderPlot({
    
    #filter data by selected local authority(s) chosen and clean names
    dat <- tmp2 %>% filter(local_authority %in% 
                             c(input$option_p2_1)) %>%
      mutate(variable=gsub("_"," ",variable)) %>%
      filter(type=="accommodation")
    
    #merge lower observations outside slider range input. 
    #e.g. show 4 types will merge ordered values 4 and below.
    dat <- dat %>%
      group_by(local_authority) %>%
      group_modify(~tail_slice(.,input$option_p2_o1)) %>%
      ungroup()
    
    #create position data for ggplot labels
    dat2 <- dat %>%
      group_by(local_authority) %>%
      mutate(cs = rev(cumsum(rev(percentage))), 
             pos = percentage/2 + lead(cs, 1),
             pos = if_else(is.na(pos), percentage/2, pos))
    
    #plot 1 Local Authority Accommodation Breakdown
    g1 <- dat %>%
      ggplot(aes(x="",y=percentage,fill = fct_inorder(variable))) +
      geom_bar(stat = "identity") + coord_polar(theta = "y",start = 0) +
      geom_label_repel(aes(y = pos,
                           label = paste0(round(percentage), "%")),
                       data = dat2,
                       size=8, show.legend = F, nudge_x = 1) +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(labels = NULL) +
      bbc_style() +
      theme(legend.text = element_text(size = 12)) +
      facet_wrap(.~local_authority)
    
    #clean legend appearance
    g1 <- g1 + guides(fill=guide_legend(nrow=3,ncol=3))
    
    #only display plot if input entry exists
    if(length(input$option_p2_1)>=1)
      g1
    
    #defined graphic parameters
  },height = 400,width = 1000)
  
  #Page 3, Plot 1 - Local Authority Comparison plot for Central Heating Types
  output$p2_p2 <- renderPlot({
    
    #filter data by selected local authority(s) chosen and clean names
    dat <- tmp2 %>% filter(local_authority %in% 
                             c(input$option_p2_1)) %>%
      mutate(variable=gsub("_"," ",variable)) %>%
      filter(type=="heating")
    
    #merge lower observations outside slider range input.
    #e.g. show 4 types will merge ordered values 4 and below.
    dat <- dat %>%
      group_by(local_authority) %>%
      group_modify(~tail_slice(.,input$option_p2_o1)) %>%
      ungroup()
    
    #create position data for ggplot labels
    dat2 <- dat %>%
      group_by(local_authority) %>%
      mutate(cs = rev(cumsum(rev(percentage))), 
             pos = percentage/2 + lead(cs, 1),
             pos = if_else(is.na(pos), percentage/2, pos))
    
    #plot 1 Local Authority Accommodation Breakdown
    g1 <- dat %>%
      ggplot(aes(x="",y=percentage,fill = fct_inorder(variable))) +
      geom_bar(stat = "identity") + coord_polar(theta = "y",start = 0) +
      geom_label_repel(aes(y = pos,
                           label = paste0(round(percentage), "%")),
                       data = dat2,
                       size=8, show.legend = F, nudge_x = 1) +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(labels = NULL) +
      bbc_style() +
      theme(legend.text = element_text(size = 12)) +
      facet_wrap(.~local_authority)
    
    #clean legend appearance
    g1 <- g1 + guides(fill=guide_legend(nrow=3,ncol=3))
    
    #only display plot if input entry exists
    if(length(input$option_p2_1)>=1)
      g1
    
    #defined graphic parameters
  },height = 400,width = 1000)
  
  #Page 4 - Mapping deep dive into specific technologies per Local Authority
  #Update choices for Input 2 based on the input of Input 2 
  observe({
    #Reactive switch to update values
    choices <- switch(input$option_p3_1,
                      "accommodation" = unique(subset(tmp5,type=="accommodation")$variable),
                      "heating" = unique(subset(tmp5,type=="heating")$variable))
    
    #Update Input 2 with new choices
    updateSelectizeInput(session, "option_p3_2", choices = choices, server = T)
  })
  
  #Page 4, Plot 1 - Top Three Local Authority's (LA's) based on technology. 
  #If accommodation is chosen, central heating systems are shown
  output$p3_p1 <- renderPlot({
    
    #Function housed in trycatch to suppress errors when no input is selected
    tryCatch({
      #filter data based on type variable selected. Select top 3 LA's based on number.
      xx <- tmp2 %>%
        filter(type==input$option_p3_1,
               variable==input$option_p3_2) %>%
        arrange(desc(value)) %>%
        slice(1:3)
      
      #filter data using LA's.
      dat <- tmp2 %>%
        filter(local_authority %in% xx$local_authority) %>%
        mutate(variable=gsub("_"," ",variable)) %>%
        filter(!type==input$option_p3_1)
      
      #merge lower observations outside slider range input.
      #e.g. show 4 types will merge ordered values 4 and below.
      #tail_slice function is defined above.
      dat <- dat %>%
        group_by(local_authority) %>%
        group_modify(~tail_slice(.,5)) %>%
        ungroup()
      
      #create position data for ggplot labels
      dat2 <- dat %>%
        group_by(local_authority) %>%
        mutate(cs = rev(cumsum(rev(percentage))), 
               pos = percentage/2 + lead(cs, 1),
               pos = if_else(is.na(pos), percentage/2, pos))
      
      #plot 1 Local Authority Breakdown Accommodation/Central Heating
      g1 <- dat %>%
        ggplot(aes(x="",y=percentage,fill = fct_inorder(variable))) +
        geom_bar(stat = "identity") + coord_polar(theta = "y",start = 0) +
        geom_label_repel(aes(y = pos,
                             label = paste0(round(percentage), "%")),
                         data = dat2,
                         size=8, show.legend = F, nudge_x = 1) +
        scale_fill_brewer(palette = "Dark2") +
        scale_y_continuous(labels = NULL) +
        bbc_style() +
        theme(legend.text = element_text(size = 12)) +
        facet_wrap(.~local_authority)
      
      #clean legend appearance
      g1 <- g1 + guides(fill=guide_legend(nrow=3,ncol=3))
      
      #return plot
      return(g1)
    }, error = function(e) {
      
      return(NULL)
    })
    
  })
  
  #Page 4, Plot 2 - Map of Top Three Local Authority's (LA's) based on technology. 
  #Function housed in trycatch to suppress errors when no input is selected
  output$p3_p2 <- renderLeaflet({
    
    #place plotting function in error trycatch to suppress error
    tryCatch({
      
      #filter data based on type variable selected. Select top 3 LA's based on number.
      xx <-tmp2 %>%
        filter(type==input$option_p3_1,
               variable==input$option_p3_2) %>%
        arrange(desc(value)) %>%
        slice(1:3)
      
      #filter shapefile data using LA's and convert from BNG to WGS84 coordinate system
      g1 <- map1 %>%
        filter(local_authority %in%
                 xx$local_authority) %>% 
        st_transform(., 4326)
      
      #create leaflet plot using stamen base map and selected shapefile data
      g1 <- leaflet() %>%
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        setView(lng = median(g1$LONG),
                lat = median(g1$LAT),
                zoom = 6) %>%
        addPolygons(data = g1,fillColor = "red",
                    color = "red")
      #return plot
      return(g1)
    }, error = function(e) {
      
      return(NULL)
    })
  })
  
  #clear leaflet plot when filter entries change
  observe({leafletProxy("p3_p2", data = NULL) %>% clearMarkers()})
}

#Create the Shiny app
shinyApp(ui, server)