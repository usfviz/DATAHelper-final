rm(list = ls())
# setwd("~/Google-Drive/0.USF/5.2Visualization/project/project-prototype")
if(!require(shinydashboard))install.packages("shinydashboard")
# <<<<<<< HEAD
library(ggvis)
# =======
if(!require(leaflet))install.packages("leaflet")
if(!require(geojsonio))install.packages("geojsonio")

# >>>>>>> a3520b93f1973bdb0fd4b38f4bce8886b3953452
library(shiny)
library(leaflet)
library(geojsonio)
library(shinydashboard)


district <- read.csv("data/neighbourhood_agg.csv")
homes <- read.csv("data/home.csv")
region.sf <- district$neighbourhood
region.sf.all <- append("All", as.character(region.sf))
home.types <- unique(homes$property_type)
amenities <- colnames(homes)[32:67]


ui <- dashboardPage(
  # dashboardHeader(title = "Simple tabs"),
  dashboardHeader(title = "Airbnb User tab"),
  
  ### sidebar
  dashboardSidebar(
    sidebarMenu(id = "menu1",
                menuItem("For owner: market analysis", tabName = "owner", icon = icon("dashboard")),
                menuItem("For visitor", tabName = "visitor",icon = icon("th"), badgeLabel = "new", badgeColor = "green")
    ),
    
    conditionalPanel(condition = "input.menu1 == 'visitor'"
                     ,textInput(inputId = "nights", label = "Type in nights to stay", 
                                value = 7, width = NULL, placeholder = "Type in stay nights")
                     ,sliderInput(inputId = "accommodate", 
                                  label = "Select accommodates range",
                                  min=1, max=16, value=c(2,8),step=1
                     )
                     ,selectInput(inputId = "region2", 
                                 label = "Select a neighborhood:",
                                 choice = region.sf.all,
                                 selected = c("All")
                                 )
                                          ,radioButtons("home_type",
                                  "Select home type:",
                                  choices = c("All", "Select home type"),
                                  selected = c("All"))
                     ,conditionalPanel(condition = "input.home_type!= 'All'",
                                       selectizeInput('select_home_type',
                                                  'Select home type:',
                                                  choices = home.types,
                                                  selected = c("Apartment"),
                                                  multiple = T)
                                      )
                     ,radioButtons("amenity",
                                   "Select home amenity:",
                                   choices = c("All", "Select amenities"),
                                   selected = c("All"))
                     ,conditionalPanel(condition = "input.amenity!= 'All'",
                                       selectizeInput('select_amenity',
                                                   'Select amenities:',
                                                   choices = amenities,
                                                   selected = c("Essentials","Wireless.Internet"),
                                                   multiple = T
                                                   )
                                       )
                     ,selectInput(inputId = "price_range", "Select Price Range", c("0-$200","$200-$500",">$500"),"0-$200")
                     ,conditionalPanel(condition = "input.price_range == '0-$200'",
                                       sliderInput(inputId = "price_1", 
                                                   label = "Choose price range",
                                                     min=0, max=200, value=c(30,180),step=10)
                                      )
                     ,conditionalPanel(condition = "input.price_range == '$200-$500'",
                                      sliderInput(inputId = "price_2", 
                                                  label = "Choose price range",
                                                  min=200, max=500, value=c(230,380),step=10)
                                      )
                     ,conditionalPanel(condition = "input.price_range == '>$500'",
                                      sliderInput(inputId = "price_3", 
                                                  label = "Choose price range",
                                                  min=500, max=7500, value=c(500,2000),step=50)
                                      )

                     )
  ),
  
  #### body
  dashboardBody(
    tabItems(
      
      ##### owner
      tabItem(tabName = "owner",
              valueBoxOutput("regionBox_title1", width = NULL),
              # h2("Dashboard tab content"),
              fluidRow(
                # ------------- map ----------------# 
                column(width = 6,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("polygonmap1", height = 500))
                       )
                ,column(width = 2
                        ,valueBoxOutput("priceBox", width = NULL)
                        ,br(),br()
                        ,valueBoxOutput("ratingBox", width = NULL)
                        ,br(),br()
                        ,valueBoxOutput("avaiBox", width = 20)
                )
                ,column(width = 3
                        ,uiOutput("ggvis_ui_price", height = 2), ggvisOutput("ggvis_price")
                        ,uiOutput("ggvis_ui_rating", height = 2), ggvisOutput("ggvis_rating")
                        ,uiOutput("ggvis_ui_avai", height = 2), ggvisOutput("ggvis_avai")
                )
                )
              ),## first tab item
      
    ### visitor 
    tabItem(tabName = "visitor",
            valueBoxOutput("regionBox_title2", width = NULL),
            fluidRow(
              # ------------- map ----------------# 
              column(width = 7,
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("polygonmap2", height = 600)))
              # ,column(width = 2,
              #         selectInput(inputId = "neighborhood2", 
              #                     label = "choose a neighborhood:",
              #                     choice = list(`East Coast` = c("NY", "NJ", "CT"),
              #                                   `West Coast` = c("WA", "OR", "CA"),
              #                                   `Midwest` = c("MN", "WI", "IA"))
              #         ))
              ,column(width = 5
                      ,column(width = 12
                              # ,box(width = 4,
                              #      title = "Bathroom", 
                              #      status = "danger",
                              #      solidHeader = TRUE,
                              #      uiOutput("home_bathroom", height = 30)
                              #      )
                              # ,box(width = 4,
                              #      title = "Bedroom",
                              #      status = "danger",
                              #      solidHeader = TRUE,
                              #      uiOutput("home_bedroom", height = 30)
                              #      )
                              ,box(width = 4,
                                   title = "Price", 
                                   status = "danger",
                                   solidHeader = TRUE,
                                   uiOutput("home_price", height=30)
                                   )
                              ,box(width = 3,
                                   title = "Beds",
                                   status = "danger",
                                   solidHeader = TRUE,
                                   uiOutput("home_beds", height = 30)
                                   )
                              ,box(width = 5,
                                   title = "Bed Type",
                                   status = "danger",
                                   solidHeader = TRUE,
                                   uiOutput("home_bedType", height = 30)
                                   )
                              
                      )

                      ,box(width = 12,
                        title = "Address", 
                        status = "primary",
                        solidHeader = TRUE,
                        uiOutput("home_details_title", height = 50),
                        collapsible = TRUE
                      )
                      ,box(width = 12,
                           title = "Transit", 
                           status = "warning",
                           solidHeader = TRUE,
                           uiOutput("home_details_transit", height = 50),
                           collapsible = TRUE,
                           collapsed = TRUE
                      )
                      ,box(width = 12,
                           title = "Neighbour",
                           status = "success",
                           solidHeader = TRUE,
                           uiOutput("home_details_neighbour", height = 50),
                           collapsible = TRUE
                      )

              )
              )## fluidRow
            )## second tab item
    ) # tabitems
    )#dashbody
  )# dashboard

server <- function(input, output, session){
  ploygon  <- geojson_read("Data/neighbourhoods.geojson", what = "sp")
  ploygon@data$CCA_1 <- ploygon@plotOrder
  pal <- colorNumeric("viridis", NULL)
  
  
  output$polygonmap1 <- renderLeaflet({
    leaflet(ploygon) %>%
      addTiles() %>%
      setView(-122.434, 37.7749,  zoom = 12) %>%
      # addPolygons(stroke = TRUE, smoothFactor = 0.5, weight = 0.5,
      #             opacity = 1, fillOpacity = 0.8,
      #             color = "#bdbdbd"
      #             ,fillColor = "#bdbdbd"
      #             ,highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE)
      # )
    addPolygons(stroke = TRUE, smoothFactor = NULL, weight = 0.5,
                opacity = 1, fillOpacity = 0.8,
                color = "lightgrey"
                ,fillColor = "lightgrey"
                ,group = "regions"
                ,layerId = ploygon@data$neighbourhood
                ,label = ploygon@data$neighbourhood
                ,highlightOptions = highlightOptions(color = "white", fillColor = "green",weight = 2,bringToFront = TRUE)
    )
  })
  
  #create empty vector to hold all click ids
  clickedIds <- reactiveValues(ids = vector())
  observeEvent(input$polygonmap1_shape_click, {
    
    #create object for clicked polygon
    click <- input$polygonmap1_shape_click
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("polygonmap1")
    
    #append all click ids in empty vector 
    clickedIds$ids <- click$id
    
  })
  

  output$regionBox_title1 <- renderValueBox({
    valueBox(
      value = tags$p(clickedIds$ids, "Market information ", style = "font-size: 80%;"),
      subtitle = tags$p("Choose Neighbourhood on Map ", style = "font-size: 100%;"),
      icon = icon("home"),
      color = "olive"
    )

  })
  
  output$avaiBox <- renderValueBox({

    # if(!is.na(clickedIds$ids)){
    df_temp <- district[district$neighbourhood == clickedIds$ids, ]
    if(nrow(df_temp)!=0){
      availability <- round(df_temp$availability_30,0)
    }else{
      availability <- "--"
    }
      
      valueBox(
        paste(availability, "%"), "Availability Rate", icon = icon("percent"),
        color = "blue"
      )
    # }
    # valueBox(
    #   paste(round(df()$availability_30,0), "%"), "Availability Rate", icon = icon("percent"),
    #   color = "blue"
    # )
  })
  
  output$priceBox <- renderValueBox({

    # if(!is.na(clickedIds$ids)){
      df_temp <- district[district$neighbourhood == clickedIds$ids, ]
      if(nrow(df_temp)!=0){
        price <- round(df_temp$price,0)
      }else{
        price <- "--"
      }
      valueBox(
        paste(price), "Average Price", icon = icon("usd"),
        color = "purple"
      )
    # }
    # valueBox(
    #   paste(round(df()$price,0)), "Average Price", icon = icon("usd"),
    #   color = "purple"
    # )
  })
  
  output$ratingBox <- renderValueBox({

    # if(!is.na(clickedIds$ids)){
      df_temp <- district[district$neighbourhood == clickedIds$ids, ]
      if(nrow(df_temp)!=0){
        rating <- round(df_temp$review_scores_rating,0)
      }else{
        rating <- "--"
      }
      valueBox(
        paste(rating, "%"), "Average Rating", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow"
      )
    

  })


  market_price <- reactive({
    df_temp <- homes[homes$neighbourhood == clickedIds$ids, ]
    df_temp %>%
      ggvis(~price,fill:="#756bb1") %>%
      layer_histograms() %>%
      add_axis("x", title = "Price ($/night)",
               properties = axis_props(title = list(fontSize=10), label=list(fontSize=8))) %>% 
      set_options(height =160, width = 300, duration=0) ## vertical
      # set_options(height =180, width = 300, duration=0)  ## horizontal
  })
  market_price %>% bind_shiny("ggvis_price", "ggvis_ui_price")  
  
  market_avai <- reactive({
    df_temp <- homes[homes$neighbourhood == clickedIds$ids, ]
    df_temp %>%
      ggvis(~availability_30, fill:="#0570b0") %>%
      layer_histograms() %>%
      # scale_numeric("x", domain = c(0, 30), nice = T) %>%
      add_axis("x", title = "Availability (days/month)",
               properties = axis_props(title = list(fontSize=10), label=list(fontSize=8))) %>% 
      set_options(height =160, width = 300, duration=0) ## vertical
      # set_options(height =180, width = 190, duration=0)  ## horizontal
  })
  market_avai %>% bind_shiny("ggvis_avai", "ggvis_ui_avai")

  market_rating <- reactive({
    df_temp <- homes[homes$neighbourhood == clickedIds$ids, ]
    df_temp %>%
      ggvis(~review_scores_rating, fill:= '#fe9929') %>%
      layer_histograms() %>%
      # scale_numeric("x", domain = c(10, 90), nice = T) %>%
      add_axis("y", title = '   ',
               properties = axis_props(title = NULL, labels=NULL))  %>%
      add_axis("x", title = "Rating (/100)",
               properties = axis_props(title = list(fontSize=10), label=list(fontSize=8))) %>% 
      set_options(height =160, width = 300, duration=0) ## vertical
      # set_options(height =180, width = 190, duration=0)  ## horizontal

  })
  market_rating %>% bind_shiny("ggvis_rating", "ggvis_ui_rating")


  #### The visitor tab  ####################################################
  
  output$polygonmap2 <- renderLeaflet({
    
    ## region
    if (input$region2 == "All"){
      df2 <- homes
    }else{    
      df2 <- homes[homes$neighbourhood == input$region2, ]
      }
    
    if(input$price_range=='0-$200') {
      PRICE =  input$price_1
      scalr = 0.02
      }
    else if(input$price_range=='$200-$500') {PRICE =  input$price_2}
    else{PRICE =  input$price_3}
      
    df2 <- df2[df2$price >= PRICE[1] & df2$price < PRICE[2], ]
    
    ## home type
    if (input$home_type == "All"){
      df2 <- df2
    }else{    
      df2 <- df2[df2$property_type %in% input$select_home_type, ]
    }
    
    ## Amenities
    if (input$amenity == "All"){
      df2 <- df2
    }else{
      for (amenity in input$select_amenity){
      df2 <- df2[df2[,amenity]==1,]
      }
    }
    
    ## Accommodates
    df2 <- df2[df2$accommodates>=input$accommodate[1] & df2$accommodates<input$accommodate[2],]

    ## Nights
    df2 <- df2[df2$minimum_nights<=input$nights & df2$maximum_nights>=input$nights,]
    
    qpal <- colorQuantile(c('#ffffb2','#c2e699','#78c679','#238443',"#00441b"), df2$price, n = 4) # quantile
    pal <- colorNumeric(palette = c("#ffffbf","#006837"), domain = df2$price) # continous
    
    if (nrow(df2) != 0){
    leaflet(ploygon) %>%
      addTiles() %>%
      setView(-122.434, 37.7749,  zoom = 12) %>%
      addPolygons(
                  layerId = df2$neighbourhood,
                  label = df2$neighbourhood,
                  fill = F, weight = 0, color = "#FFFFCC", group = "Outline") %>%
      addMarkers(data = df2, lng = ~longitude, lat = ~latitude,
                 group = df2,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'),
                 popup =
                   ~paste(
                     # "<style> div.leaflet-popup-content-wrapper { opacity: 0.89; } </style>",
                     "<style> p{text-indent: 2.0em;}</style>",
                     "<h4><b>", name, "</b></h4>",
                     '<a href=', listing_url,"><img src=", medium_url, "></a><br>",
                     "<br>",
                     "Price/night:  ","<b>$", price, '</b>, ',
                     'Reviews: <b>',  number_of_reviews, '</b>, ',
                     'Review Rating: <b>', review_scores_rating, '</b>% <br>',
                     '<a href=', listing_url,">","<h4>Go to book</h4> </a>",
                     # "<p>", math_percent,'% pass CAASPP Math', '</p>',
                     # '<b>Address: </b>', address,
                     # '<br />', '<br />', '<b>Neighborhood: </b>', neighb,
                     # '<br /><br />',
                     sep = ""),
                 options = popupOptions(closeButton = FALSE,
                                         minWidth = 900,
                                         maxWidth = 900
                                       )
                   )
    }else{
      leaflet(ploygon) %>%
        addTiles() %>%
        setView(-122.434, 37.7749,  zoom = 12)
    }
      
  })
  
  output$regionBox_title2 <- renderValueBox({
    if (input$region2 == "All"){
      df2 <- homes
      results <- "All Neighbourhoods"
    }else{    
      df2 <- homes[homes$neighbourhood == input$region2, ]
      results <- input$region2
    }
    
    if(input$price_range=='0-$200') {
      PRICE =  input$price_1
      scalr = 0.02
    }
    else if(input$price_range=='$200-$500') {PRICE =  input$price_2}
    else{PRICE =  input$price_3}
    
    df2 <- df2[df2$price >= PRICE[1] & df2$price < PRICE[2], ]
    
    ## home type
    if (input$home_type == "All"){
      df2 <- df2
    }else{    
      df2 <- df2[df2$property_type %in% input$select_home_type, ]
    }
    
    ## Amenities
    if (input$amenity == "All"){
      df2 <- df2
    }else{
      for (amenity in input$select_amenity){
        df2 <- df2[df2[,amenity]==1,]
      }
    }
    
    ## Accommodates
    df2 <- df2[df2$accommodates>=input$accommodate[1] & df2$accommodates<input$accommodate[2],]
    
    ## Nights
    df2 <- df2[df2$minimum_nights<=input$nights & df2$maximum_nights>=input$nights,]
    
    ## text for result
    if(nrow(df2)>1) text <- "Homes in "
    else text <- "Home "
    
    valueBox(
      value = tags$p(nrow(df2), text, results, style = "font-size: 80%;"),
      subtitle = tags$p("Click Results to Zoom In ", style = "font-size: 120%;"),
      icon = icon("home"),
      color = "olive"
    )
    
  })
  
  observe({
    click<-input$polygonmap2_marker_click
    print (click)
    if(is.null(click))
      return()
    home <- homes[homes$latitude==click$lat & homes$longitude==click$lng,]
    home.name <-home$name
    home.description <- home$description
    home.neighbour.overview <- home$neighborhood_overview
    home.rating <- home$review_scores_rating
    home.transit <- home$transit
    
    output$home_details_title <- renderPrint({
      div(
        "",
        tags$span(      # Creates an HTML span.
          class="foo",  # Any named args become attributes.
          paste(home$street),
          # tags$strong(paste("min. stay: ", homes$minimum_nights, "days",
          #                   "max. stay: ", homes$maximum_nights, "days")
          #             ),  # Unnamed args become children of the tag.
          ""
        ),
        ""
      )
    })
    
    output$home_details_neighbour <- renderPrint({
      div(
        "",
        tags$span(      # Creates an HTML span.
          class="foo",  # Any named args become attributes.
          paste(home.neighbour.overview),  # Unnamed args become children of the tag.
          ""
        ),
        ""
      )
    })
    
    output$home_details_transit <- renderPrint({
      div(
        "",
        tags$span(      # Creates an HTML span.
          class="foo",  # Any named args become attributes.
          paste(home.transit),  # Unnamed args become children of the tag.
          ""
        ),
        ""
      )
    })
    
    
    # output$home_bedroom <- renderPrint({
    #   div(
    #     "",
    #     tags$span(      # Creates an HTML span.
    #       class="foo",  # Any named args become attributes.
    #       paste(home$bedrooms),  # Unnamed args become children of the tag.
    #       ""
    #     ),
    #     ""
    #   )
    # })
    # 
    # output$home_bathroom <- renderPrint({
    #   div(
    #     "",
    #     tags$span(      # Creates an HTML span.
    #       class="foo",  # Any named args become attributes.
    #       paste(home$bathrooms),  # Unnamed args become children of the tag.
    #       ""
    #     ),
    #     ""
    #   )
    # })
    
    output$home_price <- renderPrint({
      div(
        "",
        tags$span(      # Creates an HTML span.
          class="foo",  # Any named args become attributes.
          tags$strong("$",paste(home$price), "/ngt"),  # Unnamed args become children of the tag.
          ""
        ),
        ""
      )
    })
    
    output$home_beds <- renderPrint({
      div(
        "",
        tags$span(      # Creates an HTML span.
          class="foo",  # Any named args become attributes.
          tags$strong("    ",paste(home$beds)),  # Unnamed args become children of the tag.
          ""
        ),
        ""
      )
    })
    
    output$home_bedType <- renderPrint({
      div(
        "",
        tags$span(      # Creates an HTML span.
          class="foo",  # Any named args become attributes.
          tags$strong(paste(home$bed_type)),  # Unnamed args become children of the tag.
          ""
        ),
        ""
      )
    })
    

  })

}

shinyApp(ui, server)

