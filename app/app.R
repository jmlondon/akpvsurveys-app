library(leaflet)
library(sp)
library(rgdal)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(dplyr)
library(DT)

load('data/akpvpolys.rda')
load('data/akpvwypts.rda')

proj.4326 <- "+proj=longlat +datum=WGS84"
akpvwypts <- spTransform(akpvwypts, CRS(proj.4326))
akpvpolys <- spTransform(akpvpolys, CRS(proj.4326))
akpvpolys_proj <- spTransform(akpvpolys, CRS("+init=epsg:3571"))
akpvwypts_proj <- spTransform(akpvwypts, CRS("+init=epsg:3571"))

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    body { padding-bottom: 150px; }"
    )),
    tags$script(src = "message-handler.js"),
    tags$style(type='text/css', ".selectize-dropdown-content {max-height: 120px; }")
    ),
  # Application title
  title = "Alaska Harbor Seal Abundance Explorer",
  p(),
  leafletOutput("mymap",height=450),
  h2("Alaska Harbor Seal Abundance Explorer"),
  fluidRow(
    tabsetPanel(
    tabPanel("Parameters",
             fluidRow(
             column(5,offset=1,p(),
                    wellPanel(
                      tags$strong('APPLICATION UNDER ACTIVE DELOPMENT.'),
                      br(),
                      tags$strong('FOR EXPLORATION AND PROTOTYPE USE ONLY'),
                      p(),
                      p('Explore the abundance of harbor seals in Alaska:'),
                      tags$ol(
                        tags$li('Adjust the slider to your desired buffer'),
                        tags$li('Zoom into the map and find your point of interest (POI); Search by place name'),
                        tags$li('Click on the map to specify/refine your POI'),
                        tags$li('Switch to the Plot Output tab to view abundance information')
                      ),
                      hr(),
                      p(tags$small('This application is being developed to assist the understanding of harbor seal abundance in relation to local Points of Interest (POI) in Alaska. The underlying dataset and statistical analysis for harbor seal abundance and trend in Alaska are under heavy development.')),p(tags$strong('Data presented for demonstration purposes only.'))
                    )),
             column(5,offset=-1,p(),
                    wellPanel(
                      textInput('geo_search','Place:',width='20em'),
                      actionButton('geo_button','Search & Zoom')
                    ),
                    wellPanel(
                      sliderInput("buffer", "POI Buffer Distance (km)",
                                  min=1,max=100,value=30),
                      tags$small('Select a buffer distance around your POI to determine which survey units are included within the plots and analysis. The buffer distance represents the radius of a circle with the POI at the center. Land barriers are not taken into account.')
                    )
             )),
                    fluidRow(
                      
             )
    ),
    tabPanel("Plot Output",
             column(8,
  plotOutput("plot1",height="300px")
  ),
  column(4,
  plotOutput("plot2",height="300px")
  ),
  fluidRow(
    column(12,
           hr()
    )
  ),
  fluidRow(
    column(10,offset=1,
           dataTableOutput(outputId="akpv_poly_table")
    )
  )
    ),
  tabPanel("About & Disclaimer",
           fluidRow(
           column(7,offset=1,p(),
                  tags$strong('Intended Use and Audience'),tags$br(),
                  tags$small('This application is intended for use by scientists and staff at the Alaska Fisheries Science Center, Alaska Regional Office, the Alaska stage agencies, the National Marine Fisheries Service, and other federal agencies.'),p(),
                  tags$strong('Prototype Application; NOT Official Communication'),tags$br(),
                  tags$small('This application should not be cited nor should the information be used without consultation. While the best efforts have been made to insure the highest quality, tools such as this are under constant development and are subject to change.'),br(),tags$small('This application is a Fundamental Research Communication developed and maintained by scientists at the NOAA Fisheries Alaska Fisheries Science Center and should not be construed as official communication of NMFS, NOAA, or the U.S. Dept. of Commerce.'),p(),
                  tags$strong('Metadata and Data Sources'),br(),
                  tags$small('Data sources with links to the NMFS InPort metadata repository are provided below.',
                  tags$ul(
                    tags$li(
                      tags$a(href="https://inport.nmfs.noaa.gov/inport/item/26740", "Aerial Survey Counts of Harbor Seals in Coastal Alaska (1998-2002)")
                    ),
                    tags$li(
                      tags$a(href="https://inport.nmfs.noaa.gov/inport/item/26741",
                             "Aerial Survey Counts of Harbor Seals in Coastal Alaska (2003-2011")
                    ),
                    tags$li(
                      tags$a(href="https://inport.nmfs.noaa.gov/inport/item/17349",
                             "Aerial Survey Units for Harbor Seals in Coastal Alaska")
                    )
                  )),p(),p(tags$small(
                    a(href="https://github.com/jmlondon/akpvsurveys-app",
                      "complete source code available on GitHub")
                  )),
                  tags$strong('Contact Information'),tags$br(),
                  tags$small('Any questions regarding this application should be sent via email to josh.london@noaa.gov')
           ))
  )
  )
),
fluidRow(
  column(12,
         br()
  )
),
fluidRow(
  column(12,
         tags$nav(class="navbar navbar-default navbar-fixed-bottom",
                  style="background:white !important",
                  tags$div(
                    img(src="noaa-fish-logo-left.png",style="float:left"),
                    img(src="noaa-fish-logo-right.png",style="float:right")
                  ),br(),br(),br(),
                  tags$div(
                    tags$small(style="float:right",
                               a(href="http://afsc.noaa.gov","AFSC HOME"),
                               " | ",
                               a(href="http://nmfs.noaa.gov","NOAA FISHERIES"),
                               " | ",
                               a(href="http://www.afsc.noaa.gov/GeneralInfo/disclaimer.htm","AFSC DISCLAIMER")
                    )
                  )
         )
  )
)
)

server <- function(input, output, session) {
  # render our leaflet map with ESRI.NatGeo
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.NatGeoWorldMap") %>% 
      setView(lng = -160, lat = 58, zoom = 4) %>% 
      removeLayersControl()
  })
  
  # reactive to hold our map click coordinates
  poi <- eventReactive(input$mymap_click,{
    return(input$mymap_click)
  })
  
  # reactive to hold our geocode search result
  g_code <- eventReactive(input$geo_button, {
    if(!is.null(input$geo_search)) {
      return(geocode(input$geo_search))
    } else return(NULL)
  })
  
  # recenter view to geocode search result
  observeEvent(g_code(),{
      leafletProxy('mymap') %>%
      clearShapes() %>% clearMarkers() %>%  
        setView(g_code()$lon,g_code()$lat,zoom=8)%>% 
      removeLayersControl()
    })
  
  poly_data <- reactive({
    poi <- poi()
    poi_sp <- SpatialPoints(cbind(poi$lng,poi$lat),
                            proj4string=CRS(proj.4326))
    poi_sp <- spTransform(poi_sp,CRS("+init=epsg:3571"))
    poi_buffer <- rgeos::gWithinDistance(akpvpolys_proj,
                                         poi_sp,input$buffer*1000,
                                         byid=TRUE)
    if(any(c(poi_buffer))) {
      dat <- akpvpolys[c(poi_buffer),]
    }
    else {dat <- NULL}
    return(dat)
  })
  
  # add polygons and markers to map on click
  observe({
    poi <- poi()
    poi_sp <- SpatialPoints(cbind(poi$lng,poi$lat),
                            proj4string=CRS(proj.4326))
    poi_sp <- spTransform(poi_sp,CRS("+init=epsg:3571"))
    poi_buffer <- rgeos::gWithinDistance(akpvpolys_proj,
                                         poi_sp,input$buffer*1000,
                                         byid=TRUE)
    wypts_buffer <- rgeos::gWithinDistance(akpvwypts_proj,
                                           poi_sp,input$buffer*1000,
                                           byid=TRUE)
    if(any(c(poi_buffer))) {
      dat <- list(polys=akpvpolys[c(poi_buffer),],
                  wypts=akpvwypts[c(wypts_buffer),])
    }
    else {dat <- NULL}
    
    leafletProxy("mymap") %>% 
        clearShapes()  %>% clearMarkers() %>% clearMarkerClusters() %>% 
        addCircles(layerId="poi_radius",
                   poi$lng,poi$lat,radius=input$buffer*1000,
                   color="black",fillOpacity = 0,weight=1,
                   group="POI Radius") %>% 
        addPolygons(data=dat$polys,layerId=~polyid,
                    weight=2.5,color="#e31c3d",fillOpacity=0.3,
                    fillColor = "#f9dede",popup=~polyid,
                    group="Survey Units") %>% 
      addCircleMarkers(data=dat$wypts,layerId=~name,
                 radius=6,color="#e7f4e4",fillColor = "#2e8540",
                 fillOpacity = 1,weight=2,
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE),
                 group="Haul-out Wypts") %>% 
      addCircleMarkers(layerId = "poi_marker",
                       poi$lng,poi$lat,radius=10,
                       color="black",fillOpacity = 0.3,weight=1,
                       group="POI") %>% 
      addLayersControl(
        overlayGroups = c("Haul-out Wypts", "Survey Units"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })

  output$plot1 <- renderPlot({
    df <- data.frame(poly_data())
    u <- max(df$abulast,na.rm=TRUE)
    l <- min(df$abulast,na.rm=TRUE)

    ggplot(data=df,aes(x=polyid,y=abulast)) + 
      geom_point(data=subset(df,polyid==input$mymap_shape_click$id),
                 aes(x=polyid,y=abulast),size=10,color="#02bfe7") + 
                   geom_point() +
      geom_pointrange(aes(ymax=sqrt(abulastvar)+abulast,
                          ymin=abulast-sqrt(abulastvar)),size=1) +
      geom_hline(y=u,color='red',alpha=0.5) + 
      geom_hline(y=l,color='red',alpha=0.5) +
      labs(x='Survey Unit Identifier',y='Abundance Estimate') +
      ggtitle('2011 Abundance Estimate by Survey Unit') +
      theme(plot.title = element_text(size=14, face="bold", vjust=2),
            axis.title.y = element_text(vjust=1),
            axis.title.x = element_text(vjust=-0.5))
  })
  
  output$plot2 <- renderPlot({
    df <- data.frame(poly_data()) %>% dplyr::select(abulast,abulastvar)
    df <- dplyr::summarise(df,abulast.sum=sum(abulast,na.rm=TRUE),
                           abulast.se=sqrt(sum(abulastvar,na.rm=TRUE)),
                           label="total")
    
   ggplot(data=df,aes(x=label,y=abulast.sum)) + geom_point() +
     geom_pointrange(aes(ymax=(abulast.sum+abulast.se),
                         ymin=(abulast.sum-abulast.se)),size=1) +
     labs(x='',y='Abundance Estimate') +
     ggtitle('2011 Total Abundance') +
     theme(plot.title = element_text(size=14, face="bold", vjust=2),
           axis.ticks.x = element_blank(),axis.text.x = element_blank(),
           axis.title.y = element_text(vjust=1))
  })
  
  output$akpv_poly_table <- renderDataTable({
    data.frame(poly_data()) %>% 
      mutate(abulastse = sqrt(abulastvar)) %>% 
      select(polyid,stockname,abulast,abulastse) %>%  
      filter(!is.na(abulast)) %>% 
    datatable(colnames=c('Survey Unit ID' = 'polyid',
                         'Stock Name' = 'stockname',
                         '2011 Abundance Est.' = 'abulast',
                         'SE Est.' = 'abulastse'),
              rownames=FALSE) %>% 
      formatRound(c('2011 Abundance Est.','SE Est.'),2)
  }, options = list(orderClasses = TRUE))

}

shinyApp(ui, server)
