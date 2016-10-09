library(shiny)
library(shinydashboard)
library(sp)
library(data.table)
library(DT)
library(leaflet)
library(data.table)
library(RColorBrewer)
library(colorRamps)
load("streets.RData")

ratings_sort=data.table(ratings)[order(rank(streetID),overall,-Potholes,dateLastOverlay)]
ratings_sort=ratings_sort[,.SD[1],by=streetID]  
ratings_sort=ratings_sort[order(overall,-Potholes)]

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Book This Street"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Current Road Status", 
                                 tabName = "currentstatus", icon = icon("dashboard")),
                        menuItem("Plan Maintainence", tabName = "createplan", 
                                 icon = icon("map"))
                        
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "currentstatus",
                                column(width = 12,
                                       fluidRow(
                                         box(
                                           width = 3,height = "50%", background = "black",
                                           "The top 5 road sections requiring the most urgent repair is shown in the table to the right.\n Each section was ranked on the current rating, the number of potholes reparied and date of last overlay.
                                            To view a prioritized road section on the map, click on the corresponding row in the table."
                                         ),
                                         box(dataTableOutput("table"),width = 9,height = "50%")),
                                       fluidRow(
                                         box(leafletOutput("mymap"),width="100%",height = "50%")))
                                #
                                
                          ),
                        tabItem(tabName = "createplan",
                                browseURL("Next36HackatonTrace-master/www/book.html", browser = getOption("browser"), encodeIfNeeded = FALSE)
                                )
                        
                      ))
)

server <- function(input, output,session) {
  ratings=reactive({ratings_sort})
  output$table <-
    DT::renderDataTable(ratings()[,.(`Block Section`=block,`Street Name`=paste(streetName,streetType,streetDirection),`Recent Pothole Count`=Potholes,`Overall Rating`=overall,`Year of Last Overlay`=dateLastOverlay)],options = list(
      pageLength = 5),selection = 'single')
  
  
  clrs <- rev(green2red(10))
  
  pal <- colorNumeric(
    palette = clrs,
    domain = streets@data$overall
  )
  observeEvent(input$table_rows_selected, {
    str(input$table_rows_selected)
    x=bbox(streets[data.frame(ratings())[as.numeric(input$table_rows_selected)[length(as.numeric(input$table_rows_selected))],"streetID"]==streets@data$STREET_ID,])
    leafletProxy("mymap") %>% fitBounds(lng1 = x[1,1],lng2 = x[1,2],lat1 = x[2,1],lat2 = x[2,2])
  })
  output$mymap <- renderLeaflet({
    leaflet(streets)%>% setView(lng = -76.142608, lat = 43.046998, zoom = 12) %>%
      addProviderTiles("Stamen.Toner",options = providerTileOptions(noWrap = TRUE),group = "Toner") %>% addPolylines(col=~pal(overall),group="Streets Overlay")%>%
      addLegend("bottomright", pal = pal, values = ~overall,
                title = "Road Quality",
                opacity = 1)%>%addMarkers(lng = potholes@data$Lat, lat = potholes@data$Lon,clusterOptions = markerClusterOptions(),group="Completed Potholes") %>%addLayersControl(
                  baseGroups = c("Toner"),
                  overlayGroups = c("Streets Overlay", "Completed Potholes"),
                  options = layersControlOptions(collapsed = FALSE))
  })
}

shinyApp(ui, server)