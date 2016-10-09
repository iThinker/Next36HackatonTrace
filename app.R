library(shiny)
library(shinydashboard)
library(sp)
library(data.table)
library(DT)
library(leaflet)
library(data.table)
library(RColorBrewer)
library(rsconnect)
deployApp()

load("streets.RData")
ratings_sort=data.table(ratings)[order(rank(streetID),overall,dateLastOverlay)]
ratings_sort=ratings_sort[,.SD[1],by=streetID]  
ratings_sort=ratings_sort[order(overall)]
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Book This Street"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Current Road Status", 
               tabName = "currentstatus", icon = icon("dashboard")),
      menuItem("Create Maintainence Plan", tabName = "createplan", 
               icon = icon("map"))
      
      )
    ),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}")    ,
    tabItems(
      tabItem(tabName = "currentstatus",
              column(width = 7,box(leafletOutput("mymap"),width="75%%")),
              column(width = 4,dataTableOutput("table"))
              #
              
      )
  ))
)

server <- function(input, output,session) {
  ratings=reactive({ratings_sort})
  output$table <-
      DT::renderDataTable(ratings()[,.(streetID,`Pothole Count`=Potholes,`Overall Rating`=overall,`Last Overlay`=dateLastOverlay)],options = list(
        pageLength = 5),selection = 'single')
  

  clrs <- rev(brewer.pal(9, "YlOrRd"))
  
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