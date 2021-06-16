library(shiny)
library(sf)
library(leaflet)
library(data.table)
library(htmltools)
library(htmlwidgets)
library(rhandsontable)
source("./OGSource.R")

defer_server <- "http://142.93.148.116/data/Defer/{z}/{x}/{y}.pbf"
defer_layer <- "Defer"
rare_server <- "http://142.93.148.116/data/Rare/{z}/{x}/{y}.pbf"
rare_layer <- "Rare"
ancient_server <- "http://142.93.148.116/data/Ancient/{z}/{x}/{y}.pbf"
ancient_layer <- "Ancient"
cb_server <- "http://142.93.148.116/data/Cutblocks/{z}/{x}/{y}.pbf"
cb_layer <- "Cutblocks"

colDefer <- data.table(ID = c(1,2,3),Col = c("#d41919","#d44402","#d402d1"))
colAncient <- data.table(ID = c(1,2),Col = c("#d61593","#d61593"))
colRare <- data.table(ID = c(1,2),Col = c("#158cd6","#1d357d"))
colCB <- data.table(ID = c(1,2,3),Col = c("#59341d","#a14c18","#e0996e"))

load("Defer_Info.Rdata")
dat[,Area := as.numeric(Area)]

ui <- fluidPage(
  fluidRow(column(2
                  ),
           column(10,
                  rHandsontableOutput("defer_info"),
                  leafletjs_defer,
                  leafletOutput("map", height = "95vh")
                  )
           )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(lng = -122.77222, lat = 51.2665, zoom = 9) %>%
        addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                         options = leaflet::pathOptions(pane = "mapPane")) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                  options = leaflet::pathOptions(pane = "mapPane")) %>%
        addPlugin() %>%
        addBGCTiles() %>%
        invokeMethod(data = colDefer, method = "addOGTiles", 
                     ~ID, ~Col, defer_server, defer_layer,1) %>%
        invokeMethod(data = colRare, method = "addOGTiles", 
                     ~ID, ~Col, rare_server, rare_layer,1) %>%
        invokeMethod(data = colAncient, method = "addOGTiles", 
                     ~ID, ~Col, ancient_server, ancient_layer,1) %>%
        invokeMethod(data = colCB, method = "addOGTiles",
                     ~ID, ~Col, cb_server, cb_layer,0.5) %>%
        leaflet::addLayersControl(
          baseGroups = c("Positron","Satellite", "OpenStreetMap","BGCs"),
          overlayGroups = c("Defer","Rare","Ancient","Cutblocks"),
          position = "topright")
    })
    
    observeEvent(input$defer_click,{
      output$clkID <- renderText({
        input$defer_click
      })
    })
    
    observeEvent(input$defer_click,{
      if(input$layer_click == "Defer"){
        output$defer_info <- renderRHandsontable({
          d1 <- dat[PolyID == input$defer_click,]
          rhandsontable(d1)
        })
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
