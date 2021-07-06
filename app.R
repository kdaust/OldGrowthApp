library(shiny)
library(sf)
library(leaflet)
library(data.table)
library(htmltools)
library(htmlwidgets)
library(rhandsontable)
library(shinyalert)
source("./OGSource.R")

mbtk <- "pk.eyJ1Ijoia2lyaWRhdXN0IiwiYSI6ImNraDJjOTNxNzBucm0ycWxxbTlrOHY5OTEifQ.GybbrNS0kJ3VZ_lGCpXwMA"

defer_server <- "http://142.93.148.116/data/Defer/{z}/{x}/{y}.pbf"
defer_layer <- "Defer"
rare_server <- "http://142.93.148.116/data/Rare/{z}/{x}/{y}.pbf"
rare_layer <- "Rare"
ancient_server <- "http://142.93.148.116/data/Ancient/{z}/{x}/{y}.pbf"
ancient_layer <- "Ancient"
cb_server <- "http://142.93.148.116/data/Cutblocks/{z}/{x}/{y}.pbf"
cb_layer <- "Cutblocks"
seral_server <- "http://142.93.148.116/data/Seral/{z}/{x}/{y}.pbf"
seral_layer <- "Seral"

colDefer <- data.table(ID = c(1,2,3),Col = c("#d41919","#d44402","#d402d1"))
colAncient <- data.table(ID = c(1,2),Col = c("#d61593","#d61593"))
colRare <- data.table(ID = c(1,2),Col = c("#158cd6","#1d357d"))
colCB <- data.table(ID = c(1,2,3),Col = c("#59341d","#a14c18","#e0996e"))
colSeral <- data.table(ID = c(3,4),Col = c("#3ac421", "#1d6e3e"))

load("./InputDat/Defer_Info.Rdata")
load("./InputDat/Ancient_Info.Rdata")
ancientDat <- rareDat
load("./InputDat/Rare_Info.Rdata")

deferDatBGC <- fread("./InputDat/DeferByBGC.csv")
forestDatBGC <- fread("./InputDat/ForestByBGC.csv")

ui <- fluidPage(
  h1("Forest Vision", style = "color: gold; background-color: #215c21; text-align: center; border-radius: 1em; padding: .5em; font-family: calibri, sans-serif; font-size: 2.8em;"),
  fluidRow(
    column(2,
           h2("Instructions"),
           p("You can turn layers on or off using the pop-up box on the top right.
             There are various choices of base layers and overlay layers. To get 
             summarised statistics by polygon, click on a polygon. To show summaries
             by BEC subzone/variant, select the BGC base layer, and click on the BGC unit.")
           # checkboxGroupInput("seralClass",label = "Show seral stage:",
           #                           choices = c(3,4),selected = 4,inline = T)
           ),
    column(10,
           useShinyalert(),
           leafletjs_defer,
           leafletOutput("map", height = "90vh")
           )
      )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(lng = -122.77222, lat = 51.2665, zoom = 9) %>%
        addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                         options = leaflet::pathOptions(pane = "mapPane")) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                  options = leaflet::pathOptions(pane = "mapPane")) %>%
        addPlugin() %>%
        addBGCTiles() %>%
        addTiles(urlTemplate = "http://142.93.148.116/data/cSI/{z}/{x}/{y}.png",
                 group = "SiteIndex",options = tileOptions(maxZoom = 15,maxNativeZoom = 12)) %>% 
        addTiles(urlTemplate = "http://142.93.148.116/data/Disturb/{z}/{x}/{y}.png",
                 group = "Disturbance",options = tileOptions(maxZoom = 15,maxNativeZoom = 12)) %>% 
        addTiles(urlTemplate = "http://142.93.148.116/data/Protected/{z}/{x}/{y}.png",
                 group = "Protected",options = tileOptions(maxZoom = 15,maxNativeZoom = 12)) %>% 
        addTiles(urlTemplate = "http://142.93.148.116/data/TreeHt/{z}/{x}/{y}.png",
                 group = "TreeHeight",options = tileOptions(maxZoom = 15,maxNativeZoom = 12)) %>% 
        addTiles(urlTemplate = "http://142.93.148.116/data/TreeVol/{z}/{x}/{y}.png",
                 group = "TreeVolume",options = tileOptions(maxZoom = 15,maxNativeZoom = 12)) %>% 
        addTiles(urlTemplate = "http://142.93.148.116/data/Seral/{z}/{x}/{y}.png",
                 group = "TreeVolume",options = tileOptions(maxZoom = 15,maxNativeZoom = 12)) %>% 
        invokeMethod(data = colDefer, method = "addOGTiles", 
                     ~ID, ~Col, defer_server, defer_layer,1) %>%
        invokeMethod(data = colRare, method = "addOGTiles", 
                     ~ID, ~Col, rare_server, rare_layer,1) %>%
        invokeMethod(data = colAncient, method = "addOGTiles", 
                     ~ID, ~Col, ancient_server, ancient_layer,1) %>%
        invokeMethod(data = colCB, method = "addOGTiles",
                     ~ID, ~Col, cb_server, cb_layer,0.5) %>%
        leaflet::addLayersControl(
          baseGroups = c("Positron","Satellite","OpenStreetMap","BGCs"),
          overlayGroups = c("SiteIndex","Disturbance","Protected","TreeHeight","TreeVolume",
                            "Seral","Defer","Rare","Ancient","Cutblocks"),
          position = "topright") %>%
        hideGroup(c("SiteIndex","Disturbance","Protected","TreeHeight","TreeVolume","Seral","Cutblocks"))
    })
    
    observeEvent(input$seralClass,{
      show <- input$seralClass
      hide <- c(3,4)[!c(3,4) %in% show]
      hide <- as.character(hide)
      dat <- list(show = show,hide = hide)
      print(dat)
      session$sendCustomMessage("hideSeral",dat)
    })
    
    observeEvent(input$defer_click,{
      output$defer_info <- renderRHandsontable({
        if(input$layer_click == "Defer"){
          d1 <- deferDat[PolyID == input$defer_click,!"PolyID"]
        }else if(input$layer_click == "Rare"){
          d1 <- rareDat[PolyID == input$defer_click,!"PolyID"]
        }else{
          d1 <- ancientDat[PolyID == input$defer_click,!"PolyID"]
        }
        d1 <- t(d1)
        colnames(d1) <- "Value"
        rhandsontable(d1,rowHeaderWidth = 140) %>%
          hot_cols(colWidths = 100)
      })
  })
    
    getBGCdat <- reactive({
      dat <- deferDatBGC[BGC == input$bgc_click,.(SIClass,Area,Var)]
      dat <- dcast(dat, Var ~ SIClass, value.var = "Area")
      dat2 <- forestDatBGC[BGC == input$bgc_click,.(SIClass,Area,Var)]
      dat2 <- dcast(dat2, Var ~ SIClass, value.var = "Area")
      return(list(defer = dat, forest = dat2))
    })
    
    output$hot_defer <- renderRHandsontable({
      dat <- getBGCdat()$defer
      rhandsontable(dat)
    })
    
    output$hot_forest <- renderRHandsontable({
      dat <- getBGCdat()$forest
      rhandsontable(dat)
    })
    
    observeEvent(input$bgc_click,{
      shinyalert(html = T,
                 text = tagList(
                   h3(paste0("Summary by BGC - ",input$bgc_click)),
                   hr(),
                   h4("Forest"),
                   rHandsontableOutput("hot_forest"),
                   hr(),
                   h4("Defer"),
                   rHandsontableOutput("hot_defer")
                   )
                   
                 )
    })
    
    observeEvent(input$defer_click,{
      
      shinyalert(html = T,
                 text = tagList(
                   h3(paste0("Summary by Polygon: ",input$layer_click)),
                   hr(),
                   rHandsontableOutput("defer_info")
                 )
                 
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
