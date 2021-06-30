bgc_tileserver <- "http://159.203.20.90/data/WNA_MAP/{z}/{x}/{y}.pbf"
bgc_tilelayer <- "WNA_MAP"

plugins <- {list(vgplugin = 
                   htmltools::htmlDependency(
                     name = "leaflet.vectorgrid",
                     version = "1.3.0",
                     src = "htmlwidgets",
                     script = "lfx-vgrid-prod.js"
                   )
)
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

addPlugin <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map
}

jscode_defer <- paste0(
'window.LeafletWidget.methods.addOGTiles = function(Class,Colour,server,layID,opa) {
      var subzoneColors = {};
      Class.forEach((id,i) => {
        const col = Colour[i];
        subzoneColors[id] = col;
      });
      
      var map = this;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, 
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: opa
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        server,
        vectorTileOptions(layID, layID, true,
                          "tilePane", subzoneColors, "ID", "ID")
      )
      console.log(subzLayer);
      this.layerManager.addLayer(subzLayer, "tile", layID, layID);

      subzLayer.on("click", function(e){
        Shiny.setInputValue("layer_click",layID);
        Shiny.setInputValue("defer_click",e.layer.properties.PolyID);
      });

      subzLayer.bringToFront();
      
      var prevPest = ["SBSdk","IDFdk3"];
      //update style for pests
      Shiny.addCustomMessageHandler("colourPest",function(fhDat){
        var pestBGC = fhDat["bgc"];
        var fhCols = fhDat["fhcol"];
        console.log(fhDat);
        prevPest.forEach((hl,i) => {
          subzLayer.resetFeatureStyle(hl);
        });
        prevPest = pestBGC;
        pestBGC.forEach((ID,i) => {
          let styleFH = {
            weight: 0,
            fillColor: fhCols[i],
            fillOpacity: 1,
            fill: true
          };
          subzLayer.setFeatureStyle(ID, styleFH);
        });

      });
      
    };

    window.LeafletWidget.methods.addSeralTiles = function(Class,Colour,server,layID,opa) {
      var subzoneColors = {};
      Class.forEach((id,i) => {
        const col = Colour[i];
        subzoneColors[id] = col;
      });
      
      var map = this;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, 
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: opa
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        server,
        vectorTileOptions(layID, layID, true,
                          "tilePane", subzoneColors, "Seral", "Seral")
      )
      this.layerManager.addLayer(subzLayer, "tile", layID, layID);

      subzLayer.on("click", function(e){
        Shiny.setInputValue("seral_click",e.layer.properties.PolyID);
      });

      subzLayer.bringToFront();
      
    };')

leafletjs_defer <-  tags$head(
  tags$script(HTML(
    jscode_defer
  ))
)

subzones_colours_ref <- fread("WNA_v12_HexCols.csv")

addBGCTiles <- function(map) {
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$BGC, "':'", subzones_colours_ref$Col,"'", collapse = ","), "}"), '
      
      L.bec_layer_opacity = 0.5
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: L.bec_layer_opacity
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bgc_tileserver, '",
        vectorTileOptions("bec_map", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_map", "BGCs");
     subzLayer.on("click", function(e){
      Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
     });
     
     var selectHighlight = "SBSdk";
      subzLayer.on("click", function(e){
        console.log(e.layer);
        subzLayer.resetFeatureStyle(selectHighlight);
        Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
        var properties = e.layer.properties
  			  highlight = properties.MAP_LABEL
  			  var style = {
            weight: 1,
            color: "#fc036f",
            fillColor: subzoneColors[properties.MAP_LABEL],
            fillOpacity: 1,
            fill: true
          }
          subzLayer.setFeatureStyle(properties.MAP_LABEL, style);
      });
      
      
      var highlight
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
		  
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
     
    }'
  ))
  map
}
