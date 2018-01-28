library(leaflet)
library(shiny)
library(sf)

https://stackoverflow.com/q/41104576/6871135


# myData <- st_read(system.file("gpkg/nc.gpkg", package="sf")) %>%
#    st_transform(4326) %>%
#   dplyr::mutate(id =
#   as("Spatial")

myData <- get_boundaries("ITA", 2) %>%
  st_transform(4326) %>%
  dplyr::filter(NAME_1 == "Lombardia") %>%
  dplyr::select(1:8) %>%
  dplyr::mutate(id = 1:13)

tbl_data <<- sf::st_set_geometry(myData, NULL)

ui <- fluidPage(
  leafletOutput("map"),
  p(),
  rHandsontableOutput("hot")
)
server <- shinyServer(function(input, output, session) {
  mydata <- reactiveVal(value = myData)
  # browser()
  # produce the basic leaflet map with single marker
  output$map <- renderLeaflet({
    factpal <- colorFactor(topo.colors(20), mydata()$NAME_2)
    state_popup <- paste0("<strong>Name of the country </strong>",
                          mydata()$NAME_2,
                          "<br><strong> information is  </strong>",
                          mydata()$NAME_2)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data=mydata(),
                  layerId=~id,
                  fillColor= ~factpal(NAME_2),
                  fillOpacity = 0.7,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = state_popup,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
  }
  )

  output$hot  <- renderRHandsontable({
    #   # if (!is.null(input$hot)) {
    #   # DF <- hot_to_r(input$hot)
    #   # myData[id == p$id, ] <- DF
    #   # setHot(DF)
    #
    #   # } else {
    #   # DF <- myData # read in our csv
    #   # setHot(DF)
    #   # }
    #   # subtable <- dplyr::filter(tbl_data)
    tbl <- mydata() %>% sf::st_set_geometry(NULL)
    # print(tbl)
    rhandsontable::rhandsontable(tbl, readOnly = FALSE)
  })

  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    aaa <- mydata()
    #append all click ids in empty vector
    clickedIds$ids <- c(clickedIds$ids, click$id)
    #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
    clickedPolys <- rwa[rwa@data$NAME_1 %in% clickedIds$ids, ]
    clickedPolys <- aaa[aaa$id %in% p$id, ]
    leafletProxy("map", session) %>% addPolygons(data = clickedPolys,
                          fillColor = "red",
                          fillOpacity = 1,
                          weight = 1,
                          color = "black",
                          stroke = T)
    # leafletProxy("map", session) %>%
    #   removeShape('selected') %>%
    #   addPolygons(data = mydata()[mydata()$id == click_cnt(), ],
    #               fillcolor = FALSE,
    #               color = '#00FFFF', opacity = 1, layerId = 'selected')
    output$hot  <- renderRHandsontable({
      tbl <- mydata() %>%
        dplyr::filter(id == p$id) %>%
        sf::st_set_geometry(NULL)
      rhandsontable::rhandsontable(tbl,readOnly = FALSE)
    })
  })

  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF <- hot_to_r(input$hot)
      # print(DF$id)
      print(DF)
      newmydata <- mydata()
      aaa <- which(newmydata$id == DF$id)
      print(aaa)
      newmydata[aaa, 1:8] <- DF
      # print(newmydata[aaa,])
      # print(newmydata)
      # newmydata[newmydata$id %in% DF$id,] <- DF
      # browser()
      # mydata <- DF
      # print(mydata)
      mydata(newmydata)
      # tbl_data[tbl_data$id == DF$id] <- DF
      # print( tbl_data[tbl_data$id == DF$id, ] )

      # myData[myData$id == DF$id, ] <- DF
    #   values[["previous"]] <- isolate(values[["DF"]])
    #   DF = hot_to_r(input$hot)
    # } else {
    #   if (is.null(values[["DF"]]))
    #     DF <- DF
    #   else
    #     DF <- values[["DF"]]
    # }
    # values[["DF"]] <- DF
    # print("change")
    # myData
  }
  })
})
shinyApp(ui, server)
