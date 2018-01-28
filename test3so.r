library(rgdal)
library(leaflet)

tmp <- tempdir()
url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)
mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")

pal <- colorQuantile("YlGn", NULL, n = 5)
state_popup <- paste0("<strong>Estado: </strong>",
                      mexico$name,
                      "<br><strong>PIB per c?pita, miles de pesos, 2008: </strong>",
                      mexico$gdp08)

# load necessary packages
library(leaflet)
library(shiny)
library(shinydashboard)


ui <- fluidPage(
  # place the contents inside a box
  shinydashboard::box(
    width = 12
    , title = "Click on the map!"
    # separate the box by a column
    , column(
      width = 2
      , shiny::actionButton( inputId = "clearHighlight"
                             , icon = icon( name = "eraser")
                             , label = "Clear the Map"
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
      )
    )
    # separate the box by a column
    , column(
      width = 10
      , leaflet::leafletOutput( outputId = "myMap"
                                , height = 850
      )
    )
  ) # end of the box
) # end of fluid page

# create the server
server <- function( input, output, session ){

  # function to create foundational map
  foundational.map <- function(){
    leaflet() %>%
      #addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      #setView( lng = -87.567215
      #         , lat = 41.822582
      #         , zoom = 11 ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons( data = mexico
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = mexico$state
                   , group = "click.list")
  }

  # reactiveVal for the map object, and corresponding output object.
  myMap_reval <- reactiveVal(foundational.map())
  output$myMap <- renderLeaflet({
    myMap_reval()
  })

  # To hold the selected map region id.
  click.list <- shiny::reactiveValues( ids = vector() )

  shiny::observeEvent( input$myMap_shape_click, ignoreNULL = T,ignoreInit = T, {

    # If already selected, first remove previous selection
    if(length(click.list)>0)
    {
      remove_id = click.list$ids
      lines.of.interest <- mexico[ which( mexico$state %in% remove_id) , ]
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        addPolylines( data = lines.of.interest
                      , layerId = lines.of.interest@data$id
                      , color = "#000000"
                      , weight = 2
                      , opacity = 0.2)
    }

    # add current selection
    click <- input$myMap_shape_click
    click.list$ids <- click$id  # we only store the last click now!
    lines.of.interest <- mexico[ which( mexico$state %in% click.list$ids ) , ]
    print(click)
    if( is.null( click$id ) ){
      req( click$id )
    } else if( !click$id %in% lines.of.interest@data$id ){
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        addPolylines( data = lines.of.interest
                      , layerId = lines.of.interest@data$id
                      , color = "#6cb5bc"
                      , weight = 5
                      , opacity = 1
        )
    }

  }) # end of shiny::observeEvent({})

  # oberver for the clearHighlight button.
  shiny::observeEvent( input$clearHighlight, {
    click.list$ids <- NULL
    myMap_reval(foundational.map()) # reset map.
  })

}

shiny::shinyApp( ui = ui, server = server)
