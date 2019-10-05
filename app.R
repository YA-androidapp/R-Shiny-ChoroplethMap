# Copyright (c) 2019 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.



# # Packages
# update.packages(ask = FALSE)

inst <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

inst("dplyr")
inst("httpuv")
inst("leaflet")
inst("rgdal")
inst("shiny")



# Dataset
shape_path = "shape/h27ka13.shp"
data_path = "H30.csv"



# Proc
shape <-
  readOGR(shape_path, stringsAsFactors = FALSE, encoding = "UTF-8")
shape@data$市区町丁  <-
  paste0(shape@data$CITY_NAME, ifelse(is.na(shape@data$S_NAME), "", shape@data$S_NAME))
head(shape@data)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("mymap"),
        p(),
        actionButton("recalc", "New points"),
        getwd()
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data_density <- eventReactive(input$recalc, {
    datacsv <-
      read.csv(data_path,
               stringsAsFactors = FALSE,
               fileEncoding = "UTF-8")
    head(datacsv)
    
    joined <-
      left_join(shape@data, datacsv, by = "市区町丁") # キーにしたい列名が異なる場合: by = c("CITY_NAME" = "市区町丁")
    
    nrow(shape@data)
    nrow(datacsv)
    nrow(joined)
    
    population_density <-
      as.numeric(shape@data$JINKO) / shape@data$AREA * 1000000 # 単位面積1 km2当たり人口密度
    household_density <-
      as.numeric(shape@data$SETAI) / shape@data$AREA * 1000000 # 単位面積1 km2当たり世帯密度
    
    crimecase_density <-
      as.numeric(joined$総合計) / as.numeric(shape@data$JINKO) * 100000 # 人口100000人当たり認知件数
    
    # data_density <- population_density
    # data_density <- household_density
    data_density <- crimecase_density
    
    
    # 欠損値(NA)を0で置換する
    data_density[is.na(data_density)] <- 0
    
    return(cbind(shape@data$X_CODE, shape@data$Y_CODE))
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = data_density())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

