# Copyright (c) 2019 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.



# # Packages
# update.packages(ask = FALSE)

inst <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
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
shape@data$市区町丁          <-
  paste0(shape@data$CITY_NAME, ifelse(is.na(shape@data$S_NAME), "", shape@data$S_NAME))
head(shape@data)



# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("人口100000人当たり認知件数"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),
      
      tags$hr(),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons(
        "sep",
        "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ","
      ),
      
      radioButtons(
        "quote",
        "Quote",
        choices = c(
          None = "",
          "Double Quote" = '"',
          "Single Quote" = "'"
        ),
        selected = '"'
      ),
      
      tags$hr(),
      
      fileInput(
        "file1",
        "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mymap"),
      p(),
      actionButton("recalc", "Draw"),
      getwd()
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  data_density <- NULL
  color_pallet <- NULL
  labels <- NULL
  
  color_pallet_reactive <- eventReactive(input$recalc, {
    # req(input$file1)
    
    tryCatch({
      datacsv <- read.csv(
        # input$file1$datapath,
        data_path,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        stringsAsFactors = FALSE,
        fileEncoding = "UTF-8"
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    
    if (input$header == FALSE) {
      cnames <- paste("c", 0:(ncol(datacsv) - 1), sep = "")
      cnames[[1]] <- "市区町丁"
      cnames[[2]] <- "value"
      colnames(datacsv) <- cnames
    }
    print(colnames(datacsv))
    print(head(datacsv))
    
    joined <-
      left_join(shape@data, datacsv, by = "市区町丁") # キーにしたい列名が異なる場合: by = c("CITY_NAME" = "市区町丁")
    
    nrow(shape@data)
    nrow(datacsv)
    nrow(joined)
    
    # population_density <-
    #   as.numeric(shape@data$JINKO) / shape@data$AREA * 1000000 # 単位面積1 km2当たり人口密度
    # household_density <-
    #   as.numeric(shape@data$SETAI) / shape@data$AREA * 1000000 # 単位面積1 km2当たり世帯密度
    
    if (input$header) {
      crimecase_density <-
        as.numeric(joined$総合計) / as.numeric(shape@data$JINKO) * 100000 # 人口100000人当たり認知件数
    } else {
      crimecase_density <-
        as.numeric(joined$value) / as.numeric(shape@data$JINKO) # 人口1人当たり認知件数
    }
    
    # 欠損値(NA)を0で置換する
    crimecase_density[is.na(crimecase_density)] <- 0
    data_density <<- crimecase_density
    
    color_pallet <- colorQuantile("Blues",
                                  domain = data_density,
                                  reverse = F,
                                  n = 6) # 分位数で塗り分け
    
    labels <<- sprintf("<strong>%s</strong><br/>%5.1f",
                       paste0(joined$MOJI),
                       data_density) %>% lapply(htmltools::HTML)
    
    return(color_pallet)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    color_pallet <- color_pallet_reactive()
    shape %>%
      leaflet() %>%
      setView(lat = 35.65,
              lng = 139.75,
              zoom = 12) %>% # 初期表示
      addProviderTiles(providers$CartoDB.Positron) %>% # ベースマップ
      addPolygons(
        fillOpacity = 0.7,
        weight = 1,
        color = "#666",
        fillColor = ~ color_pallet(data_density),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      # 凡例
      addLegend(
        "bottomright",
        pal = color_pallet,
        values = ~ data_density,
        title = ifelse(input$header, "人口100000人当たり認知件数", "人口1人当たり数")
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
