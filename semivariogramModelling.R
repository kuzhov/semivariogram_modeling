library(shiny)
library(gstat)
library(rgdal)
library(ggplot2)


# UI
ui = fluidPage(
  
  titlePanel("Semivariogram modelling"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file", "Load data [.csv]",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),

      checkboxInput("header", "Header", TRUE),
      
      div(style="display: inline-block;vertical-align:top; width: 24%;",selectInput("columnX", "Select column X", " ")),
      div(style="display: inline-block;vertical-align:top; width: 24%;",selectInput("columnY", "Select column Y", " ")),
      
      div(style="display: inline-block;vertical-align:top; width: 24%;",selectInput("columnData", "Select data column", " ")),
      
      div(style="display: inline-block;vertical-align:top; width: 25%;",numericInput("epsg", 
                   ("EPSG"), 
                   value = 0)),     
      
      
      
      tags$hr(),
      
      #
      #Cutoff
      #
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("cutoff_min", 
                                                                                     h6("Min"), 
                                                                                     value = 0)),
      div(style="display: inline-block;vertical-align:top; width: 68%;",uiOutput("slider_cutoff")),
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("cutoff_max", 
                                                                                     h6("Max"), 
                                                                                     value = 500000)),
      #
      #Width
      #
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("width_min", 
                                                                                     h6("Min"), 
                                                                                     value = 0)),
      div(style="display: inline-block;vertical-align:top; width: 68%;",uiOutput("slider_width")),
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("width_max", 
                                                                                     h6("Max"), 
                                                                                     value = 10000)),
      
      selectInput("model", h5("Select model"), c('Nug','Exp','Sph','Gau','Exc','Mat','Ste','Cir','Lin','Bes','Pen','Per','Wav','Hol','Log','Pow','Spl','Leg','Err','Int')),
      
      #
      #Nugget
      #
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("nug_min", 
                                                                                     h6("Min"), 
                                                                                     value = 0)),
      div(style="display: inline-block;vertical-align:top; width: 68%;",uiOutput("slider_nug")),
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("nug_max", 
                                                                                     h6("Max"), 
                                                                                     value = 1000)),
      #
      #Sill
      #
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("sill_min", 
                                                                                     h6("Min"), 
                                                                                     value = 0)),
      div(style="display: inline-block;vertical-align:top; width: 68%;",uiOutput("slider_sill")),
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("sill_max", 
                                                                                     h6("Max"), 
                                                                                     value = 10000)),
      #
      #Range
      #
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("range_min", 
                                                                                     h6("Min"), 
                                                                                     value = 0)),
      div(style="display: inline-block;vertical-align:top; width: 68%;",uiOutput("slider_range")),
      div(style="display: inline-block;vertical-align:top; width: 15%;",numericInput("range_max", 
                                                                                     h6("Max"), 
                                                                                     value = 300000))
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Variogram Map", plotOutput("mapa")),
                  tabPanel("Semivariogram", plotOutput("wariogram"))
                  )
      

    )
  )
)

server = function(input, output, session) {
  
  
  data = reactive({
    inputFile = input$file
    if(is.null(inputFile)) return(NULL)
    
    dane = read.csv(inputFile$datapath, header = input$header)
    
  })
  
  

  observe({
    updateSelectInput(session, "columnData", choices = names(data())) 
  })
  observe({
    updateSelectInput(session, "columnX", choices = names(data())) 
  })
  observe({
    updateSelectInput(session, "columnY", choices = names(data())) 
  })
  


  output$slider_cutoff = renderUI({
    sliderInput("cutoff", "Cutoff", min=input$cutoff_min, max=input$cutoff_max, value=250000)
  })
  output$slider_width = renderUI({
    sliderInput("width", "Width", min=input$width_min, max=input$width_max, value=5000)
  })
  output$slider_nug = renderUI({
    sliderInput("nugget", "Nugget", min=input$nug_min, max=input$nug_max, value=0)
  })
  output$slider_sill = renderUI({
    sliderInput("psill", "Sill", min=input$sill_min, max=input$sill_max, value=5000)
  })
  output$slider_range = renderUI({
    sliderInput("range", "Range", min=input$range_min, max=input$range_max, value=0)
  })
  
  output$table = renderTable({
    
    inputFile = input$file
    
    if (is.null(inputFile))
      print('Please load data')
    else{
    inputFile = read.csv(inputFile$datapath, header = input$header)
    inputPrzerobiony = inputFile
    head(inputFile, n=25)
  }})
  output$wariogram = renderPlot({
    
    inputFile = input$file
    if (is.null(inputFile))
      return(NULL)
    if (input$epsg == 0)
      print('Please enter EPSG')
    else{
    inputFile = read.csv(inputFile$datapath, header = input$header)
    inputPrzerobiony = inputFile
    
    # Zmuszanie Shiny do brania polozenia z kolumn wybranych w selectInput
    kolumnaX = subset(inputFile, select = input$columnX)
    # inputPrzerobiony$shinyX = kolumnaX
    inputPrzerobiony[ , "shinyX"] = kolumnaX
    
    kolumnaY = subset(inputFile, select = input$columnY)
    # inputPrzerobiony$shinyY = kolumnaY
    inputPrzerobiony[ , "shinyY"] = kolumnaY
    
    # Zmuszanie Shiny do brania danych z wybranej kolumny
    kolumnaData = subset(inputFile, select = input$columnData)
    inputPrzerobiony[ , "daneWejsciowe"] = kolumnaData
    
    coordinates(inputPrzerobiony) = ~shinyX+shinyY
    
    modelEPSG = as.character(input$epsg)
    projection = c('+init=epsg:',modelEPSG)
    projection = paste(projection,collapse='')
    proj4string(inputPrzerobiony) = projection
    
    inputNAfree = inputPrzerobiony[!is.na(inputPrzerobiony@data$daneWejsciowe),]
    
    wariogram = variogram(daneWejsciowe~1, inputNAfree, cutoff = input$cutoff, width = input$width)
    modelLine = vgm(nugget = input$nugget,psill = input$psill, model = input$model, range = input$range)
    linia = variogramLine(modelLine, ceiling(max(wariogram$dist)),n=100)

    ggplot(data = linia, aes(x = dist, y = gamma)) +
      geom_line(size=1.5,colour="cyan") +
      theme_bw() +
      geom_line(data=wariogram)+
      geom_point(data=wariogram,size=2) +
      scale_x_continuous(name = "Distance") +
      scale_y_continuous(name = "Semivariance")
    }
  })
  output$mapa = renderPlot({
    
    inputFile = input$file
    if (is.null(inputFile))
      return(NULL)
    inputFile = read.csv(inputFile$datapath, header = input$header)
    inputPrzerobiony = inputFile
    
    # Zmuszanie Shiny do brania polozenia z kolumn wybranych w selectInput
    kolumnaX = subset(inputFile, select = input$columnX)
    # inputPrzerobiony$shinyX = kolumnaX
    inputPrzerobiony[ , "shinyX"] = kolumnaX
    
    kolumnaY = subset(inputFile, select = input$columnY)
    # inputPrzerobiony$shinyY = kolumnaY
    inputPrzerobiony[ , "shinyY"] = kolumnaY
    
    # Zmuszanie Shiny do brania danych z wybranej kolumny
    kolumnaData = subset(inputFile, select = input$columnData)
    inputPrzerobiony[ , "daneWejsciowe"] = kolumnaData
    
    coordinates(inputPrzerobiony) = ~shinyX+shinyY
    
    modelEPSG = as.character(input$epsg)
    projection = c('+init=epsg:',modelEPSG)
    projection = paste(projection,collapse='')
    proj4string(inputPrzerobiony) = projection
    
    inputNAfree = inputPrzerobiony[!is.na(inputPrzerobiony@data$daneWejsciowe),]
    
    wariogram = variogram(daneWejsciowe~1, inputNAfree, cutoff = input$cutoff, width = input$width, map=TRUE)

      ggplot(data.frame(wariogram),aes(x=map.dx,y=map.dy,fill=map.var1))+
      geom_raster() + 
      scale_fill_gradientn(colours=topo.colors(11), name = "Semivariance", na.value = "white")+
      xlab("x") + ylab("y") + 
      theme_bw() + theme(aspect.ratio=1)
    
  })
  
}

shinyApp(ui = ui, server = server)

