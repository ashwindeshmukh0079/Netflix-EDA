library(shiny) # for shiny webapp
library(ggplot2) # for graphs
library(dplyr) # for data manipulation
library(highcharter)  # for time series

#Read Input Data (row)
dataM <- read.csv('netflix_titles.csv',TRUE,',');

# Refining data to remove null values
rowToFilter <- c(grepl(",",dataM$country))
filteredDf <- dataM[!rowToFilter,]
filteredDf <- filter(filteredDf,country != "")
filteredDf <- arrange(filteredDf,country)

#List of distinct countries for Bar Graph and Donut Chart
options <- unique(filteredDf$country)

filteredDf <- arrange(filteredDf,release_year)

#Selecting range of Year of release for Line chart
fromYr <- unique(filteredDf$release_year)
toYr <- unique(filteredDf$release_year)

ui <- fluidPage(
  
  titlePanel("Netflix Data Visualization"),
  tabsetPanel(type = "tabs",
              
              #Tab 1: Bar graph to plot type of shows(Movies/TV Shows) as function of country 
              tabPanel("Bar Graph",
                       titlePanel("Bar graph to plot Movies/TV Shows based on selected country"),
                       sidebarPanel(
                         selectInput(inputId = "checkboxVar",label = "Select multiple Countries",
                                     choices = options,
                                     selected = options[c(2,11,26)], 
                                     multiple = TRUE)
                       ),
                       mainPanel(
                         plotOutput("barGraph"),
                       )),
              
              #Tab 2: Donut Chart to plot  popular  genre in each country
              tabPanel("Donut Chart",
                       titlePanel("Donut Chart to plot type of geners popular in each country"),
                       sidebarPanel(
                         selectInput(inputId = "selectboxVar", label = "Select one country", 
                                     choices = options, multiple = FALSE, selected = options[1]),
                       ),
                       mainPanel(plotOutput("donutChart"))
              ),
              
              #Tab 3: Time series to plot rate at which netflix produces its TV shows/Movies.
              tabPanel("Time Series Graph",
                       titlePanel("Time series to plot rate at which netflix produces its TV shows/Movies"),
                       sidebarPanel(
                         selectInput(inputId = "fromYearHc", label = "Select lower Limit", 
                                     choices = fromYr, multiple = FALSE, selected = 1942),
                         selectInput(inputId = "toYearHc", label = "Select upper Limit", 
                                     choices = toYr, multiple = FALSE, selected = 2020)
                       ),
                       mainPanel(highchartOutput("lineChartHc"))
              )
  )
)

server <- function(input, output, session) {
  
  #Read Input data(raw) for server rendering and graph plots
  inputDataFrame <- reactiveFileReader(1000,NULL,'netflix_titles.csv',read.csv)
  
  # Refining data to remove null values
  filteredDataFrame <- reactive({
    row_to_discard <- c(grepl(",",inputDataFrame()$country))
    filteredFrame <- inputDataFrame()[!row_to_discard,];
    filter(filteredFrame,country != "")
  })
  
  #Get subset of input dataframe for donut chart
  donutFilter <- reactive({
    demoDf <- filteredDataFrame()
    demoDf$listed_in <- sapply(1:nrow(demoDf),function(i) unlist(strsplit(demoDf$listed_in[i], ",", fixed = TRUE))[1])  
    filter(demoDf,country == input$selectboxVar);
  })
  
  #Aggregate the subset of input datafram to plot donut chart
  donutPercentage <- reactive({
    donutFilter() %>%
      group_by(listed_in) %>%
      summarize(counts = n(),
                percentage = n()/nrow(donutFilter()))
  })
  
  #Get subset of input dataframe for line graph
  lineDataFrame <- reactive({
    filteredDataFrame() %>%
      group_by(release_year,type) %>% summarise(counts = n())
  })
  
  #Output: Plot bar graph based on selected country values from input
  output$barGraph <- renderPlot({
    plotFrame <- filter(filteredDataFrame(),country == input$checkboxVar);
    ggplot(plotFrame,aes(country, fill = type)) + geom_bar(position = "dodge")
  })
  
  #Output: Plot donut chart based on selected country value from input
  output$donutChart <- renderPlot({
    ggplot(data = donutPercentage(), 
           aes(x=2, y = percentage, fill = listed_in))+
      geom_col(color = "black") +
      coord_polar("y", start = 0) + 
      geom_text(aes(label = paste0(round(percentage*100), "%")), 
                position = position_stack(vjust = 0.5)) +
      theme(panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(), 
            plot.title = element_text(hjust = 0.5, size = 18)) +
      xlim(0.5,2.5)
  })
  
  #Output: Plot time series graph chart based on selected input range
  output$lineChartHc <- renderHighchart({
    filteredReleaseYearMovie <- filter(lineDataFrame(),release_year >= input$fromYearHc & release_year <= input$toYearHc & type == "Movie")
    filteredReleaseYearTV <- filter(lineDataFrame(),release_year >= input$fromYearHc & release_year <= input$toYearHc & type == "TV Show")
    highchart() %>%
      hc_add_series(name = "Movie",filteredReleaseYearMovie,hcaes(x = release_year, y = counts, color= counts), type ="line") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.0, 
                 pointFormat = paste("Year: <b>{point.x}</b> <br>  Count: <b>{point.y}<b>")) %>%
      hc_add_series(name="TV Show",filteredReleaseYearTV,hcaes(x = release_year, y = counts, color= counts), type ="line") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.0, 
                 pointFormat = paste("Year: <b>{point.x}</b> <br> Count: <b>{point.y}<b>"))
    
  })
  
  
}

shinyApp(ui, server)
