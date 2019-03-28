library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
ui <- fluidPage(
  useShinyjs(),
  title = "FIFA Soccer 2019 App",
  sidebarLayout(
    sidebarPanel(
      radioButtons("selection", label = h3("Select one of the options below:"), 
                   choiceNames = c("Select Player By Name", " Select By Club", "Advanced Search"),
                   choiceValues = c("Name","Club", "Advanced"),
                   inline = FALSE),
      hr(),
      conditionalPanel(
        condition = "input.selection == 'Name'",
        h3("Search by Name"),
        textInput("searchName", label = "Search player by name", placeholder = "Enter player name")
      ), # First conditional for search player by Name
      conditionalPanel(
        condition = "input.selection == 'Club'",
        h3("Search by Club"),
        textInput("searchClub", label = "Search player by club", placeholder = "Enter club")

      ), # Second conditional for search player by Club
      conditionalPanel(
        condition = "input.selection == 'Advanced'",
        h3("Advanced Search"),
        selectInput("positions",label = "Available Positions", choices = NULL, selectize = FALSE),
        sliderInput("Ages", label = "Player Ages", min=15, max=55, value = c(15,50))
      ) # Third conditional for search player Advanced Search
    ), # End sidebarPanel
    mainPanel(
      tags$img(src = "soccerball.jpeg",height = "100", width = "100"),
      dataTableOutput("nameTbl"),
      dataTableOutput("clubTbl"),
      dataTableOutput("otherTbl")
    )
  ) # End sidebarLayout
)

server <- function (input, output, session){
  fifaData <- read.csv("data.csv", stringsAsFactors = FALSE)
  fifaData <- na.omit(fifaData)
  fifaData$Image <- paste("<img src='",fifaData$Photo,"'></img>", sep="")
  fifaData$ClubImage <- paste("<img src='", fifaData$Club.Logo,"'></img>", sep ="")
  formatValue <- function (value){
    n = nchar(value)
    measure <- substr(value, n,n)

    amt <- substr(value, 2, n-1)

    amt <- as.numeric(amt)
    if (measure =="K") {
      amt <- amt *1000
    } else {
      amt <- amt*1000000
    } 
    return (amt)
  }
  fifaData$Value <- lapply(fifaData$Value, formatValue)
  positions <- unique(sort(fifaData$Position))
  ageRange <- range(fifaData$Age)
  
  # print (str(fifaData))
  observe({
    if (input$selection =="Name") {
      hide("clubTbl")
      hide("otherTbl")
      show("nameTbl")
    }
    if (input$selection == "Club"){
      show("clubTbl")
      hide("otherTbl")
      hide("nameTbl")
    }
    if (input$selection == "Advanced"){
      updateSelectInput(session,"positions",choices = positions)
      updateSliderInput(session, "Ages", min= ageRange[1], max= ageRange[2])
      hide("clubTbl")
      show("otherTbl")
      hide("nameTbl")

    }
  })
  output$nameTbl <- renderDataTable({
    db <- data.frame()
    if (input$searchName !=""){
      index = grepl(input$searchName, fifaData$Name, ignore.case = TRUE)
      db <- fifaData[index,]%>% select(Name, Image, Age, Nationality, Club, ClubImage,Position, Value)

    } else {
      db <- fifaData[1:10,] %>% select(Name, Image, Age, Nationality, Club, ClubImage,Position, Value)
    }
    return (datatable(db,options = list(pageLength = 25), escape= FALSE) %>% formatCurrency(8, currency = "€"))
  })
  output$clubTbl <- renderDataTable({
    db <- data.frame()
    if (input$searchClub !=""){
      index = grepl(input$searchClub, fifaData$Club, ignore.case = TRUE)
      db <- fifaData[index,]%>% select(Name, Image, Age, Nationality, Club, ClubImage,Position, Value)
    } else {
      db <- fifaData[1:10,] %>% select(Name, Image, Age, Nationality, Club, ClubImage,Position, Value)
    }
    return (datatable(db, options = list(pageLength = 25),escape= FALSE) %>% formatCurrency(8, currency = "€"))
  })
  output$otherTbl <- renderDataTable({
    db <- data.frame()
    db <- fifaData %>% filter(Position == input$positions & between (Age, input$Ages[1], input$Ages[2])) %>% 
          select(Name, Image, Age, Nationality, Club, ClubImage,Position, Value)
    return (datatable(db, options = list(pageLength = 25),escape= FALSE) %>% formatCurrency(8, currency = "€"))
  })
}
shinyApp(ui, server)