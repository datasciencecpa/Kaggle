library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
# library(tidyverse)
ui <- fluidPage(
  useShinyjs(),
  title = "FIFA Soccer 2019 App",
  sidebarLayout(
    sidebarPanel(
      radioButtons("selection", label = h3("Select one of the options below:"), 
                   choiceNames = c("Select Player By Name", " Select By Club", "Advanced Search", "Fun Facts"),
                   choiceValues = c("Name","Club", "Advanced", "Fun"),
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
        selectInput("positions",label = "Available Positions", choices = NULL, multiple = TRUE),
        sliderInput("Ages", label = "Player Ages", min=15, max=55, value = c(15,50))
      ),# Third conditional for search player Advanced Search
      conditionalPanel(
        condition = "input.selection == 'Fun'",
        radioButtons("funSelection", label = h3("Choose one of the options belows:"),
                     choiceNames = c("View Club Stats", "Top 10 Most Valuable Players", "Best 10 By Positions"), 
                     choiceValues = c("clubStats", "valuable10", "position10"), inline = FALSE)
      ),
      conditionalPanel(
        condition = "input.funSelection == 'position10'",
        selectInput("positions2", label = "Available Positions", choices = NULL),
        sliderInput("Ages2", label = "Player Ages", min=15, max=55, value = c(15,50))
      )
    ), # End sidebarPanel
    mainPanel(
      tags$img(src = "soccerball.jpeg",height = "100", width = "100"),
      dataTableOutput("nameTbl"),
      dataTableOutput("FunTbl")
    )
  ) # End sidebarLayout
)

server <- function (input, output, session){

  fifaData <- read.csv("data.csv", na.strings = c(""), stringsAsFactors = FALSE )
  fifaData$Image <- paste("<img src='",fifaData$Photo,"'></img>", sep="")
  fifaData$ClubImage <- paste("<img src='", fifaData$ClubLogo,"'></img>", sep ="")
  fifaData$FlagImg <- paste("<img src='", fifaData$Flag,"'></img>", sep ="")
  
  formatValue <- function (value){
    n = nchar(value)
    measure <- substr(value, n,n)

    amt <- substr(value, 2, n-1)

    amt <- as.numeric(amt)
    if (n>=3){
      if (measure =="K") {
        amt <- amt *1000
      } else {
        amt <- amt*1000000
      } 
    }
    return (amt)
  }
  fifaData$Value <- as.numeric(lapply(fifaData$Value, formatValue))
  fifaData$Wage <- as.numeric(lapply(fifaData$Wage, formatValue))

  positions <- unique(sort(fifaData$Position))
  ageRange <- range(fifaData$Age)

  observe({
    if (input$selection == "Advanced"){
      updateSelectInput(session,"positions",choices = positions, selected = positions[1])
      updateSliderInput(session, "Ages", min= ageRange[1], max= ageRange[2])
    }
    if (input$selection =="Fun"){
      shinyjs::hide ("nameTbl")
      shinyjs::show ("FunTbl")
    } else if (input$selection != "Fun") {
      updateRadioButtons(session,"funSelection", selected = "clubStats") # so that select input position will be hidden.
      shinyjs::hide ("FunTbl")
      shinyjs::show ( "nameTbl")
    }
    if (input$funSelection == "position10"){
      updateSelectInput(session,"positions2",choices = positions, selected = positions[1])
      updateSliderInput(session, "Ages2", min= ageRange[1], max= ageRange[2])
    }
  })
  dataOutput <- reactive ({
    db <- data.frame()
    if (input$selection =="Name"){
      if (input$searchName !=""){
        index = grepl(input$searchName, fifaData$Name, ignore.case = TRUE)
        db <- fifaData[index,]%>% select(Name, Image, Age, Nationality, FlagImg, Club, ClubImage,Position, Value)
        
      } else {
        db <- fifaData[1:10,] %>% select(Name, Image, Age, Nationality,FlagImg, Club, ClubImage,Position, Value)
      }
    } # End input$selection == Name
    else if (input$selection =="Club"){
      if (input$searchClub !=""){
        index = grepl(input$searchClub, fifaData$Club, ignore.case = TRUE)
        db <- fifaData[index,]%>% select(Name, Image, Age, Nationality,FlagImg, Club, ClubImage,Position, Value)
      } else {
        db <- fifaData[1:10,] %>% select(Name, Image, Age, Nationality,FlagImg, Club, ClubImage,Position, Value)
      }
    } # End input$selection == "Club"
    else {
      db <- fifaData %>% filter(Position == input$positions & between (Age, input$Ages[1], input$Ages[2])) %>% 
        select(Name, Image, Age, Nationality,FlagImg, Club, ClubImage,Position, Value)
    }
    return (datatable(db,options = list(pageLength = 25), escape= FALSE) %>% formatCurrency(9, currency = "€"))
  })
  funDataOutput <- reactive({
    db <- data.frame()
    currencyCol <- c(0)
    if (input$funSelection =="clubStats"){
      # Group by Club
      db <- fifaData %>% group_by(Club) %>% 
          summarize(Average_Age = round(mean(Age)), Youngest_Player_Age = min(Age), Oldest_Player_Age = max(Age),
                    Average_Wages = mean(Wage),Total_Wages_Paid = sum(Wage),Total_Player_Value = sum(Value))
      currencyCol <- c(5:7)
    } else if (input$funSelection == "valuable10") {
      db <- fifaData %>% arrange(desc(Value)) # First sorted the table by descending value
      db <- db[1:10,] #getting only 10 top rows
      db <- db %>% select(Name, Image, Age, Nationality,FlagImg, Club, ClubImage,Position, Value)
      currencyCol <- 9
    } else if (input$funSelection == "position10"){
      db <- fifaData %>%filter(Position == input$positions2 & between (Age, input$Ages2[1], input$Ages2[2]))
      db <- db %>%arrange(desc(Overall))
      db <- db[1:10,]
      db <- db %>% select(Name, Image, Age, Nationality,FlagImg, Club, ClubImage,Position, Overall, Value)
      currencyCol <- 10
    }
    return (datatable(db,options = list(pageLength = 25), escape= FALSE) %>% formatCurrency(currencyCol, currency = "€"))
  })
  output$nameTbl <- renderDataTable({
    dataOutput()
  })
  output$FunTbl <- renderDataTable({
    funDataOutput()
  })
}
shinyApp(ui, server)