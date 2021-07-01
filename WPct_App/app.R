library(shiny)
library(tidyverse)

#Get data from csv files for game level and season level data
name_abb <- read_csv("Team_Abbs_and_Names_1950.csv")
dat <- read_csv("team_season_Wpct.csv")
game_level_data_2 <- read_csv("game_level_data_2.csv")

ui <- fluidPage(
  #make check boxes to choose what teams to display.  Put 1/3 of team list in each of 3 columns
  fluidRow(
    column(4, checkboxGroupInput(inputId = "Team_Name", label = "Team", choices = name_abb$name[1:{round(length(name_abb$name)/3)}])),
    column(4, checkboxGroupInput(inputId = "Team_Name2", label = "Team", choices = name_abb$name[{round(length(name_abb$name)/3)+1}:{2*round(length(name_abb$name)/3)}])),
    column(4, checkboxGroupInput(inputId = "Team_Name3", label = "Team", choices = name_abb$name[{2*round(length(name_abb$name)/3)+1}:{round(length(name_abb$name))}]))
  ),
  
  fluidRow(h4(print("Use the checkboxes above to select which teams you would like to view.  Data will appear in the left most plot below. You can click and box data to explore further. A histogram of game run differential will appear on the right hand plot based on the team seasons boxed on the left hand plot"))),
  
  #Make win% error jitter plot on the left of the row. On the right, histogram of run differential in seasons that are boxed by user on the jitter plot
  fluidRow(
    column(6, plotOutput("Jitter_plot", click = "Pclick", brush = "Bclick")),
    column(6, plotOutput("Hist"))
  ),
  
  #Shows data row for single team season based on click coordinates
  fluidRow(
    verbatimTextOutput("coords")
  ),
  
  #Shows team season level data based on boxed data on jitter plot
  fluidRow(
    verbatimTextOutput("brushed_coords")
  ),
  
  #Shows game level data based on boxed data on jitter plot
  fluidRow(
    verbatimTextOutput("testing")
  )
  
)

server <- function(input, output) {
  
  #make data table based on checkbox inputs and add x jitter column to data frame (used to compare to click on plot)
  data <- reactive({
    filt <- dat[dat$name %in% c(input$Team_Name, input$Team_Name2, input$Team_Name3) , ]
    xvals <- runif(length(filt$name))
    filt %>% mutate(jit = xvals)
  })
  
  #Get game level data based on checkbox inputs
  data2 <- reactive({
    #associate full team names from checkboxes to their abbreviations in the other data tables
    filt2 <- name_abb$teamID[name_abb$name %in% c(input$Team_Name, input$Team_Name2, input$Team_Name3)]
    #filter the game level data based on the checkbox abbreviations
    filt3 <- game_level_data_2[game_level_data_2$Team %in% filt2,]
    #filter the game level data based on the years and the teams in the data that is boxed by the user
    filt4 <- filt3[format(as.Date(filt3$Date), format = "%Y") %in% brushedPoints(data(), input$Bclick)$yearID & filt3$Team %in% brushedPoints(data(), input$Bclick)$teamID,]
    filt4
  })
  
  #Make win% error jitter plot based on checkboxes
  output$Jitter_plot <- renderPlot({
    
    {data() %>% ggplot(aes(jit,PctError)) + 
        #geom_rect(fill = "red", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, alpha = 0.01) + geom_rect(fill = "green", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, alpha = 0.01) + 
        geom_boxplot() + geom_point(aes(color = name)) + 
        scale_x_continuous(name = "", limits = c(-.5,1.5)) + scale_y_continuous(name = "Actual Win Percentage - Expected Win Percentage")}
  })
  
  #Choose row from season level data table based on user click on plot
  output$coords <- renderPrint({
    
    nearPoints(data() %>% select(name, lgID, yearID, PctError, jit), input$Pclick, xvar = "jit", yvar = "PctError", threshold = 10, maxpoints = 1)
  })
  
  #Show rows from season level data table based on boxed data
  output$brushed_coords <- renderPrint({
    brushedPoints(data(), input$Bclick)
    
  })
  
  #Show rows from game level data table based on boxed data
  output$testing <- renderPrint({
    data2()
  })
  
  #histogram of run differential in seasons that are boxed by user on the jitter plot
  output$Hist <- renderPlot({
    data2() %>% ggplot(aes(RDiff)) + geom_histogram(aes(fill = Team), binwidth = 1) + scale_x_continuous(limits = c(-22,22))
  })
}

shinyApp(ui = ui, server = server)