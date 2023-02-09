#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(magrittr)
library(forcats)
library(PrettyCols)
library(DT)
library(googlesheets4)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Picker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("teams",
                        "Number of teams:",
                        min = 2,
                        max = 50,
                        value = 4),
            numericInput("skillWeight",
                         "Skill weight:",
                         min=0,
                         max=4,
                         value=1),
            numericInput("fitnessWeight",
                         "Fitness weight:",
                         min=0,
                         max=4,
                         value=1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput("ts"), 
           DT::dataTableOutput("teamsTable"),
           DT::dataTableOutput("teamsSummary")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  teamCols<-c("white","darkgrey",prettycols(name = "Summer")[c(1,2,4,10,11)],prettycols(name = "Bold")[c(1,3,4,5)])
  
  t <- read_sheet("https://docs.google.com/spreadsheets/d/1VH8lKK0qTHybxd5ytf7aeVcl7DklcWvNmwigTEOMvN0/edit?usp=sharing")  
  #t <- data.frame(Name=c("Bob","Ben","Josie","Wendy"),Gender=c("M","M","F","F"),Skill=c(1,5,3,4),Fitness=c(4,4,2,5),Experience=c("Beginner","Intermediate","Experienced","Intermediate"))  
  #t<-bind_rows(t,t,t,t)
  
  exp_levels =c("Beginner","Getting there","Intermediate","Veteran","Elite")
  
  ## Set factor levels
  t<-t %>% select(Name,Gender,Experience,Skill,Fitness) %>% 
    mutate(across(c(Gender,Experience),as.factor),
                    Gender = fct_recode(Gender,"M"="Male","F"="Female") %>% fct_relevel("M","F"),
                    Experience = fct_relevel(Experience, exp_levels))
  
  makeTeams<-reactive({
    
    mt <- t %>% filter(Gender=="M") %>%  
      mutate(Skill=Skill*input$skillWeight,
             Fitness=Fitness*input$fitnessWeight,
             Score=rowMeans(pick(c(Skill,Fitness)))) %>%
      arrange(desc(Experience),desc(Score)) %>% 
      mutate(Team=rep(c(1:input$teams,input$teams:1),length.out = n()))
      
    ft <- t %>% filter(Gender=="F") %>%  
      mutate(Score=rowMeans(pick(c(Skill,Fitness)))) %>%
      arrange(desc(Experience),desc(Score)) %>% 
      mutate(Team=rep(c(input$teams:1,1:input$teams),length.out = n()))
    
    bind_rows(mt,ft)
    
  })
  
  #shuffleTeams<-reactive({
  #  df<-makeTeams()
  #  idx=seq(1,nrow(df),by=4) %>% purrr::map(function(x){sample(x:(x+3),4)}) %>% unlist()
  #  idx
  #})
  
  output$ts<-DT::renderDataTable({
    df<-makeTeams()
    DT::datatable(df) %>% 
      formatStyle("Team", target = 'row', 
                  backgroundColor = styleEqual(1:input$teams,teamCols[1:input$teams]))
  })
  
  output$teamsTable<-DT::renderDataTable({
    df<-makeTeams() %>% 
      arrange(Team,Gender,Experience,Score)
    DT::datatable(df) %>% 
      formatStyle("Team", target = 'row', 
                  backgroundColor = styleEqual(1:input$teams,teamCols[1:input$teams]))
  })
  
  output$teamsSummary<-DT::renderDataTable({
    df<-makeTeams() %>% 
      group_by(Team)
    
    df1<-df %>% count(Experience) %>% 
      pivot_wider(Team,names_from = Experience,values_from = n)
      
    df2<-df %>% 
      summarise(Male=sum(Gender=="M"),
                Female=sum(Gender=="F"),
                Skill=mean(Skill),
                Fitness=mean(Fitness),
                Score=mean(Score)) %>% 
      left_join(df1,by = "Team")
      
    DT::datatable(df2) %>% 
      formatStyle("Team", target = 'row', 
                  backgroundColor = styleEqual(1:input$teams,teamCols[1:input$teams]))
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
