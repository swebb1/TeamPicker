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
library(purrr)

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
                         value=1),
            checkboxInput("snake","Snake draw",value = T),
            actionButton("make",label = "Make Teams"),
            actionButton("shuffle",label = "Shuffle Teams")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Player ranking", DT::dataTableOutput("ts")),
                      tabPanel("Team summary", DT::dataTableOutput("shortTeamsTable"),DT::dataTableOutput("teamsSummary")),
                      tabPanel("Full team info",DT::dataTableOutput("teamsTable")) 
          )
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
  
  teams<-reactiveVal(NULL)
  teamNum<-reactiveVal(NULL)
  
  observeEvent(input$make,{
    
    morder=c(1:input$teams,input$teams:1)
    forder=c(input$teams:1,1:input$teams)
    
    if(input$snake==F){
      morder=c(1:input$teams)
      forder=c(input$teams:1)
    }
    
    mt <- t %>% filter(Gender=="M") %>%  
      mutate(Skill=Skill*input$skillWeight,
             Fitness=Fitness*input$fitnessWeight,
             Score=rowMeans(pick(c(Skill,Fitness)))) %>%
      arrange(desc(Experience),desc(Score)) %>% 
      mutate(Team=rep(morder,length.out = n()))
    
    ft <- t %>% filter(Gender=="F") %>%  
      mutate(Score=rowMeans(pick(c(Skill,Fitness)))) %>%
      arrange(desc(Experience),desc(Score)) %>% 
      mutate(Team=rep(forder,length.out = n()))
    
    teamdf<-bind_rows(mt,ft)
    
    teams(teamdf)
    teamNum(input$teams)
    
  })
  
  observeEvent(input$shuffle,{
    
    morder=c(1:input$teams,input$teams:1)
    forder=c(input$teams:1,1:input$teams)
    
    if(input$snake==F){
      morder=c(1:input$teams)
      forder=c(input$teams:1)
    }
    
    mt <- t %>% filter(Gender=="M") %>%  
      mutate(Skill=Skill*input$skillWeight,
             Fitness=Fitness*input$fitnessWeight,
             Score=rowMeans(pick(c(Skill,Fitness)))) %>%
      arrange(desc(Experience),desc(Score)) 
  
    idx<-seq(1,nrow(mt),input$teams) %>% map(function(x){sample(x:min(((x+input$teams)-1),nrow(mt)),min(input$teams,(nrow(mt)-x)+1))}) %>% unlist()
    mt<-mt[idx,]
    
    mt<- mt %>% mutate(Team=rep(morder,length.out = n()))
    
    ft <- t %>% filter(Gender=="F") %>%  
      mutate(Score=rowMeans(pick(c(Skill,Fitness)))) %>%
      arrange(desc(Experience),desc(Score))
    
    idx<-seq(1,nrow(ft),input$teams) %>% map(function(x){sample(x:min(((x+input$teams)-1),nrow(ft)),min(input$teams,(nrow(ft)-x)+1))}) %>% unlist()
    ft<-ft[idx,]
    
    ft<- ft %>% mutate(Team=rep(forder,length.out = n()))
    
    teamdf<-bind_rows(mt,ft)
  
    teams(teamdf)
    teamNum(input$teams)
    
  })
  
  output$ts<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-teams()
      
      DT::datatable(df,options = list(pageLength = 50, info = FALSE,
                                     lengthMenu = list(c(50, -1), c("50", "All")))) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()]))
    }
  })
  
  output$teamsTable<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-teams() %>% 
        arrange(Team,Gender,Experience,Score)
      
      DT::datatable(df,
                    options = list(pageLength = -1, info = FALSE)) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()]))
    }
  })
  
  output$shortTeamsTable<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-teams() %>% 
        select(Name,Team) %>% 
        group_by(Team) %>% 
        mutate(Player=1:n()) %>% 
        pivot_wider(id_cols = Player,names_from = Team,values_from = Name)
      
      dt<-DT::datatable(df,
                    options = list(pageLength = -1, info = FALSE)) #%>% 
        #formatStyle(names(df)[-1] %>% as.list(), backgroundColor=teamCols[1:teamNum()] %>% as.list())
      
      1:teamNum() %>% map(function(x){
        dt <<- dt %>% formatStyle(as.character(x),backgroundColor = teamCols[x])
      })
      
      dt
    }         
  })
  
  output$teamsSummary<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-teams() %>% group_by(Team)
      
      df1<-df %>% count(Experience) %>% 
        pivot_wider(Team,names_from = Experience,values_from = n)
        
      df2<-df %>% 
        summarise(Male=sum(Gender=="M"),
                  Female=sum(Gender=="F"),
                  Skill=mean(Skill),
                  Fitness=mean(Fitness),
                  Score=mean(Score)) %>% 
        left_join(df1,by = "Team")
        
      DT::datatable(df2,options = list(pageLength = -1, info = FALSE)) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()]))
    }
})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
