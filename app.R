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
library(sortable)
library(readxl)
library(writexl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Picker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("sheet",label="Google sheet URL",
                          value = "https://docs.google.com/spreadsheets/d/1VH8lKK0qTHybxd5ytf7aeVcl7DklcWvNmwigTEOMvN0/edit?usp=sharing",
                          placeholder = "Sheet must be in the Glasgow Ultimate google drive with viewer access"),
            fileInput("upload","Upload Excel file",accept = c(".xlsx")),
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
            actionButton("make",label = "Make Teams", icon = icon(name = "play", lib = "font-awesome")),
            actionButton("shuffle",label = "Shuffle Teams", icon = icon(name = "shuffle", lib = "font-awesome")),
            textAreaInput("columns",label = "Column mappings"),
            uiOutput("experience"),
            uiOutput("availability"),
            textInput("saveFile","File name",value = "Teams"),
            downloadButton("save", "Download Teams")
            #img(src='GlasgowUltimateLogo2013.jpg', align = "center")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Raw", DT::dataTableOutput("raw")),
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
  mcol<-"#102E4A"
  fcol<-"#17183B"
  
  gs4_auth(cache = ".secrets", email = "glasgowultimate@googlemail.com")
  
  exp<-factor(c("Beginner","Getting there","Intermediate","Veteran","Elite"),levels=c(c("Beginner","Getting there","Intermediate","Veteran","Elite")))
  exp_levels <- reactiveVal(exp)
  column_vals <- c("Name","Gender","Experience","Skill","Fitness")

  columns<-reactive({
    c(column_vals,input$avail)
  })
    
  output$experience<-renderUI({
    tagList(
      rank_list(text = "Experience order",labels = exp_levels(),input_id = "experience")
    )
  })
  
  output$availability<-renderUI({
    tagList(
      selectInput(inputId = "avail",label="Select date columns:",multiple = T,selectize = T,choices = names(fetchSheet()))
         )
  })
  
  ## Set factor levels
  fetchSheet<-reactive({
    if(!is.null(input$upload)){
      tryCatch(
        {
          print(input$upload$datapath)
          s<-read_xlsx(input$upload$datapath) %>% 
            #select(Name,Gender,Experience,Skill,Fitness) %>% 
            mutate(across(c(Gender,Experience),as.factor),
                   Gender = fct_recode(Gender,"M"="Male","F"="Female") %>% fct_relevel("M","F"))
          
          exp_levels(s %>% pull(Experience) %>% 
                       unique() %>% as_factor() %>% 
                       fct_relevel(levels(exp)) %>% 
                       levels())
          return(s)
        },
        error = function(e){
          message("Cannot upload file")
          showModal(modalDialog(
            title = "Error",
            "Cannot upload file"
          ))
          return(NULL)
        }
      )
    }
    else{
      tryCatch({
        s<-read_sheet(input$sheet) %>% 
        #select(Name,Gender,Experience,Skill,Fitness) %>% 
        mutate(across(c(Gender,Experience),as.factor),
                        Gender = fct_recode(Gender,"M"="Male","F"="Female") %>% fct_relevel("M","F"))
        
        exp_levels(s %>% pull(Experience) %>% 
                     unique() %>% as_factor() %>% 
                     fct_relevel(levels(exp)) %>% 
                     levels())
        
        return(s)
        },
        error = function(e){
          message("Sheet Not Found")
          showModal(modalDialog(
            title = "Error",
            "Sheet not found"
          ))
          return(NULL)
        }
      )
    }
  })
  
  teams<-reactiveVal(NULL)
  teamNum<-reactiveVal(NULL)
  
  observeEvent(input$make,{
    
    if(!is.null(fetchSheet())){
      morder=c(1:input$teams,input$teams:1)
      forder=c(input$teams:1,1:input$teams)
      
      if(input$snake==F){
        morder=c(1:input$teams)
        forder=c(input$teams:1)
      }
      
      sheet<-fetchSheet() %>% 
        mutate(Experience = fct_relevel(Experience, input$experience),
               Availability=1) %>% 
        select(all_of(columns()),Availability)
      
      if(!is.null(input$avail)){
        sheet<-sheet %>% mutate(across(all_of(input$avail),~case_when(.=="Y"~1,.=="Yes"~1,.=="N"~0,.=="No"~0,.=="M"~0.5,.=="Maybe"~0.5,.default = 0))) %>% 
          mutate(Availability=rowSums(pick(all_of(input$avail))))
      }
      
      mt <- sheet %>% filter(Gender=="M") %>%  
        mutate(Skill=Skill*input$skillWeight,
               Fitness=Fitness*input$fitnessWeight,
               Score=rowMeans(pick(c(Skill,Fitness)))) %>%
        arrange(desc(Experience),desc(Availability),desc(Score)) %>% 
        mutate(Team=rep(morder,length.out = n()))
      
      ft <- sheet %>% filter(Gender=="F") %>%  
        mutate(Score=rowMeans(pick(c(Skill,Fitness)))) %>%
        arrange(desc(Experience),desc(Availability),desc(Score)) %>% 
        mutate(Team=rep(forder,length.out = n()))
      
      teamdf<-bind_rows(mt,ft)
      
      teams(teamdf)
      teamNum(input$teams)
    }
  })
  
  observeEvent(input$shuffle,{
    
    if(!is.null(fetchSheet())){
      morder=c(1:input$teams,input$teams:1)
      forder=c(input$teams:1,1:input$teams)
      
      if(input$snake==F){
        morder=c(1:input$teams)
        forder=c(input$teams:1)
      }
      
      sheet<-fetchSheet() %>% 
        mutate(Experience = fct_relevel(Experience, input$experience),
               Availability=1) %>% 
        select(all_of(columns()),Availability)
      
      if(!is.null(input$avail)){
        sheet<-sheet %>% mutate(across(all_of(input$avail),case_when(.=="Y"~1,.=="Yes"~1,.=="N"~0,.=="No"~0,.=="M"~0.5,.=="Maybe"~0.5,.default = 0))) %>%  
          mutate(Availability=rowSums(pick(all_of(input$avail))))
      }
      
      mt <- sheet %>% filter(Gender=="M") %>%  
        mutate(Skill=Skill*input$skillWeight,
               Fitness=Fitness*input$fitnessWeight,
               Score=rowMeans(pick(c(Skill,Fitness)))) %>%
        arrange(desc(Experience),desc(Availability),desc(Score)) 
    
      idx<-seq(1,nrow(mt),input$teams) %>% map(function(x){sample(x:min(((x+input$teams)-1),nrow(mt)),min(input$teams,(nrow(mt)-x)+1))}) %>% unlist()
      mt<-mt[idx,]
      
      mt<- mt %>% mutate(Team=rep(morder,length.out = n()))
      
      ft <- sheet %>% filter(Gender=="F") %>%  
        mutate(Score=rowMeans(pick(c(Skill,Fitness)))) %>%
        arrange(desc(Experience),desc(Availability),desc(Score))
      
      idx<-seq(1,nrow(ft),input$teams) %>% map(function(x){sample(x:min(((x+input$teams)-1),nrow(ft)),min(input$teams,(nrow(ft)-x)+1))}) %>% unlist()
      ft<-ft[idx,]
      
      ft<- ft %>% mutate(Team=rep(forder,length.out = n()))
      
      teamdf<-bind_rows(mt,ft)
    
      teams(teamdf)
      teamNum(input$teams)
    }  
  })
  
  output$raw<-DT::renderDataTable({
    if(!is.null(fetchSheet())){
      df<-fetchSheet()
      
      DT::datatable(df,options = list(pageLength = 50, info = FALSE,
                                      lengthMenu = list(c(50,100,150,-1), c("50","100","150","All"))))
    }
  })
  
  output$ts<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-teams()
      
      DT::datatable(df,options = list(pageLength = 50, info = FALSE,
                                     lengthMenu = list(c(50, -1), c("50", "All")))) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()])) %>% 
        formatStyle("Name", "Gender",color=styleEqual(c("F","M"),c(fcol,mcol)))
      
    }
  })
  
  output$teamsTable<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-teams() %>% 
        arrange(Team,Gender,Experience,Score)
      
      DT::datatable(df,
                    options = list(pageLength = -1, info = FALSE,
                                   lengthMenu = list(c(-1,50), c("All","50")))) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()])) %>% 
        formatStyle("Name", "Gender",color=styleEqual(c("F","M"),c(fcol,mcol)))
      
    }
  })
  
  shortTeams<-reactive({
    if(!is.null(teams())){
      df<-teams() %>% 
        select(Name,Team) %>% 
        group_by(Team) %>% 
        mutate(Player=1:n()) %>% 
        pivot_wider(id_cols = Player,names_from = Team,values_from = Name)
      df
    }
    else{
      NULL
    }
  })
  
  output$shortTeamsTable<-DT::renderDataTable({
    if(!is.null(teams())){
      df<-shortTeams()
      
      dt<-DT::datatable(df,
                    options = list(pageLength = -1, info = FALSE,
                                   lengthMenu = list(c(-1,50), c("All","50")))) %>% 
        formatStyle(names(df)[-1], color=styleEqual(teams() %>% filter(Gender=="F") %>% pull(Name),fcol)) %>% 
        formatStyle(names(df)[-1], color=styleEqual(teams() %>% filter(Gender=="M") %>% pull(Name),mcol))
      
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
        pivot_wider(Team,names_from = Experience,values_from = n) %>% 
        select(Team,all_of(exp_levels()))
        
      df2<-df %>% 
        summarise(Male=sum(Gender=="M"),
                  Female=sum(Gender=="F"),
                  Skill=mean(Skill),
                  Fitness=mean(Fitness),
                  Score=mean(Score)) %>% 
        left_join(df1,by = "Team")
        
      DT::datatable(df2,options = list(pageLength = -1, info = FALSE,
                                       lengthMenu = list(c(-1,50), c("All","50")))) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()]))
    }
  })
  
  # Download teams
  output$save <- downloadHandler(
    filename = function() {
      paste0(input$saveFile,".xlsx")
    },
    content = function(file) {
      write_xlsx(shortTeams(),file,col_names = T,format_headers = T)
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
