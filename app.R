#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(magrittr)
library(forcats)
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
    #titlePanel("Team Picker"),
    titlePanel(title=div(img(src="GlasgowUltimateLogo2013.jpg",height="10%", width="10%"), "TeamPicker",style="position:fixed")),
    br(),
    br(),
    br(),
    br(),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fluidRow(
            actionButton("make",label = "Make Teams", icon = icon(name = "play", lib = "font-awesome",style="color:white"),style="background-color: #5D70AC;color:white"),
            actionButton("shuffle",label = "Shuffle Teams", icon = icon(name = "shuffle", lib = "font-awesome",style="color:white"),style="background-color:#71B48D;color:white"),
            style = "padding:10px"
          ),
          fluidRow(
          tabsetPanel(
            tabPanel("Input", fluid = TRUE,
              br(),
              textAreaInput("sheet",label="Google sheet URL",
                          value = "https://docs.google.com/spreadsheets/d/1VH8lKK0qTHybxd5ytf7aeVcl7DklcWvNmwigTEOMvN0/edit?usp=sharing",
                          placeholder = "Sheet must be in the Glasgow Ultimate google drive with viewer access"),
              fileInput("upload","Upload Excel file",accept = c(".xlsx"))
            ),
            tabPanel("Team Options", fluid = TRUE,
              br(),
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
              checkboxInput("snake","Snake draw",value = T)
            ),
            tabPanel("Customisation", fluid = TRUE,
              br(),
              uiOutput("experience"),
              uiOutput("availability"),
              textInput(inputId = "name_col",value = "Name",label = "Name column"),
              textInput(inputId = "gender_col",value = "Gender",label = "Gender column"),
              textInput(inputId = "experience_col",value = "Experience",label = "Experience column"),
              textInput(inputId = "skill_col",value = "Skill",label = "Skill column"),
              textInput(inputId = "fitness_col",value = "Fitness",label = "Fitness column"),
            ),
            footer = tagList(
              h4("Export"),
              textInput("saveFile","File name",value = "Teams"),
              downloadButton("save", "Download Teams")
            ),
          ),
          style = "padding:10px"
          )
            #img(src='GlasgowUltimateLogo2013.jpg', align = "center")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Raw data", br(),DT::dataTableOutput("raw"),style = "overflow-y:scroll; max-height: 600px"),
                      tabPanel("Player ranking",  br(),DT::dataTableOutput("ts"),style = "overflow-y:scroll; max-height: 600px"),
                      tabPanel("Team summary",  br(),
                               h3("Teams"),
                               helpText("Select cells to lock players in teams while shuffling"),
                               DT::dataTableOutput("shortTeamsTable"),
                               br(),
                               h3("Team Summary"),
                               br(),
                               DT::dataTableOutput("teamsSummary"),
                               br(),
                               h3("Team Availability"),
                               br(),
                               uiOutput("avail_summary"),
                               DT::dataTableOutput("availSummary"),style = "overflow-y:scroll; max-height: 600px"),
                      tabPanel("Full team info", br(),DT::dataTableOutput("teamsTable"),style = "overflow-y:scroll; max-height: 600px"),
                      tabPanel("Help", br(),includeHTML("Help.html"),style = "overflow-y:scroll; max-height: 600px") 
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  teamCols<-c("white",paletteer::paletteer_d("rcartocolor::Pastel", 12))
  mcol<-"#0F3045"
  fcol<-"#570049"
  
  disable("make")
  disable("shuffle")
  
  gs4_auth(cache = ".secrets", email = "glasgowultimate@googlemail.com")
  
  ## UI values and reactives
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
  outputOptions(output, 'experience', suspendWhenHidden=FALSE)
  
  output$availability<-renderUI({
    tagList(
      selectInput(inputId = "avail",label="Select availability columns:",multiple = T,selectize = T,choices = names(fetchSheet()))
         )
  })
  outputOptions(output, 'availability', suspendWhenHidden=FALSE)
  
  output$avail_summary<-renderUI({
    tagList(
      selectInput(inputId = "avail_sum",label="Summarise by",multiple = T,selectize = T,
                  choices = c("Gender","Experience"),selected = "Team")
    )
  })
  
  ## Reactive values and functions
  teams<-reactiveVal(NULL)
  teamNum<-reactiveVal(NULL)
  lock_idx<-reactiveVal(NULL)
  
  locked<-reactive({
    df=shortTeams() %>% as.data.frame()
    idx=input$shortTeamsTable_cells_selected
    lock=df[idx]
    lock_idx(idx)
    lock
  })
  
  swap_idx<-function(x,idx){
    pos=which(idx==x)
    sw=idx[x]
    idx[x]=idx[pos]
    idx[pos]=sw
    idx
  }
  
  fetchSheet<-reactive({
    if(!is.null(input$upload)){
      tryCatch(
        {
          print(input$upload$datapath)
          s<-read_xlsx(input$upload$datapath) %>% 
            select(Name=input$name_col,Gender=input$gender_col,Experience=Experience$experience_col,Skill=input$skill_col,Fitness=input$fitness_col) %>% 
            mutate(across(c(Gender,Experience),as.factor),
                   Gender = fct_recode(Gender,"M"="Male","F"="Female") %>% fct_relevel("M","F"))
          
          exp_levels(s %>% pull(Experience) %>% 
                       unique() %>% as_factor() %>% 
                       fct_relevel(levels(exp)) %>% 
                       levels())
          
          enable("make")
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
          select(Name=input$name_col,Gender=input$gender_col,Experience=Experience$experience_col,Skill=input$skill_col,Fitness=input$fitness_col) %>% 
          mutate(across(c(Gender,Experience),as.factor),
                        Gender = fct_recode(Gender,"M"="Male","F"="Female") %>% fct_relevel("M","F"))
        
        exp_levels(s %>% pull(Experience) %>% 
                     unique() %>% as_factor() %>% 
                     fct_relevel(levels(exp)) %>% 
                     levels())
        
        enable("make")
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
      lock_idx(NULL)
      enable("shuffle")
    }
  })
  
  observeEvent(input$shuffle,{
    
    ## Work out team ordering, do females in reverse so last team gets first pick
    if(!is.null(teams()) & !is.null(teamNum())){
      morder=c(1:teamNum(),teamNum():1)
      forder=c(teamNum():1,1:teamNum())
      
      ## If not snakedraw then just cycle through picks 
      if(input$snake==F){
        morder=c(1:teamNum())
        forder=c(teamNum():1)
      }
      
      ## Get current teams
      sheet<-teams()
      
      ## Look for locks
      msheet <- sheet %>% filter(Gender == "M")
      fsheet <- sheet %>% filter(Gender == "F")
      locked_idx_m <- which(msheet$Name %in% locked())
      locked_idx_f <- which(fsheet$Name %in% locked())
      
      ## Shuffle Males within player number
      mt <- sheet %>% filter(Gender=="M") 
      idx<-seq(1,nrow(mt),teamNum()) %>% map(function(x){sample(x:min(((x+teamNum())-1),nrow(mt)),min(teamNum(),(nrow(mt)-x)+1))}) %>% unlist()
      
      ## Swap back the locked positions
      newidx=idx
      for(i in locked_idx_m){
        newidx=swap_idx(i,newidx)
      }
      idx=newidx
      
      ## Pick new teams
      mt<-mt[idx,]
      mt<- mt %>% mutate(Team=rep(morder,length.out = n()))
      
      ## Repeat for female side
      ft <- sheet %>% filter(Gender=="F")
      idx<-seq(1,nrow(ft),teamNum()) %>% map(function(x){sample(x:min(((x+teamNum())-1),nrow(ft)),min(teamNum(),(nrow(ft)-x)+1))}) %>% unlist()
      
      newidx=idx
      for(i in locked_idx_f){
        newidx=swap_idx(i,newidx)
      }
      idx=newidx
      
      ft<-ft[idx,]
      ft<- ft %>% mutate(Team=rep(forder,length.out = n()))
      
      ## Binf M & F and update teams
      teamdf<-bind_rows(mt,ft)
      teams(teamdf)
    
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
      
      dt<-DT::datatable(df,selection=list(target = 'cell',selected = lock_idx()),
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
                  Score=mean(Score) %>% sprintf("%.2f",.)) %>% 
        left_join(df1,by = "Team")
        
      DT::datatable(df2,options = list(pageLength = -1, info = FALSE,
                                       lengthMenu = list(c(-1,50), c("All","50")))) %>% 
        formatStyle("Team", target = 'row', 
                    backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()]))
    }
  })
  
  output$availSummary<-DT::renderDataTable({
    if(!is.null(teams()) & !is.null(input$avail)){
      if(input$avail[1] %in% names(teams())){
      
        df<-teams() %>% select(Team,Gender,Experience,all_of(input$avail)) %>%
          summarise(across(all_of(input$avail),sum),.by = all_of(c("Team",input$avail_sum))) %>% #c(Team,Gender,Experience)) %>% 
          arrange(pick(c("Team",input$avail_sum))) #Team,Experience,Gender)
        
        DT::datatable(df,options = list(pageLength = -1, info = FALSE,
                                         lengthMenu = list(c(-1,50), c("All","50")))) %>% 
          formatStyle("Team", target = 'row', 
                      backgroundColor = styleEqual(1:teamNum(),teamCols[1:teamNum()]))
      }
    }
  })
  
  # Download teams
  output$save <- downloadHandler(
    filename = function() {
      paste0(input$saveFile,".xlsx")
    },
    content = function(file) {
      if(!is.null(teams())){
        sheets <- list("Teams" = shortTeams(), 
                       "Full breakdown" = teams() %>% arrange(Team,Gender,Experience,Score))
        write_xlsx(sheets,file,col_names = T,format_headers = T)
      }
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
