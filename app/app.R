#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rio)
library(mongolite)
library(waiter)
library(DT)
library(markdown)
library(shinyjs)
library(stringr)

textAreaInput2 <- function (inputId, label, value = "", width = NULL, height = NULL, 
                            cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", 
                                  "horizontal"))
  }
  style <- paste("max-width: 100%;", if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), if (!is.null(height)) 
      paste0("height: ", validateCssUnit(height), ";"), if (!is.null(resize)) 
        paste0("resize: ", resize, ";"))
  if (length(style) == 0) 
    style <- NULL
  div(class = "form-group", 
      tags$label(label, `for` = inputId), tags$textarea(id = inputId, 
                                                        class = "form-control", placeholder = placeholder, style = style, 
                                                        rows = rows, cols = cols, value))
}

# options(mongodb = list(
#     "host" = "-ndgul.mongodb.net/test?retryWrites=true&w=majority",
#     "username" = "mcguinlu:",
#     "password" = readLines("password.txt")
# ))

databaseName <- "LSR"
collectionName <- "test"

#print(readLines("password.txt"))
mongo_url <- paste0("mongodb+srv://LS:",
             readLines("password.txt"),
             "@lsr.7ybnv.mongodb.net/LSR?retryWrites=true&w=majority")
db <- mongo(collection = collectionName,
            url = mongo_url)


# Define UI for application that draws a histogram
ui <- tagList(
    navbarPage(
        title = "LSR of data extraction for SR automation",
        id = "mytabsetpanel",
        theme = shinythemes::shinytheme("cerulean"),
        
    tabPanel(title = "All records",
             fluidRow(column(width = 8, h3(textOutput("total_no")),p("Click on the button on the right to download a snapshot of the database. To make decisions at the title/abstract stage, click \"Inital decision\". Records marked for inclusion at the inital stage will appear in the \"Expert decision\" tab for second review and data extraction.")),
                      column(width= 4, align = "right",br(),  downloadButton("downloadallscreened", "Download snapshot of database") )),
             hr(),
             DT::dataTableOutput("all_records"),
             
             waiter::waiter_show_on_load()),
  
    tabPanel(title = "Initial assessment",
             h3(strong("Initial decisions")),
             p("Please use the \"Include\"/\"Exclude\" buttons to make an inital decision on each record.",
             "Records marked as \"Include\" will be passed to the \"Expert decision\" tab for further screening and data extraction.",
             "Once you make a decision, the program will automatically move to the next abstract - if you make a mistake, unclick the \"Show only IDs needing a decision\" checkbox and navigate to the record you wish to correct using the drop-down box.",
             "Clicking on the link in the \"Link\" column will open up the record in a new tab via it's DOI, or if no DOI was available, will perform a Google search of the record's title."),
             h4(strong(textOutput("number_initial_undecide"))),
             uiOutput("initalID"),
             checkboxInput("showall", "Show only IDs needing an initial assessment", value = TRUE),
             actionButton("initalinclude", "Send for further assessment"),
             actionButton("initalexclude", "Discard"),
             br(),
             tableOutput("test"),
 
             ),
    
    tabPanel(title = "Expert assessment",
             value = "expert_decision_pane",
             h3(strong("Expert assessment")),
             h4(strong(textOutput("number_expert_undecide"))),
             uiOutput("expertID"),
             checkboxInput("showallexpert", "Show only records that have not been marked as \"Complete\"", value = FALSE),
             fluidRow(column(width = 6,
             actionButton("expertinclude", "Include"),
             actionButton("expertexclude", "Exclude"),
             selectInput("exclusion_reason", label = "Exclusion reason (the Exclude button won't work until this is completed.)",
                          choices = c("","No original data extraction approach","Not fitting target entities or data","Not globally applicable","Extraction from graphs or images","Duplicate","Other"))),
             column(width = 6, align = "right",downloadButton("report", "Generate report for this record")
)),
             tableOutput("expert_table"),
             hidden(div(id = "form",
             fluidRow(
               column(
                 width = 3,
                 uiOutput("basic_header"),
                 lapply(c(13,12,0:11), function(i)#######LAYOUT
                   uiOutput(paste0("q", i)))
               ),
               column(width = 8,
                      fluidRow(uiOutput("adv_header")),
                      fluidRow(
                        column(width = 6,
                               lapply(c(14:30), function(i)
                                 uiOutput(paste0(
                                   "q", i
                                 )))
                               ),
                        column(width = 6,
                               lapply(c(31:46), function(i)
                                 uiOutput(paste0(
                                   "q", i
                                 ))))
                      ), 
               
             ))
             ))
    ),
    
    tabPanel(title = "Expert decision table",
             br(),
             DT::dataTableOutput("expert_records")
             
    ),
    
    tabPanel(title = "About and Preferences",
             # Application title
             h2("About"),
             includeMarkdown("text/about.md"),
             hr(),
             h2("Preferences"),
             h4("Choose file extension"),
             selectInput(
               "downloadtype",
               "Download file extension for data",
               choices = c("xlsx","csv")
             ),
             selectInput(
               "downloadtype_report",
               "Download file extension for report",
               choices = c("pdf","docx")
             ),
             
             )
    
    ),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(5%);
             width:25%;
             height: 7%;
             left: calc(75%);
             }
             "
        )
      )
    ),    
use_waiter(),
useShinyjs()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    waiter::waiter_hide()

# Initial initial_decision --------------------------------------------------------

  # Render text to show at top
  # Details the number found by search and the number needing inital screening
  output$number_initial_undecide <- renderText({
    initaldecision()
    paste0(
      "There were ",
      db$count('{}'),
      " records found by the search, of which ",
      db$count('{"initial_decision": "Undecided"}'),
      " record(s) need an initial decision"
    )
  })
    
  # Render ID dropdown box
  output$initalID <- renderUI({
    # Take dependency on inital decision
    initaldecision()
    
    # If showall, find all records, and don't autoprogress when screening
    if (input$showall == FALSE) {
      find <- '{}'
      selected <- isolate(input$ID)
    } else {
      find <- '{"initial_decision": "Undecided"}'
      selected <- ""
    }
    
    tagList(selectInput(
      "ID",
      "ID",
      choices = db$find(find, fields = '{"_id": false,"ID": true}'), 
      selected = selected
    ))
    
  })
  
  
  # Render table showing selected record
  output$test <- renderTable({
    initaldecision()
    req(input$ID)
    
    # If showall is TRUE, define search to only find records without an inital
    # decision
    # If FALSE, find all
    if (input$showall == FALSE) {
      find <- '{}'
    } else {
      find <- '{"initial_decision": "Undecided"}'
    }
    
    # If no of records found using search is 0, show nothing
    if (nrow(db$find(find, fields = '{"_id": false,"ID": true}')) != 0) {
      data <-
        db$find(query = sprintf('{"ID" : %s}', as.numeric(input$ID)),
                fields = '{"_id": false,"ID":true,"title": true, "abstract": true, "link": true, "initial_decision": true, "expert_decision": true}')
      # Format hyperlink correctly
      data$link <-
        ifelse(data$link != "",
               paste0("<a href='", data$link, "' target='_blank'>Link</a>"),
               "")
      data$ID <- as.character(data$ID)
      
      # Fix error created by empty abstracts
      if (is.null(data$abstract)) {
      data$abstract <- "NO ABSTRACT"
      data[, c(5, 1,6,2:4)]
      } else {
      data[, c(6, 1:5)]
      }  
    }
    
  }, sanitize.text.function = function(x)
    x)
    
  # Create reactive value that captures a decision  
  initaldecision <- reactive({
    input$initalinclude
    input$initalexclude
  })

  # Make inital "Include" decision and show notification
  observeEvent(input$initalinclude, {
    db$update(query = sprintf('{"ID" : %s}', as.numeric(input$ID)),
              update = '{"$set":{"initial_decision":"Include"}}')
    showNotification(h4(paste0(
      "Set ID ",
      as.numeric(input$ID),
      " to Initial: ",
      db$find(sprintf('{"ID" : %s}', as.numeric(input$ID)), fields = '{"_id": false,"initial_decision": true}')
    )), type = "message", duration = 60)
  })
  
  # Make inital "Exclude" decision and show notification
  observeEvent(input$initalexclude, {
    db$update(query = sprintf('{"ID" : %s}', as.numeric(input$ID)),
              update = '{"$set":{"initial_decision":"Exclude"}}')
    showNotification(h4(paste0(
      "Set ID ",
      as.numeric(input$ID),
      " to Initial: ",
      db$find(sprintf('{"ID" : %s}', as.numeric(input$ID)), fields = '{"_id": false,"initial_decision": true}')
    )), type = "message", duration = 60)
  })


# Expert decision ---------------------------------------------------------

  
  output$number_expert_undecide <- renderText({
    initaldecision()
    expertdecision()
    paste0(
      "There were ",
      db$count('{"initial_decision": "Include"}'),
      " record(s) marked for further assessment, of which ",
      db$count('{"initial_decision": "Include", "q12": "FALSE"}')+
        db$count('{"initial_decision": "Include", "q12": "False"}'),
      " record(s) need expert assessment and data extraction."
    )
  })
    
  output$expertID <- renderUI({
    input$expertexclude
    initaldecision()

    if (input$showallexpert == TRUE) {
      find <- '{"initial_decision": "Include", "q12": "FALSE"}'
      selected <- ""
    } else {
      find <- '{"initial_decision": "Include"}'
      selected <- isolate(input$expert_ID)
    }
    
    test <- db$find(find, fields = '{"_id": false, "ID": true, "authors": true}')
    test$name <- paste0(test$ID," - ", gsub(",","",stringr::word(test$authors, 1)))
    
    choices <- setNames(test$ID,test$name                        )
    
    test <- input$expert_ID
    if (nrow(db$find(find, fields = '{"_id": false,"ID": true}')) != 0) {
      tagList(
        selectInput(
          inputId = "expert_ID",
          label = "ID",
          selected = selected,
          choices = choices
        )
      )
    }
  })
  
  
  observe({
    initaldecision()
    expertdecision()
    req(input$expert_ID)
    
    if (input$showallexpert == TRUE) {
      find <- '{"initial_decision": "Include", "q12": "FALSE"}'
    } else {
      find <- '{"initial_decision": "Include"}'
    }
    
    if (nrow(data.frame(db$find(find, fields = '{"_id": false,"ID": true}'))) > 0) {
      show("form")
    } else {
      hide("form")
    }
    
    updateSelectInput(session, "exclusion_reason",selected = "")
  })
      


    expertdecision <- reactive({
      input$expertinclude
      input$expertexclude
    })
    
    # Make inital decisions
    observeEvent(input$expertinclude,{
      db$update(query = sprintf('{"ID" : %s}',as.numeric(input$expert_ID)), update = '{"$set":{"expert_decision":"Include"}}')
    })
    
    observeEvent(input$expertexclude,{
      req(input$exclusion_reason)
      db$update(query = sprintf('{"ID" : %s}',as.numeric(input$expert_ID)), update = '{"$set":{"expert_decision":"Exclude", "initial_decision":"Exclude" }}')
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"exclusion_reason":"%s"}}', as.character(input$exclusion_reason))
      )
     })
    
    
    output$expert_table <- renderTable({
      expertdecision()
      req(input$expert_ID)
      
      if (input$showallexpert == TRUE) {
        find <- '{"initial_decision": "Include", "q12": "FALSE"}'
      } else {
        find <- '{}'
      }

      if(nrow(db$find(find, fields = '{"_id": false,"ID": true}')) != 0){
        data <- db$find(query = sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                        fields ='{"_id": false,"ID": true,"title": true, "abstract": true, "authors": true, "link": true, "initial_decision": true, "expert_decision": true,"q12": true}')
        
        data$link <- ifelse(data$link!="",paste0("<a href='", data$link, "' target='_blank'>Link</a>"),"")
        data$ID <- as.character(data$ID)
      if (is.null(data$abstract)) {
        data$abstract <- "NO ABSTRACT"
      } 
      if (is.null(data$authors)) {
        data$authors <- "NO AUTHORS LISTED"
      } 
        
        data <- data[, c("ID",
                 "title",
                 "authors",
                 "abstract",
                 "link",
                 "initial_decision",
                 "expert_decision",
                 "q12")]
        colnames(data)[8] <- "Complete"
        data
      }
      
      
      
    }, sanitize.text.function = function(x) x
    )
    
    
    output$basic_header <- renderUI({
      expertdecision()
      req(input$expert_ID)
      
          tagList(
          h4("Data extraction")
          )
    })
    
    output$adv_header <- renderUI({
      expertdecision()
      req(input$expert_ID)
      
          tagList(
            h4("Quality of reporting")
          )
    })
    
    
    output$q0 <- renderUI({
      expertdecision()
      req(input$expert_ID)
      
      tagList(
        selectInput("q0",
                    "Assessor initals",
                    # Initials only
                    choices = sort(c("","LS","BKO","LM","JT","JH")),
                    selected = db$find(sprintf(
                      '{"ID" : %s}', input$expert_ID
                    ),
                    fields = '{"_id": false,"q0": true}')
                    )
      )
    })
    ##################DATA EXTRACTION FORM######################################################
    
    output$q1 <- renderUI({
      expertdecision()
      req(input$expert_ID)
      
      tagList(
        selectInput("q1", "1. Data extraction approach used", width = "100%", choices =  c("","Rule-base including regex",
                                                                                 "CRF",
                                                                                 "Word embedding, eg. word2vec or glove",
                                                                                 "Character embedding",
                                                                                 "PICO embedding",
                                                                                 
                                                                                 "LSTM",
                                                                                 "CRF",
                                                                                 "SVM",
                                                                                 "APIs and metadata retrieval",
                                                                                 "PDF extraction",
                                                                                 "Other binary classifier (Naive Bayes, Decision Tree, regression.. )",
                                                                                 "Tree classifier",
                                                                                 "Ontology pipeline",
                                                                                 "BERT, SCIBERT, or BIOBERT transformer",
                                                                                 
                                                                                 "Transformer, eg. Electra, XLnet ...", 
                                                                                 "Other (please specify below)"), multiple = TRUE,
                    selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                                 fields = '{"_id": false,"q1": true}'),", "))
        )
      )
      
      
     
    })
    
    
    
    
    
    
    output$q2 <- renderUI({
      expertdecision()
      req(input$expert_ID)
      
      tagList(
        textAreaInput2("q2", "1.1 If other, please specify:", value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                          fields = '{"_id": false,"q2": true}'))
      )
    })

    
    output$q3 <- renderUI({
      expertdecision()  
      req(input$expert_ID)
      

      tagList(
        selectInput(
          "q3", 
          "2. Reported performance metrics used for evaluation", 
          width = "100%", 
          choices =  c("","Precision",
                        "Recall",
                        "F1",
                        "WSS@95",
                        "WSS",
                        "Specificity",
                        "Accuracy",
                        "AUC-ROC",
                        "Other (please specify below)"), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q3": true}'),", "))
        )
      )
    })
    
    output$q4 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      

      tagList(
        textAreaInput2("q4", "2.2 If other (incl. continuous outcomes such as 'Time saved' etc):", width = "100%", value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                                    fields = '{"_id": false,"q4": true}'))
      )
    })
  #######################################  
    # Outcome investigated
    output$q5 <- renderUI({
      expertdecision()  
      req(input$expert_ID)
      

      tagList(
        selectInput(
          "q5", 
          "3. Type of data", 
          width = "100%", 
          choices =  c("","Abstracts","Full texts","Method sections" ,"Other"), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q5": true}'),", "))
        )
        
      )
    })
    
    
   ########################################## 
    
    
    
    output$q6 <- renderUI({
      expertdecision()     
      req(input$expert_ID)
      

      tagList(
        selectInput(
          "q6", 
          "4. Target (study) design for data extraction", 
          width = "100%", 
          choices =  c("","RCT", "Cohort", "Case series","Animal studies","Cross sectional survey","Case control","Diagnostic test","Non-randomised (intervention) study","Other","Mix"), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q6": true}'),", ")))
      )
    })
   ########################################## 
    output$q7 <- renderUI({
      expertdecision()    
      req(input$expert_ID)
      

      tagList(
        selectInput(
          "q7", 
          "5. The input data format", 
          width = "100%", 
          choices =  c("","Text, or probably text file",
                       "Unclear",
                       "PDF",
                       "XML",
                       "JSON",
                       "HTML",
                       "RIS",
                       "Ontology",
                       "Other"
          ), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q7": true}'),", ")))
      )
    })
    ###########################################
    output$q8 <- renderUI({
      expertdecision() 
      req(input$expert_ID)

      tagList(
        selectInput(
          "q8", 
          "6. The output data format", 
          width = "100%", 
          choices =  c("","Text, or probably text file",
                       "Spans and/or annotations",
                       "Structured text, eg. a summary",
                       "Unclear",
                       "PDF",
                       "XML",
                       "JSON",
                       "HTML",
                       "RIS",
                       "Ontology",
                       "Other"
          ), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q8": true}'),", ")))
      )
    })
    ######################################
    output$q9 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      

      tagList(
        selectInput(
          "q9", 
          "7. Mined fields", 
          width = "100%", 
          choices =  c("","P",
                       "IC",
                       "IC (per arm)",
                       "IC (Drug name)",
                       "IC (Drug dose, strength, route, frequency, duration, etc..)",
                       
                       "O",
                       "O (time point)",
                       "O (primary or secondary outcome)",
                       "O (measurement instrument)",
                       
                       "N (per arm)",
                       "N (total)",
                       "Age",
                       "Gender",
                       
                       "Withdrawals or exclusions",
                       
                       "Randomisation",
                       "Blinding",
                       "Setting",
                       "Design",
                       "Sections (Aim, or methods, result or concl.)",
                       
                       
                       "Eligibility criteria",
                       "Exclusion criteria",
                       "Enrolment dates",
                       "Funding org",
                       "Grant number",
                       "Early stopping",
                       "Trial registration",
                       "Other (please specify below)"), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q9": true}'),", ")))
      )
    })
    ############################################
    output$q10 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      

      tagList(
        textAreaInput2("q10", "7.1 More mined fields/coments:",width = "100%", value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                                  fields = '{"_id": false,"q10": true}'))
      )
    })
    ##############################################
    output$q11 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      

        tagList(
          selectInput(
            "q11", 
            "8. Granularity of data mining", 
            width = "100%", 
            choices =  c("Entities", "Sentences", "Paragraphs/Sections","Binary for each document" ,"Other"), 
            multiple = TRUE,
            selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                         fields = '{"_id": false,"q11": true}'),", ")))
        )
      
    })
    
    #########################################
    output$q12 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        checkboxInput("q12", "Check this box to mark the record as \"Complete\" once you have finished filling out the fields.",
                      value = as.logical(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                                                          fields = '{"_id": false,"q12": true}'))
                      )
      )
      
    })
    ################################################
    output$q13 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q13", 
                       "9. NOTES on data extraction (include questions for authors and if we need to email them + contact details):",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q13": true}'))
        )
      
      
    })
    
    
    #########################################################
    #
    #QUALITY OF REPORTING
    #
    ########################################################
    output$q14 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q14",
                    "1. Are the sources for training/testing data reported?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q14": true}')))
    })
    
    output$q15 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q15", 
                       "1.1 Describe data source, eg. medline, EBM-NLP..",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q15": true}'))
      )
      
      
    })
                
                
    ##################################
    output$q16 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q16",
                    "1.2 Can data be retrieved based on the information given in the publication?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q16": true}')))
    })
    
    
    ##################################
    output$q17 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q17",
                    "2. Is there a description of the dataset used and of its characteristics?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q17": true}')))
    })
    
    output$q18 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q18", 
                       "2.1 What exactly is described (no need to extract exact numbers, just domain. EG distribution of entites, size .. )",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q18": true}'))
      )
      
      
    })
    
    
    #################################
    
    output$q19 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q19",
                    "3. Were the data pre-processed prior to data extraction?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q19": true}')))
    })
    
    output$q20 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q20", 
                       "3.1 If pre-processing techniques were applied to the data, how are they described? EG. stemming, truncation...",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q20": true}'))
      )
      
      
    })
    
    ####################################
    #################################
    
    output$q21 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q21",
                    "4. Is there a description of the algorithms used?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q21": true}')))
    })
    
    output$q22 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q22", 
                       "4.1 Please specify: EG if yes, are hyperparameters described?",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q22": true}'))
      )
      
      
    })
    #########################################
    output$q23 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q23",
                    "5. Is there a description of the hardware or computing time used?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q23": true}')))
    })
    
    #######################################
    
    output$q24 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q24",
                    "6. Can we access source code based on the information in the publication?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q24": true}')))
    })
    
    output$q25 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q25", 
                       "6.1 Please specify direct link or means of access to the code:",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q25": true}'))
      )
      
      
    })
    
    
    ###########################################
    #######################################
    
    output$q26 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q26",
                    "7. Can we obtain a runnable version of the software based on the information in the publication, eg. a web-app or desktop version?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q26": true}')))
    })
    
    output$q27 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q27", 
                       "7.1 Please specify direct link or means of access to the app:",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q27": true}'))
      )
      
      
    })
    
    ###########################################
    #######################################
    
    output$q28 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q28",
                    "8. Is there a justification/ an explanation of the model assessment?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q28": true}')))
    })
    
    output$q29 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q29", 
                       "8.1 How is assessment described? EG. error analysis done, practical test done by reviewers...",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q29": true}'))
      )
      
      
    })
    
    
    ###########################################
    #######################################
    
    output$q30 <- renderUI({
      expertdecision()  
      req(input$expert_ID)
      
      
      tagList(
        selectInput(
          "q30", 
          "9. Which basic metrics are reported (true/false positives and negatives) or can be inferred?", 
          width = "100%", 
          choices =  c("","True pos.","True neg.","False pos." ,"False neg.", "None reported"), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q30": true}'),", "))
        )
        
      )
    })
    
    
    ###########################################
    #######################################
    output$q31 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q31",
                    "10. Does the assessment include any information about trade-offs between recall and precision (also known as sensitivity and positive predictive value)?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q31": true}')))
    })
    
    output$q32 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q32", 
                       "10.1 Please describe the trade-off, EG. 'discussion about hyperparameters and its influence on false-negative retrieval', or 'plots of trade-off given'",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q32": true}'))
      )
      
      
    })
    
    
    
    ###########################################
    output$q33 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q33",
                    "11. Is the use of third-party frameworks reported and are they accessible?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q33": true}')))
    })
    
    output$q34 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q34", 
                       "11.1 Please specify, EG. 'Mallet for CRF, GENIA-tagger for POS'",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q34": true}'))
      )
      
      
    })
    ##########################################
    output$q35 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q35",
                    "12. Does the dataset or assessment measure provide a possibility to compare to other tools in same domain?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q35": true}')))
    })
    
    output$q36 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q36", 
                       "12.1 Please specify, EG. 'common entities, general-topic text' or 'tested on selective and small dataset'",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q36": true}'))
      )
      
      
    })
    ##########################################
    ###########################################
    output$q37 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q37",
                    "13. Is the influence of both visible and hidden variables in the dataset discussed? (ie. fairness/bias within dataset, see https://arxiv.org/pdf/1908.09635.pdf)",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q37": true}')))
    })
    
    output$q38 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q38", 
                       "13.1 Please specify, EG. 'Error analysis: Influence of specific entity positions leading to misclassification is described for two entity types' or 'influence of certain words such as <inclusion criteria:> discussed as factor for classifying inclusion criteria'. ",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q38": true}'))
      )
      
      
    })
    ##########################################
    output$q39 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q39",
                    "14. Is the process of avoiding over- or underfitting described? N/A for regex/rule-base becasue they are 'overfitted' by design?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q39": true}')))
    })
    
    output$q40 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q40", 
                       "14.1 Please specify, EG. 'Cross-validation, learning curves given', or . ",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q40": true}'))
      )
      
      
    })
    ##########################################
    ###########################################
    output$q41 <- renderUI({
      expertdecision()  
      req(input$expert_ID)
      
      
      tagList(
        selectInput(
          "q41", 
          "15. Is the process of splitting training from validation data described?", 
          width = "100%", 
          choices =  c("","Yes, random splits and ratio given","Yes, just ratio given","Yes, compl. different datasets" ,"Yes, other description given", "One dataset/ NA", "Not reported"), 
          multiple = TRUE,
          selected = unlist(stringr::str_split(db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                                       fields = '{"_id": false,"q41": true}'),", "))
        )
        
      )
    })
    
    ##########################################
    output$q42 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q42",
                    "16. Is the model’s adaptability to different formats and/or environments beyond training and testing data described?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q42": true}')))
    })
    
    output$q43 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q43", 
                       "16.1 Please specify, EG. 'evaluation on completely different dataset and domain', 'evaluated on multiple benchmark sets', 'discussed effects of idiosyncrasy' ",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q43": true}'))
      )
      
      
    })
    ##########################################
    output$q44 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        textAreaInput2("q44", 
                       "17. Does the paper describe caveats for using the method, or, for using automated data extraction in general?",width = "100%", 
                       value = db$find(sprintf('{"ID" : %s}',as.numeric(input$expert_ID)),
                                       fields = '{"_id": false,"q44": true}'))
      )
      
      
    })
    
    ############################################
    output$q45 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q45",
                    "18. Are sources of funding described?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q45": true}')))
    })
    
    output$q46 <- renderUI({
      expertdecision() 
      req(input$expert_ID)
      tagList(
        selectInput("q46",
                    "19. Are conflicts of interest reported?",
                    choices = c(sort(c("", "Yes", "No", "Unclear/NA"))),
                    selected = db$find(sprintf('{"ID" : %s}', input$expert_ID),
                                       fields = '{"_id": false,"q46": true}')))
    })
    ###############################################
    
    ############################################
    
    ##########################################
    
    
    
    
    
    # Update outcome, collapsing for proper saving
    # This is why it isn't in the lapply call later on
    observeEvent(input$q15,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q15":"%s"}}', paste0(input$q15,collapse = ", "))
      )
    })
    
    observeEvent(input$q30,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q30":"%s"}}', paste0(input$q30,collapse = ", "))
      )
    })
    observeEvent(input$q41,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q41":"%s"}}', paste0(input$q41,collapse = ", "))
      )
    })
    
    observeEvent(input$q5,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q5":"%s"}}', paste0(input$q5,collapse = ", "))
      )
    })
    
    observeEvent(input$q1,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q1":"%s"}}', paste0(input$q1,collapse = ", "))
      )
    })
    
    observeEvent(input$q3,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q3":"%s"}}', paste0(input$q3,collapse = ", "))
      )
    })
    
    observeEvent(input$q6,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q6":"%s"}}', paste0(input$q6,collapse = ", "))
      )
    })
    
    observeEvent(input$q7,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q7":"%s"}}', paste0(input$q7,collapse = ", "))
      )
    })
    observeEvent(input$q8,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q8":"%s"}}', paste0(input$q8,collapse = ", "))
      )
    })
    
    observeEvent(input$q9,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q9":"%s"}}', paste0(input$q9,collapse = ", "))
      )
    })
    observeEvent(input$q11,{
      db$update(
        query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
        update = sprintf('{"$set":{"q11":"%s"}}', paste0(input$q11,collapse = ", "))
      )
    })
    
    
    
    # Capture inputs
    lapply(c(0,2,4, 10,12:14, 16:29, 31:40, 42:46), function(i){
      observeEvent(input[[paste0("q",i)]],{
        db$update(
          query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
          update = sprintf('{"$set":{"q%s":"%s"}}', i, gsub("\\n","\\\\n",gsub('"','\\\\"',as.character(input[[paste0("q",i)]]))))
        )
      })
    })
        
    
    # observeEvent(input$exclusion_reason,{
    #   db$update(
    #     query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)),
    #     update = sprintf('{"$set":{"exclusion_reason":"%s"}}', as.character(input$exclusion_reason))
    #   )
    # })

# All screened ------------------------------------------------------------

output$total_no <- renderText({
  paste0("Browse decision for all ",db$count()," records")
})    
    
output$all_records <- DT::renderDataTable({
  initaldecision()
  data <- db$find(query = '{}', fields = '{"_id": false,"ID": true,"title": true, "abstract": true,"initial_decision": true, "expert_decision": true, "link": true, "authors":true}')
  data$link <- sprintf("<a href='%s' target='_blank'>Link</a>", data$link)
  data$ID <- as.character(data$ID)
  DT::datatable(
        data[,c(7,1:6)],
        rownames = FALSE,
        escape = FALSE
  )
})
    
output$downloadallscreened <- downloadHandler(
  filename = function() {
    paste0("all_screened.", input$downloadtype, sep = "")
  },
  content = function(file) {
    data <- db$find(query = '{}')
    data <- data[,c(21,1:20,22:ncol(data))]
    rio::export(data, file, format = input$downloadtype)
  }
)   


# Expert decisions --------------------------------------------------------

output$expert_records <- DT::renderDataTable({
  expertdecision()
  data <- db$find(query = '{}', fields = '{"_id": false,"ID": true,"title": true, "abstract": true,"initial_decision": true, "expert_decision": true, "link": true, "authors":true, "exclusion_reason": true, "q12": true,"q13":true,"q0":true }')
  data <- data %>% dplyr::filter(expert_decision != "")
  colnames(data)[which(colnames(data)=="q13")] <- "Background?"
  colnames(data)[which(colnames(data)=="q12")] <- "Complete?"
  colnames(data)[which(colnames(data)=="q0")] <- "Expert"
  data$link <- sprintf("<a href='%s' target='_blank'>Link</a>", data$link)
  data$ID <- as.character(data$ID)
  DT::datatable(
    data[, c(
      "ID",
      "title",
      "authors",
      "abstract",
      "link",
      "Expert",
      "expert_decision",
      "Complete?",
      "Background?"
    )], 
    rownames = FALSE,
    escape = FALSE
  )
})


# File output -------------------------------------------------------------

# Generate report for that study

output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = paste0("report.",isolate(input$downloadtype_report)),
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    tempReport <- file.path(tempdir(), "report.Rmd")
    
    if (isolate(input$downloadtype_report) == "pdf") {
    file.copy("report_pdf.Rmd", tempReport, overwrite = TRUE)
    } else {
    file.copy("report_word.Rmd", tempReport, overwrite = TRUE)
    }
    
    
    # Set up parameters to pass to Rmd document
    params <- list(ID = input$expert_ID, 
                   Title =       db$find(query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)), fields = '{"_id": false, "title": true}'),
                   Authors =     db$find(query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)), fields = '{"_id": false, "authors": true}'),
                   Abstract =    db$find(query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)), fields = '{"_id": false, "abstract": true}'),
                   Link =        db$find(query = sprintf('{"ID" : %s}', as.numeric(input$expert_ID)), fields = '{"_id": false, "link": true}'),
                   Design =      input$q1,
                   Designother = input$q2,
                   Setting =     input$q3,
                   Population =  input$q4,
                   Outcome =     input$q5,
                   Outcomeother =input$q12,
                   Size =        input$q6,
                   Findings =    input$q7,
                   Strength =    input$q8,
                   Limit =       input$q9,
                   Policyimp =   input$q10,
                   Researchimp = input$q11
    )
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport,
                      output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


}







# Run the application 
shinyApp(ui = ui, server = server)
