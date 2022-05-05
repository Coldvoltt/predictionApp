
library(shiny)

# Define UI for application that draws a histogram
library(shinydashboard)
library(shinyjs)
library(shinyalert)

ui <- dashboardPage(
  #Dashboard title
  dashboardHeader(title = 'PREDICTION APP', 
                  titleWidth = 290),
  
  #Sidebar layout
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem(
        tabName = "pred",
        text= "Prediction",
        icon = icon('search')
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
    tabItems(
      #Prediction Tab
      tabItem(
        tabName = "pred",
        column(
          width = 4,
          box(status = 'primary',
              title = "Input Values",
              width = 12,
              textInput("number", "Number", placeholder = "Input the number"),
              textInput("slr", "SLR", placeholder = "Input SLR"),
              textInput("nn", "N", placeholder = "Input N"),
              textInput("age", "Age", placeholder = "Input Age"),
              textInput("Zn", 'Zn', placeholder = "Input Zn"),
              textInput("average.wt", "Average Weight", placeholder = "Input the Average weight"),
              br()
          ),
          box(
            status = 'success',
            title = 'Prediction Result',
            solidHeader = TRUE,
            width = 12,
            div(h5('Predicted cause')),
            verbatimTextOutput("value", placeholder = TRUE),
            actionButton("cal","Calculate", icon = icon('calculator'))
          )
        ),
        
        column(
          width = 5,
          box(
            status = 'success',
            title = 'About the model',
            width = 12,
            height = 220,
            helpText(h4(
              "The Model used for prediction in this analysis is the Random Forest model. It is prefered for
                             classification because it has a higher accuracy value.",
              br(),
              "Six of the most influential features are selected for classification in the model.
                             Variable definition is in the next tab."
            ))
            
          ),
          
          box(
            status = 'success',
            title = 'About the data',
            width = 12,
            height = 500,
            helpText(h5(
              "The dataset used in this analysis is supplied by Marine Scotland on 03/04/2022 
                            on the scotland's aquaculture website and may be found in the link 
                            http://aquaculture.scotland.gov.uk/data/data.asp.", br(),
              
              "It contains 13 variables ans 221 cases about fish escapes. The variables are
                             described as follows:",
              br(),
              br(),
              "Season: a category with 4 levels, {Spring, Summer, Autumn, Winter}.",
              br(),
              br(),
              "Age: the average age of fish that escaped.",
              br(),
              br(),
              "Average.Weight: The average weight in g of escaped fishes.",
              br(),
              br(),
              "Number: The estimated number of escaped fishes.",
              br(),
              br(),
              "Cause: a category with 2 levels, describing the cause of the escape
                              whether Human error or else a Natural event.",
              br(),
              br(),
              "Producing: a category with 2 levels, describing whether the site
                             was producing within last 3 years.",
              br(),
              br(),
              "SLR: a measure of sea-lice residue recorded at the site.",
              br(),
              br(),
              "Cu: a measure of level of copper compounds detected in water at the site.",
              br(),
              br(),
              "Zu: a measure of level of zinc compounds detected in water at the site.",
              br(),
              br(),
              "N: a measure of level of nitrogen compounds detected in water at the site.",
              br(),
              br(),
              "P: a measure of level of phosphorus compounds detected in water at the site.",
              br(),
              br(),
              "Org:a measure of level of organic compounds detected in water at the site."
            ))
            
          )
        )
        
      )
    )
  )
)


# load("model.rds") #Loads saved model
model<- readRDS(file = "model.rds")

server <- shinyServer(function(input, output) {
  #Prediction model
  a<- reactiveValues(result = NULL)
  #test data without response var.
  
  observeEvent(input$cal,{
    if(input$number==""){
      shinyalert("You need to input number!", type = "error")
    }else if (input$slr==""){
      shinyalert("You need to input value for slr!", type = "error")
    }else if (input$nn==""){
      shinyalert("You need to input value for N!", type = "error")
    }else if (input$age==""){
      shinyalert("You need to input age!", type = "error")
    }else if (input$Zn==""){
      shinyalert("You need to input Zn!", type = "error")
    }else if (input$average.wt==""){
      shinyalert("You need to input average weight!", type = "error")
    } else { 
      #DF for single Prediction
      values = data.frame(Number = input$number,
                          SLR = input$slr,
                          N = input$nn,
                          Age = input$age,
                          Zn = input$Zn,
                          Average.Weight = input$average.wt)
      
      #Single Pred using RF
      a$result<- predict(model, newdata = values)
      shinyalert("Prediction succesfully made!", type = "success")
      
      output$value <- renderText({
        #Display prediction Value
        paste(a$result)
      })
      
    }
    
  })
  
  
})


# Run the application 
shinyApp(ui = ui, server = server)
