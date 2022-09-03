#--------------------------------------------------
# R UI Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(shiny))

shinyUI(fluidPage(
  
  # Application title
  navbarPage("Data Science Capstone, Swiftkey word Prediction",
             tabPanel("Home"),
           
             navbarMenu("Method",
                        tabPanel("Description", p("This app uses n-gram backoff model to predict the next word in a sentence. It is based on a subset of data prepared in advance of blogs, news and tweets. The sampled data represents n-grams of several depth level with their frequencies, as a basis for the cascading model below.")),
                       )
  ),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      textInput("sentence", "Continue the sentence ", value = "type here"),
      sliderInput("obs", "maximum predictions:",
                  min = 0, max = 6, value = 3
      )),
    
    mainPanel(
      h4("Sentence"),
      verbatimTextOutput("text"),
      
      h4("Prediction"),
      verbatimTextOutput("prediction"),
      
      mainPanel(plotOutput('dataset')),
      br(),
      br(),
      dataTableOutput("output_dataset")
    )
  )
))