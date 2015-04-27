library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Word predictor"),
        sidebarPanel(
                h2('Parameters'),
                p('Please type a sentence or part of a sentence.'),   
                
                textInput("string", label = h3("Text input"), value = "Enter sentence..."),
                
                submitButton("Submit")
                       
        ),
        
        mainPanel(
                h3('Introduction'),
                p('This shiny app will predict the next word when typing a sentence.'),
                
                h3('Results'),
                p('Wait a moment after submitting, results may take some seconds.'),
                
                verbatimTextOutput("predictmessage")
        )
))  
