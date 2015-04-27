#load the libraries
library(shiny)


#load the data
words4count <- read.table("words4count.txt")
words3count <- read.table("words3count.txt")
words2count <- read.table("words2count.txt")




#start the shiny server
shinyServer(

        function(input, output) {

                #predict function
                predictword <- reactive({
                        split <- unlist(strsplit(as.character(tolower(input$string)), split=" "))
                        
                        if (length(split) >= 3) {
                                p3 <- split[(length(split)-2)]
                                p2 <- split[(length(split)-1)]
                                p1 <- split[(length(split)-0)]
                                #n3
                                predict <- words4count[words4count$n3 == p3 &  
                                                               words4count$n2 == p2 &
                                                               words4count$n1 == p1,]$n0
                                if(is.na(predict[1])) {
                                        
                                        
                                        predict <- words3count[words3count$n2 == p2 &
                                                                       words3count$n1 == p1,]$n0
                                        if(is.na(predict[1])) {
                                                
                                                predict <- words2count[words2count$n1 == p1,]$n0
                                                
                                                if(is.na(predict[1])) {
                                                        predictoutcome<- paste("could not predict next word")
                                                        predictoutcome
                                                }
                                                else{
                                                        predictoutcome<- paste("2ngram: ",predict[1])
                                                        predictoutcome
                                                }
                                                
                                        }
                                        else{
                                                predictoutcome<- paste("3ngram: ",predict[1])
                                                predictoutcome 
                                        }
                                        
                                        
                                        
                                        
                                }
                                else{
                                        predictoutcome<- paste("4ngram: ",predict[1])
                                        predictoutcome
                                }
                                
                        }
                        else if (length(split) == 2){
                                p2 <- split[(length(split)-1)]
                                p1 <- split[(length(split)-0)]             
                                #n2
                                predict <- words3count[words3count$n2 == p2 &
                                                               words3count$n1 == p1,]$n0
                                if(is.na(predict[1])) {
                                        
                                        predict <- words2count[words2count$n1 == p1,]$n0
                                        
                                        if(is.na(predict[1])) {
                                                predictoutcome<- paste("could not predict next word")
                                                predictoutcome
                                        }
                                        else{
                                                predictoutcome<- paste("2ngram: ",predict[1])
                                                predictoutcome
                                        }
                                        
                                        
                                }
                                else{
                                        predictoutcome<- paste("3ngram: ",predict[1])
                                        predictoutcome 
                                }
                        }
                        else if (length(split) == 1){
                                p1 <- split[(length(split)-0)]                
                                #n1
                                predict <- words2count[words2count$n1 == p1,]$n0
                                
                                if(is.na(predict[1])) {
                                        predictoutcome<- paste("could not predict next word")
                                        predictoutcome
                                }
                                else{
                                predictoutcome<- paste("2ngram: ",predict[1])
                                predictoutcome
                                }
                        }
                        else {
                                predictoutcome<- paste("could not predict next word")
                                predictoutcome
                        }
                })
                
                output$predictmessage <- renderText({ paste(predictword()) })

        
        }
)