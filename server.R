library(shiny)

diabetesRisk <-  function(glucose) glucose / 200

shinyServer(
    function(input, output) {
        output$oid1 <- renderPrint({input$id1})
        output$oid2 <- renderPrint({input$id2})
        output$odate <- renderPrint({input$date})
        
        # diabetes stuff
        output$inputValue <- renderPrint({input$glucose})
        output$prediction <- renderPrint({diabetesRisk(input$glucose)})
        output$newHist <-  renderPlot({
            hist(sample(1:30, 100, replace=TRUE))
        }
        )
    }
)