library(shiny)
library(ggplot2)

diabetesRisk <-  function(glucose) glucose / 200

draw_plot <- function(mu_0, sigma_0, mu_a, sigma_a, lin, rev_order) {
    temp_range <- c(mu_0-(4*sigma_0), mu_0+(4*sigma_0), 
                    mu_a-(4*sigma_a), mu_a+(4*sigma_a))
    xvals <- seq(min(temp_range), max(temp_range), length = 1000)
    
    if (is.null(rev_order)) {
        y1 <- dnorm(xvals, mean = mu_0, sd = sigma_0)
        y2 <- dnorm(xvals, mean = mu_a, sd = sigma_a)
    } else {
        # reverse null and alternative
        y2 <- dnorm(xvals, mean = mu_0, sd = sigma_0)
        y1 <- dnorm(xvals, mean = mu_a, sd = sigma_a)
    }
    
    if (is.null(rev_order)) {
        null_grp <- rep("none", length(y1))
        null_grp[xvals < lin] = "TN"
        null_grp[xvals >= lin] = "FP"
        
        alt_grp <- rep("none", length(y2))
        alt_grp[xvals < lin] = "FN"
        alt_grp[xvals >= lin] = "TP"
        
        categories <- c(null_grp, alt_grp)
        
        y <- c(y1,y2)
        dist <- c( rep("Null", length(y1)), rep("Alternative", length(y2)) )
    } else {
        null_grp <- rep("none", length(y2))
        null_grp[xvals < lin] = "TP"
        null_grp[xvals >= lin] = "FN"
        
        alt_grp <- rep("none", length(y1))
        alt_grp[xvals < lin] = "FP"
        alt_grp[xvals >= lin] = "TN"
        
        categories <- c(null_grp, alt_grp)
        
        y <- c(y2,y1)
        dist <- c( rep("Null", length(y2)), rep("Alternative", length(y1)))
    }
    df <- data.frame(x=c(xvals,xvals), y=y, dist=dist, categories=categories)
    
    g_plot <- ggplot(df, aes(x,y, group=dist)) + geom_line() + 
        geom_ribbon(data=subset(df,x>lin & x<max(x)),
                    aes(x=x, ymax=y, group=dist, fill=categories),
                    ymin=0,
                    colour="black",
                    size=1.25,
                    alpha=0.5) + 
        geom_ribbon(data=subset(df,x>min(x) & x<lin),
                    aes(x=x, ymax=y, group=dist, fill=categories),
                    ymin=0,
                    colour="black",
                    size=1.25,
                    alpha=0.5) +
        geom_vline(xintercept = lin, size=2, color="black") +
        theme_bw(base_family = "Gill Sans", base_size = 14)
    
    if (is.null(rev_order)) {
        g_plot <- g_plot + 
                  annotate("text", label=paste("H[0]"), parse=TRUE, x = mu_0, y = max(y1) + 0.025, size = 8, colour = "black") +
                  annotate("text", label=paste("H[A]"), parse=TRUE, x = mu_a, y = max(y2) + 0.025, size = 8, colour = "black")
    } else {
        g_plot <- g_plot + 
            annotate("text", label=paste("H[A]"), parse=TRUE, x = mu_0, y = max(y2) + 0.025, size = 8, colour = "black") +
            annotate("text", label=paste("H[0]"), parse=TRUE, x = mu_a, y = max(y1) + 0.025, size = 8, colour = "black")
    }
    g_plot
}


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
        output$odate <- renderPrint({input$rev_order})
        # mean model stuff
        output$newPlot <- renderPlot({
            draw_plot(input$mu_0,
                      input$sd_0,
                      input$mu_a,
                      input$sd_a,
                      input$lin,
                      input$rev_order)
        })
        
        output$table <- renderTable({data.frame(mu=c(input$mu_0,input$mu_a), sd=c(input$sd_0, input$sd_a))})
        
    }
)