library(shiny)
library(ggplot2)
library(flux)

diabetesRisk <-  function(glucose) glucose / 200

my.env <- new.env()
my.env$ROCvalues_x <- NULL # x,y,lin values of the ROC curve
my.env$ROCvalues_y <- NULL
my.env$ROClin <- NULL
my.env$ROC_param <- NULL

draw_distribution_plot <- function(mu_0, sigma_0, mu_a, sigma_a, lin, rev_order) {
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

create_confusion_table <- function(mu_0, sigma_0, mu_a, sigma_a, lin) {
    # calculate areas of TP, TN, FP, FN
    
    TP_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=FALSE), 4)
    FN_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=TRUE), 4)
    TN_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=TRUE), 4)
    FP_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=FALSE), 4)
    
    sens <- TP_prob / (TP_prob + FN_prob)
    spec <- TN_prob / (TN_prob + FP_prob)
    ppv <- TP_prob / (TP_prob + FP_prob)
    npv <- TN_prob / (TN_prob + FN_prob)
    
    acc <- (TP_prob + TN_prob) / (TP_prob + FN_prob + TN_prob + FP_prob)
    
    confusion_table = data.frame(mu=c(TP_prob,FN_prob, sens), sd=c(FP_prob, TN_prob, spec), c(ppv, npv, acc))
    rows <- c("Test_T", "Test_F", "Sensitivity / Specificity")
    cols <- c("Observed_T", "Observed_F", "Precission (PPV) / Negative Predictive Value (NPV) / Accuracy")
    row.names(confusion_table) <- rows
    colnames(confusion_table) <- cols
    
    confusion_table
}

create_ROC_curve <- function(mu_0, sigma_0, mu_a, sigma_a, lin) {
    # calcualte the x,y value of the current value of lin
    
    
    # check to see if the distributions changed
    parameters <- c(mu_0, sigma_0, mu_a, sigma_a)
    if (is.null(my.env$ROC_param)) {
        my.env$ROC_param <- parameters
    }
    
    if (sum(my.env$ROC_param == parameters) != length(parameters)) {
        # some parameter changed, reset the ROC values
        my.env$ROCvalues_x <- NULL # x,y values of the ROC curve
        my.env$ROCvalues_y <- NULL
        my.env$ROClin <- NULL
        my.env$ROC_param <- NULL
    }
    
    if( !(lin %in% my.env$ROClin) ){
        # calculate ROC values
        
        TP_prob = pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=FALSE)
        FN_prob = pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=TRUE)
        TN_prob = pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=TRUE)
        FP_prob = pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=FALSE)
        
        sens <- TP_prob / (TP_prob + FN_prob)
        spec <- TN_prob / (TN_prob + FP_prob)
        
        my.env$ROCvalues_x <- c(my.env$ROCvalues_x, round(1-spec,4))
        my.env$ROCvalues_y <- c(my.env$ROCvalues_y, round(sens,4))
        my.env$ROClin <- c(my.env$ROClin, lin)
    }
    
    # plot current ROC curve
    temp_df <-data.frame(fpr=my.env$ROCvalues_x, sens=my.env$ROCvalues_y, decision=my.env$ROClin)
    if(length(temp_df$fpr) > 1) {
        temp_auc <- round(auc(temp_df$fpr, temp_df$sens),4)
    } else {
        temp_auc <- 0
    }
    
    ggplot(temp_df, aes(x=fpr, y=sens)) + 
           theme_bw(base_family = "Gill Sans", base_size = 14) +
           geom_line(size=1.3) + 
           geom_point(aes(x=fpr, y=sens, size=2), color="blue") +
           geom_text(aes(label=decision), hjust=1.5, vjust=-1, family="Gill Sans") +
           xlim(0,1.0) +
           ylim(0,1.0) +
           xlab("False Positive Rate (1-Specificity)") +
           ylab("Sensitivity") + 
           annotate("text", label=paste("AUC=", temp_auc, sep=" "), x=0.8, y=0.2, family = "Gill Sans") +
           theme(legend.position="none")

}

shinyServer(
    function(input, output) {
        # create distribution plot
        output$newPlot <- renderPlot({
            draw_distribution_plot(input$mu_0,
                      input$sd_0,
                      input$mu_a,
                      input$sd_a,
                      input$lin,
                      input$rev_order)
        })
        
        # create table
        output$table <- renderTable({ create_confusion_table(input$mu_0,
                                                             input$sd_0,
                                                             input$mu_a,
                                                             input$sd_a,
                                                             input$lin) })
        
        
        # create ROC curive with values
        output$ROC_plot <- renderPlot({
            create_ROC_curve(input$mu_0,
                             input$sd_0,
                             input$mu_a,
                             input$sd_a,
                             input$lin)
        })
})