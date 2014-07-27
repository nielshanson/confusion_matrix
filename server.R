library(shiny)
library(ggplot2) 
library(flux) # for Trapezoidal rule ROC calculation

# Initialize ROC environment variables
roc.env <- new.env()
roc.env$ROCvalues_x <- NULL # xvalues (FDR)
roc.env$ROCvalues_y <- NULL # y values (sensitivity)
roc.env$ROClin <- NULL # decision boundary (i.e., line)
roc.env$ROC_param <- NULL # current Gaussian parameter sets
roc.env$ROC_rev <- NULL # reverse parameter sets

#' Draw Hypothesis-testing Gaussian Plot
#' 
#' Function takes the Null and Alternative mean and standard deviations, fits Guassian
#' distributions and plots the density using 1000 interpolated points each. Areas
#' of the density are highlighted based on the linear classifier line. If rev_order
#' swaps the roles of Null and Alternative hypotheses.
#'
#' @param mu_0 mean of the Null Gaussian distribution
#' @param sigma_0 standard deviation of the Null Gaussian distribution
#' @param mu_0 mean of the Alternative Gaussian distribution
#' @param sigma_0 standard deviation of the Alternative Gaussian distribution
#' @param lin x value that will be used as the decision boundary
#' @param rev_order if not NULL will reverse the roles of Null and Alternative distributions
draw_distribution_plot <- function(mu_0, sigma_0, mu_a, sigma_a, lin, rev_order) {
    
    # calculate the overall range to draw the gaussians
    temp_range <- c(mu_0-(4*sigma_0), mu_0+(4*sigma_0), 
                    mu_a-(4*sigma_a), mu_a+(4*sigma_a))
    
    # 1000 x values to evaluate
    xvals <- seq(min(temp_range), max(temp_range), length = 1000)
    
    # calculate y1 values for each of the densities
    if (is.null(rev_order)) {
        # standard Null left Alternative right arrangement
        y1 <- dnorm(xvals, mean = mu_0, sd = sigma_0)
        y2 <- dnorm(xvals, mean = mu_a, sd = sigma_a)
    } else {
        # reverse null and alternative
        y2 <- dnorm(xvals, mean = mu_0, sd = sigma_0)
        y1 <- dnorm(xvals, mean = mu_a, sd = sigma_a)
    }
    
    # label sections of the points as their performance class
    # relative to lin
    if (is.null(rev_order)) {
        # Null Left, Alternative Right
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
        # reverse
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
    
    # create dataframe of the x and y values witht their labels
    df <- data.frame(x=c(xvals,xvals), y=y, dist=dist, categories=categories)
    
    # create the plot
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
    
    # add annotations depending on order
    if (is.null(rev_order)) {
        g_plot <- g_plot + 
                  annotate("text", label=paste("H[0]"), parse=TRUE, x = mu_0, y = max(y1) + 0.025, size = 8, colour = "black") +
                  annotate("text", label=paste("H[A]"), parse=TRUE, x = mu_a, y = max(y2) + 0.025, size = 8, colour = "black")
    } else {
        g_plot <- g_plot + 
            annotate("text", label=paste("H[A]"), parse=TRUE, x = mu_0, y = max(y2) + 0.025, size = 8, colour = "black") +
            annotate("text", label=paste("H[0]"), parse=TRUE, x = mu_a, y = max(y1) + 0.025, size = 8, colour = "black")
    }

    g_plot # return the final plot
    
}


#' Calculate Confusion Table
#' 
#' Based on the current parameters of the Null and Alternative distributions
#' and the decision boundary, calculates a Confusion Table of performance 
#' statistics.
#'
#' @param mu_0 mean of the Null Gaussian distribution
#' @param sigma_0 standard deviation of the Null Gaussian distribution
#' @param mu_0 mean of the Alternative Gaussian distribution
#' @param sigma_0 standard deviation of the Alternative Gaussian distribution
#' @param lin x value that will be used as the decision boundary
#' @param rev_order If not null reverse the role of Null and Alternative
create_confusion_table <- function(mu_0, sigma_0, mu_a, sigma_a, lin, rev_order) {
    
    # calculate areas of TP, TN, FP, FN to 4 digits
    if(is.null(rev_order)) {
        TP_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=FALSE), 4)
        FN_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=TRUE), 4)
        TN_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=TRUE), 4)
        FP_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=FALSE), 4)
    } else {
        # swaped order
        TP_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=TRUE), 4)
        FN_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=FALSE), 4)
        FP_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=TRUE), 4)
        TN_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=FALSE), 4)
    }
    
    # calcualte confusion table statistics
    sens <- TP_prob / (TP_prob + FN_prob) # sensitivity
    spec <- TN_prob / (TN_prob + FP_prob) # specificity
    ppv <- TP_prob / (TP_prob + FP_prob) # positive predictive value
    npv <- TN_prob / (TN_prob + FN_prob) # negative predictive value
    
    # calculate accurancy
    acc <- (TP_prob + TN_prob) / (TP_prob + FN_prob + TN_prob + FP_prob)
    
    # create confustion table to display results
    confusion_table = data.frame(mu=c(TP_prob,FN_prob, sens), sd=c(FP_prob, TN_prob, spec), c(ppv, npv, acc))
    rows <- c("Test_T", "Test_F", "Sensitivity / Specificity")
    cols <- c("Observed_T", "Observed_F", "Precission (PPV) / Negative Predictive Value (NPV) / Accuracy")
    row.names(confusion_table) <- rows
    colnames(confusion_table) <- cols
    
    confusion_table # return confusion table to display
}

#' Calculate ROC Curve
#' 
#' Based on the current parameters of the Null and Alternative distributions
#' calculates the ROC Curve in a piece wise way. Checks to see if value of lin
#' is new before calcuating. If parameters have changed, resets the ROC curve.
#'
#' @param mu_0 mean of the Null Gaussian distribution
#' @param sigma_0 standard deviation of the Null Gaussian distribution
#' @param mu_0 mean of the Alternative Gaussian distribution
#' @param sigma_0 standard deviation of the Alternative Gaussian distribution
#' @param lin x value that will be used as the decision boundary
#' @param rev_order If not null reverse the role of Null and Alternative
create_ROC_curve <- function(mu_0, sigma_0, mu_a, sigma_a, lin, rev_order) {
    
    # check to see if the distributions changed
    parameters <- c(mu_0, sigma_0, mu_a, sigma_a)
    if (is.null(roc.env$ROC_param)) {
        roc.env$ROC_param <- parameters
    }
    
    # some parameter changed, reset the ROC values
    if ( (sum(roc.env$ROC_param == parameters) != length(parameters)) |
             is.null(rev_order) != is.null(roc.env$ROC_rev)) {
        roc.env$ROCvalues_x <- NULL
        roc.env$ROCvalues_y <- NULL
        roc.env$ROClin <- NULL
        roc.env$ROC_param <- NULL
        roc.env$ROC_rev <- rev_order
    }
    
    # check to see if current lin boundary is new
    if( !(lin %in% roc.env$ROClin) ){
        # new lin: calculate ROC values
        if (is.null(rev_order)) {
            TP_prob = pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=FALSE)
            FN_prob = pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=TRUE)
            TN_prob = pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=TRUE)
            FP_prob = pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=FALSE)
        } else {
            # swaped order
            TP_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=TRUE), 4)
            FN_prob = round(pnorm(lin, mean=mu_0, sd=sigma_0, lower.tail=FALSE), 4)
            FP_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=TRUE), 4)
            TN_prob = round(pnorm(lin, mean=mu_a, sd=sigma_a, lower.tail=FALSE), 4)
        }
        
        # calculate sensitivity, specificity, and false positive rate
        sens <- TP_prob / (TP_prob + FN_prob)
        spec <- TN_prob / (TN_prob + FP_prob)
        fpr <- 1 - spec # false postive rate
        
        # append results to current ROC values
        roc.env$ROCvalues_x <- c(roc.env$ROCvalues_x, round(fpr,4))
        roc.env$ROCvalues_y <- c(roc.env$ROCvalues_y, round(sens,4))
        roc.env$ROClin <- c(roc.env$ROClin, lin)
    }
    
    # create temporary dataframe to plot
    temp_df <- data.frame(fpr=roc.env$ROCvalues_x, sens=roc.env$ROCvalues_y, decision=roc.env$ROClin)
    # calculate ROC using the default (Trapezoidal rule)
    if(length(temp_df$fpr) > 1) {
        temp_auc <- round(auc(temp_df$fpr, temp_df$sens),4)
    } else {
        temp_auc <- 0 # corner case
    }
    
    # create plot
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
        # reset ROC environment for new user
        roc.env$ROCvalues_x <- NULL # x,y,lin values of the ROC curve
        roc.env$ROCvalues_y <- NULL
        roc.env$ROClin <- NULL
        roc.env$ROC_param <- NULL
        roc.env$ROC_rev <- NULL
        
        # create distribution plot
        output$newPlot <- renderPlot({
            draw_distribution_plot(input$mu_0,
                      input$sd_0,
                      input$mu_a,
                      input$sd_a,
                      input$lin,
                      input$rev_order)
        })
        
        # create confusion table
        output$table <- renderTable({ create_confusion_table(input$mu_0,
                                                             input$sd_0,
                                                             input$mu_a,
                                                             input$sd_a,
                                                             input$lin,
                                                             input$rev_order) })
        
        
        # create ROC curive with values
        output$ROC_plot <- renderPlot({
            create_ROC_curve(input$mu_0,
                             input$sd_0,
                             input$mu_a,
                             input$sd_a,
                             input$lin,
                             input$rev_order)
        })
})