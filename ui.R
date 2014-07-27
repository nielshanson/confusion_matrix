library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("De-confusing the Confusion Table"),
    sidebarPanel(
        # markup
        h2('Parameters'),
        h5('Null Distribution'),
        numericInput('mu_0', 'Null Mean', 0, min=-100, max=100, step=0.2),
        numericInput('sd_0', 'Null Standard Deviation', 1, min=0.1, max=100, step=0.1),
        h5('Alternative Distribution'),
        numericInput('mu_a', 'Alternative Mean', 2, min=-100, max=100, step=0.2),
        numericInput('sd_a', 'Alternative Standard Deviation', 1, min=0.1, max=100, step=0.2),
        h5('Classifier'),
        numericInput('lin', 'Decision Boundary', 1, min=-100, max=100, step=0.2),
        h5('Other Options'),
        checkboxGroupInput("rev_order", "Reverse Null and Alternative", c("Yes" = "yes")),
        submitButton('Submit')
        ),
    mainPanel(
        h3('Comparing Distributions'),
        p('Here we compare two Normal distributions based on the paramters that you supplied. We
          also show in the graph the relative areas of correct outcomes, True Positives (TP) and 
          True Negatives (TN),  and incorrect outcomes, Type-1 Errors or False Positives (FP) and 
          Type-2 Errors or False Negatives (FN).'),
          p(em("This figure plots the Null and Alternative distributions according to their current
                paramters and decision boundary. Try changing the mean, standard deveations,
                and the decision boundary to update the figures and tables below.")),
#         h4('You entered'),
#         verbatimTextOutput('oid1'),
#         h4('You entered'),
#         verbatimTextOutput('oid2'),
#         h4('You entered'),
#         verbatimTextOutput('odate'),
#         code('some code'),
#         p('Some ordinary text.'),
#         h4('You entered'),
#         verbatimTextOutput('inputValue'),
#         h4('Which resulted in a prediction of '),
#         verbatimTextOutput('prediction'),
          plotOutput('newPlot'),
          h4('Performance Evaluation with Confusion Tables'),
          p(withMathJax("A common task in statistics and in particular the increasingly popular area of
                       Machine Learning is the assessment of a binary classifier like the simplisitic
              one above. However, performance is often a balance between the two error types
              and can be somewhat multi-faceted depending on the potential application concerns
              of the classifier.")),
          p(em("This table shows the areas and derived performance statistics of the current 
               distributions and decision boundary above.")),
          p(strong(("Warning: Doesn't work properly when Null and Alternative are reversed."))),
          tableOutput("table"),
          h4('Performance Evaluation with Reciever Operating Characteristic Curves (ROC-Curve)'),
          p('One common way of evaluating the overall effectiveness of a binary classifer 
             accross decision boundaries is via an ROC curve. The True Positive Rate Specificity 
             is plotted against the False Positive Rate (1-Sensitivity). Classifiers with curves
             that plot closer to the upper left-hand corner are considered better (a random classifer
             is a diagonal line). One statistic to assess a classifier\'s performance here is the Area
             Under the Curve. Here this esimate is generated using the Trapezoidal Rule.'),
          p(em("The plot below generates the ROC curve for the current classifier one point at a time. 
               Alter the decision boundary while keeping all other settings constant and the ROC curve
               will be generated between the ROC points currently calculated. Changing the distribution
               parameters will reset the plot.")),
          p(strong(("Warning: Doesn't work properly when Null and Alternative are reversed."))),
          plotOutput('ROC_plot')#,
#           h4('Discussion of Confusion Table Statistics'),
#           p(withMathJax("A variety of statistics exist to summarize the performance of a binary classifer based on its goals. 
#                          In particular is usually a tradoff between the cost of making a particular type of error (False 
#                          Positives or False Negatives). Its best to discuss each of these statistics separately: 
#                          $$\\mbox{True Positives (TP)} \\approx P(T|T) = P(H_a|H_a)$$
#                         
#                         "))
    )
))