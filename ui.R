library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Data Science for the Win!"),
    sidebarPanel(
        # markup
        h1('Sidebar panel'),
        h1('H1 Text'),
        h2('H2 Text'),
        h3('H3 Text'),
        h4('H4 Text'),
        numericInput('id1', 'Numeric input, labeled id1', 0, min = 0, max = 10, step =1),
        checkboxGroupInput("id2", "Checkbox",
                           c("Value 1" = "1",
                             "Value 2" = "2",
                             "Value 3" = "3")),
        dateInput("date", "Date:"),
        numericInput('mu_0', 'Null Mean', 0, min=-100, max=100, step=0.2),
        numericInput('sd_0', 'Null Standard Deviation', 1, min=0.1, max=100, step=0.1),
        numericInput('mu_a', 'Alternative Mean', 2, min=-100, max=100, step=0.2),
        numericInput('sd_a', 'Alternative Standard Deviation', 1, min=0.1, max=100, step=0.2),
        numericInput('lin', 'Decision Boundary', 1, min=-100, max=100, step=0.2),
        checkboxGroupInput("rev_order", "Reverse Null and Alternative", c("Yes" = "yes")),
        submitButton('Submit')
        ),
    mainPanel(
        h3('Comparing Distributions'),
        p('Here we compare two Normal distributions based on the paramters that you supplied. We
          also show in the graph the relative areas of correct outcomes, True Positives (TP) and 
          True Negatives (TN),  and incorrect outcomes, Type-1 Errors or False Positives (FP) and 
          Type-2 Errors or False Negatives (FN).'),
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
          # TODO Put significance and power
          h4('Performance Evaluation with Confusion Tables'),
          p(withMathJax("A common task in statistics and in particular the increasingly popular area of
                       Machine Learning is the assessment of a binary classifier like the simplisitic
                       one above. However, performance is often a balance between the two error types
                       and can be somewhat multi-faceted depending on the potential application concerns
                       of the classifier. Thus, it is convenient to summarize the performance of a classifer
                       with a number of different statistics in what is commonly called a Confusion Table 
                       shown below: $$a + b$$")),
          tableOutput("table"),
          withMathJax("Some math here $$\\alpha+\\beta$$")
    )
))