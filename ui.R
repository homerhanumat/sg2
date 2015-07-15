library(shiny)
library(rhandsontable)

# no more than this many sims at any one time
simLimit <- 10000

navbarPage(
  title = "Chi-Square Test for Association",
  tabPanel(
    title = "The Test",
fluidPage(
  sidebarPanel(
    conditionalPanel(
      condition = "output.state == 'tableSetup'",
    numericInput(inputId ="rows", "Number of rows", min = 2, max = 5, 
                 step = 1, value = 2),
    helpText("Enter row names (separated by commas):"),
    textInput(inputId = "rowNames","Row Names",
              value = "cool,warm"),
    numericInput(inputId ="cols", "Number of columns", min = 2, max = 5, 
                 step = 1, value = 2),
    helpText("Enter column names (separated by commas):"),
    textInput(inputId = "colNames","Column Names",
              value = "baiting,polite"),
    actionButton(inputId = "submitTable", "Submit the Table")
    ),
    conditionalPanel(
      condition = "output.state == 'simSetup'",
      radioButtons(inputId = "simMethod", "Simulation Method",
                   choices = c("row/col sums fixed" = "rcFix",
                               "row sums fixed" = "rFix",
                               "neither fixed" = "nFix"))
    ),
    conditionalPanel(
      condition = "output.state != 'tableSetup'",
      numericInput(inputId = "numberSims", "Number of Simulations",
                   min = 1, max = simLimit, value = 1),
      actionButton(inputId = "sim", "Simulate Now"),
      actionButton(inputId = "reset", "Start Over (Same Table)")
    ),
    conditionalPanel(
      condition = "output.state != 'tableSetup'",
      actionButton(inputId = "newTable", "Make New Table")
    )
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.submitTable == 0 || output.state == 'tableSetup'",
      rHandsontableOutput("cross")
    ),
    conditionalPanel(
      condition = "output.state == 'simSetup'",
      plotOutput("mosaicInitial", height = 350),
      fluidRow(
        column(3,
               h5("Observed"),
               tableOutput("obsTable")
        ),
        column(3,
               h5("Expected by Null"),
               tableOutput("expTable")
        ),
        column(3,offset=1,
               h5("Contributions"),
               tableOutput("contrTable")
        )
      ),
      hr(),
      h5(textOutput("remarksInitial"))
    ),
    conditionalPanel(
      condition = "output.state == 'simulating'",
      tabsetPanel(
        tabPanel(
          title = "Latest Simulation",
          plotOutput("mosaicLatest", height = 350),
          fluidRow(
            column(4,
                  h5("Simulated Table"),
                  tableOutput("latestTable")
            ),
            column(4,offset=2,
                  h5("Expected Table"),
                  tableOutput("latestExpTable")
            )
          ),
          p(textOutput("remarksLatest1")),
          tableOutput("summary1"),
          p(textOutput("remarksProbBar"))
        ),
        tabPanel(
          title = "Density Plot",
          plotOutput("densityplot"),
          p(textOutput("remarksLatest2")),
          tableOutput("summary2"),
          p(textOutput("remarksProbDensity"))
        ),
        tabPanel(
          title = "Curve",
          plotOutput("chisqCurve"),
          br(),
          splitLayout(
            checkboxInput("compareDen",
                          HTML("Compare with simulated <br>chi-square distribution")),
            checkboxInput("yates","Use Yates correction")
          ),
          p(textOutput("remarksProb"))
        ),
        id = "tabs"
      )
    )
  )
)
),# end tabPanel "The Test"
tabPanel(
  title = "About"
)

) #nd navbarPage