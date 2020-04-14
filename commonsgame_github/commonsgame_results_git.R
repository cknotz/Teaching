#######################################################
# This is Part 2 of the Tragedy of the Commons ShinyApp
#######################################################

# This app displays the results, indicating cooperators
# and defectors and allows the instructor to reset the
# game by erasing all values from the GoogleSheet that
# stores students' claims.

library(shiny)
    library(dplyr)
    library(rsconnect)
    library(googlesheets4)
    
# Authentication
sheets_auth(
    cache = ".secrets",
    email = "<GoogleAccount>@gmail.com"
)

# This is the link to the GoogleSheet
sheet <- "<insert-direct-link-to-GoogleSheet>"

# This creates the blank sheet used for the expunge
lab <- c(" "," ")
    ex <- data.frame(matrix(nrow=1,data=lab))
    rm(lab)
    

ui <- fluidPage(
    titlePanel("Results"),
    hr(),
    fluidRow(
        column(8,
        p(strong("Context:")),
        p("The overall number of coins was 100."),
        numericInput(inputId = "size",label="The number of students in this class is:",10,min=1,step=1),
        textOutput("textout1")
        ),
        column(4,
        actionButton(style = "margin-top: 120px;", inputId = "fetch","Fetch/refresh results",icon("arrow-circle-down",lib="font-awesome"))
               )),
    hr(),
    fluidRow(
        column(12,
        p(strong("What actually happened:")),
        textOutput("textout2")
        )),
    hr(),
    fluidRow(
        column(4,
            p(strong("Everyone")),
            tableOutput('table')),
        column(4,
            p(strong("Cooperators")),
            tableOutput('coop')),
        column(4,
            p(strong("Defectors")),
            tableOutput('defe'))),
    hr(),
    mainPanel(
        actionButton(inputId = "expunge","Delete data",icon("exclamation-circle",lib="font-awesome"))
        ),
)


server <- function(input, output) {
    # Computes figures for context
    vals <- reactiveValues()
    observe({
        vals$size <- input$size
        vals$fair <- round(100/input$size,digits = 1)
    })
    output$textout1 <- renderText({
        paste("If everything were divided equally, each student could have gotten",vals$fair,"coins.")
    })
    # This computes some relevant figures and triggers the output of data
    observeEvent(input$fetch,{
        
    vals$data <- sheets_read(sheet,col_types = "ci")  
        vals$data <- na.omit(vals$data)
        vals$data <- rename(vals$data,name=X1)
        vals$data <- rename(vals$data,value=X2)
        
    vals$data <- vals$data[order(-vals$data$value),]
        vals$sum <- sum(vals$data$value,na.rm = T)
        vals$mean <- mean(vals$data$value,na.rm = T)
        vals$max <- max(vals$data$value,na.rm = T)
        
    vals$coop <- filter(vals$data,value<=100/vals$size)  
    vals$defe <- filter(vals$data,value>100/vals$size) 
        
    output$textout2 <- renderText({
        paste("The sum of all claims added together was:",vals$sum,"; the average claim was",vals$mean,"and the largest claim was",vals$max,".")
    })
    output$table <- renderTable(vals$data)
    # Cooperators
    output$coop <- renderTable(vals$coop)
    # Defectors
    output$defe <- renderTable(vals$defe)
    })
    # Expunge data in Google Sheet & reset game
    expunge <- observeEvent(input$expunge,{
        sheets_write(ex,sheet,sheet = "Tabellenblatt1") # <- you may have to adapt the name of the sheet!
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
