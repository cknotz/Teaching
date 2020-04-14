#######################################################
# This is Part 1 of the Tragedy of the Commons ShinyApp
#######################################################

# This is the entry form through which students can 
# claim their part of the overall prize. Their names
# or pseudonyms and their claims are stored in a
# GoogleSheet that can later be accessed via the 
# second application.


library(shiny)
    library(shinyjs)
    library(rsconnect)
    library(googlesheets4)

sheets_auth(
    cache = ".secrets",
    email = "<GoogleAccount>@gmail.com"
)

# This is the link to the GoogleSheet
sheet <- "<insert-direct-link-to-GoogleSheet>"
    


# Define UserInterface
ui <- fluidPage(
    shinyjs::useShinyjs(),
    p(strong("Instructions")),
    p("You and your classmates need to divide 100 gold coins (or, if you consider that lame, something else you care about) between yourselves."),
    p("You decide individually how many coins you want to take out."),
    p("You can choose to take out nothing (0), everything (100) or any amount in between."),
    p("Keep in mind:"),
    tags$ul(
        tags$li("If all of you combined take out more than is available, no regular payout will take place."),
        tags$li("But: The three of you who claim the highest amounts will receive their claims, no matter what happens."),
    ),
    p(strong("Take a moment to think about what everyone else will probably do and, given this, what your best strategy is.")),
    hr(),
    fluidRow(
        column(5,style = "margin-top: 20px;", textInput(inputId="name",label="How should we call you?",value="",placeholder = "Your name/pseudonym")),
        column(2, numericInput(inputId="value",label="How many coins will you take?",value = 0,min = 0,max = 100)),
        column(2,style = "margin-top: 45px;",actionButton(inputId="submit",label="Submit"))
            )
)


# Define server
server <- function(input, output) {
   values <- reactiveValues()
    values$df <- data.frame(matrix(ncol = 2,nrow=0))
    add_data <- observeEvent(input$submit,{
        newline <- isolate(c(input$name,input$value))
        isolate(values$df <- rbind(as.matrix(values$df),unlist(newline)))
        sheets_append(as.data.frame(values$df),sheet)
        disable("submit")
    })
     observeEvent(input$submit, {
       insertUI(
           selector = "#submit",
           where="afterEnd",
           ui=p((strong("Thank you! Your choice has been submitted. You can now close this tab.")))) 
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
