#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rclipboard)

demo_text <- "Text mit Fehlern hier hinkopieren!"
demo_text_out <- "Text ohne Fehler hier rauskopieren!"

# Define UI for application that draws a histogram
ui <- fluidPage(
    rclipboardSetup(),
    # Application title
    titlePanel("Shiny Umlauts"),
   
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("PDF Inhalte in Powerpoint zu kopieren funktioniert leider nie reibungslos. Diese App entfernt zumindest etwas Müll."),
            textAreaInput("textIn", "Text hier hin kopieren:", value = NULL, width = NULL, height = "400px", placeholder = demo_text)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Formatted output"),
            actionButton("clipbtn", "Kopieren", icon = icon("clipboard")),
            textAreaInput("textOut", "Korrigierter Text:", width = "100%", height = "200px", placeholder = demo_text_out), #, placeholder = FALSE)
            #rclipButton("clipbtn", "rclipButton Copy", "Test" , icon("clipboard")),
            
            verbatimTextOutput("verbout", placeholder = FALSE),
            h3("Raw output"),
            checkboxInput("showRaw", "Show raw hex output?", value = FALSE),
            verbatimTextOutput("rawout", placeholder = FALSE)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    replaceCRs <- function(x) {
        
    }
    
    replaceWeirds <- function(x) {
        # Avoid touching these
        x %>% 
            str_replace_all("", "") %>% 
            str_replace_all("ü", "ü") %>% 
            str_replace_all("•", "") %>% 
            str_replace_all("", "") %>% 
            str_replace_all("–", "") %>% 
            str_replace_all("", "") 
         
        
        
    }
    
    fixMissingSpaces <- function(x){
        str_replace_all(x, "([a-z])([A-Z])", "\\1 \\2")
    }
    
    fixHeadingSpaces <- function(x){
        str_replace_all(x, "^[ ]*(.)", "\\1") %>% 
        str_replace_all("\n[ ]*(.)", "\n\\1")
    }
    
    toRaw <- function(x) {
        x %>% charToRaw()
    }
    
    observe({
        # We'll use the input$controller variable multiple times, so save it as x
        # for convenience.
        x <- input$textIn
        
        # This will change the value of input$inText, based on x
        updateTextAreaInput(session, "textOut", 
                            value = x %>% 
                                fixMissingSpaces() %>% 
                                replaceWeirds() %>% 
                                fixHeadingSpaces())
    })
    
        
    
    
    #output$verbout <- renderText(input$textIn %>% fixMissingSpaces())
    output$rawout <- renderText({
        if(input$showRaw)
            input$textIn %>% toRaw()
        else 
            NULL
        
        })
    if (interactive()){
        observeEvent(input$clipbtn, clipr::write_clip(input$textOut))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
