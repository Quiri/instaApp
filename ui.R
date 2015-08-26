library(shiny)

# Define UI for application
shinyUI(fluidPage(
    titlePanel("Instagram Geo Tagging"),
    sidebarLayout(
        sidebarPanel(
            textInput("user", label = h3("Enter username:")), 
            actionButton("goButton", "Go!", styleclass = "primary"),
            br(),
            br(),
            p("If you run out of inspiration, here are some suggestions:"),
            tags$ol(
                tags$li("mo.gaff"), 
                tags$li("cpp2412"), 
                tags$li("barackobama"),
                tags$li("parishilton")
            )
        ),
        
        mainPanel(
            h3("About"),
            p("This application returns a google map with geotags for the last 
20 media (pictures & videos) of the user whose username you entered in the right 
Box. To do this, the app retrieves the media a user posted on instagram and only 
keeps the posts with geolocation data (longitude & latitude )"), 
            
            conditionalPanel(
              condition = "input.goButton > 0",
                htmlOutput("view"),
                h3("Here you go:")
            ),
            
            h3("Disclaimer"),
            p("In order to establish a connection to the instagram API, a token 
              from a registered developer is needed. For the sake of simplicity, 
              I put my own token. So please, don´t share or abuse as I will be 
              hold responsible ;)")
    )
)))