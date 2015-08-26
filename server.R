library(shiny)
library(httr)
library(rjson)
library(RCurl)
library(googleVis)
library(dplyr)

load("ig_oauth_ia")
token <- ig_oauth_ia$token

shinyServer(
    function(input, output) {
        # Show progress bar while loading everything ------------------------------
        
        #progress <- shiny::Progress$new()
        #progress$set(message="Loading maps/data", value=0)

        # Use search function to search for username (and get id)
        df <- reactive({
            input$goButton
            user_name <- isolate(input$user)
            
            user_search <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=', 
                                                 user_name,'&access_token=', token, sep="")), 
                                    unexpected.escape = "keep")
            
            # Select the first result of the search and store user id in variable
            first_profile <- user_search$data[[1]]
            user_id <- first_profile$id
            
            ## Get infos about user
            user_info = fromJSON(getURL(paste('https://api.instagram.com/v1/users/', 
                                              user_id,'/?access_token=', token, sep="")), 
                                 unexpected.escape = "keep")
            count_media <- user_info$data$counts$media
            
            # Get recent media (20 pictures)
            media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/', 
                                           user_id, '/media/recent/?access_token=', token, 
                                           sep="")))
            
            # Data frame returning the pictures/vids & infos about them
            df <- data.frame(no = 1:length(media$data))
            for (i in 1:length(media$data))
            {
                #id
                df$id[i] <- media$data[[i]]$id
                #link
                df$link[i] <- media$data[[i]]$link
                #lat
                df$lat[i] <-toString(media$data[[i]]$location$latitude)
                #long
                df$long[i] <- toString(media$data[[i]]$location$longitude)
                #date
                df$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), 
                                                  origin="1970-01-01")) 
            }
            
            # Only keep media with geotags
            df <- filter(df, lat != "")
            
            # Create latlong variable for gvisMap
            df$latlong <- paste(df$lat, df$long, sep = ":")
            df
        })
        
        
          output$view <- renderGvis({
              gvisMap(df(), locationvar = "latlong" , tipvar = "date", 
                      options=list(showTip=TRUE, 
                                   showLine=TRUE, 
                                   enableScrollWheel=TRUE,
                                   mapType='terrain', 
                                   useMapTypeControl=TRUE))
          })
        
        # Turn off progress bar ---------------------------------------------------
        
        # progress$close()
        
})


