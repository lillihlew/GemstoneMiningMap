#Lilli Lewis Midterm


#Load necessary libraries
library(shiny)
library(tidyverse) 
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(geojsonio)
library(htmltools)

#Read in data frame
gemstones_clean<-read.csv("clean_data.csv")

#Make subsets
just_gemstones <- subset(gemstones_clean, commodity == "gemstone " |commodity 
                         == "gemstone" | commodity == "garnet" | commodity == 
                             "amber" | commodity == "opal" | 
                             extra_details_gemstones == "ore, zircon" | commodity 
                         == "zirconmium" | commodity == "quartz" | commodity == 
                             "quartz and quartzite" | commodity == "kyanite" | 
                             commodity == "labradorite" | commodity == "diamond")
precious_metals <- subset(gemstones_clean, commodity == "gold" | 
                              commodity == "silver" | commodity == "copper"| 
                              commodity == "titanium"| commodity == "titanium, 
                          rutile" | fac_name == "Ust-Kamenogorsk 
                          titanium-magnesium plant" | commodity == "titanium, 
                          ilmenite" | commodity == "titanium, zirconium" | 
                              commodity == "titanium, zirconium, iron" | 
                              commodity == "zirconium, titanium" | commodity == 
                              "rhenium" | commodity == "copper, zinc, palladium" 
                          | commodity == "platinum" | commodity == "platinum 
                          group metals" | commodity == "silver" | commodity == 
                              "silver" | commodity == "gold and silver" | 
                              commodity == "lead, silver, and zinc" | commodity 
                          == "copper, gold, and silver" | commodity == "zinc, 
                          gold, and silver" |  position == "116" |  position == 
                              "118" |  position == "136" |  position == "137" |  
                              position == "138" |  position == "170" |  position 
                          == "177" |  position == "180" |  position == "202" | 
                              position == "205" |  position == "206" |  position 
                          == "207" |  position == "208" |  position == "209" | 
                              position == "210" |  position == "234" |  position 
                          == "282" |  position == "285" |  position == "287" | 
                              position == "289" |  position == "292" |  position 
                          == "293" |  position == "294" |  position == "295" | 
                              position == "296" |  position == "645" |  position 
                          == "81" |  position == "853" |  position == "854" |  
                              position == "884" |  position == "924" |  position 
                          == "1071" |  position == "1342" |  position == "1481" 
                          | commodity == "gold" |  position == "15" |  position 
                          == "19" |  position == "121" |  position == "121" |  
                              position == "122" |  position == "126" |  position 
                          == "127" |  position == "129" |  position == "131" | 
                              position == "132" |  position == "134" |  position 
                          == "136" |  position == "137" |  position == "220" | 
                              position == "232" |  position == "366" |  position 
                          == "385")

clay_and_cement <- subset(gemstones_clean, commodity == "clay" | commodity == "cement" | commodity == "cement clinker")

gas_and_petroleum <- subset(gemstones_clean, commodity == "gas" | commodity == 
                                "petroleum" | commodity == "natural gas" | commodity == 
                                "natural gas" | commodity == "petroleum and natural gas" 
                            | commodity == "oil shale" | commodity == "natural gas and 
                     petroleum" | commodity == "liquified petroleum gas" | 
                                commodity == "liquefied natural gas" | commodity == 
                                "natural gas liquids" | commodity == "petroleum gas" | 
                                commodity == "methanol")



# Define UI ----
ui <- fluidPage(
    
    #Title
    titlePanel("World's Mining Facilities"),
    
    #Make the program adjust to the user's browser window size
    fluidRow(
        
        column(3, 
               #Make commodity radio buttons
               radioButtons("radio", 
                            h3("Commodity"), 
                            choices = list("Gemstones" = 1, 
                                           "Precious Metals" = 2, 
                                           "Clay and Cement" = 3,
                                           "Oil and Petroleum" = 4),
                            selected = 1)),
        
        column(3, 
               #Make country radio buttons
               radioButtons("radio2", 
                            h3("Country Filter?"), 
                            choices = list("No" = 1, 
                                           "Yes" = 2
                            ),
                            selected = 1)),
        
        column(3,
               #Make status checkboxes
               checkboxGroupInput("checkGroup", h3("Facility Status"),
                                  choices = list("Active" = 1, "Closed" = 2),selected = 1)),
        
        column(3,
               #Make country drop-down checklist
               selectInput("select", h3("Country"), 
                           choices = unique(gemstones_clean$country))),
        
        #Make map
        leafletOutput("map_gemstones")
    )       
)



# Define server logic ----
server <- function(input, output) {
    #Tell the server that I want the output to be a map
    output$map_gemstones <- renderLeaflet({ 
        
        #Connect commodity radio buttons to their respective subsets
        df<-if(input$radio== 1){
            just_gemstones
        }else if(input$radio== 2){
            precious_metals
        } else if (input$radio== 3){
            clay_and_cement
        } else {gas_and_petroleum
        }
        
        
        #Make status check boxes filter the status section  
        df<-if(input$checkGroup == c(1, 2)){
            df
        }else if(input$checkGroup == 2){
            filter(df, status !="active")
        }else{
            filter(df, status =="active")
        }
        
        
        #Connect country radio buttons to the country drop down selector and filter the countries based on that
        df<-if(input$radio2 == 1){
            df
        }else{
            filter(df, country == input$select)
        }
        
        
        #Make a map that will work with the new dataframe (df) that I made in this server
        map_gemstones <- leaflet(data = df) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude, lat = ~latitude, 
                       label = ~paste(commodity, fac_type),
                       clusterOptions = markerClusterOptions())
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)

