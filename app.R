#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

###added shinythemes package to change layout
#install.packages("shinythemes")
#install.packages("fresh")

library(shiny)
library(shinythemes)
library(fresh)
library(dipsaus)
library(shinyWidgets)

############Functions#########################################################

#action button to change the page
pageButtonUi <- function(id) {
  actionButton(NS(id, "page_change"),
               label = 'GO TO CALCULATOR', 
               style="background-color:#7AA95C; color:white")
}


#button function server
pageButtonServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$page_change, {
      updateNavbarPage(session=parentSession,
                       inputId="navbar",
                       selected="Calculator")
    })
  })
}




###########UI#############################################################


# Define UI for application that draws a histogram
ui <- fluidPage(
          titlePanel(
    
  #navigation tabs: create the tabs and define the theme with css file
                      navbarPage(
                                  title = img(src="logo.png", 
                                              width=320, 
                                              hight= '40px', 
                                              style="padding-left:5px;"), 
                                        # picture has to go in www folder
                                  id = "navbar",
                                  selected = "Home",
                                  position = "static-top",
                                  theme = "styles.css",
                                  fluid = T,
                      #Home page            
                              tabPanel("Home",
                                      fixedRow(
                                          column(width= 6,
                                                h2("Make sustainable choices.", 
                                                align="left",
                                                style="padding-top:200px;"),
                                                
                                                h4("Calculate the CO2 footprint of your meal now 
                                                    and get recommendations on how to improve 
                                                    sustainability in your daily nutrition.", 
                                                align="left",
                                                style="padding-top:40px; padding-right:80px;"
                                                ),
                                        
                                        #Button to calculater page
                                            pageButtonUi("navbar"),
                                          
                                         
                                        ),
                                        
                                        #Picture on the side
                                          column(width = 6),
                                              tags$figure(
                                                align = "right",
                                                tags$img(
                                                  src = "home.jpg",
                                                  width = "50%",
                                                  height = "auto"
                                                    )
                                              )
                                 
                                            )
                                      ),
                      
                        #Calculator page
                                  tabPanel("Calculator", 
                                           fluidRow(
                                             tags$head(
                                               tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                             #tags$style("[type = 'text'] {font-size:10px;height:20px;}"),
                                             column(width = 8,
                                                    h2("Type your ingredients here.",
                                                       style="padding-top:50px;"
                                                       ),
                                                    
                                                    column(width = 2,
                                                           textInput("textin", "Ingredients", 
                                                                     placeholder = "e.g. carrots", 
                                                                     width = "100%"),
                                                           textOutput("Ing"),
                                                           textInput("textin2", "", 
                                                                     placeholder = "e.g. carrots", 
                                                                     width = "100%"),
                                                           textOutput("Ing2"),
                                                           textInput("textin3", "", 
                                                                     placeholder = "e.g. carrots", 
                                                                     width = "100%"),
                                                           textOutput("Ing3"),
                                                           textInput("textin4", "", 
                                                                     placeholder = "e.g. carrots", 
                                                                     width = "100%"),
                                                           textOutput("Ing4"),
                                                           textInput("textin5", "", 
                                                                     placeholder = "e.g. carrots", 
                                                                     width = "100%"),
                                                           textOutput("Ing5"),
                                                           
                                                           #add button
                                                           
                                            
                                                           actionButton("add_btn", "Add Textbox")
                                                    ),
                                                    column(width = 2,
                                                           textInput("quantityin", "Qunatety", 
                                                                     placeholder = "e.g. 200", 
                                                                     width = "100%"),
                                                           textOutput("Qua"),
                                                           textInput("quantityin2", "", 
                                                                     placeholder = "e.g. 200",
                                                                     width = "100%"),
                                                           textOutput("Qua2"),
                                                           textInput("quantityin3", "", 
                                                                     placeholder = "e.g. 200", 
                                                                     width = "100%"),
                                                           textOutput("Qua3"),
                                                           textInput("quantityin4", "", 
                                                                     placeholder = "e.g. 200", 
                                                                     width = "100%"),
                                                           textOutput("Qua4"),
                                                           textInput("quantityin5", "", 
                                                                     placeholder = "e.g. 200", 
                                                                     width = "100%"),
                                                           textOutput("Qua5"),
                                                    ),
                                                    column(width = 2,
                                                           textInput("unitin", "Unit", 
                                                                     placeholder = "e.g. gr", 
                                                                     width = "100%"),
                                                           textOutput("Uni"),
                                                           textInput("unitin2", "", 
                                                                     placeholder = "e.g. gr", 
                                                                     width = "100%"),
                                                           textOutput("Uni2"),
                                                           textInput("unitin3", "", 
                                                                     placeholder = "e.g. gr", 
                                                                     width = "100%"),
                                                           textOutput("Uni3"),
                                                           textInput("unitin4", "", 
                                                                     placeholder = "e.g. gr", 
                                                                     width = "100%"),
                                                           textOutput("Uni4"),
                                                           textInput("unitin5", "", 
                                                                     placeholder = "e.g. gr", 
                                                                     width = "100%"),
                                                           textOutput("Uni5"),
                                                    ),
                                                    column(width = 2,
                                                           pickerInput(inputId = "varietyin", 
                                                                       label = "Variety", 
                                                                       choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "varietyin", 
                                                                       label = "", 
                                                                       choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "varietyin", 
                                                                       label = "", 
                                                                       choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "varietyin", 
                                                                       label = "", 
                                                                       choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "varietyin", 
                                                                       label = "", 
                                                                       choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE))
                                          
                                                    ),
                                                    column(width = 2,
                                                           pickerInput(inputId = "certificationin", 
                                                                       label = "Certification", 
                                                                       choices = c("organic","conventional", 
                                                                                   "swiss integrated production"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "certificationin", 
                                                                       label = "", 
                                                                       choices = c("organic","conventional", 
                                                                                   "swiss integrated production"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "certificationin", 
                                                                       label = "", 
                                                                       choices = c("organic","conventional", 
                                                                                   "swiss integrated production"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "certificationin", 
                                                                       label = "", 
                                                                       choices = c("organic","conventional", 
                                                                                   "swiss integrated production"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           pickerInput(inputId = "certificationin", 
                                                                       label = "", 
                                                                       choices = c("organic","conventional", 
                                                                                   "swiss integrated production"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           
                                                           actionButton('btn2',"CALCULATE", style="color:white; background-color:#7AA95C"
                                                           ),
                
                                                          )
                                                    
                                                    
                                             ),
                                             column(width = 4,
                                                    column(width = 12,
                                                    h2("Or paste the URL here."), 
                                                    textInput("textin", "URL",
                                                              placeholder = "e.g. www...", 
                                                              width = "100%"),
                                                    
                                                    #calculate button
                                                    actionButton('btn3', 
                                                                       label = 'CALCULATE', 
                                                                       style="background-color:#7AA95C; color:white"
                                                                      )
                                                          )
                                                    )
                                                    )
                                           ),
                        #Recommendations page
                                  tabPanel("Recommendations", 
                                           fluidRow(
                                             tags$head(
                                              tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                            column(width = 6,
                                                  h2("CO2 footprint of your recipe...",
                                                     style="padding-top:50px;"
                                                     ),
                                                  h4("Do you want to be more sustainable and learn more about 
                                                    food emissions? Look at the recommendations on the right 
                                                    side to lower your CO2 score, and browse to the next page
                                                    to have an interactive tool that lets you have some insights
                                                     about food emissions!", 
                                                     style="padding-top:50px"),
                                           ),
                                           column(width = 6,
                                                  h2("Our recommendations for you...",
                                                     style="padding-top:50px;"
                                                     ),
                                                  h4("Instead of...", align="center"),
                                                  h4("...change to",  align="center"),
                                                  h2("New CO2 score after the swaps:"),
                                                  
                                           ),
                                           )),
                                     
                      
                      
                        #Learn more page
                                  tabPanel("Learn more",
                                           fluidRow(
                                             tags$head(
                                               tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                          column(width = 6,
                                                 h2("Discover more about food emissions",
                                                    style="padding-top:50px;"
                                                 ),
                                                 h4("Type an ingredient and select from the drop down menus its variety and certification, and see how the score changes!", 
                                                    style="padding-top:40px; padding-right:0px;"),
                                                 column(width = 3,
                                                        textInput("textR", "Ingredients", 
                                                                  placeholder = "e.g. carrots", 
                                                                  width = "100%"),
                                                        textOutput("IngR"),
                                                        
                                                 ),
                                                 column(width = 3,
                                                        pickerInput(inputId = "varietyR", 
                                                                    label = "Variety", 
                                                                    choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                    selected = "choose", 
                                                                    options = list(`live-search` = TRUE)),
                                                 ),
                                                 column(width = 3,
                                                        pickerInput(inputId = "certificationR", 
                                                                    label = "Certification", 
                                                                    choices = c("organic","conventional", "swiss integrated production"), 
                                                                    selected = "choose", 
                                                                    options = list(`live-search` = TRUE)),
                                                 ),
                                                 column(width = 3,
                                                        #placeholder for CO2 score
                                                        textOutput("C02"),
                                                 ),
                                                 column(width = 12,
                                                        h4("See the overview about the ingredient’s relative CO2 scores
                                                       based on variety and certification", 
                                                           style="padding-left:0px; padding-top:40px; padding-right:0px;"),
                                                 ),
                                                 
                                                 #placeholder for relative CO2 score
                                        
                                          ),
                                          column(width = 6,
                                                 h2("Did you know that...",
                                                    style="padding-top:50px;"
                                                 ),
                                                 h4("...the ingredient with less emissions is", align="center"),
                                                 h4("...the ingredient with more emissions is",  align="center"),
                        
                                           ),
                                          )),
                       
                                )))
                      
                    
  
            

#############SERVER##########################################################

# Define server logic 
server <- function(input, output, session) {
  
  #Button Homepage to calculator
  pageButtonServer("navbar", parentSession = session)
  
  
    
  }
 


#############################################################################
# Run the application 
shinyApp(ui = ui, server = server)
