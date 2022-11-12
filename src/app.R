#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

run_shiny_front <- function(external_ip, port){

  options(shiny.host = "0.0.0.0", shiny.port= 8080)
###added shinythemes package to change layout
#install.packages("shinythemes")
#install.packages("fresh")
#install.packages("xlsx")
#install.packages("readxl")

library(jsonlite)
library(httr)
library(shiny)
library(shinythemes)
library(fresh)
library(dipsaus)
library(shinyWidgets)
library(readxl)
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
                                             column(width = 6,
                                                    h2("Type your ingredients here.",
                                                       style="padding-top:50px;"
                                                       ),
                                                    
                                                    column(width = 4, 
                                                           pickerInput(inputId = "textin", 
                                                                       label = "Ingredients", 
                                                                       choices = c("mushrooms", "corn", "tomato", "beans", "peas", "chickpeas", 
                                                                                   "lentils", "pineapple", "apricot", "peach", "basil", "rosemary", 
                                                                                   "thyme", "dates", "tea", "oregano", "lentils", "dates", "spices", 
                                                                                   "basil", "oregano", "beetroot", "dates", "crab", "crayfish", 
                                                                                   "langostino", "prawns", "scampi", "shrimps", "brussel sprouts", 
                                                                                   "strawberries", "blueberries", "raspberries", "french fries", 
                                                                                   "beetroot", "red cabbage", "kale", "black-eyed beans", "plum", 
                                                                                   "burger bun", "anise", "baking powder", "baking soda", "lemon balm", 
                                                                                   "balsamic vinegar", "bbq sauce", "bay leaves", "cardamom", "chervil", 
                                                                                   "chia seeds", "chili", "cinnamon", "clove", "cilantro", "cumin", 
                                                                                   "turmeric", "curry", "dill", "fish sauce", "gelatin", "ginger", 
                                                                                   "herbs", "ketchup", "lemon grass", "macadamia nuts", "marjoram", 
                                                                                   "mayonnaise", "caraway", "mustard", "mustard seeds", "mustard leaves", 
                                                                                   "nutmeg", "sesame oil", "onion powder", "paprika", "parsley", 
                                                                                   "pecans", "pepper", "pine nuts", "poppy seed", "saffron", "sage", 
                                                                                   "salt", "sea salt", "sesame", "soy sauce", "agave syrup", "tabasco sauce", 
                                                                                   "tarragon", "tomatoes paste", "vanilin", "vanilla extract", "apple vinegar", 
                                                                                   "white wine vinegar", "wasabi", "yeast extract", "yeast", "beer", 
                                                                                   "brandy", "cognac", "liquor", "tawny port", "red wine", "white wine", 
                                                                                   "wine", "half-fat margarine", "extravirgin olive oil", "olive oil", 
                                                                                   "sunflower oil", "vegan spreadable fat", "full-fat margarine", 
                                                                                   "clams", "cockles", "mussels", "oysters", "scallops", "egg", 
                                                                                   "sausage", "hot dog", "mushrooms", "morel mushrooms", "porcini mushrooms", 
                                                                                   "portobello mushrooms", "shiitake mushrooms", "wild mushrooms", 
                                                                                   "lamb's lettuce", "collard green", "kale", "cherry tomatoes", 
                                                                                   "kohlrabi", "acorn squash", "banana squash", "butternut squash", 
                                                                                   "delicata squash", "gem squash", "hubbard squash", "pumpkin", 
                                                                                   "spaghetti squash", "chives", "leek", "brussel sprouts", "arugula", 
                                                                                   "mixed salad", "sweet potato", "savoy", "wholegrain bread bun", 
                                                                                   "wheat bread bun", "bulgur", "couscous", "millet", "crispbread", 
                                                                                   "egg noodles", "lasagna noodles", "linguine", "pasta", "wholewheat noodles", 
                                                                                   "toast", "wholegrain toast", "wholegrain bread", "bread baguette", 
                                                                                   "wheat bread", "all-purpose flour", "panko", "wholewheat flour", 
                                                                                   "walnuts", "brown lentils", "green lentils", "lentils", "cream cheese", 
                                                                                   "philadelphia cream cheese", "ricotta", "low-fat curd cheese", 
                                                                                   "mozzarella", "curd cheese", "whipping cream", "schmand", "blackberries", 
                                                                                   "fig", "grapefruit", "blueberries", "raspberries", "cherries", 
                                                                                   "honeyberries", "jabuticabas", "surinam cherries", "mango", "passion fruit", 
                                                                                   "ackee", "damson", "jambul", "japanese plum", "gooseberries", 
                                                                                   "watermelon", "icing sugar", "vegetable stock", "honey", "chicken broth", 
                                                                                   "coconut milk", "beef broth", "pea veggie burger", "soy veggie burger", 
                                                                                   "peanut butter", "pastry", "puff pastry", "veggie nugget", "veggie patty", 
                                                                                   "gnocchi", "chicken nuggets", "chicken cold cuts", "coffee powder", 
                                                                                   "cocoa powder", "cheese emmentaler", "parmesan", "lupin flour", 
                                                                                   "spelt drink", "oats drink", "almond drink", "mineral water", 
                                                                                   "soy curd", "beef mince", "apple juice", "orange juice", "dark chocolate", 
                                                                                   "milk chocolade", "white chocolate", "tempeh", "venison", "vegan sausage", 
                                                                                   "thuringian sausage", "beef cold cuts", "ham", "bacon", "cashew nuts", 
                                                                                   "chestnuts", "hazelnuts", "pistachios", "artichoke", "jerusalem artichoke", 
                                                                                   "chicory", "endive", "cheddar cheese", "quinoa", "almonds", "apple", 
                                                                                   "crab apple", "fuji apple", "apricot", "eggplant", "avocado", 
                                                                                   "banana", "barley", "pearl barley", "barley", "barley", "broccoli", 
                                                                                   "butter", "buttermilk", "red cabbage", "bok choy", "white cabbage", 
                                                                                   "baby carrot", "carrot", "beef", "cauliflower", "celeriac", "celery", 
                                                                                   "cheese", "chicken", "cocoa beans", "coconut oil", "coconut", 
                                                                                   "coffee", "coffee beans", "cottonseed oil", "full cream milk", 
                                                                                   "milk", "cream", "sour cream", "cucumber", "beans", "black beans", 
                                                                                   "borlotti beans", "broad beans", "fava beans", "green beans", 
                                                                                   "kidney beans", "mung beans", "navy beans", "pinto beans", "runner beans", 
                                                                                   "beans", "black beans", "borlotti beans", "broad beans", "fava beans", 
                                                                                   "green beans", "kidney beans", "mung beans", "navy beans", "pinto beans", 
                                                                                   "runner beans", "fennel", "fish oil", "barracuda", "basa", "bass", 
                                                                                   "black cod", "blowfish", "brill", "butter fish", "catfish", "cod", 
                                                                                   "dorade", "flounder", "grouper", "haddock", "halibut", "lingcod", 
                                                                                   "monkfish", "mullet", "patagonian toothfish", "perch", "pike", 
                                                                                   "pollock", "salmon", "sanddab", "sea bass", "shad", "skate", 
                                                                                   "sole", "sturgeon", "tilefish", "turbot", "whitebait", "whitefish", 
                                                                                   "whiting", "anchovy", "hake", "tuna", "bluefish", "bombay duck", 
                                                                                   "bream", "dogfish", "herring", "john dory", "mackerel", "mahi mahi", 
                                                                                   "orange roughy", "pomfret", "pompano", "sardine", "sprat", "swordfish", 
                                                                                   "grapes", "green asparagus", "bell pepper", "green bell pepper", 
                                                                                   "habanero", "jalapeno", "okra", "red bell pepper", "red chilli", 
                                                                                   "yellow bell pepper", "iceberg lettuce", "kiwi", "lemon", "lemon juice", 
                                                                                   "lemon zest", "lime", "bibb lettuce", "lettuce", "linseed", "baby corn", 
                                                                                   "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", 
                                                                                   "cornstarch", "clementine", "mandarine", "tangerine", "bitter melon", 
                                                                                   "cantaloupe", "honeydew", "melon", "catnip", "mint", "peppermint", 
                                                                                   "spearmint", "oats", "green olives", "kalamata olives", "olives", 
                                                                                   "garlic", "green onion", "onion", "red onion", "scallion", "shallot", 
                                                                                   "spring onion", "blood orange", "kumquat", "orange", "orange peel", 
                                                                                   "palm fruit", "palm kernel oil", "palm oil", "papaya", "paris market carrot", 
                                                                                   "nectarine", "peach", "peanuts", "pear", "pineapple", "potato", 
                                                                                   "russet  potato", "potato starch", "potato", "russet  potato", 
                                                                                   "potato", "russet  potato", "baby peas", "chickpeas", "peas", 
                                                                                   "split peas", "sugar snap peas", "peas", "split peas", "sugar snap peas", 
                                                                                   "peas", "split peas", "sugar snap peas", "horseradish", "radish", 
                                                                                   "white radish", "rape oil", "red meat", "basmati rice", "brown rice", 
                                                                                   "rice", "wild rice", "rye", "rye", "rye", "lamb", "skimmed milk", 
                                                                                   "soybeans", "soybean drink", "soybean oil", "soybeans", "soybeans", 
                                                                                   "baby spinach", "new zealand spinach", "spinach", "blackcurrants", 
                                                                                   "boysenberries", "cloudberries", "cranberries", "currants", "elderberries", 
                                                                                   "goji berries", "jostaberries", "juniper berries", "marionberries", 
                                                                                   "miracle fruit", "mulberries", "pineberries", "redcurrants", 
                                                                                   "salal berries", "salmonberries", "strawberries", "wheat berries", 
                                                                                   "white currants", "brown sugar", "granulated sugar", "sugar", 
                                                                                   "sunflower seeds", "sunflower seeds", "sweetcorn", "sweet sorghum grain", 
                                                                                   "sweet sorghum stem", "pork", "ice", "tap water", "tilapia", 
                                                                                   "tofu", "tomato", "trout", "vanilla", "whey", "white asparagus", 
                                                                                   "yogurt", "zucchini"), 
                                                                       selected = "choose", 
                                                                       options = list(`live-search` = TRUE)),
                                                           verbatimTextOutput("Ing"),
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
                                                    column(width = 4,
                                                           textInput("quantityin", "Quantity", 
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
                                                    column(width = 4,
                                                
                                                           pickerInput(inputId = "unitin", 
                                                                    label = "Unit", 
                                                                    choices = c("inch", "inches", "millimeter", "millimeters", "millimetre", 
                                                                                "millimetres", "mm", "#", "g", "gram", "grams", "gs", "kg", "kgs", 
                                                                                "kilo", "kilogram", "kilogramme", "kilogrammes", "kilograms", 
                                                                                "kilos", "lb", "mg", "mgs", "milligram", "milligramme", "milligrammes", 
                                                                                "ounce", "ounces", "oz", "pound", "pounds", "centimeter", "centimeters", 
                                                                                "centimetre", "centimetres", "cm", "cms", "c", "cc", "cl", "cup", 
                                                                                "cups", "dash", "dc", "deci", "deciliter", "deciliters", "dl", 
                                                                                "fl oz", "fl pt", "fl qt", "fluid ounce", "gal", "gallon", "gallons", 
                                                                                "gill", "jigger", "liter", "liters", "ml", "p", "pinch", "pinches", 
                                                                                "pint", "pints", "pt", "q", "qt", "t", "tablespoon", "tablespoons", 
                                                                                "tbl", "tbs", "tbsp", "teaspoon", "teaspoons", "tsp", "bunch", 
                                                                                "bunches", "can", "chunk", "chunks", "head", "k", "leaf", "leaves", 
                                                                                "sheet", "sheets", "slice", "slices", "sprig", "sprigs", "stalk", 
                                                                                "stalks", "strip", "strips", "wedge", "wedges", "milliliter", 
                                                                                "millilitre", "milliliters", "quart", "dashes", "tin", "bottle", 
                                                                                "pieces", "piece", "sachet", "sachets", "scoops", "scoop", "packet", 
                                                                                "packets", "pkt", "mls", "carton", "cartons", "pod", "sized", 
                                                                                "lbs", "cans", "lts", "package", "packages", "litres", "litre", 
                                                                                "sticks", "stick", "pods", "portion", "portions", "drop", "drops", 
                                                                                "knob", "jar", "jars", "cube", "l"), 
                                                                    selected = "choose", 
                                                                    options = list(`live-search` = TRUE)),
                                                           verbatimTextOutput("Uni"),
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
                                                    #column(width = 2,
                                                           #pickerInput(inputId = "varietyin", 
                                                                       #label = "Variety", 
                                                                       #choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                       #selected = "choose", 
                                                                       #options = list(`live-search` = TRUE)),
                                                           #verbatimTextOutput("Var"),
                                                           #pickerInput(inputId = "varietyin", 
                                                                      #label = "", 
                                                                      #choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                           #pickerInput(inputId = "varietyin", 
                                                                      #label = "", 
                                                                      #choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                          # pickerInput(inputId = "varietyin", 
                                                                      #label = "", 
                                                                      #choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                           #pickerInput(inputId = "varietyin", 
                                                                      #label = "", 
                                                                      #choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE))
                                          
                                                    #),
                                                    #column(width = 2,
                                                           #pickerInput(inputId = "certificationin", 
                                                                      #label = "Certification", 
                                                                      #choices = c("organic","conventional", 
                                                                      #             "swiss integrated production"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                           #pickerInput(inputId = "certificationin", 
                                                                      #label = "", 
                                                                      #choices = c("organic","conventional", 
                                                                      #             "swiss integrated production"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                           #pickerInput(inputId = "certificationin", 
                                                                      #label = "", 
                                                                      #choices = c("organic","conventional", 
                                                                      #             "swiss integrated production"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                         #  pickerInput(inputId = "certificationin", 
                                                                      #label = "", 
                                                                      #choices = c("organic","conventional", 
                                                                      #             "swiss integrated production"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                           #pickerInput(inputId = "certificationin", 
                                                                      #label = "", 
                                                                      #choices = c("organic","conventional", 
                                                                      #             "swiss integrated production"), 
                                                                      #selected = "choose", 
                                                                      #options = list(`live-search` = TRUE)),
                                                           
                                                           actionButton('btn2',"CALCULATE", style="color:white; background-color:#7AA95C"
                                                           ),
                                                           
                
                                                          
                                                    
                                                    
                                             ),
                                             column(width = 6,
                                                    column(width = 12,
                                                    h2("Or paste the URL here."), 
                                                    textInput("urlin", "URL",
                                                              placeholder = "https://www.food.com/recipe/pretty-freaking-awesome-pulled-pork-crock-pot-484624", 
                                                              value="https://www.food.com/recipe/pretty-freaking-awesome-pulled-pork-crock-pot-484624",
                                                              width = "100%"),
                                                    textOutput("urlout"),
                                                    
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
                                                        pickerInput("textR", "Ingredients", choices = c("mushrooms", "corn", "tomato", "beans", "peas", "chickpeas", 
                                                                                                        "lentils", "pineapple", "apricot", "peach", "basil", "rosemary", 
                                                                                                        "thyme", "dates", "tea", "oregano", "lentils", "dates", "spices", 
                                                                                                        "basil", "oregano", "beetroot", "dates", "crab", "crayfish", 
                                                                                                        "langostino", "prawns", "scampi", "shrimps", "brussel sprouts", 
                                                                                                        "strawberries", "blueberries", "raspberries", "french fries", 
                                                                                                        "beetroot", "red cabbage", "kale", "black-eyed beans", "plum", 
                                                                                                        "burger bun", "anise", "baking powder", "baking soda", "lemon balm", 
                                                                                                        "balsamic vinegar", "bbq sauce", "bay leaves", "cardamom", "chervil", 
                                                                                                        "chia seeds", "chili", "cinnamon", "clove", "cilantro", "cumin", 
                                                                                                        "turmeric", "curry", "dill", "fish sauce", "gelatin", "ginger", 
                                                                                                        "herbs", "ketchup", "lemon grass", "macadamia nuts", "marjoram", 
                                                                                                        "mayonnaise", "caraway", "mustard", "mustard seeds", "mustard leaves", 
                                                                                                        "nutmeg", "sesame oil", "onion powder", "paprika", "parsley", 
                                                                                                        "pecans", "pepper", "pine nuts", "poppy seed", "saffron", "sage", 
                                                                                                        "salt", "sea salt", "sesame", "soy sauce", "agave syrup", "tabasco sauce", 
                                                                                                        "tarragon", "tomatoes paste", "vanilin", "vanilla extract", "apple vinegar", 
                                                                                                        "white wine vinegar", "wasabi", "yeast extract", "yeast", "beer", 
                                                                                                        "brandy", "cognac", "liquor", "tawny port", "red wine", "white wine", 
                                                                                                        "wine", "half-fat margarine", "extravirgin olive oil", "olive oil", 
                                                                                                        "sunflower oil", "vegan spreadable fat", "full-fat margarine", 
                                                                                                        "clams", "cockles", "mussels", "oysters", "scallops", "egg", 
                                                                                                        "sausage", "hot dog", "mushrooms", "morel mushrooms", "porcini mushrooms", 
                                                                                                        "portobello mushrooms", "shiitake mushrooms", "wild mushrooms", 
                                                                                                        "lamb's lettuce", "collard green", "kale", "cherry tomatoes", 
                                                                                                        "kohlrabi", "acorn squash", "banana squash", "butternut squash", 
                                                                                                        "delicata squash", "gem squash", "hubbard squash", "pumpkin", 
                                                                                                        "spaghetti squash", "chives", "leek", "brussel sprouts", "arugula", 
                                                                                                        "mixed salad", "sweet potato", "savoy", "wholegrain bread bun", 
                                                                                                        "wheat bread bun", "bulgur", "couscous", "millet", "crispbread", 
                                                                                                        "egg noodles", "lasagna noodles", "linguine", "pasta", "wholewheat noodles", 
                                                                                                        "toast", "wholegrain toast", "wholegrain bread", "bread baguette", 
                                                                                                        "wheat bread", "all-purpose flour", "panko", "wholewheat flour", 
                                                                                                        "walnuts", "brown lentils", "green lentils", "lentils", "cream cheese", 
                                                                                                        "philadelphia cream cheese", "ricotta", "low-fat curd cheese", 
                                                                                                        "mozzarella", "curd cheese", "whipping cream", "schmand", "blackberries", 
                                                                                                        "fig", "grapefruit", "blueberries", "raspberries", "cherries", 
                                                                                                        "honeyberries", "jabuticabas", "surinam cherries", "mango", "passion fruit", 
                                                                                                        "ackee", "damson", "jambul", "japanese plum", "gooseberries", 
                                                                                                        "watermelon", "icing sugar", "vegetable stock", "honey", "chicken broth", 
                                                                                                        "coconut milk", "beef broth", "pea veggie burger", "soy veggie burger", 
                                                                                                        "peanut butter", "pastry", "puff pastry", "veggie nugget", "veggie patty", 
                                                                                                        "gnocchi", "chicken nuggets", "chicken cold cuts", "coffee powder", 
                                                                                                        "cocoa powder", "cheese emmentaler", "parmesan", "lupin flour", 
                                                                                                        "spelt drink", "oats drink", "almond drink", "mineral water", 
                                                                                                        "soy curd", "beef mince", "apple juice", "orange juice", "dark chocolate", 
                                                                                                        "milk chocolade", "white chocolate", "tempeh", "venison", "vegan sausage", 
                                                                                                        "thuringian sausage", "beef cold cuts", "ham", "bacon", "cashew nuts", 
                                                                                                        "chestnuts", "hazelnuts", "pistachios", "artichoke", "jerusalem artichoke", 
                                                                                                        "chicory", "endive", "cheddar cheese", "quinoa", "almonds", "apple", 
                                                                                                        "crab apple", "fuji apple", "apricot", "eggplant", "avocado", 
                                                                                                        "banana", "barley", "pearl barley", "barley", "barley", "broccoli", 
                                                                                                        "butter", "buttermilk", "red cabbage", "bok choy", "white cabbage", 
                                                                                                        "baby carrot", "carrot", "beef", "cauliflower", "celeriac", "celery", 
                                                                                                        "cheese", "chicken", "cocoa beans", "coconut oil", "coconut", 
                                                                                                        "coffee", "coffee beans", "cottonseed oil", "full cream milk", 
                                                                                                        "milk", "cream", "sour cream", "cucumber", "beans", "black beans", 
                                                                                                        "borlotti beans", "broad beans", "fava beans", "green beans", 
                                                                                                        "kidney beans", "mung beans", "navy beans", "pinto beans", "runner beans", 
                                                                                                        "beans", "black beans", "borlotti beans", "broad beans", "fava beans", 
                                                                                                        "green beans", "kidney beans", "mung beans", "navy beans", "pinto beans", 
                                                                                                        "runner beans", "fennel", "fish oil", "barracuda", "basa", "bass", 
                                                                                                        "black cod", "blowfish", "brill", "butter fish", "catfish", "cod", 
                                                                                                        "dorade", "flounder", "grouper", "haddock", "halibut", "lingcod", 
                                                                                                        "monkfish", "mullet", "patagonian toothfish", "perch", "pike", 
                                                                                                        "pollock", "salmon", "sanddab", "sea bass", "shad", "skate", 
                                                                                                        "sole", "sturgeon", "tilefish", "turbot", "whitebait", "whitefish", 
                                                                                                        "whiting", "anchovy", "hake", "tuna", "bluefish", "bombay duck", 
                                                                                                        "bream", "dogfish", "herring", "john dory", "mackerel", "mahi mahi", 
                                                                                                        "orange roughy", "pomfret", "pompano", "sardine", "sprat", "swordfish", 
                                                                                                        "grapes", "green asparagus", "bell pepper", "green bell pepper", 
                                                                                                        "habanero", "jalapeno", "okra", "red bell pepper", "red chilli", 
                                                                                                        "yellow bell pepper", "iceberg lettuce", "kiwi", "lemon", "lemon juice", 
                                                                                                        "lemon zest", "lime", "bibb lettuce", "lettuce", "linseed", "baby corn", 
                                                                                                        "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", 
                                                                                                        "cornstarch", "clementine", "mandarine", "tangerine", "bitter melon", 
                                                                                                        "cantaloupe", "honeydew", "melon", "catnip", "mint", "peppermint", 
                                                                                                        "spearmint", "oats", "green olives", "kalamata olives", "olives", 
                                                                                                        "garlic", "green onion", "onion", "red onion", "scallion", "shallot", 
                                                                                                        "spring onion", "blood orange", "kumquat", "orange", "orange peel", 
                                                                                                        "palm fruit", "palm kernel oil", "palm oil", "papaya", "paris market carrot", 
                                                                                                        "nectarine", "peach", "peanuts", "pear", "pineapple", "potato", 
                                                                                                        "russet  potato", "potato starch", "potato", "russet  potato", 
                                                                                                        "potato", "russet  potato", "baby peas", "chickpeas", "peas", 
                                                                                                        "split peas", "sugar snap peas", "peas", "split peas", "sugar snap peas", 
                                                                                                        "peas", "split peas", "sugar snap peas", "horseradish", "radish", 
                                                                                                        "white radish", "rape oil", "red meat", "basmati rice", "brown rice", 
                                                                                                        "rice", "wild rice", "rye", "rye", "rye", "lamb", "skimmed milk", 
                                                                                                        "soybeans", "soybean drink", "soybean oil", "soybeans", "soybeans", 
                                                                                                        "baby spinach", "new zealand spinach", "spinach", "blackcurrants", 
                                                                                                        "boysenberries", "cloudberries", "cranberries", "currants", "elderberries", 
                                                                                                        "goji berries", "jostaberries", "juniper berries", "marionberries", 
                                                                                                        "miracle fruit", "mulberries", "pineberries", "redcurrants", 
                                                                                                        "salal berries", "salmonberries", "strawberries", "wheat berries", 
                                                                                                        "white currants", "brown sugar", "granulated sugar", "sugar", 
                                                                                                        "sunflower seeds", "sunflower seeds", "sweetcorn", "sweet sorghum grain", 
                                                                                                        "sweet sorghum stem", "pork", "ice", "tap water", "tilapia", 
                                                                                                        "tofu", "tomato", "trout", "vanilla", "whey", "white asparagus", 
                                                                                                        "yogurt", "zucchini"), selected = "choose", options = list(`live-search` = TRUE)),
                                                       
                                                        
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
server <- function(input, output, session, e = external_ip, p=port) {
  
  #Button Homepage to calculator
  pageButtonServer("navbar", parentSession = session)
  


      
    
#type_ingredients <- eventReactive(input$btn2,{
    #paste(input$textin)
   #  })
   # output$Ing <- renderText({
    #  type_ingredients()
   # })
  
type_drops <- eventReactive(input$btn2,{
    paste(input$textin)
    })
    output$Ing <- renderText({
      type_drops()
    })  

    output$Ing2 <- renderText({
      paste(input$textin2)
    })
    output$Ing3 <- renderText({
      paste(input$textin3)
    })
    output$Ing4 <- renderText({
      paste(input$textin4)
    })
    output$Ing5 <- renderText({
      paste(input$textin5)
    })
    output$Qua <- renderText({
      paste(input$quantityin)
    })
    output$Qua2 <- renderText({
      paste(input$quantityin2)
    })
    output$Qua3 <- renderText({
      paste(input$quantityin3)
    })
    output$Qua4 <- renderText({
      paste(input$quantityin4)
    })
    output$Qua5 <- renderText({
      paste(input$quantityin5)
    })
    output$Uni <- renderText({
      paste(input$unitin)
    })
    output$Uni2 <- renderText({
      paste(input$unitin2)
    })
    output$Uni3 <- renderText({
      paste(input$unitin3)
    })
    output$Uni4 <- renderText({
      paste(input$unitin4)
    })
    output$Uni5 <- renderText({
      paste(input$unitin5)
    })
   output$urlout<-renderText({
     base = paste0("http://", e,":", p,"/")
     r <- httr::GET(url=base,
                    path="get_table",
                    query=list(foodlink=input$urlin), verbose()
                    )
     fromJSON(content(r, "text"))
   }) 
    
  }
 


#############################################################################
# Run the application 
shinyApp(ui = ui, server = server)
}

