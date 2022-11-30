#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

run_shiny_front <- function(external_ip, port){

  options(shiny.host = "0.0.0.0", shiny.port= 8080)

  
#############Packages#########################################################

###added shinythemes package to change layout
#install.packages("shinythemes")
#install.packages("fresh")
#install.packages("xlsx")
#install.packages("readxl")
#install.packages("httr")
#install.packages("data.table")
#install.packages("readr")

library(plotly)
library(jsonlite)
library(httr)
library(shiny)
library(shinythemes)
library(fresh)
library(dipsaus)
library(shinyWidgets)
library(readxl)
library(shinydashboard)
library(data.table)
library(readr)


############ Functions #########################################################

#action button to change the page homepage
pageButtonUi <- function(id) {
  actionButton(NS(id, "page_change"),
               label = 'GO TO CALCULATOR', 
               style="background-color:#7AA95C; color:white")
}

#button function server homepage to calculator
pageButtonServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$page_change, {
      updateNavbarPage(session=parentSession,
                       inputId="navbar",
                       selected="Calculator")
    })
  })
}

#action button to change the page in recommendations for learn more
pageButtonUi2 <- function(id) {
  actionButton(NS(id, "page_change2"),
               label = 'LEARN MORE', 
               style="background-color:#7AA95C; color:white")

} 
#button function server for learn more
pageButtonServer2 <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$page_change2, {
      updateNavbarPage(session=parentSession,
                       inputId="navbar",
                       selected="Learn more")
    })
  })
}

#action button to change the page homepage
pageButtonUi3 <- function(id) {
  actionButton(NS(id, "page_change3"),
               label = 'GET RECOMMENDATIONS', 
               style="background-color:#7AA95C; color:white")
}
#button function server for calculator
pageButtonServer3 <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$page_change3, {
      updateNavbarPage(session=parentSession,
                       inputId="navbar",
                       selected="Recommendations")
    })
  })
}
###Placeholder code for pie charts in learn more:
#EmissionsChart <- data.frame("Categorie"=rownames(EmissionsChart), EmissionsChart)
#data <- EmissionsChart[,c('Categorie', 'X1960')]

#fig1 <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
#fig1 <- fig %>% layout(title = 'CO2 Emissions by Variety',
                      # xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      # yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


                       

###########UI#############################################################


# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("www/styles.css"),
          titlePanel(
    
  #navigation tabs: create the tabs and define the theme with css file
                      navbarPage(
                                  title = div(img(src="https://github.com/ZverM7/front_end/blob/main/www/logo.png?raw=true", 
                                              width= "35%"), 
                                              #height= '70px', 
                                              style="padding-left:5px;padding-top:10px; padding-right:100px"), 
                                        # picture has to go in www folder
                                  id = "navbar",
                                  selected = "Home",
                                  position = "static-top",
                                  theme = "styles.css",
                                  fluid = T,
                                
                      #Home page              ##############################################################           
                              tabPanel("Home",
                                      fixedRow(
                                        
                                      #1.column
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
                                            div(pageButtonUi("navbar"),
                                                style = "padding-top: 20%",
                                                align = "right"),
                                          
                                              ),
                                        
                                      #2.column
                                          column(width = 6,
                                                  style="padding-top:50px; 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/home.jpg?raw=true); 
                                                    background-size:cover; padding-bottom:70%;"
                                                 ),
                                            
                                 
                                            )
                                      ),
                      
                        #Calculator page              ############################################################## 
                                  tabPanel("Calculator", 
                                           fluidRow(
                                             tags$head(
                                               tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                             
                                             
                                        #1.column 
                                             column(width = 6,
                                                    h2("Type your ingredients here.",
                                                       style="padding-top:50px;"
                                                       ),
                                                    
                                                    div(
                                                      style = "display: grid; 
                                                      grid-template-columns: 30% repeat(3, 30%); 
                                                      grid-gap: 10px;",
                                                      
                                                      pickerInput(inputId = "textin", 
                                                                  label = "Ingredients", 
                                                                  choices = c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini"), 
                                                                  selected = "choice", 
                                                                  options = list(`live-search` = TRUE)),
                                                      
                                                      textInput("quantityin", "Quantity", 
                                                                placeholder = "e.g. 200" ),
                                                      
                                                      pickerInput(inputId = "unitin", 
                                                                  label = "Unit", 
                                                                  choices = c("#", "bottle", "bunch", "bunches", "c", "can", "cans", "carton", "cartons", "cc", "centimeter", "centimeters", "centimetre", "centimetres", "chunk", "chunks", "cl", "cm", "cms", "cube", "cup", "cups", "dash", "dashes", "dc", "deci", "deciliter", "deciliters", "dl", "drop", "drops", "fl oz", "fl pt", "fl qt", "fluid ounce", "g",  "gal", "gallon", "gallons", "gill", "gram", "grams", "gs", "head", "inch", "inches", "jar", "jars", "jigger", "k", "kg", "kgs", "kilo", "kilogram", "kilogramme", "kilogrammes", "kilograms",  "kilos", "knob", "l", "lb", "lbs", "leaf", "leaves", "liter", "liters", "litre", "litres", "lts", "mg", "mgs", "milligram", "milligramme", "milligrammes", "milliliter", "milliliters", "millilitre", "millimeter", "millimeters", "millimetre", "millimetres", "ml",  "mls", "mm", "ounce", "ounces", "oz", "p", "package", "packages",  "packet", "packets", "piece", "pieces", "pinch", "pinches", "pint",  "pints", "pkt", "pod", "pods", "portion", "portions", "pound",  "pounds", "pt", "q", "qt", "quart", "sachet", "sachets", "scoop",  "scoops", "sheet", "sheets", "sized", "slice", "slices", "sprig", "sprigs", "stalk", "stalks", "stick", "sticks", "strip", "strips",  "t", "tablespoon", "tablespoons", "tbl", "tbs", "tbsp", "teaspoon", "teaspoons", "tin", "tsp", "wedge", "wedges"), 
                                                                  selected = "choose", 
                                                                  options = list(`live-search` = TRUE))
                                                      ),
                                                      
                                                    div(
                                                      style = "display: grid; 
                                                      grid-template-columns: 30% repeat(3, 30%); 
                                                      grid-gap: 10px;",
                                                      pickerInput(inputId = "textin2",  
                                                                  choices = c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini"), 
                                                                  selected = "choice", 
                                                                  options = list(`live-search` = TRUE)),
                                                      
                                                      textInput("quantityin2",NULL, 
                                                                placeholder = "e.g. 200" ),
                                                      
                                                      pickerInput(inputId = "unitin2",  
                                                                  choices = c("#", "bottle", "bunch", "bunches", "c", "can", "cans", "carton", "cartons", "cc", "centimeter", "centimeters", "centimetre", "centimetres", "chunk", "chunks", "cl", "cm", "cms", "cube", "cup", "cups", "dash", "dashes", "dc", "deci", "deciliter", "deciliters", "dl", "drop", "drops", "fl oz", "fl pt", "fl qt", "fluid ounce", "g",  "gal", "gallon", "gallons", "gill", "gram", "grams", "gs", "head", "inch", "inches", "jar", "jars", "jigger", "k", "kg", "kgs", "kilo", "kilogram", "kilogramme", "kilogrammes", "kilograms",  "kilos", "knob", "l", "lb", "lbs", "leaf", "leaves", "liter", "liters", "litre", "litres", "lts", "mg", "mgs", "milligram", "milligramme", "milligrammes", "milliliter", "milliliters", "millilitre", "millimeter", "millimeters", "millimetre", "millimetres", "ml",  "mls", "mm", "ounce", "ounces", "oz", "p", "package", "packages",  "packet", "packets", "piece", "pieces", "pinch", "pinches", "pint",  "pints", "pkt", "pod", "pods", "portion", "portions", "pound",  "pounds", "pt", "q", "qt", "quart", "sachet", "sachets", "scoop",  "scoops", "sheet", "sheets", "sized", "slice", "slices", "sprig", "sprigs", "stalk", "stalks", "stick", "sticks", "strip", "strips",  "t", "tablespoon", "tablespoons", "tbl", "tbs", "tbsp", "teaspoon", "teaspoons", "tin", "tsp", "wedge", "wedges"), 
                                                                  selected = "choose", 
                                                                  options = list(`live-search` = TRUE)),
                                                    ),
                                                    
                                                    div(
                                                      style = "display: grid; 
                                                      grid-template-columns: 30% repeat(3, 30%); 
                                                      grid-gap: 10px;",
                                                      pickerInput(inputId = "textin3",  
                                                                  choices = c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini"), 
                                                                  selected = "choice", 
                                                                  options = list(`live-search` = TRUE)),
                                                      
                                                      textInput("quantityin3",NULL, 
                                                                placeholder = "e.g. 200" ),
                                                      
                                                      pickerInput(inputId = "unitin3",  
                                                                  choices = c("#", "bottle", "bunch", "bunches", "c", "can", "cans", "carton", "cartons", "cc", "centimeter", "centimeters", "centimetre", "centimetres", "chunk", "chunks", "cl", "cm", "cms", "cube", "cup", "cups", "dash", "dashes", "dc", "deci", "deciliter", "deciliters", "dl", "drop", "drops", "fl oz", "fl pt", "fl qt", "fluid ounce", "g",  "gal", "gallon", "gallons", "gill", "gram", "grams", "gs", "head", "inch", "inches", "jar", "jars", "jigger", "k", "kg", "kgs", "kilo", "kilogram", "kilogramme", "kilogrammes", "kilograms",  "kilos", "knob", "l", "lb", "lbs", "leaf", "leaves", "liter", "liters", "litre", "litres", "lts", "mg", "mgs", "milligram", "milligramme", "milligrammes", "milliliter", "milliliters", "millilitre", "millimeter", "millimeters", "millimetre", "millimetres", "ml",  "mls", "mm", "ounce", "ounces", "oz", "p", "package", "packages",  "packet", "packets", "piece", "pieces", "pinch", "pinches", "pint",  "pints", "pkt", "pod", "pods", "portion", "portions", "pound",  "pounds", "pt", "q", "qt", "quart", "sachet", "sachets", "scoop",  "scoops", "sheet", "sheets", "sized", "slice", "slices", "sprig", "sprigs", "stalk", "stalks", "stick", "sticks", "strip", "strips",  "t", "tablespoon", "tablespoons", "tbl", "tbs", "tbsp", "teaspoon", "teaspoons", "tin", "tsp", "wedge", "wedges"), 
                                                                  selected = "choose", 
                                                                  options = list(`live-search` = TRUE)),
                                                    ),
                                                    
                                                          
                                                   
                                                    # add ingredients button
                                                    
                                                    div(actionButton("add_btn", "Add Ingredient"), 
                                                        style = "padding-top:0px;"),
                                                    tags$div(id = "add_btn"),
                                                    
                                                    # ingredients calculate button
                                                    div(actionButton('btn2',"CALCULATE", 
                                                                        style="color:white; background-color:#7AA95C"),
                                                        style = "padding-top: 5%; padding-bottom: 5px;",
                                                        align = "right"),
                                                    
                                                    #your score is output 
                                                    tags$style(type='text/css', '#ingredout1 {color:#7AA95C; font-size: 20px;}'),
                                                    div(textOutput("ingredout1"),
                                                                style= "padding-left:15%; 
                                                                        padding-right:15%;
                                                                        padding-top:5px;",
                                                                align= "center"),
                                                    
                                                    #Score output
                                                    tags$style(type='text/css', '#ingredout {color:#7AA95C; font-size: 20px;
                                                               background-image: url(https://github.com/ZverM7/front_end/blob/main/www/cloud1.jpeg?raw=true);
                                                               background-size: auto 100%;
                                                               background-position: center center;
                                                               background-repeat: no-repeat;
                                                               line-height: 3.8;
                                                               margin: 20px;}'),
                                                    div(textOutput("ingredout"),
                                                               style= "padding-left:15%; 
                                                                      padding-right:15%;
                                                                      vertical-align: middle;",
                                                              align= "center"),
                                                    
                                                    #CO2 score explanation
                                                    tags$style(type='text/css', '#ingredout2 {color:#7AA95C; font-size: 10px;}'),
                                                    div(textOutput("ingredout2"),
                                                        style= "
                                                                padding-top:5px;",
                                                        align= "center"),
                                                    
                                             ),
                                             
                                        #2.column 
                                             column(width = 6,
                                                    style="padding-top:50px; 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/calculator.jpg?raw=true); 
                                                    background-size:cover; 
                                                    padding-bottom:70%;",
                                                    
                                                    h2("Or paste the URL on the recommendations page."), 
                                                    
                                                    h4("If you want to get the CO2 score of your recipe and get recommendations on how 
                                                       to eat greener go to the recommendation page by clicking the button below.",
                                                        style="padding-top: 50px;"),
                                                    
                                                    div(pageButtonUi3("rec"),
                                                        style = "padding-top: 15%;",
                                                        align = "right")
                                                 
                                                     )

                                                    )
                                           ),
                        #Recommendations page              ############################################################## 
                                  tabPanel("Recommendations", 
                                           fluidRow(
                                             tags$head(
                                              tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                        
                                        #1.column   
                                            column(width = 6,
                                                   h2("Paste your URL here.", 
                                                      style= "padding-top:50px; padding-bottom: 0%;"),
                                                   
                                                   div(textInput("urlin", "URL",
                                                             placeholder = "https://www...", 
                                                             #value="https://www.food.com/recipe/pretty-freaking-awesome-pulled-pork-crock-pot-484624",
                                                             width = "100%"),
                                                             align = "left"),
                                                   
                                                   
                                                # url calculate button
                                                   div(actionButton('btnR', 
                                                                    label = 'CALCULATE', 
                                                                    style="background-color:#7AA95C; color:white"),
                                                                    align= "right"
                                                      ),
                                                
                                                #Text output  
                                                  tags$style(type='text/css', '#urlout1 {color: #7AA95C; font-size: 20px;}'),
                                                  div(textOutput("urlout1"),
                                                      style = "padding-left:15%; 
                                                                padding-right:15%; 
                                                                padding-top: 5px; 
                                                                ",
                                                      align= "center"),
                                                
                                                #Co2 score output url
                                                   tags$style(type='text/css', '#urlout {color: #7AA95C; font-size: 20px;
                                                               background-image: url(https://github.com/ZverM7/front_end/blob/main/www/cloud1.jpeg?raw=true);
                                                               background-size: auto 100%;
                                                               background-position: center center;
                                                               background-repeat: no-repeat;
                                                               line-height: 3.8;
                                                               margin: 20px;}'),
                                                   div(textOutput("urlout"),
                                                       style = "padding-left:15%; 
                                                                padding-right:15%;
                                                                vertical-align: middle;",
                                                       align= "center"),
                                                
                                                #CO2 score explanation  
                                                  tags$style(type='text/css', '#urlout2 {color: #7AA95C; font-size: 10px;}'),
                                                  div(textOutput("urlout2"),
                                                    style = "padding-top: 5px;",
                                                    align= "center"),
                                                  
                                                   
                                           ),
                                           
                                        #2.column  
                                           column(width = 6,
                                                  style= "padding-top:50px; 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/recommendations.jpg?raw=true); 
                                                    background-size:cover; 
                                                    padding-bottom:40%;",
                                                  
                                                  h2("Our recommendations for you..."),
                                            
                                              # recommendations output url
                                              tags$head(tags$style("#urloutR table {border-colour: black;  
                                                                   color: #7AA95C; font-size: 10px;}", 
                                                                   media="screen", type="text/css")),
                                             
                                                  div(tableOutput("urloutR"),
                                                      #style = "white-space:pre-wrap;",
                                                      align= "center"),
                                                 
                                            #Button to learn more page
                                                  div(pageButtonUi2("prova"),
                                                      style = "padding-top: 10%; padding-bottom: 250px;",
                                                      align = "right"),
                                                  
                                                  
                                           ),
                                           )),
                                     
                      
                      
                        #Learn more page             ############################################################## 
                                  tabPanel("Learn more",
                                           fluidRow(
                                             tags$head(
                                               tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                          column(width = 6,
                                                 h2("Discover more about food emissions",
                                                    style="padding-top:50px;"
                                                 ),
                                                 h4("Type an ingredient and select from the drop down menu and see how the score changes!", 
                                                    style="padding-top:40px; padding-right:0px;"),
                                                 
                                                 #ingredient input
                                                 
                                                 div(
                                                   
                                                   
                                                   pickerInput(inputId = "textinR", 
                                                               label = "Ingredients", 
                                                               choices = unique(c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini")),
                                                               selected = NULL, 
                                                               options = list(`live-search` = TRUE))),
                                                   
                                                   
                                                   
                                                   #Table output
                                                   tags$head(tags$style("#tableL table {background-color: White; border-colour: black}", 
                                                                      media="screen", type="text/css")),
                                                   div(tableOutput("tableL"),
                                                      style = "font-size:40%" )
                                                   
                                                 
                                                 #placeholder for relative CO2 score
                                        
                                          ),
                                          column(width = 6,
                                                 style=" 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/learnmore.jpg?raw=true); 
                                                    background-size:cover; padding-bottom:350px;",
                                                 h2("Did you know that...",
                                                    style="padding-top:50px;"
                                                 ),
                                                 h4("...the ingredients with the least emissions are", 
                                                    align ="center",
                                                    style ="padding-top:40px;"),
                                                 h4(em("1) Sweet sorghum stem, with 0.0327 C02 Kg emissions per Kg 
                                                       of ingredient"), 
                                                    br(), 
                                                    br(),
                                                    em("2) Russet potatoes, with 0.0888 CO2 Kg emissions per Kg 
                                                       of ingredient"), 
                                                    br(),
                                                    br(),
                                                    em("3) Paris market carrots, with 0.192 CO2 Kg emissions per Kg 
                                                       of ingredient"), 
                                                    align ="center",
                                                    style ="font-size:15px; line-height: 1.2;
                                                    padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                    padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                                                 h4("...the ingredients with more emissions are",  
                                                    align="center"),
                                                 h4(em("1) Cocoa beans, with 19.7 C02 Kg emissions per Kg of ingredient"),
                                                    br(), 
                                                    br(),
                                                    em("2) Cheddar cheese, with 15.15499 CO2 Kg emissions per Kg 
                                                             of ingredient"), 
                                                    br(), 
                                                    br(),
                                                    em("3) Beef, with 14.9 CO2 Kg emissions per Kg of ingredient"), 
                                                    align ="center",
                                                    style ="font-size:15px; line-height: 1.2; 
                                                    padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                    padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                        
                                           ),
                                          )
                                          ),
                      
                                                           ############################
                       
                                )
                      )
  )
                      
                    
  
            

#############SERVER##########################################################
#source_python("get_recommendation_connect.py")

# Define server logic 
server <- function(input, output, session, e = "35.228.36.220", p="8080") {
  
#######page changing buttons
  
#Button Homepage to calculator
  pageButtonServer("navbar", parentSession = session)
 
#Button Recommendations to Learn more
  pageButtonServer2("prova", parentSession = session) 

#Button Calculator to Recommendations
  pageButtonServer3("rec", parentSession = session) 
  
#########
  
  
  
#add ingredients button
  i<-3
  observeEvent(input$add_btn, {
    insertUI(
      selector = "#add_btn",
      where = "beforeBegin",
      ui = tags$div(div(
        style = "display: grid; 
                          grid-template-columns: 30% repeat(3, 30%); 
                          grid-gap: 10px;",
        pickerInput(inputId = paste("textin", i+input$add_btn, sep = ""),  
                    choices = c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini"), 
                    selected = "choice", 
                    options = list(`live-search` = TRUE)),
        
        textInput(paste("quantityin", i+input$add_btn, sep = ""),NULL, 
                  placeholder = "e.g. 200" ),
        
        pickerInput(inputId = paste("unitin", i+input$add_btn, sep = ""),  
                    choices = c("#", "bottle", "bunch", "bunches", "c", "can", "cans", "carton", "cartons", "cc", "centimeter", "centimeters", "centimetre", "centimetres", "chunk", "chunks", "cl", "cm", "cms", "cube", "cup", "cups", "dash", "dashes", "dc", "deci", "deciliter", "deciliters", "dl", "drop", "drops", "fl oz", "fl pt", "fl qt", "fluid ounce", "g",  "gal", "gallon", "gallons", "gill", "gram", "grams", "gs", "head", "inch", "inches", "jar", "jars", "jigger", "k", "kg", "kgs", "kilo", "kilogram", "kilogramme", "kilogrammes", "kilograms",  "kilos", "knob", "l", "lb", "lbs", "leaf", "leaves", "liter", "liters", "litre", "litres", "lts", "mg", "mgs", "milligram", "milligramme", "milligrammes", "milliliter", "milliliters", "millilitre", "millimeter", "millimeters", "millimetre", "millimetres", "ml",  "mls", "mm", "ounce", "ounces", "oz", "p", "package", "packages",  "packet", "packets", "piece", "pieces", "pinch", "pinches", "pint",  "pints", "pkt", "pod", "pods", "portion", "portions", "pound",  "pounds", "pt", "q", "qt", "quart", "sachet", "sachets", "scoop",  "scoops", "sheet", "sheets", "sized", "slice", "slices", "sprig", "sprigs", "stalk", "stalks", "stick", "sticks", "strip", "strips",  "t", "tablespoon", "tablespoons", "tbl", "tbs", "tbsp", "teaspoon", "teaspoons", "tin", "tsp", "wedge", "wedges"), 
                    selected = "choose", 
                    options = list(`live-search` = TRUE)),
      ))
    )
  })
      
#################
  
  
  #co2 score from ingredients input
  observeEvent(input$btn2, {
    #Text
    output$ingredout1 <- renderText({
      paste("Your CO2 score is:")
    })
    #connection to the backend
    output$ingredout <- renderText({ 
      base = paste0("http://", e,":", p,"/")
      r <- httr::GET(url=base,
                     path="get_score_manual",
                     query=list(list_ing=input$textin,list_ing=input$textin2, list_ing=input$textin3 , list_ing=input$textin4, 
                                list_ing=input$textin5,list_ing=input$textin5, list_ing=input$textin6, list_ing=input$textin7,
                                list_ing=input$textin8, list_ing=input$textin9, list_ing=input$textin10, list_ing=input$textin11, 
                                list_ing=input$textin12, list_ing=input$textin13, list_ing=input$textin14, list_ing=input$textin15,
                                list_quant=input$quantityin, list_quant=input$quantityin2, list_quant=input$quantityin3, 
                                list_quant=input$quantityin4, list_quant=input$quantityin5, list_quant=input$quantityin6,
                                list_quant=input$quantityin7, list_quant=input$quantityin8, list_quant=input$quantityin9, 
                                list_quant=input$quantityin10, list_quant=input$quantityin11,list_quant=input$quantityin12,
                                list_quant=input$quantityin13, list_quant=input$quantityin14, list_quant=input$quantityin15,
                                list_meas=input$unitin, list_meas=input$unitin2, list_meas=input$unitin3, list_meas=input$unitin4, 
                                list_meas=input$unitin5, list_meas=input$unitin6, list_meas=input$unitin7, list_meas=input$unitin8,
                                list_meas=input$unitin9, list_meas=input$unitin10, list_meas=input$unitin11, list_meas=input$unitin12,
                                list_meas=input$unitin13, list_meas=input$unitin14, list_meas=input$unitin15
                                ), 
                     verbose()
      )
      paste(fromJSON(content(r, "text")))
      
    
          
    })
    #CO2 score explanation
    output$ingredout2 <- renderText({
      paste("* The CO2 score is the amount (kg) of CO2 emitted per kg of the ingredients.")
    })
    
  })

    
#co2 score from url input on recommendations page
  observeEvent(input$btnR, {
    #Text
    output$urlout1 <- renderText({
      paste("Your CO2 score is:")
    })
    output$urlout <- renderText({
     base = paste0("http://", e,":", p,"/")
     r <- httr::GET(url=base,
                    path="get_score",
                    query=list(foodlink=input$urlin), verbose()
                    )
     paste(fromJSON(content(r, "text")))
   }) 
    #CO2 score explanation
    output$urlout2 <- renderText({
      paste("* The CO2 score is the amount (kg) of CO2 emitted per kg of the ingredients.")
    })
  })
  


#connection to Malensa for Recommendations

observeEvent(input$btnR, {
  output$urloutR <- renderTable({
    base = paste0("http://35.228.95.210:5000/")
    r <- httr::GET(url=base,
                 path="get_recommendation",
                 query=list(url=input$urlin), verbose()
    )
    # r <- c(c("Apricot shortbread","https://www.food.com/recipe/apricot-shortbread-86796","0.4267"),c("Rhubarb shortbread","https://www.food.com/recipe/rhubarb-shortbread-60385","1.6733"),c("Cranberry shortbread","https://www.food.com/recipe/cranberry-shortbread-305893","0.5813"),c("Raspberry shortbread","https://www.food.com/recipe/raspberry-shortbread-80671","1.6061"),c("Almond shortbread","https://www.food.com/recipe/almond-shortbread-201417","0.2868"))
    colnames(r)<-c('Recipe Name', 'Link', 'CO2 Score')
    fromJSON(content(r, "text"))
  },
  bordered = TRUE,
  colnames = TRUE,
  rownames = FALSE,
  spacing = "xs",
  hover = TRUE,
  width = "10%"
  )
})


#learn more page

observeEvent(input$textinR, {
  df <- output$tableL <- renderTable({
    base = paste0("http://", e,":", p,"/")
    r <- httr::GET(url=base,
                   path="dash_info",
                   query=list(dash_ing = input$textinR), verbose()
    )
    fromJSON(content(r, "text"))
  },
  bordered = TRUE,
  spacing = "xs",
  hover = TRUE,
  width = "10%"
  ) 
})
}
#############################################################################

# Run the application 
shinyApp(ui = ui, server = server)

################################################
  
}
