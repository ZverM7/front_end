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
                      #Home page              ##############################################################           
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
                                            div(pageButtonUi("navbar"),
                                                style = "padding-left: 53%; padding-top: 20%"),
                                          
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
                      
                        #Calculator page              ############################################################## 
                                  tabPanel("Calculator", 
                                           fluidRow(
                                             tags$head(
                                               tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                             #tags$style("[type = 'text'] {font-size:10px;height:20px;}"),
                                             
                                             # input 
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
                                                      
                                                      textInput("quantityin2",NULL, 
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
                                                        style = "padding-left:75%; padding-top: 10%; padding-bottom: 30px;" ),
                                                    
                                                    
                                                    
                                                    #Score output
                                                    tags$style(type='text/css', '#ingredout {background-color:#7AA95C; color: black;}'),
                                                    div(textOutput("ingredout"),
                                                               style= "padding-left:15%; padding-right:20%; 
                                                                        padding-top: 10%; padding-bottom: 30px;",
                                                              align= "center")
                                                    
                                             ),
                                             
                                             
                                             column(width = 6,
                                                    style="padding-top:50px; 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/calculator.jpg?raw=true); 
                                                    background-size:cover; padding-bottom:100px;",
                                                    h2("Or paste the URL here."), 
                                                    
                                                    textInput("urlin", "URL",
                                                              placeholder = "https://www...", 
                                                              #value="https://www.food.com/recipe/pretty-freaking-awesome-pulled-pork-crock-pot-484624",
                                                              width = "100%"),
                                                    
                                                    
                                                    # url calculate button
                                                    
                                                    div(actionButton('btn3', 
                                                                       label = 'CALCULATE', 
                                                                       style="background-color:#7AA95C; color:white"
                                                                    ),
                                                        style = "padding-left:75%; padding-top: 43%; padding-bottom: 30px;"
                                                        ),
                                                    
                                                    # co2 score url
                                                    tags$style(type='text/css', '#urlout {background-color:#7AA95C; color: black;}'),
                                                    div(textOutput("urlout"),
                                                        style = "padding-left:15%; padding-right:20%; 
                                                                 padding-top: 10%; padding-bottom: 30px;",
                                                        align= "center"),
                                                 
                                                     )
                                                
                                                    
                                           
                                                    
                                                    
                                                    )
                                           ),
                        #Recommendations page              ############################################################## 
                                  tabPanel("Recommendations", 
                                           fluidRow(
                                             tags$head(
                                              tags$style("label{font-size:10px;height:10px;}")
                                             ),
                                            column(width = 6,
                                                  h2("CO2 footprint of your recipe...",
                                                     style="padding-top:50px;"
                                                     ),
                                                  br(),
                                                  h4(em("Placeholder"), 
                                                     align ="center",
                                                     style ="padding-top:30px; padding-bottom:30px; padding-left:30px;
                                                     padding-right:30px; border-style:solid; border-color:#7AA95C;"),
                                                  h4("Do you want to be more sustainable and learn more about 
                                                    food emissions? Look at the recommendations on the right 
                                                    side to lower your CO2 score, and browse to the next page
                                                    to have an interactive tool that lets you have some insights
                                                     about food emissions!", 
                                                     style="padding-top:50px"),
                                           ),
                                           column(width = 6,
                                                  style=" 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/recommendations.jpg?raw=true); 
                                                    background-size:cover;",
                                                  h2("Our recommendations for you...",
                                                     style="padding-top:50px;"
                                                     ),
                                                  h4("Instead of...", style= "padding-top:14px;",align="center"),
                                                  h4(em("Placeholder"), 
                                                     align ="center",
                                                     style ="padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                     padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                                                  h4("...change to",  align="center"),
                                                  h4(em("Placeholder"), 
                                                     align ="center",
                                                     style ="padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                     padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                                                  h2("New CO2 score after the swaps:", style= "padding-bottom: 15px;"),
                                                  h4(em("Placeholder"), 
                                                     align ="center",
                                                     style ="padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                     padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                                                 
                                                   #Button to learn more page
                                                  div(pageButtonUi2("prova"),
                                                      style = "padding-left:70%; padding-top: 10%; padding-bottom: 250px;"),
                                                  
                                                  
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
                                                 h4("Type an ingredient and select from the drop down menus its 
                                                    variety and certification, and see how the score changes!", 
                                                    style="padding-top:40px; padding-right:0px;"),
                                                 
                                                 #ingredient input
                                                 
                                                 div(
                                                   style = "display: grid; 
                                                      grid-template-columns: 25% repeat(4, 25%); 
                                                      grid-gap: 10px; padding-top:20px;",
                                                   
                                                   pickerInput(inputId = "textinR", 
                                                               label = "Ingredients", 
                                                               choices = c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini"), 
                                                               selected = "choice", 
                                                               options = list(`live-search` = TRUE)),
                                                   
                                                   pickerInput(inputId = "varietyR", 
                                                               label = "Variety", 
                                                               choices = c("canned","frozen", "fresh", "dry", "glass"), 
                                                               selected = "choose", 
                                                               options = list(`live-search` = TRUE)),
                                                   
                                                   pickerInput(inputId = "certificationR", 
                                                               label = "Certification", 
                                                               choices = c("organic","conventional", 
                                                                           "swiss integrated production"), 
                                                               selected = "choose", 
                                                               options = list(`live-search` = TRUE)),
                                                   
                                                   #placeholder for CO2 score
                                                   textOutput("C02")),
                                                   
                                                 
                                                   h4("See the overview about the ingredient’s relative CO2 scores
                                                       based on variety and certification",
                                                      style="padding-top:20px;"),
                                                
                                                 
                                          
                                                # Placeholder pie charts
                                                # fig1,
                                                # fig2,
                                                    
                                                
                                                 
                                                 #placeholder for relative CO2 score
                                        
                                          ),
                                          column(width = 6,
                                                 style=" 
                                                    background-image: url(https://github.com/ZverM7/front_end/blob/main/www/learnmore.jpg?raw=true); 
                                                    background-size:cover; padding-bottom:350px;",
                                                 h2("Did you know that...",
                                                    style="padding-top:50px;"
                                                 ),
                                                 h4("...the ingredients with less emissions are", 
                                                    align ="center",
                                                    style ="padding-top:40px;"),
                                                 h4(em("1) Sweet sorghum stem, with 0.0327 C02 Kg emissions per Kg 
                                                       of ingredient"), 
                                                    br(), 
                                                    em("2) Russet potatoes, with 0.0888 CO2 Kg emissions per Kg 
                                                       of ingredient"), 
                                                    br(), 
                                                    em("3) Paris market carrots, with 0.192 CO2 Kg emissions per Kg 
                                                       of ingredient"), 
                                                    align ="center",
                                                    style ="padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                    padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                                                 h4("...the ingredients with more emissions are",  
                                                    align="center"),
                                                 h4(em("1) Cocoa beans, with 19.7 C02 Kg emissions per Kg of ingredient"),
                                                    br(), 
                                                    em("2) Cheddar cheese, with 15.15499 CO2 Kg emissions per Kg 
                                                             of ingredient"), 
                                                    br(), 
                                                    em("3) Beef, with 14.9 CO2 Kg emissions per Kg of ingredient"), 
                                                    align ="center",
                                                    style ="padding-top:10px; padding-bottom:10px; padding-left:10px;
                                                    padding-right:10px; border-style:solid; border-color:#7AA95C;"),
                        
                                           ),
                                          )
                                          ),
                      
                                                           ############################
                       
                                )
                      )
  )
                      
                    
  
            

#############SERVER##########################################################

# Define server logic 
server <- function(input, output, session, e = "35.228.36.220", p="8080") {
  

#Button Homepage to calculator
  pageButtonServer("navbar", parentSession = session)
 
#Button Recommendations to Learn more
  pageButtonServer2("prova", parentSession = session) 

#add ingredients button
  observeEvent(input$add_btn, {
    insertUI(
      selector = "#add_btn",
      where = "beforeBegin",
      ui = tags$div(div(
        style = "display: grid; 
                                                      grid-template-columns: 30% repeat(3, 30%); 
                                                      grid-gap: 10px;",
        pickerInput(inputId = "textin4",  
                    choices = c("ingredient", "ackee", "acorn squash", "agave syrup", "all-purpose flour", "almond drink", "almonds", "anchovy", "anise", "apple", "apple juice", "apple vinegar", "apricot", "apricot", "artichoke", "arugula",  "avocado", "baby carrot", "baby corn", "baby peas", "baby spinach",  "bacon", "baking powder", "baking soda", "balsamic vinegar",  "banana", "banana squash", "barley", "barley", "barley", "barracuda",  "basa", "basil", "basil", "basmati rice", "bass", "bay leaves",  "bbq sauce", "beans", "beans", "beans", "beef", "beef broth", "beef cold cuts", "beef mince", "beer", "beetroot", "beetroot", "bell pepper", "bibb lettuce", "bitter melon", "black beans", "black beans", "black cod", "black-eyed beans", "blackberries", "blackcurrants", "blood orange", "blowfish", "blueberries", "blueberries",  "bluefish", "bok choy", "bombay duck", "borlotti beans", "borlotti beans", "boysenberries", "brandy", "bread baguette", "bream", "brill",  "broad beans", "broad beans", "broccoli", "brown lentils", "brown rice", "brown sugar", "brussel sprouts", "brussel sprouts", "bulgur", "burger bun", "butter", "butter fish", "buttermilk", "butternut squash", "cantaloupe", "caraway", "cardamom", "carrot", "cashew nuts", "catfish", "catnip", "cauliflower", "celeriac", "celery", "cheddar cheese", "cheese", "cheese emmentaler", "cherries", "cherry tomatoes", "chervil", "chestnuts", "chia seeds", "chicken", "chicken broth", "chicken cold cuts", "chicken nuggets", "chickpeas", "chickpeas", "chicory", "chili", "chives", "cilantro", "cinnamon", "clams",  "clementine", "cloudberries", "clove", "cockles", "cocoa beans", "cocoa powder", "coconut", "coconut milk", "coconut oil", "cod", "coffee", "coffee beans", "coffee powder", "cognac", "collard green", "corn", "corn", "corn", "corn", "corn kernel", "corn kernel", "corn kernel", "cornstarch", "cottonseed oil", "couscous", "crab", "crab apple", "cranberries", "crayfish", "cream", "cream cheese", "crispbread", "cucumber", "cumin", "curd cheese", "currants", "curry", "damson", "dark chocolate", "dates", "dates", "dates", "delicata squash", "dill", "dogfish", "dorade", "egg", "egg noodles",  "eggplant", "elderberries", "endive", "extravirgin olive oil", "fava beans", "fava beans", "fennel", "fig", "fish oil", "fish sauce",  "flounder", "french fries", "fuji apple", "full cream milk",  "full-fat margarine", "garlic", "gelatin", "gem squash", "ginger", "gnocchi", "goji berries", "gooseberries", "granulated sugar", "grapefruit", "grapes", "green asparagus", "green beans", "green beans", "green bell pepper", "green lentils", "green olives", "green onion", "grouper", "habanero", "haddock", "hake", "half-fat margarine",  "halibut", "ham", "hazelnuts", "herbs", "herring", "honey", "honeyberries", "honeydew", "horseradish", "hot dog", "hubbard squash", "ice","iceberg lettuce", "icing sugar", "jabuticabas", "jalapeno",  "jambul", "japanese plum", "jerusalem artichoke", "john dory", "jostaberries", "juniper berries", "kalamata olives", "kale", "kale", "ketchup", "kidney beans", "kidney beans", "kiwi", "kohlrabi","kumquat", "lamb", "lamb's lettuce", "langostino", "lasagna noodles", "leek", "lemon", "lemon balm", "lemon grass", "lemon juice","lemon zest", "lentils", "lentils", "lentils", "lettuce", "lime", "lingcod", "linguine", "linseed", "liquor", "low-fat curd cheese", "lupin flour", "macadamia nuts", "mackerel", "mahi mahi", "mandarine", "mango", "marionberries", "marjoram", "mayonnaise", "melon",  "milk", "milk chocolade", "millet", "mineral water", "mint", "miracle fruit", "mixed salad", "monkfish", "morel mushrooms",  "mozzarella", "mulberries", "mullet", "mung beans", "mung beans", "mushrooms", "mushrooms", "mussels", "mustard", "mustard leaves", "mustard seeds", "navy beans", "navy beans", "nectarine", "nutmeg", "oats", "oats drink", "okra", "olive oil", "olives", "onion", "onion powder", "orange", "orange juice", "orange peel",  "orange roughy", "oregano", "oregano", "oysters", "palm fruit",  "palm kernel oil", "palm oil", "panko", "papaya", "paprika",   "paris market carrot", "parmesan", "parsley", "passion fruit", "pasta", "pastry", "patagonian toothfish", "pea veggie burger", "peach", "peach", "peanut butter", "peanuts", "pear", "pearl barley",  "peas", "peas", "peas", "peas", "pecans", "pepper", "peppermint",  "perch", "philadelphia cream cheese", "pike", "pine nuts", "pineapple", "pineapple", "pineberries", "pinto beans", "pinto beans", "pistachios", "plum", "pollock", "pomfret", "pompano", "poppy seed", "porcini mushrooms", "pork", "portobello mushrooms", "potato", "potato", "potato",  "potato starch", "prawns", "puff pastry", "pumpkin", "quinoa",  "radish", "rape oil", "raspberries", "raspberries", "red bell pepper", "red cabbage", "red cabbage", "red chilli", "red meat", "red onion",  "red wine", "redcurrants", "rice", "ricotta", "rosemary", "runner beans",  "runner beans", "russet  potato", "russet  potato", "russet  potato", "rye", "rye", "rye", "saffron", "sage", "salal berries", "salmon",  "salmonberries", "salt", "sanddab", "sardine", "sausage", "savoy", "scallion", "scallops", "scampi", "schmand", "sea bass", "sea salt",  "sesame", "sesame oil", "shad", "shallot", "shiitake mushrooms", "shrimps", "skate", "skimmed milk", "sole", "sour cream", "soy curd",  "soy sauce", "soy veggie burger", "soybean drink", "soybean oil", "soybeans", "soybeans", "soybeans", "spaghetti squash", "spearmint",  "spelt drink", "spices", "spinach", "split peas", "split peas", "split peas", "sprat", "spring onion", "strawberries", "strawberries", "sturgeon", "sugar", "sugar snap peas", "sugar snap peas", "sugar snap peas", "sunflower oil", "sunflower seeds", "sunflower seeds", "surinam cherries","sweet potato", "sweet sorghum grain", "sweet sorghum stem",  "sweetcorn", "swordfish", "tabasco sauce", "tangerine", "tap water", "tarragon", "tawny port", "tea", "tempeh", "thuringian sausage", "thyme", "tilapia", "tilefish", "toast", "tofu", "tomato", "tomato", "tomatoes paste", "trout", "tuna", "turbot", "turmeric", "vanilin",  "vanilla", "vanilla extract", "vegan sausage", "vegan spreadable fat", "vegetable stock", "veggie nugget", "veggie patty", "venison", "walnuts", "wasabi", "watermelon", "wheat berries", "wheat bread", "wheat bread bun", "whey", "whipping cream", "white asparagus",  "white cabbage", "white chocolate", "white currants", "white radish", "white wine", "white wine vinegar", "whitebait", "whitefish",  "whiting", "wholegrain bread", "wholegrain bread bun", "wholegrain toast", "wholewheat flour", "wholewheat noodles", "wild mushrooms", "wild rice", "wine", "yeast", "yeast extract", "yellow bell pepper", "yogurt", "zucchini"), 
                    selected = "choice", 
                    options = list(`live-search` = TRUE)),
        
        textInput("quantityin2",NULL, 
                  placeholder = "e.g. 200" ),
        
        pickerInput(inputId = "unitin4",  
                    choices = c("#", "bottle", "bunch", "bunches", "c", "can", "cans", "carton", "cartons", "cc", "centimeter", "centimeters", "centimetre", "centimetres", "chunk", "chunks", "cl", "cm", "cms", "cube", "cup", "cups", "dash", "dashes", "dc", "deci", "deciliter", "deciliters", "dl", "drop", "drops", "fl oz", "fl pt", "fl qt", "fluid ounce", "g",  "gal", "gallon", "gallons", "gill", "gram", "grams", "gs", "head", "inch", "inches", "jar", "jars", "jigger", "k", "kg", "kgs", "kilo", "kilogram", "kilogramme", "kilogrammes", "kilograms",  "kilos", "knob", "l", "lb", "lbs", "leaf", "leaves", "liter", "liters", "litre", "litres", "lts", "mg", "mgs", "milligram", "milligramme", "milligrammes", "milliliter", "milliliters", "millilitre", "millimeter", "millimeters", "millimetre", "millimetres", "ml",  "mls", "mm", "ounce", "ounces", "oz", "p", "package", "packages",  "packet", "packets", "piece", "pieces", "pinch", "pinches", "pint",  "pints", "pkt", "pod", "pods", "portion", "portions", "pound",  "pounds", "pt", "q", "qt", "quart", "sachet", "sachets", "scoop",  "scoops", "sheet", "sheets", "sized", "slice", "slices", "sprig", "sprigs", "stalk", "stalks", "stick", "sticks", "strip", "strips",  "t", "tablespoon", "tablespoons", "tbl", "tbs", "tbsp", "teaspoon", "teaspoons", "tin", "tsp", "wedge", "wedges"), 
                    selected = "choose", 
                    options = list(`live-search` = TRUE)),
      ))
    )
  })
      
#co2 score from ingredients
  observeEvent(input$btn2, {
    output$ingredout <- renderText({ 
      "#backend"
      #paste(input$textin, input$textin2, input$textin3)
      })
    #pickerinput as datatable
   #pidt <- data.table("Ingredient" = c(input$textin, input$textin2, input$textin3),    
                      #"Quantity" = double(),
                      #"Unit" = character())
})

    
#url 
  observeEvent(input$btn3, {
    output$urlout <- renderText({
     base = paste0("http://", e,":", p,"/")
     r <- httr::GET(url=base,
                    path="get_table",
                    query=list(foodlink=input$urlin), verbose()
                    )
     fromJSON(content(r, "text"))
   }) 
   })
  
  }
 


#############################################################################

# Run the application 
shinyApp(ui = ui, server = server)
  
}

