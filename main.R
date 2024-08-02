library("readr")
library("leaflet")
library("ggplot2")
library("shiny")
library("bslib")
library("scales")
library("dplyr")

# install and activate packages
# imported_packages <- c(
#   "readr", "leaflet", "ggplot2", "shiny", "bslib", "scales", "dplyr"
# )
# 
# installed_packages <- rownames(installed.packages())
# for (i in imported_packages) {
#   if (!(i %in% installed_packages)) {
#     install.packages(i)
#   }
#   library(i, character.only = TRUE)
# }

# import data
f <- read_csv(
  file = "data.csv",
  col_types = cols(Date = col_date(format = "%d/%m/%Y"))
)

# define the UI
ui <- navbarPage(
  
  # CSS
  header = tags$style(HTML("
    #search {
      background-color: #007BFF; 
      color: white;
    }
    #reset {
      background-color: #DC3545;
      color: white;
    }
    #message {
      color: red;
      font-style: italic; 
      text-align: center; 
    }
  ")),
  
  # define settings for the navigation bar
  title = strong("Melbourne House Market"),
  windowTitle = "Melbourne House Market",
  position = "static-top",
  theme = bs_theme(bootswatch = "flatly", version = 4),
  lang = "en",
  
  # create the Map tab panel
  shiny::tabPanel(
    title = "Map",
    
    # create the sidebar layout for the Map page
    sidebarLayout(
      position = "left",
      
      # creates the Price plot
      sidebarPanel(
        
        # creates the plot placeholder
        plotOutput(
          outputId = "price",
          height = "150px",
          width = "100%"
        ),
        
        # creates the error message placeholder
        textOutput(
          outputId = "message"
        ),
        
        # create the Property Type radio buttons
        radioButtons(
          inputId = "property_type",
          label = strong("Property Type"),
          choiceNames = c("House", "Townhouse", "Apartment Unit"),
          choiceValues = c("h", "t", "u"),
          selected = "h"
        ),
        
        # create the Region checkbox group
        checkboxGroupInput(
          inputId = "region",
          label = strong("Area"),
          choiceNames = c("Metropolitan", "Regional"),
          choiceValues = c("m", "r"),
          selected = c("m", "r")
        ),
        
        # create the numerical input for price
        numericInput(
          inputId = "min_price",
          label = "Min Budget",
          value = 0
        ),
        numericInput(
          inputId = "max_price",
          label = "Max Budget",
          value = 9000000
        ),
        actionButton(
          inputId = "search",
          label = "Search",
        ),
        actionButton(
          inputId = "reset",
          label = "Reset",
        )
      ),
      
      # create the Leaflet map main panel placeholder
      mainPanel(
        leafletOutput(
          outputId = "map",
          width = "100%",
          height = 600
        )
      )
    )
  ),
  
  # create the About tab panel
  shiny::tabPanel(
    title = "About",
    
    # CSS
    tags$style(HTML("
    .about-section {
      padding: 10px 20px;
      border-bottom: 1px solid #e1e1e1;
      margin-bottom: 10px;
    }
    .about-section:last-child {
      border-bottom: none;
    }
    .about-title {
      font-weight: bold;
      font-size: 1.2em;
      margin-bottom: 8px;
      line-height: 2
    }
    .about-content {
      margin-left: 10px;
      line-height: 2;
    }")),
    
    # About the Page
    div(class = "about-section",
        div(class = "about-title", "Melbourne House Market Overview (2016–2017)"),
        div(class = "about-content", 
            "This map offers a comprehensive insight into the housing market of 
            Melbourne, Victoria, Australia, during 2016 and 2017, which may be 
            especially useful for users interested in property trade. From 
            this map, users can glean the following insights:"
        ),
        div(class = "about-content",
            tags$ul(
              tags$li(
                strong("Geographical distribution:"), 
                " Understand where properties were predominantly located."
              ),
              tags$li(
                strong("Price distribution:"), 
                " Get a sense of the price range across different properties."
              ),
              tags$li(
                strong("Property types:"), 
                " Differentiate between houses, townhouses, and apartment units."
              ),
              tags$li(
                strong("Areas:"), 
                " Identify properties in metropolitan and regional areas."
              ),
              tags$li(
                strong("Budget estimation:"), 
                " Filter properties based on budget preferences."
              ),
              tags$li(
                strong("Detailed property information:"), 
                " For each property, view its location, features such as the 
                number of rooms and parking spaces, and its price."
              )
            )
        )
    ),
    
    
    # About the Design Elements
    div(class = "about-section",
        div(class = "about-title", "Design Elements"),
        div(class = "about-content", "The design elements used by this page are from:"),
        div(
          class = "about-content",
          tags$ul(
            tags$li(
              "Icons: ", tags$a(href = "https://www.flaticon.com/free-icons/search-engine", "Flaticon")
            ),
            tags$li(
              "Themes: ", tags$a(href = "https://bootswatch.com/","Bootswatch")
            )
          )
        )
    ),
    
    # About the Data
    div(class = "about-section",
        div(class = "about-title", "Data Source"),
        div(
          class = "about-content", 
          "The data on this page are mainly from the ",
          tags$a(
            href = "https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot", 
            "Melbourne Housing Snapshot"),
          " by Pino (2017)."
        )
    )
  ),
)

# define the server
server <- function(input, output, session){
  
  # filter data based on user input
  filtered_price <- reactiveVal(f)
  observeEvent(input$search, {
    tmp_f <- f
    
    min_budget <- ifelse(is.na(input$min_price) || input$min_price == "", 0, input$min_price)
    max_budget <- ifelse(is.na(input$max_price) || input$max_price == "", 9000000, input$max_price)
    
    data <- tmp_f[tmp_f$Price >= min_budget & tmp_f$Price <= max_budget, ]
    filtered_price(data)
  })
  observeEvent(input$reset, {
    tmp_f <- f
    data <- tmp_f[, ]
    filtered_price(data)
    
    updateNumericInput(session = session, inputId = "min_price", value = 0)
    updateNumericInput(session = session, inputId = "max_price", value = 9000000)
  })
  
  # reactive part
  filtered_f <- reactive({
    tmp_f <- filtered_price()
    
    # filter Property Types
    data <- tmp_f[tmp_f$Type == input$property_type, ]
    
    # filter Regionnames
    if ("m" %in% input$region && !"r" %in% input$region) {
      data <- data[grepl("metropolitan", data$Regionname, ignore.case = TRUE), ]
    } else if (!"m" %in% input$region && "r" %in% input$region) {
      data <- data[!grepl("metropolitan", data$Regionname, ignore.case = TRUE), ]
    } else if (!"m" %in% input$region && !"r" %in% input$region){
      data <- data.frame()
    }
    return(data)
  })
  
  # render the Leaflet map
  output$map <- renderLeaflet({
    ff <- filtered_f()
    
    # returns a blank map if the user deselects all checkbox choices
    if (nrow(ff) == 0) {
      return(
        leaflet() |> 
          setView(lng = 144.9975, lat = -37.80, zoom = 9) |> 
          addTiles() |>
          addProviderTiles(providers$CartoDB) |>
          addMiniMap(
            tiles = providers$OpenStreetMap,
            toggleDisplay = TRUE,
            zoomAnimation = TRUE,
            width = 200,
            height = 150,
            position = "bottomright",
          ) |>
          addMeasure(
            position = "topleft",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#333333",
          ) |>
          addScaleBar(position = "bottomleft", options = scaleBarOptions())
      )
    }
    
    # display the map 
    map_melb <- leaflet(ff) |> 
      setView(lng = 144.9975, lat = -37.80, zoom = 9) |>
      addTiles() |>
      addProviderTiles(providers$CartoDB) |>
      addMiniMap(
        tiles = providers$OpenStreetMap,
        toggleDisplay = TRUE,
        zoomAnimation = TRUE,
        width = 200,
        height = 150,
        position = "bottomright",
      ) |>
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#333333",
      ) |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions())
    
    # add markers based on radio-button choices
    if("u" == input$property_type) {
      map_melb <- map_melb |>
        addMarkers(
          data = ff[ff$Type == "u",],
          lng = ~Longtitude,
          lat = ~Lattitude,
          icon = makeIcon(
            iconUrl = "icons/apartment.png", iconWidth = 25, iconHeight = 25),
          clusterOptions = markerClusterOptions(),
          group = "Apartment Unit",
          label = "Apartment Unit",
          popup = ~paste(
            "<strong>", Address, Suburb, Postcode, "</strong><br>",
            Bedroom2, "bed,", Bathroom, "bath,", Car, "car", "<br>",
            "<br>",
            "💰", format(Price, big.mark = ","), "AUD"
          ),
        )
    } else if("t" == input$property_type) {
      map_melb <- map_melb |>
        addMarkers(
          data = ff[ff$Type == "t",],
          lng = ~Longtitude,
          lat = ~Lattitude,
          icon = makeIcon(
            iconUrl = "icons/townhouse.png", iconWidth = 25, iconHeight = 25),
          clusterOptions = markerClusterOptions(),
          group = "Townhouse",
          label = "Townhouse",
          popup = ~paste(
            "<strong>", Address, Suburb, Postcode, "</strong><br>",
            Bedroom2, "bed,", Bathroom, "bath,", Car, "car", "<br>",
            "<br>",
            "💰", format(Price, big.mark = ","), "AUD"
          ),
        )
    } else if("h" == input$property_type) {
      map_melb <- map_melb |>
        addMarkers(
          data = ff[ff$Type == "h",],
          lng = ~Longtitude,
          lat = ~Lattitude,
          icon = makeIcon(
            iconUrl = "icons/house.png", iconWidth = 25, iconHeight = 25),
          clusterOptions = markerClusterOptions(),
          group = "House",
          label = "House",
          popup = ~paste(
            "<strong>", Address, Suburb, Postcode, "</strong><br>",
            Bedroom2, "bed,", Bathroom, "bath,", Car, "car", "<br>",
            "<br>",
            "💰", format(Price, big.mark = ","), "AUD"
          ),
        )
    }
  })
  
  # render the density plot about price
  output$price <- renderPlot({
    ff <- filtered_f()
    
    price_plot <- ggplot(ff) + 
      geom_histogram(bins = 30, fill = "#18B07C", alpha = 0.7) +
      labs(x = "Price", y = "Properties") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#F7F7F7", color = NA),
        panel.border = element_blank(),
        panel.grid = element_blank()
      )
    
    # display the message when the data frame is blank
    if (nrow(ff) == 0){
      output$message <- renderText({
        "Sorry, no such property!"
      })
      
      price_plot +
        xlim(0, 1) +
        ylim(0, 1) +
        scale_x_continuous(breaks = c(0, 1)) +
        scale_y_continuous(breaks = c(0, 1))
      
    } else {
      output$message <- renderText({
        "\u00A0" # Unicode for nonbreaking space
      })
      
      price_plot +
        aes(x = Price) +
        scale_x_continuous(
          breaks = c(min(ff$Price), max(ff$Price)),
          labels = label_number(scale = 1e-6, suffix = "M")
        ) +
        scale_y_continuous(
          labels = scales::comma
        )
    }
  })
}

# run the Shiny app
shinyApp(ui = ui, server = server)
