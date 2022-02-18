library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tidyverse)
library(readxl)

# Load and clean data ----------------------------------------------
starwars.load <- starwars %>%
    mutate(films = as.character(films),
           vehicles = as.character(vehicles),
           starships = as.character(starships),
           name = as.factor(name))

top.employers <- read_excel('Data/top_employers.xlsx') %>%
    mutate(Rank = as.numeric(Rank),
           Company = as.character(Company),
           Employees = as.numeric(Employees),
           Industry = as.character(Industry))

wages <- read_excel('Data/nlihc.xlsx') %>%
    mutate(Occupation = as.character(Occupation),
           Wage = as.numeric(Wage),
           Employment = as.numeric(Employment))

wages <- wages[, c('Occupation', 'Employment', 'Wage')]

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Baltimore Work")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # Menu Items ----------------------------------------------
        menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
        menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
        menuItem("Top Employers", tabName = "employers"),
        menuItem("Wages by Occupation", tabName = "wages"),
        
        # Inputs: select variables to plot ----------------------------------------------
        selectInput("worldSelect",
                    "Homeworld:",
                    choices = sort(unique(starwars.load$homeworld)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("Naboo", "Tatooine")),
        
        # Birth year Selection ----------------------------------------------
        sliderInput("birthSelect",
                    "Birth Year:",
                    min = min(starwars.load$birth_year, na.rm = T),
                    max = max(starwars.load$birth_year, na.rm = T),
                    value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
                    step = 1),
        
        # Number of employers selection ----------------------------------------------
        sliderInput("employerSelect",
                    "Top Employers:",
                    min = 1,
                    max = 30,
                    value = c(1,30),
                    step = 1),
        
        # Select occupation to highlight
        selectInput(inputId = "occ",
                    label = "Occupation:",
                    choices = unique(wages$Occupation),
                    selected = "WAITERS AND WAITRESSES")
    )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
    
    # Plot page ----------------------------------------------
    tabItem("plot",
            
            # Input and Value Boxes ----------------------------------------------
            fluidRow(
                infoBoxOutput("mass"),
                valueBoxOutput("height")
            ),
            
            # Plot ----------------------------------------------
            fluidRow(
                tabBox(title = "Plot",
                       width = 12,
                       tabPanel("Mass", plotlyOutput("plot_mass")),
                       tabPanel("Height", plotlyOutput("plot_height")))
            )
    ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
    ),
    
    # Employers page ----------------------------------------------
    tabItem("employers",
            fluidRow(
                tabBox(title = "Plot",
                       width = 12,
                       tabPanel("Employers", plotlyOutput("plot_employers"))
            ))),
            
    # Wage by Occupation page ----------------------------------------------
    tabItem("wages",
            fluidRow(
                tabBox(title = "Plot",
                       width = 12,
                       tabPanel("Wages", plotlyOutput("plot_wages"))
            )))
            
))


ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    
    # Reactive data function -------------------------------------------
    swInput <- reactive({
        starwars <- starwars.load %>%
            
            # Slider Filter ----------------------------------------------
        filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
        
        # Homeworld Filter ----------------------------------------------
        if (length(input$worldSelect) > 0 ) {
            starwars <- subset(starwars, homeworld %in% input$worldSelect)
        }
        
        # Return dataframe ----------------------------------------------
        return(starwars)
    })
    
    # Reactive top employer function -------------------------------------------
    top.employers.imp <- reactive({
        top.employers[which(top.employers$Rank >= input$employerSelect[1] & 
                             top.employers$Rank <= input$employerSelect[2]),]
    })
    
    # Create a new column to update colors on scatter plot based on which occupation selected
    wages.imp <- reactive({
        wages %>%
            add_column(z = wages$Occupation == input$occ)
    })
    
    # Reactive melted data ----------------------------------------------
    mwInput <- reactive({
        swInput() %>%
            melt(id = "name")
    })
    
    # A plot showing the mass of characters -----------------------------
    output$plot_mass <- renderPlotly({
        dat <- subset(mwInput(), variable == "mass")
        
        # Generate Plot ----------------------------------------------
        ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
    })
    
    # A plot showing top employers -----------------------------
    output$plot_employers <- renderPlotly({
        
        # Generate Plot ----------------------------------------------
        ggplot(data = top.employers.imp(), aes(x=reorder(Company, Employees), y=Employees)) + 
            geom_bar(stat = "identity") + 
            coord_flip() + 
            labs(x='') + 
            ggtitle('Top Private Sector Employers')
    })
    
    # A plot showing wages by occupation -----------------------------
    output$plot_wages <- renderPlotly({
        
        # Generate Plot ----------------------------------------------
        ggplot(wages.imp(), aes(x=Wage, y=Employment, color=z)) + 
            geom_point() + 
            theme(legend.position = 'none') + 
            ggtitle('Wages and Employment by Occupation')
    })
    
    # A plot showing the height of characters -----------------------------------
    output$plot_height <- renderPlotly({
        dat <- subset(mwInput(),  variable == "height")
        ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
    })
    
    # Data table of characters ----------------------------------------------
    output$table <- DT::renderDataTable({
        subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
    })
    
    # Mass mean info box ----------------------------------------------
    output$mass <- renderInfoBox({
        sw <- swInput()
        num <- round(mean(sw$mass, na.rm = T), 2)
        
        infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
    })
    
    # Height mean value box ----------------------------------------------
    output$height <- renderValueBox({
        sw <- swInput()
        num <- round(mean(sw$height, na.rm = T), 2)
        
        valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
    })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)