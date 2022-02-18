library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tidyverse)
library(readxl)

# Load and clean data ----------------------------------------------
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

industry <- read_excel('Data/Industry.xlsx') %>%
    mutate(Industry = as.character(Industry),
           Establishments = as.numeric(Establishments),
           Public = as.numeric(Public))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Baltimore Work")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # Menu Items ----------------------------------------------
        menuItem("Top Employers", tabName = "employers"),
        menuItem("Top Industries", tabName = "industry"),
        menuItem("Wages by Occupation", tabName = "wages"),
        
        # Number of employers selection ----------------------------------------------
        sliderInput("employerSelect",
                    "Top Employers:",
                    min = 1,
                    max = 30,
                    value = c(1,30),
                    step = 1),
        
        # Show public sector selection
        checkboxInput(inputId = "show_public",
                      label = "Show public sector",
                      value = TRUE),
        
        # Select occupation to highlight
        selectInput(inputId = "occ",
                    label = "Occupation:",
                    choices = unique(wages$Occupation),
                    selected = "WAITERS AND WAITRESSES")
    )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
    
    # Employers page ----------------------------------------------
    tabItem("employers",
            
            # Input and Value Boxes ----------------------------------------------
            fluidRow(
                valueBoxOutput("jobs"),
                valueBoxOutput("wages"),
                valueBoxOutput("establishments")
            ),
            
            # Plot and data ----------------------------------------------------
            fluidRow(
                tabBox(title = "Employers",
                       width = 12,
                       tabPanel("Plot", plotlyOutput("plot_employers")),
                       tabPanel("Data", DT::dataTableOutput("empTable"))
            ))),
            
    # Wage by Occupation page ----------------------------------------------
    tabItem("wages",
            fluidRow(
                tabBox(title = "Wages",
                       width = 12,
                       tabPanel("Plot", plotlyOutput("plot_wages")),
                       tabPanel("Data", DT::dataTableOutput("wageTable"))
            ))),
    
    # Industry page ----------------------------------------------
    tabItem("industry",
            fluidRow(
                tabBox(title = "Industries",
                       width = 12,
                       tabPanel("Plot", plotlyOutput("plot_industry")),
                       tabPanel("Data", DT::dataTableOutput("industryTable"))
                )))
            
))


ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {

    # Reactive top employer function -------------------------------------------
    top.employers.imp <- reactive({
        top.employers[which(top.employers$Rank >= input$employerSelect[1] & 
                             top.employers$Rank <= input$employerSelect[2]),]
    })
    
    # Reactive selected occupation column
    wages.imp <- reactive({
        wages %>%
            add_column(z = wages$Occupation == input$occ)
    })
    
    # Reactive show public sector data -------------------------------------------
    industry.imp <- reactive({
        if (input$show_public==0) {
            industry %>%
                filter(Public != 1)
        }
        else {
            industry
        }
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
    
    # A plot showing industries -----------------------------
    output$plot_industry <- renderPlotly({
        
        # Generate Plot ----------------------------------------------
        ggplot(data = industry.imp(), aes(x=reorder(Industry, Establishments), y=Establishments)) + 
            geom_bar(stat = "identity") + 
            coord_flip() + 
            labs(x='') + 
            ggtitle('Top Industries')
    })
    
    # Data table of employers ----------------------------------------------
    output$empTable <- DT::renderDataTable({
        top.employers.imp()
    })
    
    # Data table of wages ----------------------------------------------
    output$wageTable <- DT::renderDataTable({
        wages
    })
    
    # Data table of industries ----------------------------------------------
    output$industryTable <- DT::renderDataTable({
        industry.imp()
    })
    
    # Total jobs value box ----------------------------------------------
    output$jobs <- renderValueBox({
        valueBox(subtitle = "Jobs", value = "352.7K", color = "blue")
    })
    
    # Median wage value box ----------------------------------------------
    output$wages <- renderValueBox({
        valueBox(subtitle = "Median Wage", value = "$24.03", color = "blue")
    })
    
    # Total Establishments box ----------------------------------------------
    output$establishments <- renderValueBox({
        valueBox(subtitle = "Establishments", value = "13.7K", color = "blue")
    })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)