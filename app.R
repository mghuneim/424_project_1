#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)

newStates <- read.csv("annual_generation_state.csv", header = TRUE)
newStates$GENERATION..Megawatthours. <- gsub(",","", newStates$GENERATION..Megawatthours.)
newStates$GENERATION..Megawatthours. <- as.numeric(newStates$GENERATION..Megawatthours.)

newStates <- subset(newStates, newStates$STATE != "  ")
newStates$STATE <- factor(newStates$STATE)

newStates <- subset(newStates, newStates$GENERATION..Megawatthours. >= 0)

newStates$STATE <- as.factor(newStates$STATE)

newStates <- subset(newStates, (newStates$ENERGY.SOURCE != "Other") & 
                        (newStates$ENERGY.SOURCE != "Other Gases") &
                        (newStates$ENERGY.SOURCE != "Other Biomass") &
                        (newStates$ENERGY.SOURCE != "Pumped Storage"))


newStates$ENERGY.SOURCE <- factor(newStates$ENERGY.SOURCE)

newStates$STATE <- toupper(newStates$STATE)

levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Hydroelectric Conventional"] <- "Hydro"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Natural Gas"] <- "Gas"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"


total <- subset(newStates, (newStates$ENERGY.SOURCE == "Total") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))

at <- notTotals %>%
    group_by(YEAR) %>%
    ungroup()

nt <- notTotals %>%
    group_by(YEAR) %>%
    mutate(pc = GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.)) %>%
    ungroup()

years <- c(1990:2019)
sources <- levels(newStates$ENERGY.SOURCE)
states <- c(state.name, c("US-TOTAL", "Washington D.C."))


ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Power and the Passion"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                     sidebarMenu(
                         menuItem("Bar Graphs", tabName = "bars", icon = icon("bar-chart")),
                         menuItem("Line Graphs", tabName = "lines", icon = icon("line-chart")),
                         menuItem("Tables", tabName = "tables", icon = icon("table")),
                         menuItem("Split Screen Comparisons", tabName = "split", icon = icon("th")),
                         menuItem("About", tabName = "credits", icon = icon("check"))
                     ),
                     sidebarMenuOutput("menu"),
                     selectInput("Year (Side 1)", "Select the year to visualize", years, selected = 2019),
                     selectInput("State (Side 1)", "Select a state", states, selected = "Illinois"),
                     selectInput("Year (Side 2)", "Select the year to visualize", years, selected = 2019),
                     selectInput("State (Side 2)", "Select a state", states, selected = "Illinois"),
                     selectInput("Source", "Select an energy source", sources, selected = "Coal")
                     ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "bars",
                    fluidRow(
                        column(4,
                               fluidRow(
                                   box(title = "Bar Graph of the Amount of Energy Sources (1990-2019)", solidHeader = TRUE,
                                       status = "primary", width = 12,
                                       plotOutput("plot1", height = 200)
                                   )
                               ),
                               fluidRow(
                                   box(title = "Bar Graph of the Percentage of Energy Sources (1990-2019)", solidHeader = TRUE,
                                       status = "primary", width = 12,
                                       plotOutput("plot2", height = 200)
                                   )
                               )
                        )
                    )
        ),
        tabItem(tabName = "lines",
            column(4,
                   fluidRow(
                       box( title = "Line Chart of the Amount of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("plot3", height = 200)
                       )
                   ),
                   fluidRow(
                       box( title = "Line Chart of the Percentages of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("plot4", height = 200)
                       )
                )
            ),
            column(4,
                   fluidRow(
                   checkboxGroupInput("checkGroup1",
                                      h4("Select which energy source to output"),
                                      choices = list("All" = 1,
                                                     "Coal" = 2,
                                                     "Geothermal" = 3,
                                                     "Hydro" = 4,
                                                     "Gas" = 5,
                                                     "Nuclear" = 6,
                                                     "Petroleum" = 7,
                                                     "Solar" = 8,
                                                     "Wind" = 9,
                                                     "Wood" = 10),
                                      selected = 1),
                   plotOutput("amounts")
                   )
            )
        ),
        tabItem(tabName = "tables",
                fluidRow(
                    column(4,
                           fluidRow(
                               box(title = "Amounts of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                   dataTableOutput("tab1", height = 200)
                               )
                           ),
                           fluidRow(
                               box(title = "Percentages of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                   dataTableOutput("tab2", height = 200)
                               )
                           )
                           )
                )
                ),
        tabItem(tabName = "credits",
            h1("This app was created by Matthew Ghuneim from a CSV file given and this was finished on blah blah")
        )
        )
    )
    )

server <- function(input, output) {
    
    # reactive functions
    theme_set(theme_grey(base_size = 18)) 
    barReactive <- reactive({subset(newStates, (newStates$YEAR == input$Year) & (newStates$STATE == input$State) & (newStates$ENERGY.SOURCE != "Total"))})
    sourceReactive <- reactive({subset(newStates, (newStates$ENERGY.SOURCE == input$Source) & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))})
    amountReactive <- reactive({dcast(at, YEAR)})
    percentReactive <- reactive({dcast(nt, YEAR~ENERGY.SOURCE)})
    
    # bar graphs 
    output$plot1 <- renderPlot({
        notTotals <- barReactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            geom_bar(position = 'stack', stat = 'identity') + labs(fill = "Energy Sources")
    })
    output$plot2 <- renderPlot({
        notTotals <- barReactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            geom_bar(position = 'fill', stat = 'identity') + scale_y_continuous(labels = scales::percent) + labs(fill = "Energy Sources")
    })
    
    # line graphs
    output$plot3 <- renderPlot({
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            geom_line(stat = "summary", fun = "sum") + labs(color = "Energy Sources") + theme_bw()
    })
    output$plot4 <- renderPlot({
        ggplot(nt, aes(x = YEAR, y = pc, group = ENERGY.SOURCE, color = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            geom_line(stat = "summary", fun = "sum") + scale_y_continuous(labels = scales::percent) + labs(color = "Energy Sources") + theme_bw()
    })
    
    # tables
    output$tab1 <- DT::renderDataTable(
        DT::datatable({ 
            notTotals <- barReactive()
            as.data.frame(notTotals)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'asc'))
        ), rownames = FALSE 
        )
    )
    output$tab2 <- DT::renderDataTable(
        DT::datatable({ 
           nt <- percentReactive()
           as.data.frame(nt)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'asc'))
        ), rownames = FALSE 
        )
    )
    #checkbox
    output$amounts <- renderPlot({
        notTotals <- sourceReactive()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
