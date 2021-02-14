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
library(usmap)

# read in the csv file, get rid of the commas and convert the generation values to numeric
newStates <- read.csv("https://www.evl.uic.edu/aej/424/annual_generation_state.csv", header = TRUE)
newStates$GENERATION..Megawatthours. <- gsub(",","", newStates$GENERATION..Megawatthours.)
newStates$GENERATION..Megawatthours. <- as.numeric(newStates$GENERATION..Megawatthours.)

# get rid of the missing values 
newStates <- subset(newStates, newStates$STATE != "  ")
newStates$STATE <- factor(newStates$STATE)

# get rid of the negative values 
newStates <- subset(newStates, newStates$GENERATION..Megawatthours. >= 0)

newStates$STATE <- as.factor(newStates$STATE)

# getting rid of these extra columns for simplicity 
newStates <- subset(newStates, (newStates$ENERGY.SOURCE != "Other") & 
                        (newStates$ENERGY.SOURCE != "Other Gases") &
                        (newStates$ENERGY.SOURCE != "Other Biomass") &
                        (newStates$ENERGY.SOURCE != "Pumped Storage"))


newStates$ENERGY.SOURCE <- factor(newStates$ENERGY.SOURCE)

# capitalizing the states so that US-TOTAL and US-Total are the same 
newStates$STATE <- toupper(newStates$STATE)

# renaming some of the energy sources so it is easier to read
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Hydroelectric Conventional"] <- "Hydro"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Natural Gas"] <- "Gas"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"

# subsetting the dataset so that no total values for energy sources show up
notTotals <- subset(newStates, (newStates$ENERGY.SOURCE != "Total") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
df <- data.frame(notTotals$GENERATION..Megawatthours., notTotals$YEAR, notTotals$STATE, notTotals$ENERGY.SOURCE)

# making a dataset to store the percentages 
nt <- notTotals %>%
    group_by(YEAR) %>%
    mutate(pc = GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.)) %>%
    ungroup() %>%
    select(YEAR, pc, STATE, ENERGY.SOURCE)

# these will be useful for the menus when the user has to pick and choose what they want to output
years <- c(1990:2019)
sources <- levels(newStates$ENERGY.SOURCE)
years2 <- c(1990:2019)
sources2 <- levels(newStates$ENERGY.SOURCE)

states <- setNames(state.abb, state.name)[state.name]
states <- append(states, c("US-TOTAL" = "US-TOTAL", "Washington DC" = "DC"))

states2 <- setNames(state.abb, state.name)[state.name]
states2 <- append(states2, c("US-TOTAL" = "US-TOTAL", "Washington DC" = "DC"))

# converting the sources and states into inputs for the reactive functions
boxSources <- unique(subset(newStates$ENERGY.SOURCE, newStates$ENERGY.SOURCE != "Total"))
boxSources <- as.character(boxSources)

# create the shiny dashboard
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Power and the Passion"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                     # menu on the left hand side
                     sidebarMenu(
                         menuItem("Bar Graphs", tabName = "bars", icon = icon("bar-chart")),
                         menuItem("Line Graphs", tabName = "lines", icon = icon("line-chart")),
                         menuItem("Tables", tabName = "tables", icon = icon("table")),
                         menuItem("Split Screen Comparisons", tabName = "split", icon = icon("th")),
                         menuItem("Heat Maps", tabName = "heat", icon = icon("fire")),
                         menuItem("About", tabName = "credits", icon = icon("check"))
                     ),
                     # dropdown menus
                     sidebarMenuOutput("menu"),
                     selectInput("Year", "Select the first year to visualize", years, selected = 2019),
                     selectInput("Year2", "Select the second year to visualize", years2, selected = 2019),
                     selectInput("State", "Select one state", states, selected = "US-TOTAL"),
                     selectInput("State2", "Select another state", states2, selected = "IL"),
                     selectInput("Source", "Select an energy source", sources, selected = "Coal"),
                     selectInput("Source2", "Select another energy source", sources2, selected = "Coal")
                     ),
    dashboardBody(
        tabItems(
            # stacked bar charts
            tabItem(tabName = "bars",
                    fluidRow(
                        column(6,
                               fluidRow(
                                   box(title = "Bar Graph of the Amount of Energy Sources (1990-2019)", solidHeader = TRUE,
                                       status = "primary", width = 12,
                                       plotOutput("plot1", height = 300)
                                   )
                               )
                        ),
                        column(6,
                               fluidRow(
                                   box(title = "Bar Graph of the Percentage of Energy Sources (1990-2019)", solidHeader = TRUE,
                                       status = "primary", width = 12,
                                       plotOutput("plot2", height = 300)
                                   )
                               )
                        )
                    )
        ),
        #line charts
        tabItem(tabName = "lines",
            column(4,
                   fluidRow(
                   checkboxGroupInput("checkGroup1",
                                      h4("Select which energy source to output"),
                                      choices = c("All", boxSources),
                                      selected = "All"),
                   plotOutput("amounts")
                   )
            ),
            column(4,
                   fluidRow(
                       checkboxGroupInput("checkGroup2",
                                          h4("Select which energy source percentage to output"),
                                          choices = c("All", boxSources),
                                          selected = "All"),
                       plotOutput("amounts2")
                   )
            ),
            column(4,
                   h2("NOTE: You must uncheck All for either chart if you would like to filter out the other sources."))
        ),
        #tables
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
        #split screen
        tabItem(tabName = "split",
                    #first column (left side)
                    column(3,
                           fluidRow(
                               box(title = "Bar Graph 1 of the Amount of Energy Sources (1990-2019)", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot5", height = 150)
                           ),
                               box(title = "Bar Graph 1 of the Percentage of Energy Sources (1990-2019)", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot6", height = 150)
                               )
                           ),
                           fluidRow(
                               box(title = "Table 1 of Amounts of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                   dataTableOutput("tab3", height = 150)
                               )
                           )
                    ),
                # second column (left side)
                column(3,
                       fluidRow(
                           box( title = "Line Chart 1 of the Amount of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                plotOutput("plot7", height = 150)
                           )
                       ),
                       fluidRow(
                           box( title = "Line Chart 1 of the Percentages of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                plotOutput("plot8", height = 150)
                           )
                       ),
                       fluidRow(
                           box(title = "Table 1 of Percentages of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                               dataTableOutput("tab4", height = 150)
                           )
                       )
                ),
                # first column (right side)
                    column(3,
                           fluidRow(
                               box(title = "Bar Graph 2 of the Amount of Energy Sources (1990-2019)", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot9", height = 150)
                               )
                           ),
                           fluidRow(
                               box(title = "Bar Graph 2 of the Percentage of Energy Sources (1990-2019)", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   plotOutput("plot10", height = 150)
                               )
                           ),
                           fluidRow(
                               box(title = "Table 2 of Amounts of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                   dataTableOutput("tab5", height = 150)
                               )
                           )
                    ),
                # second column (right side)
                   column(3,
                       fluidRow(
                           box( title = "Line Chart 2 of the Amount of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                plotOutput("plot11", height = 150)
                           )
                       ),
                       fluidRow(
                           box( title = "Line Chart 2 of the Percentages of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                                plotOutput("plot12", height = 150)
                           )
                       ),
                       fluidRow(
                           box(title = "Table 2 of Percentages of Energy Sources", solidHeader = TRUE, status = "primary", width = 12,
                               dataTableOutput("tab6", height = 150)
                           )
                       )
                   ),
                ),
        # heat maps
        tabItem(tabName = "heat",
                column(4,
                       fluidRow(
                           box(title = "Heat Map 1 of the Amount of Energy Sources (1990-2019)", solidHeader = TRUE,
                               status = "primary", width = 12,
                               plotOutput("heat1", height = 200)
                           )
                       ),
                       fluidRow(
                           box(title = "Heat Map 1 of the Percentage of Energy Sources (1990-2019)", solidHeader = TRUE,
                               status = "primary", width = 12,
                               plotOutput("heat2", height = 200)
                           )
                       ),
                ),
                column(4,
                       fluidRow(
                           box(title = "Heat Map 2 of the Amount of Energy Sources (1990-2019)", solidHeader = TRUE,
                               status = "primary", width = 12,
                               plotOutput("heat3", height = 200)
                           )
                       ),
                       fluidRow(
                           box(title = "Heat Map 2 of the Percentage of Energy Sources (1990-2019)", solidHeader = TRUE,
                               status = "primary", width = 12,
                               plotOutput("heat4", height = 200)
                           )
                       )
                ),
                column(4,
                       h3("NOTE: These heat maps function the same way as the split screen visualizations, just not choosing any states."))
                ),
        #about
        tabItem(tabName = "credits",
            h1("About"),
            h2("Created by: Matthew Ghuneim"),
            h3("The data was from a CSV file provided to us and this application was last updated on February 13, 2021. The websites and data I used are listed below: "),
            h3("Original Data: https://www.eia.gov/electricity/data/state/"),
            h3("EVL Weather Example from Prof. Johnson: https://www.evl.uic.edu/aej/424/evlWeatherForR.zip"),
            h3("ggplot2: https://ggplot2.tidyverse.org/reference/"),
            h3("usmap: https://www.rdocumentation.org/packages/usmap/versions/0.5.2"),
            h3("dplyr: https://www.rdocumentation.org/packages/dplyr/versions/0.7.8"),
            h3("Shiny Tutorial: https://shiny.rstudio.com/tutorial/")
        )
        )
    )
)

server <- function(input, output, session) {
    
    # reactive functions
    
    theme_set(theme_grey(base_size = 18)) 
    
    # bar charts
    barReactive <- reactive({subset(newStates, (newStates$STATE == input$State) & (newStates$ENERGY.SOURCE %in% input$Source) & (newStates$YEAR == input$Year) & (newStates$ENERGY.SOURCE != "Total"))})
    bar2Reactive <- reactive({subset(newStates, (newStates$STATE == input$State2) & (newStates$ENERGY.SOURCE %in% input$Source2) &(newStates$YEAR == input$Year2) & (newStates$ENERGY.SOURCE != "Total"))})
    
    # line charts
    lineReactive <- reactive({subset(notTotals, (notTotals$STATE == input$State) & (notTotals$ENERGY.SOURCE %in% input$Source) & (notTotals$YEAR == input$Year) & (notTotals$ENERGY.SOURCE != "Total"))})
    linePercentReactive <- reactive({subset(nt, (nt$STATE == input$State) & (nt$ENERGY.SOURCE %in% input$Source) & (nt$YEAR == input$Year) & (nt$ENERGY.SOURCE != "Total"))})
    line2Reactive <- reactive({subset(notTotals, (notTotals$STATE == input$State2) & (notTotals$ENERGY.SOURCE == input$Source2) & (notTotals$YEAR == input$Year2) & (notTotals$ENERGY.SOURCE != "Total"))})
    line2PercentReactive <- reactive({subset(nt, (nt$STATE == input$State2) & (notTotals$ENERGY.SOURCE == input$Source2) & (notTotals$YEAR == input$Year2) & (notTotals$ENERGY.SOURCE != "Total"))})
    
    # tables 
    amountReactive <- reactive({subset(df, (df$notTotals.STATE == input$State) & (df$notTotals.YEAR == input$Year) & (df$notTotals.ENERGY.SOURCE %in% input$Source))})
    amount2Reactive <- reactive({subset(df, (df$notTotals.STATE == input$State2) & (df$notTotals.YEAR == input$Year2) & (df$notTotals.ENERGY.SOURCE %in% input$Source2))})
    percentReactive <- reactive({subset(nt, (nt$STATE == input$State) & (nt$YEAR == input$Year) & (nt$ENERGY.SOURCE %in% input$Source) & (nt$pc * 100))})
    percent2Reactive <- reactive({subset(nt, (nt$STATE == input$State2) & (nt$YEAR == input$Year2) & (nt$ENERGY.SOURCE %in% input$Source2) & (nt$pc * 100))})
    
    #checkboxes
    checkboxFilter <- reactive({subset(notTotals, (notTotals$ENERGY.SOURCE %in% input$checkGroup1))})
    checkboxFilter2 <- reactive({subset(nt, (nt$ENERGY.SOURCE %in% input$checkGroup2))})
    
    #heat maps
    mapReactive <- reactive({subset(newStates, (newStates$ENERGY.SOURCE %in% input$Source) & (newStates$YEAR %in% input$Year))})
    mapPercentReactive <- reactive({subset(ns, (ns$ENERGY.SOURCE %in% input$Source) & (ns$YEAR %in% input$Year))})
    map2Reactive <- reactive({subset(newStates, (newStates$ENERGY.SOURCE %in% input$Source2) & (newStates$YEAR %in% input$Year2))})
    mapPercent2Reactive <- reactive({subset(ns, (ns$ENERGY.SOURCE %in% input$Source2) & (ns$YEAR %in% input$Year2))})
    
    # bar graphs for the amounts and percentages on their own 
    output$plot1 <- renderPlot({
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_fill_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_bar(position = 'stack', stat = 'identity') + theme_bw() + labs(fill = "Energy Sources")
    })
    output$plot2 <- renderPlot({
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_fill_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_bar(position = 'fill', stat = 'identity') + scale_y_continuous(labels = scales::percent) + theme_bw() + labs(fill = "Energy Sources")
    })
    # bar charts for the split screen 
    output$plot5 <- renderPlot({
        notTotals <- barReactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_fill_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_bar(position = 'stack', stat = 'identity') + theme_bw() + labs(fill = "Energy Sources")
    })
    output$plot6 <- renderPlot({
        notTotals <- barReactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_fill_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_bar(position = 'fill', stat = 'identity') + scale_y_continuous(labels = scales::percent) + theme_bw() + labs(fill = "Energy Sources")
    })
    output$plot9 <- renderPlot({
        notTotals <- bar2Reactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_fill_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_bar(position = 'stack', stat = 'identity') + theme_bw() + labs(fill = "Energy Sources")
    })
    output$plot10 <- renderPlot({
        notTotals <- bar2Reactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_fill_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_bar(position = 'fill', stat = 'identity') + scale_y_continuous(labels = scales::percent) + theme_bw() + labs(fill = "Energy Sources")
    })
    
    # line graphs for the amounts and percentages on their own
    output$plot3 <- renderPlot({
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + labs(color = "Energy Sources") +  theme_bw()
    })
    output$plot4 <- renderPlot({
        ggplot(nt, aes(x = YEAR, y = pc, group = ENERGY.SOURCE, color = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + scale_y_continuous(labels = scales::percent) + labs(color = "Energy Sources") + theme_bw()
    })
    # line graphs for the split screen
    output$plot7 <- renderPlot({
        notTotals <- lineReactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            geom_line(stat = "summary", fun = "sum") + geom_point(stat = "summary", fun = "sum") + 
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                         "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                         "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) + labs(color = "Energy Sources") + theme_bw()
    })
    output$plot8 <- renderPlot({
        nt <- linePercentReactive()
        ggplot(nt, aes(x = YEAR, y = pc, group = ENERGY.SOURCE, color = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + geom_point(stat = "summary", fun = "sum") + scale_y_continuous(labels = scales::percent) + labs(color = "Energy Sources") + theme_bw()
    })
    output$plot11 <- renderPlot({
        notTotals <- line2Reactive()
        ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + geom_point(stat = "summary", fun = "sum") + labs(color = "Energy Sources") + theme_bw()
    })
    output$plot12 <- renderPlot({
        nt <- line2PercentReactive()
        ggplot(nt, aes(x = YEAR, y = pc, group = ENERGY.SOURCE, color = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + geom_point(stat = "summary", fun = "sum") + scale_y_continuous(labels = scales::percent) + labs(color = "Energy Sources") + theme_bw()
    })
    
    # tables for the amounts and percentages on their own
    output$tab1 <- DT::renderDataTable(
        DT::datatable({ 
            as.data.frame(df)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
    )
    output$tab2 <- DT::renderDataTable(
        DT::datatable({ 
           as.data.frame(nt)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'asc'))
        ), rownames = FALSE 
        )
    )
    # tables for the split screen
    output$tab3 <- DT::renderDataTable(
        DT::datatable({ 
            rawAmount <- amountReactive()
            as.data.frame(rawAmount)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
    )
    output$tab4 <- DT::renderDataTable(
        DT::datatable({ 
            rawPercent <- percentReactive()
            as.data.frame(rawPercent)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'asc'))
        ), rownames = FALSE 
        )
    )
    output$tab5 <- DT::renderDataTable(
        DT::datatable({ 
            rawAmount <- amount2Reactive()
            as.data.frame(rawAmount)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
    )
    output$tab6 <- DT::renderDataTable(
        DT::datatable({ 
            rawPercent <- percent2Reactive()
            as.data.frame(rawPercent)
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'asc'))
        ), rownames = FALSE 
        )
    )
    #check boxes to control the line graphs on their own
    output$amounts <- renderPlot({
        checks <- checkboxFilter()
        if (input$checkGroup1 == "All"){
            updateCheckboxGroupInput(session, "checkGroup1", selected = c("All", boxSources))
        }
        ggplot(checks, aes(x = YEAR, y = GENERATION..Megawatthours., color = ENERGY.SOURCE)) + 
            labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + labs(color = "Energy Sources") + theme_bw()
    })
    output$amounts2 <- renderPlot({
        checks2 <- checkboxFilter2()
        if (input$checkGroup2 == "All"){
            updateCheckboxGroupInput(session, "checkGroup2", selected = c("All", boxSources))
        }
        ggplot(checks2, aes(x = YEAR, y = pc, group = ENERGY.SOURCE, color = ENERGY.SOURCE)) + 
            labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
            scale_color_manual(values = c("Coal" = "#661100", "Geothermal" = "#e69f00", "Hydro" = "#56b4e9",
                                          "Gas" = "#009e73", "Nuclear" = "#aa4499", "Petroleum" = "#f0e442", "Solar" = "#88ccee",
                                          "Wind" = "#0072b2", "Wood" = "#d55e00", "Total" = "#cc79a7")) +
            geom_line(stat = "summary", fun = "sum") + scale_y_continuous(labels = scales::percent) + labs(color = "Energy Sources") + theme_bw()
    })
    
    # heat maps
    output$heat1 <- renderPlot({
        numbers <- mapReactive()
        numbers$fips <- fips(numbers$STATE)
        plot_usmap(data = numbers, values = "GENERATION..Megawatthours.", labels = TRUE) +
            labs(fill = "Megawatts") + scale_fill_distiller(palette = "Reds") + theme(legend.position = "right")
    })
    output$heat2 <- renderPlot({
        numbers <- mapPercentReactive()
        numbers$fips <- fips(numbers$STATE)
        plot_usmap(data = numbers, values = "pc", labels = TRUE) +
            labs(fill = "Megawatts") + scale_fill_distiller(palette = "RdPu") + theme(legend.position = "right")
    }) 
    output$heat3 <- renderPlot({
        numbers <- map2Reactive()
        numbers$fips <- fips(numbers$STATE)
        plot_usmap(data = numbers, values = "GENERATION..Megawatthours.", labels = TRUE) +
            labs(fill = "Megawatts") + scale_fill_distiller(palette = "Spectral") + theme(legend.position = "right")
    })
    output$heat4 <- renderPlot({
        numbers <- mapPercent2Reactive()
        numbers$fips <- fips(numbers$STATE)
        plot_usmap(data = numbers, values = "pc", labels = TRUE) +
            labs(fill = "Megawatts") + scale_fill_distiller(palette = "YlOrRd") + theme(legend.position = "right")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
