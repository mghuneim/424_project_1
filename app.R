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


states <- read.csv("annual_generation_state.csv", header = TRUE)
states$GENERATION..Megawatthours. <- gsub(",", "", states$GENERATION..Megawatthours.)
states$GENERATION..Megawatthours. <- as.numeric(states$GENERATION..Megawatthours.)

newStates <- subset(states, states$STATE != "  ")
newStates$STATE <- factor(newStates$STATE)


newStates$STATE <- as.factor(newStates$STATE)
newStates$TYPE.OF.PRODUCER <- as.factor(newStates$TYPE.OF.PRODUCER)
newStates$ENERGY.SOURCE <- as.factor(newStates$ENERGY.SOURCE)

newStates <- subset(states, (states$ENERGY.SOURCE != "Other") & 
                        (states$ENERGY.SOURCE != "Other Gases") &
                        (states$ENERGY.SOURCE != "Other Biomass") &
                        (states$ENERGY.SOURCE != "Pumped Storage"))

newStates <- subset(newStates, newStates$GENERATION..Megawatthours. >= 0)

newStates$ENERGY.SOURCE <- factor(newStates$ENERGY.SOURCE)

newStates$STATE <- toupper(newStates$STATE)

levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Hydroelectric Conventional"] <- "Hydro"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Natural Gas"] <- "Gas"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"


total <- subset(newStates, (newStates$ENERGY.SOURCE == "Total") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
geo <- subset(newStates, (newStates$ENERGY.SOURCE == "Geothermal") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
coal <- subset(newStates, (newStates$ENERGY.SOURCE == "Coal") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
hydro <- subset(newStates, (newStates$ENERGY.SOURCE == "Hydro") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
gas <- subset(newStates, (newStates$ENERGY.SOURCE == "Gas") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
nuclear <- subset(newStates, (newStates$ENERGY.SOURCE == "Nuclear") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
petroleum <- subset(newStates, (newStates$ENERGY.SOURCE == "Petroleum") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
solar <- subset(newStates, (newStates$ENERGY.SOURCE == "Solar") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
wind <- subset(newStates, (newStates$ENERGY.SOURCE == "Wind") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
wood <- subset(newStates, (newStates$ENERGY.SOURCE == "Wood") & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))

percentCoal <- coal$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentGeo <- geo$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentHydro <- hydro$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentGas <- gas$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentNuclear <- nuclear$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentPetroleum <- petroleum$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentSolar <- solar$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentWind <- wind$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.
percentWood <- wood$GENERATION..Megawatthours. / total$GENERATION..Megawatthours.

notTotals <- subset(newStates, (newStates$ENERGY.SOURCE != "Total"))
ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
    labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
    geom_bar(position = 'stack', stat = 'identity') + labs(fill = "Energy Sources")
    
ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + 
    labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS") +
    geom_bar(position = 'fill', stat = 'identity') + scale_y_continuous(labels = scales::percent) + labs(fill = "Energy Sources")

ggplot() + labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS", color = "Energy Sources") +
    geom_line(coal, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Coal")) +
    geom_line(geo, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Geo")) + 
    geom_line(hydro, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Hydro")) +
    geom_line(gas, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Gas")) +
    geom_line(nuclear, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Nuclear")) +
    geom_line(petroleum, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Petroleum")) +
    geom_line(wind, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Wind")) +
    geom_line(wood, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Wood")) + theme_bw() +
    scale_color_manual(values = c('Coal' = 'red', 'Geo' = 'orange', 'Hydro' = 'gold3', 'Gas' = 'green', 'Nuclear' = 'turquoise3', 'Petroleum' = 'blue'
                                  , 'Wind' = 'purple', 'Wood' = 'pink')) 

ggplot() + labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS", color = "Energy Sources") +
    geom_line(coal, mapping = aes(x = YEAR, y = percentCoal, color = "Coal")) +
    geom_line(geo, mapping = aes(x = YEAR, y = percentGeo, color = "Geo")) + 
    geom_line(hydro, mapping = aes(x = YEAR, y = percentHydro, color = "Hydro")) +
    geom_line(gas, mapping = aes(x = YEAR, y = percentGas, color = "Gas")) +
    geom_line(nuclear, mapping = aes(x = YEAR, y = percentNuclear, color = "Nuclear")) +
    geom_line(petroleum, mapping = aes(x = YEAR, y = percentPetroleum, color = "Petroleum")) +
    geom_line(wind, mapping = aes(x = YEAR, y = percentWind, color = "Wind")) +
    geom_line(wood, mapping = aes(x = YEAR, y = percentWood, color = "Wood")) + theme_bw() +
    scale_color_manual(values = c('Coal' = 'red', 'Geo' = 'orange', 'Hydro' = 'gold3', 'Gas' = 'green', 'Nuclear' = 'turquoise3', 'Petroleum' = 'blue'
                                  , 'Wind' = 'purple', 'Wood' = 'pink')) + scale_y_continuous(labels = scales::percent)


years <- c(1990:2019)

ui <- dashboardPage(
    # Application title
    skin = "red",
    dashboardHeader(title = "Power and the Passion"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                     sidebarMenuOutput("menu"),
                     selectInput("Year", "Select the year to visualize", years, selected = 2019)
                     ),
    dashboardBody(
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
            ),
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
               checkboxGroupInput("checkGroup1", 
                                  h3("Select which energy source to output (Amounts)"), 
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
               fluidRow(column(3, verbatimTextOutput("text_choice"))),
               checkboxGroupInput("checkGroup2", 
                                  h3("Select which energy source to output (Percentages)"), 
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
                                  selected = 1)
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Bar Graphs", tabName = "bars", icon = icon("bar-chart")),
            menuItem("Line Graphs", tabName = "lines", icon = icon("line-chart")),
            menuItem("Tables", tabName = "tables", icon = icon("table")),
            menuItem("Split Screen Comparisons", tabName = "split", icon = icon("th")),
            menuItem("About", tabName = "split", icon = icon("check"))
        )
    })
    
    theme_set(theme_grey(base_size = 18)) 
    barReactive <- reactive({subset(newStates, (newStates$YEAR == input$Year) & (newStates$ENERGY.SOURCE != "Total"))})
    sourceReactive <- reactive({subset(newStates, (newStates$ENERGY.SOURCE == input$ENERGY.SOURCE) & (newStates$STATE == "US-TOTAL") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))})
    
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
    output$plot3 <- renderPlot({
        ggplot() + labs(title = "Amounts of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS", color = "Energy Sources") +
            geom_line(coal, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Coal")) +
            geom_line(geo, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Geo")) + 
            geom_line(hydro, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Hydro")) +
            geom_line(gas, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Gas")) +
            geom_line(nuclear, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Nuclear")) +
            geom_line(petroleum, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Petroleum")) +
            geom_line(wind, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Wind")) +
            geom_line(wood, mapping = aes(x = YEAR, y = GENERATION..Megawatthours., color = "Wood")) + theme_bw() +
            scale_color_manual(values = c('Coal' = 'red', 'Geo' = 'orange', 'Hydro' = 'gold3', 'Gas' = 'green', 'Nuclear' = 'turquoise3', 'Petroleum' = 'blue'
                                          , 'Wind' = 'purple', 'Wood' = 'pink')) 
    })
    output$plot4 <- renderPlot({
        ggplot() + labs(title = "Percentages of Energy Sources from 1990 - 2019", x = "YEAR", y = "MEGAWATTS", color = "Energy Sources") +
            geom_line(coal, mapping = aes(x = YEAR, y = percentCoal, color = "Coal")) +
            geom_line(geo, mapping = aes(x = YEAR, y = percentGeo, color = "Geo")) + 
            geom_line(hydro, mapping = aes(x = YEAR, y = percentHydro, color = "Hydro")) +
            geom_line(gas, mapping = aes(x = YEAR, y = percentGas, color = "Gas")) +
            geom_line(nuclear, mapping = aes(x = YEAR, y = percentNuclear, color = "Nuclear")) +
            geom_line(petroleum, mapping = aes(x = YEAR, y = percentPetroleum, color = "Petroleum")) +
            geom_line(wind, mapping = aes(x = YEAR, y = percentWind, color = "Wind")) +
            geom_line(wood, mapping = aes(x = YEAR, y = percentWood, color = "Wood")) + theme_bw() +
            scale_color_manual(values = c('Coal' = 'red', 'Geo' = 'orange', 'Hydro' = 'gold3', 'Gas' = 'green', 'Nuclear' = 'turquoise3', 'Petroleum' = 'blue'
                                          , 'Wind' = 'purple', 'Wood' = 'pink')) + scale_y_continuous(labels = scales::percent)
    })
    output$text_choice <- renderPrint({
        return(paste0("You have chosen the choice ",input$checkGroup1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
