#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


states <- read.csv("annual_generation_state.csv", header = TRUE)
states$GENERATION..Megawatthours. <- gsub(",", "", states$GENERATION..Megawatthours.)
states$GENERATION..Megawatthours. <- as.numeric(states$GENERATION..Megawatthours.)

newStates <- subset(states, states$STATE != "  ")
newStates$STATE <- factor(newStates$STATE)
newStates$STATE <- toupper(newStates$STATE)

newStates$STATE <- as.factor(newStates$STATE)
newStates$TYPE.OF.PRODUCER <- as.factor(newStates$TYPE.OF.PRODUCER)
newStates$ENERGY.SOURCE <- as.factor(newStates$ENERGY.SOURCE)

newStates <- subset(states, (states$ENERGY.SOURCE != "Other") & 
                        (states$ENERGY.SOURCE != "Other Gases") &
                        (states$ENERGY.SOURCE != "Other Biomass") &
                        (states$ENERGY.SOURCE != "Pumped Storage"))

newStates <- subset(newStates, newStates$GENERATION..Megawatthours. >= 0)

newStates$ENERGY.SOURCE <- factor(newStates$ENERGY.SOURCE)

levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Hydroelectric Conventional"] <- "Hydro"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Natural Gas"] <- "Gas"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"
levels(newStates$ENERGY.SOURCE)[levels(newStates$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"

narrowYears <- subset(newStates, (newStates$ENERGY.SOURCE == "Wood") & ( newStates$ENERGY.SOURCE != "Total"))

totals <- subset(newStates, (newStates$ENERGY.SOURCE == "Total") & (newStates$TYPE.OF.PRODUCER == "Total Electric Power Industry"))
notTotals <- subset(newStates, (newStates$ENERGY.SOURCE != "Total"))
ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + geom_bar(position = 'stack', stat = 'identity')
save <- ggplot(notTotals, aes(x = YEAR, y = GENERATION..Megawatthours., fill = ENERGY.SOURCE)) + geom_bar(position = 'fill', stat = 'identity') + scale_y_continuous(labels = scales::percent)
ggplot(totals, aes(x = YEAR, y = GENERATION..Megawatthours.)) + geom_line() + geom_point()
tabEnergy <- table(newStates$YEAR, newStates$ENERGY.SOURCE, newStates$GENERATION..Megawatthours., newStates$STATE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
