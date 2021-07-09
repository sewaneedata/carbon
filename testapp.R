
library(shiny)
library (dplyr)
library(googlesheets4)
library (tidyr)
library(shinydashboard)
library(ggplot2)
library(reshape2)

dat <- read.csv('dat.csv')
write.csv(dat, '~/Documents/datascience/carbon/dat.csv')

pd <- dat %>% select(Household, Species, cole_ewel, chave, TFTF)
pd <- melt(pd, id.vars = c('Household', 'Species'))

pd <- pd %>%
    mutate(Species = case_when(
        Species == 'Akajou'~ 'Akajou',
        Species == 'A' ~ 'Akajou',
        Species == 'mango' ~ 'Mango',
        Species == 'M' ~ 'Mango',
        Species == 'Kafe' ~ 'Kafe',
        Species == 'K' ~ 'Kafe',
        Species == 'Ced' ~ 'Ced',
        Species == 'C' ~ 'Ced',
        Species == 'New Kafe' ~ 'New Kafe',
        Species == 'KK' ~ 'New Kafe'
    ))

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Carbon Test App"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Household",
                        label = "Choose a Household",
                        choices = unique(dat$Household),
                        selected = '1'),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Household")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Household <- renderPlot({
     he <- input$Household
     subhe <- pd %>%
         filter(Household == he)

     ggplot(subhe, aes(x = variable, y =value, fill = Species)) + 
         geom_bar(stat = 'identity', position = 'dodge')+ 
         labs(title = paste0('CO2 Sequestered in Household ',he), x = 'Equation', y = 'C02 est') + ggthemes::theme_wsj() + theme (legend.position = 'bottom') 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
