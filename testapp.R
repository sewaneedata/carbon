library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)

dat <- read.csv('dat.csv')
write.csv(dat, '~/Documents/datascience/carbon/dat.csv')
# pd <- dat %>% select(Household, Species, cole_ewel, chave, TFTF)
# pd <- melt(pd, id.vars = c('Household', 'Species'))
#
dat <- dat %>%
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
ui <- dashboardPage(
    dashboardHeader (title = "Zanmi Kafe Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text="Main",
                tabName="main"),
            menuItem(
                text = 'Map',
                tabName = 'map'),
            menuItem(
                text = 'About',
                tabName = 'about')
        )),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(
                tabName="main",
              fluidRow(
                  column(4,
                         selectInput(inputId = "Household",
                                     label = "Choose a Household",
                                     choices = unique(dat$Household),
                                     selected = '1'),
                         br(),
                         valueBoxOutput("tot_carb", width = '100%'),
                         br(),
                         
                         valueBoxOutput("tot_money", width = '100%'),
                         br(),
                         
                         valueBoxOutput("tot_tree", width = '100%')
                         ),
                  
                  column(8, 
                         plotOutput("Household"))
              )
            ),
            tabItem(
                tabName="map",
                h4('Maps here')
                
            ),
            tabItem(
                tabName = 'about',
                fluidPage(
                    fluidRow(
                        div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                        h4('Built in partnership with ',
                           a(href = 'http://databrew.cc',
                             target='_blank', 'Databrew'),
                           align = 'center'),
                        p('Empowering research and analysis through collaborative data science.', align = 'center'),
                        div(a(actionButton(inputId = "email", label = "info@databrew.cc",
                                           icon = icon("envelope", lib = "font-awesome")),
                              href="mailto:info@databrew.cc",
                              align = 'center')),
                        style = 'text-align:center;'
                    )
                )
            )
        )
    )
)
# Server
server <- function(input, output) {
    
    output$tot_carb <- renderValueBox({
        he <- input$Household
        subhe <- dat %>%
            filter(Household == he)
        sum_calc <- round(sum(subhe$Calculations))
        valueBox(value = sum_calc,
                 icon = icon ('globe'),
                 subtitle = paste0('Total Carbon Sequestered in Mg for Farm ', he), color = 'blue' )
    })
    output$tot_money <- renderValueBox({
        he <- input$Household
        subhe <- dat %>%
            filter(Household == he)
        sum_calc <- round(sum(subhe$Calculations))
        sum_money <- round(sum_calc*50)
        valueBox(value = paste0 ('$', sum_money),
                 icon = icon ('dollar'),
                 subtitle = paste0('Total Money Paid to Farmers (USD)'), color = 'blue')
        
    })
    output$tot_tree <- renderValueBox({
        he <- input$Household
        subhe <- dat %>%
            filter(Household == he)
        sum_tree <- round(length(subhe$Household))
        valueBox(value = sum_tree,
                 icon = icon ('tree'),
                 subtitle = paste0('Total Trees on Farm ', he), color = 'blue' )
    })
    
    output$Household <- renderPlot({
        he <- input$Household
        subhe <- dat %>%
            filter(Household == he)
        ggplot(subhe, aes(x = he, y = Calculations, fill = Species)) +
            geom_bar(stat = 'identity', position = 'dodge')+
            labs(title = paste0('CO2 Sequestered in Household ',he), x = 'Household', y = 'C02 Estimate (Mg)') + ggthemes::theme_economist() + theme (legend.position = 'bottom')
    })
}
shinyApp(ui, server)