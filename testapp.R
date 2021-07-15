library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(plotly)

dat <- read.csv('dat.csv')


# write.csv(dat, '~/Documents/datascience/carbon/dat.csv')
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
                text="Data",
                tabName="testdata_19"),
            menuItem(
                text = 'Regressions',
                tabName = 'Regression'),
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
                tabName="testdata_19",
                fluidRow(
                    column(4,
                           selectInput(inputId = "year",
                                       label = "Choose a Year",
                                      choices = c("2019", "2021"),
                                      selected = c("2019", "2021"),
                                      multiple = TRUE),
                           uiOutput('household_ui'),
                           radioButtons(inputId = "Graph",
                                        label = "Choose a graph",
                                        choices = c("Carbon sequestered","Carbon payments", "Number of trees"),
                                        selected = "Carbon sequestered"),
                           br(),
                           valueBoxOutput("tot_carb", width = '100%'),
                           br(),
                           
                           valueBoxOutput("tot_money", width = '100%'),
                           br(),
                           
                           valueBoxOutput("tot_tree", width = '100%')
                    ),
                    
                    column(8, 
                           br (),
                           br (),
                           br (),
                           br (),
                           plotOutput("Household"))
                )
            ),
            tabItem(
                tabName="Regression",
                h4('View Regression Per Household in 2019'),
                fluidRow(
                    column(12,
                           selectInput(inputId = "year_regress",
                                       label = "Choose a Year",
                                       choices = c("2019", "2021"),
                                       selected = c("2019", "2021"),
                                       multiple = TRUE),
                           uiOutput('regression_ui')),
                           # selectInput(inputId = "household_regress",
                           #             label = "Choose a Household",
                           #             choices = c ('All',unique(dat$Household)),
                           #             selected = 'All'),
                           
                           column(12, 
                                  plotlyOutput("regression_plot"))
                    
                ),
                
                
            ),
            tabItem(
                tabName = 'about',
                fluidPage(
                    fluidRow(
                        div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                        h4('Built in partnership with ',
                           a(href = 'http://datalab.sewanee.edu',
                             target='_blank', 'DataLab'),
                           align = 'center'),
                        p('Sewanee is in collaboration with the Haitian organization, Zanmi Agrikol, to incentivize 50 family farmers to grow trees instead of selling material for coal through payments made by the Sewanee Green Fund. The payments reflect the carbon sequestration of the planted trees which is quantified and analyzed by our DataLab team. The addition of these trees provides a shade canopy and enriches the soil with nutrients, enabling farmers to establish more sustainable regenerative coffee-based agroecosystems (agroforests).', align = 'center'),
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
    
    output$household_ui <- renderUI({
        
        sub_dat <- dat %>% filter(year %in% input$year)
        hh_choices <- sort(unique(sub_dat$Household))
        if(length(hh_choices) > 0){
            hh_choices <- c('All', hh_choices)
            selectInput(inputId = "Household",
                        label = "Choose a Household",
                        choices = hh_choices,
                        multiple = FALSE)
        } else {
            h3('No households for the year(s) selected')
        }
        
        
    })
    output$regression_ui <- renderUI({
        
        sub_dat <- dat %>% filter(year %in% input$year_regress)
        hh_choices <- sort(unique(sub_dat$Household))
        if(length(hh_choices) > 0){
            hh_choices <- c('All', hh_choices)
            selectInput(inputId = "household_regress",
                        label = "Choose a Household",
                        choices = hh_choices,
                        multiple = FALSE)
        } else {
            h3('No households for the year(s) selected')
        }
        
        
    })
    
    output$regression_plot <- renderPlotly({
        he <- input$household_regress
        datyear_reg <- dat %>% filter(year %in% input$year_regress)
        
        ok <- FALSE
        if(!is.null(he)){
            ok <- TRUE
        }
        

        if(ok){
            if (he == 'All'){
                message('he is all')
                subreg <-     
                    datyear_reg 
            } else {
                message('he is ', he)
                subreg <- datyear_reg %>%
                    filter (Household == he) 
            }
            if(nrow(subreg) > 0){
                p <- ggplot(data = subreg, aes(x = log(diam_cm), 
                                               y = log(Height_m),
                                              )) +
                    geom_point()+
                    geom_smooth(method = lm) +
                    facet_wrap(~Species)+
                    labs(title = paste0('Height by Diameter of Trees in Household ',he), x = ' log Tree Diameter', y = 'log Tree Height')
                ggplotly(p,
                         tooltip = 'id')
            }
                
        }
        
    })
    
    output$tot_carb <- renderValueBox({
        he <- input$Household
        subhe <- dat
        if(he != "All"){
        subhe <- dat %>%
            filter(Household == he)
        }
        
        subhe <- subhe %>% filter(year %in% input$year)
        
        sum_calc <- sum(subhe$Calculations,na.rm=TRUE)
        valueBox(value = round(sum_calc, 3),
                 icon = icon ('globe'),
                 subtitle = paste0('Total Carbon Sequestered in tons for Farm ', he), color = 'aqua' )
    })
    output$tot_money <- renderValueBox({
        he <- input$Household
        subhe <- dat
        if(he != "All"){
            subhe <- dat %>%
                filter(Household == he)
        }
        subhe <- subhe %>% filter(year %in% input$year)
        sum_calc <- (sum(subhe$Calculations))*50
        valueBox(value = paste0 ('$', round(sum_calc,2)),
                 icon = icon ('dollar'),
                 subtitle = paste0('Total Money Paid to Household (USD)'), color = 'blue')
        
    })
    output$tot_tree <- renderValueBox({
        he <- input$Household
        subhe <- dat
        if(he != "All"){
            subhe <- dat %>%
                filter(Household == he)
        }
        subhe <- subhe %>% filter(year %in% input$year)
        sum_tree <- length(subhe$Household)
        valueBox(value = sum_tree,
                 icon = icon ('tree'),
                 subtitle = paste0('Total Trees on Farm ', he), color = 'green' )
    })
    
    output$Household <- renderPlot({
        
        datyear <- dat %>% filter(year %in% input$year)
        
        he <- input$Household
        graph <- input$Graph
        
        ok <- FALSE
        if(!is.null(he)){
            ok <- TRUE
        }
        if(ok){
            # create plot based on households. 
            subhe <- if (he == 'All') {
                datyear  %>%
                    group_by( Species ) %>% 
                    summarize( Carbon = sum(Calculations))
            } else {
                datyear  %>%
                    filter(Household == he) %>% 
                    group_by( Species ) %>% 
                    summarize( Carbon = sum(Calculations))
            }
            
            payments <- if (he == 'All') {
                datyear  %>%
                    group_by( Species ) %>%
                    summarise(Pay = (sum(Calculations)*50))
            } else {
                datyear %>%
                    filter(Household == he)%>%
                    group_by( Species ) %>%
                    summarise(Pay = (sum(Calculations)*50))
            }
            
            
            Num_Tree <- if (he == 'All') {
                datyear  %>%
                    group_by( Species ) %>%
                    summarise(Tre = length(Household))
            } else {
                datyear %>%
                    filter(Household == he)%>%
                    group_by( Species ) %>%
                    summarise(Tre = length(Household))
            }
            
            
            if(input$Graph=='Carbon sequestered'){
                ggplot(subhe, aes(x = Species, y=Carbon, fill=Species)) +
                    geom_col(position = 'dodge') +
                    geom_text( aes(label=round(Carbon,3)), vjust=0) +
                    labs(title = paste0('CO2 Sequestered in Household ',he), x = 'Species', y = 'C02 Estimate (tons)') + ggthemes::theme_economist() + theme (legend.position = 'none')
            }
            else if (input$Graph == 'Carbon payments'){
                ggplot(data = payments, aes(x = Species, y = Pay, fill = Species))+
                    geom_col(position = 'dodge') +
                    geom_text( aes(label=round(Pay,2)), vjust=0) +
                    labs(title = paste0('Payments for Household ',he), x = 'Species', y = 'Payment in USD') + ggthemes::theme_economist() + theme (legend.position = 'none')
            }
            else if (input$Graph == 'Number of trees'){
                ggplot(data = Num_Tree, aes(x = Species, y = Tre, fill = Species))+
                    geom_col(position = 'dodge') +
                    geom_text( aes(label=Tre, vjust=0)) +
                    labs(title = paste0('Number of Trees for Household ',he), x = 'Species', y = 'Number of Trees') + ggthemes::theme_economist() + theme (legend.position = 'none')
            }
            
        }    
        }
        
        
    )
}


shinyApp(ui, server)

