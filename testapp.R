library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyr)

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
        text="Main Panel",
        tabName="testdata_19"),
      menuItem(
        text = 'Regressions',
        tabName = 'Regression'),
      menuItem(
        text = 'Allometric Comparison',
        tabName = 'allometric'),
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
                 plotOutput("regression_plot"))
          
        ),
        
        
      ),
      tabItem(
        tabName="allometric",
        h4('Compare Carbon Sequestration Using Different Equations'),
        fluidRow(
          column(12,
                 selectInput(inputId = "year_allometric",
                             label = "Choose a Year",
                             choices = c("2019", "2021"),
                             selected = c("2019", "2021"),
                             multiple = TRUE),
                 uiOutput('allometric_ui')),
          column(12, 
                 plotOutput("allometric_plot"))
          
        ),
        
        
      ),
      tabItem(
        tabName = 'about',
        fluidPage(
          fluidRow(
            div(img(src='Man.png', height = 200, width = 400, align = "center"), style="text-align: center;"),
            h2('Built in partnership with ',
               a(href = 'http://datalab.sewanee.edu',
                 target='_blank', 'DataLab'),
               align = 'center'),
            p('Sewanee is in collaboration with the Haitian organization, Zanmi Agrikol, to incentivize 50 family farmers to grow trees instead of selling material for coal through payments made by the Sewanee Green Fund. The payments reflect the carbon sequestration of the planted trees which is quantified and analyzed by our DataLab team. The addition of these trees provides a shade canopy and enriches the soil with nutrients, enabling farmers to establish more sustainable regenerative coffee-based agroecosystems (agroforests).', align = 'center'),
            div(a(actionButton(inputId = "github_link", label = "Zamni Kafe GitHub",
                               icon = icon("link", lib = "font-awesome")),
                  href="https://github.com/sewaneedata/carbon/wiki",
                  align = 'center')),
            h4('Project Contributors',
               align = 'left'),
            p('- Kate Baker Class of 2022',
              align = 'left'), 
            p('- Nika Gorski Class of 2022',
              align = 'left'),
            p('- Caroline Willett Class of 2023', 
              align = 'left'),
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

    output$allometric_ui <- renderUI({
        
        sub_dat <- dat %>% filter(year %in% input$year_allometric)
        if(nrow(sub_dat) > 0){
          years_selected <- input$year_allometric
          sub_dat <- dat %>% filter(year %in% years_selected)
          
          if(nrow (sub_dat) > 0){
            
            selectInput(inputId = "Species",
                        label = "Choose a Species",
                        choices = c("All","Akajou", "Mango", "Ced", "Kafe", "New Kafe"),
                        #selected = c("Akajou", "Mango", "Ced", "Kafe", "New Kafe"),
                        multiple = FALSE)
          } else {
            h3('No species for the year(s) selected')
          }
          
        } else {
            h3('No species for the year(s) selected')
        }
        
        
    })
    
   output$allometric_plot <- renderPlot({
       year_name <- input$year_allometric
       suballo <- dat %>%
           filter(year %in% year_name) %>%
           group_by(Species) %>%
           summarize( Cole_Ewel=sum(cole_ewel), 
                      TFTF=sum(TFTF), 
                      Chave=sum(chave), 
                      # Cairns_General=sum(Cairns_General), 
                      Chave_General=sum(Chave_General), 
                      Species_Specific=sum(Species_Specific))
       # allo_plot <- dat%>% filter(year %in% input$year)
       spec <- input$Species
       # save(suballo,spec file = 'temp.rda')
       if(spec == 'All'){
          
           suballo <- reshape2::melt(suballo, id.vars = 'Species')
           
           # create separate data frame with that shows total (use in geom_text)
           tot_value <- suballo %>% group_by(variable) %>% summarise(tot = sum(value))
           ggplot(suballo, aes(x = variable, y=value, fill = Species)) +
               geom_col() +
               geom_text(data=tot_value,aes(variable, 
                                            tot, 
                                            fill = NULL,
                                            label = round(tot, 2), vjust =0))+
               ggthemes::theme_economist() + 
               labs(x = "Equations", y = "Carbon Sequestered (in tons)")
       } else {
           suballo <- suballo %>% filter( Species == spec ) %>% 
               select(-Species) %>%
               gather( )
           ggplot(suballo, aes(x = key, y=value)) +
               geom_col(position = 'dodge', fill = "chocolate") +
               geom_text(aes(label = round(value, 2), vjust =0))+
               ggthemes::theme_economist()+
               labs(x = "Equations", y = "Carbon Sequestered (in tons)")
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


  output$allometric_plot <- renderPlot({
    year_name <- input$year_allometric
    spec <- input$Species
    
    ok <- FALSE
    
    if(!is.null(year_name)){
      if(!is.null(spec)){
        ok <- TRUE
      }
    }
    
    if(ok){
      suballo <- dat %>%
        filter(year %in% year_name) 
      
      suballo <- suballo %>%
        group_by(Species) %>%
        summarize( Cole_Ewel=sum(cole_ewel), 
                   TFTF=sum(TFTF), 
                   Chave_General=sum(Chave_General), 
                   Species_Specific=sum(Species_Specific))
      # allo_plot <- dat%>% filter(year %in% input$year)
      # save(suballo,spec file = 'temp.rda')
      if(spec == 'All'){
        
        suballo <- reshape2::melt(suballo, id.vars = 'Species')
        
        # create separate data frame with that shows total (use in geom_text)
        tot_value <- suballo %>% group_by(variable) %>% summarise(tot = sum(value))
        ggplot(suballo, aes(x = variable, y=value, fill = Species)) +
          geom_col() +
          geom_text(data=tot_value,aes(variable, 
                                       tot, 
                                       fill = NULL,
                                       label = round(tot, 2), vjust =0))+
          ggthemes::theme_economist() + 
          labs(x = "Equations", y = "Carbon Sequestered (in tons)")
      } else {
        suballo <- suballo %>% filter( Species == spec ) %>% 
          select(-Species) %>%
          gather( )
        ggplot(suballo, aes(x = key, y=value)) +
          geom_col(position = 'dodge', fill = "chocolate") +
          geom_text(aes(label = round(value, 2), vjust =0))+
          ggthemes::theme_economist()+
          labs(x = "Equations", y = "Carbon Sequestered (in tons)")
      }  
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
  
  output$regression_plot <- renderPlot({
    df_lm <- lm(dat$Height_m~dat$diam_cm)
    r2 <- summary(df_lm)$r.squared
    he <- input$household_regress
    datyear_reg <- dat %>% filter(year %in% input$year_regress)
    
    # save(df_lm, r2, he, datyear_reg, file = '/tmp/katebaker.Rdata')
    
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
        
        # Create a dataframe for storing r^2 values:
        r2df <- tibble(Species = sort(unique(subreg$Species))) %>%
          mutate(r2 = NA)
        
        # loop through each species (ie, each row in r2df) and calculate r2
        for(i in 1:nrow(r2df)){
          the_species <- r2df$Species[i]
          species_subreg <- subreg %>%
            filter(Species == the_species)
          species_mod <- lm(Height_m ~ diam_cm, data = species_subreg)
          r2 <- summary(species_mod)$r.squared
          r2df$r2[i] <- r2
        }
        
        # Bring r2 values into subreg
        subreg <- left_join(subreg, r2df)
        subreg <- subreg %>%
          mutate(Species = paste0(Species, ' (R-squared: ', round(r2, digits = 2), ')'))
        
        ggplot(data = subreg, 
               aes(x = log(diam_cm), 
                   y = log(Height_m),
               )) +
          geom_point(size = 0.2, alpha = 0.5)+
          geom_smooth(method = lm, se = FALSE, color = 'Red') +
          facet_wrap(~Species)+
          labs(title = paste0('Height by Diameter of Trees in Household ',he), x = ' log Tree Diameter', y = 'log Tree Height')
        # annotate("text", x=.5, y=4,
        #          label= paste("R^2 =",round(r2df$r2,digits=3)),
        #          parse=TRUE, size = 3)
      }
      
    }
    
  })
  
  output$tot_carb <- renderValueBox({
    he <- input$Household
    iyear <- input$year
    
    subhe <- dat
    
    if(!is.null(iyear)){
      subhe <- subhe %>% filter(year %in% iyear)
    }else{
      subhe <- data.frame(Household="",Calculations=0)
    }
    
    if(!is.null(he)){
      if(he != "All"){
        subhe <- subhe %>%
          filter(Household == he)
      }
    }
    
    # 
    # 
    sum_calc <- sum(subhe$Calculations,na.rm=TRUE)
    
    if(is.null(sum_calc)){
      the_title <- ' '
    } else {
      sum_calc <- round(sum_calc, digits = 3)
      the_title <- as.character(sum_calc)
    }
    
    valueBox(
      the_title,
      he,
      icon = icon ('globe'),
      subtitle = paste0('Total Carbon Sequestered in tons for Farm ', he), color = 'aqua' 
    )
  })
  
  output$tot_money <- renderValueBox({
    he <- input$Household
    iyear <- input$year
    subhe <- dat
    
    if (!is.null(he)){
      if(he != "All"){
        subhe <- subhe %>%
          filter(Household == he)
      }    
    }
    
    if(!is.null(iyear)){
      subhe <- subhe %>% filter(year %in% iyear)    
    }else{subhe<- data.frame(Household = "", Calculations =0)
    
    }
    
    sum_calc <- (sum(subhe$Calculations))*50
    if(is.null(sum_calc)){
      the_title <- ' '
    } else {
      the_title <- as.character(round(sum_calc,2))
    }
    
    valueBox(value = paste0 ('$', the_title),
             icon = icon ('dollar'),
             subtitle = paste0('Total Money Paid to Household (USD)'), color = 'blue')
    
  })
  output$tot_tree <- renderValueBox({
    he <- input$Household
    iyear <- input$year
    subhe <- dat
    
    if(!is.null(he)){
      if(he != "All"){
        subhe <- subhe %>%
          filter(Household == he)
      }    
    }
    if(!is.null(iyear)){
      subhe <- subhe %>% filter(year %in% iyear)    
    }else{subhe <- data.frame()
    
    }
    
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

