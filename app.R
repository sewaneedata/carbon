# These are the libraries that we use to build our dashboard. When you click "Run App", it will run all the code.
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggthemes)

#This is reading in the data that is created in testdata.R. Be sure that testdata is loaded and that the dat.csv is in the same folder as this code before running this line of code.
dat <- read.csv('dat.csv')

#This changes the names of the trees from just a letter to their full names. It is important to run this before working with the code below.
dat <- dat %>%
  mutate(species = case_when(
    # Species == 'Akajou'~ 'Akajou',
    species == 'A' ~ 'Akajou',
    # Species == 'mango' ~ 'Mango',
    species == 'M' ~ 'Mango',
    # Species == 'Kafe' ~ 'Kafe',
    species == 'K' ~ 'Kafe',
    # Species == 'Ced' ~ 'Ced',
    species == 'C' ~ 'Ced',
    # Species == 'New Kafe' ~ 'New Kafe',
    species == 'KK' ~ 'New Kafe'
  ))

# #This code makes the year 2021 the differnce between 2019 and 2021 so we can look at change over time.

sub2019 <- dat %>%
  dplyr::filter(year==2019) %>%
  dplyr::group_by(household, species) %>%
  dplyr::summarize(Number=n(), Total=sum(calculations))

sub2021 <- dat %>%
  dplyr::filter(year==2021) %>%
  dplyr::group_by(household, species) %>%
  dplyr::summarize(Number=n(), Total=sum(calculations))

joined_19_21 <- inner_join( sub2019, sub2021, by = c("household", "species"))

joined_19_21 <- joined_19_21 %>% mutate( sum_tree = Number.y - Number.x, calculations = Total.y - Total.x)

joined_19_21 <- joined_19_21 %>% select(-Number.x, -Total.x, -Number.y, -Total.y)

joined_19_21 <- joined_19_21 %>%
  mutate(species = case_when(
    species == 'Akajou'~ 'Akajou',
    # Species == 'A' ~ 'Akajou',
    species == 'Mango' ~ 'Mango',
    # Species == 'M' ~ 'Mango',
    species == 'Kafe' ~ 'Kafe',
    # Species == 'K' ~ 'Kafe',
    species == 'Ced' ~ 'Ced',
    # Species == 'C' ~ 'Ced',
    species == 'New Kafe' ~ 'New Kafe'#,
    # Species == 'KK' ~ 'New Kafe'
  ))

#################### Beginning of the Dashboard ###############################

#These are the names of the tabs that we have created.
ui <- dashboardPage(
  dashboardHeader (title = "Zanmi Kafe Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text="Main Panel",
        tabName="testdata_19"),
      menuItem(
        text = 'Regressions',
        tabName = 'regression'),
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
  #These are all the inputs for the main pannel.
      tabItem(
        tabName="testdata_19",
        fluidRow(
          column(4,
                 selectInput(inputId = "year",
                             label = "Choose a Year",
                             choices = c("2019", "2021", 'Total'),
                             selected = ("2019"),
                             multiple = FALSE),
                 uiOutput('household_ui'),
                 radioButtons(inputId = "graph",
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
                 downloadButton(outputId = 'household_plot_dl', 'Download plot'),
                 br (),
                 br (),
                 plotOutput("household"))
        )
      ),
  #These are all the inputs for the regressions tab.
      tabItem(
        tabName="regression",
        h4('View Regression Per Household in 2019'),
        fluidRow(
          column(12,
                 selectInput(inputId = "year_regress",
                             label = "Choose a Year",
                             choices = c("2019", "2021"),
                             selected = ("2019"),
                             multiple = FALSE),
                 uiOutput('regression_ui')),

          column(12,
                 plotOutput("regression_plot"))

        )


      ),
  # minor change
  # These are all the inputs for the allometric tab.
      tabItem(
        tabName="allometric",
        h4('Compare Carbon Sequestration Using Different Equations'),
        fluidRow(
          column(12,
                 selectInput(inputId = "year_allometric",
                             label = "Choose a Year",
                             choices = c("2019", "2021"),
                             selected = ("2019"),
                             multiple = FALSE),
                 uiOutput('allometric_ui')),
          column(12,
                 plotOutput("allometric_plot"))

        )


      ),
  #These are the inputs for the about tab.
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


##################################### Allometric###############################
 #This puts in the select ayear and species options in the alloemetic tab.------

  output$allometric_ui <- renderUI({

    sub_dat <- dat %>% dplyr::filter(year %in% input$year_allometric)
    if(nrow(sub_dat) > 0){
      years_selected <- input$year_allometric
      sub_dat <- dat %>% dplyr::filter(year %in% years_selected)

      if(nrow (sub_dat) > 0){

        selectInput(inputId = "species",
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

#This gives the plot for the allometric equations.------------------------------

  output$allometric_plot <- renderPlot({
    year_name <- input$year_allometric
    spec <- input$species

    ok <- FALSE

    if(!is.null(year_name)){
      if(!is.null(spec)){
        ok <- TRUE
      }
    }

    if(ok){
      suballo <- dat %>%
        dplyr::filter(year %in% year_name)

      suballo <- suballo %>%
        dplyr::group_by(species) %>%
        dplyr::summarize( Cole_Ewel=sum(cole_ewel),
                   TFTF=sum(TFTF),
                   Chave_General=sum(Chave_General),
                   species_specific=sum(species_specific))
      if(spec == 'All'){

        suballo <- reshape2::melt(suballo, id.vars = 'species')

        # create separate data frame with that shows total (use in geom_text)
        tot_value <- suballo %>%
          dplyr::group_by(variable) %>%
          dplyr::summarise(tot = sum(value))

        ggplot(suballo, aes(x = variable, y=value, fill = species)) +
          geom_col() +
          geom_text(data=tot_value,aes(variable,
                                       tot,
                                       fill = NULL,
                                       label = round(tot, 2), vjust =0))+
          ggthemes::theme_economist() +
          labs(x = "Equations", y = "Carbon Sequestered (in tons)")
      } else {
        suballo <- suballo %>%
          dplyr::filter( species == spec ) %>%
          dplyr::select(-species) %>%
          gather( )
        ggplot(suballo, aes(x = key, y=value)) +
          geom_col(position = 'dodge', fill = "chocolate") +
          geom_text(aes(label = round(value, 2), vjust =0))+
          ggthemes::theme_economist()+
          labs(x = "Equations", y = "Carbon Sequestered (in tons)")
      }
    }
  })


 ################################ Regressions #################################
  #This is the creates the slect inputs for the regression tab.-----------------
  output$regression_ui <- renderUI({

    sub_dat <- dat %>%
      dplyr::filter(year %in% input$year_regress)
    hh_choices <- sort(unique(sub_dat$household))
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
 #This creates the plot for the regressions tab.--------------------------------

  output$regression_plot <- renderPlot({
    df_lm <- lm(dat$height_m~dat$diam_cm)
    r2 <- summary(df_lm)$r.squared
    he <- input$household_regress
    datyear_reg <- dat %>%
      dplyr::filter(year %in% input$year_regress)

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
          dplyr::filter (household == he)
      }
      if(nrow(subreg) > 0){

        # Create a dataframe for storing r^2 values:
        r2df <- tibble(species = sort(unique(subreg$species))) %>%
          dplyr::mutate(r2 = NA)

        # loop through each species (ie, each row in r2df) and calculate r2
        for(i in 1:nrow(r2df)){
          the_species <- r2df$species[i]
          species_subreg <- subreg %>%
            dplyr::filter(species == the_species)
          species_mod <- lm(height_m ~ diam_cm, data = species_subreg)
          r2 <- summary(species_mod)$r.squared
          r2df$r2[i] <- r2
        }

        # Bring r2 values into subreg
        subreg <- dplyr::left_join(subreg, r2df)
        subreg <- subreg %>%
          dplyr::mutate(species = paste0(species, ' (R-squared: ', round(r2, digits = 2), ')'))

        ggplot(data = subreg,
               aes(x = log(diam_cm),
                   y = log(height_m)
               )) +
          geom_point(size = 0.2, alpha = 0.5)+
          geom_smooth(method = lm, se = FALSE, color = 'Red') +
          facet_wrap(~species)+
          labs(title = paste0('Height by Diameter of Trees in Household ',he), x = ' log Tree Diameter', y = 'log Tree Height')
      }

    }

  })

 ################################## Main Panel##################################

#This creates the select inputs for the main pannel-----------------------------
  output$household_ui <- renderUI({

    yr<- input$year
    if(yr =='Total'){
      hh_choices <- sort(unique(dat$household))
    } else {
      sub_dat <- dat %>% dplyr::filter(year %in% yr)
      hh_choices <- sort(unique(sub_dat$household))
    }

    hh_choices <- c('All', hh_choices)
    selectInput(inputId = "household",
                label = "Choose a Household",
                choices = hh_choices,
                multiple = FALSE)



  })

#This creates a value box for the total carbon absorbed ------------------------
  output$tot_carb <- renderValueBox({
    he <- input$household
    iyear <- input$year
    subhe <- dat
    # save(iyear, he, subhe, file = 'temp.rda')
    if(!is.null(he)){
      if(he != "All"){
        subhe <- subhe %>%
          dplyr::filter(household == he)
      }
    }
    twentyonesubhe <- subhe %>% dplyr::filter(year == 2021)
    nineteensubhe <- subhe %>% dplyr::filter(year == 2019)
    if( iyear == "2019"){
      subhe <- subhe %>% dplyr::filter( year == "2019" )
      sum_carb <- sum(subhe$calculations)
    } else if( iyear == "Total"){
      subhe <- subhe %>% dplyr::filter( year == "2021")
      sum_carb <- sum(subhe$calculations)
    } else if(iyear == "2021"){
      sum_carb <- sum(twentyonesubhe$calculations) - sum(nineteensubhe$calculations)
    }


    if(is.null(sum_carb)){
      the_title <- ' '
    } else {
      sum_carb <- round(sum_carb, digits = 3)
      the_title <- as.character(sum_carb)
    }

    valueBox(
      the_title,
      he,
      icon = icon ('globe'),
      subtitle = paste0('Total Carbon Sequestered in tons for Farm ', he), color = 'aqua'
    )
  })

#This creates a value box for the total money made------------------------------
  output$tot_money <- renderValueBox({
    he <- input$household
    iyear <- input$year
    subhe <- dat


    if (!is.null(he)){
      if(he != "All"){
        subhe <- subhe %>%
          dplyr::filter(household == he)
      }
    }

    if (iyear == '2019'){
      subhe <- subhe %>% dplyr::filter (year == '2019')
      sum_calc <- sum(subhe$calculations) * 50
    } else if (iyear == 'Total'){
      subhe <- subhe %>% dplyr::filter (year == '2021')
      sum_calc <- sum(subhe$calculations) * 50
    } else if (iyear == '2021') {
      twentyonesubhe <- subhe %>% dplyr::filter(year == 2021)
      nineteensubhe <- subhe %>% dplyr::filter(year == 2019)
      sum_calc <- (sum(twentyonesubhe$calculations) - sum(nineteensubhe$calculations))*50
    }

    if(is.null(sum_calc)){
      the_title <- ' '
    } else {
      the_title <- as.character(round(sum_calc,2))
    }

    valueBox(value = paste0 ('$', the_title),
             icon = icon ('dollar'),
             subtitle = paste0('Total Money Paid to Household (USD)'), color = 'blue')

  })

#This creates a value box for the total amount of trees planted------------------
  output$tot_tree <- renderValueBox({
    he <- input$household
    iyear <- input$year
    subhe<- dat

    if(!is.null(he)){
      if(he != "All"){
        subhe <- subhe %>%
          dplyr::filter(household == he)
      }
    }

    if( iyear == "2019"){
      subhe <- subhe %>% dplyr::filter( year == "2019" )
      sum_tree <- length(subhe$household)
    } else if( iyear == "Total"){
      subhe <- subhe %>% dplyr::filter( year == "2021")
      sum_tree <- length(subhe$household)
    } else if (iyear == '2021') {
      twentyonesubhe <- subhe %>% dplyr::filter(year == 2021)
      nineteensubhe <- subhe %>% dplyr::filter(year == 2019)
      sum_tree <- length(twentyonesubhe$household) - length(nineteensubhe$household)
    }

    valueBox(value = sum_tree,
             icon = icon ('tree'),
             subtitle = paste0('Total Trees on Farm ', he), color = 'green' )
  })

# This creates the plots for the main panel-------------------------------------
  #output$household <- renderPlot({plot(1~1)})
  
  output$household <- renderPlot({
    datyear <- dat
    iyear <- input$year
    he <- input$household
    graph <- input$graph
    # save(datyear, iyear, he, graph, file = '/tmp/katebaker.RData' )

    if (iyear =='2019'){
      datyear <- dat %>% dplyr::filter(year == 2019)
      iyear <- as.numeric(iyear)
    } else if(iyear == "Total"){
      datyear <- dat %>% dplyr::filter (year == 2021)
    } else if (iyear == '2021'){
      datyear <- joined_19_21
      iyear <- as.numeric(iyear)
    }




    ok <- FALSE
    if(!is.null(he)){
      ok <- TRUE
    }

    if(ok){
      # create plot based on households.
      if (he == 'All') {
        subhe <-  datyear  %>%
          dplyr::group_by( species ) %>%
          dplyr::summarize( Carbon = sum(calculations))
      } else {
        subhe <-datyear  %>%
          dplyr::filter(household == he) %>%
          dplyr::group_by( species ) %>%
          dplyr::summarize( Carbon = sum(calculations))
      }

      if (he == 'All') {
        payments <-  datyear  %>%
          dplyr::group_by( species ) %>%
          dplyr::summarise(Pay = (sum(calculations)*50))
      } else {
        payments <-datyear %>%
          dplyr::filter(household == he)%>%
          dplyr::group_by( species ) %>%
          dplyr::summarise(Pay = (sum(calculations)*50))
      }

      if (iyear == 2021){
        if (he == 'All') {
          Num_Tree <- joined_19_21  %>%
            dplyr::group_by(species ) %>%
            dplyr::summarize (Tre = sum( sum_tree ))
        } else{
          Num_Tree <- joined_19_21  %>%
            dplyr::filter( household == he ) %>%
            dplyr::group_by(species ) %>%
            dplyr::summarize (Tre = sum_tree )
        }
      } else {

        if (he == 'All') {
          Num_Tree <-  datyear  %>%
            dplyr::group_by( species ) %>%
            dplyr::summarise(Tre = length(household))
        } else {
          Num_Tree <-        datyear %>%
            dplyr::filter(household == he)%>%
            dplyr::group_by( species ) %>%
            dplyr::summarise(Tre = length(household))
        }
      }

      if(graph=='Carbon sequestered'){
        #plot(1~1)
        ggplot(subhe, aes(x = species, y=Carbon, fill=species)) +
          geom_bar(position = 'dodge', stat = 'identity') +
          geom_text( aes(label=round(Carbon,3)), vjust=0) +
          labs(title = paste0('CO2 Sequestered in Household ',he), x = 'Species', y = 'C02 Estimate (tons)') + 
          ggthemes::theme_economist() + theme (legend.position = 'none')
      } else if (graph == 'Carbon payments'){
        ggplot(data = payments, aes(x = species, y = Pay, fill = species))+
          geom_bar(position = 'dodge', stat = 'identity') +
          geom_text( aes(label=round(Pay,2)), vjust=0) +
          labs(title = paste0('Payments for Household ',he), x = 'Species', y = 'Payment in USD') + ggthemes::theme_economist() + theme (legend.position = 'none')
      } else if (graph == 'Number of trees'){
        ggplot(data = Num_Tree, aes(x = species, y = Tre, fill = species))+
          geom_bar(position = 'dodge', stat = 'identity') +
          geom_text( aes(label=Tre, vjust=0)) +
          labs(title = paste0('Number of Trees for Household ',he), x = 'Species', y = 'Number of Trees') + ggthemes::theme_economist() + theme (legend.position = 'none')
      }

    }
  }


  )

  output$household_plot_dl <- downloadHandler(filename = paste0("main_plot",Sys.Date(), ".png"),
                                              content = function(file) {
                                                datyear <- dat
                                                iyear <- input$year
                                                he <- input$household
                                                graph <- input$graph
                                                # save(datyear, iyear, he, graph, file = '/tmp/katebaker.RData' )

                                                if (iyear =='2019'){
                                                  datyear <- dat %>% dplyr::filter(year == 2019)
                                                  iyear <- as.numeric(iyear)
                                                } else if(iyear == "Total"){
                                                  datyear <- dat %>% dplyr::filter (year == 2021)
                                                } else if (iyear == '2021'){
                                                  datyear <- joined_19_21
                                                  iyear <- as.numeric(iyear)
                                                }




                                                ok <- FALSE
                                                if(!is.null(he)){
                                                  ok <- TRUE
                                                }

                                                if(ok){
                                                  # create plot based on households.
                                                  if (he == 'All') {
                                                    subhe <-  datyear  %>%
                                                      dplyr::group_by( species ) %>%
                                                      dplyr::summarize( Carbon = sum(calculations))
                                                  } else {
                                                    subhe <- datyear  %>%
                                                      dplyr::filter(household == he) %>%
                                                      dplyr::group_by( species ) %>%
                                                      dplyr::summarize( Carbon = sum(calculations))
                                                  }

                                                  if (he == 'All') {
                                                    payments <-  datyear  %>%
                                                      dplyr::group_by( species ) %>%
                                                      dplyr::summarise(Pay = (sum(calculations)*50))
                                                  } else {
                                                    payments <-datyear %>%
                                                      dplyr::filter(household == he)%>%
                                                      dplyr::group_by( species ) %>%
                                                      dplyr::summarise(Pay = (sum(calculations)*50))
                                                  }

                                                  if (iyear == 2021){
                                                    if (he == 'All') {
                                                      Num_Tree <- joined_19_21  %>%
                                                        dplyr::group_by(species ) %>%
                                                        dplyr::summarize (Tre = sum( sum_tree ))
                                                    } else{
                                                      Num_Tree <- joined_19_21  %>%
                                                        dplyr::filter( household == he ) %>%
                                                        dplyr::group_by(species ) %>%
                                                        dplyr::summarize (Tre = sum_tree )
                                                    }
                                                  } else {

                                                    if (he == 'All') {
                                                      Num_Tree <-  datyear  %>%
                                                        dplyr::group_by( species ) %>%
                                                        dplyr::summarise(Tre = length(household))
                                                    } else {
                                                      Num_Tree <-        datyear %>%
                                                        dplyr::filter(household == he)%>%
                                                        dplyr::group_by( species ) %>%
                                                        dplyr::summarise(Tre = length(household))
                                                    }
                                                  }

                                                  if(graph=='Carbon sequestered'){
                                                    ggplot(subhe, aes(x = species, y=Carbon, fill=species)) +
                                                      geom_bar(position = 'dodge', stat = 'identity') +
                                                      geom_text( aes(label=round(Carbon,3)), vjust=0) +
                                                      labs(title = paste0('CO2 Sequestered in Household ',he), x = 'Species', y = 'C02 Estimate (tons)') + ggthemes::theme_economist() + theme (legend.position = 'none')
                                                    ggsave(file, width = 8, height = 8)

                                                  } else if (graph == 'Carbon payments'){
                                                    ggplot(data = payments, aes(x = species, y = Pay, fill = species))+
                                                      geom_bar(position = 'dodge', stat = 'identity') +
                                                      geom_text( aes(label=round(Pay,2)), vjust=0) +
                                                      labs(title = paste0('Payments for Household ',he), x = 'Species', y = 'Payment in USD') + ggthemes::theme_economist() + theme (legend.position = 'none')
                                                    ggsave(file, width = 8, height = 8)

                                                  } else if (graph == 'Number of trees'){
                                                    ggplot(data = Num_Tree, aes(x = species, y = Tre, fill = species))+
                                                      geom_bar(position = 'dodge', stat = 'identity') +
                                                      geom_text( aes(label=Tre, vjust=0)) +
                                                      labs(title = paste0('Number of Trees for Household ',he), x = 'Species', y = 'Number of Trees') + ggthemes::theme_economist() + theme (legend.position = 'none')
                                                    ggsave(file, width = 8, height = 8)

                                                  }

                                                }

                                              })


}


shinyApp(ui, server)

