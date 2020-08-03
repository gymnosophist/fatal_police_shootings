## Shiny app build 
## install rsconnect
install.packages('rsconnect')
install.packages('shiny')
install.packages('readr')
install.packages('tidyverse')
library(shiny)
library(readr)
library(tidyverse)
library(lubridate)

## Load the data locally 
data <- read.csv('https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv'
                )

##tidy 
#data <- data %>% as_tibble()
#cols <- c('id', 'manner_of_death', 'armed', 'gender', 'race', 'city', 'state', 'signs_of_mental_illness', 'threat_level', 'flee')

#data <- data %>% filter(race != '' & 
#                          armed != '' & 
#                          gender != '' & 
#                          flee != '')

# data[cols] <- lapply(data[cols], as.factor) #lapply applies the function to all columns 

# data$date <- as.Date(data$date)

# df <- data # backup 

## build app 
rsconnect::setAccountInfo(name='gymnosophist', 
                          token='3804590CF532E4A541C47F1ACAA05C33',
                          secret='M2nG+OAh0MuN3c2xySSRlKIKv/oChegaHREzSl4J') # obviously hide this 

rsconnect::deployApp(getwd())

## Not sure where to define variables -- in the UI? Could take longer to load 

### start here 

data <- read.csv('final_df.csv')

test <- data[1:1000, ]

test <- test %>% mutate(month = month(ymd(date)), 
                        day = yday(ymd(date)), 
                        year = year(ymd(date)))                     

cols <- c('X', 'id')

test <- test[, !names(test) %in% cols]
test <- test %>% rename(city = city_x)

# set color palette 
# 'YlOrBr' 



## build app 
ui <- fluidPage( ## update layout -- maximize space for map, choose a graphic to display underneath 
  titlePanel(
  'Tracking Police Shootings -- Visualizing Data from the Washington Post'),
  selectInput('state', 'Select State:',
              choices = sort(data$state),
              selected = 'CA'),
  fluidRow(column(6, 
    leafletOutput('map')),
    column(6,
           h4("What's the pace of fatal police shootings?"),
                   # checkboxGroupInput('year_choose', label = 'Choose a year to include', 
                   #                    choices = c(2015, 2016, 2017, 2018, 2019, 2020), 
                   #                    selected = c(2015, 2016, 2017, 2018, 2019, 2020)),
           plotOutput('ytd_fatalities'))),
  # fluidRow(column(3,
  #                 h4('Select state'),
  #                 )),
  fluidRow(column(6,
             h4('Fatal shootings by demographic in selected state'),
             plotOutput('demos')), 
           column(6, 
                  h4('How do state demographics compare to fatal shootings?'), 
                  plotOutput('state_demos'))
  ))

server <- function(input,output, session){
  ### get and update data 
  
  # test <- fread(url)
  
  ### set variables used in outputs 
  filterData = reactiveVal(data)
  
  filtered_df <- reactive({
    filterData <- filterData() %>% filter(state == input$state)
  })
  
  filterStates <- reactiveVal(data)
  
  state_totals <- reactive({
    
    state_totals <- filterStates() %>% arrange(date) %>%  group_by(state, date) %>%  mutate(total = sum(id)) %>%
      ungroup()
    
    state_totals %>% 
      filter(state == input$state) %>% 
      mutate(day_of_year = yday(date), 
             year = year(date)) %>%
      group_by(year) %>%
      mutate(cv = cumsum(total)) 
 
  })
  
  state_demos <- reactive({
    
    demos %>% 
      filter(state == input$state) %>% 
      mutate(start = 0, 
             end = 1)
    
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(dragging = T, 
                                     maxZoom = 10)) %>% 
      addProviderTiles(provider = 'CartoDB.PositronNoLabels') %>% 
      setView(lat = 39.8278515, 
              lng = -98.5883632, 
              zoom = 4) %>% 
      addCircleMarkers(group = data$city_state,
                       lng = data$lon, lat = data$lat, 
                       stroke = F, 
                       radius = 3,
                       label = paste0(data$name, data$armed), 
                       clusterOptions = markerClusterOptions())
      # add counties 
      
  })
  
  output$demos <- renderPlot({ # needs better aesthetics 
    ggplot(filtered_df(), aes(x = year(ymd(date)))) + 
      geom_density( alpha = .9, position = 'identity') +
      facet_grid(body_camera ~race, labeller = labeller(body_camera = c('True' = 'Body camera enabled',
                                                                        'False' = 'No body camera'))) + 
      scale_color_brewer(palette = 'YlOrBr') + 
      theme(panel.grid = element_blank(), 
            panel.background = element_blank(), 
            axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab('Number of fatal police shootings by year') + 
      ylab('Was the officer wearing a body camera?')
      })
  
  output$ytd_fatalities <- renderPlot({ # needs better aesthetics 
    ggplot(state_totals(), aes(x = day_of_year, y = cv, color = factor(year))) + 
      geom_point(aes(shape = factor(year), color = factor(year))) + 
      geom_line() + scale_color_brewer(palette = 'YlOrBr') + 
      theme(axis.text.y.left = element_blank()) + 
      theme(panel.background = element_blank()) +
      xlab('Day of the Year') + 
      ylab('Cumulative Fatal Shootings by Police')
  })
  
  output$state_demos <- renderPlot({
    ggplot(state_demos(), aes( label = race, color = race)) + 
      geom_segment(aes(x = start, 
                   xend = end, 
                   y = pct_of_pop, 
                   yend = racial_total)) + 
      geom_label(aes(x = 0, 
                     y = pct_of_pop)) + 
      theme_bw()
    
    
  })

}

shinyApp(ui = ui, server = server)

#################
#################
#################
# Tests 
#################
#################
#################

test2 %>% filter(year %in% c(2020, 2017)) %>% tail()


install.packages('zoo')
library(zoo)

test2 <- test %>% mutate(year_mon = as.yearmon(date), 
                         id = 1)
test2 <- test2 %>% arrange((date)) %>% group_by(date) %>% summarise(total = sum(id))

test2 <- test2%>% mutate(year_mon = as.yearmon(date))

#test[c('lat', 'lon')] <-  lapply(test[c('lat','lon')], function(x) jitter(x))
#head(test)

#test <- test %>% mutate(id = 1)
test2 %>% group_by(year(year_mon), month(year_mon)) %>% summarise(monthly_total = sum(total))
test2 <- test2 %>% mutate(day_of_year = yday(date), 
                 year = year(date))
ggplot(test2 %>% group_by(year) %>% mutate(cv = cumsum(total)), 
       aes(x = day_of_year, y = cv, color = factor(year))) + 
  geom_line() + 
  scale_color_brewer(palette = 'YlOrBr') + ylab('Year') + labs(color = 'Year') + xlab('Fatal shootings')

test3 <- test2 %>% group_by(year) %>% mutate(cv = cumsum(total))

test3 %>% tail

ggplot(test3, aes(x = day_of_year,y = cv, color = factor(year))) + geom_line()

state_totals <- data %>% arrange(date) %>%  group_by(state, date) %>%  mutate(total = sum(id)) %>%
  ungroup()
  
state_totals <- state_totals %>% mutate(year = year(date))

breaks <- unique(state_totals$year)

state_totals %>% filter(state == 'CA') %>% mutate(day_of_year = yday(date), 
                                                  year = year(date)) %>%
  group_by(year) %>%
  mutate(cv = cumsum(total)) %>%
  ggplot( aes( x = day_of_year, y = cv, color = factor(year))) + 
  geom_line() + 
  theme(axis.text.y.left = element_blank())
  scale_color_brewer(palette = 'YlOrBr')
                                                                 

ggplot(state_totals, aes(x = day_of_year, y = cv, color = factor(year))) + geom_line()

demos <- read.csv('state_populations_with_shootings.csv')

head(demos)

demos %>%
  mutate(start = 0, 
         end = 1) %>% 
  filter(state == 'CA') %>% 
  ggplot(aes(label = race, color = race)) + 
  geom_segment(aes(x =  start, 
                   xend = end, 
                   y = pct_of_pop, 
                   yend = racial_total)) + 
  xlab('Rate of Fatal Shootings') + 
  theme_bw() + 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) + 
  geom_label(aes(x = 0, y = pct_of_pop)) + 
  geom_label(aes(x = 1, y = racial_total)) + 
  scale_color_brewer(palette = 'YlOrBr')


head(data)
