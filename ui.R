##ui.R##
# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title="GUN VIOLENCE"),
  
  dashboardSidebar(
    sidebarUserPanel("author: Rads"),
  
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Location", tabName = "location", icon = icon("bar-chart")),
      menuItem("Chart", tabName = "chart", icon = icon("bar-chart"),
               menuSubItem("Time Trends", tabName = "timetrends"),
               menuSubItem("Time Series", tabName = "timeseries"),
               menuSubItem("Age Distribution", tabName = "age"),
               menuSubItem("Gender Distribution", tabName = "gender")),
      menuItem("Data", tabName = "data", icon = icon("database")))
  ),
  dashboardBody(
    tabItems(
      
      # #Data
      # tabItems(
      #   tabItem(tabName = "data",
      #           fluidRow(box(DT::dataTableOutput("table"), width = 12))),
        
      #Map
      tabItem(tabName='map',
              fluidRow(box(
                leafletOutput("map",height = 650),width = 12))),
      
      #Location
      tabItem(tabName='location',
              fluidRow(
                box(sliderInput(inputId = "year_range", label = h4("Select the Year:"), 
                                min=2013, max=2018, step=1,
                                value = unique(df$year)))),
              fluidRow(
                box(plotlyOutput("location_plot1",height = 650),width = 12))),
                
      #TimeTrends
      tabItem(tabName = "timetrends",
              tabBox(
                tabPanel(
                  title='Exploring Time Trends of Gun Violence',
                         fluidRow(
                           plotlyOutput("trend_plot1",width="100%", height=500)),
                         fluidRow(
                           plotlyOutput("trend_plot3",height = 650),width = 12),
                         fluidRow(
                           plotlyOutput("trend_plot5",height = 650),width = 12)),
                tabPanel(
                  title='Exploring Time Trends of Gun Violence by Victims',
                         fluidRow(
                           plotlyOutput("trend_plot2",height = 650),width = 12),
                         fluidRow(
                           plotlyOutput("trend_plot4",height = 650),width = 12),
                         fluidRow(
                           plotlyOutput("trend_plot6",height = 650),width = 12)),
                width=12L)),
    
      #TimeSeries
      tabItem(tabName = "timeseries",
              fluidRow(
                plotlyOutput("series_plot1",height = 650),width = 12),
              fluidRow(
                plotlyOutput("series_plot2",height = 650),width = 12),
              fluidRow(
                plotlyOutput("series_plot3",height = 650),width = 12),
              fluidRow(
                plotlyOutput("series_plot4",height = 650),width = 12)),
      
      #Age
      tabItem(tabName = "age",
              fluidRow(
                plotlyOutput("age_plot1")),
              fluidRow(
                plotlyOutput("age_plot2"))),
      
      #Gender
      tabItem(tabName = "gender",
              fluidRow(
                plotlyOutput("gender_plot1")),
              fluidRow(
                plotlyOutput("gender_plot2")))
  )
)))

