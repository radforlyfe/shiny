##server.R##
shinyServer(function(input, output, session) {

#--------------------------------------------------------------------------------------------------------------------------
# #DataTable
#   output$table <- DT::renderDataTable({
#     datatable(data=df_map()) 
#   })
  
#--------------------------------------------------------------------------------------------------------------------------
#Maps
  df_map=reactive({
    df %>% 
      filter(is.na(longitude)==FALSE & is.na(latitude)==FALSE) %>% 
      filter(year!=2013 & year!=2018)
  })
  
  output$map=renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      setView(lng=-96, lat=37.8, zoom=4)
  })
  
  observe({
    proxy=leafletProxy("map", data=df_map()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers( lng = ~longitude,lat = ~latitude,
                        color = 'Red',radius =1,
                        clusterOptions = markerClusterOptions(),
                        group = 'CLUSTER') %>% 
      addCircleMarkers(lng = ~longitude,lat = ~latitude,
                       color = 'Red',radius =0.25,group = 'CIRCLE') %>% 
      addLayersControl(
        baseGroups = c("CLUSTER","CIRCLE"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
#--------------------------------------------------------------------------------------------------------------------------
#Location
  output$location_plot1 <- renderPlotly({
  df %>%
    filter((year>=input$year_range[1]) & (year<=input$year_range[2])) %>%
    group_by(state) %>%
    summarise(count = sum(n_victims)) %>% 
    ggplot(aes(x=reorder(state, desc(count)), y=count)) +
    geom_bar(aes(fill = count), stat='identity') +
    labs(x=' ', y=' ') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
#--------------------------------------------------------------------------------------------------------------------------
#TimeTrends
  #BarPlot of Gun Violence by Year
  output$trend_plot1 <- renderPlotly({
    df %>% 
      group_by(year) %>%
      summarise(count = sum(n_victims)) %>% 
      ggplot(aes(x=year, y=count)) +
      geom_bar(stat="identity", alpha=0.5, fill="green") +
      theme_classic() +
      theme(plot.title=element_text(hjust=0.5),
            axis.line = element_line()) + 
      labs(title="Gun Violence Incidents by Year",
           x="",
           y="")
  })
  
  #BarPlot of Gun Violence by Year - Split plot with dead and injured
  output$trend_plot2 <- renderPlotly({
  df %>% 
    filter(year!=2013 & year!=2018) %>% 
    group_by(year) %>%
    summarise(dead = sum(n_killed),
              injured = sum(n_injured)) %>%
    gather(key="victims", value=total, dead, injured) %>% 
    ggplot(aes(x=year)) +
    geom_col(alpha=0.5, aes(y=total, fill=victims)) + 
    #cannot do bar as it will only tell the count as two - need to use col and select y axis
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.line = element_line()) + 
    labs(title="Gun Violence Victims by Year",
         x="",
         y="")
  })
  
  #BarPlot of Gun Violence by Month
  output$trend_plot3 <- renderPlotly({
  df %>% 
    filter(year!=2013 & year!=2018) %>% 
    mutate(month = factor(month,label=mos)) %>% 
    group_by(month) %>%
    summarise(count = sum(n_victims)) %>%
    ggplot(aes(x=month, y=count)) +
    geom_bar(stat="identity", alpha=0.5, fill="pink") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line(),
          plot.title=element_text(hjust=0.5),
          panel.background = element_blank()) +
    labs(title="Gun Violence Incidents by Month",
         x="",
         y="")
  })
  
  #BarPlot of Gun Violence by MONTH - Split plot with dead and injured
  output$trend_plot4 <- renderPlotly({
  df %>% 
    filter(year!=2013 & year!=2018) %>% 
    mutate(month = factor(month,label=mos)) %>% 
    group_by(month) %>%
    summarise(dead = sum(n_killed),
              injured = sum(n_injured)) %>%
    gather(key=victims, value=total, dead, injured) %>% 
    ggplot(aes(x=month)) +
    geom_col(alpha=0.5, aes(y=total, fill=victims)) +
    #coord_flip() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line(),
          plot.title=element_text(hjust=0.5),
          panel.background = element_blank()) +
    labs(title="Gun Violence Victims by Month",
         x="",
         y="")
  })
  
  #BarPlot of Gun Violence by Day of the Week
  output$trend_plot5 <- renderPlotly({
  df %>% 
    mutate(weekday = weekdays(date),wd=wday(date))%>% 
    filter(year!=2013 & year!=2018) %>%
    group_by(weekday,wd) %>%
    summarize(count = sum(n_victims)) %>%
    #group_by(weekday) %>% 
    #summarize(count=sum(n_victims),wd=max(wd)) %>% 
    ggplot(aes(x=reorder(weekday,((wd+5)%%7)), y=count)) +
    geom_bar(stat="identity", alpha=0.5, fill="purple") +
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.line = element_line()) +
    labs(title="Gun Violence Incidents by Day of Week",
         x="",
         y="")
  })
  
  #BarPlot of Gun Violence by Day of the Week - Split plot with dead and injured
  output$trend_plot6 <- renderPlotly({
  df %>% 
    mutate(weekday = weekdays(date),wd=wday(date))%>% 
    filter(year!=2013 & year!=2018) %>%
    group_by(weekday,wd) %>%
    summarize(dead = sum(n_killed),
              injured = sum(n_injured)) %>%
    gather(key=victims, value=total, dead, injured) %>% 
    ggplot(aes(x=reorder(weekday,((wd+5)%%7)))) +
    geom_col(alpha=0.5, aes(y=total, fill=victims)) +
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.line = element_line()) +
    labs(title="Gun Violence Incidents by Day of Week",
         x="",
         y="")
  })
  
#--------------------------------------------------------------------------------------------------------------------------
#Time Series plot of Total Incidents, People Killed, People Injured
  #2014
  output$series_plot1 <- renderPlotly({
  df %>% 
    filter(year==2014) %>% 
    group_by(date) %>% 
    summarize(Total_Killed = sum(n_killed),
              Total_Injured = sum(n_injured)) %>%
    mutate(Total_Incidents = Total_Killed + Total_Injured) %>% 
    gather(key=victims, value=total, Total_Killed, Total_Injured, Total_Incidents) %>% 
    ggplot(aes(x=date,y=total)) +
    geom_line(aes(color=victims), size=1) + 
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line()) +
    scale_color_manual(values=c('#c5d9f9', "#e8baff", "#ff9f87")) +
    labs(title="Gun Violence Incidents - 2014",
         x=" ",
         y=" ")
  })
  
  #2015
  output$series_plot2 <- renderPlotly({
  df %>% 
    filter(year==2015) %>% 
    group_by(date) %>% 
    summarize(Total_Killed = sum(n_killed),
              Total_Injured = sum(n_injured)) %>%
    mutate(Total_Incidents = Total_Killed + Total_Injured) %>% 
    gather(key=victims, value=total, Total_Killed, Total_Injured, Total_Incidents) %>% 
    ggplot(aes(x=date,y=total)) +
    geom_line(aes(color=victims), size=1) + 
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line()) +
    scale_color_manual(values=c('#c5d9f9', "#e8baff", "#ff9f87")) +
    labs(title="Gun Violence Incidents - 2015",
         x=" ",
         y=" ")
  })
  
  
  #2016
  output$series_plot3 <- renderPlotly({
  df %>% 
    filter(year==2016) %>% 
    group_by(date) %>% 
    summarize(Total_Killed = sum(n_killed),
              Total_Injured = sum(n_injured)) %>%
    mutate(Total_Incidents = Total_Killed + Total_Injured) %>% 
    gather(key=victims, value=total, Total_Killed, Total_Injured, Total_Incidents) %>% 
    ggplot(aes(x=date,y=total)) +
    geom_line(aes(color=victims), size=1) + 
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line()) +
    scale_color_manual(values=c('#c5d9f9', "#e8baff", "#ff9f87")) +
    labs(title="Gun Violence Incidents - 2016",
         x=" ",
         y=" ")
  })
  
  #2017
  output$series_plot4 <- renderPlotly({
  df %>% 
    filter(year==2017) %>% 
    group_by(date) %>% 
    summarize(Total_Killed = sum(n_killed),
              Total_Injured = sum(n_injured)) %>%
    mutate(Total_Incidents = Total_Killed + Total_Injured) %>% 
    gather(key=victims, value=total, Total_Killed, Total_Injured, Total_Incidents) %>% 
    ggplot(aes(x=date,y=total)) +
    geom_line(aes(color=victims), size=1) + 
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.line = element_line()) +
    scale_color_manual(values=c('#c5d9f9', "#e8baff", "#ff9f87")) +
    labs(title="Gun Violence Incidents - 2017",
         x=" ",
         y=" ")
  })

#--------------------------------------------------------------------------------------------------------------------------
#Age Distribution
  output$age_plot1 <- renderPlotly({
  abc1 %>% 
    filter(participant_type=="Victim") %>% 
    ggplot(aes(x=participant_type)) +
    geom_bar(aes(fill=participant_age_group), position="dodge") +
    #coord_polar(theta="y") + 
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) + 
    labs(title="Victims Age Distribution",
         x=" ",
         y=" ")
  })
  
  output$age_plot2 <- renderPlotly({
  abc1 %>% 
    filter(participant_type=="Subject") %>% 
    ggplot(aes(x=participant_type)) +
    geom_bar(aes(fill=participant_age_group), position="dodge") +
    #coord_polar(theta="y") + 
    theme_classic() +
    theme(plot.title=element_text(hjust=0.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) + 
    labs(title="Suspect Age Distribution",
         x=" ",
         y=" ")
  })

#--------------------------------------------------------------------------------------------------------------------------
#Gender
  output$gender_plot1 <- renderPlotly({
    abc1 %>% 
      filter(participant_type=="Victim") %>% 
      ggplot(aes(x=participant_type)) +
      geom_bar(aes(fill=participant_gender), position="dodge") +
      #coord_polar(theta="y") + 
      theme_classic() +
      theme(plot.title=element_text(hjust=0.5),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) + 
      labs(title="Victims Gender Distribution",
           x=" ",
           y=" ")
  })
  
  output$gender_plot2 <- renderPlotly({
    abc1 %>% 
      filter(participant_type=="Subject") %>% 
      ggplot(aes(x=participant_type)) +
      geom_bar(aes(fill=participant_gender), position="dodge") +
      #coord_polar(theta="y") + 
      theme_classic() +
      theme(plot.title=element_text(hjust=0.5),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) + 
      labs(title="Suspect Gender Distribution",
           x=" ",
           y=" ")
  })
  
  
})
