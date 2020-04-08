# Libraries Used:
library(leaflet)
library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(png)
library(jpeg)
library(DT)
library(rmarkdown)

# Data Load
town_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/RI_Town_Data_All.csv", stringsAsFactors = FALSE)
age_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/Age_Distribution_Long.csv", stringsAsFactors = FALSE)
gender_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/Gender.csv", stringsAsFactors = FALSE)
hospital_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/Hospital%20Long.csv", stringsAsFactors = FALSE)
new_cases_long <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/New_Cases_Long.csv", stringsAsFactors = FALSE)
ri_confirmed_cases_wide <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/New_Cases_Wide.csv", stringsAsFactors = FALSE)

# Editing Town Data to be consistent
town_data$Town <- str_squish(town_data$Town)
town_data$Date <- str_squish(town_data$Date)
town_data$Cases <- str_squish(town_data$Cases)

# Editing Dates
town_data$Date <- as.Date(town_data$Date, format="%m/%d/%y")
age_data$Date <- as.Date(age_data$Date, format= "%m/%d/%y")
gender_data$Date <- as.Date(gender_data$Date, format= "%m/%d/%y")
hospital_data$Date <- as.Date(hospital_data$Date, "%m/%d/%y")
new_cases_long$Date <- as.Date(new_cases_long$Date, "%m/%d/%y")
ri_confirmed_cases_wide$Date <- as.Date(ri_confirmed_cases_wide$Date, "%m/%d/%y")

# Sort town Data
town_data_descending <- town_data[rev(order(as.Date(town_data$Date))),]

# Server
server <- function(input, output) {
  # rhody_map
  output$rhody_map = renderLeaflet({
    if(input$rhody_map_date != "Most Recent Data"){
      town_data <- town_data %>% 
        filter(Date == as.Date(input$rhody_map_date))
    }
    # Map Colors
    quantileNum <- 3
    probs <- seq(0, 1, length.out = quantileNum + 1)
    bins <- quantile(filtered_data()$Case_Color, probs, na.rm = TRUE, names = FALSE)
    while (length(unique(bins)) != length(bins)) {
      quantileNum <- quantileNum - 1
      probs <- seq(0, 1, length.out = quantileNum + 1)
      bins <- quantile(filtered_data()$Case_Color, probs, na.rm = TRUE, names = FALSE)
    }
    
    pal <- colorBin("YlOrRd", bins = bins)
    
    # Map
    ri_map <- leaflet(town_data) %>% 
      setView(lng = -71.4774, lat= 41.65, zoom=9)
    
    ri_map <- ri_map %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircles(lng=~Longitude, lat=~Latitude,
                 weight = 8,
                 radius = ~sqrt(Case_Color) * 100,
                 color = pal(town_data$Case_Color),
                 fillOpacity = 1,
                 label = paste(town_data$Town, "has ", town_data$Cases, "cases (click for details)"),
                 popup = paste(town_data$Town, "<br> City/Town Population:",
                               town_data$Population, "<br> Date:", 
                               town_data$Date, "<br> Number of Cases: ",
                               town_data$Cases, "<br> Percentage of RI Cases: ",
                               town_data$Case_Rate, "<br>",
                               town_data$Case, " case per ", town_data$Case_Per_Resident, " residents")) %>% 
      addLegend("bottomright", pal = pal, title = "Cases in City/Town", values = town_data$Case_Color)
    
    ri_map
  })
  
  # Animated Map:
  filtered_data <- reactive({
    town_data %>% 
      filter(Date == input$rhody_animate_date)
  })
  
  # Animated Map Tables
  output$rhody_animate_cases <- renderText({
    paste("Total Cases: ", unique(filtered_data()$Total_Cases), sep ="")
  })
  
  filtered_town_data <- reactive({
    town_data %>% 
      filter(Date == input$rhody_animate_date) %>% 
      filter(Town == input$rhody_animated_map_town)
  })
  
  output$rhody_animate_town_name <- renderText({
    paste(input$rhody_animated_map_town, " Statistics:", sep ="")
  })
  
  output$rhody_animate_date <- renderText({
    paste("Date: ", unique(filtered_data()$Date), sep="")
  })
  
  output$rhody_animate_town_cases <- renderText({
    paste("Total Cases: ", unique(filtered_town_data()$Cases))
  })
  
  output$rhody_animate_town_new_cases <- renderText({
    paste("New Cases: ", unique(filtered_town_data()$New_Cases))
  })
  
  output$rhody_animate_town_cases_per_resident <- renderText({
    paste("One Case Per ", unique(filtered_town_data()$Case_Per_Resident), "Residents")
  })
  
  output$rhody_animate_town_cases_state <- renderText({
    paste("Percentage of States Cases: ", unique(filtered_town_data()$Case_Rate))
  })
  
  output$rhody_animate_state_new_cases <- renderText({
    paste("New Cases: ", unique(filtered_data()$State_New_Cases))
  })
  
  output$rhody_animate_state_mortality <- renderText({
    paste("Mortalities: ", unique(filtered_data()$Mortality))
  })
  
  output$rhody_animate_state_hospitalization <- renderText({
    paste("Residents in Hospital: ", unique(filtered_data()$Hospitalization))
  })
  
  # Base Map
  output$rhody_map_animate<- renderLeaflet({
    ri_base_map <- leaflet() %>% 
      setView(lng = -71.4774, lat= 41.65, zoom=9) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    ri_base_map
  })
  
  # Modified Map
  observe({
    quantileNum <- 3
    probs <- seq(0, 1, length.out = quantileNum + 1)
    bins <- quantile(filtered_data()$Case_Color, probs, na.rm = TRUE, names = FALSE)
    while (length(unique(bins)) != length(bins)) {
      quantileNum <- quantileNum - 1
      probs <- seq(0, 1, length.out = quantileNum + 1)
      bins <- quantile(filtered_data()$Case_Color, probs, na.rm = TRUE, names = FALSE)
    }
    
    pal <- colorBin("YlOrRd", bins = bins)
    leafletProxy("rhody_map_animate", data=filtered_data()) %>% 
      clearShapes() %>% 
      
      addCircles(lng=~Longitude,
                 lat=~Latitude,
                 weight = 8,
                 radius = ~sqrt(Case_Color) * 100,
                 color = ~pal(Case_Color),
                 fillOpacity = 1,
                 label = paste(filtered_data()$Town, "has ", filtered_data()$Cases, "cases (click for details)"),
                 popup = paste(filtered_data()$Town, "<br> City/Town Population:",
                               filtered_data()$Population, "<br> Date:", 
                               filtered_data()$Date, "<br> Number of Cases: ",
                               filtered_data()$Cases, "<br> Percentage of RI Cases: ",
                               filtered_data()$Case_Rate, "<br>",
                               filtered_data()$Case, " case per ", filtered_data()$Case_Per_Resident, " residents"))
    
  })
  
  # Town Graph 
  output$rhody_map_town_detailed <- renderPlotly(ggplotly({
    town_data <- town_data %>% 
      filter(Town == input $rhody_detailed_graph_city_town)
    if(input$rhody_detailed_graph_start_date != min(town_data$Date)) {
      town_data <- town_data %>% 
        filter(Date >= as.Date(input$rhody_detailed_graph_start_date))
    }
    if(input$rhody_detailed_graph_end_date != "All"){
      town_data <- town_data %>% 
        filter(Date <= as.Date(input$rhody_detailed_graph_end_date))
    }
    
    ggplot(town_data, aes(x=Date, y = Case_Color)) +
      geom_line(color = "steelblue3",
                size = .5) +
      labs(title =  paste0(input$rhody_detailed_graph_city_town," Confirmed Cases"), 
           x = "Date", 
           y = "Cases") +
      theme_classic()
  }))
  
  # Rhode Island Table
  output$rhody_map_table <- DT::renderDataTable(DT::datatable({
    if (input$town_start_date != "All") {
      town_data <- town_data %>% 
        filter(Date >= as.Date(input$town_start_date))
    }
    if (input$town_end_date != "All") {
      town_data <- town_data %>% 
        filter(Date <= as.Date(input$town_end_date))
    }
    if (input$Town_Selected != "All") {
      town_data <- town_data[town_data$Town == input$Town_Selected,]
    }
    if (input$County_Selected != "All") {
      town_data <- town_data[town_data$County == input$County_Selected,]
    }
    town_data
  }))
  
  # Cases Graph
  output$rhody_confirmed_cases <- renderPlotly(ggplotly({
    if (as.Date(input$cases_start_date) != min(new_cases_long$Date)) {
      new_cases_long %>% 
        filter(Date >= as.Date(input$cases_start_date))
    }
    
    if(input$cases_end_date != "All"){
      new_cases_long <- new_cases_long %>% 
        filter(Date <= as.Date(input$cases_end_date))
    }
    if(input$cases_plot_type == "Line"){
      cases_graph <- ggplot(new_cases_long, aes(x=Date, y=Data, color = Category)) +
        geom_line() +
        labs(title="Confirmed Cases", x = "Date", y="Cases") +
        theme_classic() + scale_color_manual(name = "Category",
                                             labels=c("Confirmed Cases", "Daily Increase"),
                                             values=c("steelblue3", "firebrick"))
    } else {
      cases_graph <- ggplot(new_cases_long, aes(x=Date, y=Data, fill = Category)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title="Confirmed Cases", x = "Date", y="Cases") +
        theme_classic()+ scale_fill_manual(name = "Category",
                                           values = c("steelblue3", "firebrick"),
                                           labels=c("Confirmed Cases", "Daily Increase"))
    }
    
    cases_graph
  }))
  
  # Cases Table
  output$rhody_confirmed_cases_table <- DT::renderDataTable({
    if(as.Date(input$cases_table_start_date) != min(ri_confirmed_cases_wide$Date)) {
      ri_confirmed_cases_wide <- ri_confirmed_cases_wide %>% 
        filter(Date >= as.Date(input$cases_table_start_date))
    }
    if (input$cases_table_end_date != "All") {
      ri_confirmed_cases_wide <- ri_confirmed_cases_wide %>% 
        filter(Date <= as.Date(input$cases_table_end_date))
    }
    ri_confirmed_cases_wide
  })
  
  # Age Plot
  output$Age_Plot <- renderPlotly(ggplotly({
    if (input$age_plot_type == "Aggregate") {
      if (as.Date(input$age_start_date) != min(age_data$Date)) {
        age_data <- age_data %>% 
          filter(Date >= as.Date(input$age_start_date))
      }
      if (input$age_end_date != "All") {
        age_data <- age_data %>% 
          filter(Date <= as.Date(input$age_end_date))
      }
      age_graph <- ggplot(data=age_data, aes(x=Date, y=Number_of_Cases, fill=Age_Category)) +
        geom_bar(stat="identity", position=position_dodge()) + labs(title = "Cases By Age", 
                                                                    x= "Date", y = "Number of Cases", 
                                                                    fill = "Age Category") + 
        scale_fill_brewer(palette="Blues")
    }
    if (input$age_plot_type == "Percentage"){
      if (as.Date(input$age_start_date) != min(age_data$Date)) {
        age_data <- age_data %>% 
          filter(Date >= as.Date(input$age_start_date))
      }
      if (input$age_end_date != "All") {
        age_data <- age_data %>% 
          filter(Date <= as.Date(input$age_end_date))
      }
      age_graph <- ggplot(data=age_data, aes(x=Date, y=Percentage_of_Cases, fill=Age_Category)) +
        geom_bar(stat="identity", position=position_dodge()) + labs(title = "Percentage of Cases By Age", 
                                                                    x= "Date", 
                                                                    y = "Percentage of Cases",
                                                                    fill = "Age Category") + 
        scale_fill_brewer(palette="Blues")
    }
    age_graph
  }))
  # Age Table
  output$age_table <- DT::renderDataTable(DT::datatable({
    if (as.Date(input$age_start_date) != min(age_data$Date)) {
      age_data <- age_data %>% 
        filter(Date >= as.Date(input$age_table_start_date))
    }
    if (input$age_table_end_date != "All") {
      age_data <- age_data %>% 
        filter(Date <= as.Date(input$age_table_end_date))
    }
    if (input$age_age_category != "All") {
      age_data <- age_data %>% 
        filter(Age_Category == input$age_age_category)
    }
    age_data}))
  
  # Gender Graph
  
  output$gender_plot <- renderPlotly(ggplotly({
    if (input$gender_plot_type == "Aggregate") {
      if (as.Date(input$gender_plot_start_date) != gender_data$Date) {
        gender_data <- gender_data %>% 
          filter(Date >= as.Date(input$gender_plot_start_date))
      }
      if (input$gender_plot_end_date != "All") {
        gender_data <- gender_data %>% 
          filter(Date <= as.Date(input$gender_plot_end_date))
      }
      if (input$gender_plot_gender != "All") {
        gender_data <- gender_data %>% 
          filter(Gender == input$gender_plot_gender)
      }
      gender_graph <- ggplot(data=gender_data, aes(x=Date, y=Aggregate, fill= Gender)) +
        geom_bar(stat="identity", position=position_dodge()) +
        labs(title = "Cases By Gender", x="Date", y="Number of Cases") +
        scale_fill_manual(name = "Category",
                          values = c("darkgray", "steelblue3")) + theme_classic()
    }
    if (input$gender_plot_type == "Percentage") {
      if (as.Date(input$gender_plot_start_date) != gender_data$Date) {
        gender_data <- gender_data %>% 
          filter(Date >= as.Date(input$gender_plot_start_date))
      }
      if (input$gender_plot_end_date != "All") {
        gender_data <- gender_data %>% 
          filter(Date <= as.Date(input$gender_plot_end_date))
      }
      if (input$gender_plot_gender != "All") {
        gender_data <- gender_data %>% 
          filter(Gender == input$gender_plot_gender)
      }
      gender_graph <- ggplot(data=gender_data, aes(x=Date, y=Percentage, fill= Gender)) +
        geom_bar(stat="identity", position=position_dodge()) +
        labs(title = "Cases By Gender", x="Date", y="Percentage of Cases") +
        scale_fill_manual(name = "Category",
                          values = c("darkgray", "steelblue3"))+ theme_classic()
    }
    gender_graph
  }))
  
  # Gender Table
  output$gender_table <- DT::renderDataTable(DT::datatable({
    if (as.Date(input$gender_table_start_date) != min(gender_data$Date)) {
      gender_data <- gender_data %>% 
        filter(Date >= as.Date(input$gender_table_start_date))
    }
    if (input$gender_table_end_date != "All") {
      gender_data <- gender_data %>% 
        filter(Date <= as.Date(input$gender_table_end_date))
    }
    if (input$gender_table_gender != "All") {
      gender_data <- gender_data %>% 
        filter(Gender == input$gender_table_gender)
    }
    gender_data}))
  
  # Hospital Graph
  output$hospital_plot <- renderPlotly(ggplotly({
    if (as.Date(input$hospital_plot_start_date) != min(hospital_data$Date)) {
      hospital_data <- hospital_data %>% 
        filter(Date >= as.Date(input$hospital_plot_start_date))
    }
    if (input$hospital_plot_end_date != "All") {
      hospital_data <- hospital_data %>% 
        filter(Date <= as.Date(input$hospital_plot_end_date))
    }
    if (input$hospital_plot_category != "All") {
      hospital_data <- hospital_data %>% 
        filter(Category == input$hospital_plot_category)
    }
    
    if (input$hospital_data_type == "Aggregate") {
      if (input$hospital_plot_type == "Line Graph"){
        rhody_hospital <- ggplot(hospital_data, aes(x=Date, y=Aggregate, color = Category)) +
          geom_line() +
          labs(title="Hospitalization and Mortality", x = "Date", y="Percentage to Residents Who Tested Positive") +
          theme_classic() + scale_color_manual(name = "Category",
                                               labels=c("Hospitalization", "Mortality"),
                                               values=c("steelblue3", "firebrick"))
      }
      if (input$hospital_plot_type == "Bar Graph") {
        rhody_hospital <- ggplot(hospital_data, aes(x=Date, y=Aggregate, fill = Category)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title="Hospitalization and Mortality", x = "Date", y="Number of Residents") +
          theme_classic() + scale_fill_manual(name = "Category",
                                              values = c("steelblue3", "firebrick"))
      } 
    }
    
    if(input$hospital_data_type == "Percentage") {
      if (input$hospital_plot_type == "Line Graph") {
        rhody_hospital <- ggplot(hospital_data, aes(x=Date, y=Rate, color = Category)) +
          geom_line() +
          labs(title="Hospitalization and Mortality", x = "Date", y="Percentage to Residents Who Tested Positive") +
          theme_classic() + scale_color_manual(name = "Category",
                                               labels=c("Hospitalization", "Mortality"),
                                               values=c("steelblue3", "firebrick"))
      }
      if (input$hospital_plot_type == "Bar Graph") {
        rhody_hospital <- ggplot(hospital_data, aes(x=Date, y=Rate, fill = Category)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title="Hospitalization and Mortality", x = "Date", y="Percentage to Residents Who Tested Positive") +
          theme_classic() + scale_fill_manual(name = "Category",
                                              labels=c("Hospitalization", "Mortality"),
                                              values = c("steelblue3", "firebrick"))
      }
    }
    rhody_hospital
  }))
  
  # Hospital Table
  output$rhody_hospital_table <- DT::renderDataTable(DT::datatable({
    if (input$hospital_table_start_date != min(hospital_data$Date)) {
      hospital_data <- hospital_data %>% 
        filter(Date >=  as.Date(input$hospital_table_start_date))
    }
    if (input$hospital_table_end_date != "All") {
      hospital_data <- hospital_data %>% 
        filter(Date <= as.Date(input$hospital_table_end_date))
    }
    if(input$hospital_table_category != "All") {
      hospital_data <- hospital_data[hospital_data$Category == input$hospital_table_category,]
    }
    hospital_data   
  }))
  
  # Providence Journal
  output$providence_journal <- renderUI({
    tags$iframe(src=projo_url, height=600, width = 1200, scrolling = "auto")
  })
}

# ui
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Rhode Island COVID-19",
             navbarMenu("Town/City Information",
                        tabPanel("Animated Rhode Island Map",
                                 fluidPage(
                                   titlePanel("Animated Rhode Island Map:"),
                                   h4("(click on city/town for more information)"),
                                   fluidRow(
                                     column(4,
                                            sliderInput("rhody_animate_date", "Date:", 
                                                        min(town_data$Date),
                                                        max(town_data$Date),
                                                        value = max(town_data$Date),
                                                        step = 1,
                                                        animate= TRUE)),
                                     column(4, 
                                            selectInput("rhody_animated_map_town",
                                                        "Specific City/Town Information:",
                                                        unique(as.character(town_data$Town))))),
                                   h4(textOutput("rhody_animate_date")),
                                   leafletOutput("rhody_map_animate"),
                                   h5("Note: color is relative to the number of cases in Rhode Island on that particular day"),
                                   hr(),
                                   h4("State Statistics:"),
                                   fluidRow(
                                     column(2, 
                                            h5(textOutput("rhody_animate_cases"))),
                                     column(2,
                                            h5(textOutput("rhody_animate_state_new_cases"))),
                                     column(4,
                                            h5(textOutput("rhody_animate_state_hospitalization"))),
                                     column(4, 
                                            h5(textOutput("rhody_animate_state_mortality")))),
                                   h4(textOutput("rhody_animate_town_name")),
                                   fluidRow(
                                     column(2,
                                            h5(textOutput("rhody_animate_town_cases"))),
                                     column(2,
                                            h5(textOutput("rhody_animate_town_new_cases"))),
                                     column(4, 
                                            h5(textOutput("rhody_animate_town_cases_per_resident"))),
                                     column(4,
                                            h5(textOutput("rhody_animate_town_cases_state")))))),
                        
                        tabPanel("Detailed Rhode Island Map:",
                                 titlePanel("Detailed Rhode Island Map:"),
                                 h4("(click on city/town for more information)"),
                                 fluidRow(
                                   column(3,
                                          selectInput("rhody_map_date",
                                                      "Date:",
                                                      unique(as.character(town_data$Date)),
                                                      selected = max(town_data$Date)))),
                                 leafletOutput("rhody_map"),
                                 
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("rhody_detailed_graph_city_town",
                                                 "City/Town:",
                                                 unique(as.character(town_data$Town))),
                                     selectInput("rhody_detailed_graph_start_date",
                                                 "Start Date:",
                                                 unique(as.character(town_data$Date))),
                                     selectInput("rhody_detailed_graph_end_date",
                                                 "End Date:",
                                                 c("All",
                                                   unique(as.character(town_data$Date))))),
                                   mainPanel(                                 
                                     titlePanel("City/Town Graphs:"),
                                     h4('(City/Towns that have ">5" cases are given a value of 2)'),
                                     plotlyOutput("rhody_map_town_detailed")))),
                        tabPanel("City/Town Data:",
                                 titlePanel("Town Data Table:"),
                                 fluidRow(
                                   column(3,
                                          selectInput("town_start_date",
                                                      "Start Date:",
                                                      c("All",
                                                        unique(as.character(town_data$Date))))
                                   ),
                                   column(3,
                                          selectInput("town_end_date",
                                                      "End Date:",
                                                      c("All",
                                                        unique(as.character(town_data$Date))))
                                   ),
                                   column(3,
                                          selectInput("Town_Selected",
                                                      "City/Town:",
                                                      c("All",
                                                        unique(as.character(town_data$Town))))
                                   ),
                                   column(3,
                                          selectInput("County_Selected",
                                                      "County:",
                                                      c("All",
                                                        unique(as.character(town_data$County))))
                                   )),
                                 DT::dataTableOutput("rhody_map_table"))),
             navbarMenu("State Data",
                        tabPanel("Cases",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons("cases_plot_type",
                                                    "Plot Type:",
                                                    c("Line", "Bar")),
                                       selectInput("cases_start_date",
                                                   "Start Date:",
                                                   c(unique(as.character(new_cases_long$Date)))),
                                       selectInput("cases_end_date",
                                                   "End Date",
                                                   c("All",
                                                     unique(as.character(new_cases_long$Date))))),
                                     mainPanel(
                                       plotlyOutput("rhody_confirmed_cases"),
                                       fluidRow(
                                         column(4,
                                                selectInput("cases_table_start_date",
                                                            "Start Date:",
                                                            c(unique(as.character(new_cases_long$Date))))),
                                         column(4,
                                                selectInput("cases_table_end_date",
                                                            "End Date:",
                                                            c("All",
                                                              unique(as.character(new_cases_long$Date)))))),
                                       DT::dataTableOutput("rhody_confirmed_cases_table"))))),
                        tabPanel("Age",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons("age_plot_type", "Data Type:",
                                                    c("Aggregate", "Percentage")),
                                       selectInput("age_start_date",
                                                   "Start Date:",
                                                   unique(as.character(age_data$Date)),
                                                   selected = as.character(max(age_data$Date))),
                                       selectInput("age_end_date",
                                                   "End Date:",
                                                   c("All", unique(as.character(age_data$Date))))),
                                     mainPanel(
                                       plotlyOutput("Age_Plot"),
                                       fluidRow(
                                         column(4, 
                                                selectInput("age_table_start_date",
                                                            "Start Date:",
                                                            c(unique(as.character(age_data$Date))))),
                                         column(4, 
                                                selectInput("age_table_end_date",
                                                            "Date:",
                                                            c("All",
                                                              unique(as.character(age_data$Date))))),
                                         column(4,
                                                selectInput("age_age_category",
                                                            "Age Category:",
                                                            c("All", unique(age_data$Age_Category))))),
                                       DT::dataTableOutput("age_table"))))),
                        tabPanel("Gender",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons("gender_plot_type", "Data Type:",
                                                    c("Aggregate", "Percentage")),
                                       selectInput("gender_plot_start_date",
                                                   "Start Date:",
                                                   unique(as.character(gender_data$Date)),
                                                   selected = as.character(max(gender_data$Date))),
                                       selectInput("gender_plot_end_date",
                                                   "End Date:",
                                                   c("All", unique(as.character(gender_data$Date)))),
                                       selectInput("gender_plot_gender",
                                                   "Gender:",
                                                   c("All", unique(gender_data$Gender)))),
                                     mainPanel(
                                       # Plot
                                       plotlyOutput("gender_plot"),
                                       fluidRow(
                                         column(4, 
                                                selectInput("gender_table_start_date",
                                                            "Date:",
                                                            unique(as.character(gender_data$Date)))),
                                         column(4, 
                                                selectInput("gender_table_end_date",
                                                            "End Date:",
                                                            c("All", unique(as.character(gender_data$Date))))),
                                         column(4, 
                                                selectInput("gender_table_gender",
                                                            "Gender:",
                                                            c("All",
                                                              unique(as.character(gender_data$Gender)))))),
                                       DT::dataTableOutput("gender_table"))))),
                        tabPanel("Hospitalization",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons("hospital_plot_type", "Plot Type:",
                                                    c("Line Graph", "Bar Graph")),
                                       radioButtons("hospital_data_type", "Data Type:",
                                                    c("Aggregate", "Percentage")),
                                       selectInput("hospital_plot_start_date",
                                                   "Start Date:",
                                                   unique(as.character(hospital_data$Date))),
                                       selectInput("hospital_plot_end_date",
                                                   "End Date:",
                                                   c("All", 
                                                     unique(as.character(hospital_data$Date)))),
                                       selectInput("hospital_plot_category",
                                                   "Category:",
                                                   c("All",
                                                     unique(hospital_data$Category)))),
                                     mainPanel(
                                       # Plot
                                       plotlyOutput("hospital_plot"),
                                       # Table
                                       fluidRow(
                                         column(4, 
                                                selectInput("hospital_table_start_date",
                                                            "Start Date:",
                                                            unique(as.character(hospital_data$Date)))),
                                         column(4, 
                                                selectInput("hospital_table_end_date",
                                                            "End Date:",
                                                            c("All",
                                                              unique(as.character(hospital_data$Date))))),
                                         column(4,
                                                selectInput("hospital_table_category",
                                                            "Category:",
                                                            c("All", 
                                                              unique(as.character(hospital_data$Category)))))),
                                       DT::dataTableOutput("rhody_hospital_table")))))),
             navbarMenu("Information",
                        tabPanel("Information",
                                 fluidPage(
                                   includeMarkdown("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Markdowns/Information.md"))),
                        tabPanel("Town/City Information",
                                 fluidPage(
                                   includeMarkdown("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Markdowns/Town%20Information.md"))),
                        tabPanel("About",
                                 fluidPage(
                                   includeMarkdown("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Markdowns/About.md"))))))# Shiny App
shinyApp(ui, server)
