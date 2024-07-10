# Load necessary packages
library(shiny)
library(shinydashboard)
#library(mailR)
library(tidyverse)
library(RColorBrewer)
library(ggsci)
library(rsconnect)
library(ggrepel)
library(plotly)
library(highcharter)
library(directlabels)

# Configure RStudio Connect account
rsconnect::setAccountInfo(
  name = 'qcmuek-thomas-alexander',
  token = '0C459DADC38DE42C7AE2B7A40C558D21',
  secret = '0N2+uwnG1hKyr8eFEchrcpPg0Cg/bUf0ZOpI6mlS'
)

# Define UI using shinydashboard
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Predicted VO2 Comparison",
                  titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Help", icon = icon("question-circle"), tabName = "help"),
      menuItem("Comments", icon = icon("comment"), tabName = "comments"),
      
      # Add your logo here
      tags$div(class = "footer-logo",
               tags$a(href = "https://www.warrelatedillness.va.gov/WARRELATEDILLNESS/AHBPCE/network.asp", target = "_blank",
                      tags$img(src = "PDCEN_Logo_White.png", height = "60px", style = "display: block; margin-left: auto; margin-right: auto;")
               )
      )
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Input Parameters", status = "primary", solidHeader = TRUE, width = 4, height = 565,
                    selectInput("gender", "Gender", c("Male", "Female")),
                    selectInput("mode", "Mode", c("Treadmill", "Bike")),
                    numericInput("weight", "Weight (kg)", value = 110),
                    numericInput("height", "Height (cm)", value = 180),
                    numericInput("age", "Age", value = 40),
                    numericInput("measuredVO2", "Measured VO2 (ml/min)", value = 3000),
                    actionButton("calculate", "Calculate")
                ),
                box(title = "Predicted VO2 Max", status = "primary", solidHeader = TRUE, width = 8,
                    tableOutput("resultsTable"),
                    plotOutput("comparisonPlot")
                )
              ),
              
              fluidRow(
                box(title = "Weight vs VO2 Graphs", status = "danger", solidHeader = TRUE, width = 12,
                    sliderInput("SliderBMI", "Select BMI", value = 25, min = 20, max = 40),
                    tabsetPanel(
                      id = 'plots',
                      tabPanel("Friend vs Weight", tableOutput("resultsTableFRIEND"), plotlyOutput("friendPlot")),
                      tabPanel("Wasserman vs Weight", tableOutput("resultsTableWasserman"), plotlyOutput("wassermanPlot")),
                      tabPanel("Hansen vs Weight", tableOutput("resultsTableHansen"), plotlyOutput("hansenPlot")),
                      tabPanel("Bruce vs Weight", tableOutput("resultsTableBruce"), plotlyOutput("BrucePlot")),
                      tabPanel("Jones 2 vs Weight", tableOutput("resultsTableJones2"), plotlyOutput("jones2Plot")),
                      tabPanel("Neder vs Weight", tableOutput("resultsTableNeder"), plotlyOutput("NederPlot")),
                      tabPanel("Equations", tags$img(src = "equations3.png", height = "800px", width = "auto"))
                    ))
              )),
      tabItem(tabName = "help",
              h2("Welcome"),
              uiOutput("text1")),
      
      
      tabItem(tabName = "comments",
              h2("Have tips to improve this site?"),
              actionButton("showModal", "Leave a Comment", class = "btn-primary"))
    )
  )
)


server <- function(input, output, session) {
  
  
  
  
  # Reactive values to store the computed dataset
  results <- reactive({
    req(input$calculate) # Ensure computation is triggered by the button
    
    
    # Create initial data frame with input values
    VO2_Predicted <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age
    )
    
    # Convert weight and height to lbs and inches for FRIEND equation
    VO2_Predicted <- VO2_Predicted %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    # Calculate the predicted VO2 max values using the specified equations
    VO2_Predicted <- VO2_Predicted %>%
      mutate(
        VO2_Predicted <- VO2_Predicted %>%
          mutate(
            VO2_peak_Friend_general = case_when(
              gender == "Male" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
              gender == "Female" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
              gender == "Male" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
              gender == "Female" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
              TRUE ~ NA_real_),
            
            
            VO2_peak_wasserman = case_when(
              gender == "Male" & Mode == "Treadmill" ~ ((weight_kg * (50.72 - (0.372 * age))) * 1.11), 
              gender == "Female" & Mode == "Treadmill" ~ (((weight_kg + 42.8) * (22.78 - (0.17 * age))) * 1.11),
              gender == "Male" & Mode == "Bike" ~ (weight_kg * (50.72 - (0.372 * age))), 
              gender == "Female" & Mode == "Bike" ~ ((weight_kg + 42.8) * (22.78 - (0.17 * age))),
              TRUE ~ NA_real_),
            
            
            VO2_peak_Hansen = case_when(
              gender == "Male" & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor),
              gender == "Male" & Mode == "Bike" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor),
              gender == "Male" & Mode == "Bike" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)),
              
              gender == "Male" & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor) * 1.11,
              gender == "Male" & Mode == "Treadmill" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor) * 1.11,
              gender == "Male" & Mode == "Treadmill" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
              
              gender == "Female" & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor),
              gender == "Female" & Mode == "Bike" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor),
              gender == "Female" & Mode == "Bike" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)),
              
              gender == "Female" & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor) * 1.11,
              gender == "Female" & Mode == "Treadmill" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor) * 1.11,
              gender == "Female" & Mode == "Treadmill" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
              TRUE ~ NA_real_),
            
            
            VO2_peak_Bruce = case_when(
              gender == "Male" & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_kg)), 
              gender == "Female" & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_kg)),
              gender == "Male" & Mode == "Bike" ~ (((60 - (0.55* age)) * (weight_kg)) * 0.89), 
              gender == "Female" & Mode == "Bike" ~ (((48 - (0.37 * age)) * (weight_kg)) * 0.89),
              TRUE ~ NA_real_),
            
            VO2_peak_Jones2 = case_when(
              gender == "Male" & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000, 
              gender == "Female" & Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000,
              gender == "Male" & Mode == "Treadmill" ~ (((-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000) * 1.11), 
              gender == "Female" & Mode == "Treadmill" ~ (((-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000) * 1.11),
              TRUE ~ NA_real_),
            
            VO2_peak_Neder = case_when(
              gender == "Male" & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125), 
              gender == "Female" & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60),
              gender == "Male" & Mode == "Treadmill" ~ ((((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125)) * 1.11), 
              gender == "Female" & Mode == "Treadmill" ~ ((((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60)) * 1.11),
              TRUE ~ NA_real_)
          ))
      
    
    VO2_Predicted <- VO2_Predicted %>% 
      mutate(
        Friend_pp = (input$measuredVO2/VO2_peak_Friend_general) * 100,
        Wasserman_pp = (input$measuredVO2/VO2_peak_wasserman) * 100,
        Hansen_pp = (input$measuredVO2/VO2_peak_Hansen) * 100,
        Bruce_pp = (input$measuredVO2/VO2_peak_Bruce) * 100,
        Jones2_pp = (input$measuredVO2/VO2_peak_Jones2) * 100,
        Neder_pp = (input$measuredVO2/VO2_peak_Neder) * 100
      )
    
    
    return(VO2_Predicted)
  })
  
  #Output Text 1
  output$text1 <- renderUI({ 
    HTML("
            
            <p>We designed this app to better understand variations between VO2 predicted values among commonly used reference equations.</p>
            
            <p>Please enter all the required information under the Input Parameters and click 'Calculate'. <br>
            This will generate different percent predicted VO2 max values for each of the equations listed.</p>
            
            <p>The graphs displayed at the bottom are designed to illustrate the impact of weight/BMI on predicted VO2 max values. <br> 
            To visualize this relationship, we have plotted predicted VO2 max across a range of weights (from 50kg to 150kg) and at different ages for each equation used. <br> 
            Additionally, the predicted VO2 max at the <b>measured</b> weight is indicated by a <span style='color: blue;'>blue</span> dot, and the predicted VO2 max at the <b>ideal</b> weight is marked by a <span style='color: red;'>red</span>  dot.<br>  
            The dashed vertical line represents the predicted VO2 max at the selected BMI, which can be adjusted using slider.</p>
            
            <p><br>
            Thank you</p>
        ")
  })
  
  
  # Output tables for each tab.
  #includes BMI and weight/percent predicted for each BMI
  
  output$resultsTable <- renderTable({
    req(results())
    results() %>%
      select(
        VO2_peak_Friend_general,
        VO2_peak_wasserman,
        VO2_peak_Hansen,
        VO2_peak_Bruce,
        VO2_peak_Jones2,
        VO2_peak_Neder
      ) %>% 
      rename(
        "FRIEND Predicted" = VO2_peak_Friend_general,
        "Wasserman Predicted" = VO2_peak_wasserman,
        "Hansen Predicted" = VO2_peak_Hansen,
        "Bruce Predicted" = VO2_peak_Bruce,
        "Jones 2 Predicted" = VO2_peak_Jones2,
        "Neder Predicted" = VO2_peak_Neder
      )
  })
  
  output$resultsTableFRIEND <- renderTable({
    
    # Create initial data frame with input values
    VO2_Predicted_tableFRIEND <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      height_cm = input$height,
      age = input$age
    )
    
    VO2_Predicted_tableFRIEND <- VO2_Predicted_tableFRIEND %>%
      mutate(weight_kg = (input$SliderBMI * ((height_cm/100) * (height_cm/100))))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    VO2_Predicted_tableFRIEND <- VO2_Predicted_tableFRIEND %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    # Calculate the predicted VO2 max values using the specified equations
    # no correction needed 
    VO2_Predicted_tableFRIEND <- VO2_Predicted_tableFRIEND %>%
      mutate(
        VO2_peak_Friend_general = case_when(
          gender == "Male" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
          gender == "Female" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
          gender == "Male" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
          gender == "Female" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
          TRUE ~ NA_real_))
    
    VO2_Predicted_tableFRIEND <- VO2_Predicted_tableFRIEND %>%
      mutate(
        BMI = input$SliderBMI,
        "Percent Predicted" = (input$measuredVO2/VO2_peak_Friend_general) * 100
      )
    
    VO2_Predicted_tableFRIEND <- VO2_Predicted_tableFRIEND %>%
      select(
        BMI,
        VO2_peak_Friend_general,
        "Percent Predicted") %>% 
      rename(
        "VO2 Predicted" = VO2_peak_Friend_general
      )
  })
  
  output$resultsTableWasserman <- renderTable({
    
    # Create initial data frame with input values
    VO2_Predicted_tableWasserman <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      height_cm = input$height,
      age = input$age
    )
    
    VO2_Predicted_tableWasserman <- VO2_Predicted_tableWasserman %>%
      mutate(weight_kg = (input$SliderBMI * ((height_cm/100) * (height_cm/100))))
    
    
    
    # Calculate the predicted VO2 max values using the specified equations
    # needs to be corrected for Mode (* 1.11)
    VO2_Predicted_tableWasserman <- VO2_Predicted_tableWasserman %>%
      mutate(
        VO2_peak_wasserman = case_when(
          gender == "Male" & Mode == "Treadmill" ~ ((weight_kg * (50.72 - (0.372 * age))) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ ((weight_kg + 42.8) * (22.78 - (0.17 * age)) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (weight_kg * (50.72 - (0.372 * age))), 
          gender == "Female" & Mode == "Bike" ~ (weight_kg + 42.8) * (22.78 - (0.17 * age)),
          TRUE ~ NA_real_))
    
    VO2_Predicted_tableWasserman <- VO2_Predicted_tableWasserman %>%
      mutate(
        BMI = input$SliderBMI,
        "Percent Predicted" = (input$measuredVO2/VO2_peak_wasserman) * 100
      )
    
    VO2_Predicted_tableWasserman <- VO2_Predicted_tableWasserman %>%
      select(
        BMI,
        VO2_peak_wasserman,
        "Percent Predicted") %>% 
      rename(
        "VO2 Predicted" = VO2_peak_wasserman
      )
  })
  
  output$resultsTableHansen <- renderTable({
    
    # Create initial data frame with input values
    VO2_Predicted_tableHansen <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      height_cm = input$height,
      age = input$age
    )
    
    VO2_Predicted_tableHansen <- VO2_Predicted_tableHansen %>%
      mutate(weight_kg = (input$SliderBMI * ((height_cm/100) * (height_cm/100))))
    
    VO2_Predicted_tableHansen <- VO2_Predicted_tableHansen %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    
    # Calculate the predicted VO2 max values using the specified equations
    VO2_Predicted_tableHansen <- VO2_Predicted_tableHansen %>%
      mutate(
        VO2_peak_Hansen = case_when(
          gender == "Male" & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor),
          gender == "Male" & Mode == "Bike" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor),
          gender == "Male" & Mode == "Bike" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)),
          
          gender == "Male" & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor) * 1.11,
          gender == "Male" & Mode == "Treadmill" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor) * 1.11,
          gender == "Male" & Mode == "Treadmill" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
          
          gender == "Female" & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor),
          gender == "Female" & Mode == "Bike" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor),
          gender == "Female" & Mode == "Bike" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)),
          
          gender == "Female" & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor) * 1.11,
          gender == "Female" & Mode == "Treadmill" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor) * 1.11,
          gender == "Female" & Mode == "Treadmill" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
          TRUE ~ NA_real_))
    
    VO2_Predicted_tableHansen <- VO2_Predicted_tableHansen %>%
      mutate(
        BMI = input$SliderBMI,
        "Percent Predicted" = (input$measuredVO2/VO2_peak_Hansen) * 100
      )
    
    VO2_Predicted_tableHansen <- VO2_Predicted_tableHansen %>%
      select(
        BMI,
        VO2_peak_Hansen,
        "Percent Predicted") %>% 
      rename(
        "VO2 Predicted" = VO2_peak_Hansen
      )
  })
  
  output$resultsTableBruce <- renderTable({
    
    # Create initial data frame with input values
    VO2_Predicted_tableBruce <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      height_cm = input$height,
      age = input$age
    )
    
    VO2_Predicted_tableBruce <- VO2_Predicted_tableBruce %>%
      mutate(weight_kg = (input$SliderBMI * ((height_cm/100) * (height_cm/100))))
    
    
    
    # Calculate the predicted VO2 max values using the specified equations
    # needs to corrected for mode (*0.89 for bike)
    VO2_Predicted_tableBruce <- VO2_Predicted_tableBruce %>%
      mutate(
        VO2_peak_Bruce = case_when(
          gender == "Male" & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_kg)), 
          gender == "Female" & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_kg)),
          gender == "Male" & Mode == "Bike" ~ (((60 - (0.55* age)) * (weight_kg)) * 0.89), 
          gender == "Female" & Mode == "Bike" ~ (((48 - (0.37 * age)) * (weight_kg)) * 0.89),
          TRUE ~ NA_real_))
    
    
    VO2_Predicted_tableBruce <- VO2_Predicted_tableBruce %>%
      mutate(
        BMI = input$SliderBMI,
        "Percent Predicted" = (input$measuredVO2/VO2_peak_Bruce) * 100
      )
    
    VO2_Predicted_tableBruce <- VO2_Predicted_tableBruce %>%
      select(
        BMI,
        VO2_peak_Bruce,
        "Percent Predicted") %>% 
      rename(
        "VO2 Predicted" = VO2_peak_Bruce
      )
  })  
  
  output$resultsTableJones2 <- renderTable({
    
    # Create initial data frame with input values
    VO2_Predicted_tableJones2 <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      height_cm = input$height,
      age = input$age
    )
    
    VO2_Predicted_tableJones2 <- VO2_Predicted_tableJones2 %>%
      mutate(weight_kg = (input$SliderBMI * ((height_cm/100) * (height_cm/100))))
    
    
    
    # Calculate the predicted VO2 max values using the specified equations
    # needs to be corrected, (* 1.11)
    VO2_Predicted_tableJones2 <- VO2_Predicted_tableJones2 %>%
      mutate(
        VO2_peak_Jones2 = case_when(
          gender == "Male" & Mode == "Treadmill" ~ (((-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ (((-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000, 
          gender == "Female" & Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000,
          TRUE ~ NA_real_))
    
    
    VO2_Predicted_tableJones2 <- VO2_Predicted_tableJones2 %>%
      mutate(
        BMI = input$SliderBMI,
        "Percent Predicted" = (input$measuredVO2/VO2_peak_Jones2) * 100
      )
    
    VO2_Predicted_tableJones2 <- VO2_Predicted_tableJones2 %>%
      select(
        BMI,
        VO2_peak_Jones2,
        "Percent Predicted") %>% 
      rename(
        "VO2 Predicted" = VO2_peak_Jones2
      )
  })  
  
  output$resultsTableNeder <- renderTable({
    
    # Create initial data frame with input values
    VO2_Predicted_tableNeder <- data.frame(
      gender = input$gender,
      Mode = input$mode,
      height_cm = input$height,
      age = input$age
    )
    
    VO2_Predicted_tableNeder <- VO2_Predicted_tableNeder %>%
      mutate(weight_kg = (input$SliderBMI * ((height_cm/100) * (height_cm/100))))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    VO2_Predicted_tableNeder <- VO2_Predicted_tableNeder %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    # Calculate the predicted VO2 max values using the specified equations
    # need to correct for mode. (* 1.11)
    VO2_Predicted_tableNeder <- VO2_Predicted_tableNeder %>%
      mutate(
        VO2_peak_Neder= case_when(
            gender == "Male" & Mode == "Treadmill" ~ (((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125) * 1.11), 
            gender == "Female" & Mode == "Treadmill" ~ (((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60) * 1.11),
            gender == "Male" & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125), 
            gender == "Female" & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60),
            TRUE ~ NA_real_))
    
    VO2_Predicted_tableNeder <- VO2_Predicted_tableNeder %>%
      mutate(
        BMI = input$SliderBMI,
        "Percent Predicted" = (input$measuredVO2/VO2_peak_Neder) * 100
      )
    
    VO2_Predicted_tableNeder <- VO2_Predicted_tableNeder %>%
      select(
        BMI,
        VO2_peak_Neder,
        "Percent Predicted") %>% 
      rename(
        "VO2 Predicted" = VO2_peak_Neder
      )
  })
  
  # Output plots
  output$comparisonPlot <- renderPlot({
    
    
    # progress bar with Percent predicted    
    data <- results()
    
    data <- data %>% 
      select(
        Friend_pp,
        Wasserman_pp,
        Hansen_pp,
        Bruce_pp,
        Jones2_pp,
        Neder_pp
      ) %>% 
      rename(
        FRIEND = Friend_pp,
        Wasserman = Wasserman_pp,
        Hansen = Hansen_pp,
        Bruce = Bruce_pp,
        "Jones 2" = Jones2_pp,
        Neder = Neder_pp
      )
    
    data_long <- data %>% 
      pivot_longer(
        cols = c("FRIEND", "Wasserman", "Hansen", "Bruce", "Jones 2", "Neder"),
        names_to = "Equation",
        values_to = "Percent"
      )
    
    data_long$Percent <- round(data_long$Percent, digits = 0)
    
    data_long %>% 
      ggplot() +
      geom_col(aes(x = Equation, y = 100), fill = I("lightgrey"), alpha = 0.5) +
      geom_col(aes(x = Equation, y = Percent, fill = Percent)) +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 75) +
      geom_text(aes(x = Equation, y = Percent, label = paste0(Percent, "%")), vjust = -0.5, color = "black", fontface = "bold") +
      labs(title = "Percent Predicted VO2 Max",
           x = "Equation",
           y = "Percent Predicted (%)") +
      theme_minimal() +
      theme(
        legend.position = "none" ,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text =  element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()) +
      coord_flip()  # Optional: Flips the axes for a horizontal bar plot
    
    
    
    
    # plot for actual values      
    # results() %>%
    #   ggplot() +
    #   geom_point(aes(x = "Actual VO2", y = input$measuredVO2), width = 0.15, size = 5) +
    #   geom_point(aes(x = "Friend", y = VO2_peak_Friend_general), width = 0.15, size = 5) +
    #   geom_point(aes(x = "Wasserman", y = VO2_peak_wasserman), width = 0.15, size = 5) +
    #   geom_point(aes(x = "Hansen", y = VO2_peak_Hansen), width = 0.15, size = 5) +  
    #   geom_point(aes(x = "Bruce", y = VO2_peak_Bruce), width = 0.15, size = 5) + 
    #   geom_point(aes(x = "Jones 2", y = VO2_peak_Jones2), width = 0.15, size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "")  +
    #   scale_color_jco() +
    #    theme_grey() +
    #   scale_fill_jco() 
    
    
  })
  
  output$friendPlot <- renderPlotly({
    
    # Plotting code for Friend equation
    
    data <- data.frame(
      weight_assumed_kg = seq(from = 50, to = 150, by = 1),
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age,
      age_Plus5= input$age + 5,
      age_Plus10= input$age + 10,
      age_Plus15= input$age + 15,
      age_Minus5= input$age - 5,
      age_Minus10= input$age - 10,
      age_Minus15= input$age - 15,
      measured_VO2 = input$measuredVO2
    )
    
    data <- data %>%
      mutate(
        slider_weight = input$SliderBMI * ((height_cm/100) * (height_cm/100)))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    data <- data %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    
    data <- data %>% 
      pivot_longer(
        cols = c(age, age_Plus5, age_Plus10, age_Plus15, age_Minus5, age_Minus10,age_Minus15),
        names_to = "Age_Group",
        values_to = "age"
      )
    
    
    data <- data %>%
      mutate(
        VO2_peak_assumed = case_when(
          gender == "Male" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * (weight_assumed_kg * 2.20462) ) + (0.68 * height_in) - (0.46 * 2)) * weight_assumed_kg,
          gender == "Female" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * (weight_assumed_kg * 2.20462)) + (0.68 * height_in) - (0.46 * 2)) * weight_assumed_kg,
          gender == "Male" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * (weight_assumed_kg * 2.20462)) + (0.68 * height_in) - (0.46 * 1)) * weight_assumed_kg,
          gender == "Female" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * (weight_assumed_kg * 2.20462)) + (0.68 * height_in) - (0.46 * 1)) * weight_assumed_kg,
          TRUE ~ NA_real_),
        
        
        VO2_peak_ideal = case_when(
          gender == "Male" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * (2.20462  * weight_ideal)) + (0.68 * height_in) - (0.46 * 2)) * weight_ideal,
          gender == "Female" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * (2.20462  * weight_ideal)) + (0.68 * height_in) - (0.46 * 2)) * weight_ideal,
          gender == "Male" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * (2.20462  * weight_ideal)) + (0.68 * height_in) - (0.46 * 1)) * weight_ideal,
          gender == "Female" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * (2.20462  * weight_ideal)) + (0.68 * height_in) - (0.46 * 1)) * weight_ideal,
          TRUE ~ NA_real_),
        
        VO2_peak_actual = case_when(
          gender == "Male" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
          gender == "Female" & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
          gender == "Male" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
          gender == "Female" & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
          TRUE ~ NA_real_)
        
      )
    
    
    data <- data %>% 
      mutate(Age_Group = case_when(
        Age_Group == "age" ~ "Current Age",
        Age_Group == "age_Plus5" ~ "Age + 5",
        Age_Group == "age_Plus10" ~ "Age + 10",
        Age_Group == "age_Plus15" ~ "Age + 15",
        Age_Group == "age_Minus5"~ "Age - 5",
        Age_Group == "age_Minus10" ~ "Age - 10",
        Age_Group == "age_Minus15" ~ "Age - 15"))
    
    
    
    # FRIEND_Plot <- ggplot(data) +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Friend_assumed), color = "black") +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Friend_AgeP5), color = "red") +
    #   geom_text(aes(x = 150, y = VO2_peak_Friend_AgeP5, label = "Age + 5")) +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Friend_ideal,
    #                  text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Friend_ideal), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Friend_ideal * 100)))), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Friend_actual,
    #                  text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Friend_actual), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Friend_actual * 100)))), color = "blue", size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight") +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(
    #     limits = c(1000, 6000),
    #     breaks = seq(1000, 6000, by = 1000),  # Major breaks
    #     minor_breaks = seq(1500, 5500, by = 1000)
    #   ) +
    #   theme_grey() +
    #   theme(
    #     axis.text = element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12),
    #     panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50")
    #   )
    
    # Convert to Plotly and specify tooltip information
   
    FRIEND_Plot <- ggplot(data) +
      geom_line(data = filter(data, Age_Group == "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linewidth = 1.2) +
      
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_ideal, y = VO2_peak_ideal,
                     text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_ideal), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_ideal * 100)))), color = "red", size = 5) +
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_kg, y = VO2_peak_actual,
                     text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_actual), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_actual * 100)))), color = "blue", size = 5) +
      
      geom_line(data = filter(data, Age_Group != "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linetype = "dashed") +
      
      labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", color = "Age Groups") +
      geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
      scale_y_continuous(
        limits = c(1000, 6000),
        breaks = seq(1000, 6000, by = 1000),  # Major breaks
        minor_breaks = seq(1500, 5500, by = 1000)
      ) +
      theme_grey() +
      scale_color_manual(values = c(
        "Current Age" = "black", 
        "Age + 5"     = "#7AA6DCFF",
        "Age + 10"    = "#EFC000FF",
        "Age + 15"    = "#868686FF",
        "Age - 5"     = "#CD534CFF",
        "Age - 10"    = "#8F7700FF",
        "Age - 15"    = "#003C67FF")) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50"),
        legend.position= "bottom"
      )
    
    
    FRIEND_Plot <- ggplotly(FRIEND_Plot, tooltip = "text", dynamicTicks= TRUE)
    FRIEND_Plot
    
    
    
  })
  
  output$wassermanPlot <- renderPlotly({
    
    # Plotting code for wesserman equation
    
    data <- data.frame(
      weight_assumed_kg = seq(from = 50, to = 150, by = 1),
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age,
      age_Plus5= input$age + 5,
      age_Plus10= input$age + 10,
      age_Plus15= input$age + 15,
      age_Minus5= input$age - 5,
      age_Minus10= input$age - 10,
      age_Minus15= input$age - 15,
      measured_VO2 = input$measuredVO2
    )
    
    data <- data %>%
      mutate(
        slider_weight = input$SliderBMI * ((height_cm/100) * (height_cm/100)))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    data <- data %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    data <- data %>% 
      pivot_longer(
        cols = c(age, age_Plus5, age_Plus10, age_Plus15, age_Minus5, age_Minus10,age_Minus15),
        names_to = "Age_Group",
        values_to = "age"
      )
    
    # corrected for Mode *1.11 if Treadmill
    data <- data %>%
      mutate(
        VO2_peak_assumed = case_when(
          gender == "Male" & Mode == "Treadmill" ~ ((weight_assumed_kg * (50.72 - (0.372 * age))) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ ((weight_assumed_kg + 42.8) * (22.78 - (0.17 * age)) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (weight_assumed_kg * (50.72 - (0.372 * age))), 
          gender == "Female" & Mode == "Bike" ~ (weight_assumed_kg + 42.8) * (22.78 - (0.17 * age)),
          TRUE ~ NA_real_),
        
        
        VO2_peak_ideal = case_when(
          gender == "Male" & Mode == "Treadmill" ~ ((weight_ideal * (50.72 - (0.372 * age))) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ ((weight_ideal + 42.8) * (22.78 - (0.17 * age)) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (weight_ideal * (50.72 - (0.372 * age))), 
          gender == "Female"& Mode == "Bike" ~ (weight_ideal + 42.8) * (22.78 - (0.17 * age)),
          TRUE ~ NA_real_),
        
        
        VO2_peak_actual = case_when( #(ml/min)
          gender == "Male" & Mode == "Treadmill" ~ ((weight_kg * (50.72 - (0.372 * age))) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ ((weight_kg + 42.8) * (22.78 - (0.17 * age)) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (weight_kg * (50.72 - (0.372 * age))), 
          gender == "Female" & Mode == "Bike" ~ (weight_kg + 42.8) * (22.78 - (0.17 * age)),
          TRUE ~ NA_real_)
        
      )
    
    data <- data %>% 
      mutate(Age_Group = case_when(
        Age_Group == "age" ~ "Current Age",
        Age_Group == "age_Plus5" ~ "Age + 5",
        Age_Group == "age_Plus10" ~ "Age + 10",
        Age_Group == "age_Plus15" ~ "Age + 15",
        Age_Group == "age_Minus5"~ "Age - 5",
        Age_Group == "age_Minus10" ~ "Age - 10",
        Age_Group == "age_Minus15" ~ "Age - 15"))
    
    # wasserman_Plot <- ggplot(data) +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak.wasserman_assumed), color = "black") +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak.wasserman_ideal,
    #                  text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak.wasserman_ideal), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak.wasserman_ideal * 100)))), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak.wasserman_actual,
    #                  text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak.wasserman_actual), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak.wasserman_actual * 100)))), color = "blue", size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight") +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(
    #     limits = c(1000, 6000),
    #     breaks = seq(1000, 6000, by = 1000),  # Major breaks
    #     minor_breaks = seq(1500, 5500, by = 1000)
    #   ) +
    #   theme_grey() +
    #   theme(
    #     axis.text = element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12),
    #     panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50")
    #   )
    # 
    
    
    
    wasserman_Plot <-  ggplot(data) +
      geom_line(data = filter(data, Age_Group == "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linewidth = 1.2) +
      
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_ideal, y = VO2_peak_ideal,
                     text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_ideal), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_ideal * 100)))), color = "red", size = 5) +
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_kg, y = VO2_peak_actual,
                     text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_actual), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_actual * 100)))), color = "blue", size = 5) +
      
      geom_line(data = filter(data, Age_Group != "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linetype = "dashed") +
      
      labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", color = "Age Groups") +
      geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
      scale_y_continuous(
        limits = c(1000, 6000),
        breaks = seq(1000, 6000, by = 1000),  # Major breaks
        minor_breaks = seq(1500, 5500, by = 1000)
      ) +
      theme_grey() +
      scale_color_manual(values = c(
        "Current Age" = "black", 
        "Age + 5"     = "#7AA6DCFF",
        "Age + 10"    = "#EFC000FF",
        "Age + 15"    = "#868686FF",
        "Age - 5"     = "#CD534CFF",
        "Age - 10"    = "#8F7700FF",
        "Age - 15"    = "#003C67FF")) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50"),
        legend.position= "bottom"
      )
    
    
    # Convert to Plotly and specify tooltip information
    wasserman_Plot <- ggplotly(wasserman_Plot, tooltip = "text", dynamicTicks= TRUE)
    wasserman_Plot
    
    # data %>%
    #   ggplot() +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak.wasserman_assumed)) +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak.wasserman_ideal), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak.wasserman_actual ), color = "blue", size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight")  +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(limits = c(1000, 6000), breaks = seq(1000, 6000, by = 1000)) +
    #   scale_color_jco() +
    #   theme_grey() +
    #   scale_fill_jco() +
    #   theme(
    #     axis.text =  element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12))
    
  })
  
  output$hansenPlot <- renderPlotly({
    
    # Plotting code for Friend equation
    
    data <- data.frame(
      weight_assumed_kg = seq(from = 50, to = 150, by = 1),
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age,
      age_Plus5= input$age + 5,
      age_Plus10= input$age + 10,
      age_Plus15= input$age + 15,
      age_Minus5= input$age - 5,
      age_Minus10= input$age - 10,
      age_Minus15= input$age - 15,
      measured_VO2 = input$measuredVO2
    )
    
    data <- data %>%
      mutate(
        slider_weight = input$SliderBMI * ((height_cm/100) * (height_cm/100)))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    data <- data %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        )
      )
    
    data <- data %>% 
      pivot_longer(
        cols = c(age, age_Plus5, age_Plus10, age_Plus15, age_Minus5, age_Minus10,age_Minus15),
        names_to = "Age_Group",
        values_to = "age"
      )
    
    data <- data %>%
      mutate(cycle_factor = case_when(
      gender == "Male" ~ 50.72 - 0.372 * age,
      gender == "Female" ~ 22.78 - 0.17 * age,
      TRUE ~ NA_real_
    ))
    
    data <- data %>%
      mutate(
        VO2_peak_assumed = case_when(
          gender == "Male" & Mode == "Bike" & weight_assumed_kg < weight_ideal ~ (((weight_ideal + weight_assumed_kg) / 2) * cycle_factor),
          gender == "Male" & Mode == "Bike" & weight_assumed_kg == weight_ideal ~ (weight_assumed_kg * cycle_factor),
          gender == "Male" & Mode == "Bike" & weight_assumed_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_assumed_kg - weight_ideal)),
          
          gender == "Male" & Mode == "Treadmill" & weight_assumed_kg < weight_ideal ~ (((weight_ideal + weight_assumed_kg) / 2) * cycle_factor) * 1.11,
          gender == "Male" & Mode == "Treadmill" & weight_assumed_kg == weight_ideal ~ (weight_assumed_kg * cycle_factor) * 1.11,
          gender == "Male" & Mode == "Treadmill" & weight_assumed_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_assumed_kg - weight_ideal)) * 1.11,
          
          gender == "Female" & Mode == "Bike" & weight_assumed_kg < weight_ideal ~ (((weight_ideal + weight_assumed_kg + 86) / 2) * cycle_factor),
          gender == "Female" & Mode == "Bike" & weight_assumed_kg == weight_ideal ~ ((weight_assumed_kg + 43) * cycle_factor),
          gender == "Female" & Mode == "Bike" & weight_assumed_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_assumed_kg - weight_ideal)),
          
          gender == "Female" & Mode == "Treadmill" & weight_assumed_kg < weight_ideal ~ (((weight_ideal + weight_assumed_kg + 86) / 2) * cycle_factor) * 1.11,
          gender == "Female" & Mode == "Treadmill" & weight_assumed_kg == weight_ideal ~ ((weight_assumed_kg + 43) * cycle_factor) * 1.11,
          gender == "Female" & Mode == "Treadmill" & weight_assumed_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_assumed_kg - weight_ideal)) * 1.11,
          TRUE ~ NA_real_),
        
        
        VO2_peak_ideal = case_when(
          gender == "Male" & Mode == "Bike" ~ (weight_ideal * cycle_factor),
          gender == "Male" & Mode == "Treadmill" ~ (weight_ideal * cycle_factor) * 1.11,
          
          gender == "Female" & Mode == "Bike" ~ ((weight_ideal + 43) * cycle_factor),
          gender == "Female" & Mode == "Treadmill"  ~ ((weight_ideal + 43) * cycle_factor) * 1.11,
          TRUE ~ NA_real_),
        
        
        VO2_peak_actual = case_when(#(ml/min)
          gender == "Male" & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor),
          gender == "Male" & Mode == "Bike" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor),
          gender == "Male" & Mode == "Bike" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)),
          
          gender == "Male" & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor) * 1.11,
          gender == "Male" & Mode == "Treadmill" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor) * 1.11,
          gender == "Male" & Mode == "Treadmill" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
          
          gender == "Female" & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor),
          gender == "Female" & Mode == "Bike" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor),
          gender == "Female" & Mode == "Bike" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)),
          
          gender == "Female" & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor) * 1.11,
          gender == "Female" & Mode == "Treadmill" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor) * 1.11,
          gender == "Female" & Mode == "Treadmill" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
          TRUE ~ NA_real_)
      )
    
    data <- data %>% 
      mutate(Age_Group = case_when(
        Age_Group == "age" ~ "Current Age",
        Age_Group == "age_Plus5" ~ "Age + 5",
        Age_Group == "age_Plus10" ~ "Age + 10",
        Age_Group == "age_Plus15" ~ "Age + 15",
        Age_Group == "age_Minus5"~ "Age - 5",
        Age_Group == "age_Minus10" ~ "Age - 10",
        Age_Group == "age_Minus15" ~ "Age - 15"))
    
    
    
    # Hansen_Plot <- ggplot(data) +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Hansen_assumed), color = "black") +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Hansen_ideal,
    #                  text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Hansen_ideal), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Hansen_ideal * 100)))), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Hansen_actual,
    #                  text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Hansen_actual), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Hansen_actual * 100)))), color = "blue", size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight") +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(
    #     limits = c(1000, 6000),
    #     breaks = seq(1000, 6000, by = 1000),  # Major breaks
    #     minor_breaks = seq(1500, 5500, by = 1000)
    #   ) +
    #   theme_grey() +
    #   theme(
    #     axis.text = element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12),
    #     panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50")
    #   )
    
    
    Hansen_Plot <- ggplot(data) +
      geom_line(data = filter(data, Age_Group == "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linewidth = 1.2) +
      
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_ideal, y = VO2_peak_ideal,
                     text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_ideal), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_ideal * 100)))), color = "red", size = 5) +
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_kg, y = VO2_peak_actual,
                     text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_actual), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_actual * 100)))), color = "blue", size = 5) +
      
      geom_line(data = filter(data, Age_Group != "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linetype = "dashed") +
      
      labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", color = "Age Groups") +
      geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
      scale_y_continuous(
        limits = c(1000, 6000),
        breaks = seq(1000, 6000, by = 1000),  # Major breaks
        minor_breaks = seq(1500, 5500, by = 1000)
      ) +
      theme_grey() +
      scale_color_manual(values = c(
        "Current Age" = "black", 
        "Age + 5"     = "#7AA6DCFF",
        "Age + 10"    = "#EFC000FF",
        "Age + 15"    = "#868686FF",
        "Age - 5"     = "#CD534CFF",
        "Age - 10"    = "#8F7700FF",
        "Age - 15"    = "#003C67FF")) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50"),
        legend.position= "bottom"
      )
    
    
    # Convert to Plotly and specify tooltip information
    Hansen_Plot <- ggplotly(Hansen_Plot, tooltip = "text", dynamicTicks= TRUE)
    Hansen_Plot
    
    # data %>%
    #   ggplot() +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Hansen_assumed)) +
    #   geom_point(aes(x = weight_ideal,y = VO2_peak_Hansen_ideal), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg,y = VO2_peak_Hansen_actual ), color = "blue", size = 5) +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(limits = c(1000, 6000), breaks = seq(1000, 6000, by = 1000)) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight")  +
    #   scale_color_jco() +
    #   theme_grey() +
    #   scale_fill_jco() +
    #   theme(
    #     axis.text =  element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12))
    # 
    # 
    
  })
  
  output$BrucePlot <- renderPlotly({
    
    # Plotting code for Friend equation
    
    data <- data.frame(
      weight_assumed_kg = seq(from = 50, to = 150, by = 1),
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age,
      age_Plus5= input$age + 5,
      age_Plus10= input$age + 10,
      age_Plus15= input$age + 15,
      age_Minus5= input$age - 5,
      age_Minus10= input$age - 10,
      age_Minus15= input$age - 15,
      measured_VO2 = input$measuredVO2
    )
    
    data <- data %>%
      mutate(
        slider_weight = input$SliderBMI * ((height_cm/100) * (height_cm/100)))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    data <- data %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    
    data <- data %>% 
      pivot_longer(
        cols = c(age, age_Plus5, age_Plus10, age_Plus15, age_Minus5, age_Minus10,age_Minus15),
        names_to = "Age_Group",
        values_to = "age"
      )
    
    #corrected for Mode: 0.89 * for bike
    
    data <- data %>%
      mutate(
        
        
        VO2_peak_assumed = case_when(
          gender == "Male" & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_assumed_kg)), 
          gender == "Female" & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_assumed_kg)),
          gender == "Male" & Mode == "Bike" ~ (((60 - (0.55* age)) * (weight_assumed_kg)) * 0.89), 
          gender == "Female" & Mode == "Bike" ~ (((48 - (0.37 * age)) * (weight_assumed_kg)) * 0.89),
          TRUE ~ NA_real_),
        
        
        VO2_peak_ideal = case_when(
          gender == "Male" & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_ideal)), 
          gender == "Female" & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_ideal)),
          gender == "Male" & Mode == "Bike" ~ (((60 - (0.55* age)) * (weight_ideal)) * 0.89), 
          gender == "Female" & Mode == "Bike" ~ (((48 - (0.37 * age)) * (weight_ideal)) * 0.89),
          TRUE ~ NA_real_),
        
        
        VO2_peak_actual = case_when(#(ml/min)
          gender == "Male" & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_kg)), 
          gender == "Female" & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_kg)),
          gender == "Male" & Mode == "Bike" ~ (((60 - (0.55* age)) * (weight_kg)) * 0.89), 
          gender == "Female" & Mode == "Bike" ~ (((48 - (0.37 * age)) * (weight_kg)) * 0.89),
          TRUE ~ NA_real_)
      )
    
    data <- data %>% 
      mutate(Age_Group = case_when(
        Age_Group == "age" ~ "Current Age",
        Age_Group == "age_Plus5" ~ "Age + 5",
        Age_Group == "age_Plus10" ~ "Age + 10",
        Age_Group == "age_Plus15" ~ "Age + 15",
        Age_Group == "age_Minus5"~ "Age - 5",
        Age_Group == "age_Minus10" ~ "Age - 10",
        Age_Group == "age_Minus15" ~ "Age - 15"))
    
    
    # Bruce_Plot <- ggplot(data) +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Bruce_assumed), color = "black") +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Bruce_ideal,
    #                  text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Bruce_ideal), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Bruce_ideal * 100)))), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Bruce_actual,
    #                  text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Bruce_actual), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Bruce_actual * 100)))), color = "blue", size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight") +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(
    #     limits = c(1000, 6000),
    #     breaks = seq(1000, 6000, by = 1000),  # Major breaks
    #     minor_breaks = seq(1500, 5500, by = 1000)
    #   ) +
    #   theme_grey() +
    #   theme(
    #     axis.text = element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12),
    #     panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50")
    #   )
    
    
    Bruce_Plot <- ggplot(data) +
      geom_line(data = filter(data, Age_Group == "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linewidth = 1.2) +
      
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_ideal, y = VO2_peak_ideal,
                     text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_ideal), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_ideal * 100)))), color = "red", size = 5) +
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_kg, y = VO2_peak_actual,
                     text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_actual), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_actual * 100)))), color = "blue", size = 5) +
      
      geom_line(data = filter(data, Age_Group != "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linetype = "dashed") +
      
      labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", color = "Age Groups") +
      geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
      scale_y_continuous(
        limits = c(1000, 6000),
        breaks = seq(1000, 6000, by = 1000),  # Major breaks
        minor_breaks = seq(1500, 5500, by = 1000)
      ) +
      theme_grey() +
      scale_color_manual(values = c(
        "Current Age" = "black", 
        "Age + 5"     = "#7AA6DCFF",
        "Age + 10"    = "#EFC000FF",
        "Age + 15"    = "#868686FF",
        "Age - 5"     = "#CD534CFF",
        "Age - 10"    = "#8F7700FF",
        "Age - 15"    = "#003C67FF")) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50"),
        legend.position= "bottom"
      )
    
    # Convert to Plotly and specify tooltip information
    Bruce_Plot <- ggplotly(Bruce_Plot, tooltip = "text", dynamicTicks= TRUE)
    Bruce_Plot
    
    # data %>%
    #   ggplot() +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Bruce_assumed)) +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Bruce_ideal), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Bruce_actual ), color = "blue", size = 5) +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(limits = c(1000, 6000), breaks = seq(1000, 6000, by = 1000)) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight")  +
    #   scale_color_jco() +
    #   theme_grey() +
    #   scale_fill_jco() +
    #   theme(
    #     axis.text =  element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12))
    
    
    
  })
  
  output$jones2Plot <- renderPlotly({
    
    # Plotting code for Friend equation
    
    data <- data.frame(
      weight_assumed_kg = seq(from = 50, to = 150, by = 1),
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age,
      age_Plus5= input$age + 5,
      age_Plus10= input$age + 10,
      age_Plus15= input$age + 15,
      age_Minus5= input$age - 5,
      age_Minus10= input$age - 10,
      age_Minus15= input$age - 15,
      measured_VO2 = input$measuredVO2
    )
    
    data <- data %>%
      mutate(
        slider_weight = input$SliderBMI * ((height_cm/100) * (height_cm/100)))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    data <- data %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    # corrected for mode: 1.11 for tread
    
    data <- data %>% 
      pivot_longer(
        cols = c(age, age_Plus5, age_Plus10, age_Plus15, age_Minus5, age_Minus10,age_Minus15),
        names_to = "Age_Group",
        values_to = "age"
      )
    
    
    
    data <- data %>%
      mutate(
        VO2_peak_assumed = case_when(
          gender == "Male" & Mode == "Treadmill" ~ (((-3.76 + 0.034 * height_cm + 0.022 * weight_assumed_kg - 0.028 * age) * 1000) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ (((-2.26 + 0.025 * height_cm + 0.01 * weight_assumed_kg - 0.018 * age) * 1000) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_assumed_kg - 0.028 * age) * 1000, 
          gender == "Female" & Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_assumed_kg - 0.018 * age) * 1000,
          TRUE ~ NA_real_),
        
        
        VO2_peak_ideal = case_when(
          gender == "Male" & Mode == "Treadmill" ~ (((-3.76 + 0.034 * height_cm + 0.022 * weight_ideal - 0.028 * age) * 1000) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ (((-2.26 + 0.025 * height_cm + 0.01 * weight_ideal - 0.018 * age) * 1000) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_ideal - 0.028 * age) * 1000, 
          gender == "Female" & Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_ideal - 0.018 * age) * 1000,
          TRUE ~ NA_real_),
        
        
        VO2_peak_actual = case_when(#(ml/min)
          gender == "Male" & Mode == "Treadmill" ~ (((-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000) * 1.11), 
          gender == "Female"& Mode == "Treadmill" ~ (((-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000) * 1.11),
          gender == "Male" & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000, 
          gender == "Female"& Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000,
          TRUE ~ NA_real_)
      )
    
    data <- data %>% 
      mutate(Age_Group = case_when(
        Age_Group == "age" ~ "Current Age",
        Age_Group == "age_Plus5" ~ "Age + 5",
        Age_Group == "age_Plus10" ~ "Age + 10",
        Age_Group == "age_Plus15" ~ "Age + 15",
        Age_Group == "age_Minus5"~ "Age - 5",
        Age_Group == "age_Minus10" ~ "Age - 10",
        Age_Group == "age_Minus15" ~ "Age - 15"))
    
    # Jones2_Plot <- ggplot(data) +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Jones2_assumed), color = "black") +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Jones2_ideal,
    #                  text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Jones2_ideal), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Jones2_ideal * 100)))), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Jones2_actual,
    #                  text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
    #                               sprintf("%.2f", VO2_peak_Jones2_actual), "ml/min<br>Percent Predicted:",
    #                               sprintf("%.2f%%", (measured_VO2 / VO2_peak_Jones2_actual * 100)))), color = "blue", size = 5) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight") +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(
    #     limits = c(1000, 6000),
    #     breaks = seq(1000, 6000, by = 1000),  # Major breaks
    #     minor_breaks = seq(1500, 5500, by = 1000)
    #   ) +
    #   theme_grey() +
    #   theme(
    #     axis.text = element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12),
    #     panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50")
    #   )
    
    
    Jones2_Plot <- ggplot(data) +
      geom_line(data = filter(data, Age_Group == "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linewidth = 1.2) +
      
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_ideal, y = VO2_peak_ideal,
                     text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_ideal), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_ideal * 100)))), color = "red", size = 5) +
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_kg, y = VO2_peak_actual,
                     text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_actual), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_actual * 100)))), color = "blue", size = 5) +
      
      geom_line(data = filter(data, Age_Group != "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linetype = "dashed") +
      
      labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", color = "Age Groups") +
      geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
      scale_y_continuous(
        limits = c(1000, 6000),
        breaks = seq(1000, 6000, by = 1000),  # Major breaks
        minor_breaks = seq(1500, 5500, by = 1000)
      ) +
      theme_grey() +
      scale_color_manual(values = c(
        "Current Age" = "black", 
        "Age + 5"     = "#7AA6DCFF",
        "Age + 10"    = "#EFC000FF",
        "Age + 15"    = "#868686FF",
        "Age - 5"     = "#CD534CFF",
        "Age - 10"    = "#8F7700FF",
        "Age - 15"    = "#003C67FF")) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50"),
        legend.position= "bottom"
      )
    
    
    # Convert to Plotly and specify tooltip information
    Jones2_Plot <- ggplotly(Jones2_Plot, tooltip = "text", dynamicTicks= TRUE)
    Jones2_Plot
    
    
    # data %>%
    #   ggplot() +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Jones2_assumed)) +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Jones2_ideal), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Jones2_actual ), color = "blue", size = 5) +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(limits = c(1000, 6000), breaks = seq(1000, 6000, by = 1000)) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight")  +
    #   scale_color_jco() +
    #   theme_grey() +
    #   scale_fill_jco() +
    #   theme(
    #     axis.text =  element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12))
    # 
    
    
  })
  
  output$NederPlot <- renderPlotly({
    
    # Plotting code for Friend equation
    
    data <- data.frame(
      weight_assumed_kg = seq(from = 50, to = 150, by = 1),
      gender = input$gender,
      Mode = input$mode,
      weight_kg = input$weight,
      height_cm = input$height,
      age = input$age,
      age_Plus5= input$age + 5,
      age_Plus10= input$age + 10,
      age_Plus15= input$age + 15,
      age_Minus5= input$age - 5,
      age_Minus10= input$age - 10,
      age_Minus15= input$age - 15,
      measured_VO2 = input$measuredVO2
    )
    
    data <- data %>%
      mutate(
        slider_weight = input$SliderBMI * ((height_cm/100) * (height_cm/100)))
    
    # Convert weight and height to lbs and inches for FRIEND equation
    data <- data %>%
      mutate(
        weight_lbs = weight_kg * 2.20462,
        height_in = height_cm / 2.54,
        weight_ideal = case_when(
          gender == "Male" ~ 0.79 * height_cm - 60.7,
          gender == "Female" ~ 0.65 * height_cm - 42.8,
          TRUE ~ NA_real_
        ),
        cycle_factor = case_when(
          gender == "Male" ~ 50.72 - 0.372 * age,
          gender == "Female" ~ 22.78 - 0.17 * age,
          TRUE ~ NA_real_
        )
      )
    
    # corrected for mode * 1.11
    data <- data %>% 
      pivot_longer(
        cols = c(age, age_Plus5, age_Plus10, age_Plus15, age_Minus5, age_Minus10,age_Minus15),
        names_to = "Age_Group",
        values_to = "age"
      )
    
    
    
    data <- data %>%
      mutate(
        VO2_peak_assumed = case_when(
          gender == "Male" & Mode == "Treadmill" ~ (((-24.3 * age) + (10.2 * weight_assumed_kg) + (8.3 * height_cm) + 1125) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ (((-13.7 * age) + (10.2 * weight_assumed_kg) + (8.3 * height_cm) + 60) * 1.11),
          gender == "Male" & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_assumed_kg) + (8.3 * height_cm) + 1125), 
          gender == "Female" & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_assumed_kg) + (8.3 * height_cm) + 60),
          TRUE ~ NA_real_),
        
        
        VO2_peak_ideal = case_when(
          gender == "Male" & Mode == "Treadmill" ~ (((-24.3 * age) + (10.2 * weight_ideal) + (8.3 * height_cm) + 1125) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ (((-13.7 * age) + (10.2 * weight_ideal) + (8.3 * height_cm) + 60) * 1.11),
          gender == "Male" & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_ideal) + (8.3 * height_cm) + 1125), 
          gender == "Female" & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_ideal) + (8.3 * height_cm) + 60),
          TRUE ~ NA_real_),
        
        
        VO2_peak_actual = case_when(
          gender == "Male" & Mode == "Treadmill" ~ (((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125) * 1.11), 
          gender == "Female" & Mode == "Treadmill" ~ (((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60) * 1.11),
          gender == "Male" & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125), 
          gender == "Female" & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60),
          TRUE ~ NA_real_)
      )
    
    data <- data %>% 
      mutate(Age_Group = case_when(
        Age_Group == "age" ~ "Current Age",
        Age_Group == "age_Plus5" ~ "Age + 5",
        Age_Group == "age_Plus10" ~ "Age + 10",
        Age_Group == "age_Plus15" ~ "Age + 15",
        Age_Group == "age_Minus5"~ "Age - 5",
        Age_Group == "age_Minus10" ~ "Age - 10",
        Age_Group == "age_Minus15" ~ "Age - 15"))
    
    
    Neder_Plot <- ggplot(data) +
      geom_line(data = filter(data, Age_Group == "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linewidth = 1.2) +
      
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_ideal, y = VO2_peak_ideal,
                     text = paste("Ideal Weight:", sprintf("%.2f", weight_ideal), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_ideal), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_ideal * 100)))), color = "red", size = 5) +
      geom_point(data = filter(data, Age_Group == "Current Age"),
                 aes(x = weight_kg, y = VO2_peak_actual,
                     text = paste("Measured Weight:", sprintf("%.2f", weight_kg), "kg<br>VO2 Peak Predicted:",
                                  sprintf("%.2f", VO2_peak_actual), "ml/min<br>Percent Predicted:",
                                  sprintf("%.2f%%", (measured_VO2 / VO2_peak_actual * 100)))), color = "blue", size = 5) +
      
      geom_line(data = filter(data, Age_Group != "Current Age"), aes(x = weight_assumed_kg, y = VO2_peak_assumed, color = Age_Group), linetype = "dashed") +
      
      labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", color = "Age Groups") +
      geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
      scale_y_continuous(
        limits = c(1000, 6000),
        breaks = seq(1000, 6000, by = 1000),  # Major breaks
        minor_breaks = seq(1500, 5500, by = 1000)
      ) +
      theme_grey() +
      scale_color_manual(values = c(
        "Current Age" = "black", 
        "Age + 5"     = "#7AA6DCFF",
        "Age + 10"    = "#EFC000FF",
        "Age + 15"    = "#868686FF",
        "Age - 5"     = "#CD534CFF",
        "Age - 10"    = "#8F7700FF",
        "Age - 15"    = "#003C67FF")) +
      theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 12),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "gray50"),
        legend.position= "bottom"
      )
    
    
    # Convert to Plotly and specify tooltip information
    Neder_Plot <- ggplotly(Neder_Plot, tooltip = "text", dynamicTicks= TRUE)
    Neder_Plot
    
    
    # data %>%
    #   ggplot() +
    #   geom_line(aes(x = weight_assumed_kg, y = VO2_peak_Jones2_assumed)) +
    #   geom_point(aes(x = weight_ideal, y = VO2_peak_Jones2_ideal), color = "red", size = 5) +
    #   geom_point(aes(x = weight_kg, y = VO2_peak_Jones2_actual ), color = "blue", size = 5) +
    #   geom_vline(xintercept = data$slider_weight, linetype = "dashed", color = "black") +
    #   scale_y_continuous(limits = c(1000, 6000), breaks = seq(1000, 6000, by = 1000)) +
    #   labs(y = "VO2 Peak Predicted ml/min", x = "Weight (kg)", caption = "Blue Dot = VO2 @ Actual Weight | Red Dot = VO2 @ Ideal Weight")  +
    #   scale_color_jco() +
    #   theme_grey() +
    #   scale_fill_jco() +
    #   theme(
    #     axis.text =  element_text(face = "bold", size = 12),
    #     axis.title = element_text(face = "bold", size = 14),
    #     plot.caption = element_text(size = 12))
    # 
    
    
  })
  
  # observeEvent(input$showModal, {
  #   showModal(modalDialog(
  #     title = "Your Comment or Question",
  #     textInput("comment", "Enter your comment/question:"),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("submit", "Submit", class = "btn-primary")
  #     )
  #   ))
  # })
  # 
  # 
  # # Load configuration
  # config <- config::get()
  # 
  # # Use the config settings
  # email_user <- config$email_user
  # email_pass <- config$email_pass
  # 
  # observeEvent(input$submit, {
  #   removeModal()
  #   
  #   tryCatch({
  #     
  #     send.mail(from = email_user,
  #               to = "cpx.equations@gmail.com",
  #               subject = "Email from CPX App",
  #               body = paste("Comment/Question:", input$comment),
  #               smtp = list(host.name = "smtp.gmail.com", port = 587,
  #                           user.name = email_user,
  #                           passwd = email_pass, ssl = TRUE),
  #               authenticate = TRUE,
  #               send = TRUE)
  #     
  #     # Show notification on successful email send
  #     showNotification("Comment sent successfully!", type = "message")
  #   }, error = function(e) {
  #     # Handle error, provide feedback
  #     showNotification("Failed to send comment.", type = "error")
  #   })
  # })
  # 
}

shinyApp(ui = ui, server = server)


