library(sf)
library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(gridExtra)
library(grid)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(gt)
library(reshape2)

# read in zip code shape file
location <- "data/cb_2018_us_zcta510_500k/cb_2018_us_zcta510_500k.shp"
tx <- st_read(location, stringsAsFactors=FALSE)

# read in list of michigan zip codes
zip_mi <- read.csv("data/mi_zip_list.csv")
# and filter the shape file information down to only michigan zip codes
tx_mi <- filter(tx, GEOID10 %in% zip_mi$Zip.Code)

# get geo information on county boundaries
county_geo <- readRDS(file = "data/county_map_bones.rds")
county_geo$NAME <- gsub(" County, Michigan", "", county_geo$NAME)

patient_data <- read.csv("data/patient_data_sample.csv")
patient_data <- patient_data %>% mutate(PATIENT_RACE = case_when(PATIENT_RACE == "W" ~ "White or Caucasian", 
                                                                 PATIENT_RACE == "B" ~ "Black or African American", 
                                                                 PATIENT_RACE == "O" ~ "Other", 
                                                                 PATIENT_RACE == "U" ~ "Unknown",                                                                  PATIENT_RACE == "A" ~ "Asian", 
                                                                 PATIENT_RACE == "M" ~ "Multiple", 
                                                                 PATIENT_RACE == "I" ~ "American Indian and Alaska Native", 
                                                                 PATIENT_RACE == "D" ~ "Patient Refused", 
                                                                 PATIENT_RACE == "P" ~ "Native Hawaiian and Other Pacific Islander", 
                                                                 T ~ PATIENT_RACE))
patient_data <- patient_data %>% mutate(specimen_collection = as_date(paste0(substr(SPECIMEN_COLLECTION_DATE, 1, 4), "-", substr(SPECIMEN_COLLECTION_DATE, 5, 6), "-", substr(SPECIMEN_COLLECTION_DATE, 7, 8))))

covid_cases_county <- read.csv("data/cases_deaths_by_county_state_of_michigan_20220420.csv")
covid_cases_county <- filter(covid_cases_county, CASE_STATUS == "Confirmed") %>% mutate(Date = substr(Date, 1, 10), 
                                                                                        week = epiweek(as_date(Date)))

covid_cases_county <- filter(covid_cases_county, as_date(Date) >= min(as_date(paste0(substr(patient_data$SPECIMEN_COLLECTION_DATE, 1, 4), "-", substr(patient_data$SPECIMEN_COLLECTION_DATE, 5, 6), "-", substr(patient_data$SPECIMEN_COLLECTION_DATE, 7, 8)))))

covid_cases_county <- covid_cases_county %>% mutate(week = case_when(nchar(as.character(week)) == 1 ~ paste0("0", week), 
                                                                     T ~ as.character(week)), 
                                                    case_week = case_when(week == "52" ~ paste0(year(as_date(Date)) - 1, "-", week), 
                                                                          T ~ paste0(year(as_date(Date)), "-", week)))

covid_cases_county2 <- covid_cases_county %>% mutate(COUNTY = case_when(COUNTY == "Detroit City" ~ "Wayne", 
                                                                        T ~ COUNTY))

covid_cases_county <- covid_cases_county %>% group_by(COUNTY, case_week) %>% summarize(confirmed_cases = sum(Cases, na.rm = TRUE)) 

covid_cases_2S <- filter(covid_cases_county, COUNTY %in% c("Wayne", "Washtenaw", "Detroit City", "Monroe")) %>% mutate(COUNTY = "Region 2S") %>% group_by(COUNTY, case_week) %>% summarize(confirmed_cases = sum(confirmed_cases, na.rm = TRUE))

covid_cases_state <- covid_cases_county %>% mutate(COUNTY = "State") %>% group_by(COUNTY, case_week) %>% summarize(confirmed_cases = sum(confirmed_cases, na.rm = TRUE))

covid_cases_all <- rbind(covid_cases_county, covid_cases_2S, covid_cases_state)

region_crosswalk <- read.csv("data/michigan_merc_region_county_crosswalk_populations_byCounty_withDetCity.csv")


# Define UI for application that draws a histogram
ui <- shinyUI(
        fluidPage(
            navbarPage(HTML("MI-SAPPHIRE"), 
                       navbarMenu("Charts & Tables",
                                  tabPanel("Strains",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("SARS-CoV-2 Samples")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="sc2_strains",
                                                            width = 3, 
                                                            radioButtons("presentation_prop", h6(strong("Presentation:")), choices = c("Count", "Percent"), selected = "Count"), 
                                                            radioButtons("presentation_cov", h6(strong("Coverage Line:")), choices = c("On", "Off"), selected = "Off"), 
                                                            h6("Note: Coverage Line is only available on Count presentation choice for Overall & 2S Region")
                                                            
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, plotOutput(outputId = "overall_strain_plot"))),
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                                   fluidRow(column(12, plotOutput(outputId = "region_strain_plot"))), 
                                                   
                                                   fluidRow(column(12, plotOutput(outputId = "county_strain_plot", height = 2000))), 
                                                   fluidRow(column(12, plotOutput(outputId = "county_strain_plot2", height = 700)))
                                                   
                                               )
                                           )
                                  ),
                                  tabPanel("Coverage by County",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("County Coverage - Sequences vs. Confirmed Case Totals")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="county_cov",
                                                            width = 3, 
                                                            h6("Coverage is calculated as the number of samples available & sequenced from a county,
                                                            divided by the total cases in the county, and multiplied by 100.")
                                                            
                                                            #radioButtons("map_1_1_region_add", h6(strong("Region Highlight:")), choices = c("Off", "1", "2N", "2S", "3", "5", "6", "7", "8"), selected = "Off")
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, plotOutput("county_coverage_table")))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  ), 
                                  tabPanel("Strains by Demographics",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("SARS-CoV-2 Samples")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="strains_demos_side",
                                                            width = 3, 
                                                            radioButtons("demo_presentation_prop", h6(strong("Presentation:")), choices = c("Count", "Percent"), selected = "Count"), 
                                                            radioButtons("demo_type_choice", h6(strong("Demographic Breakout:")), choices = c("Age", "Sex", "Race"), selected = "Age")
                                                            #radioButtons("map_1_1_region_add", h6(strong("Region Highlight:")), choices = c("Off", "1", "2N", "2S", "3", "5", "6", "7", "8"), selected = "Off")
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, plotOutput("overall_demo_strain_plot")))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  ), 
                                  tabPanel("Strains by Demographics - Cross Reference",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("SARS-CoV-2 Samples")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="cr_strains_demo_side",
                                                            width = 3, 
                                                            dateRangeInput("cr_strains_demo_date", h6(strong("Date Range:")), 
                                                                           start = as_date(min(patient_data$specimen_collection)), 
                                                                           end = as_date(max(patient_data$specimen_collection))), 
                                                            selectInput("cr_strains_demo_county", h6(strong("County Selection:")), 
                                                                        choices = c("All", "Jackson", "Lenawee", "Livingston", 
                                                                                    "Monroe", "Oakland", "Washtenaw", "Wayne", "Other Michigan", 
                                                                                    "Other Non-Michigan")), 
                                                            checkboxGroupInput("cr_strains_demo_cross", 
                                                                               h3("Select Demographic Grouping:"), 
                                                                               choices = c("Age", "Sex", "Race"),
                                                                               selected = c("Age", "Sex", "Race")),
                                                            h6("Cells are highlighted in pink if the percentage value is > 50%")
                                                            #radioButtons("map_1_1_region_add", h6(strong("Region Highlight:")), choices = c("Off", "1", "2N", "2S", "3", "5", "6", "7", "8"), selected = "Off")
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, gt_output("cr_strains_demo_table")))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  )
                       ),
                       navbarMenu("Maps", 
                                  tabPanel("ZIP Code Level",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("Zip Code Level Geography")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="zip_map",
                                                            width = 3, 
                                                            h6("Data is only available back to 01 August 2021", align = "center")
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, leafletOutput(outputId = "zip_code_map_base")))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  ), 
                                  tabPanel("County Level",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("County Level Geography")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="county_map",
                                                            width = 3,  
                                                            radioButtons("county_map_strain_choice", h6(strong("Strain Selection:")), choices = c("Alpha", "Delta", "Omicron", "Other"), selected = "Omicron"), 
                                                            dateRangeInput("county_map_strain_date", h6(strong("Date Range:")), 
                                                                           start = as_date(min(patient_data$specimen_collection)), 
                                                                           end = as_date(max(patient_data$specimen_collection))), 
                                                            radioButtons("county_map_pres_choice", h6(strong("Presentation:")), 
                                                                         choices = c("Count", "Percent"), 
                                                                         selected = "Count")
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, leafletOutput(outputId = "county_map_base", height = 600)))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  )
                                  
                       )
                       
                       
                       
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    overall_strain_coverage_data <- reactive({
        pd <- patient_data %>% group_by(collection_week) %>% summarize(count = length(unique(PATIENT_MRN)))
        pd <- as.data.frame(pd)
        pd <- merge(pd, filter(covid_cases_all, COUNTY == "State"), by.x = c("collection_week"), by.y = c("case_week"), all = TRUE)
        #pd <- pd %>% mutate(coverage = round((count / confirmed_cases)*100, 1))
        pd <- pd %>% mutate(coverage = case_when(is.na(round((count / confirmed_cases)*100, 1)) ~ 0, 
                                                 T ~ round((count / confirmed_cases)*100, 1)))
    })
    
    overall_strain_plot_data <- reactive({
        
        if (input$presentation_prop == "Count"){
            patient_data_grouped <- patient_data %>% group_by(collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
            patient_data_grouped <- as.data.frame(patient_data_grouped)
            
        } else if (input$presentation_prop == "Percent"){
            patient_data_grouped <- patient_data %>% group_by(collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
            patient_data_grouped2 <- patient_data %>% group_by(collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("collection_week")) %>% mutate(count = round((count2/total)*100, 1))
        } else {
            x <- 0
        }
    })
    
    
    
    region_strain_coverage_data <- reactive({
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        patient_data_region <- filter(patient_data, tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name))
        
        pd <- patient_data_region %>% group_by(collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
        pd <- as.data.frame(pd)
        pd <- merge(pd, filter(covid_cases_all, COUNTY == "Region 2S"), by.x = c("collection_week"), by.y = c("case_week"), all = TRUE)
        pd <- pd %>% mutate(coverage = case_when(is.na(round((count / confirmed_cases)*100, 1)) ~ 0, 
                                                 T ~ round((count / confirmed_cases)*100, 1)))
    })
    
    region_strain_plot_data <- reactive({
        
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        patient_data_region <- filter(patient_data, tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name))
        
        if (input$presentation_prop == "Count"){
            patient_data_grouped <- patient_data_region %>% group_by(collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
        } else if (input$presentation_prop == "Percent"){
            patient_data_grouped <- patient_data_region %>% group_by(collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
            patient_data_grouped2 <- patient_data_region %>% group_by(collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("collection_week")) %>% mutate(count = round((count2/total)*100, 1))
        } else {
            x <- 0
        }
    })
    
    county_strain_plot_data <- reactive({
        
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        top_counties <- patient_data %>% group_by(PATIENT_COUNTY) %>% summarize(person_count = length(unique(PATIENT_MRN)))
        top_counties <- filter(top_counties, person_count > 80)
        patient_data_county <- patient_data %>% mutate(county_group = case_when(tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(top_counties$PATIENT_COUNTY) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(region_crosswalk$County.Name) ~ "Other MI Counties",
                                                                                T ~ "Non MI Counties"))
        
        if (input$presentation_prop == "Count"){
            patient_data_grouped <- patient_data_county %>% group_by(county_group, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
        } else if (input$presentation_prop == "Percent"){
            patient_data_grouped <- patient_data_county %>% group_by(county_group, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
            patient_data_grouped2 <- patient_data_county %>% group_by(county_group, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("county_group", "collection_week")) %>% mutate(count = round((count2/total)*100, 1))
        } else {
            x <- 0
        }
    })
    
    output$overall_strain_plot <- renderPlot({
       
        ospd <- overall_strain_plot_data()
        cov_overall <- overall_strain_coverage_data()
        
        #print(tail(cov_overall, n = 20))
        
        if (input$presentation_prop == "Count"){
            if (input$presentation_cov == "On"){
                ggplot(ospd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                    geom_line(data = cov_overall, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
                    labs(title = "Overall", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade", 
                         caption = paste0("Max Week Coverage = ", max(cov_overall$coverage), "%")) + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            } else {
                ggplot(ospd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                    #geom_line(data = cov_overall, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
                    labs(title = "Overall", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade") + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
        } else {
            ggplot(ospd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                geom_bar(stat = "identity") + 
                #geom_line(data = ospd, aes(x = case_week, y = coverage)) + 
                theme_bw() + 
                scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
                labs(title = "Overall", 
                     x = "Sample Collection Week", 
                     y = input$presentation_prop, 
                     fill = "SARS-CoV-2 Clade") + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }
        
    })
    
    
    output$region_strain_plot <- renderPlot({
        
        rspd <- region_strain_plot_data()
        cov_region <- region_strain_coverage_data()
        if (input$presentation_prop == "Count"){
            if (input$presentation_cov == "On"){
                ggplot(rspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                    geom_line(data = cov_region, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
                    labs(title = "Public Health Region 2S", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade", 
                         caption = paste0("Max Week Coverage = ", max(cov_region$coverage), "%\nRegion placement based on residential address")) + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            } else {
                ggplot(rspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                    #geom_line(data = cov_region, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) +
                    labs(title = "Public Health Region 2S", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade", 
                         caption = "Region placement based on residential address") + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
        } else {
            ggplot(rspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                geom_bar(stat = "identity") + 
                theme_bw() + 
                scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) +
                labs(title = "Public Health Region 2S", 
                     x = "Sample Collection Week", 
                     y = input$presentation_prop, 
                     fill = "SARS-CoV-2 Clade", 
                     caption = "Region placement based on residential address") + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }
        
    })
    
    output$county_strain_plot <- renderPlot({
        
        cspd <- county_strain_plot_data()
        cspd <- filter(cspd, !county_group %in% c("Other MI Counties", "Non MI Counties"))
        #cspd$county_group <- factor(cspd$county_group, levels = c("Monroe", "Washtenaw", "Wayne",
        #                                                          "Other MI Counties", "Non MI Counties"))
        ggplot(cspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
            geom_bar(stat = "identity") + 
            theme_bw() + 
            scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
            labs(title = "County Groupings",
                 subtitle = "Counties with 80+ Samples",
                 x = "Sample Collection Week", 
                 y = input$presentation_prop, 
                 fill = "SARS-CoV-2 Clade", 
                 caption = "County placement based on residential address") + 
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            facet_wrap(.~county_group, ncol = 1)
        
    })
    
    output$county_strain_plot2 <- renderPlot({
        
        cspd2 <- county_strain_plot_data()
        cspd2 <- filter(cspd2, county_group %in% c("Other MI Counties", "Non MI Counties"))
        #cspd$county_group <- factor(cspd$county_group, levels = c("Monroe", "Washtenaw", "Wayne",
        #                                                          "Other MI Counties", "Non MI Counties"))
        ggplot(cspd2, aes(x = collection_week, y = count, fill = extra_clade)) + 
            geom_bar(stat = "identity") + 
            theme_bw() + 
            scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
            labs(title = "County Data Categories",
                 subtitle = "Fewer than 80 Samples Available per County",
                 x = "Sample Collection Week", 
                 y = input$presentation_prop, 
                 fill = "SARS-CoV-2 Clade", 
                 caption = "County placement based on residential address") + 
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            facet_wrap(.~county_group, ncol = 1)
        
    })
    
    
    output$county_coverage_table <- renderPlot({
        
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        top_counties <- patient_data %>% group_by(PATIENT_COUNTY) %>% summarize(person_count = length(unique(PATIENT_MRN)))
        top_counties <- filter(top_counties, person_count > 80)
        
        patient_data_county <- patient_data %>% mutate(county_group = case_when(tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(top_counties$PATIENT_COUNTY) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(region_crosswalk$County.Name) ~ "Other MI Counties",
                                                                                T ~ "Non MI Counties"))
        
        patient_data_grouped <- patient_data_county %>% group_by(county_group, collection_week) %>% summarize(count = length(unique(PATIENT_MRN)))
        
        county_case_data <- filter(covid_cases_all, tolower(COUNTY) %in% tolower(unique(patient_data_grouped$county_group)))
        
        #county_case_data$COUNTY <- toupper(county_case_data$COUNTY)
        pd <- merge(patient_data_grouped, county_case_data, by.x = c("collection_week", "county_group"), by.y = c("case_week", "COUNTY"), all = TRUE)
        
        pd <- filter(pd, county_group != "Non MI Counties" & county_group != "Other MI Counties")
        
        pd <- pd %>% mutate(coverage = case_when(is.na(round((count / confirmed_cases)*100, 1)) ~ 0, 
                                                 T ~ round((count / confirmed_cases)*100, 1)))
        
        ggplot(pd, aes(x = collection_week, y = coverage, group = county_group, color = county_group)) + 
            geom_line(size = 2, alpha = 0.7) + 
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            labs(x = "Sample Collection Week", 
                 y = "Cases Covered with Genetic Sample (%)", 
                 color = "County")
        
    })
    
    
    overall_strain_by_demos_plot_data <- reactive({
        
        if (input$demo_presentation_prop == "Count"){
            if (input$demo_type_choice == "Age"){
                patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
                patient_data_grouped <- patient_data %>% group_by(approx_age_group, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
               
                names(patient_data_grouped)[names(patient_data_grouped) == 'approx_age_group'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else if (input$demo_type_choice == "Sex"){
                patient_data_grouped <- patient_data %>% group_by(PATIENT_SEX, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
                
                names(patient_data_grouped)[names(patient_data_grouped) == 'PATIENT_SEX'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else {
                # choice is Race
                patient_data_grouped <- patient_data %>% group_by(PATIENT_RACE, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
                names(patient_data_grouped)[names(patient_data_grouped) == 'PATIENT_RACE'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            }
            
        } else if (input$demo_presentation_prop == "Percent"){
            if (input$demo_type_choice == "Age"){
                patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
                patient_data_grouped <- patient_data %>% group_by(approx_age_group, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
                patient_data_grouped2 <- patient_data %>% group_by(approx_age_group, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
                patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("approx_age_group", "collection_week")) %>% mutate(count = round((count2/total)*100, 1))
                names(patient_data_grouped)[names(patient_data_grouped) == 'approx_age_group'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else if (input$demo_type_choice == "Sex"){
                patient_data_grouped <- patient_data %>% group_by(PATIENT_SEX, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
                patient_data_grouped2 <- patient_data %>% group_by(PATIENT_SEX, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
                patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("PATIENT_SEX", "collection_week")) %>% mutate(count = round((count2/total)*100, 1))
                names(patient_data_grouped)[names(patient_data_grouped) == 'PATIENT_SEX'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else {
                patient_data_grouped <- patient_data %>% group_by(PATIENT_RACE, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
                patient_data_grouped2 <- patient_data %>% group_by(PATIENT_RACE, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
                patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("PATIENT_RACE", "collection_week")) %>% mutate(count = round((count2/total)*100, 1))
                names(patient_data_grouped)[names(patient_data_grouped) == 'PATIENT_RACE'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            }
                
        } else {
            x <- 0
        }
    })
    
    output$overall_demo_strain_plot <- renderPlot({
        
        osbdpd <- overall_strain_by_demos_plot_data() 
        #cov_overall <- overall_strain_coverage_data()
        
        #print(tail(cov_overall, n = 20))
        
        if (input$presentation_prop == "Count"){
                ggplot(osbdpd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                    #geom_line(data = cov_overall, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) +
                    labs(title = paste0("Overall - ", input$demo_presentation_prop),
                         subtitle = paste0("Grouped by ", input$demo_type_choice),
                         x = "Sample Collection Week", 
                         y = input$demo_presentation_prop, 
                         fill = "SARS-CoV-2 Clade") + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                facet_wrap(.~demo_group)
        } else {
            ggplot(osbdpd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                geom_bar(stat = "identity") + 
                #geom_line(data = ospd, aes(x = case_week, y = coverage)) + 
                theme_bw() + 
                scale_fill_manual(values = c("#466060", "#EAD7D7", "#57886C", "#BD4F6C", "#1D201F")) + 
                labs(title = paste0("Overall - ", input$demo_presentation_prop), 
                     subtitle = paste0("Grouped by ", input$demo_type_choice),
                     x = "Sample Collection Week", 
                     y = input$demo_presentation_prop, 
                     fill = "SARS-CoV-2 Clade") + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                facet_wrap(.~demo_group)
        }
        
    })
    
    
    cross_table_data <- reactive({
        if (input$cr_strains_demo_county == "All"){
            patient_data <- filter(patient_data, specimen_collection >= input$cr_strains_demo_date[1] & 
                                       specimen_collection <= input$cr_strains_demo_date[2])
        } else if (input$cr_strains_demo_county == "Other Michigan"){
            patient_data <- filter(patient_data, specimen_collection >= input$cr_strains_demo_date[1] & 
                                       specimen_collection <= input$cr_strains_demo_date[2] & PATIENT_COUNTY %in% toupper(region_crosswalk$County.Name) & 
                                       !PATIENT_COUNTY %in% c("Jackson", "Lenawee", "Livingston", 
                                                              "Monroe", "Oakland", "Washtenaw", "Wayne"))
        } else if (input$cr_strains_demo_county == "Other Non-Michigan"){
            patient_data <- filter(patient_data, specimen_collection >= input$cr_strains_demo_date[1] & 
                                       specimen_collection <= input$cr_strains_demo_date[2] & !PATIENT_COUNTY %in% toupper(region_crosswalk$County.Name))
        } else {
            patient_data <- filter(patient_data, specimen_collection >= input$cr_strains_demo_date[1] & 
                                       specimen_collection <= input$cr_strains_demo_date[2] & PATIENT_COUNTY == toupper(input$cr_strains_demo_county))
        }
        
        patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
        
        if (setequal(input$cr_strains_demo_cross, c("Age", "Sex", "Race"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group, PATIENT_SEX, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group + PATIENT_SEX + PATIENT_RACE ~ extra_clade, value.var = c("total"))
        } else if (setequal(input$cr_strains_demo_cross, c("Age", "Sex"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group, PATIENT_SEX) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group + PATIENT_SEX ~ extra_clade, value.var = c("total"))
        }  else if (setequal(input$cr_strains_demo_cross, c("Age", "Race"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group + PATIENT_RACE ~ extra_clade, value.var = c("total"))
        } else if (setequal(input$cr_strains_demo_cross, c("Sex", "Race"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, PATIENT_SEX, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, PATIENT_SEX + PATIENT_RACE ~ extra_clade, value.var = c("total"))
        } else if (setequal(input$cr_strains_demo_cross, c("Sex"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, PATIENT_SEX) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, PATIENT_SEX ~ extra_clade, value.var = c("total"))
        } else if (setequal(input$cr_strains_demo_cross, c("Race"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, PATIENT_RACE ~ extra_clade, value.var = c("total"))
        } else if (setequal(input$cr_strains_demo_cross, c("Age"))){
            patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group) %>% summarize(total = length(unique(PATIENT_MRN)))
            patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group ~ extra_clade, value.var = c("total"))
        } else {
            x <- "None"
        }
        patient_data_grouped[is.na(patient_data_grouped)] <- 0
        
        
        if("PATIENT_SEX" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$PATIENT_SEX <- ""
        }
        if("PATIENT_RACE" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$PATIENT_RACE <- ""
        }
        if("approx_age_group" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$approx_age_group <- ""
        }
        
        #print(ncol(patient_data_grouped))
        if("Alpha" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$Alpha <- 0
        }
        if("Delta" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$Delta <- 0
        }
        if("Omicron" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$Omicron <- 0
        }
        if("Other" %in% colnames(patient_data_grouped)){
            x <- 0
        } else {
            patient_data_grouped$Other <- 0
        }
        
        patient_data_grouped <- patient_data_grouped %>% mutate(total = Alpha + Delta + Omicron + Other) %>% arrange(-total)
        
        patient_data_grouped <- patient_data_grouped %>% mutate(AlphaPercent = paste0(round((Alpha / total)* 100, 1), "%"), 
                                                                DeltaPercent = paste0(round((Delta / total)* 100, 1), "%"), 
                                                                OmicronPercent = paste0(round((Omicron / total)* 100, 1), "%"),
                                                                OtherPercent = paste0(round((Other / total)* 100, 1), "%"))
        
        patient_data_grouped <- patient_data_grouped %>% select(approx_age_group, PATIENT_SEX, PATIENT_RACE, Alpha, AlphaPercent, Delta, DeltaPercent, Omicron, OmicronPercent, Other, OtherPercent, total)
        
        return(patient_data_grouped)
    })
    
    output$cr_strains_demo_table <- render_gt({
        print(input$cr_strains_demo_cross == c("Age", "Sex"))
        pdg <- cross_table_data()
        
        pdg %>% gt() %>% tab_style(
            style = list(
                cell_fill(color = "#EAD7D7")
            ),
            locations = cells_body(
                columns =  vars(`AlphaPercent`),
                rows = as.numeric(gsub("%", "", `AlphaPercent`)) > 50
            )) %>% 
            tab_style(
                style = list(
                    cell_fill(color = "#EAD7D7")
                ),
                locations = cells_body(
                    columns =  vars(`DeltaPercent`),
                    rows = as.numeric(gsub("%", "", `DeltaPercent`)) > 50
                )) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#EAD7D7")
                ),
                locations = cells_body(
                    columns =  vars(`OmicronPercent`),
                    rows = as.numeric(gsub("%", "", `OmicronPercent`)) > 50
                )) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#EAD7D7")
                ),
                locations = cells_body(
                    columns =  vars(`OtherPercent`),
                    rows = as.numeric(gsub("%", "", `OtherPercent`)) > 50
                )) %>%
            cols_label(
            total = "Total", 
            PATIENT_SEX = "Sex", 
            PATIENT_RACE = "Race", 
            approx_age_group = "Age", 
            AlphaPercent = "", 
            DeltaPercent = "", 
            OmicronPercent = "", 
            OtherPercent = ""
        ) %>% 
            tab_style(
                style = cell_text(
                    size = "small"
                ),
                locations = cells_body(
                    columns = everything(),
                    rows = everything()
                )
            ) %>%
            tab_style(
                style = cell_text(
                    size = "small"
                ),
                locations = cells_column_labels(
                    columns = everything()
                )
            )
        
    })
    
    output$zip_code_map_base <- renderLeaflet({
        leaflet() %>% addTiles() %>% addPolygons(data = tx_mi$geometry,
                                  fillColor = "#b1caf2",
                                  color = "#000000", # you need to use hex colors
                                  #label = lapply(map_2_labs, htmltools::HTML),
                                  fillOpacity = 0.7,
                                  weight = 1,
                                  smoothFactor = 0.2) 
    })
    
    
    # county_strain_map_data <- reactive({
    #     
    #     county_geo$NAME <- toupper(county_geo$NAME)
    #     county_strain_merge <- merge(county_geo, patient_data, by.x = c("NAME"), by.y = c("PATIENT_COUNTY"), all.y = TRUE)
    #     county_strain_merge <- county_strain_merge %>% group_by(geometry, extra_clade, NAME) %>% summarize(sam_count = length(unique(PATIENT_MRN)))
    # })
    
    output$county_map_base <- renderLeaflet({
        
        #print(head(county_strain_map_data()))
        patient_data_date <- filter(patient_data, specimen_collection >= input$county_map_strain_date[1] & specimen_collection <= input$county_map_strain_date[2])
        patient_data1 <- patient_data_date %>% group_by(extra_clade, PATIENT_COUNTY) %>% summarize(sam_count = length(unique(PATIENT_MRN)))
        patient_data_total <- patient_data_date %>% group_by(PATIENT_COUNTY) %>% summarize(sam_total = length(unique(PATIENT_MRN)))
        patient_data1 <- merge(patient_data1, patient_data_total, by = c("PATIENT_COUNTY"))
        patient_data1 <- patient_data1 %>% mutate(percent = round((sam_count/sam_total)*100, 1))
        patient_data2 <- filter(patient_data1, extra_clade == input$county_map_strain_choice)
        
        county_geo$NAME <- toupper(county_geo$NAME)
        #summary(county_geo)
        county_strain_merge <- merge(county_geo, patient_data2, by.x = c("NAME"), by.y = c("PATIENT_COUNTY"), all.x = TRUE)
        
        
        county_strain_merge <- st_transform(county_strain_merge, "+proj=longlat +ellps=WGS84 +datum=WGS84")
        #county_strain_merge <- as.data.frame(county_strain_merge)
        
        
        
        # map_1_labs <- lapply(seq(nrow(county_strain_merge)), function(i){
        #     paste0('<p> County: ', county_strain_merge[i, "NAME"], '</p>', 
        #            input$county_map_strain_choice, '<p> Sample Count: ', county_strain_merge[i, "sam_count"], '</p>') 
        # })
        
        covid_cases_county2_date <- filter(covid_cases_county2, Date >= input$county_map_strain_date[1] & Date <= input$county_map_strain_date[2])
        case <- covid_cases_county2 %>% group_by(COUNTY) %>% summarize(total_cases = sum(Cases, na.rm = TRUE))
        case$COUNTY <- toupper(case$COUNTY)
        
        if (input$county_map_pres_choice == "Count"){ 
            
            pall<-colorNumeric(c("#EAD7D7","#BD4F6C"), as.numeric(patient_data1$sam_count))
            
                map_1_labs <- county_strain_merge %>% select(NAME, extra_clade, sam_count)
                map_1_labs <- merge(map_1_labs, case, by.x = c("NAME"), by.y = c("COUNTY"), all.x = TRUE)
                map_1_labs <- as.data.frame(map_1_labs)
                map_1_labs <-  map_1_labs %>% mutate(X = paste0('<p> <b>County:</b> ', str_to_title(NAME), '</p>', 
                                                                '<p> <b>', input$county_map_strain_choice, ' Sample Count:</b> ', sam_count, '</p>', 
                                                                '<p> <b>Confirmed Case Count:</b> ', total_cases, '</p>'))
                map_1_labs <- as.list(map_1_labs$X)
                
                title_label <- paste0('<center><p><b>', input$county_map_strain_choice, " Counts by Michigan County</b></p></center>", 
                                      '<center><p>', input$county_map_strain_date[1], " to ", input$county_map_strain_date[2], "</p></center>")
                out_map <- leaflet() %>% addTiles() %>% addPolygons(data = county_strain_merge$geometry,
                                                                    fillColor = pall(county_strain_merge$sam_count),
                                                                    color = "#000000", # you need to use hex colors
                                                                    label = lapply(map_1_labs, htmltools::HTML),
                                                                    fillOpacity = 0.7,
                                                                    weight = 1,
                                                                    smoothFactor = 0.2) %>% 
                    addLegend("bottomright", pal = pall, values = patient_data1$sam_count,
                              title = paste0("Count of ", input$county_map_strain_choice),
                              opacity = 1) %>% 
                    addControl(title_label, position = "topright")
                
        } else if (input$county_map_pres_choice == "Percent"){
            
            pall<-colorNumeric(c("#EAD7D7","#BD4F6C"), as.numeric(patient_data1$percent))
            
                map_1_labs <- county_strain_merge %>% select(NAME, extra_clade, percent)
                map_1_labs <- merge(map_1_labs, case, by.x = c("NAME"), by.y = c("COUNTY"), all.x = TRUE)
                map_1_labs <- as.data.frame(map_1_labs)
                map_1_labs <-  map_1_labs %>% mutate(X = paste0('<p> <b>County:</b> ', str_to_title(NAME), '</p>', 
                                                                '<p> <b>', input$county_map_strain_choice, ' Sample Percent:</b> ', percent, '%</p>', 
                                                                '<p> <b>Confirmed Case Count:</b> ', total_cases, '</p>'))
                map_1_labs <- as.list(map_1_labs$X)
                
                title_label <- paste0('<center><p><b>', input$county_map_strain_choice, " Percents by Michigan County</b></p></center>", 
                                      '<center><p>', input$county_map_strain_date[1], " to ", input$county_map_strain_date[2], "</p></center>")
                out_map <- leaflet() %>% addTiles() %>% addPolygons(data = county_strain_merge$geometry,
                                                                    fillColor = pall(county_strain_merge$percent),
                                                                    color = "#000000", # you need to use hex colors
                                                                    label = lapply(map_1_labs, htmltools::HTML),
                                                                    fillOpacity = 0.7,
                                                                    weight = 1,
                                                                    smoothFactor = 0.2) %>% 
                    addLegend("bottomright", pal = pall, values = patient_data1$percent,
                              title = paste0("Percent of ", input$county_map_strain_choice),
                              opacity = 1) %>% 
                    addControl(title_label, position = "topright")
        } else {
            x = 0
        }
        
        return(out_map)

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
