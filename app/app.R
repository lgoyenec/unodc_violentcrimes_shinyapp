# Laura Goyeneche
# August 31, 2018
# H1 R Shiny - data cleaning
# -------------------------------------------------------------------

# Libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Working directory
cd   = "/Users/lgoye/OneDrive/Documents/GitHub/hw1_lgoyenec"
data = readRDS(paste0(cd,"/app/master_data.rds"))

# Color Palette
cP = c("#0D1B2A","#1B263B","#415A77","#778DA9","#E0E1DD")
bP = rev(brewer.pal(5,"Blues"))
    
# User interface
# -------------------------------------------------------------------
ui = tagList(
    navbarPage(
        theme = shinytheme("cerulean"),
        title = strong("UNODC Violent Crimes"),
        tabPanel("Trends",
                 sidebarPanel(
                     width = 3, 
                     radioButtons("input1",
                                  "Select Crime Type:",
                                  choices = unique(data$Crimename)),
                     em(p(strong("Note:"),
                          "Apply in all tabs, including",
                          strong("Data explorer"),
                          br(),
                          "For Tab 2 corresponds to x-axis", 
                          style = "font-size:11px")),
                     br(),
                     radioButtons("input4",
                                  "Select Crime Type:",
                                  choices = unique(data$Crimename),
                                  selected = "Robbery"),
                     em(p(strong("Note:"),"Apply just for Tab 2 and corresponds to y-axis", style = "font-size:11px")),
                     br(),
                     radioButtons("input2",
                                  "Select geographical category:",
                                  choices = names(data)[1:3]),
                     em(p(strong("Note:"),"Apply in all tabs", style = "font-size:11px")),
                     br(),
                     radioButtons("input3",
                                  "Select indicator:",
                                  choices = c("Count","Rate")),
                     em(p(strong("Note:"),"Apply in all tabs", style = "font-size:11px")),
                     br(),
                     sliderInput("input5",
                                 "Year",
                                 min = min(data$Year),
                                 max = max(data$Year),
                                 value = max(data$Year),
                                 sep = ""),
                     em(p(strong("Note:"),"Apply just for Tab 3", style = "font-size:11px"))),
                 mainPanel(
                     fluidRow(useShinydashboard(),
                              tags$head(tags$style(HTML(".small-box {height: 150px}"))),
                              valueBoxOutput("vB1", width = 6),
                              valueBoxOutput("vB2", width = 6)),
                     br(),
                     fluidRow(
                         tabsetPanel(
                             tabPanel("Tab 1", 
                                      h5(strong("Annual crimes (rate per 100.000) for Top 5*"), align = "center"),
                                      plotlyOutput("plot1"), br(),
                                      p(em(strong("*Note:"),
                                           "Based on the total number of selected crime over time per"), 
                                        style = "font-size:11px")),
                             tabPanel("Tab 2", 
                                      h5(strong("Annual crimes (rate per 100.000) for Top 5"), align = "center"),
                                      plotlyOutput("plot2"), br(),
                                      p(em(strong("*Note:"),
                                           "Based on the total number of selected crimes over time"),
                                        style = "font-size:11px")),
                             tabPanel("Tab 3", 
                                      h5(strong("Frequency for geographical category"), align = "center"),
                                      plotlyOutput("plot3"))) 
                     )
                 )
            ),
        tabPanel("Data explorer",
                 h5(strong(paste("Crimes and rate per 100.000 inhabitants"))),
                 hr(),
                 DT::dataTableOutput("tab"),
                 hr(),
                 h5(strong("Download data for selected variable:")),
                 downloadButton('downloadData',"Download data")),
        tabPanel("More",
                 h4("Data source:"),
                 p("UNODC"),
                 h6("Author:"),
                 p("Laura Goyeneche"))
    )
)

# Server function
# -------------------------------------------------------------------
server = function(input, output) {
    
    output$vB1 = renderValueBox({
        # Total number (rate) of crimes for selected variable input1
        data %>%
            filter(Crimename == input$input1 & Year == input$input5) %>%
            summarise(n = formatC(sum(Count), big.mark = ",", format = "d")) %>%
            valueBox(.,
                     p(strong(paste(input$input1,
                                    "crimes in",
                                    input$input5)),
                       style = "font-size:15px"),
                     icon = icon("exclamation-circle"),
                     color = "teal")
    })

    output$vB2 = renderValueBox({
        # Rate per 100,000 individuals
        data %>%
            filter(Crimename == input$input1 & Year == input$input5) %>%
            group_by(Country) %>%
            summarise(n = sum(Count)) %>%
            arrange(-n) %>%
            top_n(1) %>%
            select(Country) %>%
            valueBox(.,
                     p(strong(
                         paste("Corresponds to the country with most",input$input1,"crimes in",input$input5)),
                       style = "font-size:15px"),
                     icon = icon("globe-americas"),
                     color = )
    })
    
    output$plot1 = renderPlotly({
        temp = 
            data %>%
            filter(Crimename == input$input1) %>% 
            group_by(Year) %>%
            summarise(n = sum(!!sym(input$input3)))
        
        if(input$input2 %in% c("Subregion","Country")) {
            NameSelect = 
                data %>%
                filter(Crimename == input$input1) %>%
                group_by(!!sym(input$input2)) %>%
                summarise(n = sum(Count)) %>%
                arrange(-n) %>%
                top_n(5) %>%
                pull(!!sym(input$input2))
            dataF = 
                data %>% 
                filter(!!sym(input$input2) %in% NameSelect)
        } else {
            dataF = data
        }
        
        dataF %>% 
            filter(Crimename == input$input1) %>% 
            mutate(!!sym(input$input2) := factor(!!sym(input$input2), ordered = T)) %>%
            group_by(Year,!!sym(input$input2)) %>%
            summarise(n = sum(!!sym(input$input3))) %>%
            plot_ly(
                ., 
                type = "bar",
                x =~ Year,
                y =~ n, 
                color = .[[input$input2]], 
                colors = bP,
                alpha = 0.7) %>%
            layout(barmode = "stack") %>% 
            add_trace(
                data = temp,
                x =~ Year, 
                y =~ n,
                color = I("#CB3650"),
                type = "scatter",
                mode = "lines+markers",
                inherit = F, 
                showlegend = T,
                name = HTML(paste("Annual total of crimes (rate) <br>for all<b>",input$input2,"<b><br>"))
            ) %>%
            layout(yaxis = list(title = input$input1, range =~ c(0, max(n)*1.7)),
                   legend = list(x = 0.01, y = 1, font = list(size = 10)))
    })
    
    output$plot2 = renderPlotly({
            
        data %>%
            filter(Year == input$input5) %>%
            spread(Crimename,!!sym(input$input3)) %>%
            mutate(!!sym(input$input2) := factor(!!sym(input$input2), ordered = T)) %>%
            group_by(!!sym(input$input2)) %>%
            summarise(n1 = sum(!!sym(input$input1), na.rm = T), n2 = sum(!!sym(input$input4), na.rm = T)) %>%
            mutate(n3 = n1 + n2) %>%
            plot_ly(
                x =~ n1, 
                y =~ n2,
                type = "scatter",
                mode = "markers",
                color = I("#4ECDC4"), 
                marker = list(size = 15, sizemode = "diameter"),
                text = .[[input$input2]],
                showlegend = F) %>%
            #add_text(textfont = list(size = 9, color = "black"),textposition = "top right") %>%
            layout(xaxis = list(title = input$input1),
                   yaxis = list(title = input$input4))
    })
    
    output$plot3 = renderPlotly({
        
        temp = 
            data %>%
            filter(Year == input$input5 & Crimename == input$input1) %>%
            group_by(Year,!!sym(input$input2)) %>%
            summarise(n = sum(Count, na.rm = T)) %>%
            arrange(n) %>%
            mutate(!!sym(input$input2) := factor(!!sym(input$input2), levels = .[[input$input2]])) %>% 
            rename(category = !!sym(input$input2))
        
        if(input$input2 %in% c("Subregion","Country")){
            temp %>%
                arrange(-n) %>%
                filter(as.numeric(row.names(temp)) <= 25) %>%
                arrange(n) %>%
                plot_ly(color = I("#1B4965"), size = I(8), height = 500) %>%
                add_segments(x = 0, xend =~ n, y =~ category, yend =~ category, showlegend = F) %>%
                add_markers(x =~ n, y =~ category, name = "", color = I("#FFC857"), size = 120) %>%
                layout(xaxis = list(title = input$input1),
                       yaxis = list(title = ""))
        } else {
            temp %>%
                plot_ly(color = I("#1B4965"), size = I(8)) %>%
                add_segments(x = 0, xend =~ n, y =~ category, yend =~ category, showlegend = F) %>%
                add_markers(x =~ n, y =~ category, name = "", color = I("#FFC857"), size = 250) %>%
                layout(xaxis = list(title = input$input1),
                       yaxis = list(title = ""))
        }
    })
    
    dataFilter = reactive({
        data = data %>% filter(Crimename == input$input1)
    })
    
    output$tab = DT::renderDataTable({
        DT::datatable(dataFilter(),
                      options = list(pageLength = 10), 
                      rownames = F)
    })
    
    output$downloadData = downloadHandler(
        filename = function() {paste("data-", Sys.Date(), ".csv", sep = "")},
        content = function(file) {write.csv(dataFilter(),file)}
    )
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------