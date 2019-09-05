# Laura Goyeneche
# August 31, 2018
# H1 R Shiny - data cleaning
# -------------------------------------------------------------------

# Libraries
library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Working directory
cd   = "/Users/lgoye/OneDrive/Documents/GitHub/hw1_lgoyenec"
data = readRDS(paste0(cd,"/app/master_data.rds"))

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
                          "In Tab 2 corresponds to x-axis", 
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
                     fluidRow(
                         column(12,
                                h4("Gaugets")),
                         tabsetPanel(
                             tabPanel("Tab 1", plotlyOutput("plot1")),
                             tabPanel("Tab 2", plotlyOutput("plot2")),
                             tabPanel("Tab 3", plotlyOutput("plot3"))
                         )
                     )
                 )
            ),
        tabPanel("Data explorer",
                 h4(strong("Crimes and rate per 1.000.000 inhabitants")),
                 hr(),
                 DT::dataTableOutput("tab"),
                 hr(),
                 h5(strong("Download data for selected variable:")),
                 downloadButton('downloadData',"Download data")),
        tabPanel("More")
    )
)

# Server function
# -------------------------------------------------------------------
server = function(input, output) {
    
    output$plot1 = renderPlotly({
        temp = 
            data %>%
            filter(Crimename == input$input1) %>% 
            group_by(Year) %>%
            summarise(n = sum(!!sym(input$input3)))
        
        data %>% 
            filter(Crimename == input$input1) %>% 
            mutate(!!sym(input$input2) := factor(!!sym(input$input2), ordered = T)) %>%
            group_by(Year,!!sym(input$input2)) %>%
            summarise(n = sum(!!sym(input$input3))) %>%
            top_n(3, n) %>%
            plot_ly(
                ., 
                type = "bar",
                x =~ Year,
                y =~ n, 
                color = .[[input$input2]], 
                colors = rev(brewer.pal(length(unique(.[[input$input2]])),"Blues"))) %>%
            layout(barmode = "stack") %>% 
            add_trace(
                data = temp,
                x =~ Year, 
                y =~ n,
                color = I("#FF8C42"),
                type = "scatter",
                size = I(2),
                mode = "lines+markers",
                inherit = F, 
                showlegend = T,
                name = HTML("Annual total of crimes/total rate <br>for all regions/subregions/countries <br>")
            ) %>%
            layout(yaxis = list(title = input$input1),
                   legend = list(font = list(size = 11)))
    })
    
    output$plot2 = renderPlotly({
        data %>%
            spread(Crimename,!!sym(input$input3)) %>%
            mutate(!!sym(input$input2) := factor(!!sym(input$input2), ordered = T)) %>%
            group_by(Year,!!sym(input$input2)) %>%
            summarise(n1 = sum(!!sym(input$input1), na.rm = T), n2 = sum(!!sym(input$input4), na.rm = T)) %>%
            mutate(n3 = n1 + n2) %>%
            plot_ly(
                x =~ n1, 
                y =~ n2,
                type = "scatter",
                mode = "markers",
                color = .[[input$input2]],
                marker = list(size =~ n3/quantile(n3,probs = 0.25), sizemode = "diameter")
            ) %>%
            layout(xaxis = list(title = input$input1),
                   yaxis = list(title = input$input4)
            )
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
                plot_ly(color = I("#1B4965"), size = I(8)) %>%
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
        data = 
            data %>%
            filter(Crimename == input$input1)
            
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
