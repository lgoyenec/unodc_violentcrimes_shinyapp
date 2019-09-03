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
library(RColorBrewer)

# Working directory
cd   = "/Users/lgoye/OneDrive/Documents/GitHub/hw1_lgoyenec"
data = readRDS(paste0(cd,"/app/master_data.rds"))

# User interface
# -------------------------------------------------------------------
ui = tagList(
    navbarPage(
        theme = shinytheme("cerulean"),
        title = "UNODC Violent Crimes",
        tabPanel("Trends",
                 sidebarPanel(
                     width = 3, 
                     radioButtons("input1",
                                  "Crime Type:",
                                  choices = unique(data$Crimename)),
                     radioButtons("input4",
                                  "Crime Type 2:",
                                  choices = unique(data$Crimename)),
                     radioButtons("input2",
                                  "Category:",
                                  choices = names(data)[1:3]),
                     radioButtons("input3",
                                  "Measure:",
                                  choices = c("Count","Rate")),
                     sliderInput("input5",
                                 "Year",
                                 min = min(data$Year),
                                 max = max(data$Year),
                                 value = max(data$Year))),
                 mainPanel(
                     fluidRow(
                         column(12,
                                h4("Gaugets")),
                         tabsetPanel(
                             tabPanel("Trends", plotlyOutput("plot1")),
                             tabPanel("Bubble", plotlyOutput("plot2")),
                             tabPanel("Lollipop", plotlyOutput("plot3"))
                         )
                     )
                 )
            ),
        tabPanel("Data explorer",
                 DT::dataTableOutput("tab")),
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
                color = I("red4"),
                type = "scatter",
                mode = "lines+markers",
                inherit = F, 
                showlegend = F
            ) %>%
            layout(
                height = 600,
                yaxis = list(title = input$input1)
            )
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
            layout(
                height = 600,
                xaxis = list(title = input$input1),
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
                plot_ly(color = I("gray80")) %>%
                add_segments(x = 0, xend =~ n, y =~ category, yend =~ category, showlegend = F) %>%
                add_markers(x =~ n, y =~ category, name = "", color = I("pink")) %>%
                layout(height = 600,
                       xaxis = list(title = input$input1),
                       yaxis = list(title = ""))
        } else {
            temp %>%
                plot_ly(color = I("gray80")) %>%
                add_segments(x = 0, xend =~ n, y =~ category, yend =~ category, showlegend = F) %>%
                add_markers(x =~ n, y =~ category, name = "", color = I("pink")) %>%
                layout(height = 600, 
                       xaxis = list(title = input$input1),
                       yaxis = list(title = ""))
        }
    })
    
    output$tab = DT::renderDataTable({
        DT::datatable(data, 
                      options = list(pageLength = 20), 
                      rownames = F)
    })
    
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
