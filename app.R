
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(ggthemes)
library(ggmap)


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel(h3('FDNY Dispatch Stats (2013 - 2018)')),
    

    # Sidebar layout 
    sidebarLayout(
        sidebarPanel(
            
            # daterange input
            dateRangeInput('dates', label = strong('Date Range'), format = 'mm/dd/yyyy',
                           start = '2013-01-01', end = '2018-12-31', 
                           min = '2013-01-01', max = '2018-12-31'),
            
            # borough input
            checkboxGroupInput('boro', label = strong('Borough'), 
                               choices = list('Manhattan' = 'MANHATTAN', 
                                              'Brooklyn' = 'BROOKLYN', 
                                              'Queens' = 'QUEENS', 
                                              'Bronx' = 'BRONX', 
                                              'Staten Island' = 'RICHMOND / STATEN ISLAND'),
                               selected = c('MANHATTAN', 'BROOKLYN', 'QUEENS')),
            
            # classification input
            checkboxGroupInput('class', label = strong('Incident Classification'), 
                               choices = unique(df$Classification_Grp),
                               selected = c('Structural Fires', 'Medical MFAs', 'NonMedical MFAs')),
            
            # if-valid input
            selectInput("onlyValid", label = strong("Only Show Valid Calls"),
                        choices = list('Yes' = 'N',
                                       'No' = 'n'),
                        selected = 'N'),
            
            # time window input
            checkboxGroupInput('timeWin', label = strong('Incident Time Window'), 
                               choices = list('Midnight - 3 AM' = 'Midnight - 3 AM',
                                              '3 AM - 6 AM' = '3 AM - 6 AM',
                                              '6 AM - 9 AM' = '6 AM - 9 AM',
                                              '9 AM - 12 PM' = '9 AM - 12 PM',
                                              '12 PM - 3 PM' = '12 PM - 3 PM',
                                              '3 PM - 6 PM' = '3 PM - 6 PM',
                                              '6 PM - 9 PM' = '6 PM - 9 PM',
                                              '9 PM - Midnight' = '9 PM - Midnight'),
                               selected = unique(df$Time_Window)),
            
            width = 3
            
            ),

        # plot charts by diff metrics
        mainPanel(
            fluidRow(
                column(6, plotOutput('byBoro')),
                column(6, plotOutput('byTimeWin'))
            ),
            
            fluidRow(
                column(6, plotOutput('byResponse')),
                column(6, plotOutput('byClass'))
            ),
            
            
            fluidRow(
                column(12, plotOutput('byMon'))
                # column(6, plotOutput('byResponse'))
            )
           
        )
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output){
    
    # reactive function
    dfSelected <- reactive({
        df %>%
            filter(Borough %in% input$boro) %>%
            filter(Date >= as.Date(input$dates)[1] & Date <= as.Date(input$dates)[2]) %>%
            filter(Classification_Grp %in% input$class) %>%
            filter(Time_Window %in% input$timeWin) %>%
            filter(If_Valid != input$onlyValid)
    })
    
    
    # by borough chart
    output$byBoro <- renderPlot({
        
        dfSelected() %>%
            ggplot(aes(x = Year)) +
            geom_bar(aes(fill = Borough), position = 'dodge') +
            labs(x = 'Year',
                 y = 'Incident Count',
                 title = 'FDNY Dispatches By Borough',
                 caption = 'Data Source: NYC Open Data') +
            scale_x_continuous(breaks = seq(year(input$dates)[1], year(input$dates)[2], by = 1)) +
            theme_classic() +
            scale_fill_gdocs() +
            theme(legend.position = 'bottom') +
            theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5, lineheight = 2),
                  plot.caption = element_text(color = 'dark blue', face = 'italic', hjust = 1),
                  axis.title.x = element_text(hjust = 1, size = 11, face = 'bold'),
                  axis.title.y = element_text(size = 11, face = 'bold'),
                  legend.position = 'bottom') +
            guides(fill = guide_legend(title = 'Borough',
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       title.theme = element_text(face = 'bold.italic', size = 10.5)))
        
    })
    
    
    # by time window chart
    output$byTimeWin <- renderPlot({
        
        dfSelected() %>%
            ggplot(aes(x = Time_Window)) +
            geom_bar(aes(fill = Borough)) +
            theme(legend.position = 'bottom') +
            labs(x = 'Time Window', 
                 y = 'Incident Count',
                 title = 'FDNY Dispatches By Time Window',
                 caption = 'Data Source: NYC Open Data') +
            scale_x_discrete(limits = c('Midnight - 3 AM', '3 AM - 6 AM', '6 AM - 9 AM', '9 AM - 12 PM', '12 PM - 3 PM', '3 PM - 6 PM', '6 PM - 9 PM', '9 PM - Midnight')) +
            theme_classic() +
            scale_fill_gdocs() +
            theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5, lineheight = 2),
                  plot.caption = element_text(color = 'dark blue', face = 'italic', hjust = 1),
                  legend.position = 'bottom',
                  axis.title.x = element_text(hjust = 1, size = 11, face = 'bold'),
                  axis.title.y.left = element_text(size = 11, face = 'bold'),
                  axis.text.x = element_text(angle = 30, hjust = 1)) +
            guides(fill = guide_legend(title = 'Borough', 
                                       title.position = 'top', 
                                       title.hjust = 0.5,
                                       title.theme = element_text(face = 'bold.italic', size = 10.5)))
        
    })
    
    
    # by month chart
    output$byMon <- renderPlot({
        
        dfSelected() %>%
            group_by(Month, Borough) %>%
            summarize(Ct = n()) %>%
            ggplot(aes(x = Month, y = Ct)) +
            # geom_col(aes(fill = Borough), position = 'dodge') +
            geom_point(aes(color = Borough)) +
            geom_line(aes(color = Borough, group = Borough), size = 1) +
            theme(legend.position = 'bottom') +
            labs(x = 'Month', 
                 y = 'Incident Count',
                 title = 'FDNY Dispatches By Month',
                 caption = 'Data Source: NYC Open Data') +
            scale_x_discrete(limits = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')) +
            expand_limits(x = 0, y = 0) +
            scale_fill_calc() +
            theme_classic() + 
            theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5, lineheight = 2),
                  plot.caption = element_text(color = 'dark blue', face = 'italic', hjust = 1),
                  legend.position = 'bottom',
                  axis.title.x = element_text(hjust = 1, size = 11, face = 'bold'),
                  axis.title.y.left = element_text(size = 11, face = 'bold'),
                  axis.text.x = element_text(angle = 30, hjust = 1)) +
            guides(color = guide_legend(title = 'Borough', 
                                       title.position = 'top', 
                                       title.hjust = 0.5,
                                       title.theme = element_text(face = 'bold.italic', size = 10.5)))
        
    })
    
    
    
    # by classification chart
    output$byClass <- renderPlot({
        
        dfSelected() %>%
            ggplot(aes(x = Year)) +
            geom_bar(aes(fill = Classification_Grp), position = 'dodge') +
            theme(legend.position = 'bottom') +
            labs(x = 'Year', 
                 y = 'Incident Count',
                 title = 'FDNY Dispatches By Classification',
                 caption = 'Data Source: NYC Open Data') +
            scale_x_continuous(breaks = seq(year(input$dates[1]), year(input$dates[2]), by = 1)) +
            theme_tufte() +
            scale_fill_ptol() +
            theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5, lineheight = 2),
                  plot.caption = element_text(color = 'dark blue', face = 'italic', hjust = 1),
                  legend.position = 'bottom',
                  axis.title.x = element_text(hjust = 1, size = 11, face = 'bold'),
                  axis.title.y.left = element_text(size = 11, face = 'bold')) +
            guides(fill = guide_legend(title = 'Classification', 
                                       title.position = 'top', 
                                       title.hjust = 0.5,
                                       title.theme = element_text(face = 'bold.italic', size = 10.5)))
        
    })
    
    
    
    # by response time chart
    output$byResponse <- renderPlot({
        
        dfSelected() %>%
            filter(strtoi(CallToOnScene_Sec) > 0) %>%
            group_by(Classification_Grp, Borough, CallToOnScene_Sec) %>%
            summarize(AvgResponse = mean(strtoi(CallToOnScene_Sec))) %>%
            ggplot(aes(x = Classification_Grp, y = AvgResponse)) +
            geom_col(aes(fill = Borough), position = 'dodge') +
            theme(legend.position = 'bottom') +
            labs(x = 'Classification',
                 y = 'Average Response Time (Sec)',
                 title = 'Response Time By Classification',
                 caption = 'Data Source: NYC Open Data') +
            theme_tufte() +
            scale_fill_gdocs() +
            theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5, lineheight = 2),
                  plot.caption = element_text(color = 'dark blue', face = 'italic', hjust = 1),
                  legend.position = 'bottom',
                  axis.title.x = element_text(hjust = 1, size = 11, face = 'bold'),
                  axis.title.y.left = element_text(size = 11, face = 'bold'),
                  axis.text.x = element_text(angle = 30, hjust = 1)) +
            guides(fill = guide_legend(title = 'Borough', 
                                       title.position = 'top', 
                                       title.hjust = 0.5,
                                       title.theme = element_text(face = 'bold.italic', size = 10.5)))
        
    })
    
    
    # # show valid calls only chart
    # output$byValid <- renderPlot({
    #     
    #     df %>%
    #         filter(Borough == input$boro) %>%
    #         filter(Date >= as.Date(input$dates)[1] & Date <= as.Date(input$dates)[2]) %>%
    #         filter(Classification_Grp == input$class) %>%
    #         filter(Time_Window == input$timeWin) %>%
    #         filter(If_Valid != input$onlyValid) %>%
    #         group_by(Year) %>%
    #         ggplot(aes(x = Year)) +
    #         geom_bar(aes(fill = If_Valid)) +
    #         theme(legend.position = 'bottom') +
    #         labs(x = 'Year', 
    #              y = 'Incident Count',
    #              title = 'FDNY Dispatches By Valid Calls',
    #              caption = 'Data Source: NYC Open Data') +
    #         scale_x_continuous(breaks = seq(year(input$dates[1]), year(input$dates[2]), by = 1)) +
    #         theme_classic() + 
    #         theme(plot.title = element_text(size = 15, face = 'bold', hjust = 0.5, lineheight = 2),
    #               plot.caption = element_text(color = 'dark blue', face = 'italic', hjust = 1),
    #               legend.position = 'bottom',
    #               axis.title.x = element_text(hjust = 1, size = 11, face = 'bold'),
    #               axis.title.y.left = element_text(size = 11, face = 'bold')) +
    #         guides(fill = guide_legend(title = 'Valid Call', 
    #                                    title.position = 'top', 
    #                                    title.hjust = 0.5,
    #                                    title.theme = element_text(face = 'bold.italic', size = 10.5)))
    #     
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
