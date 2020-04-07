#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library (tidyverse)
library (lubridate)
library (DT)
library(plotly)

#global variables
file_data = "corona_data.csv"

#global init
tb_data =read_delim(file_data , delim = ";"  , col_types = cols())  %>% 
    mutate ( last_updated_hour =  floor_date (last_updated , unit = "hour") )


tb_countries = tibble(Country = "All Countries")%>% 
                union_all (tb_data %>% 
                            arrange(desc (TotalCases))%>% 
                            distinct(Country))


last_updated = tb_data %>% 
               summarise(last_updated = max (last_updated_hour)) %>% 
               pull ()

interval_choices = c("1 day" , "1 week" , "1 month")



# Define UI 
ui <- fluidPage(

    theme = shinytheme("sandstone") ,
    
    tags$head(
      tags$style(".well , .dataTables_wrapper {background-color: white; 
                        font-family: \"Trebuchet MS\",  Helvetica, sans-serif;
                        font-size: 14px}") 
      #,tags$style(".svg-container{background-color: black; border: 1px solid #777;
      #                                    font-family: Arial, Helvetica, sans-serif;}")  
      
    ) ,
    
    
    titlePanel("COVID-19 CORONAVIRUS Evolution"),
    tags$p ("Data Source: "
                , tags$a(href="https://www.worldometers.info/coronavirus/", "https://www.worldometers.info/coronavirus/" , target="_blank") ) ,
    

    
    textOutput("lastupdated" ),

    sidebarLayout(
        sidebarPanel  (

          width = 2 ,
            selectInput("interval", label = "Interval" , choices = interval_choices  ) ,
            selectInput("countries", label = "Country", choices = tb_countries) ,
            tags$strong( tags$p ("Top 20 by Total Cases") ) ,
            plotOutput("gg_bar_current" ,click = "plot_click" ,  dblclick = "plot_dblclick" )
        ) ,
        mainPanel( width = 9 ,
                   fluidRow( 
                             plotlyOutput("gg_evolution") ,
                             #verbatimTextOutput("debug") ,                   
                             dataTableOutput("result")
                            )
                  )
    )   
)

# Define Server
server <- function(input, output, session) {
    
    
    
    initial_date_range = reactive({ 
            tb_data %>%
            distinct(last_updated_hour) %>%
            mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
            distinct(datetime_range) %>%
            pull () %>%
            range ()
    } )
    
    
    
    
    
    tb_result =reactive(  {
          
                        req(input$interval)
                        
                        if (input$countries == "All Countries") {
                                
                                tb_data %>% 
                                arrange(desc (last_updated_hour))%>%
                                mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
                                select (-c(TotalRecovered,last_updated,last_updated_hour,ActiveCases ))%>% 
                                group_by( datetime_range , Country)%>% 
                                filter(row_number() == 1) %>%
                                group_by( datetime_range) %>%
                                summarise(  TotalCases = sum(TotalCases)
                                            , TotalDeaths = sum(TotalDeaths ,na.rm = T) )%>% 
                                ungroup()%>%
                                arrange (datetime_range)%>%
                                mutate (NewCases = TotalCases - lag(TotalCases) 
                                        , NewDeaths= TotalDeaths - lag(TotalDeaths) )}
                            
                        else {
                                  tb_data %>% 
                                  arrange(desc (last_updated_hour))%>%
                                  mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
                                  select (-c(TotalRecovered,last_updated,last_updated_hour,ActiveCases ))%>% 
                                  filter (Country %in% c(input$countries)) %>% 
                                  # filtter on barplot
                                  #filter ( Country %in% global$selected_country | global$selected_country == "*" ) %>% 
                                  group_by( datetime_range) %>%
                                  filter(row_number() == 1) %>%
                                  ungroup()%>%
                                  arrange (datetime_range)%>%
                                  mutate( NewCases = TotalCases - lag(TotalCases) 
                                          , NewDeaths= TotalDeaths - lag(TotalDeaths) 
                                  ) 
                        }
        
            }
    )
    
    tb_result_current = reactive (  {
        
        
                                    tb_data %>% 
                                    mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
                                    arrange(desc (last_updated_hour)) %>%
                                    group_by( Country ) %>%
                                    filter (row_number()==1)%>%
                                    arrange(desc(TotalCases))%>%
                                    head(20) }
                                    )
                                


    
    plotly_selected_date_range =    reactive( {
        
        tb=    event_data("plotly_relayout" , source = "plotly1" ) %>%
                as_tibble()
        
        if (!is_empty(tb) && str_detect( str_c( names(tb) , collapse = "/"), "xaxis.range") ) {
            
                tb %>%
                select (matches( "xaxis.range") )%>%
                pivot_longer(everything() , values_to = "date_range")%>%
                select (date_range) %>%
                pull ()%>%
                range ()
        }
        else {
            
            initial_date_range ()
        }  
    }
    )
    
    
    output$lastupdated = renderText( str_c("Last Updated: " ,last_updated, collapse = T))
    
 
    output$result <- renderDataTable ({
        datatable (tb_result () %>%
                       filter( datetime_range >=  plotly_selected_date_range () [1] & datetime_range <=plotly_selected_date_range () [2] )%>%
                        rename(Date = datetime_range ) 

                   , options = list(searching = FALSE ,pageLength = 5) )
    })
    
    output$gg_evolution = renderPlotly (
        {
        

                
            interval = input$interval
            
            tick_format = case_when(
                interval == "1 day" ~ "%b %d" ,
                interval == "6 hours" ~ "%d" ,
                interval %in% c ("1 week" , "1 month") ~ "%b %d" ,
                
                TRUE ~ "%H" 
            )
            
            dtick = case_when(
                interval == "1 day" ~ 2* 86400000.0 , #  1 day = 86400000.0
                interval == "6 hours" ~ 86400000.0  , # 1 hr = 3600000 miliseconds
                interval == "1 week" ~ 7* 86400000.0  , # 1 hr = 3600000 miliseconds
                interval == "1 month" ~ 15* 86400000.0  , 
                TRUE ~ 2* 86400000.0
            )
            
            
            x_title = case_when(
                interval == "1 day" ~ "Date" ,
                interval == "6 hours" ~ "Hour" ,
                TRUE ~ "Date" 
            )

            
            curr_totals =tb_result () %>% 
                            arrange(desc (datetime_range)) %>% 
                            head(1)%>% 
                            select (TotalCases, TotalDeaths)
  
            title = str_c(input$countries , 
                          " Total Cases: " , curr_totals$TotalCases, 
                          " Total Deaths: " ,curr_totals$TotalDeaths, collapse = T)
            
            tb_result () %>% 
                plot_ly( x= ~datetime_range , mode = "lines" , source = "plotly1" )  %>% 
                add_lines(y= ~TotalCases , name = "Total Cases" ,color = I ("#044289") )%>% 
                add_lines(y= ~TotalDeaths  , name = "Total Deaths",  yaxis = "y2" , color = I ("#d73a49") )%>% 
                add_text(text = ~NewCases ,y= ~TotalCases ,textposition='top center' , showlegend =T 
                         ,hovertemplate  = str_c ("Total Cases: %{y}" , "New Cases: %{text}" , "%{x}", sep = "<br>" )
                         ,color = I ("#044289") , name ="New Cases"  )%>% 
                add_text(text = ~NewDeaths , y= ~TotalDeaths , yaxis = "y2",textposition='bottom center' 
                         ,  showlegend = T, name ="New Deaths" 
                         , hovertemplate  = str_c ("Total Deaths: %{y}" , "New Deaths: %{text}", "%{x}" , sep = "<br>" )
                         , color = I ("#d73a49")
                )%>% 
                layout(yaxis2 = list(
                    overlaying = "y",
                    side = "right",
                    title = "Total Deaths" ,
                    anchor = "free" ,
                    position = 0.95
                    
                ),
                xaxis = list(title = "Date" 
                             , type = "date"
                             , tick0 = ~ min (datetime_range)  #tick0
                             , tickformat = tick_format
                             , dtick =dtick ), # dtick in miliseconds
                yaxis = list(title = "Total Cases" , zeroline = F),
                margin = list(b = 100) ,
                legend = list(x = 0.1, y = 0.9) ,
                title = title ,
                titlefont = list (family = "Trebuchet MS, sans-serif" 
                                  , size = 18
                                  #, color = "#e1e4e8"
                                  )
                ) %>% 
                config(  displaylogo = FALSE ,  modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d" , "zoom3d", "zoomInGeo" , "zoomOutGeo"
                                                                           ,"toImage" , "zoomInMapbox" , "zoomOutMapbox"
                                                                           , "sendDataToCloud" , "pan2d" , "lasso2d" , "select2d"
                                                                           , "hoverClosestCartesian", "hoverCompareCartesian"
                                                                           , "toggleHover" , "toggleSpikelines"))
            
        
        }
    )
    
    output$debug = renderPrint(
                                {  plotly_selected_date_range ()
                                })
    
    output$gg_bar_current = renderPlot( {
        
                                           curr_total = max (tb_result_current()$TotalCases)
                                        
                                            tb_result_current ()  %>% 
                                            #mutate(selected = ifelse(Country %in% global$selected_country | global$selected_country == "*", "yes" , "no" ) )%>%
                                            # ggplot(aes ( reorder (Country , TotalCases) ,  TotalCases , fill=selected)) +
                                            ggplot(aes ( reorder (Country , TotalCases) ,  TotalCases) )+
                                            geom_bar(stat = "Identity") +
                                            scale_y_continuous(breaks  = NULL) +
                                            #scale_fill_manual(values = c("yes" = "#3e3f3a", "no" = "grey" ), guide = FALSE )+
                                            labs (x=NULL , y=NULL) +
                                            coord_flip()+
                                            theme_minimal() +
                                            theme(panel.grid.major = element_blank() ,
                                                  panel.grid.minor = element_blank()) 
                                            
        
                                        }
                                      )
    
    
}

#App
shinyApp(ui, server)

