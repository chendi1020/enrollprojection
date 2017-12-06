#



library(shiny)
library(shinyBS)
library(DT)
library(shinythemes)

library(highcharter)

shinyUI(fluidPage( 
        
        #theme = shinytheme("cerulean"),
                  navbarPage("Enrollment Projection",
                            
                             tabPanel("By Class Level",
                                      fluidRow(
                                             
                                              column(2,
                                                     column(12,
                                                            br(),
                                                            #uiOutput('cohort'),
                                                            numericInput("ftdcu", label = h5("New First Time Undergrad Input"), value = 7890),
                                                            numericInput("tran", label = h5("New Transfer Undergrad Input"), value = 1550),
                                                            numericInput("grad", label = h5("New Grad Input"), value = 1750),
                                                            numericInput("spadd", label = h5("Addtional Spring adds"), value = 0),
                                                            selectInput("outyr", label = h5("Years to project out"), 
                                                                        choices = list("3 yrs" = 3, "5 yrs" = 5, "10 yrs" = 10), 
                                                                        selected = 5),
                                                            selectInput("Includeyr", label = h5("Historical Years Included"), 
                                                                        choices = list("Since 2000" = 1004, "Since 2005" = 1054,
                                                                                       "Since 2010" = 1104), 
                                                                        selected = 1054),
                                                            selectInput("breakdown", label = h5("Result breakdown by"), 
                                                                        choices = list("overall", "student level", "class level"), 
                                                                        selected = "student level"),
                                                            
                                                            downloadButton('downloadds', 'Download'),
                                                            
                                                            
                                                              br(),
                                                              br(),
                                                              br(),
                                                              img(src='IS Logo.svg',
                                                                  style="display: block; margin-left: auto; margin-right: auto;"),
                                                            br(),
                                                            p(paste('updated at 2017-12-05'))
                                                            
                                                            
                                                            
                                                     )
                                                    # column(6,
                                                     #       br(),
                                                            
                                                      #      sliderInput("wsp1", label = h6("Fall to Spring Weight 1"), min = 0, 
                                                       #                 max = 1, step = 0.25, value = 1),
                                                            
                                                        #    sliderInput("wsp2", label = h6("Fall to Spring Weight 2"), min = 0, 
                                                         #               max = 1, step = 0.25,value = 0),
                                                          #  sliderInput("wsp3", label = h6("Fall to Spring Weight 3"), min = 0, 
                                                           #             max = 1,step = 0.25, value = 0),
                                                            #sliderInput("wfa1", step = 0.25,label = h6("Spring to Fall Weight 1"), min = 0, 
                                                             #           max = 1, value = 0.75),
                                                            #sliderInput("wfa2", step = 0.25,label = h6("Spring to Fall Weight 2"), min = 0, 
                                                             #           max = 1, value = 0.25),
                                                            #sliderInput("wfa3", label = h6("Spring to Fall Weight 3"), min = 0, 
                                                             #           max = 1,step = 0.25, value = 0)
                                                            
                                                            
                                                            
                                                   #  )    
                                                   )
                                              ,
                                              column(10,
                                                     
                                                     column(2, sliderInput("wsp1", label = h6("% weight Fall to Spring previous yr"), min = 0, 
                                                                           max = 1, step = 0.25, value = 1),
                                                            bsTooltip("wsp1", "% using previous yr data as a part to estimate future fall to spring re-ernoll rate")),
                                                    
                                                     column(2, sliderInput("wsp2", label = h6("% weight Fall to Spring 2-yr back"), min = 0, 
                                                                           max = 1, step = 0.25,value = 0),
                                                            
                                                            bsTooltip("wsp2", "% using data 2-yrs back as a part to estimate future fall to spring re-ernoll rate")),
                                                     #column(2, uiOutput("fsw2")),
                                                     column(2, sliderInput("wsp3", label = h6("% weight Fall to Spring 3-yr back"), min = 0, 
                                                                           max = 1,step = 0.25, value = 0),
                                                            bsTooltip("wsp3", "% using data 3-yrs back as a part to estimate future fall to spring re-ernoll rate")),
                                                     column(2,sliderInput("wfa1", step = 0.25,label = h6("% weight Spring to Fall previous yr"), min = 0, 
                                                                          max = 1, value = 0.75),
                                                            bsTooltip("wfa1", "% using previous yr data as a part to estimate future spring to nexts fall re-ernoll rate")),
                                                     column(2, sliderInput("wfa2", step = 0.25,label = h6("% weight Spring to Fall 2-yr back"), min = 0, 
                                                                           max = 1, value = 0.25),
                                                            bsTooltip("wfa2", "% using data 2-yrs back as a part to estimate future spring to nexts fall re-ernoll rate")),
                                                     column(2, sliderInput("wfa3", label = h6("% weight Spring to Fall 3-yr back"), min = 0, 
                                                                           max = 1,step = 0.25, value = 0),
                                                            bsTooltip("wfa3", "% using data 3-yrs back as a part to estimate future spring to nexts fall re-ernoll rate")) ,
                                                    
                                                    
                                                      div(textOutput("w1", inline = T), style = "font-size: 90%;position: relative;text-align: center;color:#18453B;"),
                                                
                                                     tabsetPanel(
                                                             tabPanel('Chart',
                                                                     
                                                                      br(),
                                                                      #verbatimTextOutput('modelprint'),
                                                                      highchartOutput("trend",height = "520px"),
                                                                     # plotOutput('chart',height = "500px"),
                                                                     #DT::dataTableOutput('tb') 
                                                                     div(DT::dataTableOutput("tb"), style = "font-size: 90%;")
                                                                      )
                                                            
                                                             
                                                             
                                                             
                                                                      )
                                                         )
                                              
                                              
                                              ))
                            
                             
                             
                             ),
                  
                  tags$style(type = 'text/css', '.navbar { background-color: #18453B;
                           font-family: Arial;
                           font-size: 13px;
                           color: #FFFFFF; }',
                             
                             '.navbar-default .navbar-brand {
                             color: #FFFFFF;
                           }'
                             
                  ),
                  tags$head(tags$style(HTML('
                              
                                            
                                            .tooltip .tooltiptext {
                                            visibility: hidden;
                                            width: 150px;
                                            background-color: darkgreen;
                                            font-size: 9px;
                                            color: #000000;
                                            text-align: center;
                                            border-radius: 6px;
                                            padding: 5px 0;
                                            position: absolute;
                                            z-index: 1;
                                            bottom: 100%;
                                            left: 50%;
                                            margin-left: -60px;
                                            }
                                            
                                            .tooltip:hover .tooltiptext {
                                            visibility: visible;
                                            opacity: 1;
                                            }
                                            ')))
                  
                  
                  
                  
                  )
                  )


