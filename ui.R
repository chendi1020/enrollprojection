#



library(shiny)
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
                                                            p(paste('updated at ', Sys.Date()))
                                                            
                                                            
                                                            
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
                                                     
                                                     column(2, sliderInput("wsp1", label = h6("Fall to Spring Weight 1"), min = 0, 
                                                                           max = 1, step = 0.25, value = 1)),
                                                     column(2, sliderInput("wsp2", label = h6("Fall to Spring Weight 2"), min = 0, 
                                                                           max = 1, step = 0.25,value = 0)),
                                                     column(2, sliderInput("wsp3", label = h6("Fall to Spring Weight 3"), min = 0, 
                                                                           max = 1,step = 0.25, value = 0)),
                                                     column(2,sliderInput("wfa1", step = 0.25,label = h6("Spring to Fall Weight 1"), min = 0, 
                                                                          max = 1, value = 0.75)),
                                                     column(2, sliderInput("wfa2", step = 0.25,label = h6("Spring to Fall Weight 2"), min = 0, 
                                                                           max = 1, value = 0.25)),
                                                     column(2, sliderInput("wfa3", label = h6("Spring to Fall Weight 3"), min = 0, 
                                                                           max = 1,step = 0.25, value = 0)) ,
                                                     tabsetPanel(
                                                             tabPanel('Chart',
                                                                     
                                                                      br(),
                                                                      #verbatimTextOutput('modelprint'),
                                                                      highchartOutput("trend",height = "550px"),
                                                                     # plotOutput('chart',height = "500px"),
                                                                     DT::dataTableOutput('tb')     
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
                             
                  )
                  
                  
                  
                  
                  )
                  )


