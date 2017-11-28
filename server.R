library(shiny)
library(dplyr)
library(reshape2)

library(highcharter)

library(DT)

load("data/enrlfs17.Rda")
source("./enrlprojfuc.R", local = TRUE)

#
#proj <- proj %>% mutate(predterm = ifelse(Fterm>2017, 'Estimated','Actual'),
 #                       Termcode= paste0('20',substr(as.character(Fterm),2,3)))
shinyServer(function(input, output) {
        dat <- reactive({
                req(input$ftdcu, input$tran, input$grad, input$spadd, input$wsp1, input$wsp2, input$wsp3,
                    input$wfa1, input$wfa2, input$wfa3) 
                
                #FS18
                
                for (m in 1:as.numeric( input$outyr) ){
                        proj <- prjiterate(dset=proj,
                                           w1sp=input$wsp1,w2sp=input$wsp2, w3sp=input$wsp3, w1f=input$wfa1,
                                           w2f=input$wfa2, w3f=input$wfa3, 
                                           ftdcu.1 = input$ftdcu, tran.1 = input$tran, grad.1 = input$grad, sprgad.1 = input$spadd)    
                }
                proj %>% mutate(predterm = ifelse(Fterm>1174, 'Proj','Actu'),
                                Termcode= paste0('Fall 20',substr(as.character(Fterm),2,3)),
                                lvl= ifelse(classlvl.x %in% c(as.character(1:4),'spcl'),' undergrad',
                                            ifelse(classlvl.x %in% c('Phd','master'),'grad','professional')),
                                classlvl= ifelse(classlvl.x=='1','(1)freshmen',
                                                 ifelse(classlvl.x=='2','(2)sophmore',
                                                        ifelse(classlvl.x=='3','(3)junior',
                                                               ifelse(classlvl.x=='4','(4)senior', classlvl.x)))))%>%
                        select(Fterm, Termcode,predterm,cnt.x,classlvl,lvl)%>%filter(! is.na(cnt.x))
                        
                
                
        })
        
        datagg <- reactive({
                req(input$ftdcu, input$tran, input$grad, input$spadd, input$wsp1, input$wsp2, input$wsp3,
                    input$wfa1, input$wfa2, input$wfa3, input$breakdown) 
                if (input$breakdown =='overall'){
                        ds <- dat() %>% mutate(lvl='overall')%>% group_by(Fterm, Termcode,predterm,lvl)%>% summarise(headcount= ceiling( sum(cnt.x, na.rm=T)) )%>% ungroup()
                        #ds <- dcast(ds, by ~ Termcode, value.var = 'headcount')
                }
                else if (input$breakdown =='student level'){
                        ds <- dat() %>% group_by(Fterm, Termcode,predterm, lvl)%>% summarise(headcount= ceiling( sum(cnt.x, na.rm=T)))%>% ungroup()
                        #ds <- dcast(ds, lvl ~ Termcode, value.var = 'headcount')
                }
                else if (input$breakdown =='class level'){
                        ds <- dat() %>% group_by(Fterm, Termcode,predterm, classlvl)%>% summarise(headcount= ceiling(sum(cnt.x, na.rm=T)))%>% ungroup()
                      # names(ds)[names(ds)=='classlvl']<-'lvl'
                }
                
                
        })
        
        output$tb <- DT::renderDataTable({
                agg <- datagg()%>% mutate(maxterm=max(Fterm))%>% filter(Fterm>=maxterm-120)
                if (input$breakdown =='class level'){
                        agg<- dcast(agg, classlvl ~ Termcode, value.var = 'headcount')
                }
                else {
               agg<- dcast(agg, lvl ~ Termcode, value.var = 'headcount')
                }
                
                
                      t<-  datatable(agg, extensions = 'Responsive',options = list(dom = 't'), rownames= FALSE) 
                      
                      for (i in 1:as.numeric( input$outyr)){
                              t<- t%>% formatStyle(names(agg)[14+i- as.numeric( input$outyr) ],   fontWeight = 'bold', color = 'green'
                                                   )  
                              
                      }
                    
                 for (k in names(agg)[-1]){
                        t<- t%>% formatCurrency(columns = k, currency = "", interval = 3, mark = ",", digit=0)
                 }     
                
                     t
             
                        
        })
        
        
        output$trend <- renderHighchart({
                agg <- datagg() %>% mutate(Act= ifelse(Fterm<=1174, headcount, NA),
                                           project= ifelse(Fterm>1174, headcount, NA))
                corseq <- c("#7fc97f", "#beaed4", "#fdc086", "#b10026",'#386cb0','#f0027f','#bf5b17','#666666')
                hc <- highchart() %>%  hc_xAxis(categories = agg$Termcode)
                if (input$breakdown =='class level'){
                       # hchart(agg, "line", hcaes(x = Termcode, y = headcount, group =classlvl),
                        #       color = c("#7fc97f", "#beaed4", "#fdc086", "#b10026",'#386cb0','#f0027f','#bf5b17','#666666'))%>%
                        #        hc_yAxis(title=list(text="Headcount"))%>% hc_xAxis(title=list(text=""))%>%
                        #hc_tooltip(table = TRUE, sort = TRUE) %>% hc_add_theme(hc_theme_google())
                       # hc <- highchart() %>%  hc_xAxis(categories = agg$Termcode)
                        #corseq <- c("#7fc97f", "#beaed4", "#fdc086", "#b10026",'#386cb0','#f0027f','#bf5b17','#666666')
                        
                        
                        for (i in seq(unique(agg$classlvl))){
                                hc<- hc%>% hc_add_series( name= paste0('Actual ', unique(agg$classlvl)[i]), type="line",marker =  list(enabled=F),
                                                          data =  agg[agg$classlvl==unique(agg$classlvl)[i],]$Act, color=corseq[i]) %>%
                                        
                                        hc_xAxis(categories =agg[agg$classlvl==unique(agg$classlvl)[i],]$Termcode) %>% 
                                        hc_yAxis(title=list(text="Headcount"), min=0)%>%
                                        hc_add_series(name = paste0('Proj ',unique(agg$classlvl)[i]), type='line', dashStyle='longdash',  color=corseq[i],
                                                      marker =  list(fillColor= '#FFFFFF',
                                                                     lineWidth= 2,
                                                                     radius=3,
                                                                     lineColor= corseq[i],
                                                                     symbol="circle"),
                                                      data = agg[agg$classlvl==unique(agg$classlvl)[i],]$project)
                                
                        }
                        
                        hc
                        
                }
                else {
                        #hchart(agg, "line", hcaes(x =Termcode, y = headcount, group =lvl))%>%
                         #       hc_yAxis(title=list(text="Headcount"),
                          #               min=0)%>% hc_xAxis(title=list(text=""))%>%hc_tooltip(table = TRUE, sort = TRUE) %>% hc_add_theme(hc_theme_google())
                        for (i in seq(unique(agg$lvl))){
                                hc<- hc%>% hc_add_series( name= paste0('Actual ', unique(agg$lvl)[i]), type="line",marker =  list(enabled=F),
                                                          data =  agg[agg$lvl==unique(agg$lvl)[i],]$Act, color=corseq[i]) %>%
                                        
                                        hc_xAxis(categories =agg[agg$lvl==unique(agg$lvl)[i],]$Termcode) %>% 
                                        hc_yAxis(title=list(text="Headcount"), min=0)%>%
                                        hc_add_series(name = paste0('Proj ',unique(agg$lvl)[i]), type='line', dashStyle='longdash',  color=corseq[i],
                                                      marker =  list(fillColor= '#FFFFFF',
                                                                     lineWidth= 2,
                                                                     radius=3,
                                                                     lineColor= corseq[i],
                                                                     symbol="circle"),
                                                      data = agg[agg$lvl==unique(agg$lvl)[i],]$project)
                                
                        }
                        
                        hc
                 }
                
              
                
        })
        
        
        output$downloadds <- downloadHandler(
            
                
                
                
                filename = function() { 
                        paste('enrollproj', input$outyr, '.csv', sep='') 
                },
                
                content = function(file) {
                        
                        write.csv(datagg(), file)
                }
        )
        
  
})
