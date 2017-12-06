prjiterate <- function(dset, w1sp, w2sp, w3sp, w1f,w2f, w3f, ftdcu.1,tran.1, grad.1, sprgad.1){
        #reweight
        if (w1sp==0 & w2sp==0 & w3sp==0){
                w1spr <- 1/3
                w2spr <- 1/3
                w3spr <- 1/3
        }
        else {
        w1spr <- w1sp/( w1sp+ w2sp+ w3sp)
        w2spr <- w2sp/( w1sp+ w2sp+ w3sp)
        w3spr <- w3sp/( w1sp+ w2sp+ w3sp)
        }
        
        if (w1f==0 & w2f==0 & w3f==0) {
                w1fr <- 1/3
                w2fr <- 1/3
                w3fr <- 1/3
        }
        else {
                w1fr <- w1f/(w1f+w2f+w3f)
                w2fr <- w2f/(w1f+w2f+w3f)
                w3fr <- w3f/(w1f+w2f+w3f)     
        }
        
        
        #get the retention percent from last 3 terms
        for (k1 in names(dset)[! names(dset) %in% c('Fterm','classlvl.x','cnt.x','cnt.y.x','cnt.y.y')]){
                for (t in 1:3){
                        dset <- dset %>% mutate( var= ifelse(is.na(cnt.x), NA,lag(dset[,k1],8*t)  )   )
                        names(dset)[names(dset) =='var'] <- paste0(k1,'_',t)
                        dset
                }
                
        }
        # calcuate the weighted percentage and fall to spring count
        for (m in c('x','y')){
                for (i in c(as.character(1:4),'spcl','master','Phd','g-p')){
                        k <- paste0(i, paste0('.',m,'_',seq(1:3)))
                        
                        if (m=='x'){
                                dset$var <- dset[,k[1]] * w1spr + dset[,k[2]] * w2spr +dset[,k[3]] * w3spr 
                                dset$varhc <- dset$cnt.x*dset$var    
                                names(dset)[names(dset) %in% c( 'var', 'varhc') ] <- c( paste0(m,'cal_',i),paste0(m,'cal_',i,'_hc' ))
                        }
                        else if (m=='y'){
                                dset$var= dset[,k[1]] * w1fr + dset[,k[2]] * w2fr +dset[,k[3]] * w3fr 
                                
                                names(dset)[names(dset) %in% c( 'var') ] <- paste0(m,'cal_',i)
                        }
                        
                        
                }
                
        }
        
        #get spring new count using last spring new distribution to distribute the addtional spring adds
        dset <- dset %>% group_by(Fterm)%>% mutate(newxtll= sum(new.x_1))%>% ungroup() %>% 
                mutate(spnewPct= new.x_1/newxtll)%>% select(-c(newxtll))%>% mutate(est_sp_new= new.x_1+ sprgad.1*spnewPct)
        
        #fall to spring sum across rows
        for (i in c(as.character(1:4),'spcl','master','Phd','g-p')){
                names(dset)[names(dset)==paste0('xcal_',i,'_hc')] <- 'vr'
                dset <- dset %>% group_by(Fterm)%>% mutate(vr= sum(vr, na.rm=T))%>% ungroup()
                names(dset)[names(dset) %in% c( 'vr') ] <- paste0('sp_sum',i)
        }
        
        #transpose and add the calcuated counts
        spest <-dset[,c('Fterm', paste0('sp_sum',c(as.character(1:4),'spcl','master','Phd','g-p')))]%>% unique()%>% filter(sp_sum1>0)
        spest <- melt(spest, id.vars = 'Fterm')%>% mutate(classlvl.x= substr(variable,7, length(variable)))
        
        
        
        dset <- merge(dset, spest, by=c('Fterm','classlvl.x'), all.x = T)
        dset <- dset %>% select(-c(variable))%>% mutate(estSpring= value+ est_sp_new)%>% select(-c(value))
        
        #calcuate teh estimate new fall students
        dset <- dset %>% mutate(
                estnew_fall= ifelse( classlvl.x=='1', new.y_1+ ftdcu.1-FTDCU + 0.15*(tran.1-Transfer),
                                     ifelse(classlvl.x=='2',  new.y_1+ 0.5* (tran.1-Transfer), 
                                            ifelse(classlvl.x=='3', new.y_1+ 0.3*(tran.1- Transfer), 
                                                   ifelse(classlvl.x=='4', new.y_1+ 0.05*(tran.1- Transfer),
                                                          ifelse(classlvl.x=='spcl', new.y_1,
                                                                 ifelse(classlvl.x=='master', new.y_1+ 0.74*(grad.1-Grad), 
                                                                        ifelse(classlvl.x=='Phd',new.y_1+ 0.26*(grad.1-Grad),
                                                                               new.y_1)))))))
                
        )
        
        #cal ycal_hc based on the ycal weight
        for (i in c(as.character(1:4),'spcl','master','Phd','g-p')){
                dset <- dset %>% mutate(var= dset[, paste0('ycal_',i)]*estSpring)
                names(dset)[names(dset)=='var'] <- paste0('ycal_',i,'_hc')
        }
        
        #add up across rows
        for (i in c(as.character(1:4),'spcl','master','Phd','g-p')){
                names(dset)[names(dset)==paste0('ycal_',i,'_hc')] <- 'vr'
                dset <- dset %>% group_by(Fterm)%>% mutate(vr= sum(vr, na.rm=T))%>% ungroup()
                names(dset)[names(dset) %in% c( 'vr') ] <- paste0('fa_sum',i)
        }
        
        #compute for next fall est
        faest <-dset[,c('Fterm', paste0('fa_sum',c(as.character(1:4),'spcl','master','Phd','g-p')))]%>% unique()%>% filter(fa_sum1>0)
        faest <- melt(faest, id.vars = 'Fterm')%>% mutate(classlvl.x= substr(variable,7, length(variable)))
        
        dset<- merge(dset, faest, by=c('Fterm','classlvl.x'), all.x = T)
        dset <- dset %>% select(-c(variable))%>% mutate(estFall= value+ estnew_fall)%>% select(-c(value))
        
        
        #reorganize the data
        act1 <- dset[,c('Fterm','classlvl.x','cnt.x', paste0(c(as.character(1:4),'spcl','master','Phd','g-p'),'.x'),
                       'new.x','cnt.y.x', paste0(c(as.character(1:4),'spcl','master','Phd','g-p'),'.y'), 'new.y','cnt.y.y',
                       'FTDCU','Transfer','Grad')]%>% filter(! is.na(`1.x`))
        
        act2 <-dset[,c('Fterm','classlvl.x','cnt.x', 
                      paste0('xcal_', c(as.character(1:4),'spcl','master','Phd','g-p')),'est_sp_new','estSpring',
                      paste0('ycal_', c(as.character(1:4),'spcl','master','Phd','g-p')),
                      'estnew_fall',
                      'estFall', '1.x')]%>% filter( is.na(`1.x`))%>% select(-c(`1.x`))%>% mutate(FTDCU= ftdcu.1,
                                                                                                 Transfer= tran.1,
                                                                                                 Grad= grad.1)
        
        names(act2) <- names(act1)
        
        #incluing 2018 estimated numbers, using actual 2017 enrollment
        dset <- rbind(act1, act2)
        #get the estfall cnt.y.y move down
       dset  %>% mutate(cnt.x= ifelse(! is.na(cnt.x), cnt.x, lag(cnt.y.y,8) )  )
}

#proj1 <- prjiterate1(dset=proj,w1sp=1,w2sp=0, w3sp=0, w1f=0.75, w2f=0.25, w3f=0, ftdcu.1 = 7890, tran.1 = 1550, grad.1 = 1750, sprgad.1 = 0)
#proj1 <- prjiterate1(dset=proj1,w1sp=1,w2sp=0, w3sp=0, w1f=0.75, w2f=0.25, w3f=0, ftdcu.1 = 7890, tran.1 = 1550, grad.1 = 1750, sprgad.1 = 0)

#proj1 %>% filter(! is.na(cnt.x) & is.na(`1.x`))%>% mutate(lvl= ifelse(classlvl.x %in% c(as.character(1:4),'spcl'),'ungrd','other'))%>%
 #       group_by(lvl)%>% summarise(ttl=sum(cnt.x))



