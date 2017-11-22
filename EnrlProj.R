source("H:/R setup/ODBC Connection.R")

source("H:/R setup/OracleDbusing ROracle.R")

#exclude summer from FS10
StuDs <- sqlQuery(Non_Aggregated,"select *
                  from student_data
                  where Term_Seq_Id>=1004 
                  ")
library(dplyr)
library(reshape2)
StuDs <- StuDs %>% mutate(StuLvl= ifelse(Student_Level_Code %in% c('AT','EL','LU','UN'), 'undergrad',
                                         ifelse(Student_Level_Code %in% c('GC','GR','LG','PD'), 'grad',
                                                'g-p')),
                          classlvl= ifelse(Class_Code %in% c('E','L') & StuLvl=='undergrad', 'spcl',
                                           ifelse(Class_Code %in% c('M','GC','L'),'master',
                                                  ifelse(Class_Code %in% c('D','DD','DG','P'),'Phd',
                                                        ifelse(StuLvl=='g-p','g-p', Class_Code) ))))


##FS10 - SS11
#term <- 1104
termjoin <- function(term){
        a <- StuDs %>% filter(Term_Seq_Id==term)
        if ( substr(as.character(term),4,4)=='4'){
                b <- StuDs %>% filter(Term_Seq_Id==term+2)
                t <- merge(a, b, by='Pid', all.x = T)
                new <- merge(b,a, by='Pid', all.x = T)%>% filter(is.na(classlvl.y))%>% group_by(classlvl.x)%>% summarise(new=n())%>% ungroup()
                t <- t %>% group_by(classlvl.x, classlvl.y)%>% summarise(n=n()) %>% ungroup()%>% group_by(classlvl.x)%>% mutate(ttl=sum(n))%>%
                        mutate(Pct= n/ttl)
                }
        else if ( substr(as.character(term),4,4)=='6'){
                b <- StuDs %>% filter(Term_Seq_Id==term+8)
               # sum
                sum <- StuDs %>% filter(Term_Seq_Id==term+6)
                sum <- rbind(as.data.frame(sum), as.data.frame(a))%>% select(Pid, classlvl, Term_Seq_Id)%>% group_by(Pid)%>% mutate(minterm= min(Term_Seq_Id))%>% filter(minterm==Term_Seq_Id)
                t <- merge(sum, b, by='Pid', all.x = T)
                new <- merge(b, sum, by='Pid', all.x = T)%>% filter(is.na(classlvl.y))%>% group_by(classlvl.x)%>% summarise(new=n())%>% ungroup()
                t1 <- t %>% group_by(classlvl.x, classlvl.y)%>% summarise(n=n()) %>% ungroup()
                t2 <- t%>% filter(Term_Seq_Id.x==term)%>% group_by(classlvl.x)%>% summarise(ttl=n())%>% ungroup()
                t <- merge(t1, t2, by='classlvl.x')%>% mutate(Pct=n/ttl)
                 }
       
        
       # t <- merge(a, b, by='Pid', all.x = T)
        
        #t <- t %>% group_by(classlvl.x, classlvl.y)%>% summarise(n=n()) %>% ungroup()%>% group_by(classlvl.x)%>% mutate(ttl=sum(n))%>%
         #       mutate(Pct= n/ttl)
        
        t <- dcast(t, classlvl.x + ttl ~ classlvl.y, value.var = 'Pct')
        
        ##new spring
        #if ( substr(as.character(term),4,4)=='4'){
         #       new <- merge(b,a, by='Pid', all.x = T)%>% filter(is.na(classlvl.y))%>% group_by(classlvl.x)%>% summarise(new=n())%>% ungroup()
        #}
        #else if ( substr(as.character(term),4,4)=='6'){
         #       new <- merge(b, sum, by='Pid', all.x = T)%>% filter(is.na(classlvl.y))%>% group_by(classlvl.x)%>% summarise(new=n())%>% ungroup()
                
        #}
       
        
        c <- merge(b,a, by='Pid', all.x = T)%>% group_by(classlvl.x)%>% summarise(cnt=n())%>% ungroup()
        
        t<- merge(t, new, by='classlvl.x')
        t<- merge(t, c, by='classlvl.x')
        
        for (i in names(t)[! names(t) %in% c('classlvl.x','ttl')]){
                t[,i]<- ifelse(is.na(t[,i]),0, t[,i])
        }
        
        t <- t[, names(t) != 'NA']
        names(t)[names(t) %in% c('ttl','cnt')] <- c('cnt.x','cnt.y')
        t
}

termjoinyr <- function(term){
        a1 <- termjoin(term)
        b1 <- termjoin(term+2)
        c1 <- merge(a1, b1[, names(b1) != 'cnt.x'], by='classlvl.x')
        c1$Fterm <- term
        c1
}


#call function
ds <- data.frame()
for (k in seq(from=1004, to=1164, by=10)){
        tst <- termjoinyr(k)
        ds <- rbind(ds, tst)
}


#get FTU and transfter and grad number
input<- StuDs %>% filter(Term_Classification %in% c('NEW','CONT')) %>%
        mutate(type= ifelse(Student_Level_Code=='UN' & Lvl_Entry_Status=='FRST', 'FTDCU',
                            ifelse(Student_Level_Code=='UN', 'Transfer',
                                   ifelse(Student_Level_Code=='GR','Grad','Other'))))%>% filter(type !='Other' & Term_Seq_Id %in% seq(from=1004, to=1174, by=10))%>%
        group_by(Term_Seq_Id, type)%>% summarise(hc= sum(c_student))%>% ungroup()
input <- dcast(input, Term_Seq_Id ~ type, value.var = 'hc')

cnt <- StuDs %>% filter(Term_Seq_Id %in% seq(from=1004, to=1174, by=10))%>% group_by(Term_Seq_Id, classlvl)%>%
        summarise(cnt.x= sum(c_student))%>% ungroup()

input <- merge(input, cnt , by='Term_Seq_Id')%>% rename(Fterm= Term_Seq_Id,classlvl.x=classlvl )

##project 2018 2019
proj <- data.frame(Fterm= sort(rep(seq(from=1184,to=1184+90, by=10),8)), classlvl.x= rep(unique(StuDs$classlvl),10))  
proj <- proj %>% mutate(cnt.x= NA, FTDCU=NA,  Transfer=NA,Grad=NA)

input <- rbind(input, proj) %>% arrange(Fterm, classlvl.x)


proj <- merge(ds, input, by=c('Fterm','classlvl.x','cnt.x'), all.y = T)


##
#names(dst)[names(dst) %in% c('1.x','1.y','2.x','2.y','3.x','3.y','4.x','4.y')] <- c('Frsh.x','Frsh.y','Soph.x','Soph.y',
 #                                                                                   'Junr.x','Junr.y','Senr.x','Senr.y')

###below will be a function


#write.csv(dst, file = "S:/Institutional Research/ProjWhse/Enrollment/Enrollment Tableau/enrlprojds.csv",
 #         row.names = F, na=""
  #        )


#call function
source("S:/Institutional Research/ProjWhse/Enrollment/Enrollment Visual/Shiny/enrlprojfuc.R")
proj1 <- prjiterate(dset=proj,w1sp=1,w2sp=0, w3sp=0, w1f=0.75, w2f=0.25, w3f=0, ftdcu.1 = 7890, tran.1 = 1550, grad.1 = 1750, sprgad.1 = 0)

proj1 %>% filter(! is.na(cnt.x) & is.na(`1.x`))%>% mutate(lvl= ifelse(classlvl.x %in% c(as.character(1:4),'spcl'),'ungrd','other'))%>%
       group_by(lvl)%>% summarise(ttl=sum(cnt.x))


save(proj, file = "S:/Institutional Research/ProjWhse/Enrollment/Enrollment Visual/Shiny/data/enrlfs17.Rda" )
load("S:/Institutional Research/ProjWhse/Enrollment/Enrollment Visual/Shiny/data/enrlfs17.Rda")

#add termcode
#TERM <- sqlFetch(SISInfo,"TERM")


#2018


















tst <-  dstfa[,c('Fterm','classlvl.x','cnt.x', paste0(c(as.character(1:4),'spcl','master','Phd','g-p'),'.x'),
                'new.x','cnt.y.x', paste0(c(as.character(1:4),'spcl','master','Phd','g-p'),'.y'), 'new.y','cnt.y.y',
                   paste0('xcal_', c(as.character(1:4),'spcl','master','Phd','g-p')),'est_sp_new','estSpring','estnew_fall',
                'estFall','FTDCU','Transfer','Grad')]

t <- dstfa %>% filter(Fterm==1174)%>% select(Fterm, classlvl.x,cnt.x, xcal_1, est_sp_new,sp_sum1, sp_sum2,estSpring, new.y,
                                             new.y_1, FTDCU,  Transfer, Grad, ycal_1,  fa_sum1, ycal_2,ycal_3,estnew_fall, estFall)

#get fall new
