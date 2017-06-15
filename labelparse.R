# xbrlpractice
library(rvest)
library(data.table)
library(stringr)


labelparse<-function(labxml){
  
  lab=read_html(labxml)
  
  #Get all the loc
  loc1<- html_nodes(lab,xpath='//loc') 
  loc2<-html_attrs(loc1)
  loc3<-as.data.table(t(sapply(loc2,'[')))
  names(loc3)<-gsub('[[:punct:]]','_',names(loc3))
  
  #Only keep the locator
  loc3<-loc3[xlink_type=='locator']
  
  t1<-str_split(loc3$xlink_href,"#")
  loc3[,instanceloc:=sapply(t1,function(x) x[2]),]
  
  #Get the label links
  lab1<- html_nodes(lab,xpath='//label') 
  lab2<-html_attrs(lab1)
  lab21<-html_text(lab1)
  lab3<-as.data.table(t(sapply(lab2,'[')))
  names(lab3)<-gsub('[[:punct:]]','_',names(lab3))
  lab3[,label:=lab21,]
  
  #Only keep the locator
  lab3<-lab3[xlink_type=='resource']
  
  #Now get the labelarc
  labarc1<- html_nodes(lab,xpath='//labelarc') 
  labarc2<-html_attrs(labarc1)
  labarc3<-as.data.table(t(sapply(labarc2,'[')))
  names(labarc3)<-gsub('[[:punct:]]','_',names(labarc3))
  
  #Only keep the arc
  labarc3<-labarc3[xlink_type=='arc']
  
  #Bringing all together now 
  
  #Arc and resource first
  temp1<-merge(lab3[xml_lang=="en-US"                  
                    ,c("xlink_label","id","label")                  
                    ,with=FALSE]            
               ,labarc3[,c("xlink_to","xlink_from")           
                        ,with=FALSE]     
               ,by.x="xlink_label"            
               ,by.y="xlink_to")
  
  #get the locator now 
  labelparse<-merge(temp1          
                    ,loc3[,c("xlink_label","instanceloc")   
                          ,with=FALSE]  
                    ,by.x="xlink_from"          
                    ,by.y="xlink_label")
  
  #Get the label type
  ll1<-str_split(labelparse$id,"_")
  
  labtype<-sapply(ll1,function(x) x[1])
  labgrp<-sapply(ll1,function(x) x[2])
  
  ll2<-str_split(labelparse$instanceloc,"_")
  
  instgrp<-sapply(ll2,function(x) x[1])
  instkey<-sapply(ll2,function(x) x[2])
  
  #Append to the dataset
  labelparse[,':='(label_type=labtype        
                   ,label_grp=labgrp          
                   ,inst_grp=instgrp            
                   ,inst_key=instkey),]
  
  setcolorder(labelparse,c("xlink_from","xlink_label","id","label_type" 
                           ,"label_grp","instanceloc","inst_grp","inst_key","label"))
  
  return(labelparse)
  }

