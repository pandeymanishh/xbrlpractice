
pre_Parse<-function(prexml){
  
#Get the presentation links

prelink<-html_nodes(prexml,xpath='//presentationlink') 

for(i in seq(1,length(prelink))){  
  
  header<-str_split(prelink[i] %>% html_attr('xlink:role'),"/",simplify = TRUE)
  
  header<-header[1,ncol(header)]
  
  #Now extract the locators and arcs
  
  #First the locators
  
  tmp1<-data.table("type"=prelink[i] %>% html_nodes('loc') %>% html_attr(name='xlink:type')  
                 ,"href"=prelink[i] %>% html_nodes('loc') %>% html_attr(name='xlink:href') 
                 ,"label"=prelink[i] %>% html_nodes('loc') %>% html_attr(name='xlink:label'))
  
  #Add the label key 
  tmp1[,keyloc:=sapply(str_split(href,"#"),function(x) x[2]),]
  
  #Add the presentation link header
  tmp1[,PresentationLink:=header,]
  
  #Noe get the presentation arc
  
  tmp2<-data.table("type"=prelink[i] %>% html_nodes('presentationarc') %>% html_attr(name='xlink:type')  
                   ,"order"=prelink[i] %>% html_nodes('presentationarc') %>% html_attr(name='order')  
                   ,"prefLab"=prelink[i] %>% html_nodes('presentationarc') %>% html_attr(name='preferredlabel') 
                   ,"arcr"=prelink[i] %>% html_nodes('presentationarc') %>% html_attr(name='xlink:arcrole')
                   ,"from"=prelink[i] %>% html_nodes('presentationarc') %>% html_attr(name='xlink:from')  
                   ,"to"=prelink[i] %>% html_nodes('presentationarc') %>% html_attr(name='xlink:to')  )
  
  #Add the label key 
  tmp2[,':='(prefLabel=sapply(str_split(prefLab,"/"),function(x) x[6])
             ,arcRole=sapply(str_split(arcr,"/"),function(x) x[6])),]
  
  #Add the presentation link header
  tmp2[,PresentationLink:=header,]
  
  if(i==1){  
    pre_loc<-tmp1
    pre_arc<-tmp2
  }else{  
    pre_loc<-rbind(pre_loc,tmp1)
    pre_arc<-rbind(pre_arc,tmp2)
  }
                             
  return(list("pre_loc"=pre_loc,"pre_arc"=pre_arc))                           
}
}

