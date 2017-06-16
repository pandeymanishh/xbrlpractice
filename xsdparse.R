#Getting ready with xsd file 

##Primary there roletypes and element tags

setwd("/home/manish/Downloads/0001000180-16-000068-xbrl/")

library(rvest)

#xsd is path to the xsd file

xsdParse<-function(xsd){
 
#Read the file in   
xsd_in<-read_html(xsd)

#Extract the roletype (& definition) and the elements
  
xsdele<-html_nodes(xsd_in,xpath='//element') %>% html_attrs()
xsdpre<-html_nodes(xsd_in,xpath='//roletype') %>% html_attrs()
xsdpredef<-html_nodes(xsd_in,xpath='//roletype') %>% html_node('definition') %>% html_text()

#Definition table 
tt1<-rbindlist(lapply(xsdpre,function(x) as.data.frame(t(x))),fill=TRUE)
row.names(tt1)<-NULL

#Now create the final dataset
xsd_pre<-data.table("id"=tt1[,"id"]
                    ,"roleurl"=tt1[,"roleuri"]
                    ,"pre_def"=xsdpredef)

#Get the element
xsd_ele<-rbindlist(lapply(xsdele,function(x) as.data.frame(t(x))),fill=TRUE)
row.names(xsd_ele)<-NULL
                          
return("Pre_Def"=xsd_pre,"Ele_Details"=xsd_ele)
                          
}                          
                          
                          
                          

