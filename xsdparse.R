#Getting ready with xsd file 

##Primary there roletypes and element tags

setwd("/home/manish/Downloads/0001000180-16-000068-xbrl/")

library(rvest)

xsd<-read_html("sndk-20160103.xsd")

xsdele<-html_nodes(xsd,xpath='//element') %>% html_attrs()
xsdpre<-html_nodes(xsd,xpath='//roletype') %>% html_attrs()
xsdpredef<-html_nodes(xsd,xpath='//roletype') %>% html_node('definition') %>% html_text()

#132 definitions

tt1<-rbindlist(lapply(xsdpre,function(x) as.data.frame(t(x))),fill=TRUE)
row.names(tt1)<-NULL

#Now create the final dataset
xsd_ele<-data.table("id"=tt1[,"id"]
                    ,"roleurl"=tt1[,"roleuri"]
                    ,"pre_def"=xsdpredef)


#Get the element
tt2<-rbindlist(lapply(xsdele,function(x) as.data.frame(t(x))),fill=TRUE)
row.names(tt2)<-NULL

