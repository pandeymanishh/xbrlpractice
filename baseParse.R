
#From the instance file 
#get the context information cleaned and ready
ins_cont<-html_nodes(ins,xpath='//context')

#Get the unit
ins_unit<-html_nodes(ins,xpath='//unit')

#Get the footnote
ins_footnote<-html_nodes(ins,xpath='//footnotelink')

###---------------------------------------------------------------------###
#Working with the footnote part
ft.loc<-ins_footnote %>% html_nodes(xpath='//loc') %>% html_attrs()
ft.arc<-ins_footnote %>% html_nodes(xpath='//footnotearc') %>% html_attrs()
ft.note<-ins_footnote %>% html_nodes(xpath='//footnote') %>% html_attrs()

ft.loc1<-rbindlist(lapply(ft.loc,function(x){as.data.frame(t(x))}),fill=TRUE)
ft.arc1<-rbindlist(lapply(ft.arc,function(x){as.data.frame(t(x))}),fill=TRUE)
ft.note1<-rbindlist(lapply(ft.note,function(x){as.data.frame(t(x))}),fill=TRUE)

ft.note2<-data.table(ft.note1
                     ,"text"=ins_footnote %>% html_nodes(xpath='//footnote') %>% html_text())


#Work with the contexts

#If a context has two segments then separate out that context
cleanContext<-function(x){

#Get the segments clean as well 
tt<- x %>% xml_contents() %>% xml_contents()
t1<- tt %>% xml_text()
t2<- tt %>% xml_name()

seg<-tt[t2 %in% c("segment")]
seg.dim<-seg %>% xml_contents() %>% xml_attr('dimension')
seg.txt<-seg %>% xml_contents() %>% xml_text()

t1<-data.table(t(t1[t2 %in% c("identifier","instant","startdate","enddate")]))
t2<-t2[t2 %in% c("identifier","instant","startdate","enddate")]
names(t1)<-t2

#Get the context id as well 
cont.id<-xml_attr(x,attr="id")

if(length(seg)==0){
  return(data.table("id"=cont.id,t1))
}else{
  tt1<-data.table("id"=cont.id
                  ,t1
                  ,"seg_dim"=seg.dim
                  ,"seg_val"=seg.txt)
  return(tt1)
}

}

cont1<-rbindlist(sapply(ins_cont,cleanContext),fill = TRUE)

###-------------------------------------------------------------------------------###
#Clean the unit section

###-------------------------------------------------------------------------------###

#get the base nodes
base_node<- xml_child(x = ins) %>%  xml_children()  %>%  xml_contents() 
#Get the node names
base_name<-xml_name(base_node)
#Now get the node value
base_text<-xml_text(base_node)

#Get the name and text together
base_cons1<-data.table("NodeName"=base_name
                       ,"NodeText"=base_text)

base_cons


#get the value for each node 
node_val<-html_text(base_node)

#Get the attributes

getattr<-base_node %>% html_attrs()

#Function to clean the list
cleanlist<-function(x){
  
  tt<-as.data.frame(t(x))
  setDT(tt)
  for (i in seq_along(tt)) set(tt, i=which(is.na(tt[[i]])), j=i, value='1')
  return(tt)
}

getattr1<-lapply(getattr,cleanlist)

names(getattr1)
#Add the node values to this table
getattr2<-data.table(getattr1,"Value"=node_val)

#Extract only us-gaap nodes



