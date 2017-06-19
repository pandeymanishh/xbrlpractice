setwd("/home/manish/Downloads/0001000180-16-000068-xbrl/")

# xbrlpractice
library(rvest)
library(data.table)
library(stringr)

pre<-read_html("sndk-20160103_pre.xml")
xsd<-read_html("sndk-20160103.xsd")
lab<-read_html("sndk-20160103_lab.xml")
ins<-read_html("sndk-20160103.xml")

source("/home/manish/Documents/XBRLParse/labelparse.R")
source("/home/manish/Documents/XBRLParse/Preparse.R")
source("/home/manish/Documents/XBRLParse/xsdparse.R")
