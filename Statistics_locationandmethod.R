library(ggplot2)
library(tidyverse)
library(readxl)
library(patchwork)
library(lattice)
library(lubridate)
library(RColorBrewer)
library(readxl)
library(ggpubr)
library(rstatix)
setwd("C:\\Users\\lblan\\OneDrive\\Escritorio\\CEAB\\2022\\First_chapter")
##
datastat <- read_excel("C:/Users/lblan/OneDrive/Escritorio/CEAB/2022/First_chapter/data_analysis_cens.xlsx", 
                       col_types = c("numeric", "date", "date", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
datset_field <- datastat[!(datastat$location=="3"),]
datset_field_bg <- datastat[!(datastat$location=="3" | datastat$`hl/bg` == "1"),]

#Creamos dataframe del jard?n bot?nico para cada tipo de m?todo de captura
datset_jar <- datastat[!(datastat$location=="2" | datastat$location=="3"),]
datset_jar_hl<-datastat[!(datastat$location=="2" | datastat$location=="3"  | datastat$`hl/bg` == "2"),]
datset_jar_bg<-datastat[!(datastat$location=="2" | datastat$location=="3" | datastat$`hl/bg` == "1"),]
ks.test(datset_jar_bg$total_lived, "pnorm", mean(datset_jar_bg$total_lived, na.rm=T), sd(datset_jar_bg$total_lived,na.rm=T)) #pvalor <0.05 con trampas BG-sentinel
ks.test(datset_jar_hl$total_lived, "pnorm", mean(datset_jar_hl$total_lived, na.rm=T), sd(datset_jar_hl$total_lived,na.rm=T)) #pvalor >0.05 con HL.
test_jar_bghl <- wilcox.test(datset_jar$total_lived~datset_jar$`hl/bg`, exact = FALSE, paired = FALSE)
test_jar_bghl #p-value <0.05. There are significative differences between 2 methods of capture in the Botanical Garden

#Creamos dataframe de Palafolls para cada tipo de m?todo de captura
datset_pal<-datastat[!(datastat$location=="1" | datastat$location=="3"),]
datset_pal_hl<-datastat[!(datastat$location=="1" | datastat$location=="3"| datastat$`hl/bg` == "2"),]
datset_pal_bg<-datastat[!(datastat$location=="1" | datastat$location=="3"| datastat$`hl/bg` == "1"),]
ks.test(datset_pal_bg$total_lived, "pnorm", mean(datset_pal_bg$total_lived, na.rm=T), sd(datset_pal_bg$total_lived,na.rm=T)) #pvalor <0.05 con trampas BG-sentinel
ks.test(datset_pal_hl$total_lived, "pnorm", mean(datset_pal_hl$total_lived, na.rm=T), sd(datset_pal_hl$total_lived,na.rm=T)) #pvalor >0.05 con HL.
test_pal_bghl <- wilcox.test(datset_pal$total_lived~datset_pal$`hl/bg`, exact = FALSE, paired= FALSE)
test_pal_bghl #p-value <0.5. There are significative differences between 2 methods of capture in Palafolls

#WE SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THE SURVIVAL BY FIELD LOCATION (URBAN VS SEMI-URBAN)
ks.test(datset_jar$total_lived, "pnorm", mean(datset_jar$total_lived, na.rm=T), sd(datset_jar$total_lived,na.rm=T)) #pvalor <0.05 en Jardin
ks.test(datset_pal$total_lived, "pnorm", mean(datset_pal$total_lived, na.rm=T), sd(datset_pal$total_lived,na.rm=T)) #pvalor <0.05 en Palafolls
test_location <- wilcox.test(datset_field$total_lived~datset_field$location, exact = FALSE, paired = FALSE)
test_location#p-value <0.5. There are significative differences between survival in 2 field locations.

#Prueba t de Student para dos muestras independientes (con distribucion normal)
t.test(datset_pal_hl$total_lived,datset_jar_hl$total_lived) #p-value <0.05, SIGNIFICANT DIFFERENCES BETWEEN SURVIVAL BY HL METHOD IN THE FIELD LOCATIONS
wilcox.test(datset_field_bg$total_lived~datset_field_bg$location,exact = FALSE, paired = FALSE) #pvalue<0.5. There are significative differences betwwen survival by bg method in the field locations


