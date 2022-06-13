library(brms)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(ranger)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(readxl)
library(showtext)
library(lubridate)
setwd("/home/usuaris/l.blanco/FirstChapter/")
datasurv <- 
  read_excel("/home/usuaris/l.blanco/FirstChapter/datasurv.xlsx", 
             col_types = c("numeric", 
                           "text", "date", "text", "numeric", "numeric", 
                           "text", "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric"))

datasurv$location<- as.factor(datasurv$location)
datasurv$method<- as.factor(datasurv$method)
str(datasurv)
datasurv <- na.omit(datasurv) 

get_prior(total_lived|cens(censored) ~ daily_acc_gdd + photoperiod + minrh + 
            maxrh + (1|location) + (1|method), data = datasurv, family = weibull())

model1 <- brm(formula = total_lived | cens(censored)~ daily_acc_gdd + 
                photoperiod + minrh + maxrh + (1|location) + (1|method), 
              data = datasurv, family = weibull(), 
              prior = c(set_prior("student_t(3,0,2.5)", class = "sd", group = "location")),
              warmup = 1000, iter = 5000, chains = 4, control = list(adapt_delta = 0.95))
plot(model1)
save(model1, file = "/home/usuaris/l.blanco/FirstChapter/model1.Rdata")

model2 <- brm(formula = total_lived | cens(censored)~ mintemperature + 
                photoperiod  + maxrh + (1|location) + (1|method), 
              data = datasurv, family = weibull(), 
              prior = c(set_prior("student_t(3,0,2.5)", class = "sd", group = "location")),
              warmup = 1000, iter = 5000, chains = 4, control = list(adapt_delta = 0.95))
plot(model2)
save(model2, file = "/home/usuaris/l.blanco/FirstChapter/model2.Rdata")





