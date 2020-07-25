#### Init ####

options(stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(reshape2)

#### Read and Clean ####

DF_raw <- read.csv("Sleep_log.csv")

DF_raw <- DF_raw %>% 
    mutate(Start = as.POSIXct(Start)) %>%
    mutate(End = as.POSIXct(End)) %>%
    mutate(Start2 = Start + 24*60*60) %>%
    mutate(End2 = End + 24*60*60)

#### Create DF_aft ####

DF_aft <- 
    DF_raw %>% 
    group_by(Date = as.Date(Start)) %>% 
    mutate(nap_start = min(Start)) %>% 
    filter(Start == nap_start) %>%
    ungroup() %>%
    mutate(nap_end = End) %>%
    mutate(nap_length = difftime(nap_end, nap_start, units = "hours") %>% as.numeric()) %>%
    mutate(nap_start_time = 
               strftime(nap_start, format="%H:%M:%S") %>% as.POSIXct(format="%H:%M:%S")) %>%
    mutate(nap_end_time = 
               strftime(nap_end, format="%H:%M:%S") %>% as.POSIXct(format="%H:%M:%S")) %>%
    mutate(day_month = format(Start,"%m-%d"))

# removing anomalies (days she didn't nap)
DF_aft <- 
    DF_aft %>% filter(Date != '2020-06-17') %>% filter(Date != '2020-06-17') 

#### Create DF_night ####

DF_night <- 
    DF_raw %>% 
    group_by(Date = as.Date(Start)) %>% 
    mutate(night_start = max(Start)) %>% 
    filter(Start == night_start) %>%
    ungroup() %>%
    mutate(night_end = End) %>%
    mutate(night_length = difftime(night_end, night_start, units = "hours") %>% as.numeric()) %>%
    mutate(night_start_time = 
               strftime(night_start, format="%H:%M:%S") %>% as.POSIXct(format="%H:%M:%S")) %>%
    mutate(night_end_time = 
               strftime(night_end, format="%H:%M:%S") %>% as.POSIXct(format="%H:%M:%S")) %>%
    mutate(day_month = format(Start,"%m-%d"))

DF_night <- 
    DF_night %>% filter(Date != Sys.Date()) # today's nap shouldn't be classified as night sleep.