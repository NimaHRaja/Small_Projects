source("init.R")

rbind(
    DF_night %>% 
        filter(Date != Sys.Date()) %>%
        # filter(night_start_time >= as.POSIXct("18:00:00", format="%H:%M:%S")) %>%
        as.data.frame() %>% select(night_start_time,night_end_time) %>% melt(),
    DF_aft %>% 
        # filter(nap_end_time >= as.POSIXct("12:00:00", format="%H:%M:%S")) %>% 
        as.data.frame() %>% 
        select(nap_start_time,nap_end_time) %>% melt()) %>%
    ggplot(aes(x = variable , y = value, fill = variable)) + geom_boxplot()

rbind(
    DF_night %>%
        filter(Date != Sys.Date()) %>%
        filter(Date != '2020-06-17') %>% 
        # filter(night_start_time >= as.POSIXct("18:00:00", format="%H:%M:%S")) %>%
        as.data.frame() %>% select(night_start_time,night_end_time) %>% melt(),
    DF_aft %>% 
        filter(Date != '2020-06-17') %>% 
        # filter(nap_end_time >= as.POSIXct("12:00:00", format="%H:%M:%S")) %>% 
        as.data.frame() %>% 
        select(nap_start_time,nap_end_time) %>% melt()) %>%
    ggplot(aes(x = variable , y = value, fill = variable)) + geom_boxplot()

