source("init.R")

rbind(
    DF_night %>% 
        as.data.frame() %>% select(night_start_time,night_end_time) %>% melt(),
    DF_aft %>% 
        as.data.frame() %>% select(nap_start_time,nap_end_time) %>% melt()) %>%
    ggplot(aes(x = variable , y = value, fill = variable)) + geom_boxplot()


rbind(
    DF_night %>%  
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        as.data.frame() %>% select(night_start_time,night_end_time) %>% melt(),
    DF_aft %>% 
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        as.data.frame() %>% select(nap_start_time,nap_end_time) %>% melt()) %>%
    ggplot(aes(x = variable , y = value, fill = variable)) + geom_boxplot()



rbind(
    DF_night %>% 
        as.data.frame() %>% select(night_start_time,night_end_time) %>% melt() %>%
        mutate(subset = "All"),
    DF_aft %>% 
        as.data.frame() %>% select(nap_start_time,nap_end_time) %>% melt() %>%
        mutate(subset = "All"),
    DF_night %>%  
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        as.data.frame() %>% select(night_start_time,night_end_time) %>% melt() %>%
        mutate(subset = "last 7 days"),
    DF_aft %>% 
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        as.data.frame() %>% select(nap_start_time,nap_end_time) %>% melt() %>%
        mutate(subset = "last 7 days")#,
    # DF_night %>%  
    #     filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
    #     as.data.frame() %>% select(night_start_time,night_end_time) %>% melt() %>%
    #     mutate(subset = "last 28 days"),
    # DF_aft %>% 
    #     filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
    #     as.data.frame() %>% select(nap_start_time,nap_end_time) %>% melt() %>%
    #     mutate(subset = "last 28 days")
    ) %>%
    ggplot(aes(x = variable , y = value, fill = subset)) + geom_boxplot()
