inner_join(DF_aft, DF_night, by = "Date") %>%
    filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
    ggplot(aes(x = nap_start_time, y = night_start_time)) + geom_point(colour = "blue") + 
    geom_smooth(method = "lm")


inner_join(DF_aft, DF_night, by = "Date") %>%
    filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
    ggplot(aes(x = nap_end_time, y = night_start_time)) + geom_point(colour = "blue") + 
    geom_smooth(method = "lm")



DF %>% filter(Description == "Supervision") %>% group_by(Date) %>% 
    summarise(num_sup = n()) %>% ungroup() %>%
    inner_join(DF_aft, by = "Date") %>% 
    ggplot(aes(x = nap_start_time, y = num_sup)) + geom_point() + geom_smooth(method = "lm")



DF %>% filter(Description == "Supervision") %>% group_by(Date) %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 28) %>%  
    summarise(num_sup = n()) %>% ungroup() %>%
    inner_join(DF_aft, by = "Date") %>% View()
    ggplot(aes(x = as.factor(num_sup), y = nap_start_time)) + geom_boxplot(fill = "lightgreen")
