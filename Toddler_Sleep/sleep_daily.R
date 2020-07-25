source("sleep_24h.R")

full_join(
    DF_night %>% select(night_length, Date),
    DF_aft %>% select(nap_length, Date), 
    by = "Date") %>% 
    melt(id.vars = "Date") %>% 
    filter(!is.na(value)) %>%
    # mutate(sleep = factor(variable, levels = c("nap_length", "night_length"))) %>%
    ggplot(aes(x = Date, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "stack")

rbind(
    DF_24h_summary %>%
        filter(Date %>% strftime(format ="%H:%M:%S") == "11:00:00") %>%
        mutate(subset = "all"),
    DF_24h_summary %>%
        filter(Date %>% strftime(format ="%H:%M:%S") == "11:00:00") %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days"),
    DF_24h_summary %>%
        filter(Date %>% strftime(format ="%H:%M:%S") == "11:00:00") %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
        mutate(subset = "last 28 days")) %>%
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    ggplot(aes(x = Date, y = sleep_24h, colour = subset)) + 
    geom_point() + geom_smooth(method = "lm")