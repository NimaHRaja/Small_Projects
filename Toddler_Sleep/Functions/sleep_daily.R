### Finalised.
### Generates graphs describing the time-series of sleep_daily

source("Functions/sleep_24h.R")

sleep_daily_p1 <- 
    full_join(
        DF_night %>% select(night_length, Date),
        DF_aft %>% select(nap_length, Date), 
        by = "Date") %>% 
    melt(id.vars = "Date") %>% 
    filter(!is.na(value)) %>%
    # mutate(sleep = factor(variable, levels = c("nap_length", "night_length"))) %>%
    ggplot(aes(x = Date, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "stack") +
    ylab("sleep (h)")

sleep_daily_p2 <- 
    full_join(
        DF_night %>% select(night_length, Date),
        DF_aft %>% select(nap_length, Date), 
        by = "Date") %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 14) %>%
    melt(id.vars = "Date") %>% 
    filter(!is.na(value)) %>%
    # mutate(sleep = factor(variable, levels = c("nap_length", "night_length"))) %>%
    ggplot(aes(x = Date, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "stack") +
    ylab("sleep (h)")

sleep_daily_p3 <- 
    rbind(
        DF_24h_summary %>%
            filter(Date %>% strftime(format ="%H:%M:%S") %in% c("10:00:00","11:00:00")) %>%
            mutate(subset = "all"),
        DF_24h_summary %>%
            filter(Date %>% strftime(format ="%H:%M:%S") %in% c("10:00:00","11:00:00")) %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 90) %>% 
            mutate(subset = "last 90 days"),
        DF_24h_summary %>%
            filter(Date %>% strftime(format ="%H:%M:%S") %in% c("10:00:00","11:00:00")) %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
            mutate(subset = "last 28 days"),
        DF_24h_summary %>%
            filter(Date %>% strftime(format ="%H:%M:%S") %in% c("10:00:00","11:00:00")) %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
            mutate(subset = "last 7 days")) %>%
    mutate(sleep_24h = sleep_24h / 60) %>%
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    ggplot(aes(x = Date, y = sleep_24h, colour = subset)) + 
    geom_point() + geom_smooth(method = "lm", formula = 'y ~ x') +
    ylab("sleep (h)")

sleep_daily_p4 <- 
    full_join(
        DF_night %>% select(Date, night_length),
        DF_aft %>% select(Date, nap_length), 
        by = "Date") %>% 
    mutate(nap_length = if_else(is.na(nap_length), 0, nap_length)) %>%
    mutate(tot_sleep = night_length + nap_length) %>%
    mutate(week_day = wday(Date, label = TRUE)) %>%
    filter(difftime(Sys.time(), Date, units = "days") <= 28 &
               difftime(Sys.time(), Date, units = "days") > 1) %>% 
    select(-Date) %>%
    melt(id.vars  = "week_day") %>% 
    ggplot(aes(x = week_day, y = value, fill = variable)) + 
    geom_boxplot() +
    ylab("length (h)") +
    ggtitle("last 28 days")

sleep_daily_p5 <- 
    full_join(
        DF_night %>% select(Date, night_length),
        DF_aft %>% select(Date, nap_length), 
        by = "Date") %>% 
    mutate(nap_length = if_else(is.na(nap_length), 0, nap_length)) %>%
    mutate(tot_sleep = night_length + nap_length) %>%
    mutate(week_day = wday(Date, label = TRUE)) %>%
    filter(difftime(Sys.time(), Date, units = "days") <= 28 &
               difftime(Sys.time(), Date, units = "days") > 1) %>% 
    ggplot(aes(x = week_day, y = tot_sleep)) + 
    geom_boxplot(fill = "lightgreen") +
    ylab("length (h)") +
    ggtitle("last 28 days")
