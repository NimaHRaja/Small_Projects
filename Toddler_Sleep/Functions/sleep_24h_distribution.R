#### Finalised
#### Finds the distribution of sleep_24h for different time periods.

source("Functions/sleep_24h.R")

min_sleep <- 
    DF_24h_summary %>% 
    # mutate(sleep_24h = 60 * sleep_24h) %>%
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    summarise(min(sleep_24h)) %>%
    as.numeric() - 10


max_sleep <- 
    DF_24h_summary %>% 
    # mutate(sleep_24h = 60 * sleep_24h) %>%
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    summarise(max(sleep_24h)) %>%
    as.numeric() + 10


sleep24h_distribution_flat <-
    DF_24h_summary %>% 
    # mutate(sleep_24h = 60 * sleep_24h) %>% 
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    mutate(date_before = lag(Date,1), sleep24h_before = lag(sleep_24h, 1)) %>%
    filter(!is.na(date_before)) %>% 
    mutate(join_value = 1) %>%
    inner_join(
        data.frame(sleep_24h_value = seq(min_sleep, max_sleep), join_value = 1), 
        by = "join_value") %>%
    mutate(num_minutes = 
               ifelse(sleep_24h == sleep24h_before & sleep_24h == sleep_24h_value, 
                      difftime(Date, date_before, units = "mins"), 
                      ifelse((sleep_24h <= sleep_24h_value & sleep24h_before > sleep_24h_value) |
                                 (sleep_24h >= sleep_24h_value & sleep24h_before < sleep_24h_value),
                             1,0)))

sleep24h_distribution_summary_all <- 
    sleep24h_distribution_flat %>% select(sleep_24h_value, num_minutes) %>% 
    mutate(sleep_24h_value = sleep_24h_value / 60) %>%
    group_by(sleep_24h_value) %>%
    summarise(num_minutes = sum(num_minutes)) %>% 
    ungroup() %>%
    mutate(num_minutes = num_minutes/sum(num_minutes)) %>%
    mutate(subset = "all")

# sleep24h_distribution_summary_all %>%
#     ggplot(aes(x = sleep_24h_value, weight = num_minutes)) + geom_density(colour = "Blue")

# sleep24h_distribution_summary_all %>%   
#     ggplot(aes(x = sleep_24h_value, weight = num_minutes)) + 
#     geom_histogram(fill = "Blue")


# sleep24h_distribution_flat %>% 
#     filter(difftime(Sys.time(), Date, units = "days") < 3) %>% 
#     select(sleep_24h_value, num_minutes) %>% 
#     mutate(sleep_24h_value = sleep_24h_value / 60) %>%
#     group_by(sleep_24h_value) %>%
#     summarise(num_minutes = sum(num_minutes)) %>% 
#     ungroup() %>%
#     mutate(num_minutes = num_minutes/sum(num_minutes)) %>%
#     ggplot(aes(x = sleep_24h_value, weight = num_minutes)) + geom_density(colour = "Blue") +
#     ggtitle("past 3 days")

#### Create 7 and 28 days subsets 

sleep24h_distribution_summary_last07 <- 
    sleep24h_distribution_flat %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
    select(sleep_24h_value, num_minutes) %>% 
    mutate(sleep_24h_value = sleep_24h_value / 60) %>%
    group_by(sleep_24h_value) %>%
    summarise(num_minutes = sum(num_minutes)) %>% 
    ungroup() %>%
    mutate(num_minutes = num_minutes/sum(num_minutes)) %>%
    mutate(subset = " last 07 days")


sleep24h_distribution_summary_last28 <- 
    sleep24h_distribution_flat %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
    select(sleep_24h_value, num_minutes) %>% 
    mutate(sleep_24h_value = sleep_24h_value / 60) %>%
    group_by(sleep_24h_value) %>%
    summarise(num_minutes = sum(num_minutes)) %>% 
    ungroup() %>%
    mutate(num_minutes = num_minutes/sum(num_minutes)) %>%
    mutate(subset = " last 28 days")


sleep24h_distribution_summary_last90 <- 
    sleep24h_distribution_flat %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 90) %>% 
    select(sleep_24h_value, num_minutes) %>% 
    mutate(sleep_24h_value = sleep_24h_value / 60) %>%
    group_by(sleep_24h_value) %>%
    summarise(num_minutes = sum(num_minutes)) %>% 
    ungroup() %>%
    mutate(num_minutes = num_minutes/sum(num_minutes)) %>%
    mutate(subset = " last 90 days")

#### sleep_24h distribution graphs ####

sleep24h_distribution_p1 <- 
    rbind(sleep24h_distribution_summary_all, 
          sleep24h_distribution_summary_last07, 
          sleep24h_distribution_summary_last28, 
          sleep24h_distribution_summary_last90) %>%
    ggplot(aes(x = sleep_24h_value, weight = num_minutes, group = subset, colour = subset)) + 
    geom_density()


sleep24h_distribution_p2 <-
    rbind(sleep24h_distribution_summary_all, 
          sleep24h_distribution_summary_last07, 
          sleep24h_distribution_summary_last28, 
          sleep24h_distribution_summary_last90) %>%
    ggplot(aes(x = subset, y = sleep_24h_value, weight = num_minutes, fill = subset)) + 
    geom_boxplot(outlier.alpha = 0)


sleep24h_distribution_p3 <-
    rbind(sleep24h_distribution_summary_all, 
          sleep24h_distribution_summary_last07, 
          sleep24h_distribution_summary_last28, 
          sleep24h_distribution_summary_last90) %>%
    arrange(sleep_24h_value) %>%
    group_by(subset) %>%
    mutate(num_minutes_cdf = cumsum(num_minutes)) %>%
    ungroup() %>%
    ggplot(aes(x = sleep_24h_value, y = num_minutes_cdf, colour = subset)) + 
    geom_line()
