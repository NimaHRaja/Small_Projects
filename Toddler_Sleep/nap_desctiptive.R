source("init.R")

##### nap_start distribution #####

# DF_aft %>% 
#     ggplot(aes(x = nap_start_time)) + geom_density(colour = "blue")

# DF_aft %>% filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_start_time)) + geom_density(colour = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_start_time)) + geom_histogram(fill = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(y = nap_start_time)) + geom_boxplot(fill = "green")

rbind(
    DF_aft %>%
        mutate(subset = "all"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    # filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = nap_start_time, colour = subset)) + geom_density()

# rbind(
#     DF_aft %>%
#         mutate(subset = "all"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%
#     # filter(Date != '2020-06-17') %>% 
#     ggplot(aes(y = nap_start_time, fill = subset)) + geom_boxplot()

##### nap_end distribution #####

# DF_aft %>% 
#     ggplot(aes(x = nap_end_time)) + geom_density(colour = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_end_time >= as.POSIXct("12:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_end_time)) + geom_density(colour = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_end_time >= as.POSIXct("12:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_end_time)) + geom_histogram(fill = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_end_time >= as.POSIXct("12:00:00", format="%H:%M:%S")) %>%
#     ggplot(aes(y = nap_end_time)) + geom_boxplot(fill = "green")

rbind(
    DF_aft %>%
        mutate(subset = "all"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    # filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = nap_end_time, colour = subset)) + geom_density()

# rbind(
#     DF_aft %>%
#         mutate(subset = "all"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%    
#     # filter(Date != '2020-06-17') %>% 
#     ggplot(aes(x = subset, y = nap_end_time, fill = subset)) + geom_boxplot()

###### nap_length distribution #####

# DF_aft %>% 
#     ggplot(aes(x = nap_length)) + geom_density(colour = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     ggplot(aes(x = nap_length)) + geom_density(colour = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     ggplot(aes(x = nap_length)) + geom_histogram(fill = "blue")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     ggplot(aes(y = nap_length)) + geom_boxplot(fill = "green")

rbind(
    DF_aft %>%
        mutate(subset = "all"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = nap_length, colour = subset)) + geom_density()

# rbind(
#     DF_aft %>%
#         mutate(subset = "all"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%    
#     filter(Date != '2020-06-17') %>% 
#     ggplot(aes(y = nap_length, fill = subset)) + geom_boxplot()

###### nap_end v nap_start #####

# DF_aft %>%
#     ggplot(aes(x = nap_start_time, y = nap_end_time)) + geom_point(colour = "red")

# DF_aft %>% 
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month)) + 
#     geom_point(colour = "red")

# rbind(
#     DF_aft %>%
#         mutate(subset = "all"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month, colour = subset)) + 
#     geom_point()


DF_aft %>% 
    # filter(Date != '2020-06-17') %>% 
    # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
    ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month)) + 
    geom_point(colour = "green") +
    geom_text(colour = "red") + 
    geom_smooth(method = "lm")

# rbind(
#     DF_aft %>%
#         mutate(subset = "all"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_start_time, y = nap_end_time, colour = subset, label = day_month)) + 
#     geom_point() +
#     geom_text() + 
#     geom_smooth(method = "lm")

###### start/end/length v time

DF_aft %>% 
    # filter(Date != '2020-06-17') %>% 
    select(Date, nap_start_time, nap_end_time) %>% 
    melt(id.vars = "Date") %>% 
    ggplot(aes(x = Date, y = value, colour = variable)) + 
    geom_line() + geom_point()

DF_aft %>% 
    # filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = Date, y = nap_length))+ 
    geom_bar(stat = "identity", fill = "blue")

###### Prob_nap 
prob_nap <-
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
    mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
    filter(time > min(DF_aft$nap_start_time - 10 * 60)) %>%
    filter(time < max(DF_aft$nap_end_time + 10 * 60)) %>%
    mutate(join_value = 1) %>%
    full_join(DF_aft %>% select(Date, nap_start_time, nap_end_time) %>% mutate(join_value = 1), 
              by = "join_value") %>% 
    mutate(value = if_else(time > nap_start_time & time < nap_end_time, 1, 0)) %>%
    mutate(value = value + if_else(time == nap_start_time | time == nap_end_time, 1/2,0))%>% 
    group_by(time) %>% 
    summarise(prob = sum(value)) %>% 
    ungroup() %>%
    mutate(prob = prob / max(prob))

prob_nap %>% ggplot(aes(x = time, y = prob)) + geom_point(colour = "blue")
