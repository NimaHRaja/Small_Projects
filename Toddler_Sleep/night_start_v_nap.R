source("init.R")

#### nap_end ####

inner_join(DF_aft, DF_night, "Date") %>% 
    ggplot(aes(x = nap_end_time, y = night_start_time)) + geom_point(colour = "blue")

# inner_join(DF_aft, DF_night, "Date") %>% 
#     filter(as.POSIXct(Start.y) > as.POSIXct(End.x)) %>% 
#     ggplot(aes(x = nap_end_time, y = night_start_time)) + geom_point(colour = "blue")

inner_join(DF_aft, DF_night, c("Date", "day_month")) %>%
    ggplot(aes(x = nap_end_time, y = night_start_time, label = day_month)) + 
    geom_point(colour = "green") +
    geom_text(colour = "red") +
    geom_smooth(method = "lm")

#### nap_length ####

inner_join(DF_aft, DF_night, "Date") %>%
    # mutate(nap_length = difftime(as.POSIXct(End.x), as.POSIXct(Start.x), "minutes") %>% as.numeric()) %>% 
    ggplot(aes(x = nap_length, y = night_start_time)) + geom_point(colour = "blue")


# inner_join(DF_aft, DF_night, "Date") %>%
#     mutate(nap_length = difftime(as.POSIXct(End.x), as.POSIXct(Start.x), "minutes") %>% as.numeric()) %>% 
#     filter(as.POSIXct(Start.y) > as.POSIXct(End.x)) %>% 
#     ggplot(aes(x = nap_length, y = night_start_time)) + geom_point(colour = "blue")


inner_join(DF_aft, DF_night, c("Date", "day_month")) %>%
    # mutate(nap_length = difftime(as.POSIXct(End.x), as.POSIXct(Start.x), "minutes") %>% as.numeric()) %>% 
    # filter(as.POSIXct(Start.y) > as.POSIXct(End.x)) %>% 
    ggplot(aes(x = nap_length, y = night_start_time, label = day_month)) + 
    geom_point(colour = "green") +
    geom_text(colour = "red") +
    geom_smooth(method = "lm")

########## sleep_24h

source("sleep_24h.R")

DF_night %>% 
    # filter(Date != Sys.Date()) %>%
    mutate(Date = Start) %>% 
    inner_join(DF_24h_summary, by = "Date") %>%
    filter(sleep_24h > 6) %>%
    ggplot(aes(x = sleep_24h, y= night_start_time, label = day_month)) + 
    geom_point(colour = "green") +
    geom_text(colour = "red") +
    geom_smooth(method = "lm")

##############
# 

# DF_24h_start <- 
#     inner_join(DF_night %>% mutate(night_start = as.POSIXct(night_start)), 
#                DF_summary, by = c("night_start" = "Date"))
# 
# DF_24h_start <- 
#     DF_24h_start %>% 
#     filter(night_start != min(DF_24h_start$night_start))  %>% 
#     filter(night_start != max(DF_24h_start$night_start)) 
# 
# 
# DF_24h_start %>%
#     mutate(start_night = strftime(night_start, format="%H:%M:%S") %>% as.POSIXct(format="%H:%M:%S")) %>%
#     ggplot(aes(x = sleep_24h, y = start_night)) + geom_point(colour = "blue")
# 
