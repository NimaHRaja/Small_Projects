# source("init.R")

##### night_start distribution #####

# DF_night %>% 
#     # filter(Date != Sys.Date()) %>%
#     ggplot(aes(x = night_start_time)) + geom_density(colour = "blue")

# DF_night %>% 
#     # filter(Date != Sys.Date()) %>%
#     # filter(Date != '2020-06-17') %>% 
#     # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
#     ggplot(aes(x = night_start_time)) + geom_density(colour = "blue")

# DF_night %>% 
#     # filter(Date != Sys.Date()) %>%
#     ggplot(aes(x = night_start_time)) + geom_histogram(fill = "blue")

# DF_night %>% 
#     # filter(Date != Sys.Date()) %>%
#     ggplot(aes(y = night_start_time)) + geom_boxplot(fill = "green")

rbind(
    DF_night %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
        mutate(subset = "last 28 days")) %>%
    # filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_start_time, colour = subset)) + geom_density()

# rbind(
#     DF_night %>%
#         mutate(subset = "all"),
#     DF_night %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%
#     filter(Date != Sys.Date()) %>%
#     ggplot(aes(y = night_start_time, fill = subset)) + geom_boxplot()

##### night_end distribution #####

# DF_night %>% 
#     filter(Date != Sys.Date()) %>%
#     ggplot(aes(x = night_end_time)) + geom_density(colour = "blue")

# DF_night %>% 
#     filter(Date != Sys.Date()) %>%
#     # filter(Date != '2020-06-17') %>% 
#     # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
#     ggplot(aes(x = night_end_time)) + geom_density(colour = "blue")

# DF_night %>% 
#     filter(Date != Sys.Date()) %>%
#     ggplot(aes(x = night_end_time)) + geom_histogram(fill = "blue")

# DF_night %>%
#     filter(Date != Sys.Date()) %>%
#     ggplot(aes(y = night_end_time)) + geom_boxplot(fill = "green")

rbind(
    DF_night %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
        mutate(subset = "last 28 days")) %>%
    # filter(Date != '2020-06-17') %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_end_time, colour = subset)) + geom_density()

# rbind(
#     DF_night %>%
#         mutate(subset = "all"),
#     DF_night %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%    
#     # filter(Date != '2020-06-17') %>% 
#     filter(Date != Sys.Date()) %>%
#     ggplot(aes(y = night_end_time, x = subset, fill = subset)) + geom_boxplot()

#### nap_length distribution ####

# DF_night %>% 
#     ggplot(aes(x = night_length)) + geom_density(colour = "blue")

# DF_night %>% filter(Date != '2020-06-17') %>% 
#     ggplot(aes(x = night_length)) + geom_density(colour = "blue")

# DF_night %>% filter(Date != '2020-06-17') %>% 
#     filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#     ggplot(aes(x = night_length)) + geom_density(colour = "blue")

# DF_night %>% 
#     ggplot(aes(x = night_length)) + geom_histogram(fill = "blue")

# DF_night %>% filter(Date != '2020-06-17') %>% 
#     ggplot(aes(x = night_length)) + geom_histogram(fill = "blue")

# DF_night %>% filter(Date != '2020-06-17') %>% 
#     filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#     ggplot(aes(x = night_length)) + geom_histogram(fill = "blue")

# DF_night %>% 
#     #filter(Date != '2020-06-17') %>% 
#     ggplot(aes(y = night_length)) + geom_boxplot(fill = "green")

# DF_night %>% 
#     #filter(Date != '2020-06-17') %>% 
#     filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#     ggplot(aes(y = night_length)) + geom_boxplot(fill = "green")


rbind(
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days"),
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
        mutate(subset = "last 28 days")) %>%
    ggplot(aes(x = night_length, colour = subset)) + geom_density()

# rbind(
#     DF_night %>%
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         mutate(subset = "all"),
#     DF_night %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         mutate(subset = "last 7 days"),
#     DF_night %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         mutate(subset = "last 28 days")) %>%    
#     # filter(Date != '2020-06-17') %>% 
#     ggplot(aes(x = subset, y = night_length, fill = subset)) + geom_boxplot()

#### night_end v night_start ####

# DF_night %>%
#     ggplot(aes(x = night_start_time, y = night_end_time)) + 
#     geom_point(colour = "red")

# DF_night %>% 
#     filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#     # filter(Date != Sys.Date()) %>%
#     # filter(Date != '2020-06-17') %>% 
#     # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
#     ggplot(aes(x = night_start_time, y = night_end_time)) + 
#     geom_point(colour = "red")

# rbind(
#     DF_night %>%
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         mutate(subset = "all"),
#     DF_night %>%
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%
#     ggplot(aes(x = night_start_time, y = night_end_time, label = day_month, colour = subset)) + 
#     geom_point()

DF_night %>% 
    filter(Date != Sys.Date()) %>%
    mutate(recent = difftime(Sys.time(), Date, units = "days") <= 7) %>%
    # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
    ggplot(aes(x = night_start_time, y = night_end_time, label = day_month)) + 
    geom_point(colour = "green") +
    geom_text(aes(colour = recent)) + 
    geom_smooth(method = "lm") + 
    theme(legend.position = "none")

# rbind(
#     DF_night %>%
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         mutate(subset = "all"),
#     DF_night %>%
#         filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days")) %>%
#     ggplot(aes(x = night_start_time, y = night_end_time, colour = subset, label = day_month)) + 
#     geom_point() +
#     geom_text() + 
#     geom_smooth(method = "lm")

#### start/end/length v time ####

DF_night %>% 
    # filter(Date != '2020-06-17') %>% 
    filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
    select(Date, night_start_time, night_end_time) %>% 
    melt(id.vars = "Date") %>% 
    ggplot(aes(x = Date, y = value, colour = variable)) + 
    geom_line() + geom_point()



DF_night %>% 
    filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
    ggplot(aes(x = Date, y = night_length))+ 
    geom_bar(stat = "identity", fill = "blue")

#### prob_night ####

prob_night <-
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
    mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
    mutate(join_value = 1) %>%
    full_join(DF_night %>% select(Date, night_start_time, night_end_time) %>% mutate(join_value = 1), 
              by = "join_value") %>% 
    mutate(value = if_else(time > night_start_time | time < night_end_time, 1, 0)) %>%
    mutate(value = value + if_else(time == night_start_time | time == night_end_time, 1/2,0))%>% 
    group_by(time) %>% 
    summarise(prob = sum(value)) %>% 
    ungroup() %>%
    mutate(prob = prob / max(prob))

prob_night %>% ggplot(aes(x = time, y = prob)) + geom_point(colour = "blue")

get_prob_night_start <- function(df_local, subset){
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
        mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
        filter(time > min(df_local$night_start_time - 10 * 60)) %>%
        filter(time < max(df_local$night_start_time + 10 * 60)) %>%
        mutate(join_value = 1) %>%
        full_join(df_local %>% select(Date, night_start_time) %>% mutate(join_value = 1), 
                  by = "join_value") %>% 
        mutate(value = if_else(time >= night_start_time, 1, 0)) %>%
        group_by(time) %>% 
        summarise(num_days = sum(value)) %>% 
        ungroup() %>%
        mutate(prob = num_days / (max(num_days) + no_night_days)) %>%
        mutate(subset = subset)
}

rbind(
    get_prob_night_start(
        DF_night, "All"),
    get_prob_night_start(
        DF_night %>% filter(difftime(Sys.time(), Date, units = "days") <= 7), "Last 7 days"),
    get_prob_night_start(
        DF_night %>% filter(difftime(Sys.time(), Date, units = "days") <= 28), "Last 28 days")) %>% 
    ggplot(aes(x = time, y = prob, colour = subset)) + geom_line()

get_prob_night_end <- function(df_local, subset){
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
        mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
        filter(time > min(df_local$night_end_time - 10 * 60)) %>%
        filter(time < max(df_local$night_end_time + 10 * 60)) %>%
        mutate(join_value = 1) %>%
        full_join(df_local %>% select(Date, night_end_time) %>% mutate(join_value = 1), 
                  by = "join_value") %>% 
        mutate(value = if_else(time >= night_end_time, 1, 0)) %>%
        group_by(time) %>% 
        summarise(num_days = sum(value)) %>% 
        ungroup() %>%
        mutate(prob = num_days / (max(num_days) + no_night_days)) %>%
        mutate(subset = subset)
}

rbind(
    get_prob_night_end(
        DF_night, "All"),
    get_prob_night_end(
        DF_night %>% filter(difftime(Sys.time(), Date, units = "days") <= 7), "Last 7 days"),
    get_prob_night_end(
        DF_night %>% filter(difftime(Sys.time(), Date, units = "days") <= 28), "Last 28 days")) %>% 
    ggplot(aes(x = time, y = prob, colour = subset)) + geom_line()
