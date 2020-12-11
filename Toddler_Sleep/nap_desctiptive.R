# source("Functions/init.R")

#### nap_start ####

nap_desctiptive_p1 <- 
    rbind(
        DF_aft %>%
            mutate(subset = "all"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
            mutate(subset = "last 07 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
            mutate(subset = "last 28 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 90) %>% 
            mutate(subset = "last 90 days")) %>%
    ggplot(aes(x = nap_start_time, colour = subset)) + geom_density()

nap_desctiptive_p2 <- 
    DF_aft %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 28 &
               difftime(Sys.time(), Date, units = "days") > 1) %>% 
    ggplot(aes(x = week_day, y= nap_start_time)) + 
    geom_boxplot(fill = "lightgreen") +
    ggtitle("last 28 days")


#### nap_end ####

nap_desctiptive_p3 <- 
    rbind(
        DF_aft %>%
            mutate(subset = "all"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 07) %>% 
            mutate(subset = "last 7 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
            mutate(subset = "last 28 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 90) %>% 
            mutate(subset = "last 90 days")) %>%
    ggplot(aes(x = nap_end_time, colour = subset)) + geom_density()

nap_desctiptive_p4 <- 
    DF_aft %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 28 &
               difftime(Sys.time(), Date, units = "days") > 1) %>% 
    ggplot(aes(x = week_day, y= nap_end_time)) + 
    geom_boxplot(fill = "lightgreen") +
    ggtitle("last 28 days")


#### nap_length ####

nap_desctiptive_p5 <-
    rbind(
        DF_aft %>%
            mutate(subset = "all"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
            mutate(subset = "last 07 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
            mutate(subset = "last 28 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 90) %>% 
            mutate(subset = "last 90 days")) %>%
    ggplot(aes(x = nap_length, colour = subset)) + geom_density()

nap_desctiptive_p6 <- 
    DF_aft %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 28 &
               difftime(Sys.time(), Date, units = "days") > 1) %>% 
    ggplot(aes(x = week_day, y= nap_length)) + 
    geom_boxplot(fill = "lightgreen") +
    ggtitle("last 28 days")

nap_desctiptive_p7 <- 
    DF_aft %>% 
    ggplot(aes(x = Date, y = nap_length))+ 
    geom_bar(stat = "identity", fill = "blue")


#### nap_end v nap_start ####

# DF_aft %>% 
#     mutate(recent = difftime(Sys.time(), Date, units = "days") <= 7) %>%
#     # filter(Date != '2020-06-17') %>% 
#     # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
#     ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month)) + 
#     geom_point(colour = "green") +
#     geom_text(aes(colour = recent)) + 
#     geom_smooth(method = "lm") + 
#     theme(legend.position = "none")

# rbind(
#     DF_aft %>%
#         mutate(subset = "all"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 90) %>% 
#         mutate(subset = "last 90 days"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 28) %>% 
#         mutate(subset = "last 28 days"),
#     DF_aft %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 07 days")) %>% 
#     ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month, colour = subset)) + 
#     geom_text()

nap_desctiptive_p8 <- 
    rbind(
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 28 &
                       difftime(Sys.time(), Date, units = "days") > 7) %>%
            mutate(subset = "last 28 days"),
        DF_aft %>%
            filter(difftime(Sys.time(), Date, units = "days") <= 7) %>%
            mutate(subset = "last 07 days")) %>%   
    ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month)) + 
    geom_text(aes(colour = subset)) + 
    geom_smooth(method = "lm") + 
    theme(legend.position = "none")

nap_desctiptive_p9 <- 
    DF_aft %>%
    mutate(week_day = as.character(week_day)) %>%
    filter(difftime(Sys.time(), Date, units = "days") <= 28) %>%   
    ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month, colour = week_day)) + 
    geom_text()

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

nap_desctiptive_p10 <- 
    DF_aft %>% 
    select(Date, nap_start_time, nap_end_time) %>% 
    melt(id.vars = "Date") %>% 
    ggplot(aes(x = Date, y = value, colour = variable)) + 
    geom_line() + geom_point()


#### Prob_nap ####

get_prob_nap_all <- function(df_local, subset){
    
    tot_days <- df_local %>% summarise(n_distinct(Date)) %>% as.integer()
    
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
        mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
        filter(time > min(DF_aft$nap_start_time - 10 * 60)) %>%
        filter(time < max(DF_aft$nap_end_time + 10 * 60)) %>%
        mutate(join_value = 1) %>%
        full_join(df_local %>% select(Date, nap_start_time, nap_end_time) %>% mutate(join_value = 1), 
                  by = "join_value") %>% 
        mutate(value = if_else(time > nap_start_time & time < nap_end_time, 1, 0)) %>%
        mutate(value = value + if_else(time == nap_start_time | time == nap_end_time, 1/2,0))%>% 
        group_by(time) %>% 
        summarise(num_days = sum(value)) %>%
        ungroup() %>%
        # mutate(prob = num_days / (max(num_days) + no_nap_days)) %>%
        mutate(prob = num_days / tot_days) %>%
        mutate(subset = subset)
}

nap_desctiptive_p11 <- 
    rbind(
        get_prob_nap_all(
            DF_aft, "All"),
        get_prob_nap_all(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 7), "Last 07 days"),
        get_prob_nap_all(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 28), "Last 28 days"),
        get_prob_nap_all(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 90), "Last 90 days")) %>%
    ggplot(aes(x = time, y = prob, colour = subset)) + geom_line()


get_prob_nap_start <- function(df_local, subset){
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
        mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
        filter(time > min(df_local$nap_start_time - 10 * 60)) %>%
        filter(time < max(df_local$nap_start_time + 10 * 60)) %>%
        mutate(join_value = 1) %>%
        full_join(df_local %>% select(Date, nap_start_time) %>% mutate(join_value = 1), 
                  by = "join_value") %>% 
        mutate(value = if_else(time >= nap_start_time, 1, 0)) %>%
        group_by(time) %>% 
        summarise(num_days = sum(value)) %>% 
        ungroup() %>%
        # mutate(prob = num_days / (max(num_days) + no_nap_days)) %>%
        mutate(prob = num_days / max(num_days)) %>%
        mutate(subset = subset)
}

nap_desctiptive_p12 <- 
    rbind(
        get_prob_nap_start(
            DF_aft, "All"),
        get_prob_nap_start(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 7), "Last 07 days"),
        get_prob_nap_start(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 28), "Last 28 days"),
        get_prob_nap_start(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 90), "Last 90 days")) %>% 
    ggplot(aes(x = time, y = prob, colour = subset)) + geom_line()

get_prob_nap_end <- function(df_local, subset){
    data.frame(time = seq(1,24*60) * 60 - 3660) %>%
        mutate(time = Sys.Date() %>% as.POSIXct() + time) %>%
        filter(time > min(df_local$nap_end_time - 10 * 60)) %>%
        filter(time < max(df_local$nap_end_time + 10 * 60)) %>%
        mutate(join_value = 1) %>%
        full_join(df_local %>% select(Date, nap_end_time) %>% mutate(join_value = 1), 
                  by = "join_value") %>% 
        mutate(value = if_else(time >= nap_end_time, 1, 0)) %>%
        group_by(time) %>% 
        summarise(num_days = sum(value)) %>% 
        ungroup() %>%
        # mutate(prob = num_days / (max(num_days) + no_nap_days)) %>%
        mutate(prob = num_days / max(num_days)) %>%
        mutate(subset = subset)
}

nap_desctiptive_p13 <- 
    rbind(
        get_prob_nap_end(
            DF_aft, "All"),
        get_prob_nap_end(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 7), "Last 07 days"),
        get_prob_nap_end(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 28), "Last 28 days"),
        get_prob_nap_end(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 90), "Last 90 days")) %>% 
    ggplot(aes(x = time, y = prob, colour = subset)) + geom_line()





get_prob_nap_length <- function(df_local, subset){
    inner_join(
        data.frame(time = seq(0,(max(df_local$nap_length)*60+15)),
                   join_value = 1),
        left_join(
            data.frame(Date = seq(min(df_local$Date), max(df_local$Date),1)),
            df_local %>% select(Date, nap_length),
            by = "Date") %>% 
            mutate(nap_length = if_else(is.na(nap_length),0,nap_length)) %>% 
            mutate(join_value = 1), 
        by = "join_value") %>% 
        mutate(value = if_else(time/60 <= nap_length , 1, 0)) %>%
        group_by(time) %>% 
        summarise(num_days = sum(value)) %>% 
        ungroup() %>% 
        mutate(prob = num_days / max(num_days)) %>%
        mutate(subset = subset)
}

nap_desctiptive_p14 <- 
    rbind(
        get_prob_nap_length(
            DF_aft, "All"),
        get_prob_nap_length(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 7), "Last 07 days"),
        get_prob_nap_length(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 28), "Last 28 days"),
        get_prob_nap_length(
            DF_aft %>% filter(difftime(Sys.time(), Date, units = "days") <= 90), "Last 90 days")) %>% 
    ggplot(aes(x = time/60, y = prob, colour = subset)) + 
    geom_line() +
    xlab("nap_length")
