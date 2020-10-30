source("init.R")

DF <- read.csv("NightWeaning_Log.csv") %>% 
    mutate(Start = as.POSIXct(Start, tz = "GMT")) %>%
    mutate(End = as.POSIXct(End, tz = "GMT")) %>% 
    arrange(Start) %>%
    mutate(Date = as.Date(Start) - if_else(Start %>% format("%H") < 12,1,0)) %>%
    mutate(duration = difftime(End, Start, units = "hours") %>% as.numeric())

NightWeaning_test1 <- 
    DF %>% mutate(test = lag(End, 1))%>% filter(Start != test & note != "Start_Sleep") %>% dim()

NightWeaning_p1 <- 
    DF %>% arrange(desc(Start)) %>%
    ggplot(aes(x = Date, y = duration, fill = Description, group = Start)) + 
    geom_col( position = position_stack(reverse = TRUE))

DF2 <- 
    DF %>% rbind(
        DF %>% filter(note == "Start_Sleep") %>% group_by(Date) %>% summarise(End = min(Start)) %>% 
            ungroup() %>% mutate(Start = as.POSIXct(Date) + 20*60*60) %>% 
            mutate(Description = "Awake") %>% mutate(note = "") %>% 
            mutate(duration = difftime(End, Start, units = "hours") %>% as.numeric()) %>%
            select("Start","End","Description","note","Date","duration"),
        DF %>% group_by(Date) %>% summarise(Start = max(End)) %>% 
            ungroup() %>% mutate(End = as.POSIXct(Date) + (24+9)*60*60) %>% 
            mutate(Description = "Awake") %>% mutate(note = "") %>% 
            mutate(duration = difftime(End, Start, units = "hours") %>% as.numeric()) %>%
            select("Start","End","Description","note","Date","duration"))

NightWeaning_p2 <-
    DF2 %>% arrange(desc(Start)) %>%
    ggplot(aes(x = Date, y = duration, fill = Description, group = Start)) + 
    geom_col( position = position_stack(reverse = TRUE))

NightWeaning_p3 <-
    DF %>% group_by(Date, Description) %>% #, .drop = FALSE) %>% 
    summarise(num_sup = n()) %>% 
    dcast(Date ~ Description, value.var = "num_sup") %>% 
    mutate(num_Supervision = if_else(condition = is.na(Supervision),0,as.numeric(Supervision))) %>%
    ggplot(aes(x = Date, y = num_Supervision)) + geom_bar(fill = "blue", stat = "identity")

NightWeaning_p4 <-
    DF %>% group_by(Date, Description) %>%
    summarise(len_sup = sum(duration)) %>% 
    dcast(Date ~ Description, value.var = "len_sup") %>%
    mutate(time_supervision = if_else(condition = is.na(Supervision),0,as.numeric(Supervision))) %>%
    ggplot(aes(x = Date, y = time_supervision)) + geom_point(colour = "blue")

NightWeaning_p5 <-
    DF %>% 
    filter(Description == "Sleep") %>% 
    group_by(Date) %>% 
    summarise(max_uninterupted = max(duration)) %>%
    ggplot(aes(x = Date, y = max_uninterupted)) + 
    geom_point(colour = "blue") + ylim(c(0,12))

NightWeaning_p6 <-
    DF %>% 
    filter(Description == "Sleep") %>% 
    group_by(Date) %>% 
    summarise(max_uninterupted = max(duration)) %>%
    ggplot(aes(x = max_uninterupted)) +
    geom_histogram(binwidth = 0.5, fill = "blue")

NightWeaning_p7 <- 
    DF %>% 
    group_by(Date, Description) %>%
    summarise(tot_time = sum(duration)) %>%
    ungroup() %>%
    mutate(tot_time = if_else(Description == "Sleep", tot_time, -tot_time)) %>%
    ggplot(aes(x = Date, y = tot_time, fill = Description)) + geom_bar(stat = "identity")

NightWeaning_p8 <- 
    DF %>% 
    group_by(Date, Description) %>%
    summarise(tot_time = sum(duration)) %>%
    ungroup() %>%
    mutate(tot_time = if_else(Description == "Supervision", -tot_time, tot_time)) %>%
    ggplot(aes(x = Date, y = tot_time, fill = Description)) + geom_bar(stat = "identity")

inner_join(
    DF2 %>% mutate(join_value = 1),
    data.frame(time = (seq(0:(60*13))*60 - 5*60*60 - 60), join_value = 1),
    by = "join_value") %>%
    mutate(time2 = time + 24*60*60 + as.POSIXct(Date)) %>% 
    mutate(time2 = time2 + if_else(Date > as.Date('2020-10-24'), -60*60,0)) %>%
    mutate(difftime(time2, Start)) %>%
    filter(time2 >= (Start - 3600) & time2 < (End - 3600)) %>%
    group_by(time, Description) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    dcast(time ~ Description, value.var = "count") %>%
    replace(is.na(.), 0) %>% 
    mutate(prob_supervision = Supervision/(Supervision + Sleep + Awake + Co_Sleep)) %>%
    mutate(time = as.POSIXct(Sys.Date()) + time) %>% 
    ggplot(aes(x = time, y = prob_supervision)) + geom_line(colour = "blue")


# library(ggplot2); library(scales)
# datetime <- data.frame(date = c("1/1/2015", "1/1/2015", "2/1/2015", "3/1/2015"), 
#                        time = c("2:00:00","3:00:00", "5:00:00", "2:00:00"),
#                        type = c("a", "b","a", "b"))
# datetime$date <- as.Date(datetime$date, format="%m/%d/%Y")
# datetime$time <- as.POSIXct(datetime$time, format="%H:%M:%S")
# datetime %>%
# ggplot(aes(x = date, y = time, fill = type)) +
#     geom_bar(stat = "identity") +
#     xlab("Day") +
#     ylab("Time") +
#     coord_cartesian(ylim=c(as.POSIXct("0:0:0", format="%H:%M:%S"),
#                            as.POSIXct("23:59:59", format="%H:%M:%S"))) +
#     scale_y_datetime(breaks = date_breaks("1 hour"),
#                      labels = date_format("%H:%M")) +
#     scale_x_date(breaks = pretty_breaks(10))

