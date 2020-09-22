options(stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)

DF <- read.csv("NightWeaning_Log.csv")

DF <- DF %>% 
    mutate(Start = as.POSIXct(Start, tz = "GMT")) %>%
    mutate(End = as.POSIXct(End, tz = "GMT")) %>% 
    arrange(Start) %>%
    mutate(Date = as.Date(Start) - if_else(Start %>% format("%H") < 12,1,0)) %>%
    mutate(duration = difftime(End, Start, units = "hours") %>% as.numeric())

DF %>% mutate(test = lag(End, 1))%>% filter(Start != test & note != "Start_Sleep") %>% dim()


DF %>% arrange(desc(Start)) %>%
    ggplot(aes(x = Date, y = duration, fill = Description, group = Start)) + 
    geom_col( position = position_stack(reverse = TRUE))



DF <- 
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


DF %>% arrange(desc(Start)) %>%
    ggplot(aes(x = Date, y = duration, fill = Description, group = Start)) + 
    geom_col( position = position_stack(reverse = TRUE))



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

