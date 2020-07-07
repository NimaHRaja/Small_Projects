source("init.R")


DF_night %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_start_time)) + geom_density(colour = "blue")

DF_night %>% 
    filter(Date != Sys.Date()) %>%
    # filter(Date != '2020-06-17') %>% 
    # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
    ggplot(aes(x = night_start_time)) + geom_density(colour = "blue")

DF_night %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_start_time)) + geom_histogram(fill = "blue")

DF_night %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(y = night_start_time)) + geom_boxplot(fill = "green")

rbind(
    DF_night %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_start_time, colour = subset)) + geom_density()

rbind(
    DF_night %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    filter(Date != Sys.Date()) %>%
    ggplot(aes(y = night_start_time, fill = subset)) + geom_boxplot()



#####


DF_night %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_end_time)) + geom_density(colour = "blue")

DF_night %>% 
    filter(Date != Sys.Date()) %>%
    # filter(Date != '2020-06-17') %>% 
    # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
    ggplot(aes(x = night_end_time)) + geom_density(colour = "blue")

DF_night %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_end_time)) + geom_histogram(fill = "blue")

DF_night %>%
    filter(Date != Sys.Date()) %>%
    ggplot(aes(y = night_end_time)) + geom_boxplot(fill = "green")

rbind(
    DF_night %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    # filter(Date != '2020-06-17') %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(x = night_end_time, colour = subset)) + geom_density()

rbind(
    DF_night %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%    
    # filter(Date != '2020-06-17') %>% 
    filter(Date != Sys.Date()) %>%
    ggplot(aes(y = night_end_time, x = subset, fill = subset)) + geom_boxplot()



######


DF_night %>% 
    ggplot(aes(x = night_length)) + geom_density(colour = "blue")

DF_night %>% filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = night_length)) + geom_density(colour = "blue")

DF_night %>% filter(Date != '2020-06-17') %>% 
    filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
    ggplot(aes(x = night_length)) + geom_density(colour = "blue")

DF_night %>% 
    ggplot(aes(x = night_length)) + geom_histogram(fill = "blue")

DF_night %>% filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = night_length)) + geom_histogram(fill = "blue")

DF_night %>% filter(Date != '2020-06-17') %>% 
    filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
    ggplot(aes(x = night_length)) + geom_histogram(fill = "blue")

DF_night %>% 
    #filter(Date != '2020-06-17') %>% 
    ggplot(aes(y = night_length)) + geom_boxplot(fill = "green")

DF_night %>% 
    #filter(Date != '2020-06-17') %>% 
    filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
    ggplot(aes(y = night_length)) + geom_boxplot(fill = "green")


rbind(
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    ggplot(aes(x = night_length, colour = subset)) + geom_density()

rbind(
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "last 7 days")) %>%    
    filter(Date != '2020-06-17') %>% 
    ggplot(aes(x = subset, y = night_length, fill = subset)) + geom_boxplot()





######


DF_night %>%
    ggplot(aes(x = night_start_time, y = night_end_time)) + 
    geom_point(colour = "red")



DF_night %>% 
    filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
    # filter(Date != Sys.Date()) %>%
    # filter(Date != '2020-06-17') %>% 
    # filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
    ggplot(aes(x = night_start_time, y = night_end_time)) + 
    geom_point(colour = "red")




rbind(
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    ggplot(aes(x = night_start_time, y = night_end_time, label = day_month, colour = subset)) + 
    geom_point()





DF_night %>% 
    filter(Date != Sys.Date()) %>%
    filter(night_start_time >= as.POSIXct("16:00:00", format="%H:%M:%S")) %>% 
    ggplot(aes(x = night_start_time, y = night_end_time, label = day_month)) + 
    geom_point(colour = "green") + 
    geom_text(colour = "red") + 
    geom_smooth(method = "lm")




rbind(
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
        mutate(subset = "last 7 days")) %>%
    ggplot(aes(x = night_start_time, y = night_end_time, colour = subset, label = day_month)) + 
    geom_point() +
    geom_text() + 
    geom_smooth(method = "lm")

######

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
