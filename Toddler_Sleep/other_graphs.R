DF_aft %>%
    mutate(recent = difftime(Sys.time(), Date, units = "days") <= 7) %>%
    # filter(Date != '2020-06-17') %>%
    # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
    ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month)) +
    geom_point(colour = "green") +
    geom_text(aes(colour = recent)) +
    geom_smooth(method = "lm") +
    theme(legend.position = "none")

rbind(
    DF_aft %>%
        mutate(subset = "all"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 90) %>%
        mutate(subset = "last 90 days"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 28) %>%
        mutate(subset = "last 28 days"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>%
        mutate(subset = "last 07 days")) %>%
    ggplot(aes(x = nap_start_time, y = nap_end_time, label = day_month, colour = subset)) +
    geom_text()

rbind(
    DF_aft %>%
        mutate(subset = "all"),
    DF_aft %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>%
        mutate(subset = "last 7 days")) %>%
    # filter(Date != '2020-06-17') %>%
    # filter(nap_start_time <= as.POSIXct("16:00:00", format="%H:%M:%S")) %>%
    ggplot(aes(x = nap_start_time, y = nap_end_time, colour = subset, label = day_month)) +
    geom_point() +
    geom_text() +
    geom_smooth(method = "lm")

rbind(
    DF_night %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "all"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 7) %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "last 7 days"),
    DF_night %>%
        filter(difftime(Sys.time(), Date, units = "days") <= 28) %>%
        filter(!(Date == Sys.Date() & strftime(night_start, format="%H:%M:%S") < "16:00:00")) %>%
        mutate(subset = "last 28 days")) %>%
    # filter(Date != '2020-06-17') %>%
    ggplot(aes(x = subset, y = night_length, fill = subset)) + geom_boxplot()

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
