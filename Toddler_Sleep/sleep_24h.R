source("init.R")

#### Generate all date/times of interest ####

all_dates <- 
    rbind(DF_raw$Start, DF_raw$End, DF_raw$Start2, DF_raw$End2, Sys.time()) %>% 
    as.vector() %>% 
    unique() %>%
    as.POSIXct.numeric( origin = "1970-01-01") %>% 
    c(seq(min(as.Date(DF_raw$Start)), max(as.Date(DF_raw$Start)) + 1, 1) %>% 
          as.POSIXct() + 10*60*60) %>% 
    c(seq(min(as.Date(DF_raw$Start)), max(as.Date(DF_raw$Start)) + 1, 1) %>% 
          as.POSIXct() + 16*60*60)

#### Create DF_24h_flat ####

DF_24h_flat <- 
    expand.grid(all_dates, DF_raw$ID) 

names(DF_24h_flat) <- c("Date", "ID")

DF_24h_flat <- 
    inner_join(DF_raw, DF_24h_flat, by = "ID") %>% 
    filter(Date <= Sys.time())

DF_24h_flat <- 
    DF_24h_flat %>% mutate(value = 
                               ifelse(Date >= End & Date <= Start2, 
                                      difftime(End, Start, units = "hours"), 
                                      ifelse(Date >= Start & Date <= End, 
                                             difftime(Date, Start, units = "hours"),
                                             ifelse(Date >= Start2 & Date <= End2, 
                                                    difftime(End2, Date, units = "hours"),0)))) 

# DF_24h_flat %>% arrange(Date) %>% 
#     ggplot(aes(x = Date, y = value, colour = as.factor(ID))) + geom_line()

#### Create DF_24h_summary ####

DF_24h_summary <- 
    DF_24h_flat %>% 
    group_by(Date) %>% 
    summarise(sleep_24h = sum(value)) 

#### DF_24h_summary Graph ####

DF_24h_summary %>% 
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    arrange(Date) %>% 
    ggplot(aes(x = Date, y = sleep_24h)) + geom_line(colour = "blue")
# DF_raw %>% geom_rect(aes(xmin = Start, xmax = End, ymin = 9, ymax = 12))

DF_24h_summary %>% 
    filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
    filter(difftime(Sys.time(), Date, units = "days") <= 7) %>%
    arrange(Date) %>% 
    ggplot(aes(x = Date, y = sleep_24h)) + geom_line(colour = "blue")


#### 11AM and 5PM graphs (deprecated) ####

# DF_24h_summary %>% 
#     filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
#     filter(Date %>% strftime(format ="%H:%M:%S") %in% c("11:00:00", "17:00:00")) %>%
#     ggplot(aes(x = Date, y = sleep_24h)) + geom_point(colour = "blue")

# DF_24h_summary %>% 
#     filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
#     filter(Date %>% strftime(format ="%H:%M:%S") %in% c("11:00:00", "17:00:00")) %>%
#     ggplot(aes(x = sleep_24h)) + geom_histogram(fill = "blue")

# DF_24h_summary %>% 
#     filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
#     filter(Date %>% strftime(format ="%H:%M:%S") %in% c("11:00:00", "17:00:00")) %>%
#     ggplot(aes(x = sleep_24h)) + geom_density(colour = "blue")

##### Recent/All comparison graphs (deprecated) #####

# rbind(
#     DF_24h_summary %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days"),
#     DF_24h_summary %>%
#         mutate(subset = "all")) %>%
#     filter(difftime(Date, min(Date), units = "days") >= 1) %>% 
#     ggplot(aes(x = sleep_24h, colour = subset)) + geom_density()

# rbind(
#     DF_24h_summary %>%
#         filter(difftime(Sys.time(), Date, units = "days") <= 7) %>% 
#         mutate(subset = "last 7 days"),
#     DF_24h_summary %>%
#         mutate(subset = "all")) %>%
#     filter(difftime(Date, min(Date), units = "days") >= 1) %>%
#     ggplot(aes(x = subset , y = sleep_24h, fill = subset)) + geom_boxplot()

