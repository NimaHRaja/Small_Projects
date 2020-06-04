electricity_data <- 
    readLines(electricity_file) %>%
    gsub(pattern = "=", replacement = "") %>%
    readHTMLTable()


DF_electricity_raw <- 
    electricity_data[[1]] %>%
    filter(!is.na(Date)) %>% 
    filter(!is.na(Reading)) %>% 
    mutate(Date = gsub("\n", "", Date)) %>% 
    mutate(Date = gsub("td>", "", Date)) %>%
    mutate(Reading = gsub("d>", "", Reading)) %>%
    mutate(Reading = gsub("\n", "", Reading)) %>% 
    filter(!grepl("E", Reading)) %>% mutate(Reading = as.numeric(Reading))%>% 
    mutate(Date = as.POSIXct(Date, format = "%b %d, %Y", tz = "GMT")) 


DF_electricity <- 
    DF_electricity_raw %>% 
    mutate(consumption = -lead(Reading,1) + Reading) %>% 
    mutate(days_between = difftime(Date, lead(Date,1), units = "days") %>% as.numeric()) %>%
    mutate(consumption = consumption/days_between)

DF_electricity_clean <- 
    left_join(
        data.frame(Date = seq(min(DF_electricity$Date), max(DF_electricity$Date), by = "days")), 
        DF_electricity, 
        by = "Date") %>% 
    fill(consumption, .direction = "up") %>%
    select(Date, consumption)
