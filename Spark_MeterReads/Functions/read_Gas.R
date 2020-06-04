gas_data <- 
    readLines(gas_file) %>%
    gsub(pattern = "=", replacement = "") %>%
    readHTMLTable()


DF_gas_raw <- 
    gas_data[[1]] %>%
    filter(!is.na(Date)) %>% 
    filter(!is.na(Reading)) %>% 
    mutate(Date = gsub("\n", "", Date)) %>% 
    mutate(Date = gsub("td>", "", Date)) %>%
    mutate(Date = gsub("d>", "", Date)) %>%
    mutate(Reading = gsub("d>", "", Reading)) %>%
    mutate(Reading = gsub("\n", "", Reading)) %>%
    mutate(Reading = gsub("t", "", Reading)) %>% 
    filter(!grepl("E", Reading)) %>% mutate(Reading = as.numeric(Reading))%>% 
    mutate(Date = as.POSIXct(Date, format = "%b %d, %Y", tz = "GMT")) 


DF_gas <- 
    DF_gas_raw %>% 
    mutate(consumption = -lead(Reading,1) + Reading) %>% 
    mutate(days_between = difftime(Date, lead(Date,1), units = "days") %>% as.numeric()) %>%
    mutate(consumption = consumption/days_between)

DF_gas_clean <- 
    left_join(
        data.frame(Date = seq(min(DF_gas$Date), max(DF_gas$Date), by = "days")), 
        DF_gas, 
        by = "Date") %>% 
    fill(consumption, .direction = "up") %>%
    select(Date, consumption)
