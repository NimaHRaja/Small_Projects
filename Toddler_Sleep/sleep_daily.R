source("init.R")

full_join(
    DF_night %>% select(night_length, Date),
    DF_aft %>% select(nap_length, Date), 
    by = "Date") %>% 
    melt(id.vars = "Date") %>% 
    filter(!is.na(value)) %>%
    # mutate(sleep = factor(variable, levels = c("nap_length", "night_length"))) %>%
    ggplot(aes(x = Date, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "stack")
