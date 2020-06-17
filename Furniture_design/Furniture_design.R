library(dplyr)
library(ggplot2)

DF <- read.csv(file = "our_furniture.csv")

DF <- DF %>% mutate(x1 = x, 
                    x2 = x + ifelse(V, L, W),
                    y1 = y,
                    y2 = y + ifelse(V, W, L),
                    r  = ifelse(r == "room" | r == "blocker", ".", r))

# jpeg("Sample.jpg")
DF %>% ggplot() +
    geom_rect(mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=as.factor(r)), 
              color="black", alpha = DF$t) + 
    geom_text(aes(x=(x1+x2)/2, y=(y1+y2)/2, label=r), size=4) + 
    scale_fill_manual(values = 
                          c("black", "blue", "green", "red", "yellow", "orange", "brown", "purple"))+ 
    coord_fixed(ratio = 1) +
    theme(legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
# dev.off()
