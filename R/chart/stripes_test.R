library(ggplot)
library(lubridate)
library(tidyverse)

dtset <- tibble(
    date = seq((today()-years(100)), today(), by = 'years')  
  ) %>% 
  mutate( temp = seq(1:nrow(.))/10+rnorm(nrow(.)))

dtset %>% 
  ggplot(aes(x=date, fill=temp, color=temp)) +
  geom_bar(aes(y=1), stat="identity",
           position = position_dodge2(width = 0, padding=0)) +
  scale_fill_gradient2(low="darkblue", mid="lightblue", high="red") +
  scale_color_gradient2(low="darkblue", mid="lightblue", high="red") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.border = element_blank()
  )




