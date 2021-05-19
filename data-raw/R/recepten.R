#recepten

library(openxlsx)
library(dplyr)
library(tidyr)

dat <- read.xlsx("data-raw/data/recepten.xlsx", sheet = "recepten")


menubase <- dat %>% group_by(genre, naam, recept, tijd, kcal) %>%
  summarize(n = n(),
            .groups = "drop") %>% select(-n)


recepten <- dat %>% select(naam, pp, unit, ingredienten)

producten <- read.xlsx("data-raw/data/recepten.xlsx", sheet = "producten")

producten <- read.xlsx("data-raw/data/recepten.xlsx", sheet = "producten")

andereboodschappen <- read.xlsx("data-raw/data/recepten.xlsx", sheet = "boodschappen") %>%
  mutate(unit = as.character(unit))

save(menubase, file = "data/menubase.rda")
save(menubase, file = "wisselmenu/menubase.rda")
save(recepten, file = "data/recepten.rda")
save(recepten, file = "wisselmenu/recepten.rda")
save(producten, file = "data/producten.rda")
save(producten, file = "wisselmenu/producten.rda")
save(andereboodschappen, file = "data/andereboodschappen.rda")
save(andereboodschappen, file = "wisselmenu/andereboodschappen.rda")
