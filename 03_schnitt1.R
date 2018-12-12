# 02 schnitt1.R: Berechnet die Durchschnitte

# setwd('~/Dropbox/Signal&Rauschen/06_Daten & Visualisierung/')  # Arndt Pfad
# setwd('~/Git/signalundrauschen')

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

df <- read.csv('daten/umfragedaten.csv', stringsAsFactors = F) %>% tbl_df()

df$monat <- month(df$datum) # Monat der Umfrage

df$woche <- week(df$datum) # Woche der Umfrage

# 2016-2017-2018 
wochenschnitt20162017 <- df %>% filter(!is.na(jahr), !is.na(woche)) %>%
  subset(jahr == 2018 | jahr == 2017 | jahr == 2016) %>%
  group_by(jahr, woche, partei) %>%
  summarise(stimmanteil = mean(stimmanteil, na.rm = T)) %>%
  filter(!is.na(stimmanteil)) %>%
  mutate(date = as.Date(paste(jahr, woche, 1, sep="-"), "%Y-%U-%u"),
         partei = recode(partei, afd = 'AfD', cdu_csu = 'CDU/CSU',
                         fdp = 'FDP', gruene = "Green",
                         linke_pds = 'Left', piraten = 'Piraten',
                         sonstige = 'Sonstige', spd = 'SPD')) %>%
  filter(!is.na(date)) %>%
  spread(partei, stimmanteil) %>% ungroup() %>%
  select(date, `CDU/CSU`, SPD, `Left`, AfD, `Green`,
         FDP)
# 
write.csv(wochenschnitt20162017, 'daten/schnitt20162017.csv', row.names = F)
# 
# # Viz
long <- reshape2::melt(wochenschnitt20162017, id="date")
ggplot(data=long,
  aes(x=date, y=value, colour=variable)) +
  geom_line()



