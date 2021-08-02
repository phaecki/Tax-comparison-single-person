# Pakete laden
library(tidyverse)
library(readr)
library(kableExtra)
library(knitr)

# CSV-Datei in Variable laden
steuern <- read_delim("https://www.daten-analyse.ch/r-data/steuervergleich/steuervergleich.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Mittelwert
mean_steuern <- mean(steuern$Total.Steuern)

# Steuerbetrag als Tabelle ausgeben, alphabetisch sortiert nach Kanton
steuerbetrag <- steuern %>% 
  select(Kanton, Total.Steuern) %>% 
  mutate(Total.Steuern = format(Total.Steuern, big.mark = "'")) %>% 
  rename('Steuerbetrag in CHF' = Total.Steuern)

kbl(steuerbetrag) %>% 
  kable_styling(full_width = FALSE, position = "left") %>%
  column_spec(1, width = "18em") %>%
  column_spec(2, width = "12em")

# Steuerbetrag als Tabelle ausgeben, sortiert nach Steuerbelastung
steuerbetrag <- steuerbetrag %>% 
  arrange(`Steuerbetrag in CHF`)

kbl(steuerbetrag) %>% 
  kable_styling(full_width = FALSE, position = "left") %>%
  column_spec(1, width = "18em") %>%
  column_spec(2, width = "12em")

# Steuerbetrag als Plot ausgeben
steuern %>% 
  ggplot(aes(reorder(Kantonskuerzel, Total.Steuern), Total.Steuern)) +
  geom_point() +
  labs(x = "Kantone",
       y = "Steuerbetrag in CHF") +
  theme_linedraw() +
  ylim(0, 16000) +
  geom_hline(yintercept = mean_steuern, size = 0.5, linetype = "dashed", 
             colour = "red") +
  annotate("text", x = 5.2, y = 11700, size = 4, hjust = 0, 
           label = "Schweizer\nDurchschnitt")