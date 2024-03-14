library(tidyverse)
library(writexl)
library(readxl)

# Relevante Spalten auswählen und als Excel-Datei abspeichern
ja2 <- read_tsv("data/ja2.txt")
ja2_bearbeitet <- ja2 %>% 
  select(kontext_vor, ja, kontext_nach)

write_xlsx(ja2_bearbeitet, "data/ja2_bearbeitet.xlsx")

# Excel-Datei einlesen
ja <- read_excel("C:/01_Home_Dir/01_Lora/Uni/Master Linguistik Freiburg/1. Semester/Zweitspracherwerbstheorien/Hausarbeit/data/ja2_bearbeitet.xlsx")

# Bereinigung: ja als nicht-Modalpartikel herausfiltern
ja_clean <- ja %>% 
  filter(modpartikel == 1) 

# Häufigkeit der Illokutionstypen errechnen
ja_clean_perc <- ja_clean %>% 
  count(illokutionstyp) %>% 
  add_count(wt = n) %>% 
  mutate(perc = round(n/nn * 100, digits=2)) %>% 
  view()

# Reihenfolge der Spalten bearbeiten und bessere Labels hinzufügen
ja_clean <- ja_clean %>%
  mutate(funktion_ges_new = factor(funktion_ges, 
                               levels = c(1, 2, 6, 7, 3, 4, 5, 8, 9, 0),  
                               labels = c("Bekanntheit", "Staunen", "Empörung", "Widerspruch/Vorwurf", "Willensvertärkung", "Freundlichkeit", "Überraschung", "Mitgefühl", "andere", "unbestimmt")))


ja_clean$funktion_ges <- as.factor(ja_clean$funktion_ges)

ja_clean_perc <- ja_clean %>% 
  count(funktion_ges) %>% 
  add_count(wt = n) %>% 
  mutate(perc = round(n/nn * 100, digits=2))

joined_data <- left_join(ja_clean, ja_clean_perc, by = "funktion_ges")

# Häufigkeit der Funktionstypen
plot <- joined_data %>% 
  mutate(perc = round(perc, digits = 1), 
         perc_label = str_c(perc, "%")) %>%
  ggplot() + 
  aes(x = funktion_ges_new, fill = funktion_ges_new) + 
  geom_bar() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal() +
  labs(x = "Funktion", 
       y = "Häufigkeit",
       title = "Funktionen der Modalpartikel ja", 
       fill = "Funktionen") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = perc_label, y = n),
            position = position_dodge(width = 1), 
            vjust = 0,
            size = 3) +
  theme(axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

ggsave("ja_funktionen_high_res_new.png", plot, width = 8, height = 6, dpi = 600)

# Bereinigten Datensatz zusammenstellen und als Excel-Datei abspeichern
# Spalten:
# modpartikel: 0 = keine Modalpartikel, 1 Modalpartikel
# illokutionstyp: 0 = anderer, 1 = Aussage, 2 = emphatische Aussage, 3 = Exklamativsatz
# n : absolute Häufigkeit
# perc : relative Häufigkeit
ja_bereinigt <- joined_data %>%
  select(kontext_vor, ja, kontext_nach, modpartikel, illokutionstyp, funktion_ges_new, n, perc) %>% 
  view()

write_xlsx(ja_bereinigt, path = "data/ja_bereinigt.xlsx", col_names = TRUE)


# Verschiedene Teildatensätze anzeigen lassen
ja_clean %>% 
  filter(illokutionstyp == 2) %>% 
  view()

joined_data %>% 
  filter(funktion_ges_new == "andere") %>% 
  view()

