library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

pollock_placereceipt <- read_csv("/Users/nataliejonas/Desktop/crabs/all_pollock.csv")

places_standardized <- pollock_placereceipt %>%
  filter(!is.na(`PLACE OF RECEIPT`), `PLACE OF RECEIPT` != "") %>%
  mutate(sovereignt = case_when(
    str_detect(`PLACE OF RECEIPT`, regex("CHINA|QINGDAO|DALIAN|SHANGHAI|YANTAI|NINGBO|SHENZHEN|TIANJIN|RIZHAO|ZHOUSHAN|GUANGZHOU|XIAMEN|LIANYUNGANG", ignore_case = TRUE)) ~ "CHINA",
    str_detect(`PLACE OF RECEIPT`, regex("KOREA|BUSAN|PUSAN|INCHEON", ignore_case = TRUE)) ~ "REPUBLIC OF KOREA",
    str_detect(`PLACE OF RECEIPT`, regex("JAPAN|TOKYO|OSAKA|KOBE|YOKOHAMA", ignore_case = TRUE)) ~ "JAPAN",
    str_detect(`PLACE OF RECEIPT`, regex("VIETNAM|HO CHI MINH|VUNG TAU|HAIPHONG|HANOI", ignore_case = TRUE)) ~ "VIETNAM",
    str_detect(`PLACE OF RECEIPT`, regex("TAIWAN|KEELUNG|KAOHSIUNG|TAIPEI", ignore_case = TRUE)) ~ "TAIWAN",
    str_detect(`PLACE OF RECEIPT`, regex("INDIA|MUNDRA|NHAVA|CHENNAI|KOLKATA|MUMBAI|HAZIRA", ignore_case = TRUE)) ~ "INDIA",
    str_detect(`PLACE OF RECEIPT`, regex("GERMANY|HAMBURG|BREMERHAVEN", ignore_case = TRUE)) ~ "GERMANY",
    str_detect(`PLACE OF RECEIPT`, regex("FINLAND|KOTKA|HELSINKI", ignore_case = TRUE)) ~ "FINLAND",
    str_detect(`PLACE OF RECEIPT`, regex("POLAND|GDANSK|KWIDZYN", ignore_case = TRUE)) ~ "POLAND",
    str_detect(`PLACE OF RECEIPT`, regex("CANADA|VANCOUVER|MONTREAL|TORONTO", ignore_case = TRUE)) ~ "CANADA",
    str_detect(`PLACE OF RECEIPT`, regex("RUSSIA|VLADIVOSTOK|MOSCOW", ignore_case = TRUE)) ~ "RUSSIA",
    str_detect(`PLACE OF RECEIPT`, regex("BRAZIL|SANTOS|VITORIA|RIO DE JANEIRO", ignore_case = TRUE)) ~ "BRAZIL",
    str_detect(`PLACE OF RECEIPT`, regex("PAKISTAN|PORT QASIM|KARACHI", ignore_case = TRUE)) ~ "PAKISTAN",
    str_detect(`PLACE OF RECEIPT`, regex("NORWAY|OSLO|BERGEN", ignore_case = TRUE)) ~ "NORWAY",
    str_detect(`PLACE OF RECEIPT`, regex("NETHERLANDS|ROTTERDAM|AMSTERDAM", ignore_case = TRUE)) ~ "NETHERLANDS",
    str_detect(`PLACE OF RECEIPT`, regex("UNITED STATES|USA|\\bUS\\b|LOS ANGELES|NEW YORK|SEATTLE|CHICAGO|HOUSTON", ignore_case = TRUE)) ~ "UNITED STATES OF AMERICA",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sovereignt)) %>%
  count(sovereignt, name = "shipments")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(sovereignt = str_to_upper(sovereignt)) %>%
  left_join(places_standardized, by = "sovereignt")

labels <- world %>%
  filter(!is.na(shipments)) %>% #keep countries with at least a shipment
  group_by(sovereignt, shipments) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>% #accouning for if a country has islands
  sf::st_centroid() %>%
  mutate(lon = sf::st_coordinates(.)[, 1], lat = sf::st_coordinates(.)[, 2])

ggplot(world) +
  geom_sf(aes(fill = shipments), colour = "white", linewidth = 0.1) +
  geom_label_repel(data = labels,
                   aes(x = lon, y = lat, label = paste0(sovereignt, " (", shipments, ")")),
                   size = 2, fontface = "bold",
                   fill = alpha("white", 0.85), colour = "black",
                   label.padding = unit(0.1, "lines"), label.size = 0.2,
                   min.segment.length = 0, segment.colour = "grey30",
                   max.overlaps = Inf) +
  scale_fill_gradient(low = "#cccccc", high = "#1c1c1c", na.value = "#d9dddc",
                      name = "Shipments", trans = "log10") +
  theme_void() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 15, margin = margin(b = 8)),
    plot.subtitle = element_text(hjust = 0.5, size = 8, color = "#3e424b", margin = margin(b = 6)),
    plot.caption  = element_text(hjust = 1, size = 8, color = "#3e424b", face = "bold")
  ) +
  labs(
    title    = "Where Does Alaskan Pollock End Up?",
    subtitle = "Pollock, fished in Alaska, rarely stays there. The Bering Sea and Gulf of Alaska\n fuel the world's pollock demand. But, the waters have been in crisis for years.",
    caption  = "2025 Data from ImportGenius"
  )

ggsave("~/Desktop/crabs/pollock_receipt_map.pdf", width = 12, height = 6)
