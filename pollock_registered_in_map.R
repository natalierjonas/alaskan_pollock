library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

df <- read_csv("/Users/nataliejonas/Desktop/crabs/all_pollock.csv")

world_register <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(sovereignt = str_to_upper(sovereignt)) %>%
  left_join(count(df, `SHIPMENT REGISTERED IN`),
            by = c("sovereignt" = "SHIPMENT REGISTERED IN")) %>%
  rename(shipments = n)

top_countries <- world_register %>%
  filter(!is.na(shipments)) %>%
  slice_max(shipments, n = 10) %>%
  group_by(sovereignt, shipments) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_centroid() %>%
  mutate(lon = sf::st_coordinates(.)[, 1], lat = sf::st_coordinates(.)[, 2])

pollock <- ggplot(world_register) +
  geom_sf(aes(fill = shipments), colour = "white", linewidth = 0.1) +
  geom_label_repel(data = top_countries,
                   aes(x = lon, y = lat, label = paste0(sovereignt, " (", shipments, ")")),
                   size = 2, fontface = "bold",
                   fill = alpha("white", 0.85), colour = "black",
                   label.padding = unit(0.1, "lines"), label.size = 0.2,
                   min.segment.length = 0, segment.colour = "grey30",
                   max.overlaps = Inf) +
  scale_fill_gradient(low = "#728fce", high = "#123456", na.value = "#b7c9e2",
                      name = "Shipments", trans = "log10") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 15,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 8,
      color = "#3e424b",
      margin = margin(b = 6)
    ),
    plot.caption = element_text(
      hjust = 1,
      size = 8,
      color = "#3e424b",
      face = "bold"
    )
  ) +
  labs(
    title = "What Countries Are Shipping Alaskan Pollock?",
    subtitle = "Some countries sell the ability to fly their flag to other nations, a tactic used to skirt more stringent fishing regulation.\n Pollock sourced from Alaska, an important global stock, moved around the world on ships from all over the world.",
    caption = "2025 Data from ImportGenius"
  )

pollock

ggsave("~/Desktop/crabs/pollock_map_FOC.pdf", plot = pollock, width = 12, height = 8)

