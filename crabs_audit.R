library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(scales)

setwd('~/Desktop/crabs')
pollock_2025 <- read_csv("pollock_2025.csv")
view(pollock_2025)

#csv contains all pollock mentions in export data from Jan 20 to end of 2025 season in late September, 2025.
# just exploring here
pollock_filtered <- pollock_2025 %>%
  select(-c("CARGO SEQUENCE NO.", "REGULAR COUNTRY CODE", "SHIPPER COUNTRY CODE", "CONTAINER NUMBER")) %>%
  filter(
    str_detect(str_trim(`PRODUCT DESCRIPTION`),
               regex("pollock", ignore_case = TRUE)) &
      `USCS PORT` %in% c("Anchorage, Alaska",
                         "Seattle, Washington",
                         "Tacoma, Washington")
  )

pollock_filtered <- pollock_filtered %>%
  distinct(`BILL OF LADING`, .keep_all = TRUE)

view(pollock_filtered) #confirming no duplicate "bill of landings"

# filtered to make sure, removed 3
# 95% of pollock is fished from the Bering Sea Aleutian Islands and often goes straight to its intended target,
# Dutch Harbor is not a recognized USCS port, however
#filtering for only Anchorage, Seattle and Tacoma only removed 30 rows.
# this is because fleets that operate in bering sea are based in these places, but the fishing
# only actually occurs in the Bering Sea/Gulf of Alaska

pollock_filtered %>%
  summarise(total = sum(`SHIPPING WEIGHT (KG)`, na.rm = TRUE))

pollock_filtered %>%
  count(`FOREIGN PORT`, sort = TRUE) %>%
  slice_head(n = 5)

#china, Korea and "Tonosi", a tugboat? with a foreign port registration?

pollock_filtered %>%
  count(`VESSEL NAME`, sort = TRUE) %>%
  slice_head(n = 10)

#top 7 all flying under maltese flags, 8/10 Malta flags, 1 Portugal and 1 Panama
#although incredibly likely all 133 million+ were fished in the bering sea, lets be more 
# specific just to see

pollock_alaska <- pollock_filtered %>%
  filter(
    str_detect(`PRODUCT DESCRIPTION`,
               regex("Alaska Pollock|Alaskan Pollock", ignore_case = TRUE))
  )

view(pollock_alaska)

pollock_alaska %>%
  summarise(total = sum(`SHIPPING WEIGHT (KG)`, na.rm = TRUE))

#still, 55,262,440 (although likely many don't say "Alaska" or "Alaskan" but are from there)

pollock_alaska %>%
  count(`FOREIGN PORT`, sort = TRUE) %>%
  slice_head(n = 5)

all_pollock <- read_csv("all_pollock.csv")
view(all_pollock)

gross_by_port <- all_pollock %>%
  group_by(`FOREIGN PORT`) %>%
  summarise(total_gross_weight = sum(`GROSS WEIGHT (KG)`, na.rm = TRUE)) %>%
  arrange(desc(total_gross_weight)) %>%
  slice_head(n = 5) 

port_gross <- ggplot(gross_by_port, aes(x = reorder(`FOREIGN PORT`, total_gross_weight),
                          y = total_gross_weight)) +
  geom_col(fill = "#728fce") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Where Most Alaskan Pollock Ends Up",
    x = "Foreign Port",
    y = "Total Gross Weight (KG)"
  ) +
  theme_minimal()

all_pollock %>%
  summarise(total = sum(`GROSS WEIGHT (KG)`, na.rm = TRUE))


ggsave("~/Desktop/crabs/top_port_map.pdf", plot = port_gross, width = 12, height = 6)

