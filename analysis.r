
library(readr)
sector_data <- read_csv("cleaned_sector_data.csv")
per_capita_income_data <- read_csv("cleaned_per_capita_income_data.csv")


View(sector_data)
View(per_capita_income_data)

per_capita_income_data <- per_capita_income_data %>%
  filter(STATE_LEVEL1 != "Union Territories" | STATE == "Delhi" | STATE == "Jammu & Kashmir")

sector_data <- sector_data %>%
  filter(STATE != "Delhi" & STATE != "Jammu & Kashmir" & STATE != "Puducherry" & STATE != "Chandigarh" & STATE != "Andaman and Nicobar")


# add "total_value_actuals" state-wise, sector-wise and store it in a new column
sector_data <- sector_data %>%
  group_by(STATE, `CLASSIFICATION BASED ON SECTOR`) %>%
  mutate(state_sector_total = sum(as.numeric(total_value_actuals), na.rm = TRUE)) %>%
  ungroup()

# plot year wise "total_value_actuals" for each sector for each state
ggplot(sector_data, aes(x = YEAR, y = as.numeric(total_value_actuals), color = `CLASSIFICATION BASED ON SECTOR`)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ STATE, scales = "free_y") +
  labs(title = "Year-wise Total Value Actuals by Sector and State",
       x = "Year",
       y = "Total Value Actuals",
       color = "Sector") +
  theme_minimal() +
  theme(legend.position = "bottom")


# total_value_actuals plot for karnataka
ggplot(
  subset(sector_data, STATE == "Karnataka"),
  aes(
    x = YEAR,
    y = as.numeric(total_value_actuals),
    color = `CLASSIFICATION BASED ON SECTOR`
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = paste("Year-wise Total Value Actuals by Sector — ", "Karnataka"),
    x = "Year",
    y = "Total Value Actuals",
    color = "Sector"
  ) +
  scale_color_manual(values = c("green", "white", "orange")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.line = element_line(color = "white"),
    axis.ticks = element_line(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.position = "bottom"
  )

# extract dataframe of unique states from sector_data
unique_states <- unique(sector_data$STATE)

# install.packages("svglite")
library(svglite)
# draw total_value_actuals plot for each state in unique_states
# for (state in unique_states) {
#   p <- ggplot(
#     subset(sector_data, STATE == state),
#     aes(
#       x = YEAR,
#       y = as.numeric(total_value_actuals),
#       color = `CLASSIFICATION BASED ON SECTOR`
#     )
#   ) +
#     geom_line() +
#     geom_point() +
#     labs(
#       title = paste("Year-wise Total Value Actuals by Sector — ", state),
#       x = "Year",
#       y = "Total Value Actuals",
#       color = "Sector"
#     ) +
#     scale_color_manual(values = c("green", "white", "orange")) +
#     scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
#     theme_void() +
#     theme(
#       plot.background = element_rect(fill = "black", color = NA),
#       panel.background = element_rect(fill = "black", color = NA),
#       legend.background = element_rect(fill = "black", color = NA),
#       legend.key = element_rect(fill = "black", color = NA),
#       axis.title = element_text(color = "white"),
#       axis.text = element_text(color = "white"),
#       axis.line = element_line(color = "white"),
#       axis.ticks = element_line(color = "white"),
#       plot.title = element_text(color = "white", hjust = 0.5),
#       legend.title = element_text(color = "white"),
#       legend.text = element_text(color = "white"),
#       legend.position = "bottom"
#     )
  
#   print(p)
#   # save each image as png
#   ggsave(paste0("total_value_actuals_", state, ".png"), plot = p, width = 10, height = 6)
# }

# histogram of CAGR % statewise
state_cagr <- per_capita_income_data %>%
  group_by(STATE) %>%
  summarise(`CAGR(%)` = dplyr::last(na.omit(`CAGR(%)`))) %>%
  ungroup()

# sort by CAGR for nicer plotting
state_cagr <- state_cagr %>%
  arrange(`CAGR(%)`)

# Plot a bar chart (horizontal)
ggplot(state_cagr, aes(x = reorder(STATE, `CAGR(%)`), y = `CAGR(%)`)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "CAGR(%) of Per Capita Income by State (2013–2024)",
    x = "State",
    y = "CAGR(%)"
  ) +
  theme_minimal()



# pie chart of sector wise contribution in total_value_actuals
sector_contribution <- sector_data %>%
  group_by(`CLASSIFICATION BASED ON SECTOR`) %>%
  summarise(total_contribution = sum(as.numeric(total_value_actuals), na.rm = TRUE)) %>%
  ungroup()


sector_year <- read_csv("cleaned_sector_data.csv") # has STATE, CLASSIFICATION BASED ON SECTOR, YEAR, total_value_actuals
sector_year <- sector_year %>%
mutate(total_value_actuals = as.numeric(total_value_actuals))

state_sector_totals <- sector_year %>%
group_by(STATE, `CLASSIFICATION BASED ON SECTOR`) %>%
summarise(total_value_actuals = sum(total_value_actuals, na.rm = TRUE), .groups = "drop")

state_sector_share <- state_sector_totals %>%
group_by(STATE) %>%
mutate(state_total = sum(total_value_actuals, na.rm = TRUE),
pct = ifelse(state_total > 0, total_value_actuals / state_total, NA_real_)) %>%
ungroup()

ggplot(
state_sector_share,
aes(x = "", y = pct, fill = `CLASSIFICATION BASED ON SECTOR`)
) +
geom_bar(width = 1, stat = "identity", position = "fill") +
coord_polar(theta = "y") +
facet_wrap(~ STATE) +
labs(
title = "Sector-wise Contribution to Total Value Actuals by State",
x = NULL,
y = NULL,
fill = "Sector"
) +
theme_void() +
theme(
legend.position = "bottom",
strip.text = element_text(face = "bold")
)





# draw and save pie chart for each state in unique_states

sector_year <- read_csv("cleaned_sector_data.csv") # has STATE, CLASSIFICATION BASED ON SECTOR, YEAR, total_value_actuals
sector_year <- sector_year %>%
mutate(total_value_actuals = as.numeric(total_value_actuals))

state_sector_totals <- sector_year %>%
group_by(STATE, `CLASSIFICATION BASED ON SECTOR`) %>%
summarise(total_value_actuals = sum(total_value_actuals, na.rm = TRUE), .groups = "drop")


state_sector_share <- state_sector_totals %>%
group_by(STATE) %>%
mutate(state_total = sum(total_value_actuals, na.rm = TRUE),
pct = ifelse(state_total > 0, total_value_actuals / state_total, NA_real_)) %>%
ungroup()

for (st in unique_states) {
  # Filter data for the current state
  state_data <- state_sector_share %>% filter(STATE == st)
  
  # Create the plot for the current state
p <- ggplot(state_data, aes(x = "", y = pct, fill = `CLASSIFICATION BASED ON SECTOR`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  labs(
    title = paste("Sector-wise Contribution in", st),
    x = NULL,
    y = NULL,
    fill = "Sector"
  ) +
  theme_void() +
  theme(
  legend.position = "bottom",
  plot.title = element_text(
    size = 16, 
    face = "bold", 
    hjust = 0.5,
    margin = margin(t = 25, b = 20)  # top and bottom margin
  )
)
  
  # Save the plot - adjust path and filename as needed
  ggsave(
    filename = paste0("sector_pie_", gsub(" ", "_", st), ".png"),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300
  )
}



  
#   # Create the plot for the current state
# p <- ggplot(state_data, aes(x = "", y = pct, fill = `CLASSIFICATION BASED ON SECTOR`)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar(theta = "y") +
#   geom_text(
#     aes(label = scales::percent(pct, accuracy = 1)),
#     position = position_stack(vjust = 0.5),
#     size = 4,
#     color = "white",
#     fontface = "bold"
#   ) +
#   labs(
#     title = paste("Sector-wise Contribution for", "Andaman and Nicobar"),
#     x = NULL,
#     y = NULL,
#     fill = "Sector"
#   ) +
#   theme_void() +
#   theme(
#   legend.position = "bottom",
#   plot.title = element_text(
#     size = 16, 
#     face = "bold", 
#     hjust = 0.5,
#     margin = margin(t = 25, b = 20)  # top and bottom margin
#   )
# )
  
#   # Save the plot - adjust path and filename as needed
#   ggsave(
#     filename = paste0("sector_pie_", gsub(" ", "_", "Karnataka"), ".png"),
#     plot = p,
#     width = 6,
#     height = 6,
#     dpi = 300
#   )
