# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read in datasets
percapitaincomedata <- read.csv("cleaned_per_capita_income_data.csv", stringsAsFactors = FALSE)
sectordata <- read.csv("cleaned_sector_data.csv", stringsAsFactors = FALSE)

colnames(percapitaincomedata)
colnames(sectordata)

# Prepare CAGR dataframe by state
statecagr <- percapitaincomedata %>%
  group_by(STATE) %>%
  summarise(
    CAGR = last(na.omit(`CAGR...`)),
    .groups = "drop"
  ) %>%
  arrange(CAGR)

# Prepare sector share data by state
statesectorshare <- sectordata %>%
  filter(`CLASSIFICATION.BASED.ON.SECTOR` %in% c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")) %>%
  group_by(STATE, `CLASSIFICATION.BASED.ON.SECTOR`) %>%
  summarise(total_value_actuals = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop") %>%
  group_by(STATE) %>%
  mutate(pct = total_value_actuals / sum(total_value_actuals) * 100) %>%
  ungroup()

# Define colors
cagr_colors <- c("Top 10" = "darkgreen", "Middle States" = "gray70", "Bottom 10" = "firebrick")
sector_colors <- c("PRIMARY SECTOR" = "brown", "SECONDARY SECTOR" = "blue", "TERTIARY SECTOR" = "green")

# 1. Horizontal Bar Chart of CAGR of Per Capita Income by State
statecagr <- statecagr %>%
  mutate(
    tier = case_when(
      CAGR >= sort(CAGR, decreasing = TRUE)[10] ~ "Top 10",
      CAGR <= sort(CAGR)[10] ~ "Bottom 10",
      TRUE ~ "Middle States"
    ),
    tier = factor(tier, levels = c("Top 10", "Middle States", "Bottom 10"))
  )

ggplot(statecagr, aes(x = reorder(STATE, CAGR), y = CAGR, fill = tier)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Top 10" = "orange", "Middle States" = "grey", "Bottom 10" = "#159a0c")) +
  labs(
    title = "CAGR of Per Capita Income by State (2013-2024)",
    x = "State",
    y = "CAGR (%)",
    fill = "Performance Tier"
  ) +
  theme_minimal()

# 2. Stacked Horizontal Bar Chart of Sector Composition for Top 10 and Bottom 10 States
top_bottom_states <- statecagr %>%
  arrange(desc(CAGR)) %>%
  slice(c(1:10, (n() - 9):n())) %>%
  pull(STATE)

sector_share_top_bottom <- statesectorshare %>%
  filter(STATE %in% top_bottom_states) %>%
  mutate(CLASSIFICATION_BASED_ON_SECTOR = factor(CLASSIFICATION.BASED.ON.SECTOR, 
                                                levels = c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")))

ggplot(sector_share_top_bottom, aes(x = reorder(STATE, -pct), y = pct, fill = CLASSIFICATION_BASED_ON_SECTOR)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = sector_colors) +
  labs(
    title = "Sectoral Composition of Top 10 and Bottom 10 States by CAGR",
    x = "State",
    y = "Sector Share (%)",
    fill = "Sector"
  ) +
  theme_minimal()

# 3.1 Grouped Bar Chart for Himalayan States Sector Shares
himalayan_states <- c("Sikkim", "Jammu Kashmir", "Arunachal Pradesh", "Himachal Pradesh", "Uttarakhand")

region_data <- sectordata %>%
  filter(STATE %in% himalayan_states,
         CLASSIFICATION.BASED.ON.SECTOR %in% c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")) %>%
  group_by(STATE, CLASSIFICATION.BASED.ON.SECTOR) %>%
  summarise(total = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop") %>%
  group_by(STATE) %>%
  mutate(pct = total / sum(total) * 100) %>%
  ungroup()

ggplot(region_data, aes(x = STATE, y = pct, fill = CLASSIFICATION.BASED.ON.SECTOR)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  scale_fill_manual(values = sector_colors) +
  labs(
    title = "Sectoral Shares for Himalayan States",
    x = "State",
    y = "Percentage",
    fill = "Sector"
  ) +
  theme_minimal()


# 3.2 Northern Plains
northern_plains_states <- c("Punjab", "Haryana", "Uttar Pradesh", "Bihar", "Delhi")

northern_plains_df <- sectordata %>%
  filter(STATE %in% northern_plains_states &
         `CLASSIFICATION.BASED.ON.SECTOR` %in% c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")) %>%
  group_by(STATE, `CLASSIFICATION.BASED.ON.SECTOR`) %>%
  summarise(sector_total = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop") %>%
  group_by(STATE) %>%
  mutate(pct = sector_total / sum(sector_total) * 100) %>%
  ungroup()

ggplot(northern_plains_df, aes(x = STATE, y = pct, fill = `CLASSIFICATION.BASED.ON.SECTOR`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("PRIMARY SECTOR" = "brown", "SECONDARY SECTOR" = "blue", "TERTIARY SECTOR" = "green")) +
  labs(title = "Sectoral Shares: Northern Plains",
       x = "State", y = "Percentage", fill = "Sector") +
  theme_minimal()


# 3.3 Arid and Semi-Arid
arid_semi_arid_states <- c("Rajasthan", "Gujarat", "Madhya Pradesh", "Chhattisgarh", "Jharkhand")

arid_semi_arid_df <- sectordata %>%
  filter(STATE %in% arid_semi_arid_states &
         `CLASSIFICATION.BASED.ON.SECTOR` %in% c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")) %>%
  group_by(STATE, `CLASSIFICATION.BASED.ON.SECTOR`) %>%
  summarise(sector_total = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop") %>%
  group_by(STATE) %>%
  mutate(pct = sector_total / sum(sector_total) * 100) %>%
  ungroup()

ggplot(arid_semi_arid_df, aes(x = STATE, y = pct, fill = `CLASSIFICATION.BASED.ON.SECTOR`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("PRIMARY SECTOR" = "brown", "SECONDARY SECTOR" = "blue", "TERTIARY SECTOR" = "green")) +
  labs(title = "Sectoral Shares: Arid & Semi-Arid States",
       x = "State", y = "Percentage", fill = "Sector") +
  theme_minimal()


# 3.4 Deccan, Coastal, Peninsular
deccan_peninsular_states <- c("Maharashtra", "Goa", "Karnataka", "Andhra Pradesh", "Telangana", "Tamil Nadu", "Kerala")

deccan_peninsular_df <- sectordata %>%
  filter(STATE %in% deccan_peninsular_states &
         `CLASSIFICATION.BASED.ON.SECTOR` %in% c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")) %>%
  group_by(STATE, `CLASSIFICATION.BASED.ON.SECTOR`) %>%
  summarise(sector_total = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop") %>%
  group_by(STATE) %>%
  mutate(pct = sector_total / sum(sector_total) * 100) %>%
  ungroup()

ggplot(deccan_peninsular_df, aes(x = STATE, y = pct, fill = `CLASSIFICATION.BASED.ON.SECTOR`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("PRIMARY SECTOR" = "brown", "SECONDARY SECTOR" = "blue", "TERTIARY SECTOR" = "green")) +
  labs(title = "Sectoral Shares: Deccan, Coastal, Peninsular",
       x = "State", y = "Percentage", fill = "Sector") +
  theme_minimal()


# 3.5 North-Eastern States
north_eastern_states <- c("Assam", "Arunachal Pradesh", "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Tripura", "Sikkim")

north_eastern_df <- sectordata %>%
  filter(STATE %in% north_eastern_states &
         `CLASSIFICATION.BASED.ON.SECTOR` %in% c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")) %>%
  group_by(STATE, `CLASSIFICATION.BASED.ON.SECTOR`) %>%
  summarise(sector_total = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop") %>%
  group_by(STATE) %>%
  mutate(pct = sector_total / sum(sector_total) * 100) %>%
  ungroup()

ggplot(north_eastern_df, aes(x = STATE, y = pct, fill = `CLASSIFICATION.BASED.ON.SECTOR`)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("PRIMARY SECTOR" = "brown", "SECONDARY SECTOR" = "blue", "TERTIARY SECTOR" = "green")) +
  labs(title = "Sectoral Shares: North-Eastern States",
       x = "State", y = "Percentage", fill = "Sector") +
  theme_minimal()



# 4. Scatter Plot of CAGR vs Average Tertiary Sector Percentage by Region
# Calculate sector share percentage by state
sectordata_pct <- sectordata %>%
  group_by(STATE) %>%
  mutate(
    totalvalueactuals = as.numeric(total_value_actuals),
    pct = total_value_actuals / sum(total_value_actuals, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Calculate average tertiary sector percentage per state
tertiary_share <- sectordata_pct %>%
  filter(`CLASSIFICATION.BASED.ON.SECTOR` == "TERTIARY SECTOR") %>%
  group_by(STATE) %>%
  summarise(
    avg_tertiary_pct = mean(pct, na.rm = TRUE),
    .groups = "drop"
  )

scatter_data <- merge(statecagr, tertiary_share, by = "STATE")

colnames(statecagr)
colnames(tertiary_share)
library(dplyr)

# Calculate total GSDP per state by summing totalvalueactuals across all sectors
total_gsdp_df <- sectordata %>%
  group_by(STATE) %>%
  summarise(total_gsdp = sum(as.numeric(total_value_actuals), na.rm = TRUE), .groups = "drop")

# Assuming statecagr already exists with columns STATE and CAGR

# Merge CAGR and total_gsdp into a single dataframe
statecagr_extended <- merge(statecagr, total_gsdp_df, by = "STATE")

# Merge with tertiary_share to get avg_tertiary_pct as well
scatter_data <- merge(statecagr_extended, tertiary_share, by = "STATE")

# Now scatter_data has all necessary columns for plotting


ggplot(scatter_data, aes(x = avg_tertiary_pct, y = CAGR, color = STATE, size = total_gsdp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "CAGR vs Tertiary Sector Percentage by Region",
    x = "Average Tertiary Sector Percentage",
    y = "CAGR (%)",
    color = "Region",
    size = "Total GSDP"
  ) +
  theme_minimal()

# 5. Heat Map of Sectoral Composition by State
heat_data <- statesectorshare %>%
  select(STATE, CLASSIFICATION.BASED.ON.SECTOR, pct) %>%
  pivot_wider(names_from = CLASSIFICATION.BASED.ON.SECTOR, values_from = pct, values_fill = 0) %>%
  arrange(desc(statecagr$CAGR[match(STATE, statecagr$STATE)])) %>%
  mutate(STATE = factor(STATE, levels = STATE)) %>%
  pivot_longer(cols = c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR"), 
               names_to = "Sector", values_to = "Percentage")

ggplot(heat_data, aes(x = Sector, y = STATE, fill = Percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(
    title = "Heat Map of Sectoral Composition by State",
    x = "Sector",
    y = "State",
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
