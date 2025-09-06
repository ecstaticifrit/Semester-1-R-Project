library(ggplot2) # for data visualization
library(readxl) # to read Excel files
library(dplyr) # for data wrangling
library(tidyr) # for reshaping data
library(stringr) # for cleaning text
library(janitor) # for cleaning column names

data <- read_excel("Components and Economic Activity of Domestic Products at Factor Cost - State Wise-(COMP_ECO_ACT_NSDP.xlsx")

# delete all rows where value of STATE column is equal to "Delhi" or "Jammu & Kashmir"
# data <- data %>%
#   filter((STATE %in% c("Delhi", "Jammu & Kashmir") & STATE_LEVEL1 %in% c("Union Territories")) | !STATE_LEVEL1 %in% c("Union Territories"))

# delete all rows where value of CLASSIFICATION BASED ON INDUSTRY column is equal to "NSVA at basic prices"
data <- data %>%
  filter(`CLASSIFICATION BASED ON INDUSTRY` != "NSVA at basic prices")

# aggregate industry values into sectors
# Primary Sector: Mining & Quarrying, Agriculture, Forestry and Fishing
# Secondary Sector: Electricity, Gas, Water Supply & Other Utility, Construction, Manufacturing
# Tertiary Sector: Trade, Hotels, Transport and Communication, Financial Services, Other Services
data <- data %>%
  mutate(`CLASSIFICATION BASED ON SECTOR` = ifelse(`CLASSIFICATION BASED ON INDUSTRY` %in% c("Mining & Quarrying", "Agriculture, Forestry and Fishing"),
                                                   "PRIMARY SECTOR",
                                            ifelse(`CLASSIFICATION BASED ON INDUSTRY` %in% c("Electricity, Gas, Water Supply & Other Utility", "Construction", "Manufacturing"),
                                                   "SECONDARY SECTOR",
                                            ifelse(`CLASSIFICATION BASED ON INDUSTRY` %in% c("Trade, Repair, Hotels and Restaurant", "Transport, Storage,Communication and Services Related to Broadcasting", "Financial Services", "Other Services", "Public Administration", "Real Estate, Ownership of Dwellings & Professional Services", "Public Administration, Defence and Other Services"),
                                                   "TERTIARY SECTOR",
                                                   NA))))

# colnames(data)

df_clean_summary <- data %>%
  mutate(
    YEAR = suppressWarnings(readr::parse_integer(as.character(YEAR)))
  ) %>%
  group_by(STATE, `CLASSIFICATION BASED ON SECTOR`, YEAR) %>%
  summarise(
    total_value_actuals = sum(suppressWarnings(as.numeric(`VALUE in ACTUALS`)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_value_actuals = format(total_value_actuals, scientific = FALSE, trim = TRUE)  # character for nice printing
    # or with separators: format(total_value_actuals, scientific = FALSE, big.mark = ",")
  )


sectors <- c("PRIMARY SECTOR", "SECONDARY SECTOR", "TERTIARY SECTOR")

df_clean_summary <- data %>%
  mutate(
    YEAR = suppressWarnings(readr::parse_integer(as.character(YEAR)))
  ) %>%
  group_by(STATE, `CLASSIFICATION BASED ON SECTOR`, YEAR) %>%
  summarise(
    total_value_actuals = sum(suppressWarnings(as.numeric(`VALUE in ACTUALS`)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add rows for both 2023 and 2024 for every state–sector
  complete(
    STATE,
    `CLASSIFICATION BASED ON SECTOR` = sectors,
    YEAR = c(2023, 2024)
  ) %>%
  arrange(STATE, `CLASSIFICATION BASED ON SECTOR`, YEAR, .by_group = TRUE) %>%
  # Compute CAGR from 2013 to 2022 per state–sector, then populate 2023 and 2024
  group_by(STATE, `CLASSIFICATION BASED ON SECTOR`) %>%
  mutate(
    v_2013 = total_value_actuals[YEAR == 2013],
    v_2022 = total_value_actuals[YEAR == 2022],
    n_years = 2022 - 2013,                               # 9 years
    cagr = ifelse(!is.na(v_2013) & v_2013 > 0 & !is.na(v_2022) & v_2022 >= 0,
                  (v_2022 / v_2013)^(1 / n_years) - 1,   # CAGR
                  NA_real_),
    # Fill 2023 if missing/zero using 2022 and CAGR
    total_value_actuals = ifelse(
      YEAR == 2023 & (is.na(total_value_actuals) | total_value_actuals == 0),
      ifelse(!is.na(cagr) & !is.na(v_2022),
             v_2022 * (1 + cagr),
             total_value_actuals),
      total_value_actuals
    ),
    # Now fill 2024 from the (possibly just-computed) 2023 value and CAGR
    v_2023_calc = total_value_actuals[YEAR == 2023],
    total_value_actuals = ifelse(
      YEAR == 2024 & (is.na(total_value_actuals) | total_value_actuals == 0),
      ifelse(!is.na(cagr) & !is.na(v_2023_calc),
             v_2023_calc * (1 + cagr),
             total_value_actuals),
      total_value_actuals
    )
  ) %>%
  ungroup() %>%
  # Optional pretty display (character) while keeping numeric totals in total_value_actuals
  mutate(
    total_value_actuals = format(total_value_actuals, scientific = FALSE, trim = TRUE),
    v_2013 = format(v_2013, scientific = FALSE, trim = TRUE),
    v_2022 = format(v_2022, scientific = FALSE, trim = TRUE)
  )





View(df_clean_summary)





# View(data)


data2 <- read_excel("DQ_DBIE_Excel_17_8_2025_2130.xlsx")

# coerce YEAR to integer
data2 <- data2 %>%
  mutate(
    YEAR = suppressWarnings(readr::parse_integer(as.character(YEAR)))
  )

# coerce VALUE in ACTUALS to numeric
data2 <- data2 %>%
  mutate(
    `VALUE in ACTUALS` = suppressWarnings(as.numeric(`VALUE in ACTUALS`))
  )

data2 <- data2 %>%
  mutate(
    YEAR = suppressWarnings(readr::parse_integer(as.character(YEAR))),
    `VALUE in ACTUALS` = suppressWarnings(as.numeric(`VALUE in ACTUALS`))
  ) %>%
  filter(`PRICE TYPE` == "Current Prices")

