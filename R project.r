library(ggplot2) # for data visualization
library(readxl) # to read Excel files
library(dplyr) # for data wrangling
library(tidyr) # for reshaping data
library(stringr) # for cleaning text
library(janitor) # for cleaning column names

data <- read_excel("Components and Economic Activity of Domestic Products at Factor Cost - State Wise-(COMP_ECO_ACT_NSDP.xlsx")

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

# group data2 by states chronologically
data2 <- data2 %>%
  arrange(STATE, YEAR)

data2 <- data2 %>%
  filter(`PRICE TYPE` != "Constant Price")

data2 <- data2 %>%
  filter(`PRICE TYPE` != "Constant Price")

# filter out rows where year is between 2013 and 2024
data2 <- data2 %>%
  filter(YEAR >= 2013 & YEAR <= 2024)

# add a row with YEAR 2024 for states which dont have 2024
data2 <- data2 %>%
  complete(STATE, YEAR = c(2013:2024))

# get values of other columns from 2023 except year
data2 <- data2 %>%
  group_by(STATE) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# add a column named "CAGR" that calculates the compound annual growth rate of VALUE in ACTUALS for each state from 2013 to 2023
data2 <- data2 %>%
  group_by(STATE) %>%
  mutate(
    v_2013 = `VALUE in ACTUALS`[YEAR == 2013],
    v_2024 = `VALUE in ACTUALS`[YEAR == 2024],
    n_years = 2024 - 2013,                               # 12 years
    CAGR = ifelse(!is.na(v_2013) & v_2013 > 0 & !is.na(v_2024) & v_2024 >= 0,
                  (v_2024 / v_2013)^(1 / n_years) - 1,   # CAGR
                  NA_real_)
  ) %>%
  ungroup()


# for each state, calculate percentage change over 12 year period from 2013 to 2024 in per capita income of each state
data2 <- data2 %>%
  group_by(STATE) %>%
  mutate(
    perc_change_2013_2024 = ifelse(!is.na(v_2013) & v_2013 > 0 & !is.na(v_2024) & v_2024 >= 0,
                                   (v_2024 - v_2013) / v_2013 * 100,
                                   NA_real_)
  ) %>%
  ungroup()

# for each state, calculate percentage change over 11 year period from 2013 to 2024 in per capita income
data2 <- data2 %>%
  group_by(STATE) %>%
  mutate(
    perc_change_2013_2024 = ifelse(!is.na(v_2013) & v_2013 > 0 & !is.na(v_2024) & v_2024 >= 0,
                                   (v_2024 - v_2013) / v_2013 * 100,
                                   NA_real_)
  ) %>%
  ungroup()


# rename CAGR column to CAGR(%)
data2 <- data2 %>%
  rename(`CAGR(%)` = CAGR) %>%
  mutate(`CAGR(%)` = `CAGR(%)` * 100)

# rearrange data2. group by state and year. sort CAGR % in ascending order
data2 <- data2 %>%
  arrange(`CAGR(%)`,STATE, YEAR)


# rearrange data2. group by state and year. sort CAGR % in ascending order
data2 <- data2 %>%
  arrange(`perc_change_2013_2024`,STATE, YEAR)


View(data2)


# save data and data2 in csv
write.csv(df_clean_summary, "cleaned_sector_data.csv", row.names = FALSE)
write.csv(data2, "cleaned_per_capita_income_data.csv", row.names = FALSE)
