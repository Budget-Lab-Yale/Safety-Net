#---------------------------------------------------
# medicaid_snap_fy2025.R
# 
# Distributional analysis of changes in Medicaid and
# SNAP spending in FY2025 House Budget Resolution for
# CY2026.
#---------------------------------------------------

#--------------------------
# Set simulation parameters
#--------------------------

# Set working directory
setwd('~/project/repositories/Safety-Net')

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Set vintage(s)
data_vintage <- '2025031013'

# Set filepaths
#a. CPS ASEC data
cps_path <- paste0('/gpfs/gibbs/project/sarin/shared/raw_data/CPS-ASEC/v1/', data_vintage,'/historical/')
#b. Historical/projected economic variables
macro_proj_path <- '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2024121811/baseline/'
#c. Output path
out_path <- '/gpfs/gibbs/project/sarin/shared/model_data/Safety-Net/'

# Create directories if they don't yet exist
for (x in c(cps_path)) {
  if (file.exists(x) == FALSE) {
    dir.create(gsub("/historical/","",x))
    dir.create(x)
  }
}

# Set random seed 
set.seed(85675443)


#--------------------------
# Set policy parameters
#--------------------------
# Sources:
# House Budget Resolution: https://docs.house.gov/meetings/BU/BU00/20250213/117894/BILLS-119HConRes14ih.pdf
# January 2025 BEO: https://www.cbo.gov/system/files/2025-01/51118-2025-01-Budget-Projections.xlsx
# June 2024 Medicaid: https://www.cbo.gov/system/files/2024-06/51301-2024-06-medicaid.xlsx
# June 2024 CHIP: https://www.cbo.gov/system/files/2024-06/51296-2024-06-chip.xlsx
# January 2025 SNAP: https://www.cbo.gov/system/files/2025-01/51312-2025-01-snap.xlsx

# Calculate percent cuts to SNAP and Medicaid based on:
# a. Reconciliation instructions to committees in House BR (multiplied by 4/3 to account for
#    additional $500B in unspecified decifit reduction on top of $1.5T of specified cuts).
# b. CBO January 2025 BEO figures (totals) for SNAP and Medicaid for FY2026-34 (assuming cuts over 9 years)
snap_cut <- (230 * (4/3)) / 1007.709
medicaid_cut <- (880 * (4/3)) / 7733.752

# Average per-person Federal spending on Medicaid + CHIP, CY2026
# Using June 2024 detailed baseline projections (latest available), taking weighted
# average of FY2026 and FY2027.
# For children, combining Medicaid and CHIP. For non-disabled adults, combining 
# ACA expansion and non-expansion populations.
avg_medicaid_elderly <- (0.75*16630 + 0.25*16720)
avg_medicaid_child <- 0.75*((31/38) * 2280 + (7/38) * 2778) + 0.25*((31/38) * 2360 + (7/38) * 2840)
avg_medicaid_nondisab_adult <- 0.75*((18/32) * 3750 + (14/32) * 8110) + 0.25*((18/32) * 3870 + (14/32) * 8400)
avg_medicaid_disab_adult <- (0.75*21210 + 0.25*21520)

# Total SNAP and Medicaid/CHIP benefits, CY2026
# For SNAP, using January 2025 baseline projections.
total_snap_ben <- 0.75*96816+0.25*99514
# For Medicaid/CHIP, using June 2024 (latest available), excluding long-term
# institutional care (since CPS ASEC sample frame excludes institutionalized 
# individuals).
total_medicaid_ben <-  1000 * (0.75*(578 - 63 + 20) + 0.25*(604 - 65 + 20))


#-------------------------------
# Read in Budget Lab functions
#-------------------------------
source('./src/utils.R')

# Get API key(s)
parse_api_keys('/gpfs/gibbs/project/sarin/hre2/api_keys.csv', keep=c('ipums'))
set_ipums_api_key(ipums_key)


#-------------------------------
# Load in ASEC data
#-------------------------------

# Get tax unit IDs from raw ASEC PUMS (there's an issue with this variable in IPUMS)
tax_ids <- read.csv(file.path(cps_path,'pppub24.csv'), colClasses = c(PERIDNUM = 'character')) %>% select(c('PERIDNUM','TAX_ID'))
names(tax_ids) <- tolower(names(tax_ids))

# Pull selected 2024 ASEC variables from IPUMS
asec_data <- ipums_pull(
  collection = 'cps',
  samples_csv ='./config/cps_ipums_samples.csv',
  variables_csv ='./config/cps_ipums_vars.csv',
  download_path = cps_path,
  keep_files = TRUE)

# Merge in tax IDs from raw PUMS
asec_data$peridnum <- as.character(asec_data$uh_peridnum_a1)
asec_data <- asec_data %>% inner_join(tax_ids, by = "peridnum")


#-------------------------------------------------
# Demographic adjustment factors for 2026 vs. 2024
#-------------------------------------------------

# Load in historical/projected demographic data
demog_proj <- bind_rows(read.csv(file.path(macro_proj_path,'historical.csv')), read.csv(file.path(macro_proj_path,'projections.csv'))) %>%
  select('year', contains('married_'), contains('unmarried_'))

# Reshape from wide to long format (for married/unmarried variables)
demog_reshape <- demog_proj %>% pivot_longer(cols = starts_with(c("married_", "unmarried_")),
                                              names_to = c(".value", "age"),
                                              names_pattern = "(married|unmarried)_(\\d+)") %>%
                              pivot_wider(id_cols = c("age"), 
                                          names_from = "year", 
                                          values_from = c("married", "unmarried"),
                                          names_glue = "{.value}_{year}")
# Match ASEC age bins
demog_reshape$age <- as.numeric(demog_reshape$age)
demog_reshape$age[demog_reshape$age >= 80 & demog_reshape$age <= 84] <- 80
demog_reshape$age[demog_reshape$age >= 85] <- 85

# Collapse (sum) by age
demog_reshape <- demog_reshape %>%
  group_by(age) %>%
  dplyr::summarize(across(c(married_2024, married_2026, unmarried_2024, unmarried_2026), sum, na.rm = TRUE))

# Create demographic adjustment factors
demog_adjust <- demog_reshape %>%
  mutate(
    demog_adjust_married = married_2026 / married_2024,
    demog_adjust_unmarried = unmarried_2026 / unmarried_2024
  ) %>%
  mutate(
    demog_adjust_married = ifelse(is.na(demog_adjust_married), 1, demog_adjust_married),
    demog_adjust_unmarried = ifelse(is.na(demog_adjust_unmarried), 1, demog_adjust_unmarried)
  ) %>%
  select(age, demog_adjust_married, demog_adjust_unmarried)


#------------------------------
# Tax unit and AGI construction
#------------------------------
# Corrections for dependent filers
asec_data$adjginc[asec_data$adjginc == 99999999] <- 0
asec_data$dep_filer <- ifelse(asec_data$depstat > 0 & asec_data$filestat != 6, 1, 0)
asec_data$adjginc[asec_data$dep_filer == 1] <- 0
asec_data$filestat[asec_data$dep_file == 1] <- 6

# Compute 2026 tax unit weights
asec_data <- asec_data %>%
  left_join(demog_adjust, by = 'age') %>%
  mutate(
    asecwt_2026 = case_when(
      marst %in% c(1, 2, 3) ~ asecwt * demog_adjust_married,
      marst %in% c(4, 5, 6) ~ asecwt * demog_adjust_unmarried
    )
  )

# Calculate mean person-weight within tax unit
asec_data <- asec_data %>%
  group_by(tax_id) %>%
  mutate(tax_unit_wt_2026 = mean(asecwt_2026)) %>%
  ungroup()


#--------------------------------------
# Baseline transfer income construction
#--------------------------------------
# a. SNAP
# Compute each SNAP-recipient family's 2023 per-person receipt
snap_data <- asec_data %>%
  select(serial, famunit, foodstamp) %>%
  group_by(serial, famunit) %>%
  mutate(famsize = n()) %>%
  ungroup() %>%
  distinct()

snap_data <- snap_data %>%
  group_by(serial, famunit, famsize) %>%
  dplyr::summarize(snap_spending_2023 = sum(foodstamp)) %>%
  mutate(snap_spending_2023 = snap_spending_2023 / famsize)

# Merge back to main data
asec_data <- asec_data %>%
  left_join(snap_data, by = c("serial", "famunit"))

# b. Medicaid
# Assign each Medicaid recipient's 2026 spending level
asec_data <- asec_data %>%
  mutate(
    medicaid_spending_2026 = case_when(
      # Child
      himcaidly == 2 & (age >= 0 & age <= 18) ~ avg_medicaid_child,
      # Elderly
      himcaidly == 2 & age >= 65 ~ avg_medicaid_elderly,
      # Disabled adult
      himcaidly == 2 & (age >= 19 & age <= 64) & disabwrk == 2 ~ avg_medicaid_disab_adult,
      # Nondisabled adult
      himcaidly == 2 & (age >= 19 & age <= 64) & disabwrk == 1 ~ avg_medicaid_nondisab_adult,
      TRUE ~ 0
    )
  )

# c. Collapse to tax-unit level
tax_unit_data <- asec_data %>%
  group_by(tax_id) %>%
  dplyr::summarize(
    snap_spending_2023 = sum(snap_spending_2023, na.rm = TRUE),
    medicaid_spending_2026 = sum(medicaid_spending_2026, na.rm = TRUE),
    agi_2023 = sum(adjginc, na.rm = TRUE),
    tax_unit_wt_2026 = mean(tax_unit_wt_2026, na.rm = TRUE)
  )


#--------------------------------------
# AGI percentile cutoff construction
#--------------------------------------
# Generate small random noise to break ties
tax_unit_data <- tax_unit_data %>%
  mutate(agi_2023_noise = agi_2023 + runif(n()) * (agi_2023 >= 0))

# Calculate percentile cutoffs
positive_agi <- tax_unit_data %>% filter(agi_2023 >= 0)
tax_unit_data <- tax_unit_data %>% mutate( 
                    agi_pctile = cut(
                                x      = agi_2023_noise,
                                breaks = wtd.quantile(positive_agi$agi_2023_noise, positive_agi$tax_unit_wt_2026, seq(0, 1, 0.001)),
                                labels = FALSE
                              )
                            )
# Assign AGI groups
# Quintiles
tax_unit_data <- tax_unit_data %>%
  mutate(agi_group = case_when(
    is.na(agi_pctile) ~ 0,
    agi_pctile < 200 ~ 1,
    agi_pctile < 400 ~ 2,
    agi_pctile < 600 ~ 3,
    agi_pctile < 800 ~ 4,
    agi_pctile >= 800 ~ 5
    ))

# Top groups
tax_unit_data <- tax_unit_data %>%
  mutate(
    top10 = ifelse(agi_pctile >= 901,1,0),
    top5 = ifelse(agi_pctile >= 951,1,0),
    top1 = ifelse(agi_pctile >= 991,1,0),
    top01 = ifelse(agi_pctile > 999,1,0),
  )

#-------------------------------
# Adjustments to transfer income
#--------------------------------
#a. Adjustment 1: assume uniform distribution across top 5% of observations to
#   account for small n.
top5 <- tax_unit_data %>% filter(agi_pctile>=950)
snap_top5 <- sum(top5$snap_spending_2023 * top5$tax_unit_wt_2026)/sum(top5$tax_unit_wt_2026)
medicaid_top5 <- sum(top5$medicaid_spending_2026 * top5$tax_unit_wt_2026)/sum(top5$tax_unit_wt_2026)
tax_unit_data <- tax_unit_data %>% mutate(
                      snap_spending_2023 = ifelse(is.na(agi_pctile), snap_spending_2023,ifelse(agi_pctile>=950, snap_top5, snap_spending_2023)),
                      medicaid_spending_2026 = ifelse(is.na(agi_pctile), medicaid_spending_2026, ifelse(agi_pctile>=950, medicaid_top5, medicaid_spending_2026)),
                      )

#b. Adjustment 2: scale up all transfer spending to CBO 2026 projection to account for undercount in ASEC and/or
#                 for changes in program spending between year of data and 2026
# Calculate adjustment factors for undercount
snap_adj <- total_snap_ben / (sum(tax_unit_data$snap_spending_2023 * tax_unit_data$tax_unit_wt_2026)/1000000)
medicaid_adj <- total_medicaid_ben / (sum(tax_unit_data$medicaid_spending_2026 * tax_unit_data$tax_unit_wt_2026)/1000000)
# Apply adjustment factors
tax_unit_data <- tax_unit_data %>%
  mutate(
    snap_spending_2026_base = snap_adj * snap_spending_2023,
    medicaid_spending_2026_base = medicaid_adj * medicaid_spending_2026
  )


#--------------------------------------------------
# Collapse to baseline income group, apply scenario
#--------------------------------------------------
# Baseline values
#Quintiles
summary_data <- tax_unit_data %>%
  group_by(agi_group) %>%
  dplyr::summarize(
    snap_spending_2026_base = sum(snap_spending_2026_base * tax_unit_wt_2026)/1000000,
    medicaid_spending_2026_base = sum(medicaid_spending_2026_base * tax_unit_wt_2026)/1000000,
    total_tax_units = sum(tax_unit_wt_2026)/1000000
  )

agi_grp_ctr <- 6
for (x in c('top10','top5','top1','top01')) {
  summary_data_top <- tax_unit_data %>% filter(get(x) == 1) %>%
    dplyr::summarize(
      snap_spending_2026_base = sum(snap_spending_2026_base * tax_unit_wt_2026)/1000000,
      medicaid_spending_2026_base = sum(medicaid_spending_2026_base * tax_unit_wt_2026)/1000000,
      total_tax_units = sum(tax_unit_wt_2026)/1000000
    ) %>%
    mutate(agi_group = agi_grp_ctr)
  summary_data <- bind_rows(summary_data, summary_data_top)
  agi_grp_ctr <- agi_grp_ctr + 1
}

# Apply cuts and calculate changes
summary_data <- summary_data %>%
  mutate(
    snap_spending_2026_housebr = snap_spending_2026_base * (1 - snap_cut),
    medicaid_spending_2026_housebr = medicaid_spending_2026_base * (1 - medicaid_cut),
    avg_chg_snap = (snap_spending_2026_housebr - snap_spending_2026_base) / total_tax_units,
    avg_chg_medicaid = (medicaid_spending_2026_housebr - medicaid_spending_2026_base) / total_tax_units
  )

# Apply value labels to AGI groups
summary_data <- summary_data %>%
  mutate(agi_group_label = case_when(
    agi_group == 0 ~ 'Negative income',
    agi_group == 1 ~ 'Bottom quintile',
    agi_group == 2 ~ 'Second quintile',
    agi_group == 3 ~ 'Middle quintile',
    agi_group == 4 ~ 'Fourth quintile',
    agi_group == 5 ~ 'Top quintile',
    agi_group == 6 ~ 'Top 10%',
    agi_group == 7 ~ 'Top 5%',
    agi_group == 8 ~ 'Top 1%',
    agi_group == 9 ~ 'Top 0.1%'
  ))


# Export to CSV
table_order<- c('agi_group','agi_group_label','total_tax_units',
                'snap_spending_2026_base',  'snap_spending_2026_housebr', 'avg_chg_snap',
                'medicaid_spending_2026_base', 'medicaid_spending_2026_housebr', 'avg_chg_medicaid')
summary_data <- summary_data[, table_order]
write.csv(summary_data, file = paste0(out_path,"snap_medicaid_cy2026.csv"), row.names = FALSE, na="")
