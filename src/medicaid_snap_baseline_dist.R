#---------------------------------------------------
# medicaid_snap_baseline_dist.R
# 
# Creates baseline distributions of SNAP/Medicaid by
# tax unit (sorted by AGI) based on CBO baseline 
# projections and CPS-ASEC measures of receipt.
#---------------------------------------------------

#--------------------------
# Set parameters
#--------------------------

# Set working directory
setwd('~/project/repositories/Safety-Net')

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Set filepaths
#a. CPS ASEC data
cps_path <- paste0('/gpfs/gibbs/project/sarin/shared/raw_data/CPS-ASEC/v1/2025031013/historical/')
#b. CBO baseline projections
cbo_path <- '/gpfs/gibbs/project/sarin/shared/raw_data/CBO-Safety-Net/v1/20250619/baseline/'
#c. CBO LTBO
ltbo_path <- '/gpfs/gibbs/project/sarin/shared/raw_data/CBO-LTBO/v3/20250327/baseline/'
#d. Macro-Projections
macro_proj_path <- '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2025040115/baseline/'
#d. Output path
out_path <- '/gpfs/gibbs/project/sarin/shared/model_data/Safety-Net/v1/2025062612/baseline/'


# Create directories if they don't yet exist
for (x in c(cps_path)) {
  if (file.exists(x) == FALSE) {
    dir.create(gsub("/historical/","",x))
    dir.create(x)
  }
}

#Set first and last years of Medicaid/SNAP projections
firstyr_snap_proj <- 2036
firstyr_medicaid_proj <- 2035
lastyr_proj <- 2055

#Set first/last years of final dataset
firstyr_dist <- 2025
lastyr_dist <- 2054

#-------------------------------
# Read in Budget Lab functions
#-------------------------------
source('./src/utils.R')

# Get API key(s)
parse_api_keys('/gpfs/gibbs/project/sarin/hre2/api_keys.csv', keep=c('ipums'))
set_ipums_api_key(ipums_key)

#---------------------------------------------
# Read in and process CBO baseline projections
#---------------------------------------------
#SNAP
snap_cbo <- read.xlsx(file.path(cbo_path, 'snap.xlsx'),
                      startRow = 8, 
                      skipEmptyRows=TRUE, 
                      skipEmptyCols = TRUE, 
                      colNames=FALSE) %>%
              slice(c(1,5,8,12,15,16)) %>%
                select(-X1,-X2,-X14,-X15) %>%
                    t %>%
                      as.data.frame %>%
                        mutate_if(is.character,as.numeric) 
names(snap_cbo) <- c('fy',
                     'outlays_snap',
                     'benefits_snap',
                     'admin_snap',
                     'avg_monthly_part_snap',
                     'avg_monthly_ben_snap')

#CHIP
chip_cbo <- read.xlsx(file.path(cbo_path, 'chip.xlsx'),
                      startRow = 8, 
                      skipEmptyRows=TRUE, 
                      skipEmptyCols = TRUE, 
                      colNames=FALSE) %>%
  slice(c(1,5,10,12)) %>%
  select(-X1,-X2,-X3,-X15,-X16) %>%
  t %>%
  as.data.frame %>%
  mutate_if(is.character,as.numeric)
names(chip_cbo) <- c('fy',
                     'outlays_chip',
                     'enrolled_chip',
                     'avg_ben_chip')

#Medicaid (ex. CHIP)
medicaid_cbo <- read.xlsx(file.path(cbo_path, 'medicaid.xlsx'),
                          startRow = 8, 
                          skipEmptyRows=TRUE, 
                          skipEmptyCols = TRUE, 
                          colNames=FALSE) %>%
                  slice(c(1,4,12,14,27,28,29,30,31,36,37,38,39,40)) %>%
                    select(-X1,-X2,-X3,-X4,-X16,-X17) %>% 
                      t %>%
                        as.data.frame %>%
                          mutate_if(is.character,as.numeric)
names(medicaid_cbo) <- c('fy',
                         'outlays_medicaid',
                         'benefits_medicaid_inst',
                         'benefits_medicaid',
                         'enrolled_medicaid_aged',
                         'enrolled_medicaid_disabled',
                         'enrolled_medicaid_child',
                         'enrolled_medicaid_adult_trad',
                         'enrolled_medicaid_adult_aca',
                         'avg_ben_medicaid_aged',
                         'avg_ben_medicaid_disabled',
                         'avg_ben_medicaid_child',
                         'avg_ben_medicaid_adult_trad',
                         'avg_ben_medicaid_adult_aca')

#Combine Medicaid and CHIP data and consolidate adult categories (since we
#can't make those distinctions in the ASEC data)
medicaid_chip_cbo <- medicaid_cbo %>%
                      left_join(chip_cbo, by = 'fy') %>%
                        mutate(benefits_medicaid_chip_noninst = benefits_medicaid + outlays_chip - benefits_medicaid_inst,
                               avg_ben_medicaid_chip_child = (enrolled_medicaid_child/(enrolled_medicaid_child + enrolled_chip) * avg_ben_medicaid_child) +
                                                             (enrolled_chip/(enrolled_medicaid_child + enrolled_chip) * avg_ben_chip),
                               avg_ben_medicaid_adult = (enrolled_medicaid_adult_trad/(enrolled_medicaid_adult_trad + enrolled_medicaid_adult_aca) * avg_ben_medicaid_adult_trad) +
                                                        (enrolled_medicaid_adult_aca/(enrolled_medicaid_adult_trad + enrolled_medicaid_adult_aca) * avg_ben_medicaid_adult_aca)
                        ) %>%
                          select(-starts_with('enrolled'), -avg_ben_medicaid_child, -avg_ben_chip, -avg_ben_medicaid_adult_trad, -avg_ben_medicaid_adult_aca)

# Get long-run growth factors for SNAP ("other mandatory") and Medicaid from 
# Long-Term Budget Outlook
outlays_growth <- read.xlsx(file.path(ltbo_path, 'LTBO-budget.xlsx'), 
                               sheet = '1. Summary Ext Baseline',
                               startRow = 10, 
                               skipEmptyRows=TRUE, 
                               skipEmptyCols = TRUE, colNames=TRUE) %>%
                        select(Fiscal.year,starts_with('Medicaid'), Other.mandatory, starts_with('Gross.domestic.product')) %>%
                          mutate_if(is.character,as.numeric) %>%
                            rename(fy = Fiscal.year,
                                   outlays_medicaid_gdp = starts_with('Medicaid'),
                                   outlays_mand_other_gdp = Other.mandatory,
                                   gdp = starts_with('Gross.domestic.product')) %>%
                                mutate(
                                  outlays_medicaid = outlays_medicaid_gdp/100 * gdp,
                                  outlays_mand_other = outlays_mand_other_gdp/100 * gdp
                                ) %>%
                                  filter(!is.na(fy)) %>%
                                    mutate(
                                      outlays_medicaid_gr = outlays_medicaid/lag(outlays_medicaid),
                                      outlays_mand_other_gr = outlays_mand_other/lag(outlays_mand_other)
                                    ) %>%
                                      select(fy, outlays_medicaid_gr, outlays_mand_other_gr)

# Combine total benefits for SNAP and Medicaid/CHIP, express in millions of dollars, and project forward using LTBO growth factors
benefits_totals_fy <- medicaid_chip_cbo %>%
                      full_join(snap_cbo, by ='fy') %>%
                        select(fy, benefits_snap, benefits_medicaid_chip_noninst) %>%
                          mutate(benefits_medicaid_chip_noninst = 1000 * benefits_medicaid_chip_noninst) %>%
                          full_join(outlays_growth, by='fy')
for (y in (firstyr_snap_proj-1):lastyr_proj) {
  benefits_totals_fy <- benefits_totals_fy %>% 
      mutate(benefits_snap = case_when(
        fy == y & is.na(benefits_snap) ~ lag(benefits_snap) * outlays_mand_other_gr,
        TRUE ~ benefits_snap
      ))
}
for (y in (firstyr_medicaid_proj-1):lastyr_proj) {
  benefits_totals_fy <- benefits_totals_fy %>% 
    mutate(benefits_medicaid_chip_noninst = case_when(
      fy == y & is.na(benefits_medicaid_chip_noninst) ~ lag(benefits_medicaid_chip_noninst) * outlays_medicaid_gr,
      TRUE ~ benefits_medicaid_chip_noninst
    ))
}
benefits_totals_fy <- benefits_totals_fy %>% select(-starts_with('outlays'))

# For average per-beneficiary Medicaid spending, grow by average growth of last three years of projection period
benefits_avg_medicaid_fy <- medicaid_chip_cbo %>%
                              select(fy, starts_with('avg')) %>%
                                rename(avg_ben_aged = avg_ben_medicaid_aged,
                                     avg_ben_disabled = avg_ben_medicaid_disabled,
                                     avg_ben_child = avg_ben_medicaid_chip_child,
                                     avg_ben_adult = avg_ben_medicaid_adult)

benefits_medicaid_growth <- benefits_avg_medicaid_fy %>%
                              mutate(
                                avg_ben_aged_gr = (avg_ben_aged/lag(avg_ben_aged,n=3))^(1/3),
                                avg_ben_disabled_gr = (avg_ben_disabled/lag(avg_ben_disabled,n=3))^(1/3),
                                avg_ben_child_gr = (avg_ben_child/lag(avg_ben_child,n=3))^(1/3),
                                avg_ben_adult_gr = (avg_ben_adult/lag(avg_ben_adult,n=3))^(1/3),
                              ) %>%
                                filter(fy==(firstyr_medicaid_proj-1)) %>%
                                  select(ends_with('gr'))
for (y in firstyr_medicaid_proj:lastyr_proj) {
  benefits_avg_medicaid_fy <- benefits_avg_medicaid_fy %>% add_row(fy = y)
}
benefits_avg_medicaid_fy <- benefits_avg_medicaid_fy %>% 
                          left_join(benefits_medicaid_growth, by=character())
for (y in firstyr_medicaid_proj:lastyr_proj) {
  for (var in c('avg_ben_aged','avg_ben_disabled','avg_ben_child','avg_ben_adult')) {
    benefits_avg_medicaid_fy <- benefits_avg_medicaid_fy %>% 
                                  mutate(!!sym(var) := case_when(
                                    fy == y & is.na(!!sym(var)) ~ lag(!!sym(var)) * !!sym(paste0(var,'_gr')),
                                    TRUE ~ !!sym(var)) )
  }
}  
benefits_avg_medicaid_fy <- benefits_avg_medicaid_fy %>% select(-ends_with('gr'))

# Finally, convert total and average benefits from FY to CY:
benefits_totals_cy <- benefits_totals_fy %>%
                        mutate(benefits_snap = .25 * lead(benefits_snap) + .75 * benefits_snap,
                               benefits_medicaid_chip_noninst = .25 * lead(benefits_medicaid_chip_noninst) + .75 * benefits_medicaid_chip_noninst) %>%
                          rename(year = fy)
benefits_avg_medicaid_cy <- benefits_avg_medicaid_fy %>%
                              mutate(avg_ben_aged     = .25 * lead(avg_ben_aged)     + .75 * avg_ben_aged,
                                     avg_ben_disabled = .25 * lead(avg_ben_disabled) + .75 * avg_ben_disabled,
                                     avg_ben_child    = .25 * lead(avg_ben_child)    + .75 * avg_ben_child,
                                     avg_ben_adult    = .25 * lead(avg_ben_adult)    + .75 * avg_ben_adult) %>%
                                rename(year = fy)

#-------------------------------
# Load in and process ASEC data
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
  keep_files = TRUE,
  api_key = ipums_key)

# Merge in tax IDs from raw PUMS
asec_data$peridnum <- as.character(asec_data$uh_peridnum_a1)
asec_data <- asec_data %>% inner_join(tax_ids, by = "peridnum")

# Corrections for dependent filers
asec_data$adjginc[asec_data$adjginc == 99999999] <- 0
asec_data$dep_filer <- ifelse(asec_data$depstat > 0 & asec_data$filestat != 6, 1, 0)
asec_data$adjginc[asec_data$dep_filer == 1] <- 0
asec_data$filestat[asec_data$dep_file == 1] <- 6

# Compute each SNAP-recipient family's 2023 per-person receipt
snap_data <- asec_data %>%
  select(serial, famunit, foodstamp) %>%
  group_by(serial, famunit) %>%
  mutate(famsize = n()) %>%
  ungroup() %>%
  distinct()

snap_data <- snap_data %>%
  group_by(serial, famunit, famsize) %>%
    dplyr::summarize(snap_spending = sum(foodstamp)) %>%
      mutate(snap_spending = snap_spending / famsize)

# Merge back to main data
asec_data <- asec_data %>%
  left_join(snap_data, by = c("serial", "famunit"))


#-------------------------------
# Main (calendar year) loop
#-------------------------------

asec_data_pre_loop <- asec_data

for (y in firstyr_dist:lastyr_dist) {
  
  asec_data <- asec_data_pre_loop
  
  #-----------------------------------------
  # Construct weights 
  #-----------------------------------------
  #Generate demographic adjustment factors
  demog_adjust <- bind_rows(read.csv(file.path(macro_proj_path,'historical.csv')), 
                            read.csv(file.path(macro_proj_path,'projections.csv'))) %>%
                    select('year', contains('married_'), contains('unmarried_')) %>%
                      filter(year %in% c(2024,y)) %>%
                        pivot_longer(cols = starts_with(c("married_", "unmarried_")),
                                     names_to = c(".value", "age"),
                                     names_pattern = "(married|unmarried)_(\\d+)") %>%
                            pivot_wider(id_cols = c("age"), 
                                        names_from = "year", 
                                        values_from = c("married", "unmarried"),
                                        names_glue = "{.value}_{year}") %>%
                              mutate(age = as.numeric(age)) %>%
                                mutate(age = case_when(
                                                age>=80 & age<=84 ~ 80,
                                                age>=85 ~ 85,
                                                TRUE ~ age)) %>%
                                  group_by(age) %>%
                                    dplyr::summarize(across(c(starts_with('married'),starts_with('unmarried')), 
                                                            sum, na.rm = TRUE)) %>%
                                      mutate(
                                        demog_adjust_married = ifelse(is.na(!!sym(paste0('married_',y)) / married_2024), 1, !!sym(paste0('married_',y)) / married_2024),
                                        demog_adjust_unmarried = ifelse(is.na(!!sym(paste0('unmarried_',y)) / unmarried_2024), 1, !!sym(paste0('unmarried_',y)) / unmarried_2024)
                                      ) %>%
                                        select(age, demog_adjust_married, demog_adjust_unmarried)
  
  # Compute adjusted person-weights
  asec_data <- asec_data %>%
    left_join(demog_adjust, by = 'age') %>%
    mutate(
      asecwt_adj = case_when(
        marst %in% c(1, 2, 3) ~ asecwt * demog_adjust_married,
        marst %in% c(4, 5, 6) ~ asecwt * demog_adjust_unmarried
      )
    )
  
  # Calculate mean person-weight within tax unit
  asec_data <- asec_data %>%
    group_by(tax_id) %>%
    mutate(tax_unit_wt = mean(asecwt_adj)) %>%
    ungroup()
  
  
  #-----------------------------------------
  # Construct baseline Medicaid spending 
  #-----------------------------------------
  # Assign each Medicaid recipient's spending level
  asec_data <- asec_data %>%
    left_join(select(filter(benefits_avg_medicaid_cy, year == y),-year),by=character()) %>%
    mutate(
      medicaid_spending = case_when(
        # Child
        himcaidly == 2 & (age >= 0 & age <= 18) ~ avg_ben_child,
        # Elderly
        himcaidly == 2 & age >= 65 ~ avg_ben_aged,
        # Disabled adult
        himcaidly == 2 & (age >= 19 & age <= 64) & disabwrk == 2 ~ avg_ben_disabled,
        # Nondisabled adult
        himcaidly == 2 & (age >= 19 & age <= 64) & disabwrk == 1 ~ avg_ben_adult,
        TRUE ~ 0
      )
    )
  
  # Collapse to tax-unit level
  tax_unit_data <- asec_data %>%
    group_by(tax_id) %>%
    dplyr::summarize(
      snap_spending = sum(snap_spending, na.rm = TRUE),
      medicaid_spending = sum(medicaid_spending, na.rm = TRUE),
      agi_2023 = sum(adjginc, na.rm = TRUE),
      tax_unit_wt = mean(tax_unit_wt, na.rm = TRUE)
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
      breaks = wtd.quantile(positive_agi$agi_2023_noise, positive_agi$tax_unit_wt, seq(0, 1, 0.001)),
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
  snap_top5 <- sum(top5$snap_spending * top5$tax_unit_wt)/sum(top5$tax_unit_wt)
  medicaid_top5 <- sum(top5$medicaid_spending * top5$tax_unit_wt)/sum(top5$tax_unit_wt)
  tax_unit_data <- tax_unit_data %>% mutate(
    snap_spending = ifelse(is.na(agi_pctile), snap_spending, ifelse(agi_pctile>=950, snap_top5, snap_spending)),
    medicaid_spending = ifelse(is.na(agi_pctile), medicaid_spending, ifelse(agi_pctile>=950, medicaid_top5, medicaid_spending)),
  )
  
  #b. Adjustment 2: scale up all transfer spending to CBO projection to account for undercount in ASEC and/or
  #                 for changes in program spending between ASEC and CBO year
  # Get total benefits for year
  tax_unit_data <- tax_unit_data %>% left_join(select(filter(benefits_totals_cy, year==y),-year), by=character())
  # Calculate adjustment factors for undercount
  snap_adj <- mean(tax_unit_data$benefits_snap) / (sum(tax_unit_data$snap_spending * tax_unit_data$tax_unit_wt)/1000000)
  medicaid_adj <- mean(tax_unit_data$benefits_medicaid_chip_noninst) / (sum(tax_unit_data$medicaid_spending * tax_unit_data$tax_unit_wt)/1000000)
  # Apply adjustment factors
  tax_unit_data <- tax_unit_data %>%
    mutate(
      snap_spending_base = snap_adj * snap_spending,
      medicaid_spending_base = medicaid_adj * medicaid_spending
    )
  
  
  #--------------------------------------------------
  # Collapse to income groups
  #--------------------------------------------------
  # Baseline values
  #Quintiles
  summary_data <- tax_unit_data %>%
    group_by(agi_group) %>%
    dplyr::summarize(
      snap_spending_base = sum(snap_spending_base * tax_unit_wt)/1000000,
      medicaid_spending_base = sum(medicaid_spending_base * tax_unit_wt)/1000000,
      total_tax_units = sum(tax_unit_wt)/1000000
    )
  
  agi_grp_ctr <- 6
  for (x in c('top10','top5','top1','top01')) {
    summary_data_top <- tax_unit_data %>% filter(get(x) == 1) %>%
      dplyr::summarize(
        snap_spending_base = sum(snap_spending_base * tax_unit_wt)/1000000,
        medicaid_spending_base = sum(medicaid_spending_base * tax_unit_wt)/1000000,
        total_tax_units = sum(tax_unit_wt)/1000000
      ) %>%
      mutate(agi_group = agi_grp_ctr)
    summary_data <- bind_rows(summary_data, summary_data_top)
    agi_grp_ctr <- agi_grp_ctr + 1
  }
  
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
  
  # Add to master summary dataframe
  table_order_snap <- c('agi_group','agi_group_label','snap_spending_base')
  summary_data_snap <- summary_data[, table_order_snap] %>% rename(!!sym(paste0('snap_spending_',y)) := snap_spending_base)
  table_order_medicaid <- c('agi_group','agi_group_label','medicaid_spending_base')
  summary_data_medicaid <- summary_data[, table_order_medicaid] %>% rename(!!sym(paste0('medicaid_spending_',y)) := medicaid_spending_base)
  
  if (y == firstyr_dist) {
    summary_data_allyrs_snap <- summary_data_snap
    summary_data_allyrs_medicaid <- summary_data_medicaid
  } else {
    summary_data_allyrs_snap <- summary_data_allyrs_snap %>% left_join(summary_data_snap, by=c('agi_group','agi_group_label'))
    summary_data_allyrs_medicaid <- summary_data_allyrs_medicaid %>% left_join(summary_data_medicaid, by=c('agi_group','agi_group_label'))
  }
  
}

# Export to CSV
write.csv(summary_data_allyrs_snap, file = paste0(out_path,"snap_baseline_dist.csv"), row.names = FALSE, na="")
write.csv(summary_data_allyrs_medicaid, file = paste0(out_path,"medicaid_baseline_dist.csv"), row.names = FALSE, na="")
