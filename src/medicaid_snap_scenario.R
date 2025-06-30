#---------------------------------------------------
# medicaid_snap_scenario.R
# 
# Run scenario distributional analysis of changes to 
# Medicaid/SNAP.
#---------------------------------------------------

#--------------------------
# Set parameters
#--------------------------

# Set working directory
setwd('~/project/repositories/Safety-Net')

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Set filepaths
#a. Baseline Medicaid/SNAP distributions
baseline_path <- '/gpfs/gibbs/project/sarin/shared/model_data/Safety-Net/v1/2025062612/baseline'
#b. Output path
out_path <- '/gpfs/gibbs/project/sarin/shared/model_data/Safety-Net/v1/'

#Set first and last years of scenarios
firstyr_dist <- 2025
lastyr_dist  <- 2034

# Scenario list
scenario_list <- c('house_20250522','senate_20250627','senate_20250627_scott_amendment') 

# Share of Medicaid changes going to beneficiaries, and to provider labor/capital
medicaid_beneficiary_share <- .44
medicaid_provider_share_lab <- (1-medicaid_beneficiary_share) * .70
medicaid_provider_share_cap <- (1-medicaid_beneficiary_share) * .30

# Degree of state response (as percent of overall Federal cuts)
state_resp_factor = .01

#-------------------------------------------------------
# Read in Budget Lab baseline distributions and toplines
#-------------------------------------------------------
snap_baseline_data <- read.csv(file.path(baseline_path,'snap_baseline_dist.csv'))
medicaid_baseline_data <- read.csv(file.path(baseline_path,'medicaid_baseline_dist.csv'))

#-------------------------------------------------------
# Loop over specified scenarios
#-------------------------------------------------------
for (scenario in scenario_list) {

  #Generate filepaths
  out_path_full <- file.path(out_path,format(Sys.time(), '%Y%m%d%H%M'),scenario)
  
  #Read in toplines
  toplines <- read.csv(paste0('./config/',scenario,'.csv'))
  
  # Check if topline file contains sufficient years of data for fiscal-to-calendar
  # year conversion. If not, project final year forward.
  if (!(lastyr_dist+1) %in% toplines$year) {
    toplines <- rbind(toplines, data.frame(year = lastyr_dist+1,
                                           snap_changes = NA, 
                                           medicaid_changes = NA, 
                                           medicaid_changes_add_provider = NA)) %>%
      mutate(
        across(
          c(snap_changes, medicaid_changes, medicaid_changes_add_provider),
          ~ ifelse(year == lastyr_dist+1 & is.na(.x), 
                   lag(.x) * ifelse(is.na(lag(.x) / lag(.x, n=2)),1,is.na(lag(.x) / lag(.x, n=2))), 
                   .x)
        )                    
      )
  }
  # Convert toplines from fiscal year to calendar year
  toplines <- toplines %>%
    mutate(
      across(
        c(snap_changes, medicaid_changes, medicaid_changes_add_provider),
        ~ .25 * lead(.x) + .75 * .x
      ) 
    )%>%
        filter(year %in% seq(firstyr_dist,lastyr_dist))

  
  #-------------------------------
  # Main (calendar year) loop
  #-------------------------------
  for (y in firstyr_dist:lastyr_dist) {
    
    
    # Merge baseline data and toplines
    dist <- snap_baseline_data %>%
              select(agi_group, agi_group_label,paste0('snap_spending_',y)) %>%
                left_join(
                  select(medicaid_baseline_data, agi_group, agi_group_label,paste0('medicaid_spending_',y)),
                  by = c('agi_group','agi_group_label')
                  ) %>%
                  rename(snap_baseline := !!paste0('snap_spending_',y), medicaid_baseline := !!paste0('medicaid_spending_',y)) %>%
                    left_join(filter(toplines, year == y), by = character())
    
    # Allocate portion of Medicaid changes to providers
    medicaid_changes_provider_lab <- medicaid_provider_share_lab * sum(mean(dist$medicaid_changes),mean(dist$medicaid_changes_add_provider))
    medicaid_changes_provider_cap <- medicaid_provider_share_cap * sum(mean(dist$medicaid_changes),mean(dist$medicaid_changes_add_provider))
    if (y == firstyr_dist) {
      medicaid_provider <- data.frame(y,medicaid_changes_provider_lab, medicaid_changes_provider_cap)
    } else{
      medicaid_provider <- bind_rows(medicaid_provider, data.frame(y, medicaid_changes_provider_lab, medicaid_changes_provider_cap))
    }
      
    # Distribute to beneficiaries by baseline shares
    base_total <- dist %>%
                    filter(agi_group %in% c(0,1,2,3,4,5)) %>%
                      dplyr::summarise(snap_total = sum(snap_baseline, na.rm = TRUE),
                                       medicaid_total = sum(medicaid_baseline, na.rm = TRUE))
    dist <- dist %>% 
              left_join(base_total, by=character()) %>%
                mutate(snap_scenario     = snap_baseline * (1 + ((1-state_resp_factor) * snap_changes/snap_total)),
                       medicaid_scenario = medicaid_baseline * (1 + (medicaid_beneficiary_share * (1-state_resp_factor) *  medicaid_changes/medicaid_total))) %>%
                  select(agi_group,agi_group_label, snap_baseline, snap_scenario, medicaid_baseline, medicaid_scenario)
  
                                                        
    # Export to CSV (beneficiaries)
    dir.create(file.path(out_path_full,'beneficiary'), recursive = T, showWarnings = F)
    write.csv(dist, file = paste0(out_path_full,'/beneficiary/', y, '.csv'), row.names = FALSE, na="")
  }
  
  # Export to CSV (providers)
  names(medicaid_provider) <- c('year','medicaid_changes_provider_lab','medicaid_changes_provider_cap')
  dir.create(file.path(out_path_full,'provider'), recursive = T, showWarnings = F)
  write.csv(medicaid_provider, file = paste0(out_path_full,'/provider/medicaid_provider.csv'), row.names = FALSE, na="")
}