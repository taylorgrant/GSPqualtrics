# Shiny App - Qualtrics Visual and Table #

pacman::p_load(tidyverse, janitor, here, glue, qsurvey, qualtRics)

# temporary - will have this on a cron job in the rstudio side # 
# sids <- qualtRics::all_surveys() %>%
#   dplyr::mutate(creationDate = as.Date(creationDate)) %>%
#   dplyr::arrange(desc(creationDate))
# saveRDS(sids, "/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

# load Qualtrics Survey IDs
sids <- readRDS("/srv/shiny-server/qualtrics-viz/data/qualtrics_sids.rds")

