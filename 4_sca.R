library(quantreg)
library(tidyverse)
library(glue)
library(Hmisc)
library(tictoc)
library(mice)
library(magrittr)
library(furrr)

rm(list = ls())

# 1. Load Data ----
load("Data/mice.Rdata")
load("Data/df_analysis.Rdata")

imp_long <- make_long(imp$Unem_6Months) %>%
  filter(imp == 1)
rm(imp, id_obs, make_long)


# 2. Model Arguments ----
covars <- c("NSID", "Survey_Weight_W8", "GHQ_W8_Likert", 
            "Unem_", "FinDiff_W1", "HHKids_W1", "HHType_W1") %>%
  glue_collapse("|") %>%
  str_subset(names(df), ., TRUE)
covars
rm(df)

all_specs <- map_dfr(1:length(covars),
                     ~ combn(covars, .x, simplify = FALSE) %>%
                       map(glue_collapse, " + ") %>%
                       map_dfr(enframe, value = "covars")) %>%
  select(-name) 

get_sca <- function(covars, tau){
  form <- glue("GHQ_W8_Likert ~ Unem_6Months + {covars}") %>%
    as.formula()
  
  mod <- rq(form, tau = tau, data = imp_long, 
            weights = Survey_Weight_W8) %>%
    coef()
  
  mod[str_detect(names(mod), "^Unem")]
}

# 3. Run SCA ----
# Main SCA
plan(multisession, workers = 4)
tic()
set.seed(2)
sca_res <- all_specs %>%
  sample_n(20000) %>%
  add_row(covars =  glue_collapse(covars, " + "), .before = 1) %>%
  add_row(covars = str_subset(covars, "Status_W8", TRUE) %>%
            glue_collapse(" + "), .after = 1) %>%
  distinct(covars, .keep_all = TRUE) %>%
  uncount(9, .id = "tau") %>%
  mutate(tau = tau/10) %>%
  select(covars, tau) %>%
  mutate(coef = future_map2_dbl(covars, tau, 
                                get_sca, .progress = TRUE)) %>%
  group_by(covars) %>%
  mutate(id = cur_group_id(), .before = 1) %>%
  ungroup() %>%
  mutate(status = str_detect(covars, "Status_W8"),
         status_clean = ifelse(status == TRUE,
                               "+ Current Status",
                               "Long-Term Association") %>%
           factor(c("Long-Term Association", "+ Current Status")),
         n_vars = str_split(covars, " \\+ ") %>%
           map_dbl(length),
         mod = (n_vars == length(!!covars) - 1 + status)) %>%
  select(id, tau, mod, status, status_clean, coef)
toc()
future:::ClusterRegistry("stop")

save(sca_res, file = "Data/sca_results.Rdata")
