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

# 2. Model Arguments ----
covars <- c("NSID", "Survey_Weight_W8", "GHQ_W8_Likert", 
            "Unem_", "Status_W8",
            "FinDiff_W1", "HHKids_W1", "HHType_W1") %>%
  glue_collapse("|") %>%
  str_subset(names(df), ., TRUE) %>%
  glue_collapse(" + ")
covars

sep_vars <- c("FinDiff_W1", "HHKids_W1", "HHType_W1") %>%
  glue_collapse(" + ")

mod_forms <- c(
  bivar = "GHQ_W8_Likert ~ Unem_6Months",
  mh = "GHQ_W8_Likert ~ Unem_6Months + GHQ_W2_Caseness + GHQ_W4_Likert",
  full = glue("GHQ_W8_Likert ~ Unem_6Months + {covars}"),
  scar = glue("GHQ_W8_Likert ~ Unem_6Months + Status_W8 + {covars}"),
  gender = glue("GHQ_W8_Likert ~ Unem_6Months + {str_replace(covars, 'Female', '1')}"),
  sep = glue("GHQ_W8_Likert ~ Unem_6Months + {covars} + {sep_vars}"),
  Unem_3Months = glue("GHQ_W8_Likert ~ Unem_3Months + {covars}"),
  Unem_9Months = glue("GHQ_W8_Likert ~ Unem_9Months + {covars}"),
  Unem_12Months = glue("GHQ_W8_Likert ~ Unem_12Months + {covars}")
)

sexes <- list(male = "Male", female = "Female",
              all = c("Male", "Female"))

mod_specs <- expand_grid(mod = names(mod_forms),
                         sex = names(sexes),
                         tau  = 1:9/10,
                         sample = c("cc", "mi")) %>%
  filter(!(sex == "all" & mod == "gender"),
         !(sex != "all" & mod != "gender")) %>%
  mutate(unem_var = ifelse(str_detect(mod, "^Unem"), mod, "Unem_6Months"))

# Weighted Means and Modes
wtd_mean_or_mode <- function(var, weight){
  if (is.factor(var)){
    wtd.table(var, weight, type = "table") %>%
      enframe() %>%
      arrange(desc(value)) %>%
      slice(1) %>%
      pull(name) %>%
      factor(levels = levels(var))
  }
  else{
    wtd.mean(var, weight)
  }
}

convert_factor <- function(var, name){
  if (is.factor(var)){
    name <- paste0(name, as.character(var))
    var <- 1
  }
  set_names(var, name)
}

df_m <- make_long(imp$Unem_6Months) %>%
  filter(imp > 0) %>%
  select(-NSID) %>%
  mutate(`(Intercept)` = 1)

v_means <- map(df_m, wtd_mean_or_mode,
               df_m$Survey_Weight_W8) %>%
  imap(convert_factor) %>%
  set_names(NULL) %>%
  do.call("c", .)

rm(df_m)


# 3. Model Functions ----
# Run Models
get_rq <- function(mod_form, df, tau){
  df_a <- df %>%
    sample_frac(1, TRUE)
  
  coefs <- rq(mod_form, tau = tau, 
              data = df_a, weights = Survey_Weight_W8) %>%
    coef() %>%
    enframe(name = "term", value = "estimate")
}

boot_rq <- function(spec_id, spec_imp){
  spec <- slice(mod_specs, !!spec_id)
  
  df_reg <- imp[[spec$unem_var]] %>%
    make_long() %>%
    filter(imp == !!spec_imp,
           Female %in% sexes[[spec$sex]])
  
  mod_form <- mod_forms[spec$mod] %>%
    as.formula()
  
  map_dfr(1:500,
          ~ get_rq(mod_form, df_reg, spec$tau),
          .id = "boot") %>%
    mutate(boot = as.integer(boot))
}

get_nobs <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  spec$imp <- ifelse(spec$sample == "cc", 0, 1)
  
  df_reg <- imp[[spec$unem_var]] %>%
    make_long() %>%
    filter(imp == !!spec$imp,
           Female %in% sexes[[spec$sex]])
  
  mod_forms[spec$mod] %>%
    as.formula() %>%
    lm(df_reg) %>%
    nobs()
}

# Run and Clean All Models
get_res <- function(spec_id){
  spec <- slice(mod_specs, !!spec_id)
  
  if (spec$sample == "cc"){
    df_res <- tibble(imp = 0)
  } else {
    df_res <- tibble(imp = 1:28) 
  } 
  
  # Run Models
  df_res <- df_res %>%
    mutate(res = map(imp, ~ boot_rq(spec_id, .x))) %>%
    unnest(res)
  
  get_ci <- function(x){
    quantile(x, c(.5, .025, .975)) %>%
      as_tibble_row() %>%
      rename(beta = 1, lci = 2, uci = 3)
  }
  
  # Coefficients
  coefs <- df_res %>%
    group_by(term) %>%
    summarise(get_ci(estimate))
  
  # Prediction
  pred <- tibble(unem = c(0, 1)) %>%
    expand_grid(df_res) %>%
    mutate(value = case_when(str_detect(term, "^Unem") ~ unem,
                             !is.na(v_means[term]) ~ v_means[term],
                             TRUE ~ 0)) %>%
    group_by(unem, imp, boot) %>%
    summarise(estimate = sum(value*estimate),
              .groups = "drop")  %>%
    group_by(unem) %>%
    summarise(get_ci(estimate)) 
  
  tibble(coefs = list(coefs),
         pred = list(pred),
         nobs = get_nobs(spec_id))
}


# 3. Quantile Regressions ----
set.seed(1)
tic()
plan(multisession, workers = 4)
qreg_res <- mod_specs %>%
  mutate(spec_id = row_number(), .before = 1) %>%
  sample_frac() %>%
  mutate(res = future_map(spec_id, get_res, 
                          .progress = TRUE,
                          .options = furrr_options(seed = 123))) %>%
  unnest(res) %>%
  arrange(spec_id) %>%
  select(-spec_id)
future:::ClusterRegistry("stop")
toc()

save(qreg_res, file = "Data/qreg_results.Rdata")