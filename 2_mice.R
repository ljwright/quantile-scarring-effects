library(Hmisc)
library(tidyverse)
library(mice)
library(haven)
library(glue)
library(tictoc)
library(lavaan)
library(furrr)
library(gallimaufr)

rm(list=ls())

# 1. Load and Format Dataset ----
df_raw <- read_dta("Data/Dataset.dta") %>%
  as_factor() %>%
  zap_labels() %>%
  zap_formats()


# 2. Create LOC Factor ----
df_loc <- df_raw %>%
  select(NSID, Survey_Weight_W2, matches("LOC(.*)Item")) %>%
  drop_na()

int <- paste0("Int_LOC_W2_Item", 1:3)
ext <- paste0("Ext_LOC_W2_Item", 1:3)
items <- c(ext, int)

int_cov <- combn(int, 2, simplify = FALSE) %>%
  map_chr(paste, collapse = " ~~ ") %>%
  paste(collapse = "\n")

df <- glue("LOC_Factor_W2 =~ {paste(items, collapse = ' + ')}; {int_cov}") %>%
  cfa(df_loc, estimator = "DWLS") %>%
  lavPredict() %>%
  as.data.frame() %>% as_tibble() %>%
  bind_cols(select(df_loc, "NSID")) %>%
  full_join(df_raw, by = "NSID") %>%
  mutate(mean = wtd.mean(LOC_Factor_W2, Survey_Weight_W2),
         sd = wtd.var(LOC_Factor_W2, Survey_Weight_W2) %>% sqrt(),
         LOC_Factor_W2 = (LOC_Factor_W2 - mean) / sd) %>%
  select(-mean, -sd, -matches("LOC(.*)Item"), -Survey_Weight_W2) %>%
  relocate(NSID, Survey_Weight_W8)
save(df, file = "Data/df_analysis.Rdata")
rm(df_loc, df_raw, int, ext, int_cov, items)


# 3. MICE Quantile ----
load("Data/df_analysis.Rdata")

m <- 28
n_core <- 4
max_it <- 10

unem_var <- str_subset(names(df), "^Unem")

id_obs <- df %>%
  filter(!is.na(GHQ_W8_Likert)) %>%
  pull(NSID) %>%
  unique()

make_long <- function(imps){
  imps %>%
    complete("long", TRUE) %>%
    as_tibble() %>%
    filter(NSID %in% id_obs) %>%
    rename(imp = .imp) %>%
    select(-matches("^\\."))
}


imp <- list()
for (unem in unem_var){
  df_quantile <- df %>%
    select(-all_of(str_subset(unem_var, unem, TRUE)))
  
  pred <- make.predictorMatrix(df_quantile)
  pred[, c("Female", "NSID")] <- 0
  
  set.seed(1)
  tic()
  imp$female <- parlmice(data = df_quantile[df_quantile$Female == "Female", ],
                         n.core = n_core, n.imp.core = ceiling(m/n_core),
                         maxit = max_it, predictorMatrix = pred)
  imp$male <- parlmice(data = df_quantile[df_quantile$Female == "Male", ],
                       n.core = n_core, n.imp.core = ceiling(m/n_core),
                       maxit = max_it, predictorMatrix = pred)
  imp[[unem]] <- rbind(imp$female, imp$male)
  imp$female <- NULL
  imp$male <- NULL
  save(imp, id_obs, make_long, file = "Data/mice.Rdata")
  toc() 
}

# 4. Check Chains ----
load("Data/mice.Rdata")
map(imp, plot_chains)
