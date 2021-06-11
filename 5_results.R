library(tidyverse)
library(glue)
library(Hmisc)
library(mice)
library(flextable)
library(officer)
library(patchwork)
library(magrittr)
library(paletteer)
library(gallimaufr)

rm(list = ls())

# 1. Load Data ----
load("Data/mice.Rdata")
load("Data/df_analysis.Rdata")

imp_long <- make_long(imp$Unem_6Months) %>%
  filter(imp > 0) %>%
  arrange(imp, NSID)
rm(imp, id_obs, make_long)

df <- df %>%
  select(-Unem_3Months, -Unem_9Months, -Unem_12Months)

pretty_lbls <- c(
  n = "n",
  GHQ_W8_Likert = "GHQ-12 @ Age 25",
  Unem_6Months = "Youth Unemployment",
  Female = "Gender",
  IMD_W2 = "IMD",
  LOC_Factor_W2 = "Locus of Control",
  Status_W8 = "Current Economic Activity",
  GHQ_W2_Caseness = "GHQ-12 @ Age 14/15",
  GHQ_W4_Likert = "GHQ-12 @ Age 16/17",
  GenHealth_W2 = "Self-Rated Health @ Age 14/15",
  GenHealth_W4 = "Self-Rated Health @ Age 16/17",
  Disabled = "Disabled",
  Risk_W1 = "Risk Behaviours",
  SchoolAtt_W2 = "Attitude to School",
  Bullied_Waves = "# Waves Bullied, 1-3",
  Education_W8 = "Qualifications",
  NSSEC3_W1 = "Parental NS-SEC",
  ParentEduc5_W1 = "Parental Education",
  Ethnicity = "Ethnicity",
  FinDiff_W1 = "Financial Difficulties",
  HHKids_W1 = "# Household Children",
  HHType_W1 = "Household Type",
  `(Intercept)` = "Constant",
  obs = "Observations",
  imps = "Imputations"
) %>%
  make_lbls(imp_long) %>%
  mutate(cat_clean = ifelse(cat == "FemaleFemale", "Female", cat_clean))
save(pretty_lbls, file = "Data/pretty_lbls.Rdata")

# 2. Descriptive Statistics ----
# Main Descriptives
n_prop <- function(df){
  df %>%
    mutate(value = str_replace(string, ",", "") %>%
             as.numeric()) %>%
    group_by(var) %>%
    mutate(value = round(value*100/sum(value), 2)) %>%
    ungroup() %>%
    mutate(string = ifelse(var == "n", glue("{string} ({value}%)"), string)) %>%
    select(-value)
}


desc <- list()
desc$cc <- get_desc(df, id_var = "NSID",
                    group_var = "Unem_6Months") %>%
  drop_na(group_var) %>%
  select(-miss) %>%
  n_prop()

desc$mi <- get_desc(imp_long, id_var = "NSID", 
                    imp_var = "imp", 
                    weight_var = "Survey_Weight_W8", 
                    group_var = "Unem_6Months") %>%
  select(-miss) %>%
  n_prop()

# Missingness
desc$miss <- df %>%
  get_desc("NSID") %>%
  select(var, miss) %>%
  distinct()

# Likelihood Ratio Test
lr_test <- function(var){
  weights <- imp_long %>%
    filter(imp == 1) %>%
    pull(Survey_Weight_W8)
  
  form <- paste("Unem_6Months ~", var) %>%
    as.formula()
  
  p <- imp_long %>%
    filter(imp > 0) %>%
    nest(data = -imp) %>%
    mutate(model = map(data,
                       function(x) glm(form, family = "binomial",
                                       data = x, weights = weights))) %>%
    pull(model) %>%
    as.mira() %>%
    D3() %>%
    summary() %>%
    pluck("comparisons") %>%
    pluck("p.value")
  
  ifelse(p <= 0.05, "*", "")
}

lr_tests  <- tibble(var = names(imp_long)) %>%
  filter(!(var %in% c("Unem_6Months", "Survey_Weight_W8", "imp", "NSID"))) %>%
  mutate(signif = map_chr(var, lr_test))
save(lr_tests, file = "Data/lr_tests.Rdata")

load("Data/lr_tests.Rdata")
desc$lr_tests <- lr_tests

# Combine
desc$tbl <- bind_rows(cc = desc$cc, mi = desc$mi, .id = "data") %>%
  mutate(cat = ifelse(var == cat & var != "Female", cat, glue("{var}{cat}"))) %>%
  inner_join(pretty_lbls, by = c("var", "cat")) %>%
  arrange(index) %>%
  left_join(desc$lr_tests, by = "var") %>%
  left_join(desc$miss, by = "var") %>%
  mutate(unem = match(group_var, levels(df$Unem_6Months)) %>%
           paste0("unem", ., "_", data),
         across(c(miss, signif), 
                ~ ifelse(level > 1 | is.na(.x), "", .x))) %>%
  select(-data, -group_var) %>%
  pivot_wider(names_from = "unem", values_from = "string") %>%
  mutate(unem2_mi = ifelse(level == 1, glue("{unem2_mi}{signif}", .na = ""), unem2_mi),
         var_clean = ifelse(var_ref == cat_clean, "", var_clean)) %>%
  select(var_clean, cat_clean, unem1_cc, unem2_cc, miss, unem1_mi, unem2_mi)

# Make Flextable
head_lbls <- list(var_clean = "", cat_clean = "Variable", unem1_cc = "<6 + Months\nUnemployment",
                  unem2_cc = "6+ Months\nUnemployment", miss = "% Missing",
                  unem1_mi = "<6 + Months\nUnemployment", unem2_mi = "6+ Months\nUnemployment")

span_lbls <- list(var_clean = "", cat_clean = "", unem1_cc = "Unweighted Observed Data",
                  unem2_cc = "Unweighted Observed Data",
                  unem1_mi = "Weighted Imputed Data",  unem2_mi = "Weighted Imputed Data")

desc$flextable <- make_flx(desc$tbl, head_lbls, span_lbls)
desc$flextable
save_as_docx(desc$flextable, path = "Tables/descriptive_statistics.docx")

rm(desc, head_lbls, span_lbls, lr_test, lr_tests, n_prop)

# 3. Quantile Analysis ----
# Load Data
load("Data/qreg_results.Rdata")

mod_names <- c(bivar = "Bivariate", mh = "+ Adolescent Mental Health",
               full = "Long-Term Association", scar = "+ Current Status",
               gender = "Long-Term Association", sep = "+ Socio-Economic Background",
               Unem_3Months = "3+", Unem_6Months = "6+",
               Unem_9Months = "9+", Unem_12Months = "12+")

qreg_res <- qreg_res %>%
  uncount(ifelse(mod == "full", 2, 1), .id = "id") %>%
  mutate(mod = ifelse(id == 2, "Unem_6Months", mod),
         mod_clean = mod_names[mod] %>% 
           factor(unique(mod_names)),
         q = glue("Q{tau*100}"),
         sex_u = str_to_title(sex)) %>%
  select(-id, -unem_var)

# Predictions
qreg_pred <- qreg_res %>%
  unnest(pred) %>%
  select(-coefs) %>%
  mutate(unem_clean = levels(df$Unem_6Months)[unem+1] %>%
           factor(levels(df$Unem_6Months)))

# Coefficients
qreg_obs <- function(df){
  df_c <- slice(df, 1)
  
  tibble(term = c("obs", "imps"),
         string = c(format(df_c$nobs, big.mark = ",") %>% trimws(),
                    df_c$imps)) %>%
    expand_grid(select(df_c, -term, -string,
                       -beta, -lci, -uci)) %>%
    bind_rows(df) %>%
    select(-imps, -nobs)
}

qreg_coefs <- qreg_res %>%
  unnest(coefs) %>%
  select(-pred) %>%
  mutate(string = glue("{round(beta, 2)}\n({round(lci, 2)}, {round(uci, 2)})"),
         imps = ifelse(sample == "cc", "-", "28")) %>%
  group_split(sample, sex, mod, tau) %>%
  map_dfr(qreg_obs) %>%
  arrange(mod_clean) %>%
  mutate(mod_t = ifelse(sex == "all", mod, sex) %>%
           fct_reorder(row_number())) %>%
  left_join(pretty_lbls, by = c("term" = "cat"))


# Plots
make_plot <- function(df, var, palette, ylab, file_name, hline = TRUE){
  
  p <- ggplot(df) +
    aes(x = tau, y = beta, ymin = lci, ymax = uci,
        color = {{ var }}, fill = {{ var }}, linetype = {{ var }})
  
  if (hline == TRUE) p <- p + geom_hline(yintercept = 0)
  
  p <- p +
    geom_ribbon(color = NA, alpha = 0.2) +
    geom_line() +
    scale_colour_paletteer_d(palette) +
    scale_fill_paletteer_d(palette) +
    scale_x_continuous(breaks = 1:9/10) +
    labs(x = "Quantile", y = ylab,
         color = NULL, fill = NULL, linetype = NULL) +
    theme_minimal() +
    theme(legend.position = c(.15, .85))
  
  glue("Images/{file_name}.png") %>%
    ggsave(plot = p, height = 9.9,
           width = 21, units = "cm", dpi = 600)
  
  p
}

qreg_coefs %>%
  filter(str_detect(term, "^Unem"),
         mod %in% c("full", "scar"),
         sample == "mi") %>%
  make_plot(mod_clean, "RColorBrewer::Set1", 
            "Marginal Effect",
            "quantile_margin")

qreg_coefs %>%
  filter(str_detect(term, "^Unem"),
         sex != "all",
         sample == "mi") %>%
  make_plot(sex_u,
            "jcolors::pal8", "Marginal Effect",
            "quantile_gender")

qreg_coefs %>%
  filter(str_detect(term, "^Unem"),
         mod %in% c("full", "sep"),
         sample == "mi") %>%
  make_plot(mod_clean, "RColorBrewer::Accent", 
            "Marginal Effect",
            "quantile_sep")

qreg_coefs %>%
  filter(str_detect(term, "^Unem"),
         str_detect(mod, "^Unem"),
         sample == "mi") %>%
  make_plot(mod_clean, "RColorBrewer::Set1", 
            "Marginal Effect",
            "quantile_unem") +
  labs(color = "Months Unemployment",
       fill = "Months Unemployment",
       linetype = "Months Unemployment")
ggsave("Images/quantile_unem.png", height = 9.9, 
       width = 21, units = "cm", dpi = 600)

qreg_pred %>%
  filter(mod == "full", 
         sample == "mi") %>%
  make_plot(unem_clean, "RColorBrewer::Dark2", 
            "Predicted GHQ Score",
            "quantile_pred", FALSE) +
  scale_y_continuous(breaks = c(4, 8, 12, 16, 20, 24))



# Tables
make_table <- function(df, var_cols, file_name, headers){
  flx <- flextable(df) %>%
    merge_v(j = 1) %>%
    set_header_labels(values = headers) %>%
    border_remove() %>%
    fontsize(size = 11, part = "all") %>%
    merge_h(part = "header") %>%
    border_inner_h(border = fp_border(color = "gray30", style = "dashed")) %>%
    hline_top(border = fp_border(color = "black", width = 2), part = "all") %>%
    hline_bottom(border = fp_border(color = "black", width = 2), part = "all") %>%
    border(i = nrow(df) - 1, 
           border.top = fp_border(color = "black", style = "solid")) %>%
    border(i = nrow(df), border.top = fp_border(color = "grey90")) %>%
    fix_border_issues(part = "all") %>%
    align(j = 1:var_cols, align="right", part = "all") %>%
    align(j = (var_cols + 1):ncol(df), align="center", part = "all") %>%
    valign(j = 1, valign = "top") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    autofit()
  
  glue("Tables/{file_name}.docx") %>%
    save_as_docx(flx, path = .)
  
  flx
}

qreg_tbl <- list()

# Short Table
qreg_tbl$short <- function(sample){
  file_name <- glue("quantile_main_{sample}")
  
  qreg_coefs %>% 
    filter(sample == !!sample,
           str_detect(term, "^(Unem|imps|obs)")) %>%
    mutate(q = ifelse(term %in% c("imps", "obs"), cat_clean, q) %>%
             factor(c(glue("Q{1:9*10}"), "Observations", "Imputations"))) %>%
    select(q, mod_t, string) %>% distinct() %>%
    arrange(mod_t, q) %>%
    pivot_wider(id_cols = q, names_from = mod_t, values_from = string) %>%
    select(q, bivar, mh, full, scar, male, female) %>%
    make_table(1, file_name, 
               list(q = "Quantile",
                    bivar= "(1)", mh = "(2)",
                    full = "(3)", scar = "(4)",
                    male = "(5)", female = "(6)")) %>%
    border(i = 1:9, border.top = fp_border(color = "grey90"))
}
map(c("cc", "mi"), qreg_tbl$short)

# Long Table
qreg_tbl$long <- function(sample, sex, mod){
  file_name <- glue("quantile_full_{sample}_{sex}_{mod}")
  
  qreg_coefs %>%
    filter(sample == !!sample,
           sex == !!sex,
           mod == !!mod) %>%
    mutate(var_ref = ifelse(var_ref == cat_clean, "", var_clean),
           var_clean = ifelse(levels <= 2, "", var_clean),
           var_ref = ifelse(term %in% c("obs", "imps"), " ", var_clean)) %>%
    arrange(q, index, level) %>%
    pivot_wider(id_cols = c(var_clean, cat_clean, index, level), 
                names_from = q, values_from = string) %>%
    arrange(index, level) %>%
    select(var_clean, cat_clean, matches("^Q")) %>%
    make_table(2, file_name,
               list(var_clean = "", cat_clean = "Variable"))
}

qreg_coefs %>%
  select(sample, sex, mod) %>%
  distinct() %>% 
  filter(!str_detect(mod, "^Unem")) %$%
  pwalk(list(sample, sex, mod), qreg_tbl$long)


rm(qreg_obs, qreg_coefs, qreg_res, qreg_tbl,
   qreg_pred, mod_names, make_plot, make_table)

# 5. Specification Curve Analysis ---
# Load Data
load("Data/sca_results.Rdata")

sca_res <- sca_res %>%
  mutate(status_clean = fct_reorder(status_clean, status))


# Plots
sca_plot <- ggplot(sca_res) +
  aes(x = tau, y = coef, group = id,
      color = status_clean) +
  geom_hline(yintercept = 0) +
  geom_line(alpha = 0.01) +
  geom_line(data = filter(sca_res, mod == TRUE),
            aes(linetype = status),
            color = "black") +
  scale_color_manual(values = paletteer_c("scico::roma", 2)) +
  scale_x_continuous(breaks = 1:9/10) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         linetype = FALSE) +
  labs(x = "Quantile", y = "Marginal Effect", color = NULL) +
  theme_minimal() +
  theme(legend.position = c(.15, .85))
ggsave("Images/quantile_sca.png", plot = sca_plot, 
       height = 9.9,  width = 21, units = "cm", dpi = 300)

ggplot(sca_res) +
  aes(x = tau, y = coef, group = tau,
      color = status_clean, fill = status_clean) +
  facet_wrap(~ status_clean) +
  geom_hline(yintercept = 0) +
  geom_boxplot(position = position_dodge(0.6), alpha = 0.2) +
  scale_color_manual(values = paletteer_c("scico::roma", 2)) +
  scale_fill_manual(values =  paletteer_c("scico::roma", 2)) +
  scale_x_continuous(breaks = 1:9/10) +
  labs(x = "Quantile", y = "Marginal Effect", 
       color = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = c(.15, .85),
        strip.text = element_blank())
ggsave("Images/sca_boxplot.png", height = 9.9,
       width = 21, units = "cm", dpi = 300)

imp_long %>%
  filter(imp > 0) %>%
  summarise(sd = wtd.var(GHQ_W8_Likert, Survey_Weight_W8) %>%
              sqrt())
0.75/6.41
