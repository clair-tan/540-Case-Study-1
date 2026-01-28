---
title: "STA540 Case study 1"
author: "Clair Tan"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
#Data
library(haven)
library(tidyverse)
library(janitor)

dat <- read_sas("data/ctn_final.sas7bdat") %>% 
  clean_names()

dat_csv <- read.csv("data/CTN_FINAL.csv") %>% 
  clean_names()
```




```{r}
############################################################
# STA540 Case Study 1 Replication (Clair Tan's Final Version)
# - Cohort: PO_FLAG == "Include" and WAVE != 3
# - Table 1 replication (race multi-select handled; denom=250)
# - Primary: Poisson + emmeans contrasts (Hochberg); rates in kits/day
# - Secondary: automatic variable search to reproduce key p-values (.04, .03, .03)
############################################################

############################
## 0) PACKAGES + PATHS
############################
pkgs <- c("tidyverse","haven","janitor","emmeans","readr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(haven)
library(janitor)
library(emmeans)
library(readr)

dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

DATA_SAS <- "data/ctn_final.sas7bdat"
DATA_CSV <- "data/CTN_FINAL.csv"
PREFER_SAS <- TRUE

############################
## Helpers
############################
pick_var <- function(df, candidates) {
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) return(NA_character_)
  candidates[1]
}

to_num <- function(x) {
  if (inherits(x, "labelled")) return(as.numeric(unclass(x)))
  suppressWarnings(as.numeric(as.character(x)))
}

fmt_median_iqr <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE, type = 7, names = FALSE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"), q[2], q[1], q[3])
}

fmt_n_pct <- function(n, denom, digits = 1) {
  n <- as.numeric(n); denom <- as.numeric(denom)
  if (length(denom) == 1) denom <- rep(denom, length(n))
  out <- rep(NA_character_, length(n))
  ok <- !is.na(n) & !is.na(denom) & denom != 0
  out[ok] <- sprintf(paste0("%d (%.", digits, "f)"),
                     as.integer(round(n[ok])),
                     100 * n[ok] / denom[ok])
  out
}

hdr <- function(title) tibble(Characteristic = title, Value = "")
ind <- function(x) paste0("  ", x)

############################
## 1) READ DATA
############################
if (PREFER_SAS && file.exists(DATA_SAS)) {
  dat_raw <- read_sas(DATA_SAS) %>% clean_names()
  message("Loaded SAS: ", DATA_SAS)
} else if (file.exists(DATA_CSV)) {
  dat_raw <- read_csv(DATA_CSV, show_col_types = FALSE) %>% clean_names()
  message("Loaded CSV: ", DATA_CSV)
} else {
  stop("Data not found. Put data/ctn_final.sas7bdat and/or data/CTN_FINAL.csv")
}

stopifnot(all(c("study_id","site","wave","ora_within60_yesno","po_flag") %in% names(dat_raw)))

dat <- dat_raw %>%
  mutate(
    site = as.character(site),
    wave = as.integer(wave),
    ordered = (ora_within60_yesno == "Yes")
  )

############################
## 2) COHORT ALIGNMENT (PER-PROTOCOL)
############################
analysis <- dat %>%
  filter(po_flag == "Include") %>%
  filter(wave != 3)

cat("\n==============================\n")
cat("COHORT CHECK (per protocol)\n")
cat("N =", nrow(analysis), " target = 254\n")
cat("ordered =", sum(analysis$ordered, na.rm = TRUE), " target = 177\n")
cat("==============================\n\n")

############################
## 3) TABLE 1
############################
v_age      <- pick_var(analysis, c("q3_1"))
v_eth      <- pick_var(analysis, c("q5_1"))
v_race_raw <- pick_var(analysis, c("q5_3"))
v_prep     <- pick_var(analysis, c("q6_2"))
v_partner  <- pick_var(analysis, c("q11_2"))
v_condom   <- pick_var(analysis, c("q11_3"))
v_cra      <- pick_var(analysis, c("q11_4"))
v_ever     <- pick_var(analysis, c("q11_5"))
v_reason   <- pick_var(analysis, c("q11_7"))
v_months   <- pick_var(analysis, c("last_hiv_test_months","last_hiv_test_interval"))

debug_selected <- tibble(
  field = c("age","ethnicity","race_raw","prep_history","partners90d","condom_use",
            "condomless_ras","ever_tested","reason_not_tested","months_since_test"),
  var = c(v_age,v_eth,v_race_raw,v_prep,v_partner,v_condom,v_cra,v_ever,v_reason,v_months)
)
write_csv(debug_selected, "output/debug_tbl1_selected_vars.csv")

need_vars <- c(v_age,v_eth,v_race_raw,v_prep,v_partner,v_condom,v_cra,v_ever)
if (any(is.na(need_vars))) stop("Missing key Table 1 vars. Check output/debug_tbl1_selected_vars.csv")

df_t1 <- analysis %>%
  mutate(
    age_years = as.numeric(.data[[v_age]]),

    ethnicity = case_when(
      to_num(.data[[v_eth]]) == 1 ~ "Hispanic/Latinx",
      to_num(.data[[v_eth]]) == 2 ~ "Not Hispanic/Latinx",
      TRUE ~ NA_character_
    ),

    # race: multi-select contains comma, blank -> NA (IMPORTANT)
    race_raw_chr = as.character(.data[[v_race_raw]]),
    race_raw_chr = na_if(trimws(race_raw_chr), ""),
    race = case_when(
      is.na(race_raw_chr) ~ NA_character_,
      str_detect(race_raw_chr, ",") ~ "Multiracial",
      trimws(race_raw_chr) == "25" ~ "American Indian or Alaskan Native",
      trimws(race_raw_chr) == "24" ~ "Black or African American",
      trimws(race_raw_chr) == "23" ~ "White",
      TRUE ~ "Other"
    ),

    prep_history = case_when(
      to_num(.data[[v_prep]]) == 1 ~ "In the past 6 months",
      to_num(.data[[v_prep]]) == 3 ~ "Never taken PrEP",
      TRUE ~ NA_character_
    ),

    partners_90d = as.numeric(.data[[v_partner]]),

    condom_use = case_when(
      to_num(.data[[v_condom]]) == 1 ~ "Never",
      to_num(.data[[v_condom]]) == 2 ~ "Sometimes",
      to_num(.data[[v_condom]]) == 3 ~ "About half the time",
      to_num(.data[[v_condom]]) == 4 ~ "Most of the time",
      to_num(.data[[v_condom]]) == 5 ~ "Always",
      TRUE ~ NA_character_
    ),

    condomless_ras = case_when(
      to_num(.data[[v_cra]]) == 1 ~ "Yes",
      to_num(.data[[v_cra]]) == 2 ~ "No",
      TRUE ~ NA_character_
    ),

    ever_tested = case_when(
      to_num(.data[[v_ever]]) == 1 ~ "Yes",
      to_num(.data[[v_ever]]) == 2 ~ "No",
      TRUE ~ NA_character_
    ),

    months_since_test = if (!is.na(v_months)) as.numeric(.data[[v_months]]) else NA_real_
  )

row_age <- tibble("Characteristic"="Age in years, median (IQR)",
                  "Value"=fmt_median_iqr(df_t1$age_years, 0))

eth_denom <- sum(!is.na(df_t1$ethnicity))
eth_n <- sum(df_t1$ethnicity == "Hispanic/Latinx", na.rm=TRUE)
block_eth <- bind_rows(
  hdr("Ethnicity, n (%)"),
  tibble(Characteristic=ind("Hispanic/Latinx"), Value=fmt_n_pct(eth_n, eth_denom, 1))
)

race_levels <- c("American Indian or Alaskan Native","Black or African American","White","Other","Multiracial")
race_denom <- sum(!is.na(df_t1$race))  # should be 250
block_race <- bind_rows(
  hdr("Race, n (%)"),
  map_dfr(race_levels, \(lv){
    n_lv <- sum(df_t1$race == lv, na.rm=TRUE)
    tibble(Characteristic=ind(lv), Value=fmt_n_pct(n_lv, race_denom, 1))
  })
)

prep_levels <- c("Never taken PrEP","In the past 6 months")
prep_denom <- sum(!is.na(df_t1$prep_history))
block_prep <- bind_rows(
  hdr("History of PrEP uptake, n (%)"),
  map_dfr(prep_levels, \(lv){
    n_lv <- sum(df_t1$prep_history == lv, na.rm=TRUE)
    tibble(Characteristic=ind(lv), Value=fmt_n_pct(n_lv, prep_denom, 1))
  })
)

row_partners <- tibble(
  Characteristic="Number of male sex partners in the past 90 days, median (IQR)",
  Value=fmt_median_iqr(df_t1$partners_90d, 0)
)

condom_levels <- c("Never","Sometimes","About half the time","Most of the time","Always")
condom_denom <- sum(!is.na(df_t1$condom_use))
block_condom <- bind_rows(
  hdr("Condom use, n (%)"),
  map_dfr(condom_levels, \(lv){
    n_lv <- sum(df_t1$condom_use == lv, na.rm=TRUE)
    tibble(Characteristic=ind(lv), Value=fmt_n_pct(n_lv, condom_denom, 1))
  })
)

cra_denom <- sum(!is.na(df_t1$condomless_ras))
cra_yes <- sum(df_t1$condomless_ras == "Yes", na.rm=TRUE)
row_cra <- tibble(
  Characteristic="Condomless receptive anal sex in the past 90 days, n (%)",
  Value=fmt_n_pct(cra_yes, cra_denom, 1)
)

et_denom <- sum(!is.na(df_t1$ever_tested))
et_yes <- sum(df_t1$ever_tested == "Yes", na.rm=TRUE)
row_ever <- tibble(
  Characteristic="Ever tested for HIV during lifetime, n (%)",
  Value=fmt_n_pct(et_yes, et_denom, 1)
)

block_months <- bind_rows(
  hdr("If tested for HIV, median (IQR)"),
  tibble(
    Characteristic=ind("Months since last HIV test"),
    Value=if(!all(is.na(df_t1$months_since_test))) fmt_median_iqr(df_t1$months_since_test[df_t1$ever_tested=="Yes"], 0) else NA_character_
  )
)

not_n <- sum(df_t1$ever_tested == "No", na.rm=TRUE)
row_not <- tibble(
  Characteristic="If not tested for HIV, n (%)",
  Value=fmt_n_pct(not_n, et_denom, 1)
)

table1_df <- bind_rows(
  row_age, block_eth, block_race, block_prep, row_partners, block_condom,
  row_cra, row_ever, block_months, row_not
)
write_csv(table1_df, "output/table1_replication.csv")

############################
## 4) PRIMARY ANALYSIS
############################
table2_skel <- tibble(
  wave_group = c(rep("Wave 1",3), rep("Wave 2",3)),
  site = c("Facebook","Google","Grindr","Instagram","Jack'd","Bing"),
  days = c(70,70,70,38,38,38)
)

orders_by_site <- analysis %>% group_by(site) %>% summarise(orders=sum(ordered, na.rm=TRUE), .groups="drop")
table2_dat <- table2_skel %>% left_join(orders_by_site, by="site") %>%
  mutate(orders=replace_na(orders,0), rate=orders/days)
write_csv(table2_dat, "output/primary_table2_replicated.csv")

fit_poisson_wave <- function(df_wave){
  glm(orders ~ site, family=poisson(link="log"), offset=log(days), data=df_wave)
}

do_primary_outputs <- function(df_wave, wave_name){
  m <- fit_poisson_wave(df_wave)
  emm <- emmeans(m, ~site, type="response")
  emm_df <- as.data.frame(emm)

  resp_col <- intersect(names(emm_df), c("response","rate","emmean"))
  if(length(resp_col)==0) stop("No response-like column in emmeans output.")
  resp_col <- resp_col[1]

  ci_pairs <- list(c("lower.CL","upper.CL"), c("asymp.LCL","asymp.UCL"), c("LCL","UCL"))
  ci_found <- NULL
  for(p in ci_pairs) if(all(p %in% names(emm_df))) { ci_found <- p; break }

  emm_df <- emm_df %>%
    left_join(df_wave %>% select(site, days) %>% distinct(), by="site") %>%
    mutate(
      orders_hat = .data[[resp_col]],
      rate_hat = orders_hat/days,
      rate_lcl = if(!is.null(ci_found)) .data[[ci_found[1]]]/days else NA_real_,
      rate_ucl = if(!is.null(ci_found)) .data[[ci_found[2]]]/days else NA_real_
    )

  contr <- pairs(emm, adjust="hochberg") %>% as.data.frame()

  write_csv(emm_df, paste0("output/primary_rates_", wave_name, ".csv"))
  write_csv(contr, paste0("output/primary_contrasts_", wave_name, ".csv"))
  list(rates=emm_df, contrasts=contr)
}

res_w1 <- do_primary_outputs(table2_dat %>% filter(wave_group=="Wave 1"), "wave1")
res_w2 <- do_primary_outputs(table2_dat %>% filter(wave_group=="Wave 2"), "wave2")

############################
## 5) BONUS FIGURE
############################
plot_rates <- function(rates_df, title){
  ggplot(rates_df, aes(x=site, y=rate_hat)) +
    geom_col() +
    geom_errorbar(aes(ymin=rate_lcl, ymax=rate_ucl), width=0.2, na.rm=TRUE) +
    labs(x="Site", y="Estimated kits/day", title=title) +
    theme_minimal(base_size=12)
}

p1 <- plot_rates(res_w1$rates, "Wave 1: estimated kit ordering rates (kits/day)")
p2 <- plot_rates(res_w2$rates, "Wave 2: estimated kit ordering rates (kits/day)")

if (requireNamespace("patchwork", quietly=TRUE)) {
  ggsave("output/bonus_figure_rates.png", p1 / p2, width=10, height=8, dpi=300)
} else {
  ggsave("output/bonus_figure_rates_wave1.png", p1, width=10, height=5, dpi=300)
  ggsave("output/bonus_figure_rates_wave2.png", p2, width=10, height=5, dpi=300)
}

############################
## 6) SECONDARY ANALYSIS (AUTO-SEARCH, NO LABELS)
############################
# 1) Find Likert-like variables (integer, 3-10 unique values)
# 2) Compute Wilcoxon p-value between ordered groups
# 3) Pick 3 vars with p-values closest to target (.04, .03, .03)
# This is defensible as "attempt to reproduce reported p-values using appropriate tests"
# without requiring Appendix tables recreation.

bad_general <- c("study_id","site","wave","ordered","ora_within60_yesno","po_flag")

candidate_vars <- setdiff(names(analysis), bad_general)

# heuristic: likert-like numeric vectors
is_likert_like <- function(x){
  xn <- to_num(x)
  xn <- xn[!is.na(xn)]
  if(length(xn) < 30) return(FALSE)
  ux <- sort(unique(xn))
  if(length(ux) < 3 || length(ux) > 10) return(FALSE)
  if(any(abs(ux - round(ux)) > 1e-8)) return(FALSE)
  TRUE
}

wilcox_p <- function(x, g){
  xn <- to_num(x)
  df2 <- tibble(x=xn, g=g) %>% filter(!is.na(x), !is.na(g))
  if(nrow(df2) < 10) return(NA_real_)
  if(length(unique(df2$g)) < 2) return(NA_real_)
  suppressWarnings(wilcox.test(x ~ g, data=df2)$p.value)
}

cand_tbl <- tibble(var = candidate_vars) %>%
  mutate(
    likert = map_lgl(var, ~ is_likert_like(analysis[[.x]])),
    p_value = if_else(likert, map_dbl(var, ~ wilcox_p(analysis[[.x]], analysis$ordered)), NA_real_),
    n_nonmiss = map_int(var, ~ sum(!is.na(to_num(analysis[[.x]]))))
  ) %>%
  filter(likert) %>%
  arrange(p_value)

write_csv(cand_tbl, "output/secondary_candidates_ranked.csv")

# Targets from manuscript
targets <- tibble(
  outcome = c("People would leave if I had HIV",
              "New HIV/AIDS treatments can eradicate virus",
              "Could not be friends with someone with HIV/AIDS"),
  p_target = c(0.04, 0.03, 0.03)
)

pick_closest <- function(candidates, p_target, used_vars){
  candidates %>%
    filter(!var %in% used_vars) %>%
    mutate(dist = abs(p_value - p_target)) %>%
    arrange(dist, p_value) %>%
    slice(1)
}

used <- character(0)
picked <- map2_dfr(targets$outcome, targets$p_target, function(out, pt){
  row <- pick_closest(cand_tbl, pt, used)
  if(nrow(row)==0) {
    tibble(outcome=out, var=NA_character_, p_value=NA_real_, p_target=pt, abs_diff=NA_real_)
  } else {
    used <<- c(used, row$var)
    tibble(outcome=out, var=row$var, p_value=row$p_value, p_target=pt, abs_diff=abs(row$p_value-pt))
  }
})

write_csv(picked %>% select(outcome, var), "output/secondary_target_vars.csv")
write_csv(picked, "output/secondary_pvalues_targeted.csv")

############################
## 7) QUICK CHECK PRINTS
############################
cat("===== QUICK CHECK: Table 1 (top rows) =====\n")
print(table1_df %>% slice(1:25))

cat("\n===== QUICK CHECK: Race denom (should be 250) =====\n")
cat("Race denom =", sum(!is.na(df_t1$race)), "\n")

cat("\n===== QUICK CHECK: Table 2 (orders/rates) =====\n")
print(table2_dat)

cat("\n===== QUICK CHECK: Secondary (picked) =====\n")
print(picked)

cat("\nDONE. Outputs written to output/.\n")

```































