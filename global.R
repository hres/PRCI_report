library(readxl)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(DT)

report <- read_excel("./data/real_data.xlsx", sheet = 1, skip = 5,
    na = "N/A")

late_2 <- report %>% filter(`Stage 2 late` == 1) %>% nrow()
late_3 <- report %>% filter(`Stage 3 late` == 1) %>% nrow()
late_4 <- report %>% filter(`Stage 4 late` == 1) %>% nrow()
late_5 <- report %>% filter(`Stage 5 late` == 1) %>% nrow()
stages_late <- data.frame(stage = factor(c("Manufacturer Proposes Redactions",
        "Health Canada Assessment", "Revised Redaction Proposal",
        "QA and Publication")), late = c(late_2, late_3,
    late_4, late_5))

report <- report %>%
    mutate(stage = factor(case_when(
        .[[10]] != 0 ~ 6,
        .[[18]] != 0 ~ 5,
        .[[16]] != 0 ~ 4,
        .[[14]] != 0 ~ 3,
        .[[12]] != 0 ~ 2, 
        TRUE ~ 1)),
    state = recode_factor(stage,
        `1` = "Screening", 
        `2` = "Manufacturer Proposes Redactions",
        `3` = "Health Canada Assessment",
        `4` = "Revised Redaction Proposal",
        `5` = "QA and Publication",
        `6` = "Published"),
    late = factor(case_when(
        stage == 1 ~ 0,
        stage == 2 ~ .[[13]],
        stage == 3 ~ .[[15]],
        stage == 4 ~ .[[17]],
        stage == 5 ~ .[[19]],
        TRUE ~ .[[11]])),
    proactive = factor(ifelse(.[[27]] == "HPFB", 1, 0)),
    date = as.Date(`Publish date`)) %>%
    select(37, 38, 39, 40) %>%
    filter(state != "Screening")