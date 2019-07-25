library(readxl)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(DT)
library(lubridate)
library(plotly)
library(shiny)

report <- read_excel("./data/PRCI_data.xlsx", sheet = 1, skip = 5,
    na = "N/A") %>%
    filter(!is.na(SCN))
pipeline <- read_excel("./data/PRCI_pipeline.xlsx", na = "N/A")

report_published <- report %>%
    filter(!is.na(.[[10]]) | !is.na(.[[11]]))
late_2 <- sum(report_published$`Stage 2 late`, na.rm = TRUE)
late_3 <- sum(report_published$`Stage 3 late`, na.rm = TRUE)
late_4 <- sum(report_published$`Stage 4 late`, na.rm = TRUE)
late_5 <- sum(report_published$`Stage 5 late`, na.rm = TRUE)
stages_late <- data.frame(stage = factor(c("Manufacturer Proposes Redactions",
        "Health Canada Assessment", "Revised Redaction Proposal",
        "QA and Publication")),
    late = c(late_2, late_3, late_4, late_5))

report <- report %>%
    mutate(stage = factor(case_when(
        (!is.na(.[[10]]) | !is.na(.[[11]])) ~ 6,
        (!is.na(.[[16]]) | !is.na(.[[17]])) ~ 5,
        (!is.na(.[[14]]) | !is.na(.[[15]])) ~ 4,
        (!is.na(.[[12]]) | !is.na(.[[13]])) ~ 3,
        (!is.na(.[[8]]) | !is.na(.[[9]])) ~ 2, 
        TRUE ~ 1)),
    state = recode_factor(stage,
        `1` = "Screening", 
        `2` = "Manufacturer Proposes Redactions",
        `3` = "Health Canada Assessment",
        `4` = "Revised Redaction Proposal",
        `5` = "QA and Publication",
        `6` = "Published"),
    late = factor(case_when(
        stage == 1 ~ FALSE,
        stage == 2 ~ FALSE,
        stage == 3 ~ !is.na(.[[13]]) & .[[13]] == 1,
        stage == 4 ~ !is.na(.[[15]]) & .[[15]] == 1,
        stage == 5 ~ !is.na(.[[17]]) & .[[17]] == 1,
        TRUE ~ !is.na(.[[11]]) & .[[11]] == 1)),
    proactive = factor(ifelse(!is.na(`Request origin`) &
        `Request origin` == "HPFB", 1, 0))) %>%
    select(stage, state, late, proactive) %>%
    filter(state != "Screening")
