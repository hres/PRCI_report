# load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(DT)
library(lubridate)
library(plotly)
library(shiny)
library(shinycssloaders)

#load modules:
source('modules/timeline_module.R')
source('modules/published_package_module.R')
source('modules/inprogress_module.R')

# read Excel data and turn into useable format
report <- read_excel("./data/PRCI_data.xlsx", sheet = 1, skip = 5,
    na = "N/A") %>%
    filter(!is.na(SCN))
pipeline <- read_excel("./data/PRCI_pipeline.xlsx", na = "N/A") %>%
    filter(!is.na(Publication)) %>%
    arrange(Publication) %>%
    group_by(Publication,`Product category`) %>%
    summarise(Products = paste(Product, collapse = "\n"),
        Date = Publication[1])
report <- report %>%
    mutate(stage = factor(case_when(
        !is.na(`Publish date`) ~ 6,
        !is.na(`Time in Stage 4 (Redaction comments sent to manufacturer to Final review start)`) ~ 5,
        !is.na(`Time in Stage 3 (Proposal received to Redaction comments sent to manufacturer)`) ~ 4,
        !is.na(`Time in Stage 2 (Proposal requested to Proposal received)`) ~ 3,
        !is.na(`Active date`) ~ 2, 
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
            stage == 3 ~ !is.na(`Stage 2 late`) & `Stage 2 late` == 1,
            stage == 4 ~ !is.na(`Stage 3 late`) & `Stage 3 late` == 1,
            stage == 5 ~ !is.na(`Stage 4 late`) & `Stage 4 late` == 1,
            TRUE ~ !is.na(`Package late`) & `Package late` == 1)),
        proactive = factor(ifelse(!is.na(`Request origin`) &
            `Request origin` == "HPFB", 1, 0))) %>%
    select(stage, state, late, proactive,`Product type`) %>%
    filter(state != "Screening")
