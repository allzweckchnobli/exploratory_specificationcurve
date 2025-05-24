library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)

data <- read.csv("plotdata_final.csv")

filter_primary_measure <- c("AUC-ROC","h-Score","Accuracy")
filter_secondary_measure <- c("None","AUC-ROC","h-Score","Accuracy")

filter_type_options <- c("One vs. All", "One vs. One","All vs. All")
filter_dataverse_options <- c("Full Data","GT different baseline","SR different baseline","SR different all","GT different all")
filter_classifier_options <- c("Deepseek R1","Mean Top-5 + Random Forest","Random Forest","Top-5 Similarity")
filter_specifications_ds_options <- c("Definitions","Examples","Both")
filter_specifications_other_options <- c("MEAN","CLS","MAX")
filter_preprocessing_options <- c("None","Normalized","Pre-Processed","Top200")
filter_mechanism_options <- c(
  "Baseline" = "CA",
  "Knowledge" = "EN",
  "Need" = "GM",
  "Social Norms" = "SI")

## Helper-Tables
### measures and SD 
helper_measures <- data.frame(
  name = c("AUC-ROC","h-Score","Accuracy"),
  intname = c("aucroc","ent","accuracy"),
  sdname = c("sd_aucroc","sd_ent","sd_accuracy")
)

