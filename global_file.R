library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)

data <- read.csv("plotdata_final.csv")

filter_primary_measure <- c("AUC-ROC","h-Score","Accuracy")
filter_secondary_measure <- c("None","AUC-ROC","h-Score","Accuracy")

filter_type_options <- c("All", "All vs. All", "One vs. All", "One vs. One")
filter_dataverse_options <- c("All","Full Data","GT different all","GT different baseline","SR different all","SR different baseline")
filter_classifier_options <- c("All","Deepseek R1","Mean Top-5 + Random Forest","Random Forest","Top-5 Similarity")
filter_specifications_ds_options <- c("All","Definitions","Examples","Both")
filter_specifications_other_options <- c("All","CLS","MEAN","MAX")
filter_preprocessing_options <- c("All","None","Normalized","Pre-Processed","Top200")
filter_mechanism_options <- c(
  "All" = "All",
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