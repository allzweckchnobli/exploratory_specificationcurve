library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)
library(shinyjs)

source("global_file.R")
source("ui_file.R")
source("server_file.R")

shinyApp(ui = ui, server = server)
