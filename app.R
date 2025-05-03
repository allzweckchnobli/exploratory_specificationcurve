library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(patchwork)
library(shinythemes)
library(shinyjs)

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
