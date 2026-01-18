# Required Libraries
# install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "RColorBrewer", "plotly"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(DT)

###load data
load("data/phylum_name.rda")
load("data/class_name.rda")
load("data/order_name.rda")
load("data/family_name.rda")
load("data/genus_name.rda")
load("data/species_name.rda")

load("data/expression_data_phylum.rda")
load("data/sample_info_phylum.rda")
load("data/variable_info_phylum.rda")

load("data/expression_data_class.rda")
load("data/sample_info_class.rda")
load("data/variable_info_class.rda")

load("data/expression_data_order.rda")
load("data/sample_info_order.rda")
load("data/variable_info_order.rda")

load("data/expression_data_family.rda")
load("data/sample_info_family.rda")
load("data/variable_info_family.rda")

load("data/expression_data_genus.rda")
load("data/sample_info_genus.rda")
load("data/variable_info_genus.rda")

load("data/expression_data_species.rda")
load("data/sample_info_species.rda")
load("data/variable_info_species.rda")

###small tools, get massdataset accoring to level
get_level_dataset <- function(level) {
  switch(
    level,
    "Phylum"  = list(
      expression_data = expression_data_phylum,
      sample_info = sample_info_phylum,
      variable_info = variable_info_phylum
    ),
    "Class"   = list(
      expression_data = expression_data_class,
      sample_info = sample_info_class,
      variable_info = variable_info_class
    ),
    "Order"   = list(
      expression_data = expression_data_order,
      sample_info = sample_info_order,
      variable_info = variable_info_order
    ),
    "Family"  = list(
      expression_data = expression_data_family,
      sample_info = sample_info_family,
      variable_info = variable_info_family
    ),
    "Genus"   = list(
      expression_data = expression_data_genus,
      sample_info = sample_info_genus,
      variable_info = variable_info_genus
    ),
    "Species" = list(
      expression_data = expression_data_species,
      sample_info = sample_info_species,
      variable_info = variable_info_species
    ),
    NULL
  )
}


group_color <-
  c(
    "Negative" = "#2ca02c",
    "Positive" = "#ff7f0e",
    "Low_risk" = "#1f77b4",
    "High_risk" = "#d62728",
    "Non-Persistent" = "#9467bd",
    "Persistent" = "#8c564b"
  )

group_levels <-
  c(
    "Negative",
    "Positive",
    "Low_risk",
    "High_risk",
    "Non-Persistent",
    "Persistent"
  )
