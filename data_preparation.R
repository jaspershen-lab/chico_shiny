# load("data/species_microbiome_data.rda")
# load("data/phylum_microbiome_data.rda")
# load("data/class_microbiome_data.rda")
# load("data/order_microbiome_data.rda")
# load("data/family_microbiome_data.rda")
# load("data/genus_microbiome_data.rda")
# 
# ###species data
# species_microbiome_data <-
#   species_microbiome_data %>%
#   massdataset::activate_mass_dataset("sample_info") %>%
#   dplyr::filter(risk != "Blank")
# 
# expression_data_species <-
#   species_microbiome_data@expression_data
# sample_info_species <-
#   species_microbiome_data@sample_info
# variable_info_species <-
#   species_microbiome_data@variable_info
# 
# sample_info_species$persistent[sample_info_species$persistent == "P"] <- "Persistent"
# sample_info_species$persistent[sample_info_species$persistent == "NP"] <- "Non-Persistent"
# 
# save(expression_data_species, file = "data/expression_data_species.rda")
# save(sample_info_species, file = "data/sample_info_species.rda")
# save(variable_info_species, file = "data/variable_info_species.rda")
# 
# ###phylum data
# phylum_microbiome_data <-
#   phylum_microbiome_data %>%
#   massdataset::activate_mass_dataset("sample_info") %>%
#   dplyr::filter(risk != "Blank")
# 
# expression_data_phylum <-
#   phylum_microbiome_data@expression_data
# sample_info_phylum <-
#   phylum_microbiome_data@sample_info
# variable_info_phylum <-
#   phylum_microbiome_data@variable_info
# 
# sample_info_phylum$persistent[sample_info_phylum$persistent == "P"] <- "Persistent"
# sample_info_phylum$persistent[sample_info_phylum$persistent == "NP"] <- "Non-Persistent"
# 
# save(expression_data_phylum, file = "data/expression_data_phylum.rda")
# save(sample_info_phylum, file = "data/sample_info_phylum.rda")
# save(variable_info_phylum, file = "data/variable_info_phylum.rda")
# 
# ##class data
# class_microbiome_data <-
#   class_microbiome_data %>%
#   massdataset::activate_mass_dataset("sample_info") %>%
#   dplyr::filter(risk != "Blank")
# expression_data_class <-
#   class_microbiome_data@expression_data
# sample_info_class <-
#   class_microbiome_data@sample_info
# variable_info_class <-
#   class_microbiome_data@variable_info
# sample_info_class$persistent[sample_info_class$persistent == "P"] <- "Persistent"
# sample_info_class$persistent[sample_info_class$persistent == "NP"] <- "Non-Persistent"
# save(expression_data_class, file = "data/expression_data_class.rda")
# save(sample_info_class, file = "data/sample_info_class.rda")
# save(variable_info_class, file = "data/variable_info_class.rda")
# 
# 
# ##order data
# order_microbiome_data <-
#   order_microbiome_data %>%
#   massdataset::activate_mass_dataset("sample_info") %>%
#   dplyr::filter(risk != "Blank")
# expression_data_order <-
#   order_microbiome_data@expression_data
# sample_info_order <-
#   order_microbiome_data@sample_info
# variable_info_order <-
#   order_microbiome_data@variable_info
# sample_info_order$persistent[sample_info_order$persistent == "P"] <- "Persistent"
# sample_info_order$persistent[sample_info_order$persistent == "NP"] <- "Non-Persistent"
# 
# save(expression_data_order, file = "data/expression_data_order.rda")
# save(sample_info_order, file = "data/sample_info_order.rda")
# save(variable_info_order, file = "data/variable_info_order.rda")
# 
# ##family data
# family_microbiome_data <-
#   family_microbiome_data %>%
#   massdataset::activate_mass_dataset("sample_info") %>%
#   dplyr::filter(risk != "Blank")
# 
# expression_data_family <-
#   family_microbiome_data@expression_data
# sample_info_family <-
#   family_microbiome_data@sample_info
# variable_info_family <-
#   family_microbiome_data@variable_info
# sample_info_family$persistent[sample_info_family$persistent == "P"] <- "Persistent"
# sample_info_family$persistent[sample_info_family$persistent == "NP"] <- "Non-Persistent"
# save(expression_data_family, file = "data/expression_data_family.rda")
# save(sample_info_family, file = "data/sample_info_family.rda")
# save(variable_info_family, file = "data/variable_info_family.rda")
# 
# ##genus data
# genus_microbiome_data <-
#   genus_microbiome_data %>%
#   massdataset::activate_mass_dataset("sample_info") %>%
#   dplyr::filter(risk != "Blank")
# expression_data_genus <-
#   genus_microbiome_data@expression_data
# sample_info_genus <-
#   genus_microbiome_data@sample_info
# variable_info_genus <-
#   genus_microbiome_data@variable_info
# sample_info_genus$persistent[sample_info_genus$persistent == "P"] <- "Persistent"
# sample_info_genus$persistent[sample_info_genus$persistent == "NP"] <- "Non-Persistent"
# save(expression_data_genus, file = "data/expression_data_genus.rda")
# save(sample_info_genus, file = "data/sample_info_genus.rda")
# save(variable_info_genus, file = "data/variable_info_genus.rda")
# 
# 
# phylum_name <-
#   phylum_microbiome_data@variable_info$variable_id
# 
# class_name <-
#   class_microbiome_data@variable_info$variable_id
# 
# order_name <-
#   order_microbiome_data@variable_info$variable_id
# 
# family_name <-
#   family_microbiome_data@variable_info$variable_id
# 
# genus_name <-
#   genus_microbiome_data@variable_info$variable_id
# 
# species_name <-
#   species_microbiome_data@variable_info$variable_id
# 
# save(phylum_name, file = "data/phylum_name.rda")
# save(class_name, file = "data/class_name.rda")
# save(order_name, file = "data/order_name.rda")
# save(family_name, file = "data/family_name.rda")
# save(genus_name, file = "data/genus_name.rda")
# save(species_name, file = "data/species_name.rda")
