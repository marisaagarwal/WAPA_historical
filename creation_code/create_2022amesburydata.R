# 2022-05-16


## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file = "2022 Field Data.xlsx"
    
    # call to data
    all_data = 
        paste0(data_locale, data_file) %>%
        read_excel()

    
## 2. Groom data
    
    # check structure of data
    str(all_data)
    
    # fill in data
    all_data %<>%
        fill(date) %>%
        fill(site) %>% 
        fill(transect) %>%
        fill(point_location)  
    
    # add unique site_transect ID column
    all_data %<>%
        mutate(ID = paste(all_data$site, "_", all_data$transect))

    
## 3. Create Amesbury format
    
    # convert distance from M to CM
    all_data %<>%
        mutate(distance_to_point_m = distance_to_point_cm/100)    
    
    
    
    
    
# ## 4. Create NMDS format / 'vegan'-ify data
    
# need to make a complete species list, first, then populate it per transect
    
#     
#     # summarize data
#     all_data_summary = 
#         all_data %>%
#             group_by(site, transect, species) %>%
#             summarise(count = n())
#     
#     # drop NA species
#     all_data_summary %>%
#         
#     
#     # fill NAs in count column with zeros
#     all_data_summary %<>%
#         mutate(count = replace_na(count, 0))
#     
#     # put into a 'vegan'-friendly format
#     all_data_summary_vegan = 
#         methodstest_datasummary %>%
#         # dplyr::select(-c(Source, Metric, `Species Listed (from paper)`)) %>%
#         pivot_wider(names_from = species,
#                     values_from = count, 
#                     values_fn = sum, 
#                     values_fill = 0)    
    
    
    
    
    
    
    