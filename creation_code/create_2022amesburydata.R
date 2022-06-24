# 2022-05-16


## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file = "2022 Field Data - Revisiting Amesbury et al. (1999).xlsx"
    
    # call to data
    all_data = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "raw_data")
    
    species_codes = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "species_codes")
    
    metadata = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "metadata") %>%
            mutate(ID = paste(site, "_", transect))

    
## 2. Groom all_data
    
    # check structure of data
    str(all_data)
    
    # # fill in data
    # all_data %<>%
    #     fill(date) %>%
    #     fill(site) %>% 
    #     fill(transect) %>%
    #     fill(point_location)  
    
    # add unique site_transect ID column
    all_data %<>%
        mutate(ID = paste(all_data$site, "_", all_data$transect))

    
## 3. Create Amesbury format
    
    # convert distance from M to CM
    all_data %<>%
        mutate(distance_to_point_m = distance_to_point_cm/100)    
    
    
## 4. Create 'vegan'-ify data for NMDS approaches
    
    current_data_vegan = 
        all_data %>%
            dplyr::select(-c(date, point_location, quarter, 
                             distance_to_point_cm, colony_diameter1_cm, colony_diameter2_cm)) %>%
            group_by(site, transect, species) %>%
            summarise(count = n()) %>%
            filter(!is.na(species)) %>%
            pivot_wider(names_from = species, values_from = count, values_fill = 0)
    
    current_data_vegan = 
        merge(surveysummary_2022 %>%
                  dplyr::select(c(site, transect, qualitative_transect_position, substrate_type)),
              current_data_vegan)
    
    
  