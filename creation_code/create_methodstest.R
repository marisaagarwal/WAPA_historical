# 2022-04-29


## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file = "Amesbury methods test 1.xlsx"
    
    # call to data
    methodstest_data = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = 1)


## 2. Groom data
    
    # check structure of data
    str(methodstest_data)
    
    # fill in data
    methodstest_data %<>%
        fill(`sampling type`) %>%
        fill(point_location) %>%
        fill(test_number)
    

## 3. Create Amesbury format
    
    # convert distance from M to CM
    methodstest_data %<>%
        mutate(distance_to_point_M = distance_to_point/100)
    
    # new df's for PCQM technique
    true_random_df = 
        methodstest_data %>%
        filter(`sampling type` == "true_random") %>%
        mutate(distance_to_point_M = distance_to_point/100) %>%
        dplyr::select(c(point_location, quarter, species, distance_to_point_M, size))
    
    semi_random_df = 
        methodstest_data %>%
        filter(`sampling type` == "semi_random") %>%
        mutate(distance_to_point_M = distance_to_point/100) %>%
        dplyr::select(c(point_location, quarter, species, distance_to_point_M, size))
    
    systematic_df = 
        methodstest_data %>%
        filter(`sampling type` == "systematic") %>%
        mutate(distance_to_point_M = distance_to_point/100) %>%
        dplyr::select(c(point_location, quarter, species, distance_to_point_M, size))
    

## 4. Create NMDS format / 'vegan'-ify data
    
    # summarize data
    methodstest_datasummary = 
        methodstest_data %>%
            group_by(`sampling type`, test_number, species) %>%
            summarise(count = n())

    # fill NAs in count column with zeros
    methodstest_datasummary %<>%
        mutate(count = replace_na(count, 0))
    
    # put into a 'vegan'-friendly format
    methodstest_datasummary_vegan = 
        methodstest_datasummary %>%
            # dplyr::select(-c(Source, Metric, `Species Listed (from paper)`)) %>%
            pivot_wider(names_from = species,
                        values_from = count, 
                        values_fn = sum, 
                        values_fill = 0)
    
    
    
    
    
    
    
    