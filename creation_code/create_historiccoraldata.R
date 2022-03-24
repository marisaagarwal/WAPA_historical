# 2022-03-07


## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file = "Amesbury et al. (1999) Survey Data.xlsx"
    
    # call to data
    amesbury_data = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "Amesbury (1999) Data")
    
    amesbury_summary = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "Amesbury (1999) Summary")
    
    amesbury_metadata = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "Amesbury (1999) Metadata")
    
    # check structure of data
    str(amesbury_data)


## 2. Groom data
    
    # fill NAs in Value column with zeros
    amesbury_data %<>%
        mutate(Value = replace_na(Value, 0))
    
    # put into a 'vegan'-friendly format
    amesbury_data_vegan = 
        amesbury_data %>%
            dplyr::select(-c(Source, Metric, `Species Listed (from paper)`)) %>%
                pivot_wider(names_from = `Species Listed (2022 taxonomy)`,
                            values_from = Value, 
                            values_fn = sum, 
                            values_fill = 0)
    
    amesbury_data_vegan_NMDS = 
        amesbury_data_vegan %>%
            filter(!Transect == c(19, 24))     # remove Agat-19 & Agat-24 because no corals along either transect

    

    
    
    
    
    