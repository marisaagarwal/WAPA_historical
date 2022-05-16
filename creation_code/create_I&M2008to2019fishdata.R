# 2022-04-29


## 1. Set up

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file = "qry_fish_data_all_2008-2019.xlsx"
    
    # call to data
    IandM_data = 
        paste0(data_locale, data_file) %>%
        read_excel()
    

## 2. Groom data
    
    # check out data structure
    str(IandM_data)
    
    # convert data classes
    IandM_data %<>%
        mutate(Number = as.double(Number))
    

## 3. Create species list from data
    
    full_species_list = 
        IandM_data %>%
            group_by(Family, Genus, Species) %>%
            summarize(n_indv_observed = sum(Number),
                      average_length = mean(AvgLgth)) %>%
            arrange(desc(n_indv_observed))
    
    # # export species list
    # write_xlsx(full_species_list, "data/I&M 2008-2019 fish summary.xlsx")
    
    
