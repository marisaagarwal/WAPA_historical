# 2022-07-26

## 1. Set up

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_historiccoraldata.R"))
    
    
## 2. Historic data
    
    combined_historic_specaccum_curves %>%
        ggplot(aes(x = transect, y = richness, color = position)) +
            geom_point() +
            geom_line() +
            facet_wrap(~site) +
            theme_light()
    
## 3. 2022 data
    
    
## 4. Merged data    
            
