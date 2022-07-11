#2022-06-27


## 1. Set up

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_historic&2022data.R"))


## 2.  Plot percent cover
    
    summary_acrossyears %>%
        ggplot(aes(x = survey_year, y = percent_cover)) +
            geom_boxplot() +
            facet_grid(site~qualitative_transect_position) +
            theme_light()
    

## 2.  Plot species richness
    
    summary_acrossyears %>%
        ggplot(aes(x = survey_year, y = sp_richness)) +
        geom_boxplot() +
        facet_grid(site~qualitative_transect_position) +
        theme_light()
    