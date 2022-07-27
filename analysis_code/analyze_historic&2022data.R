#2022-06-27


## 1. Set up

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_2022amesburydata.R"))
    source(paste0(data_locale, "analyze_historiccoraldata.R"))
    

## 2. Combine dataframes

    summary_acrossyears = 
        bind_rows(amesbury_summary %>%
                    mutate(survey_year = "1999") %>%
                    rename(sp_richness = Diversity, 
                           percent_cover = `Percent Coral Cover`, 
                           site = Site, 
                           transect = Transect,
                           qualitative_transect_position = reefflat_transect_position), 
                  surveysummary_2022 %>% 
                      mutate(survey_year = "2022") %>%
                      as_tibble()) %>%
        dplyr::select(site, transect, sp_richness, percent_cover, qualitative_transect_position,
                      dist_to_crest_m, dist_to_shore_m, dist_to_freshwater_m, survey_year) %>%
        drop_na(qualitative_transect_position) %>%
        mutate(qualitative_transect_position = recode(qualitative_transect_position, 
                                                      inner = "inner_flat", outer = "outer_flat"))
    
    
## 3. Compare percent cover across time periods
    
    # check assumptions
        # homogeneity of variance --> p>0.05 is good
        levene_test(percent_cover ~ survey_year * site * qualitative_transect_position,
                    data = summary_acrossyears)
        # normality --> p>0.05 is good
        summary_acrossyears %>%
            group_by(survey_year) %>%
            shapiro_test(percent_cover)
        summary_acrossyears %>%
            group_by(site) %>%
            shapiro_test(percent_cover)
        summary_acrossyears %>%
            group_by(qualitative_transect_position) %>%
            shapiro_test(percent_cover)
        
    # perform anova with interaction effects
    summary(aov(percent_cover ~ survey_year * site * qualitative_transect_position,
                data = summary_acrossyears))
        

## 4. Compare diversity across time periods
    
    # check assumptions
        # homogeneity of variance --> p>0.05 is good
        levene_test(sp_richness ~ survey_year * site * qualitative_transect_position, 
                    data = summary_acrossyears)
        # normality --> p>0.05 is good
        summary_acrossyears %>%
            group_by(survey_year) %>%
            shapiro_test(sp_richness)
        summary_acrossyears %>%
            group_by(site) %>%
            shapiro_test(sp_richness)
        summary_acrossyears %>%
            group_by(qualitative_transect_position) %>%
            shapiro_test(sp_richness)
    
    # perform anova with interaction effects
    summary(aov(sp_richness ~ survey_year * site * qualitative_transect_position,
                data = summary_acrossyears))    
    
    