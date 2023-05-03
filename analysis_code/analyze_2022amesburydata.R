#2022-05-16


## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_2022amesburydata.R"))
    
    
## 2. Alpha Diversity Calculation ----
    
    diversity_2022 = 
        all_data %>%
            group_by(site, transect, ID) %>%
            summarise(sp_richness = length(unique(species))) %>%
            mutate(ID = paste(site, "_", transect))
    
  
## 3. PCQM Percent Cover Calculation ----
    
    # load in functions from Mitchell (2007)
    # source("http://math.hws.edu/pcqm/pcqm.txt")
    
    # calculating relative density, relative cover, relative freq
    
        # all_data %>%
        #     dplyr::select(-c(date)) %>%
        #     group_by(site, transect) %>%
        #     importance.val()
    
    # calculating total percent cover for the transect
    
        # prepping the data and putting it into nested form
        all_data_percentcover = 
            all_data %>%
                mutate(area_cm2 = (pi*((colony_diameter1_cm/2)*(colony_diameter2_cm/2)))/4) %>%
                nest_by(ID)

        # the function! 
        amesbury_totalcover_function <- function(df, group) {
            
            df %>% 
                drop_na(species) %>%
                group_by({{group}}) %>% 
                summarise(freq_per_quarter = n()/nrow(df),
                          indv_per_m2 = freq_per_quarter * (1 / ((sum(df$distance_to_point_m, na.rm = T) / (4*length(unique(df$point_location))))^2)),
                          total_area_cm2 = sum(area_cm2, na.rm = T),
                          mean_area_cm2 = mean(area_cm2, na.rm = T)) %>%
                mutate(total_indv_per_m2 = sum(indv_per_m2),
                       cover_area_m2 = mean_area_cm2 * indv_per_m2) %>%
                summarise(total_cover_per_m2 = sum(cover_area_m2)) %>%
                transmute(proportion_cover_on_transect = total_cover_per_m2 / 10000)
        }

        # generating the percent_cover number and transect names
        percent_cover_output = 
            as.data.frame(do.call(rbind, 
                                  all_data %>%
                                      mutate(area_cm2 = (pi*((colony_diameter_cm/2)^2))/4) %>%
                                      nest_by(ID) %>%
                                      map(.x = all_data_percentcover$data, 
                                          .f = ~ amesbury_totalcover_function(df = .x, group = species)))) %>%
            mutate(percent_cover = proportion_cover_on_transect *100,
                   rowID = row_number()) 
        
        transect_ID = 
            all_data_percentcover %>% 
                dplyr::select(ID) %>%
                ungroup() %>%
                    mutate(rowID = row_number()) %>%
                    as.data.frame()
        
        # merging transect names and percent cover values
        percent_cover_output = merge(transect_ID, percent_cover_output)
        
             
## 4. Combining metadata, diversity, and % cover outputs ----
        
    surveysummary_2022 = merge(metadata, diversity_2022) 
    surveysummary_2022 = merge(surveysummary_2022, percent_cover_output)
    
    
## 5. Diversity Analysis ----
    
    # summary stats
    surveysummary_2022 %>%
        group_by(site, qualitative_transect_position) %>%
        summarise(mean_sp_richness = mean(sp_richness), 
                  std_error_sp_richness = std.error(sp_richness))
    
    # # differences in richness by site
    # surveysummary_2022 %>%
    #     t_test(sp_richness ~ site)
    # 
    # # differences in richness by location of transect on reef
    # surveysummary_2022 %>%
    #     t_test(sp_richness ~ qualitative_transect_position)
    # 
    # surveysummary_2022 %>%
    #     group_by(site) %>%
    #     anova_test(sp_richness ~ qualitative_transect_position)
    # 
    # # differences in richness by transect's location on reef for each site
    #     #Asan
    #     surveysummary_2022 %>%
    #         filter(site == "Asan") %>%
    #         t_test(sp_richness ~ qualitative_transect_position)
    #     #Agat
    #     surveysummary_2022 %>%
    #         filter(site == "Agat") %>%
    #         t_test(sp_richness ~ qualitative_transect_position)
    
    # overall differences
    surveysummary_2022 %>%
        anova_test(sp_richness ~ site * qualitative_transect_position)
        
            # check assumptions
                # homogeneity of variance --> p>0.05 is good
                surveysummary_2022 %>%
                    levene_test(sp_richness ~ site)
                surveysummary_2022 %>%
                    levene_test(sp_richness ~ qualitative_transect_position)
                # normality --> p>0.05 is good
                surveysummary_2022 %>%
                    group_by(site) %>%
                    shapiro_test(sp_richness)
                surveysummary_2022 %>%
                    group_by(qualitative_transect_position) %>%
                    shapiro_test(sp_richness)
                
            # post-hoc
            surveysummary_2022 %>%
                tukey_hsd(sp_richness ~ site * qualitative_transect_position)
        
    # difference between inner and outer flat within each site
    surveysummary_2022 %>%
        group_by(site) %>%
        t_test(sp_richness ~ qualitative_transect_position)
        
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            surveysummary_2022 %>%
                group_by(site) %>%
                levene_test(sp_richness ~ qualitative_transect_position)
            # normality --> p>0.05 is good
            surveysummary_2022 %>%
                group_by(qualitative_transect_position) %>%
                shapiro_test(sp_richness)
        
    
## 6. Percent Cover Analysis ----
    
    # # differences in percent coral cover by site
    # surveysummary_2022 %>%
    #     t_test(percent_cover ~ site)
    #     
    # surveysummary_2022 %>%
    #     group_by(site) %>%
    #     summarise(mean_percentcover = mean(percent_cover))
    #     
    # # differences in percent coral cover by location of transect on reef
    # surveysummary_2022 %>%
    #     t_test(percent_cover ~ qualitative_transect_position)
    #     
    # # differences in percent cover by transect's location on reef for each site
    #     #Asan
    #     surveysummary_2022 %>%
    #         filter(site == "Asan") %>%
    #         t_test(percent_cover ~ qualitative_transect_position)
    #     #Agat
    #     surveysummary_2022 %>%
    #         filter(site == "Agat") %>%
    #         t_test(percent_cover ~ qualitative_transect_position)
    
   # summary stats
   surveysummary_2022 %>%
        group_by(site, qualitative_transect_position) %>%
        summarise(mean_percent_cover = mean(percent_cover), 
                  std_error_percent_cover = std.error(percent_cover))   
            
    # overall differences
    surveysummary_2022 %>%
        mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
        anova_test(transformed_percent_cover ~ site * qualitative_transect_position)
        
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            surveysummary_2022 %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
                levene_test(transformed_percent_cover ~ site)
            surveysummary_2022 %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
                levene_test(transformed_percent_cover ~ qualitative_transect_position)
            # normality --> p>0.05 is good
            surveysummary_2022 %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
                group_by(site) %>%
                shapiro_test(transformed_percent_cover)
            surveysummary_2022 %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
                group_by(qualitative_transect_position) %>%
                shapiro_test(transformed_percent_cover)
            
        # post-hoc testing
        surveysummary_2022 %>%
            mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
            tukey_hsd(transformed_percent_cover ~ site * qualitative_transect_position)
        
    # difference between inner and outer flat within each site
    surveysummary_2022 %>%
        mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
        group_by(site) %>%
        t_test(transformed_percent_cover ~ qualitative_transect_position)

        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            surveysummary_2022 %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
                group_by(site) %>%
                levene_test(transformed_percent_cover ~ qualitative_transect_position)
            # normality --> p>0.05 is good
            surveysummary_2022 %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%
                group_by(qualitative_transect_position) %>%
                shapiro_test(transformed_percent_cover)
        
## 7. NMDS (species level) ----
        
    # set up data        
    current_data_vegan = 
        merge(surveysummary_2022 %>%
                dplyr::select(c(site, transect, qualitative_transect_position, substrate_type)),
            current_data_vegan)
        
    # conduct NMDS 
    currentcoral_NMDS = metaMDS(current_data_vegan[,5:ncol(current_data_vegan)], 
                                 k = 2,
                                 distance = "bray", 
                                 trymax = 100)
        
    # examine stressplot & baseplot
    stressplot(currentcoral_NMDS)
    plot(currentcoral_NMDS)
        
    # create parsed down grouping dataframe and add row_ID column
    reference_current = 
        current_data_vegan %>%
        dplyr::select(c(site, transect, qualitative_transect_position, substrate_type)) %>%
        ungroup() %>%
        mutate(row_ID = row_number())
        
    # extract data for plotting
    plotting_current_NMDS = 
        scores(currentcoral_NMDS, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
        
    plotting_current_NMDS = merge(reference_current, plotting_current_NMDS)
        
    # fit environmental and species vectors
    currentcoral_envfit =
        envfit(currentcoral_NMDS, 
               reference_current, 
               permutations = 999,
               na.rm = TRUE) # this fits environmental vectors
        
    currentcoral_speciesfit =
        envfit(currentcoral_NMDS, 
               current_data_vegan[,5:ncol(current_data_vegan)], 
               permutations = 999,
               na.rm = T) # this fits species vectors      
    
    # which species contribute to differences in NMDS plots?
    current_species_scores =
        as.data.frame(scores(currentcoral_speciesfit,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    current_species_scores = cbind(current_species_scores, 
                                    Species = rownames(current_species_scores))        #add species names to dataframe
    
    current_species_scores = cbind(current_species_scores,
                                    pval = currentcoral_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    current_species_scores = cbind(current_species_scores,
                                    abrev = abbreviate(current_species_scores$Species,
                                                       minlength = 4, 
                                                       method = "both"))                #abbreviate species names
    
    significant_current_species_scores = subset(current_species_scores,
                                                 pval <= 0.05)                          #subset data to show species significant at 0.05
    
    # which environmental factors contribute to differences in NMDS plots?
    current_env_scores =
        as.data.frame(scores(currentcoral_envfit,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    current_env_scores = cbind(current_env_scores, 
                                   Species = rownames(current_env_scores))        #add species names to dataframe
    
    current_env_scores = cbind(current_env_scores,
                                   pval = currentcoral_envfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    # current_env_scores = cbind(current_env_scores,
    #                                abrev = abbreviate(current_env_scores$Species,
    #                                                   minlength = 4, 
    #                                                   method = "both"))                #abbreviate environmental factor names
    
    significant_current_env_scores = subset(current_env_scores,
                                                pval <= 0.05)                          #subset data to show environmental factors significant at 0.05

    
## 8. PERMANOVA of NMDS (species level) ----

    # difference in community based on transect's position on reef or site? 
    
        # transect position assumption: do groups have homogeneous variances? 
        dis = vegdist(current_data_vegan[,5:ncol(current_data_vegan)], method="bray")
        mod = betadisper(dis, reference_current$qualitative_transect_position)
        anova(mod)      # p>0.05, proceed
            # plot(mod)
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(current_data_vegan[,5:ncol(current_data_vegan)], method="bray")
        mod = betadisper(dis, reference_current$site)
        anova(mod)      # p>0.05, proceed
            # plot(mod)
    
    adonis2(current_data_vegan[,5:ncol(current_data_vegan)] ~ qualitative_transect_position * site, 
            data = reference_current, 
            permutations = 9999,
            method = "bray")                        # there are differences in community based on site and transect position, p>0.05
    
    
    # difference in community based on transect's position on reef within each site?
    # Asan
    asan_current_data_vegan = 
        current_data_vegan %>%
        filter(site == "Asan")
    
    asan_reference_current = 
        reference_current %>%
        filter(site == "Asan")
    
            # assumption: do groups have homogeneous variances? 
            dis = vegdist(asan_current_data_vegan[,5:ncol(asan_current_data_vegan)],
                          method="bray")
            mod = betadisper(dis, asan_reference_current$qualitative_transect_position)
            anova(mod)      # p>0.05, proceed
                # plot(mod)
    
    adonis2(asan_current_data_vegan[,5:ncol(asan_current_data_vegan)] ~ qualitative_transect_position, 
            data = asan_reference_current, 
            permutations = 9999,
            method = "bray")                    # no difference in community, p>0.05
    
    # Agat
    agat_current_data_vegan = 
        current_data_vegan %>%
        filter(site == "Agat")
    
    agat_reference_current = 
        reference_current %>%
        filter(site == "Agat")
    
            # assumption: do groups have homogeneous variances? 
            dis = vegdist(agat_current_data_vegan[,5:ncol(agat_current_data_vegan)],
                          method="bray")
            mod = betadisper(dis, agat_reference_current$qualitative_transect_position)
            anova(mod)      # p>0.05, proceed
                # plot(mod)
    
    adonis2(agat_current_data_vegan[,5:ncol(agat_current_data_vegan)] ~ qualitative_transect_position, 
            data = agat_reference_current, 
            permutations = 9999,
            method = "bray")                    # yes, p<0.05    
    
    
## 7.5 NMDS (genus level) ----
    
    current_data_vegan_genus = 
        current_data_vegan %>%
            pivot_longer(cols = c(5:ncol(current_data_vegan)), 
                         names_to = "code", 
                         values_to = "count")
    
    current_data_vegan_genus = merge(current_data_vegan_genus, species_codes)
    
    current_data_vegan_genus = 
        current_data_vegan_genus %>%
            dplyr::select(-c(code)) %>%
            group_by(site, transect, qualitative_transect_position, substrate_type, genus) %>%
            summarise(count = sum(count)) %>%
                pivot_wider(names_from = genus, values_from = count)
    
    # conduct NMDS 
    currentcoral_genus_NMDS = metaMDS(current_data_vegan_genus[,5:ncol(current_data_vegan_genus)], 
                                k = 2,
                                distance = "bray", 
                                trymax = 100)
    
    # examine stressplot & baseplot
    stressplot(currentcoral_genus_NMDS)
    plot(currentcoral_genus_NMDS)
    
    # create parsed down grouping dataframe and add row_ID column
    reference_current_genus = 
        current_data_vegan_genus %>%
        dplyr::select(c(site, transect, qualitative_transect_position, substrate_type)) %>%
        ungroup() %>%
        mutate(row_ID = row_number())
    
    # extract data for plotting
    plotting_current_genus_NMDS = 
        scores(currentcoral_genus_NMDS, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
    
    plotting_current_genus_NMDS = merge(reference_current_genus, plotting_current_genus_NMDS)
    
    # fit environmental and species vectors
    currentcoral_envfit_genus =
        envfit(currentcoral_genus_NMDS, 
               reference_current_genus, 
               permutations = 999,
               na.rm = TRUE) # this fits environmental vectors
    
    currentcoral_speciesfit_genus =
        envfit(currentcoral_genus_NMDS, 
               current_data_vegan_genus[,5:ncol(current_data_vegan_genus)], 
               permutations = 999,
               na.rm = T) # this fits species vectors      
    
    # which species contribute to differences in NMDS plots?
    current_species_scores_genus =
        as.data.frame(scores(currentcoral_speciesfit_genus,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    current_species_scores_genus = cbind(current_species_scores_genus, 
                                   Species = rownames(current_species_scores_genus))        #add species names to dataframe
    
    current_species_scores_genus = cbind(current_species_scores_genus,
                                   pval = currentcoral_speciesfit_genus$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    current_species_scores_genus = cbind(current_species_scores_genus,
                                   abrev = abbreviate(current_species_scores_genus$Species,
                                                      minlength = 4, 
                                                      method = "both"))                #abbreviate species names
    
    significant_current_species_scores_genus = subset(current_species_scores_genus,
                                                pval <= 0.05)                          #subset data to show species significant at 0.05
    
    # which environmental factors contribute to differences in NMDS plots?
    current_env_scores_genus =
        as.data.frame(scores(currentcoral_envfit_genus,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    current_env_scores_genus = cbind(current_env_scores_genus, 
                               Species = rownames(current_env_scores_genus))        #add species names to dataframe
    
    current_env_scores_genus = cbind(current_env_scores_genus,
                               pval = currentcoral_envfit_genus$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    # current_env_scores = cbind(current_env_scores,
    #                                abrev = abbreviate(current_env_scores$Species,
    #                                                   minlength = 4, 
    #                                                   method = "both"))                #abbreviate environmental factor names
    
    significant_current_env_scores_genus = subset(current_env_scores_genus,
                                            pval <= 0.05)                          #subset data to show environmental factors significant at 0.05
    
    
    
## 8. Environmental Factors (distance from shore/crest/freshwater) ----
    
    # diversity and distance to shore, % cover and distance to shore
    summary(lm(sp_richness ~ dist_to_shore_m, data = surveysummary_2022))
    summary(lm(percent_cover ~ dist_to_shore_m, data = surveysummary_2022))
    
    # diversity and distance to reef crest, % cover and distance to reef crest
    summary(lm(sp_richness ~ dist_to_crest_m, data = surveysummary_2022))
    summary(lm(percent_cover ~ dist_to_crest_m, data = surveysummary_2022))

    #diversity and distance to freshwater, % cover and distance to freshwater
    summary(lm(sp_richness ~ dist_to_freshwater_m, data = surveysummary_2022))
    summary(lm(percent_cover ~ dist_to_freshwater_m, data = surveysummary_2022))
    
    
## 9. Coral Size Analysis ----
    
    # differences in coral size by species, transect's location on reef, & site
    
    #     # currently failing bc not enough data for each species?
    #     full_join(x = all_data %>%
    #                   mutate(area_cm2 = (pi*((colony_diameter1_cm/2)*(colony_diameter2_cm/2)))/4),
    #               y = surveysummary_2022) %>%
    #         dplyr::select(c("site", "transect", "species", "qualitative_transect_position", "area_cm2")) %>%
    #         filter(site == "Asan") %>%
    #         group_by(species) %>%
    #         t_test(area_cm2 ~ qualitative_transect_position) 
    #  
    # summary_species_sizes = 
    #     full_join(x = all_data %>%
    #                   mutate(area_cm2 = (pi*((colony_diameter1_cm/2)*(colony_diameter2_cm/2)))/4),
    #               y = surveysummary_2022) %>%
    #     dplyr::select(c("site", "transect", "species", "qualitative_transect_position", "area_cm2")) %>%
    #     group_by(site, qualitative_transect_position, species) %>%
    #     summarise(mean_coral_area = mean(area_cm2),
    #               std_error_coral_area = std.error(area_cm2))
    

## 10. Species Accumulation Curves ----
    
    # # specaccum across all transects
    # current_specaccum_all = specaccum(current_data_vegan[,5:ncol(current_data_vegan)])      
    # current_specaccum_all = with(current_specaccum_all, data.frame(sites, richness, sd)) 
    # current_specaccum_all %<>%
    #     rename(transect = "sites")
    # 
    # # specaccum by transect's position on reef & site
    # current_ASAN_inner = current_data_vegan %>% 
    #     filter(site == "Asan") %>%
    #     filter(qualitative_transect_position == "inner_flat")
    # current_specaccum_ASAN_innerflat = specaccum(current_ASAN_inner[,5:ncol(current_ASAN_inner)])
    # current_specaccum_ASAN_innerflat = with(current_specaccum_ASAN_innerflat, data.frame(sites, richness, sd)) 
    # current_specaccum_ASAN_innerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Asan", 
    #            position = "inner_flat")
    # 
    # current_ASAN_outer = current_data_vegan %>% 
    #     filter(site == "Asan") %>%
    #     filter(qualitative_transect_position == "outer_flat")
    # current_specaccum_ASAN_outerflat = specaccum(current_ASAN_outer[,5:ncol(current_ASAN_outer)])
    # current_specaccum_ASAN_outerflat = with(current_specaccum_ASAN_outerflat, data.frame(sites, richness, sd)) 
    # current_specaccum_ASAN_outerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Asan", 
    #            position = "outer_flat")
    # 
    # current_AGAT_inner = current_data_vegan %>% 
    #     filter(site == "Agat") %>%
    #     filter(qualitative_transect_position == "inner_flat")
    # current_specaccum_AGAT_innerflat = specaccum(current_AGAT_inner[,5:ncol(current_AGAT_inner)])
    # current_specaccum_AGAT_innerflat = with(current_specaccum_AGAT_innerflat, data.frame(sites, richness, sd)) 
    # current_specaccum_AGAT_innerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Agat", 
    #            position = "inner_flat")
    # 
    # current_AGAT_outer = current_data_vegan %>% 
    #     filter(site == "Agat") %>%
    #     filter(qualitative_transect_position == "outer_flat")
    # current_specaccum_AGAT_outerflat = specaccum(current_AGAT_outer[,5:ncol(current_AGAT_outer)])
    # current_specaccum_AGAT_outerflat = with(current_specaccum_AGAT_outerflat, data.frame(sites, richness, sd)) 
    # current_specaccum_AGAT_outerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Agat", 
    #            position = "outer_flat")
    # 
    # combined_current_specaccum_curves = 
    #     rbind(current_specaccum_ASAN_innerflat, current_specaccum_ASAN_outerflat,
    #           current_specaccum_AGAT_innerflat, current_specaccum_AGAT_outerflat) %>%
    #     mutate(year = 2022)
    
    
    # sample-based rarefaction & extrapolation curves (hill number q = 0)
    
    # by individual 
        # pull in and select useful data
        indv_iNEXT_2022_data = 
            merge(
                surveysummary_2022 %>% 
                    dplyr::select(site, transect, qualitative_transect_position) %>%
                    mutate(survey_year = 2022), 
                all_data %>%
                    dplyr::select(-c(date, point_location, quarter, 
                                     distance_to_point_cm, colony_diameter1_cm, colony_diameter2_cm)) %>%
                    group_by(site, transect, species) %>%
                    summarise(count = n()) %>%
                    filter(!is.na(species)) )
        # beautify data
        indv_iNEXT_2022_data =  
            as.data.frame(
                indv_iNEXT_2022_data %>%
                    mutate(site_zone_year = paste(site, sep = "_", qualitative_transect_position, survey_year)) %>%
                    group_by(site_zone_year, species) %>%
                    summarise(count = sum(count)) %>%
                    mutate(site_zone_year = recode(site_zone_year, 
                                         Agat_inner_flat_2022 = "Agat Inner Reef Flat",
                                         Agat_outer_flat_2022 = "Agat Outer Reef Flat",
                                         Asan_inner_flat_2022 = "Asan Inner Reef Flat",
                                         Asan_outer_flat_2022 = "Asan Outer Reef Flat")) %>%
                    pivot_wider(names_from = site_zone_year, 
                                values_from = count, 
                                values_fill = 0)) 
        # set rownames to be the species
        rownames(indv_iNEXT_2022_data) = indv_iNEXT_2022_data %>% pull(species)
        # remove the species name column 
        indv_iNEXT_2022_data %<>%
            dplyr::select(-c(species))
        # determine how far out to extrapolate, use the maximum value from colSums (here, max # = 385)
        colSums(indv_iNEXT_2022_data)
        # make the models 
        indv_iNEXT_2022_models = iNEXT(x = indv_iNEXT_2022_data, q = 0, datatype = "abundance", endpoint = 385, nboot = 100)
        

    # by transect  
        # data formatting
            transect_iNEXT_2022_data = 
                merge(
                    surveysummary_2022 %>% 
                        dplyr::select(site, transect, qualitative_transect_position) %>%
                        mutate(survey_year = 2022), 
                    all_data %>%
                        dplyr::select(-c(date, point_location, quarter, 
                                         distance_to_point_cm, colony_diameter1_cm, colony_diameter2_cm, 
                                         distance_to_point_m)) )
            
            transect_iNEXT_2022_data %<>%
                filter(!is.na(species)) %>%
                mutate(site_zone_year = paste(site, sep = "_", qualitative_transect_position, survey_year),
                       transect = paste("transect", sep = "_", transect)) %>%
                dplyr::select(-c(site, qualitative_transect_position, survey_year, ID))
            
        # set up occurrence data
            transect_iNEXT_2022_data %<>%
                group_by(site_zone_year, transect) %>%
                count(species) %>%
                ungroup() %>%
                group_by(site_zone_year) %>%
                complete(transect, species) %>%
                mutate(occurence = n/n) %>%
                dplyr::select(-c(n)) %>%
                mutate(occurence = replace_na(occurence, 0)) %>%
                ungroup()
            
        # nest by site/zone
            transect_iNEXT_2022_data %<>%
                nest_by(site_zone_year)
            
        # create and format site-zone combos
            # Agat Inner
            occurence_Agat_inner_flat_2022 = as.data.frame(
                transect_iNEXT_2022_data$data[[1]] %>%
                pivot_wider(values_from = occurence, 
                            names_from = transect) %>%
                add_column(transect_42 = 0, 
                           transect_54 = 0, 
                           transect_34 = 0) )
                # set rownames to be the species
                rownames(occurence_Agat_inner_flat_2022) = 
                    occurence_Agat_inner_flat_2022 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Agat_inner_flat_2022 %<>%
                    dplyr::select(-c(species))
                
            # Agat Outer
                occurence_Agat_outer_flat_2022 = as.data.frame(
                    transect_iNEXT_2022_data$data[[2]] %>%
                        pivot_wider(values_from = occurence, 
                                    names_from = transect) )
                # set rownames to be the species
                rownames(occurence_Agat_outer_flat_2022) = 
                    occurence_Agat_outer_flat_2022 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Agat_outer_flat_2022 %<>%
                    dplyr::select(-c(species))
                
            # Asan Inner
                occurence_Asan_inner_flat_2022 = as.data.frame(
                    transect_iNEXT_2022_data$data[[3]] %>%
                        pivot_wider(values_from = occurence, 
                                    names_from = transect) )
                # set rownames to be the species
                rownames(occurence_Asan_inner_flat_2022) = 
                    occurence_Asan_inner_flat_2022 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Asan_inner_flat_2022 %<>%
                    dplyr::select(-c(species))
            
            # Asan Outer
                occurence_Asan_outer_flat_2022 = as.data.frame(
                    transect_iNEXT_2022_data$data[[4]] %>%
                        pivot_wider(values_from = occurence, 
                                    names_from = transect) )
                # set rownames to be the species
                rownames(occurence_Asan_outer_flat_2022) = 
                    occurence_Asan_outer_flat_2022 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Asan_outer_flat_2022 %<>%
                    dplyr::select(-c(species))  

        # create the list
        transect_iNEXT_2022_list = list(occurence_Agat_inner_flat_2022, occurence_Agat_outer_flat_2022, 
                         occurence_Asan_inner_flat_2022, occurence_Asan_outer_flat_2022)  
          
        names(transect_iNEXT_2022_list) = c("Agat Inner Reef Flat", "Agat Outer Reef Flat", 
                                            "Asan Inner Reef Flat", "Asan Outer Reef Flat")
        
        # create the model
        transect_iNEXT_2022_models = iNEXT(transect_iNEXT_2022_list, datatype = "incidence_raw", q = 0, endpoint = 10, nboot = 100)
        
