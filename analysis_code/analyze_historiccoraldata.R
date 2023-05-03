#2022-03-24


## 1. Set up ----

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_historiccoraldata.R"))
    
    
## 2. NMDS (species level) ----
    
    # conduct NMDS 
    historiccoral_NMDS = metaMDS(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)], 
                                 k = 2,
                                 distance = "bray", 
                                 trymax = 100)
    
    # examine stressplot & baseplot
    stressplot(historiccoral_NMDS)
    plot(historiccoral_NMDS)
    
    # create parsed down grouping dataframe and add row_ID column
    reference_amesbury = 
        amesbury_data_vegan_NMDS %>%
        dplyr::select(c(Site, Transect, qualitative_transect_position)) %>%
        mutate(row_ID = row_number())
    
    # extract data for plotting
    plotting_historic_NMDS = 
        scores(historiccoral_NMDS, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
    
    plotting_historic_NMDS = merge(reference_amesbury, plotting_historic_NMDS)
    

    # fit environmental and species vectors
    historiccoral_envfit =
        envfit(historiccoral_NMDS, 
               reference_amesbury, 
               permutations = 999) # this fits environmental vectors
    
    historiccoral_speciesfit =
        envfit(historiccoral_NMDS, 
               amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)], 
               permutations = 999) # this fits species vectors
    
    
    # which species contribute to differences in NMDS plots?
    historic_species_scores =
        as.data.frame(scores(historiccoral_speciesfit,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    historic_species_scores = cbind(historic_species_scores, 
                                    Species = rownames(historic_species_scores))        #add species names to dataframe
    
    historic_species_scores = cbind(historic_species_scores,
                                    pval = historiccoral_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    historic_species_scores = cbind(historic_species_scores,
                                    abrev = abbreviate(historic_species_scores$Species,
                                                       minlength = 4, 
                                                       method = "both"))                #abbreviate species names
    
    significant_historic_species_scores = subset(historic_species_scores,
                                                 pval <= 0.05)                          #subset data to show species significant at 0.05


## 2.5. NMDS (genus level) ----
    
    amesbury_data_vegan_NMDS_genus = 
        amesbury_data_vegan_NMDS %>%
            pivot_longer(cols = c(4:ncol(amesbury_data_vegan_NMDS)), 
                                      names_to = "name", 
                                      values_to = "count") %>%
            separate(col = name, sep = " ", into = c("genus", "species")) %>%
            group_by(Site, Transect, qualitative_transect_position, genus) %>%
            summarise(count = sum(count)) %>%
            pivot_wider(names_from = genus, values_from = count)
    
    # conduct NMDS 
    historiccoral_NMDS_genus = metaMDS(amesbury_data_vegan_NMDS_genus[,4:ncol(amesbury_data_vegan_NMDS_genus)], 
                                 k = 2,
                                 distance = "bray", 
                                 trymax = 100)
    
    # examine stressplot & baseplot
    stressplot(historiccoral_NMDS_genus)
    plot(historiccoral_NMDS_genus)
    
    # create parsed down grouping dataframe and add row_ID column
    reference_amesbury_genus = 
        amesbury_data_vegan_NMDS_genus %>%
        dplyr::select(c(Site, Transect, qualitative_transect_position)) %>%
        ungroup() %>%
        mutate(row_ID = row_number())
    
    # extract data for plotting
    plotting_historic_NMDS_genus = 
        scores(historiccoral_NMDS_genus, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
    
    plotting_historic_NMDS_genus = merge(reference_amesbury_genus, plotting_historic_NMDS_genus)
    
    
    # fit environmental and species vectors
    historiccoral_envfit_genus =
        envfit(historiccoral_NMDS_genus, 
               reference_amesbury_genus, 
               permutations = 999) # this fits environmental vectors
    
    historiccoral_speciesfit_genus =
        envfit(historiccoral_NMDS_genus, 
               amesbury_data_vegan_NMDS_genus[,4:ncol(amesbury_data_vegan_NMDS_genus)], 
               permutations = 999) # this fits species vectors
    
    
    # which species contribute to differences in NMDS plots?
    historic_species_scores_genus =
        as.data.frame(scores(historiccoral_speciesfit_genus,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    historic_species_scores_genus = cbind(historic_species_scores_genus, 
                                    Species = rownames(historic_species_scores_genus))        #add species names to dataframe
    
    historic_species_scores_genus = cbind(historic_species_scores_genus,
                                    pval = historiccoral_speciesfit_genus$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    historic_species_scores_genus = cbind(historic_species_scores_genus,
                                    abrev = abbreviate(historic_species_scores_genus$Species,
                                                       minlength = 4, 
                                                       method = "both"))                #abbreviate species names
    
    significant_historic_species_scores_genus = subset(historic_species_scores_genus,
                                                 pval <= 0.05)                          #subset data to show species significant at 0.05
    
    
    

## 3. PERMANOVA (test for stat sig differences between groups) ----

    # species-level
    # difference in community based on transect's position on reef or site? 
    
        # transect position assumption: do groups have homogeneous variances? 
        dis = vegdist(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)],method="bray")
        mod = betadisper(dis, reference_amesbury$qualitative_transect_position)
        anova(mod)      # p>0.05, proceed
            # plot(mod)
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)], method="bray")
        mod = betadisper(dis, reference_amesbury$Site)
        anova(mod)      # p>0.05, proceed
            # plot(mod)
    
    adonis2(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)] ~ qualitative_transect_position * Site, 
            data = reference_amesbury, 
            permutations = 9999,
            method = "bray")                        # no, p>0.05
    
    # genus-level
    # difference in community based on transect's position on reef or site? 
    
        # transect position assumption: do groups have homogeneous variances? 
        dis = vegdist(amesbury_data_vegan_NMDS_genus[,4:ncol(amesbury_data_vegan_NMDS_genus)],method="bray")
        mod = betadisper(dis, reference_amesbury_genus$qualitative_transect_position)
        anova(mod)      # p>0.05, proceed
        # plot(mod)
        
        # site assumption: do groups have homogeneous variances? 
        dis = vegdist(amesbury_data_vegan_NMDS_genus[,4:ncol(amesbury_data_vegan_NMDS_genus)], method="bray")
        mod = betadisper(dis, reference_amesbury_genus$Site)
        anova(mod)      # p>0.05, proceed
        # plot(mod)
        
    adonis2(amesbury_data_vegan_NMDS_genus[,4:ncol(amesbury_data_vegan_NMDS_genus)] ~ qualitative_transect_position * Site, 
            data = reference_amesbury_genus, 
            permutations = 9999,
            method = "bray")                        # no, p>0.05
    
        
    # # difference in community based on transect's position on reef within each site?
    #     # Asan
    #     asan_amesbury_data_vegan_NMDS = 
    #         amesbury_data_vegan_NMDS %>%
    #             filter(Site == "Asan")
    #     
    #     asan_reference_amesbury = 
    #         reference_amesbury %>%
    #         filter(Site == "Asan")
    #     
    #             # assumption: do groups have homogeneous variances? 
    #             dis = vegdist(asan_amesbury_data_vegan_NMDS[,4:ncol(asan_amesbury_data_vegan_NMDS)],
    #                           method="bray")
    #             mod = betadisper(dis, asan_reference_amesbury$qualitative_transect_position)
    #             anova(mod)      # p>0.05, proceed
    #             # plot(mod)
    #     
    #     adonis2(asan_amesbury_data_vegan_NMDS[,4:ncol(asan_amesbury_data_vegan_NMDS)] ~ qualitative_transect_position, 
    #             data = asan_reference_amesbury, 
    #             permutations = 9999,
    #             method = "bray")                    # no, p>0.05
    # 
    #     # Agat
    #     agat_amesbury_data_vegan_NMDS = 
    #         amesbury_data_vegan_NMDS %>%
    #         filter(Site == "Agat")
    #     
    #     agat_reference_amesbury = 
    #         reference_amesbury %>%
    #         filter(Site == "Agat")
    #     
    #             # assumption: do groups have homogeneous variances? 
    #             dis = vegdist(agat_amesbury_data_vegan_NMDS[,4:ncol(agat_amesbury_data_vegan_NMDS)],
    #                           method="bray")
    #             mod = betadisper(dis, agat_reference_amesbury$qualitative_transect_position)
    #             anova(mod)      # p>0.05, proceed
    #             # plot(mod)
    #     
    #     adonis2(agat_amesbury_data_vegan_NMDS[,4:ncol(agat_amesbury_data_vegan_NMDS)] ~ qualitative_transect_position, 
    #             data = agat_reference_amesbury, 
    #             permutations = 9999,
    #             method = "bray")                    # no, p>0.05    
    # 
    # 
    # # # difference in community based on transect number? --> no (which is good), p>0.05
    # # adonis2(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)] ~ Transect,
    # #         data = reference_amesbury,
    # #         permutations = 9999,
    # #         method = "bray")

    
## 4. Diversity ----
    
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        group_by(Site, reefflat_transect_position) %>%
        summarise(mean_sp_richness = mean(Diversity), 
                  std_error_sp_richness = std.error(Diversity))
    
    # overall difference between sites
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        anova_test(Diversity ~ Site * reefflat_transect_position)
    
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                levene_test(Diversity ~ Site)
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                levene_test(Diversity ~ reefflat_transect_position)
            # normality --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site) %>%
                shapiro_test(Diversity)
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site) %>%
                shapiro_test(Diversity)
            
        # post-hoc testing
        amesbury_summary %>%
            filter(`Position on Reef` == "reef flat") %>%
            tukey_hsd(Diversity ~ Site * reefflat_transect_position)
            
    # # difference between inner and outer flat within each site
    # amesbury_summary %>%
    #     filter(`Position on Reef` == "reef flat") %>%
    #     group_by(Site) %>%
    #         anova_test(Diversity ~ reefflat_transect_position)
    # 
    #     # check assumptions
    #         # homogeneity of variance --> p>0.05 is good
    #         amesbury_summary %>%
    #             filter(`Position on Reef` == "reef flat") %>%
    #             group_by(Site) %>%
    #             levene_test(Diversity ~ reefflat_transect_position)
    #         # normality --> p>0.05 is good
    #         amesbury_summary %>%
    #             filter(`Position on Reef` == "reef flat") %>%
    #             group_by(Site, reefflat_transect_position) %>%
    #             shapiro_test(Diversity)
    # 
    #    # pairsise t-tests
    #         amesbury_summary %>%
    #             filter(`Position on Reef` == "reef flat") %>%
    #             filter(Site == "Agat") %>%
    #             t_test(Diversity ~ reefflat_transect_position)
    #         amesbury_summary %>%
    #             filter(`Position on Reef` == "reef flat") %>%
    #             filter(Site == "Asan") %>%
    #             t_test(Diversity ~ reefflat_transect_position)
            

## 5. Percent Cover ----
    
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        group_by(Site, reefflat_transect_position) %>%
        summarise(mean_percent_cover = mean(`Percent Coral Cover`), 
                  std_error_percent_cover = std.error(`Percent Coral Cover`))
    
    # overall differences
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        anova_test(`Percent Coral Cover` ~ Site * reefflat_transect_position)
    
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                levene_test(`Percent Coral Cover` ~ Site)
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                levene_test(`Percent Coral Cover` ~ reefflat_transect_position)
            # normality --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site) %>%
                mutate(percent_cover = `Percent Coral Cover`) %>%
                shapiro_test(percent_cover)
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(reefflat_transect_position) %>%
                mutate(percent_cover = `Percent Coral Cover`) %>%
                shapiro_test(percent_cover)
    
    # # difference between inner and outer flat within each site
    # amesbury_summary %>%
    #     filter(`Position on Reef` == "reef flat") %>%
    #     group_by(Site) %>%
    #     anova_test(`Percent Coral Cover` ~ reefflat_transect_position)
    # 
    #     # check assumptions
    #         # homogeneity of variance --> p>0.05 is good
    #         amesbury_summary %>%
    #             filter(`Position on Reef` == "reef flat") %>%
    #             group_by(Site) %>%
    #             levene_test(`Percent Coral Cover` ~ reefflat_transect_position)
    #         # normality --> p>0.05 is good
    #         amesbury_summary %>%
    #             filter(`Position on Reef` == "reef flat") %>%
    #             group_by(Site, reefflat_transect_position) %>%
    #             shapiro_test(`Percent Coral Cover`)
            

## 6. Species Accumulation ----
            
    # # specaccum across all transects
    # historic_specaccum_all = specaccum(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)])      
    # historic_specaccum_all = with(historic_specaccum_all, data.frame(sites, richness, sd)) 
    # historic_specaccum_all %<>%
    #     rename(transect = "sites")
    # 
    # # specaccum by transect's position on reef & site
    #     
    # amesbury_ASAN_inner = amesbury_data_vegan_NMDS %>% 
    #     filter(Site == "Asan") %>%
    #     filter(qualitative_transect_position == "inner_flat")
    # historic_specaccum_ASAN_innerflat = specaccum(amesbury_ASAN_inner[,4:ncol(amesbury_ASAN_inner)])
    # historic_specaccum_ASAN_innerflat = with(historic_specaccum_ASAN_innerflat, data.frame(sites, richness, sd)) 
    # historic_specaccum_ASAN_innerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Asan", 
    #            position = "inner_flat")
    # 
    # amesbury_ASAN_outer = amesbury_data_vegan_NMDS %>% 
    #     filter(Site == "Asan") %>%
    #     filter(qualitative_transect_position == "outer_flat")
    # historic_specaccum_ASAN_outerflat = specaccum(amesbury_ASAN_outer[,4:ncol(amesbury_ASAN_outer)])
    # historic_specaccum_ASAN_outerflat = with(historic_specaccum_ASAN_outerflat, data.frame(sites, richness, sd)) 
    # historic_specaccum_ASAN_outerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Asan", 
    #            position = "outer_flat")
    # 
    # amesbury_AGAT_inner = amesbury_data_vegan_NMDS %>% 
    #     filter(Site == "Agat") %>%
    #     filter(qualitative_transect_position == "inner_flat")
    # historic_specaccum_AGAT_innerflat = specaccum(amesbury_AGAT_inner[,4:ncol(amesbury_AGAT_inner)])
    # historic_specaccum_AGAT_innerflat = with(historic_specaccum_AGAT_innerflat, data.frame(sites, richness, sd)) 
    # historic_specaccum_AGAT_innerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Agat", 
    #            position = "inner_flat")
    # 
    # amesbury_AGAT_outer = amesbury_data_vegan_NMDS %>% 
    #     filter(Site == "Agat") %>%
    #     filter(qualitative_transect_position == "outer_flat")
    # historic_specaccum_AGAT_outerflat = specaccum(amesbury_AGAT_outer[,4:ncol(amesbury_AGAT_outer)])
    # historic_specaccum_AGAT_outerflat = with(historic_specaccum_AGAT_outerflat, data.frame(sites, richness, sd)) 
    # historic_specaccum_AGAT_outerflat %<>%
    #     rename(transect = "sites") %>%
    #     mutate(site = "Agat", 
    #            position = "outer_flat")
    # 
    # combined_historic_specaccum_curves = 
    #     rbind(historic_specaccum_ASAN_innerflat, historic_specaccum_ASAN_outerflat,
    #           historic_specaccum_AGAT_innerflat, historic_specaccum_AGAT_outerflat) %>%
    #     mutate(year = 1999)
    
    # sample-based rarefaction & extrapolation curves (hill number q = 0)
            
        # by individual
        indv_iNEXT_1999_data = 
            amesbury_data %>%
                rename(site = "Site",
                       transect = "Transect", 
                       species = "Species Listed (2022 taxonomy)") %>%
                mutate(species = recode(species,
                                        `Leptastrea purpurea` = "LPUR",
                       `Pocillopora damicornis` = "PDAM",
                       `Porites lutea` = "PMAS", 
                       `Heliopora coerulea` = "HCOE",
                       `Porites rus` = "PRUS",
                       `Goniastrea retiformis` = "GRET",
                       `Porites cylindrica` = "PCYL",
                       `Pavona divaricata` = "PDIV",
                       `Pavona venosa` = "PVEN",
                       `Pavona decussata` = "PDEC",
                       `Porites lichen` = "PMAS",
                       `Acropora aspera` = "AASP",
                       `Porites lobata` = "PMAS")) %>%
                group_by(site, transect, qualitative_transect_position, species) %>%
                summarise(count = sum(Value)) %>%
                mutate(survey_year = "1999")
                       
        indv_iNEXT_1999_data = 
            as.data.frame(
                indv_iNEXT_1999_data %>%
                    mutate(site_zone_year = paste(site, sep = "_", qualitative_transect_position, survey_year), 
                           site_zone_year = recode(site_zone_year, 
                                                   Agat_inner_flat_1999 = "Agat Inner Reef Flat",
                                                   Asan_inner_flat_1999 = "Asan Inner Reef Flat", 
                                                   Agat_outer_flat_1999 = "Agat Outer Reef Flat", 
                                                   Asan_outer_flat_1999 = "Asan Outer Reef Flat")) %>%
                    group_by(site_zone_year, species) %>%
                    summarise(count = sum(count)) %>%
                    pivot_wider(names_from = site_zone_year, 
                                values_from = count, 
                                values_fill = 0))
        
        rownames(indv_iNEXT_1999_data) = indv_iNEXT_1999_data %>% pull(species)
        
        indv_iNEXT_1999_data %<>%
            dplyr::select(-c(species))
        
        indv_iNEXT_1999_models = iNEXT(x = indv_iNEXT_1999_data, q = 0, datatype = "abundance",  endpoint = 385, nboot = 100)

        
    # by transect
        
        # data formatting
        transect_iNEXT_1999_data = 
            amesbury_data %>%
            rename(site = "Site",
                   transect = "Transect", 
                   species = "Species Listed (2022 taxonomy)") %>%
            mutate(species = recode(species,
                                    `Leptastrea purpurea` = "LPUR",
                                    `Pocillopora damicornis` = "PDAM",
                                    `Porites lutea` = "PMAS", 
                                    `Heliopora coerulea` = "HCOE",
                                    `Porites rus` = "PRUS",
                                    `Goniastrea retiformis` = "GRET",
                                    `Porites cylindrica` = "PCYL",
                                    `Pavona divaricata` = "PDIV",
                                    `Pavona venosa` = "PVEN",
                                    `Pavona decussata` = "PDEC",
                                    `Porites lichen` = "PMAS",
                                    `Acropora aspera` = "AASP",
                                    `Porites lobata` = "PMAS")) %>%
            group_by(site, transect, qualitative_transect_position, species) %>%
            summarise(count = sum(Value)) %>%
            ungroup() %>%
            mutate(survey_year = "1999") %>%
            mutate(site_zone_year = paste(site, sep = "_", qualitative_transect_position, survey_year), 
                   site_zone_year = recode(site_zone_year, 
                                           Agat_inner_flat_1999 = "Agat Inner Reef Flat",
                                           Asan_inner_flat_1999 = "Asan Inner Reef Flat", 
                                           Agat_outer_flat_1999 = "Agat Outer Reef Flat", 
                                           Asan_outer_flat_1999 = "Asan Outer Reef Flat"),
                   transect = paste("transect", sep = "_", transect)) %>%
            dplyr::select(-c(site, qualitative_transect_position, survey_year))
        
        # set up occurrence data
        transect_iNEXT_1999_data %<>%
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
        transect_iNEXT_1999_data %<>%
            nest_by(site_zone_year)
        
        # create and format site-zone combos
            # Agat Inner
            occurence_Agat_inner_flat_1999 = as.data.frame(
                transect_iNEXT_1999_data$data[[1]] %>%
                    pivot_wider(values_from = occurence, 
                                names_from = transect) %>%
                    add_column(transect_19 = 0, 
                               transect_24 = 0) )
                # set rownames to be the species
                rownames(occurence_Agat_inner_flat_1999) = 
                    occurence_Agat_inner_flat_1999 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Agat_inner_flat_1999 %<>%
                    dplyr::select(-c(species))
            
            # Agat Outer
            occurence_Agat_outer_flat_1999 = as.data.frame(
                transect_iNEXT_1999_data$data[[2]] %>%
                    pivot_wider(values_from = occurence, 
                                names_from = transect) )
                # set rownames to be the species
                rownames(occurence_Agat_outer_flat_1999) = 
                    occurence_Agat_outer_flat_1999 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Agat_outer_flat_1999 %<>%
                    dplyr::select(-c(species))
            
            # Asan Inner
            occurence_Asan_inner_flat_1999 = as.data.frame(
                transect_iNEXT_1999_data$data[[3]] %>%
                    pivot_wider(values_from = occurence, 
                                names_from = transect) )
                # set rownames to be the species
                rownames(occurence_Asan_inner_flat_1999) = 
                    occurence_Asan_inner_flat_1999 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Asan_inner_flat_1999 %<>%
                    dplyr::select(-c(species))
            
            # Asan Outer
            occurence_Asan_outer_flat_1999 = as.data.frame(
                transect_iNEXT_1999_data$data[[4]] %>%
                    pivot_wider(values_from = occurence, 
                                names_from = transect) )
                # set rownames to be the species
                rownames(occurence_Asan_outer_flat_1999) = 
                    occurence_Asan_outer_flat_1999 %>% 
                    pull(species)
                # remove the species name column 
                occurence_Asan_outer_flat_1999 %<>%
                    dplyr::select(-c(species))
       
        # create the list
        transect_iNEXT_1999_list = list(occurence_Agat_inner_flat_1999, occurence_Agat_outer_flat_1999, 
                                        occurence_Asan_inner_flat_1999, occurence_Asan_outer_flat_1999)  
                
        names(transect_iNEXT_1999_list) = c("Agat Inner Reef Flat", "Agat Outer Reef Flat", 
                                            "Asan Inner Reef Flat", "Asan Outer Reef Flat")         
                
        # create the model
        transect_iNEXT_1999_models = iNEXT(transect_iNEXT_1999_list, datatype = "incidence_raw", q = 0, endpoint = 10, nboot = 100)       
                
    