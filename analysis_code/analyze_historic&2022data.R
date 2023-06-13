#2022-06-27


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    # load in the data
    source(paste0(data_locale, "analyze_2022amesburydata.R"))
    
    # point to data locale
    data_locale = "analysis_code/"
    # load in the data
    source(paste0(data_locale, "analyze_historiccoraldata.R"))
    

## 2. Combine dataframes ----

    # diversity & percent cover
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
    
    # species accumulation
    # specaccum_acrossyears = rbind(combined_current_specaccum_curves, combined_historic_specaccum_curves)
    
    
    
## 3. Compare percent cover across time ----
    
    # overall difference
    summary_acrossyears %>%
        mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
        anova_test(transformed_percent_cover ~ survey_year * site * qualitative_transect_position)
    
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            summary_acrossyears %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
                levene_test(transformed_percent_cover ~ survey_year)
            summary_acrossyears %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
                levene_test(transformed_percent_cover ~ site)
            summary_acrossyears %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
                levene_test(transformed_percent_cover ~ qualitative_transect_position)
            # normality --> p>0.05 is good
            summary_acrossyears %>%
                group_by(survey_year) %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
                shapiro_test(transformed_percent_cover)
            summary_acrossyears %>%
                group_by(site) %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
                shapiro_test(transformed_percent_cover)
            summary_acrossyears %>%
                group_by(qualitative_transect_position) %>%
                mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
                shapiro_test(transformed_percent_cover)
        
        # post-hoc test
        summary_acrossyears %>%
            mutate(transformed_percent_cover = percent_cover^(1/3)) %>%  
            tukey_hsd(transformed_percent_cover ~ survey_year * site * qualitative_transect_position)
        

## 4. Compare richness across time ----
        
        # overall difference
        summary_acrossyears %>%
            anova_test(sp_richness ~ survey_year * site * qualitative_transect_position)
        
        summary_acrossyears %>%
            group_by(site, qualitative_transect_position) %>%
            t_test(sp_richness ~ survey_year)
        
            # check assumptions
                # homogeneity of variance --> p>0.05 is good
                summary_acrossyears %>%
                    levene_test(sp_richness ~ survey_year)
                summary_acrossyears %>%
                    levene_test(sp_richness ~ site)
                summary_acrossyears %>%
                    levene_test(sp_richness ~ qualitative_transect_position)
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
        
        # post-hoc test
        summary_acrossyears %>%
            tukey_hsd(sp_richness ~ survey_year * site * qualitative_transect_position)
    

## 5. Compare communities ----
    
    # * 5.1  prepare data ----
    prepped_current_data = 
        current_data_vegan %>%
            dplyr::select(-c(substrate_type)) %>%
            mutate(year = "2022", 
                   site_position_year = paste(site, 
                                              qualitative_transect_position, 
                                              year, 
                                              sep = "_")) %>%
            dplyr::select(year, everything()) %>%
            dplyr::select(site_position_year, everything())
    
    prepped_historic_data = 
        amesbury_data_vegan_NMDS %>%
            rename(LPUR = `Leptastrea purpurea`,
                   PDAM = `Pocillopora damicornis`,
                   PLUT = `Porites lutea`, 
                   HCOE = `Heliopora coerulea`,
                   PRUS = `Porites rus`,
                   GRET = `Goniastrea retiformis`,
                   PCYL = `Porites cylindrica`,
                   PDIV = `Pavona divaricata`,
                   PVEN = `Pavona venosa`,
                   PDEC = `Pavona decussata`,
                   PLIC = `Porites lichen`,
                   AASP = `Acropora aspera`,
                   PLOB = `Porites lobata`, 
                   site = "Site",
                   transect = "Transect") %>%
            rowwise() %>%
            mutate(PMAS = sum(c_across(c(PLUT,PLIC,PLOB)))) %>%
            ungroup() %>%                      
            dplyr::select(-c(PLUT, PLIC, PLOB)) %>%
            mutate(year = "1999", 
                   site_position_year = paste(site, 
                                              qualitative_transect_position, 
                                              year, 
                                              sep = "_")) %>%
            dplyr::select(year, everything()) %>%
            dplyr::select(site_position_year, everything())
    
    combined_community_data = 
        bind_rows(prepped_current_data, prepped_historic_data) %>%
        replace(is.na(.), 0)
                    
    
    # * 5.2 Asan: Inner '99 vs Inner '22 ----
    asan_inner_99_22 = 
        combined_community_data %>%
        filter(site == "Asan" & qualitative_transect_position == "inner_flat")
        
        # ** 5.2.1 NMDS ----
        asan_inner_99_22_NMDS = metaMDS(asan_inner_99_22[,6:ncol(asan_inner_99_22)], 
                                     k = 2,
                                     distance = "bray", 
                                     trymax = 100)
    
        # examine stressplot & baseplot
        stressplot(asan_inner_99_22_NMDS)
        plot(asan_inner_99_22_NMDS)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_asan_inner_99_22 = 
            asan_inner_99_22 %>%
            dplyr::select(c(year, transect)) %>%
            mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_asan_inner_99_22_NMDS = 
            scores(asan_inner_99_22_NMDS, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_asan_inner_99_22_NMDS = merge(reference_asan_inner_99_22, plotting_asan_inner_99_22_NMDS)
        
        # fit environmental and species vectors
        asan_inner_99_22_envfit =
            envfit(asan_inner_99_22_NMDS, 
                   reference_asan_inner_99_22, 
                   permutations = 999) # this fits environmental vectors
        
        asan_inner_99_22_speciesfit =
            envfit(asan_inner_99_22_NMDS, 
                   asan_inner_99_22[,6:ncol(asan_inner_99_22)], 
                   permutations = 999) # this fits species vectors
        
        
        # which species contribute to differences in NMDS plots?
        asan_inner_99_22_species_scores =
            as.data.frame(scores(asan_inner_99_22_speciesfit,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        asan_inner_99_22_species_scores = cbind(asan_inner_99_22_species_scores, 
                                        Species = rownames(asan_inner_99_22_species_scores))        #add species names to dataframe
        
        asan_inner_99_22_species_scores = cbind(asan_inner_99_22_species_scores,
                                        pval = asan_inner_99_22_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        asan_inner_99_22_species_scores = cbind(asan_inner_99_22_species_scores,
                                        abrev = abbreviate(asan_inner_99_22_species_scores$Species,
                                                           minlength = 4, 
                                                           method = "both"))                #abbreviate species names
        
        significant_asan_inner_99_22_species_scores = subset(asan_inner_99_22_species_scores,
                                                     pval <= 0.05)
        
        
        # ** 5.2.2 PERMANOVA ----
        # difference in community based on year? 
        
        # assumptions: do groups have homogeneous variances? 
        dis = vegdist(asan_inner_99_22[,6:ncol(asan_inner_99_22)], method="bray")
        mod = betadisper(dis, reference_asan_inner_99_22$year)
        anova(mod)      # p>0.05, proceed
        
        # test
        adonis2(asan_inner_99_22[,6:ncol(asan_inner_99_22)] ~ year, 
                data = reference_asan_inner_99_22, 
                permutations = 9999,
                method = "bray")                        # there ARE NOT differences in community based on survey year, p>0.05
        
    

    # * 5.3 Asan: Outer '99 vs Outer '22 ----
        
        asan_outer_99_22 = 
            combined_community_data %>%
            filter(site == "Asan" & qualitative_transect_position == "outer_flat")
        
        # ** 5.3.1 NMDS ----
        asan_outer_99_22_NMDS = metaMDS(asan_outer_99_22[,6:ncol(asan_outer_99_22)], 
                                        k = 2,
                                        distance = "bray", 
                                        trymax = 100)
        
        # examine stressplot & baseplot
        stressplot(asan_outer_99_22_NMDS)
        plot(asan_outer_99_22_NMDS)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_asan_outer_99_22 = 
            asan_outer_99_22 %>%
            dplyr::select(c(year, transect)) %>%
            mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_asan_outer_99_22_NMDS = 
            scores(asan_outer_99_22_NMDS, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_asan_outer_99_22_NMDS = merge(reference_asan_outer_99_22, plotting_asan_outer_99_22_NMDS)
        
        # fit environmental and species vectors
        asan_outer_99_22_envfit =
            envfit(asan_outer_99_22_NMDS, 
                   reference_asan_outer_99_22, 
                   permutations = 999) # this fits environmental vectors
        
        asan_outer_99_22_speciesfit =
            envfit(asan_outer_99_22_NMDS, 
                   asan_outer_99_22[,6:ncol(asan_outer_99_22)], 
                   permutations = 999) # this fits species vectors
        
        
        # which species contribute to differences in NMDS plots?
        asan_outer_99_22_species_scores =
            as.data.frame(scores(asan_outer_99_22_speciesfit,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        asan_outer_99_22_species_scores = cbind(asan_outer_99_22_species_scores, 
                                                Species = rownames(asan_outer_99_22_species_scores))        #add species names to dataframe
        
        asan_outer_99_22_species_scores = cbind(asan_outer_99_22_species_scores,
                                                pval = asan_outer_99_22_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        asan_outer_99_22_species_scores = cbind(asan_outer_99_22_species_scores,
                                                abrev = abbreviate(asan_outer_99_22_species_scores$Species,
                                                                   minlength = 4, 
                                                                   method = "both"))                #abbreviate species names
        
        significant_asan_outer_99_22_species_scores = subset(asan_outer_99_22_species_scores,
                                                             pval <= 0.05)
        
        
        # ** 5.3.2 PERMANOVA ----
        # difference in community based on year? 
        
        # assumptions: do groups have homogeneous variances? 
        dis = vegdist(asan_outer_99_22[,6:ncol(asan_outer_99_22)], method="bray")
        mod = betadisper(dis, reference_asan_outer_99_22$year)
        anova(mod)      # p>0.05, proceed
        
        # test
        adonis2(asan_outer_99_22[,6:ncol(asan_outer_99_22)] ~ year, 
                data = reference_asan_outer_99_22, 
                permutations = 9999,
                method = "bray")                        # there ARE NOT differences in community based on survey year, p>0.05
        
    
    # * 5.4 Agat: Inner '99 vs Inner '22 ----
        
        agat_inner_99_22 = 
            combined_community_data %>%
            filter(site == "Agat" & qualitative_transect_position == "inner_flat")
        
        # ** 5.4.1 NMDS ----
        agat_inner_99_22_NMDS = metaMDS(agat_inner_99_22[,6:ncol(agat_inner_99_22)], 
                                        k = 2,
                                        distance = "bray", 
                                        trymax = 100)
        
        # examine stressplot & baseplot
        stressplot(agat_inner_99_22_NMDS)
        plot(agat_inner_99_22_NMDS)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_agat_inner_99_22 = 
            agat_inner_99_22 %>%
            dplyr::select(c(year, transect)) %>%
            mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_agat_inner_99_22_NMDS = 
            scores(agat_inner_99_22_NMDS, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_agat_inner_99_22_NMDS = merge(reference_agat_inner_99_22, plotting_agat_inner_99_22_NMDS)
        
        # fit environmental and species vectors
        agat_inner_99_22_envfit =
            envfit(agat_inner_99_22_NMDS, 
                   reference_agat_inner_99_22, 
                   permutations = 999) # this fits environmental vectors
        
        agat_inner_99_22_speciesfit =
            envfit(agat_inner_99_22_NMDS, 
                   agat_inner_99_22[,6:ncol(agat_inner_99_22)], 
                   permutations = 999) # this fits species vectors
        
        
        # which species contribute to differences in NMDS plots?
        agat_inner_99_22_species_scores =
            as.data.frame(scores(agat_inner_99_22_speciesfit,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        agat_inner_99_22_species_scores = cbind(agat_inner_99_22_species_scores, 
                                                Species = rownames(agat_inner_99_22_species_scores))        #add species names to dataframe
        
        agat_inner_99_22_species_scores = cbind(agat_inner_99_22_species_scores,
                                                pval = agat_inner_99_22_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        agat_inner_99_22_species_scores = cbind(agat_inner_99_22_species_scores,
                                                abrev = abbreviate(agat_inner_99_22_species_scores$Species,
                                                                   minlength = 4, 
                                                                   method = "both"))                #abbreviate species names
        
        significant_agat_inner_99_22_species_scores = subset(agat_inner_99_22_species_scores,
                                                             pval <= 0.05)
        
        
        # ** 5.4.2 PERMANOVA ----
        # difference in community based on year? 
        
        # assumptions: do groups have homogeneous variances? 
        dis = vegdist(agat_inner_99_22[,6:ncol(agat_inner_99_22)], method="bray")
        mod = betadisper(dis, reference_agat_inner_99_22$year)
        anova(mod)      # p>0.05, proceed
        
        # test
        adonis2(agat_inner_99_22[,6:ncol(agat_inner_99_22)] ~ year, 
                data = reference_agat_inner_99_22, 
                permutations = 9999,
                method = "bray")                        # there ARE NOT differences in community based on survey year, p>0.05
        
    
    # * 5.5 Agat: Outer '99 vs Outer '22 ----

        agat_outer_99_22 = 
            combined_community_data %>%
            filter(site == "Agat" & qualitative_transect_position == "outer_flat")
        
        # ** 5.5.1 NMDS ----
        agat_outer_99_22_NMDS = metaMDS(agat_outer_99_22[,6:ncol(agat_outer_99_22)], 
                                        k = 2,
                                        distance = "bray", 
                                        trymax = 100)
        
        # examine stressplot & baseplot
        stressplot(agat_outer_99_22_NMDS)
        plot(agat_outer_99_22_NMDS)
        
        # create parsed down grouping dataframe and add row_ID column
        reference_agat_outer_99_22 = 
            agat_outer_99_22 %>%
            dplyr::select(c(year, transect)) %>%
            mutate(row_ID = row_number())
        
        # extract data for plotting
        plotting_agat_outer_99_22_NMDS = 
            scores(agat_outer_99_22_NMDS, display = "sites") %>% 
            as.data.frame() %>% 
            rownames_to_column("row_ID")
        
        plotting_agat_outer_99_22_NMDS = merge(reference_agat_outer_99_22, plotting_agat_outer_99_22_NMDS)
        
        # fit environmental and species vectors
        agat_outer_99_22_envfit =
            envfit(agat_outer_99_22_NMDS, 
                   reference_agat_outer_99_22, 
                   permutations = 999) # this fits environmental vectors
        
        agat_outer_99_22_speciesfit =
            envfit(agat_outer_99_22_NMDS, 
                   agat_outer_99_22[,6:ncol(agat_outer_99_22)], 
                   permutations = 999) # this fits species vectors
        
        
        # which species contribute to differences in NMDS plots?
        agat_outer_99_22_species_scores =
            as.data.frame(scores(agat_outer_99_22_speciesfit,
                                 display = "vectors"))                                      #save species intrinsic values into dataframe
        
        agat_outer_99_22_species_scores = cbind(agat_outer_99_22_species_scores, 
                                                Species = rownames(agat_outer_99_22_species_scores))        #add species names to dataframe
        
        agat_outer_99_22_species_scores = cbind(agat_outer_99_22_species_scores,
                                                pval = agat_outer_99_22_speciesfit$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
        
        
        agat_outer_99_22_species_scores = cbind(agat_outer_99_22_species_scores,
                                                abrev = abbreviate(agat_outer_99_22_species_scores$Species,
                                                                   minlength = 4, 
                                                                   method = "both"))                #abbreviate species names
        
        significant_agat_outer_99_22_species_scores = subset(agat_outer_99_22_species_scores,
                                                             pval <= 0.05)
        
        
        # ** 5.5.2 PERMANOVA ----
        # difference in community based on year? 
        
        # assumptions: do groups have homogeneous variances? 
        dis = vegdist(agat_outer_99_22[,6:ncol(agat_outer_99_22)], method="bray")
        mod = betadisper(dis, reference_agat_outer_99_22$year)
        anova(mod)      # p>0.05, proceed
        
        # test
        adonis2(agat_outer_99_22[,6:ncol(agat_outer_99_22)] ~ year, 
                data = reference_agat_outer_99_22, 
                permutations = 9999,
                method = "bray")                        # there ARE NOT differences in community based on survey year, p>0.05
        

        
# 6. For plotting community composition by species ----
        
        historic_comm_formatted = 
            amesbury_data %>%
            mutate(Species = recode(`Species Listed (2022 taxonomy)`, 
                                    `Acropora aspera` = "AASP",
                                    `Goniastrea retiformis` = "GRET",
                                    `Heliopora coerulea` = "HCOE",
                                    `Leptastrea purpurea` = "LPUR",
                                    `Pavona decussata` = "PDEC",
                                    `Pavona divaricata` = "PDIV",
                                    `Pavona venosa` = "PVEN",
                                    `Pocillopora damicornis` = "PDAM",
                                    `Porites cylindrica` = "PCYL",
                                    `Porites lichen` = "PMAS",
                                    `Porites lobata` = "PMAS",
                                    `Porites lutea` = "PMAS",
                                    `Porites rus` = "PRUS"),
                   year = "1999") %>%
            dplyr::select(-c(`Species Listed (2022 taxonomy)`))
        
        current_comm_formatted = 
            current_community_long %>%
            mutate(year = "2022",
                   Site  = site, 
                   Transect = transect) %>%
            dplyr::select(-c(site, transect))
        
        comm_for_plotting = 
            rbind(current_comm_formatted, historic_comm_formatted) 

        
## 7. Compare diversity across time ----
        
    # * 7.1 Shannon's diversity index ----
        
        # overall difference
        combined_diversity %>%
            anova_test(shannon_diversity ~ year * site * position)
        
        combined_diversity %>%
            group_by(site, position) %>%
            t_test(shannon_diversity ~ year)
        
            # check assumptions
                # homogeneity of variance --> p>0.05 is good
                combined_diversity %>%
                    levene_test(shannon_diversity ~ year)
                combined_diversity %>%
                    levene_test(shannon_diversity ~ site)
                combined_diversity %>%
                    levene_test(shannon_diversity ~ position)
                # normality --> p>0.05 is good
                combined_diversity %>%
                    group_by(year) %>%
                    shapiro_test(shannon_diversity)
                combined_diversity %>%
                    group_by(site) %>%
                    shapiro_test(shannon_diversity)
                combined_diversity %>%
                    group_by(position) %>%
                    shapiro_test(shannon_diversity)
        
        # post-hoc test
        combined_diversity %>%
            tukey_hsd(shannon_diversity ~ year * site * position)
        
   # * 7.2 beta diversity / species turnover  ----
        
        # overall
        overall_beta = 
            bind_rows(beta_ames_sites %>%
                            pivot_longer(cols = c(2:14), names_to = "species", values_to = "count") %>%
                            mutate(year = 1999) %>%
                            group_by(species) %>%
                            summarise(count = sum(count)) %>%
                            pivot_wider(names_from = species, values_from = count) %>%
                            ungroup() %>%
                            rowwise() %>%
                            mutate(PMAS = sum(c_across(c(PLUT,PLIC,PLOB)))) %>%
                            ungroup() %>%                      
                            dplyr::select(-c(PLUT, PLIC, PLOB)),
                      beta_current_sites %>%
                          pivot_longer(cols = c(2:30), names_to = "species", values_to = "count") %>%
                          mutate(year = 2022) %>%
                          group_by(species) %>%
                          summarise(count = sum(count)) %>%
                          pivot_wider(names_from = species, values_from = count)) %>%
                replace(is.na(.), 0)
        
        row.names(overall_beta) = c("1999", "2022")
        overall_beta_matrix =  as.matrix(overall_beta,rownames.force = T)
        class(overall_beta_matrix) = "numeric"
        
            # perform test
            dis.chao(overall_beta_matrix, index="jaccard", version="prob")

        # by unit
        
            # Asan 
            Asan_beta = bind_rows(beta_ames_sites %>%
                                    rowwise() %>%
                                    mutate(PMAS = sum(c_across(c(PLUT,PLIC,PLOB)))) %>%
                                    ungroup() %>%                      
                                    dplyr::select(-c(PLUT, PLIC, PLOB)) %>%
                                    filter(site == "Asan"),
                                  beta_current_sites %>%
                                      filter(site == "Asan")) %>%
                            replace(is.na(.), 0)
            
            row.names(Asan_beta) = c("1999", "2022")
            Asan_beta_matrix =  as.matrix(Asan_beta,rownames.force = T)
            Asan_beta_matrix =  Asan_beta_matrix[,colnames(Asan_beta_matrix) != "site"]
            class(Asan_beta_matrix) = "numeric"
            
                # perform test
                dis.chao(Asan_beta_matrix, index="jaccard", version="prob")
            
            # by reef flat position within Asan
                
                # inner reef flat
                Asan_inner_beta = bind_rows(
                    amesbury_data %>%
                        mutate(species = recode(`Species Listed (2022 taxonomy)`,
                                                `Leptastrea purpurea` = "LPUR",
                                                `Pocillopora damicornis` = "PDAM",
                                                `Porites lutea` = "PLUT", 
                                                `Heliopora coerulea` = "HCOE",
                                                `Porites rus` = "PRUS",
                                                `Goniastrea retiformis` = "GRET",
                                                `Porites cylindrica` = "PCYL",
                                                `Pavona divaricata` = "PDIV",
                                                `Pavona venosa` = "PVEN",
                                                `Pavona decussata` = "PDEC",
                                                `Porites lichen` = "PLIC",
                                                `Acropora aspera` = "AASP",
                                                `Porites lobata` = "PLOB"),
                               site = Site,
                               transect = Transect) %>%
                        filter(site == "Asan") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(Value = sum(Value)) %>%
                        pivot_wider(names_from = species,
                                    values_from = Value,
                                    values_fill = 0) %>%
                        rowwise() %>%
                        mutate(PMAS = sum(c_across(c(PLUT)))) %>%
                        ungroup() %>%                      
                        dplyr::select(-c(PLUT)) %>%
                        mutate(year = 1999),
                    current_data_vegan %>%
                        pivot_longer(cols = c(5:33), names_to = "species") %>%
                        filter(site == "Asan") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(value = sum(value)) %>%
                        pivot_wider(names_from = species, values_from = value) %>%
                        mutate(year = 2022)) %>%
                    replace(is.na(.), 0) %>%
                    filter(qualitative_transect_position == "inner_flat")
                
                row.names(Asan_inner_beta) = c("1999", "2022")
                Asan_inner_beta_matrix =  as.matrix(Asan_inner_beta,rownames.force = T)
                Asan_inner_beta_matrix =  Asan_inner_beta_matrix[,colnames(Asan_inner_beta_matrix) != "qualitative_transect_position"]
                Asan_inner_beta_matrix =  Asan_inner_beta_matrix[,colnames(Asan_inner_beta_matrix) != "year"]
                class(Asan_inner_beta_matrix) = "numeric"
                
                    # perform test
                    dis.chao(Asan_inner_beta_matrix, index="jaccard", version="prob")
                
                # outer reef flat
                Asan_outer_beta = bind_rows(
                    amesbury_data %>%
                        mutate(species = recode(`Species Listed (2022 taxonomy)`,
                                                `Leptastrea purpurea` = "LPUR",
                                                `Pocillopora damicornis` = "PDAM",
                                                `Porites lutea` = "PLUT", 
                                                `Heliopora coerulea` = "HCOE",
                                                `Porites rus` = "PRUS",
                                                `Goniastrea retiformis` = "GRET",
                                                `Porites cylindrica` = "PCYL",
                                                `Pavona divaricata` = "PDIV",
                                                `Pavona venosa` = "PVEN",
                                                `Pavona decussata` = "PDEC",
                                                `Porites lichen` = "PLIC",
                                                `Acropora aspera` = "AASP",
                                                `Porites lobata` = "PLOB"),
                               site = Site,
                               transect = Transect) %>%
                        filter(site == "Asan") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(Value = sum(Value)) %>%
                        pivot_wider(names_from = species,
                                    values_from = Value,
                                    values_fill = 0) %>%
                        rowwise() %>%
                        mutate(PMAS = sum(c_across(c(PLUT)))) %>%
                        ungroup() %>%                      
                        dplyr::select(-c(PLUT)) %>%
                        mutate(year = 1999),
                    current_data_vegan %>%
                        pivot_longer(cols = c(5:33), names_to = "species") %>%
                        filter(site == "Asan") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(value = sum(value)) %>%
                        pivot_wider(names_from = species, values_from = value) %>%
                        mutate(year = 2022)) %>%
                    replace(is.na(.), 0) %>%
                    filter(qualitative_transect_position == "outer_flat")
                    
                row.names(Asan_outer_beta) = c("1999", "2022")
                Asan_outer_beta_matrix =  as.matrix(Asan_outer_beta,rownames.force = T)
                Asan_outer_beta_matrix =  Asan_outer_beta_matrix[,colnames(Asan_outer_beta_matrix) != "qualitative_transect_position"]
                Asan_outer_beta_matrix =  Asan_outer_beta_matrix[,colnames(Asan_outer_beta_matrix) != "year"]
                class(Asan_outer_beta_matrix) = "numeric"
                    
                    # perform test
                    dis.chao(Asan_outer_beta_matrix, index="jaccard", version="prob")
                    
            # Agat 
            Agat_beta = bind_rows(beta_ames_sites %>%
                                      rowwise() %>%
                                      mutate(PMAS = sum(c_across(c(PLUT,PLIC,PLOB)))) %>%
                                      ungroup() %>%                      
                                      dplyr::select(-c(PLUT, PLIC, PLOB)) %>%
                                      filter(site == "Agat"),
                                  beta_current_sites %>%
                                      filter(site == "Agat")) %>%
                replace(is.na(.), 0)
            
            row.names(Agat_beta) = c("1999", "2022")
            Agat_beta_matrix =  as.matrix(Agat_beta,rownames.force = T)
            Agat_beta_matrix =  Agat_beta_matrix[,colnames(Agat_beta_matrix) != "site"]
            class(Agat_beta_matrix) = "numeric"
            
                # perform test
                dis.chao(Agat_beta_matrix, index="jaccard", version="prob")
            
            # by reef flat position within Agat
                
                # inner reef flat
                Agat_inner_beta = bind_rows(
                    amesbury_data %>%
                        mutate(species = recode(`Species Listed (2022 taxonomy)`,
                                                `Leptastrea purpurea` = "LPUR",
                                                `Pocillopora damicornis` = "PDAM",
                                                `Porites lutea` = "PLUT", 
                                                `Heliopora coerulea` = "HCOE",
                                                `Porites rus` = "PRUS",
                                                `Goniastrea retiformis` = "GRET",
                                                `Porites cylindrica` = "PCYL",
                                                `Pavona divaricata` = "PDIV",
                                                `Pavona venosa` = "PVEN",
                                                `Pavona decussata` = "PDEC",
                                                `Porites lichen` = "PLIC",
                                                `Acropora aspera` = "AASP",
                                                `Porites lobata` = "PLOB"),
                               site = Site,
                               transect = Transect) %>%
                        filter(site == "Agat") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(Value = sum(Value)) %>%
                        pivot_wider(names_from = species,
                                    values_from = Value,
                                    values_fill = 0) %>%
                        rowwise() %>%
                        mutate(PMAS = sum(c_across(c(PLUT, PLIC, PLOB)))) %>%
                        ungroup() %>%                      
                        dplyr::select(-c(PLUT, PLIC, PLOB)) %>%
                        mutate(year = 1999),
                    current_data_vegan %>%
                        pivot_longer(cols = c(5:33), names_to = "species") %>%
                        filter(site == "Agat") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(value = sum(value)) %>%
                        pivot_wider(names_from = species, values_from = value) %>%
                        mutate(year = 2022)) %>%
                    replace(is.na(.), 0) %>%
                    filter(qualitative_transect_position == "inner_flat")
                
                row.names(Agat_inner_beta) = c("1999", "2022")
                Agat_inner_beta_matrix =  as.matrix(Agat_inner_beta,rownames.force = T)
                Agat_inner_beta_matrix =  Agat_inner_beta_matrix[,colnames(Agat_inner_beta_matrix) != "qualitative_transect_position"]
                Agat_inner_beta_matrix =  Agat_inner_beta_matrix[,colnames(Agat_inner_beta_matrix) != "year"]
                class(Agat_inner_beta_matrix) = "numeric"
                
                    # perform test
                    dis.chao(Agat_inner_beta_matrix, index="jaccard", version="prob")
                
                # outer reef flat
                Agat_outer_beta = bind_rows(
                    amesbury_data %>%
                        mutate(species = recode(`Species Listed (2022 taxonomy)`,
                                                `Leptastrea purpurea` = "LPUR",
                                                `Pocillopora damicornis` = "PDAM",
                                                `Porites lutea` = "PLUT", 
                                                `Heliopora coerulea` = "HCOE",
                                                `Porites rus` = "PRUS",
                                                `Goniastrea retiformis` = "GRET",
                                                `Porites cylindrica` = "PCYL",
                                                `Pavona divaricata` = "PDIV",
                                                `Pavona venosa` = "PVEN",
                                                `Pavona decussata` = "PDEC",
                                                `Porites lichen` = "PLIC",
                                                `Acropora aspera` = "AASP",
                                                `Porites lobata` = "PLOB"),
                               site = Site,
                               transect = Transect) %>%
                        filter(site == "Agat") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(Value = sum(Value)) %>%
                        pivot_wider(names_from = species,
                                    values_from = Value,
                                    values_fill = 0) %>%
                        rowwise() %>%
                        mutate(PMAS = sum(c_across(c(PLUT, PLIC, PLOB)))) %>%
                        ungroup() %>%                      
                        dplyr::select(-c(PLUT, PLIC, PLOB)) %>%
                        mutate(year = 1999),
                    current_data_vegan %>%
                        pivot_longer(cols = c(5:33), names_to = "species") %>%
                        filter(site == "Agat") %>%
                        group_by(qualitative_transect_position, species) %>%
                        summarise(value = sum(value)) %>%
                        pivot_wider(names_from = species, values_from = value) %>%
                        mutate(year = 2022)) %>%
                    replace(is.na(.), 0) %>%
                    filter(qualitative_transect_position == "outer_flat")
                
                row.names(Agat_outer_beta) = c("1999", "2022")
                Agat_outer_beta_matrix =  as.matrix(Agat_outer_beta,rownames.force = T)
                Agat_outer_beta_matrix =  Agat_outer_beta_matrix[,colnames(Agat_outer_beta_matrix) != "qualitative_transect_position"]
                Agat_outer_beta_matrix =  Agat_outer_beta_matrix[,colnames(Agat_outer_beta_matrix) != "year"]
                class(Agat_outer_beta_matrix) = "numeric"
                
                    # perform test
                    dis.chao(Agat_outer_beta_matrix, index="jaccard", version="prob")
                
            
    