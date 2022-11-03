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
    
        
    # difference in community based on transect's position on reef within each site?
        # Asan
        asan_amesbury_data_vegan_NMDS = 
            amesbury_data_vegan_NMDS %>%
                filter(Site == "Asan")
        
        asan_reference_amesbury = 
            reference_amesbury %>%
            filter(Site == "Asan")
        
                # assumption: do groups have homogeneous variances? 
                dis = vegdist(asan_amesbury_data_vegan_NMDS[,4:ncol(asan_amesbury_data_vegan_NMDS)],
                              method="bray")
                mod = betadisper(dis, asan_reference_amesbury$qualitative_transect_position)
                anova(mod)      # p>0.05, proceed
                # plot(mod)
        
        adonis2(asan_amesbury_data_vegan_NMDS[,4:ncol(asan_amesbury_data_vegan_NMDS)] ~ qualitative_transect_position, 
                data = asan_reference_amesbury, 
                permutations = 9999,
                method = "bray")                    # no, p>0.05
    
        # Agat
        agat_amesbury_data_vegan_NMDS = 
            amesbury_data_vegan_NMDS %>%
            filter(Site == "Agat")
        
        agat_reference_amesbury = 
            reference_amesbury %>%
            filter(Site == "Agat")
        
                # assumption: do groups have homogeneous variances? 
                dis = vegdist(agat_amesbury_data_vegan_NMDS[,4:ncol(agat_amesbury_data_vegan_NMDS)],
                              method="bray")
                mod = betadisper(dis, agat_reference_amesbury$qualitative_transect_position)
                anova(mod)      # p>0.05, proceed
                # plot(mod)
        
        adonis2(agat_amesbury_data_vegan_NMDS[,4:ncol(agat_amesbury_data_vegan_NMDS)] ~ qualitative_transect_position, 
                data = agat_reference_amesbury, 
                permutations = 9999,
                method = "bray")                    # no, p>0.05    

    
    # # difference in community based on transect number? --> no (which is good), p>0.05
    # adonis2(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)] ~ Transect,
    #         data = reference_amesbury,
    #         permutations = 9999,
    #         method = "bray")

    
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
            
    # difference between inner and outer flat within each site
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        group_by(Site) %>%
            anova_test(Diversity ~ reefflat_transect_position)
            
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site) %>%
                levene_test(Diversity ~ reefflat_transect_position)
            # normality --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site, reefflat_transect_position) %>%
                shapiro_test(Diversity)
            
       # # pairsise t-tests      
       #      amesbury_summary %>%
       #          filter(`Position on Reef` == "reef flat") %>%
       #          filter(Site == "Agat") %>%
       #          t_test(Diversity ~ reefflat_transect_position)    
       #      amesbury_summary %>%
       #          filter(`Position on Reef` == "reef flat") %>%
       #          filter(Site == "Asan") %>%
       #          t_test(Diversity ~ reefflat_transect_position) 
            

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
    
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        t_test(`Percent Coral Cover` ~ Site)
    
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
                shapiro_test(`Percent Coral Cover`)
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(reefflat_transect_position) %>%
                shapiro_test(`Percent Coral Cover`)
    
    # difference between inner and outer flat within each site
    amesbury_summary %>%
        filter(`Position on Reef` == "reef flat") %>%
        group_by(Site) %>%
        anova_test(`Percent Coral Cover` ~ reefflat_transect_position)
    
        # check assumptions
            # homogeneity of variance --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site) %>%
                levene_test(`Percent Coral Cover` ~ reefflat_transect_position)
            # normality --> p>0.05 is good
            amesbury_summary %>%
                filter(`Position on Reef` == "reef flat") %>%
                group_by(Site, reefflat_transect_position) %>%
                shapiro_test(`Percent Coral Cover`)
            

## 6. Species Accumulation ----
            
    # specaccum across all transects
    historic_specaccum_all = specaccum(amesbury_data_vegan_NMDS[,4:ncol(amesbury_data_vegan_NMDS)])      
    historic_specaccum_all = with(historic_specaccum_all, data.frame(sites, richness, sd)) 
    historic_specaccum_all %<>%
        rename(transect = "sites")

    # specaccum by transect's position on reef & site
        
    amesbury_ASAN_inner = amesbury_data_vegan_NMDS %>% 
        filter(Site == "Asan") %>%
        filter(qualitative_transect_position == "inner_flat")
    historic_specaccum_ASAN_innerflat = specaccum(amesbury_ASAN_inner[,4:ncol(amesbury_ASAN_inner)])
    historic_specaccum_ASAN_innerflat = with(historic_specaccum_ASAN_innerflat, data.frame(sites, richness, sd)) 
    historic_specaccum_ASAN_innerflat %<>%
        rename(transect = "sites") %>%
        mutate(site = "Asan", 
               position = "inner_flat")
    
    amesbury_ASAN_outer = amesbury_data_vegan_NMDS %>% 
        filter(Site == "Asan") %>%
        filter(qualitative_transect_position == "outer_flat")
    historic_specaccum_ASAN_outerflat = specaccum(amesbury_ASAN_outer[,4:ncol(amesbury_ASAN_outer)])
    historic_specaccum_ASAN_outerflat = with(historic_specaccum_ASAN_outerflat, data.frame(sites, richness, sd)) 
    historic_specaccum_ASAN_outerflat %<>%
        rename(transect = "sites") %>%
        mutate(site = "Asan", 
               position = "outer_flat")
    
    amesbury_AGAT_inner = amesbury_data_vegan_NMDS %>% 
        filter(Site == "Agat") %>%
        filter(qualitative_transect_position == "inner_flat")
    historic_specaccum_AGAT_innerflat = specaccum(amesbury_AGAT_inner[,4:ncol(amesbury_AGAT_inner)])
    historic_specaccum_AGAT_innerflat = with(historic_specaccum_AGAT_innerflat, data.frame(sites, richness, sd)) 
    historic_specaccum_AGAT_innerflat %<>%
        rename(transect = "sites") %>%
        mutate(site = "Agat", 
               position = "inner_flat")
    
    amesbury_AGAT_outer = amesbury_data_vegan_NMDS %>% 
        filter(Site == "Agat") %>%
        filter(qualitative_transect_position == "outer_flat")
    historic_specaccum_AGAT_outerflat = specaccum(amesbury_AGAT_outer[,4:ncol(amesbury_AGAT_outer)])
    historic_specaccum_AGAT_outerflat = with(historic_specaccum_AGAT_outerflat, data.frame(sites, richness, sd)) 
    historic_specaccum_AGAT_outerflat %<>%
        rename(transect = "sites") %>%
        mutate(site = "Agat", 
               position = "outer_flat")
    
    combined_historic_specaccum_curves = 
        rbind(historic_specaccum_ASAN_innerflat, historic_specaccum_ASAN_outerflat,
              historic_specaccum_AGAT_innerflat, historic_specaccum_AGAT_outerflat) %>%
        mutate(year = 1999)
    
    

    