#2022-03-24


## 1. Set up

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_historiccoraldata.R"))
    
    
## 2. NMDS
    
    # conduct NMDS 
    historiccoral_NMDS = metaMDS(amesbury_data_vegan_NMDS[,4:58], 
                                 k = 2,
                                 distance = "bray", 
                                 trymax = 100)
    
    # examine stressplot & baseplot
    stressplot(historiccoral_NMDS)
    plot(historiccoral_NMDS)
    
    # create parsed down grouping dataframe and add row_ID column
    reference_amesbury = 
        amesbury_data_vegan_NMDS %>%
        dplyr::select(c(Site, Transect, `Position on Reef`)) %>%
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
               amesbury_data_vegan_NMDS[,4:58], 
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
    

## 3. PERMANOVA (test for stat sig differences between groups)

    # difference in community based on transect's position on reef? 
    
        # assumption: do groups have homogenous variances? 
        dis = vegdist(amesbury_data_vegan_NMDS[,4:58],method="bray")
        mod = betadisper(dis, reference_amesbury$`Position on Reef`)
        anova(mod)      # p>0.05, proceed
            # plot(mod)

    adonis2(amesbury_data_vegan_NMDS[,4:58] ~ `Position on Reef`, 
            data = reference_amesbury, 
            permutations = 999,
            method = "bray")                        # yes, p<0.05
    
    
    # difference in community based on site?
    
        # assumption: do groups have homogenous variances? 
        dis = vegdist(amesbury_data_vegan_NMDS[,4:58],method="bray")
        mod = betadisper(dis, reference_amesbury$Site)
        anova(mod)      # p>0.05, proceed
        # plot(mod)
    
    adonis2(amesbury_data_vegan_NMDS[,4:58] ~ Site, 
            data = reference_amesbury, 
            permutations = 999,
            method = "bray")                        # no, p>0.05
    
    # # difference in community based on transect number? --> no (which is good), p>0.05
    # adonis2(amesbury_data_vegan_NMDS[,4:58] ~ Transect, 
    #         data = reference_amesbury, 
    #         permutations = 999,
    #         method = "bray")
    # 
    # # difference in community based on site, transect position, transect number? --> kinda?, p<0.05
    # adonis2(amesbury_data_vegan_NMDS[,4:58] ~ Site + `Position on Reef`, 
    #         data = reference_amesbury, 
    #         permutations = 999,
    #         method = "bray", 
    #         by = "margin")
    

## 4. which species are most responsible for differences?
    
    # # example from internet, would need to change things around
    # permanova = #the permanova function
    # coef <- coefficients(permanova)["group1",]
    # top.coef <- coef[rev(order(abs(coef)))[1:20]]
    # par(mar = c(3, 14, 2, 1))
    # barplot(sort(top.coef), horiz = T, las = 1, main = "Top taxa")
    
    
    
    