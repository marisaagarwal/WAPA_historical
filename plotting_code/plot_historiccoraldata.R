#2022-03-24


## 1. Set up

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_historiccoraldata.R"))
    

## 2. Plot NMDS

    # NMDS plot of coral communities at each site
    plotting_historic_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Site, shape = `Position on Reef`)) +
            geom_point(size = 3, alpha = 0.4) +
            geom_text(label = plotting_historic_NMDS$Transect) +
            # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
    
    plotting_historic_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Site, shape = Site)) +
            geom_point(size = 3, alpha = 0.4) +
            # geom_text(label = plotting_historic_NMDS$Transect) +
            # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
    
    plotting_historic_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = `Position on Reef`, shape = `Position on Reef`)) +
            geom_point(size = 3, alpha = 0.4) +
            # geom_text(label = plotting_historic_NMDS$Transect) +
            # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
        
    # how do different species contribute to NMDS separation?
        ggplot() +
            geom_point(data = plotting_historic_NMDS,
                       aes(x = NMDS1, y = NMDS2, 
                           color = Site, 
                           shape = `Position on Reef`),
                       size = 3, 
                       alpha = 0.8) +
            geom_segment(data = significant_historic_species_scores,
                         aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                         arrow = arrow(length = unit(0.25, "cm")),
                         colour = "grey10", 
                         lwd = 0.3) +                                               # add vector arrows of significant env variables
            ggrepel::geom_text_repel(data = significant_historic_species_scores, 
                                     aes(x=NMDS1, y=NMDS2, label = abrev),
                                     cex = 3, 
                                     direction = "both", 
                                     segment.size = 0.25) +                          # add labels for species
            theme_light()
    
    
    
    