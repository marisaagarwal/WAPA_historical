#2022-04-29


## 1. Set up

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_methodstest.R"))

    
## 2. Plot NMDS
    
    # NMDS plot of coral communities at each site
    plotting_test_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = `sampling type`, shape = test_number)) +
            geom_point(size = 3, alpha = 0.4) +
            # geom_text(label = plotting_test_NMDS$`sampling type`) +
            # ggrepel::geom_text_repel(label = plotting_test_NMDS$`sampling type`) +
            # stat_ellipse(linetype = 2, size = 1) +
            theme_light()
    
    
## 3. Plot NMDS with significant species overlaid
    
    ggplot() +
        geom_point(data = plotting_test_NMDS,
                   aes(x = NMDS1, y = NMDS2, 
                       color = `sampling type`, 
                       shape = test_number),
                   size = 3, 
                   alpha = 0.8) +
        geom_segment(data = significant_test_species_scores,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant env variables
        ggrepel::geom_text_repel(data = significant_test_species_scores, 
                                 aes(x=NMDS1, y=NMDS2, label = abrev),
                                 cex = 3, 
                                 direction = "both", 
                                 segment.size = 0.25) +                          # add labels for species
        theme_light()
    
    
    