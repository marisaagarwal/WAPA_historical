#2022-03-24


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_historiccoraldata.R"))
    

## 2. Plot NMDS (species level) ----

    # NMDS plot of coral communities at each site
    plotting_historic_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, 
                   # color = Site, 
                   color = qualitative_transect_position)) +
            # geom_text(label = plotting_historic_NMDS$Transect) +
            # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
            stat_ellipse(geom = "polygon", aes(color = qualitative_transect_position, 
                                               fill = qualitative_transect_position),
                         alpha = 0.2) +
            geom_point(size = 3, alpha = 0.6) +
            scale_x_continuous(limits = c(-5, 5)) +
            scale_y_continuous(limits = c(-3,3)) +
            scale_fill_manual(values = c("chocolate2", "darkblue"), 
                              labels = c("Inner Flat", "Outer Flat")) +
            scale_color_manual(values = c("chocolate2", "darkblue"), 
                               labels = c("Inner Flat", "Outer Flat")) +
            facet_wrap(~Site) +
            labs(fill = "Location", color = "Location") +
            theme_pubr(legend = "right")
    
    
    # by site
    find_hull = function(plotting_historic_NMDS) plotting_historic_NMDS[chull(plotting_historic_NMDS$NMDS1, plotting_historic_NMDS$NMDS2), ]
    hulls = ddply(plotting_historic_NMDS, "Site", find_hull)
    
    ggplot() +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = Site, color = Site),
                     alpha = 0.2) +
        geom_point(data = plotting_historic_NMDS, 
                   aes(x = NMDS1, y = NMDS2, color = Site)) +
        scale_fill_manual(values = c("chocolate2", "darkblue")) +
        scale_color_manual(values = c("chocolate2", "darkblue")) +
        geom_label(aes(label = c("Agat", "Asan"), x = c(0.5, -1.5), y = c(0.8, -0.65)),
                    color = "black", fill = c("chocolate2", "darkblue"), size = 3, alpha = 0.3) +
        geom_text(aes(label = "PERMANOVA; p = 0.542", x = -1.8, y = 1), size = 3) +
        theme_bw() +
        labs_pubr() +
        theme(legend.position = "none") +
        rremove("grid") 
    
    plotting_historic_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = Site, shape = Site)) +
            geom_point(size = 3, alpha = 0.4) +
            # geom_text(label = plotting_historic_NMDS$Transect) +
            # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
            stat_ellipse(linetype = 2, size = 1) +
            theme_light()
    
    # by reef flat zone
    find_hull = function(plotting_historic_NMDS) plotting_historic_NMDS[chull(plotting_historic_NMDS$NMDS1, plotting_historic_NMDS$NMDS2), ]
    hulls = ddply(plotting_historic_NMDS, "qualitative_transect_position", find_hull)
    
    ggplot() +
        geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                       fill = qualitative_transect_position, color = qualitative_transect_position),
                     alpha = 0.2) +
        geom_point(data = plotting_historic_NMDS, 
                   aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position)) +
        scale_fill_manual(values = c("magenta3", "lightgreen")) +
        scale_color_manual(values = c("magenta3", "lightgreen")) +
        geom_label(aes(label = c("Inner Reef Flat", "Outer Reef Flat"), x = c(-0.4, -1.5), y = c(0.45, -0.5)),
                   color = "black", fill = c("magenta3", "lightgreen"), size = 3, alpha = 0.5) +
        geom_text(aes(label = "PERMANOVA; p = 0.747", x = -1.8, y = 1), size = 3) +
        theme_bw() +
        labs_pubr() +
        theme(legend.position = "none") +
        rremove("grid") 
    
    plotting_historic_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position, shape = qualitative_transect_position)) +
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
                           shape = qualitative_transect_position),
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
        

## 2.5. Plot NMDS (genus level) ----
        
    # # NMDS plot of coral communities at each site
    #     plotting_historic_NMDS_genus %>%
    #         ggplot(aes(x = NMDS1, y = NMDS2, color = Site, shape = qualitative_transect_position)) +
    #         geom_point(size = 3, alpha = 0.4) +
    #         geom_text(label = plotting_historic_NMDS_genus$Transect) +
    #         # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
    #         stat_ellipse(linetype = 2, size = 1) +
    #         theme_light()
    #     
    #     plotting_historic_NMDS_genus %>%
    #         ggplot(aes(x = NMDS1, y = NMDS2, color = Site, shape = Site)) +
    #         geom_point(size = 3, alpha = 0.4) +
    #         # geom_text(label = plotting_historic_NMDS$Transect) +
    #         # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
    #         stat_ellipse(linetype = 2, size = 1) +
    #         theme_light()
    #     
    #     plotting_historic_NMDS_genus %>%
    #         ggplot(aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position, shape = qualitative_transect_position)) +
    #         geom_point(size = 3, alpha = 0.4) +
    #         # geom_text(label = plotting_historic_NMDS$Transect) +
    #         # ggrepel::geom_text_repel(label = plotting_historic_NMDS$Transect) +
    #         stat_ellipse(linetype = 2, size = 1) +
    #         theme_light()
    #     
    #     # how do different species contribute to NMDS separation?
    #     ggplot() +
    #         geom_point(data = plotting_historic_NMDS_genus,
    #                    aes(x = NMDS1, y = NMDS2, 
    #                        color = Site, 
    #                        shape = qualitative_transect_position),
    #                    size = 3, 
    #                    alpha = 0.8) +
    #         geom_segment(data = significant_historic_species_scores_genus,
    #                      aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
    #                      arrow = arrow(length = unit(0.25, "cm")),
    #                      colour = "grey10", 
    #                      lwd = 0.3) +                                               # add vector arrows of significant env variables
    #         ggrepel::geom_text_repel(data = significant_historic_species_scores_genus, 
    #                                  aes(x=NMDS1, y=NMDS2, label = abrev),
    #                                  cex = 3, 
    #                                  direction = "both", 
    #                                  segment.size = 0.25) +                          # add labels for species
    #         theme_light()
        
    
    
        
        
## 3. Plot species composition ----
        
    amesbury_data %>%
        ggplot(aes(x = as.character(Transect), y = Value, fill = `Species Listed (2022 taxonomy)`)) +
            geom_bar(position = "fill", stat = "identity") +
            facet_grid(Site~qualitative_transect_position) +
            theme_light()
    
    amesbury_data %>%
        ggplot(aes(x = Site, y = Value, fill = `Species Listed (2022 taxonomy)`)) +
            geom_bar(position = "fill", stat = "identity") +
            theme_light()
    
    amesbury_data %>%
        ggplot(aes(x = qualitative_transect_position, y = Value, fill = `Species Listed (2022 taxonomy)`)) +
        geom_bar(position = "fill", stat = "identity") +
        scale_x_discrete(labels=c("inner_flat" = "Inner Flat", 
                                  "outer_flat" = "Outer Flat")) +
        facet_wrap(~Site) +
        labs(x = "Reef Flat Location", y = "Proportion of Coral Community",
             fill = "Species") +
        theme_pubr(legend = "right")
    

## 4. Plot species accumulation curves ----
    
    # combined_historic_specaccum_curves %>%
    #     ggplot(aes(x = transect, y = richness, color = position)) +
    #         geom_point() +
    #         geom_line() +
    #         facet_wrap(~site) +
    #         theme_light()
    
    # by individuals
    ggiNEXT(indv_iNEXT_1999_models, type = 1) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        scale_y_continuous(limits = c(0, 28), breaks = c(0, 5, 10, 15, 20, 25)) + 
        labs(x = "Number of Corals Sampled", y = "Species Richness") +
        theme_pubr(legend = "right") +
        guides(linetype = "none", 
               fill = guide_legend(title="WAPA Unit & Reef Flat Zone"),
               color = guide_legend(title="WAPA Unit & Reef Flat Zone"),
               shape = guide_legend(title="WAPA Unit & Reef Flat Zone"))
    
    # by transects
    ggiNEXT(transect_iNEXT_1999_models, type = 1) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        scale_y_continuous(limits = c(0, 28), breaks = c(0, 5, 10, 15, 20, 25)) + 
        scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) + 
        labs(x = "Number of Transects Sampled", y = "Species Richness") +
        theme_pubr(legend = "right") +
        guides(linetype = "none", 
               fill = guide_legend(title="WAPA Unit & Reef Flat Zone"),
               color = guide_legend(title="WAPA Unit & Reef Flat Zone"),
               shape = guide_legend(title="WAPA Unit & Reef Flat Zone"))
    
    
    
    
    