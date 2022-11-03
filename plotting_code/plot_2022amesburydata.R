#2022-06-03


## 1. Set up ----

    # point to data locale
    data_locale = "analysis_code/"
    
    # load in the data
    source(paste0(data_locale, "analyze_2022amesburydata.R"))
    
    
## 2. Plot diversity ----
    
    surveysummary_2022 %>%
        group_by(site, qualitative_transect_position) %>%
        summarise(mean_richness = mean(sp_richness),
                  stderror_richness = std.error(sp_richness)) %>%
            ggplot(aes(x = site, y = mean_richness, fill = qualitative_transect_position)) +
                geom_col(position = "dodge", color = "black", alpha = 0.6) +
                geom_point(position = position_dodge(0.9)) +
                geom_errorbar(aes(ymin = mean_richness - stderror_richness,
                                  ymax = mean_richness + stderror_richness),
                              position = position_dodge(0.9), width = 0.2) +
                scale_fill_manual(values = c("chocolate2", "darkblue"), 
                                  labels = c("Inner Flat", "Outer Flat")) +
                labs(x = "Site", y = "Species Richness", fill = "Location") +
                theme_pubr(legend = "right")
    

## 3. Plot percent cover ----
    
    surveysummary_2022 %>%
        group_by(site, qualitative_transect_position) %>%
        summarise(mean_percentcover = mean(percent_cover),
                  stderror_percentcover = std.error(percent_cover)) %>%
            ggplot(aes(x = site, y = mean_percentcover, fill = qualitative_transect_position)) +
                geom_col(position = "dodge", color = "black", alpha = 0.6) +
                geom_point(position = position_dodge(0.9)) +
                geom_errorbar(aes(ymin = mean_percentcover - stderror_percentcover,
                                  ymax = mean_percentcover + stderror_percentcover),
                              position = position_dodge(0.9), width = 0.2) +
                scale_fill_manual(values = c("chocolate2", "darkblue"), 
                                  labels = c("Inner Flat", "Outer Flat")) +
                labs(x = "Site", y = "Percent Cover", fill = "Location") +
                theme_pubr(legend = "right")
    
    
## 4. Plot in relation to environmental factors ----
    
    # diversity
    surveysummary_2022 %>%
        ggplot() +
            geom_point(aes(x = dist_to_shore_m, y = sp_richness), color = "red") +
            geom_smooth(aes(x = dist_to_shore_m, y = sp_richness), color = "red", method = "lm") +
            geom_point(aes(x = dist_to_crest_m, y = sp_richness),color = "blue") +
            geom_smooth(aes(x = dist_to_crest_m, y = sp_richness), color = "blue", method = "lm") +
            geom_point(aes(x = dist_to_freshwater_m, y = sp_richness),color = "green") +
            geom_smooth(aes(x = dist_to_freshwater_m, y = sp_richness), color = "green", method = "lm") +
            labs(x = "Distance to Shore (red) / Crest (blue) / Freshwater (green)")+
            theme_light()
    
    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type, y = sp_richness)) +
        geom_boxplot() +
        theme_light()
    
    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type, y = sp_richness, color = substrate_type)) +
        geom_jitter() +
        facet_grid(site~qualitative_transect_position) +
        theme_light() 
    
    
    # percent cover
    surveysummary_2022 %>%
        ggplot() +
        geom_point(aes(x = dist_to_shore_m, y = percent_cover), color = "red") +
        geom_smooth(aes(x = dist_to_shore_m, y = percent_cover), color = "red", method = "lm") +
        geom_point(aes(x = dist_to_crest_m, y = percent_cover),color = "blue") +
        geom_smooth(aes(x = dist_to_crest_m, y = percent_cover), color = "blue", method = "lm") +
        geom_point(aes(x = dist_to_freshwater_m, y = percent_cover),color = "green") +
        geom_smooth(aes(x = dist_to_freshwater_m, y = percent_cover), color = "green", method = "lm") +
        labs(x = "Distance to Shore (red) / Crest (blue) / Freshwater (green)") +
        theme_light()

    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type, y = percent_cover)) +
        geom_boxplot() +
        theme_light()
    
    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type, y = percent_cover, color = substrate_type)) +
        geom_jitter() +
        facet_grid(site~qualitative_transect_position) +
        theme_light() 


## 5. Plot NMDS (species level) ----
    
    # NMDS plot of coral communities at each site
    plotting_current_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = site)) +
        geom_point(size = 3, alpha = 0.4) +
        geom_text(label = plotting_current_NMDS$transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    # # NMDS plot of coral communities at different locations along the reef flat
    # plotting_current_NMDS %>%
    #     ggplot(aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position)) +
    #     geom_point(size = 3, alpha = 0.4) +
    #     geom_text(label = plotting_current_NMDS$transect) +
    #     stat_ellipse(linetype = 2, size = 1) +
    #     theme_light()
    
    # NMDS plot of coral communities at different sites & locations on reef flat
    plotting_current_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position)) +
        geom_point(size = 3, alpha = 0.4) +
        stat_ellipse(geom = "polygon", aes(color = qualitative_transect_position, 
                                           fill = qualitative_transect_position),
                     alpha = 0.2) +        facet_wrap(~site) +
        geom_point(size = 3, alpha = 0.6) +
        scale_x_continuous(limits = c(-3, 3)) +
        scale_y_continuous(limits = c(-2,2)) +
        scale_fill_manual(values = c("chocolate2", "darkblue"), 
                          labels = c("Inner Flat", "Outer Flat")) +
        scale_color_manual(values = c("chocolate2", "darkblue"), 
                           labels = c("Inner Flat", "Outer Flat")) +
        facet_wrap(~site) +
        labs(fill = "Location", color = "Location") +
        theme_pubr(legend = "right")
    
    # how do different species contribute to NMDS separation?
    ggplot() +
        geom_point(data = plotting_current_NMDS,
                   aes(x = NMDS1, y = NMDS2, 
                       color = qualitative_transect_position, 
                       shape = site),
                   size = 3, 
                   alpha = 0.8) +
        stat_ellipse(data = plotting_current_NMDS, 
                     aes(x = NMDS1, y = NMDS2, 
                         color = qualitative_transect_position),
                     linetype = 2, size = 1) +
        geom_segment(data = significant_current_species_scores,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant env variables
        ggrepel::geom_text_repel(data = significant_current_species_scores, 
                                 aes(x=NMDS1, y=NMDS2, 
                                     label = abrev),
                                 cex = 3, 
                                 direction = "both", 
                                 segment.size = 0.25) +                          # add labels for species
        theme_light()
    

    
## 5.5. Plot NMDS (genus level) ----
    
    # NMDS plot of coral communities at each site
    plotting_current_genus_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = site
        )) +
        geom_point(size = 3, alpha = 0.4) +
        geom_text(label = plotting_current_NMDS$transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    # NMDS plot of coral communities at different locations along the reef flat
    plotting_current_genus_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position)) +
        geom_point(size = 3, alpha = 0.4) +
        geom_text(label = plotting_current_genus_NMDS$transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    # NMDS plot of coral communities at different sites & locations on reef flat
    plotting_current_genus_NMDS %>%
        ggplot(aes(x = NMDS1, y = NMDS2, color = qualitative_transect_position, shape = site)) +
        geom_point(size = 3, alpha = 0.4) +
        # geom_text(label = plotting_current_NMDS$transect) +
        stat_ellipse(linetype = 2, size = 1) +
        theme_light()
    
    # how do different species contribute to NMDS separation?
    ggplot() +
        geom_point(data = plotting_current_genus_NMDS,
                   aes(x = NMDS1, y = NMDS2, 
                       color = qualitative_transect_position, 
                       shape = site),
                   size = 3, 
                   alpha = 0.8) +
        stat_ellipse(data = plotting_current_genus_NMDS, 
                     aes(x = NMDS1, y = NMDS2, 
                         color = qualitative_transect_position),
                     linetype = 2, size = 1) +
        geom_segment(data = significant_current_species_scores_genus,
                     aes(x = 0, xend=NMDS1, y=0, yend=NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     colour = "grey10", 
                     lwd = 0.3) +                                               # add vector arrows of significant env variables
        ggrepel::geom_text_repel(data = significant_current_species_scores_genus, 
                                 aes(x=NMDS1, y=NMDS2, 
                                     label = abrev),
                                 cex = 3, 
                                 direction = "both", 
                                 segment.size = 0.25) +                          # add labels for species
        theme_light()   
    
    
    
## 6. Plot site characteristics (i.e., substrate type) ----
    
    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type)) +
            geom_bar() +
            facet_wrap(~site, ncol = 1)
    
    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type)) +
            geom_bar(aes(fill = qualitative_transect_position), position = "dodge") +
            facet_wrap(~ site, ncol = 1)
    
    surveysummary_2022 %>%
        ggplot(aes(x = substrate_type)) +
            geom_bar(aes(fill = qualitative_transect_position), position = "dodge")
    

## 7. Plot community composition barchart ----
    
    # amesbury_data %>%
    #     ggplot(aes(x = as.character(Transect), y = Value, fill = `Species Listed (2022 taxonomy)`)) +
    #     geom_bar(position = "fill", stat = "identity") +
    #     facet_wrap(~qualitative_transect_position) +
    #     theme_light()
            
  current_community_long = 
      current_data_vegan %>%
          dplyr::select(-c(substrate_type))
  
  current_community_long =
    current_community_long %>%
      pivot_longer(cols = c(4:ncol(current_community_long)), 
                   names_to = "Species", values_to = "Value")
  
  species_present = 
      current_community_long %>%
      filter(Value > 0) %>%
      group_by(site, qualitative_transect_position, Species) %>%
      summarise(Value = sum(Value))
  
  length(unique(species_present$Species))
  
  current_community_long %>%
      group_by(Species) %>%
      summarise(Value = sum(Value))
  
  current_community_long %>%
      ggplot(aes(x = as.character(transect), y = Value, fill = Species)) +
      geom_bar(position = "fill", stat = "identity") +
      facet_grid(site~qualitative_transect_position) +
      theme_light()
  
  current_community_long %>%
      ggplot(aes(x = qualitative_transect_position, y = Value, fill = Species)) +
          geom_bar(position = "fill", stat = "identity") +
          facet_wrap(~site) +
          scale_x_discrete(labels=c("inner_flat" = "Inner Flat", 
                                    "outer_flat" = "Outer Flat")) +
          labs(x = "Location", y = "Proportion of Coral Community", 
               fill = "Species Code") +
          theme_pubr(legend = "right")
  
  current_community_long %>%
      ggplot(aes(x = site, y = Value, fill = Species)) +
      geom_bar(position = "fill", stat = "identity") +
      theme_light()
      
  
## 8. Plot coral sizes ----
    
  full_join(x = all_data %>%
                mutate(area_cm2 = (pi*((colony_diameter1_cm/2)*(colony_diameter2_cm/2)))/4),
            y = surveysummary_2022) %>%
      dplyr::select(c("site", "transect", "species", "qualitative_transect_position", "area_cm2")) %>%
    ggplot(aes(x = species, y = area_cm2, fill = qualitative_transect_position)) +
        geom_boxplot() +
        facet_wrap(~site)
  
  summary_species_sizes %>%
      filter(site == "Agat") %>%
      ggplot(aes(x = species, y = mean_coral_area, fill = qualitative_transect_position)) +
        geom_col(position = "dodge") +
        geom_errorbar(aes(ymin = mean_coral_area - std_error_coral_area,
                          ymax = mean_coral_area + std_error_coral_area),
                      position = position_dodge(0.9), width = 0.2) 
        # facet_wrap(~site, ncol = 1)
      

## 9. Plot species accumulation curves ----
  
  combined_current_specaccum_curves %>%
      ggplot(aes(x = transect, y = richness, color = position)) +
          geom_point() +
          geom_line() +
          facet_wrap(~site) +
          theme_light()
  
            
            
    
    