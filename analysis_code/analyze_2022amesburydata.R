#2022-05-16


## 1. Set up

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_2022amesburydata.R"))
    
    
## 2. Alpha Diversity Calculation 
    
    diversity_2022 = 
        all_data %>%
            group_by(site, transect, ID) %>%
            summarise(sp_richness = length(unique(species))) %>%
            mutate(ID = paste(site, "_", transect))
            
  
## 3. PCQM Percent Cover Calculation
    
    # load in functions from Mitchell (2007)
    source("http://math.hws.edu/pcqm/pcqm.txt")
    
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
        
             
## 4. Combining metadata, diversity, and % cover outputs
        
    surveysummary_2022 = merge(metadata, diversity_2022) 
    surveysummary_2022 = merge(surveysummary_2022, percent_cover_output)
    
    
## 5. Diversity Analysis
    
    # differences in richness by site
    surveysummary_2022 %>%
        t_test(sp_richness ~ site)

    # differences in richness by location of transect on reef
    surveysummary_2022 %>%
        t_test(sp_richness ~ qualitative_transect_position)
    
    # differences in richness by transect's location on reef for each site
        #Asan
        surveysummary_2022 %>%
            filter(site == "Asan") %>%
            t_test(sp_richness ~ qualitative_transect_position)
        #Agat
        surveysummary_2022 %>%
            filter(site == "Agat") %>%
            t_test(sp_richness ~ qualitative_transect_position)
        
    
## 6. Percent Cover Analysis

    
        
        
## 7. Community Composition Analysis: NMDS approach
        
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
    
        
        
## 8. Coral size (area) vs distance from shore/crest/water  
    
    