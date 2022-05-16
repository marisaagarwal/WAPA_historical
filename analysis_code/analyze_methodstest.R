#2022-04-29


## 1. Set up

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_methodstest.R"))
    

## 2. NMDS
    
    # conduct NMDS 
    test_NMDS = metaMDS(methodstest_datasummary_vegan[,3:9], 
                                 k = 2,
                                 distance = "bray", 
                                 trymax = 100)
    
    # examine stressplot & baseplot
    stressplot(test_NMDS)
    plot(test_NMDS)

    # create parsed down grouping dataframe and add row_ID column
    reference_methodstest = 
        methodstest_datasummary_vegan %>%
            dplyr::select(c(`sampling type`, test_number)) %>%
            mutate(row_ID = row_number())
    
    # extract data for plotting
    plotting_test_NMDS = 
        scores(test_NMDS, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("row_ID")
    
    plotting_test_NMDS = merge(reference_methodstest, plotting_test_NMDS)    
    
    
## 3. PERMANOVA (test for stat sig differences between groups)
    
    # difference in community based on sampling type? 
    
        # assumption: do groups have homogenous variances? 
        dis = vegdist(methodstest_datasummary_vegan[,3:9], method="bray")
        mod = betadisper(dis, reference_methodstest$`sampling type`)
        anova(mod)      # p>0.05, proceed
        # plot(mod)
    
    adonis2(methodstest_datasummary_vegan[,3:9] ~ `sampling type`, 
            data = reference_methodstest, 
            permutations = 999,
            method = "bray")                        # yes, p<0.05   
    
    
## 4. Which Species Contribute NMDS differences?
    
    # fit environmental and species vectors
    test_envfit =
        envfit(test_NMDS, 
               reference_methodstest, 
               permutations = 999) # this fits environmental vectors
    
    test_speciesfit =
        envfit(test_NMDS, 
               methodstest_datasummary_vegan[,3:9], 
               permutations = 999) # this fits species vectors
    
    # obtain scores & p-values
    test_species_scores =
        as.data.frame(scores(test_speciesfit,
                             display = "vectors"))                                      #save species intrinsic values into dataframe
    
    test_species_scores = cbind(test_species_scores, 
                                species = rownames(test_species_scores))        #add species names to dataframe
    
    test_species_scores = cbind(test_species_scores,
                                pval = test_species_scores$vectors$pvals)      #add pvalues to dataframe so you can select species which are significant
    
    
    test_species_scores = cbind(test_species_scores,
                                abrev = abbreviate(test_species_scores$species,
                                                       minlength = 4, 
                                                       method = "both"))                #abbreviate species names
    
    significant_test_species_scores = subset(test_species_scores,
                                                 pval <= 0.05)   
    
## 5. Amesbury analysis
    
    # load in functions from Mitchell (2007)
    source("http://math.hws.edu/pcqm/pcqm.txt")
    
    # calculating relative density, relative cover, relative freq 
       
         # true random 
        importance.val(z = true_random_df)
        
        # semi random
        importance.val(z = semi_random_df)
        
        # systematic
        importance.val(z = systematic_df)
        
    # # calculating total percent cover for the transect
    #     
    #     # for true random
    #     
    #         # find mean distance
    #         mean_distance = (sum(true_random_df$distance_to_point_M) / (4*length(unique(true_random_df$point_location))))
    #     
    #         # find absolute density
    #         abs_density = (1 / (mean_distance^2))
    #     
    #         # find total percent cover
    #         true_random_percent_cover = 
    #             true_random_df %>%
    #                 mutate(area_cm2 = (pi*(size^2))/4) %>%
    #                 group_by(species) %>%
    #                 summarise(freq_per_quarter = n()/nrow(true_random_df),
    #                           indv_per_m2 = freq_per_quarter * abs_density,
    #                           total_area_cm2 = sum(area_cm2),
    #                           mean_area_cm2 = mean(area_cm2)) %>%
    #                 mutate(total_indv_per_m2 = sum(indv_per_m2),
    #                        cover_area_m2 = mean_area_cm2 * indv_per_m2) %>%
    #                 summarise(total_cover_per_m2 = sum(cover_area_m2)) %>%
    #                 transmute(proportion_cover_on_transect = total_cover_per_m2 / 10000)



    # making a function to do this ^ for each transect
            
        # the function! 
        amesbury_totalcover_function <- function(df, group) {
                    
            df %>% 
                group_by({{group}}) %>% 
                summarise(freq_per_quarter = n()/nrow(df),
                          indv_per_m2 = freq_per_quarter * (1 / ((sum(df$distance_to_point_M) / (4*length(unique(df$point_location))))^2)),
                          total_area_cm2 = sum(area_cm2),
                          mean_area_cm2 = mean(area_cm2)) %>%
                mutate(total_indv_per_m2 = sum(indv_per_m2),
                       cover_area_m2 = mean_area_cm2 * indv_per_m2) %>%
                summarise(total_cover_per_m2 = sum(cover_area_m2)) %>%
                transmute(proportion_cover_on_transect = total_cover_per_m2 / 10000)
           
                }
                
                    # test_df1 = true_random_df %>% mutate(area_cm2 = (pi*(size^2))/4)
                    # function_test(df = test_df1, group = species) 
       
         # prepping the data and putting it into nested form
        test_data = 
            methodstest_data %>%
                mutate(area_cm2 = (pi*(size^2))/4) %>%
                nest_by(`sampling type`)
    
        # generating the percent_cover number and transect names
        percent_cover_output = 
            as.data.frame(do.call(rbind, 
                                  methodstest_data %>%
                                      mutate(area_cm2 = (pi*(size^2))/4) %>%
                                      nest_by(`sampling type`) %>%
                                      map(.x = test_data$data, 
                                          .f = ~ amesbury_totalcover_function(df = .x, group = species)))) %>%
            mutate(percent_cover = proportion_cover_on_transect *100,
                   rowID = row_number()) 
    
        transect_ID = 
            test_data %>% 
                dplyr::select(`sampling type`) %>%
                ungroup() %>%
                mutate(rowID = row_number()) %>%
                as.data.frame()
        
        # merging transect names and percent cover values
        final_percent_cover_output = merge(transect_ID, percent_cover_output)        
        
        