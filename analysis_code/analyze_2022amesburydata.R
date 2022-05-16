#2022-05-16


## 1. Set up

    # point to data locale
    data_locale = "creation_code/"
    
    # load in the data
    source(paste0(data_locale, "create_2022amesburydata.R"))

    
## 2. Amesbury-type analysis
    
    # load in functions from Mitchell (2007)
    source("http://math.hws.edu/pcqm/pcqm.txt")
    
    # calculating relative density, relative cover, relative freq
    
    all_data %>%
        dplyr::select(-c(date)) %>%
        group_by(site, transect) %>%
        importance.val()
    
    # calculating total percent cover for the transect
    
        # prepping the data and putting it into nested form
        all_data_percentcover = 
            all_data %>%
                mutate(area_cm2 = (pi*((colony_diameter_cm/2)^2))/4) %>%
                nest_by(transect)

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
                                      nest_by(transect) %>%
                                      map(.x = all_data_percentcover$data, 
                                          .f = ~ amesbury_totalcover_function(df = .x, group = species)))) %>%
            mutate(percent_cover = proportion_cover_on_transect *100,
                   rowID = row_number()) 
        
        transect_ID = 
            all_data_percentcover %>% 
                dplyr::select(transect) %>%
                ungroup() %>%
                    mutate(rowID = row_number()) %>%
                    as.data.frame()
        
        # merging transect names and percent cover values
        percent_cover_output = merge(transect_ID, percent_cover_output)
    
    