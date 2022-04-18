# 2022-04-14


## 1. Set up 

    # point to data locale
    data_locale = "data/"
    
    # point to data file
    data_file = "Amesbury et al. (1999) Survey Data.xlsx"
    
    # call to data
    transect_locations = 
        paste0(data_locale, data_file) %>%
        read_excel(sheet = "Amesbury (1999) Metadata")
    
    # check structure of data
    str(transect_locations)   
    
    
## 2. Groom data (format lat and long)
    
    # split apart lat and long into their own columns & add ID column
    transect_locations %<>%
        separate(col = "GPS Location",
                 into = c("DM_latitude", "DM_longitude"),
                 sep = ",") %>%
        mutate(id_column = c(1:nrow(transect_locations)))
    

## 3. Convert coordinates from Degrees-Minutes to Decimal Degrees
    
    # load required packages
    library(parzer)

    # create decimal degree coordinates from data
    transect_locations_DD = 
        parse_lon_lat(lat = transect_locations$DM_latitude, 
                      lon = transect_locations$DM_longitude) %>%
        mutate(id_column = c(1:nrow(transect_locations)))
    
    # merge data frames & rename columns
    transect_locations = 
        merge(transect_locations, transect_locations_DD) %>%
        rename(DD_latitude = lat, 
               DD_longitude = lon)
    

## 4. Export data for GIS plotting
    
    # add file to relevant folders within project
    write.csv(transect_locations, "data/transect_locations.csv")
    write.csv(transect_locations, "mapping/transect_locations.csv")
    
    
    
    