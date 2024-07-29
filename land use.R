# github.com/teyalex/wyckoff

# SETUP

  # setwd("/Volumes/SANDISK 1TB/code/EJI")
  
  library(tidyverse)

  # creating historic land use data frame
  
    landuse <- read.csv("Historic_Land_Use_Data_20240724.csv", stringsAsFactors = TRUE) %>%
      select(map_year, X2021_bbl, facility_type, facility_feature) %>%
      rename(year = map_year, bbl = X2021_bbl, fac_type = facility_type, fac_feature = facility_feature) %>%
      na.omit() %>%
      filter(!fac_type %in% "Unknown") %>%
      filter(str_detect(bbl, "^303168|^303167|^303177|^303176|^303189|^303188|^303200|^303199|^303211|^303210|^303222|^303221|^303238|^303237|^303249|^303248|^303260|^303259|^303271|^303270|^303281|^303280|^303291|^303290|^303302|^303301|^303311|^303310|^303320|^303319|^303329|^303328|^303338|^303337|^303444|^303346|^303544|^303354|^303545|^303546|^303379|^303547|^303386|^303548|^303393|^303549|^303400|^303550|^303407|^303551|^303413|^303552|^303539|^303553|^303540|^303554|^303541|^303555|^303556|^303542|^303557"))
  
  # creating current land use data frame
  
    pluto <- read.csv("PLUTO.csv", stringsAsFactors = TRUE) %>%
      select(bbl, yearbuilt, landuse) %>%
      na.omit() %>% 
      filter(str_detect(bbl, "^303168|^303167|^303177|^303176|^303189|^303188|^303200|^303199|^303211|^303210|^303222|^303221|^303238|^303237|^303249|^303248|^303260|^303259|^303271|^303270|^303281|^303280|^303291|^303290|^303302|^303301|^303311|^303310|^303320|^303319|^303329|^303328|^303338|^303337|^303444|^303346|^303544|^303354|^303545|^303546|^303379|^303547|^303386|^303548|^303393|^303549|^303400|^303550|^303407|^303551|^303413|^303552|^303539|^303553|^303540|^303554|^303541|^303555|^303556|^303542|^303557"))
    
    pluto$landuse <- factor(pluto$landuse, levels = as.character(1:11))
  
  # adding descriptions to landuse codes
  
    descriptions = c("1-2 Family", "Multi-Family Walk-Up", "Multi-Family Elevator",
                     "Mixed Residential & Commercial", "Commercial & Office", "Industrial & Manufacturing",
                     "Transportation & Utility", "Public Facilities & Institutions",
                     "Open Space & Outdoor Recreation", "Parking Facilities", "Vacant Land")
    
    pluto_descr <- data.frame(
      landuse = factor(1:11, levels = 1:11),
      descr = factor(descriptions, levels = descriptions)
    )
      
    pluto <- pluto %>%
      left_join(pluto_descr, by = "landuse") %>% 
      mutate(cat_name = case_when(
        landuse %in% c("1", "2", "3") ~ "Residential",
        landuse %in% "4" ~ "Mixed",
        landuse %in% c("5", "6", "7", "8") ~ "Nonresidential",
        landuse %in% c("9", "10", "11") ~ "Other",
        ))
    
    # creating version of pluto data frame binned by decades
    
      pluto_yearbins <- pluto %>%
        mutate(yearbuilt = as.numeric(yearbuilt),
               decade = floor(yearbuilt / 10) * 10)
  
# PLOTS
  
  # custom color scales for land uses
  
    plutocolors <- c(
      "1-2 Family" = "firebrick1",
      "Multi-Family Walk-Up" = "firebrick2",
      "Multi-Family Elevator" = "firebrick3",
      "Mixed Residential & Commercial" = "purple1",
      "Commercial & Office" = "steelblue2",
      "Industrial & Manufacturing" = "steelblue3",
      "Transportation & Utility" = "steelblue", 
      "Public Facilities & Institutions" = "steelblue4",
      "Open Space & Outdoor Recreation" = "seagreen",
      "Parking Facilities" = "gray80",
      "Vacant Land" = "gray60"
      )
  
    landusecolors <- c(
      "Residential/dwelling" = "firebrick2",
      "Railroad Yards" = "steelblue",
      "Parking" = "gray80",
      "Other" = "steelblue4",
      "Not reported" = "steelblue4"
      )

  # historical land use bar chart
  
    ggplot(landuse, aes(x = fac_type, fill = fac_feature)) +
      geom_bar() +
      scale_fill_manual(values = landusecolors) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(face = "italic"),
            plot.caption = element_text(face = "italic"),
            panel.grid.major.x = element_blank()
            ) +
      labs(title = "Historic land use on Wyckoff Avenue",
           subtitle = str_wrap("Land uses recorded sporadically on blocks bordering Wyckoff Ave., Kings Co., N.Y. from 1888 to 1981", 90),
           caption = str_wrap("Historic land use data maintained by the NYC Office of Environmental Mediation, updated Nov. 2021. Accessed July 2024 via NYC Open Data. github.com/teyalex/wyckoff", 110),
           x = "Land use type",
           y = "Number of lots",
           fill = "Land use category")

  # modern land use stacked bar chart

    ggplot(pluto, aes(x = cat_name, fill = descr)) +
      geom_bar() +
      scale_fill_manual(values = plutocolors) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(face = "italic"),
            plot.caption = element_text(face = "italic"),
            panel.grid.major.x = element_blank()
            ) +
      labs(title = "Contemporary land use on Wyckoff Avenue",
           subtitle = str_wrap("Land uses on blocks bordering Wyckoff Ave., Kings Co., N.Y. in 2024", 90),
           caption = str_wrap("Primary Land Use Tax Lot Output data maintained by the NYC Department of City Planning, updated July 2024. Accessed July 2024 via NYC Open Data. github.com/teyalex/wyckoff", 95),
           x = "Land use type",
           y = "Number of lots",
           fill = "Land use category"
           )

  # construction types binned by decades

    ggplot(pluto_yearbins, aes(x = factor(decade), fill = descr)) +
      geom_bar() +
      scale_fill_manual(values = plutocolors) +
      scale_x_discrete(breaks = c(0, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980,
                                  1990, 2000, 2010, 2020),
                       labels = c("N.D.", "1890s", "1900s", "1910s", "1920s", "1930s", "1940s",
                                  "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")) +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(face = "italic"),
            plot.caption = element_text(face = "italic"),
            panel.grid.major.x = element_blank()
      ) +
      labs(title = "Historic construction on Wyckoff Avenue",
           subtitle = str_wrap("Types of construction on blocks bordering Wyckoff Ave, Kings Co., N.Y.", 90),
           caption = str_wrap("Primary Land Use Tax Lot Output data maintained by the NYC Department of City Planning, updated July 2024. Accessed July 2024 via NYC Open Data. github.com/teyalex/wyckoff", 95),
           x = "Land use type",
           y = "Number of lots",
           fill = "Land use category"
      )