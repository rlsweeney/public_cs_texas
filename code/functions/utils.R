#===========================================================================
# landcover classification dictionary
#===========================================================================
classification <-
  list(
    "Open_Water" = 11, 
    "Perennial_IceSnow" = 12,
    "Developed_OS" = 21, 
    "Developed_LI" = 22, 
    "Developed_MI" = 23, 
    "Developed_HI" = 24, 
    "Barren" = 31, 
    "Deciduous_Forest" = 41, 
    "Evergreen_Forest" = 42, 
    "Mixed_Forest" = 43, 
    "Dwarf_Scrub" = 51, 
    "Shrub_Scrub" = 52, 
    "Grassland" = 71, 
    "Sedge" = 72, 
    "Lichens" = 73,
    "Moss" = 74, 
    "Pasture" = 81, 
    "Cultivated_Crops" = 82,
    "Woody_Wetlands" = 90, 
    "Herbaceous_Wetlands" = 95
  ) 
classification_dict <-  
  bind_cols("Classification" = names(classification), 
            "value" = reduce(classification, c))

#===========================================================================
# flag lessee's who we think are brokers
#===========================================================================
broker_regex <-
  c("propert", " land", "servic", "interest", "assoc", "dwyer",
    "david william", "johnson clay", "clay johnson", "san luis", "larry",
    "mayne", "trimble", "soape", "doug", "hodges", "bellomy", "blair",
    "robbins", "veritas", "horn", "culpepper", "cannon", "wormser", "reeves",
    "basin ventures", "transcendent", "dawson", "weatherl", "san saba",
    "saye", "christensen", "hemus", "kimmeridge", "burleson", "champion",
    "cole stephen", "dixon", "mccabe", "wolcott", "armstrong", "ramsey",
    "alpine", "webb", "huber", "shorthorn", "cervus", "angelle", "pinnacle",
    "mike gaddy", "parke", "leascher", "harris william p", "landsmith",
    "perry", "chalfant") %>%
  paste(collapse = "|") %>%
  regex(ignore_case = T)

nonbroker_regex <-
  regex("westridge|horizon|neuhaus|alta mesa", ignore_case = T)

#===========================================================================
# function for finding closest shale play
#===========================================================================
closest_shale <- function(df, tolerance = 0){
  start <- Sys.time()

  load(file.path(shape, "shale_plays.Rda"))

  df_crs <- st_crs(df)

  df <-
    df %>%
    st_transform(utm_crs) %>%
    mutate(id = row_number())

  # first find ones that are already on a shale
  onshale <-
    st_join(df, shale_plays, left = FALSE) %>%
    mutate(ShaleDist = 0)

  # now find closet shale
  # use the boundary of the shale and simplify so that there are less points
    # to find distances to
  shale_points <-
    shale_plays %>%
    st_simplify(dTolerance = tolerance) %>% 
    st_boundary() %>% 
    st_cast("POINT") 

  # make a plain shale dataset without geometries to use for closest_shale
  shale_data <-
    shale_points %>%
    as.data.frame() %>%
    select(-geometry) 

  closest_shale_values <-
    df %>%
    anti_join(select(as.data.frame(onshale), id))
  
  if(sum(st_geometry_type(st_geometry(df)) != "POINT") > 0){
    closest_shale_values <-
      closest_shale_values %>%
      st_centroid 
  } 

  closest_shale_values <- st_distance(closest_shale_values, shale_points) 


  # find shale formation that the lease centroid is closest to
  closest_shale_formation <-
    closest_shale_values %>%
    apply(1, which.min) %>%
    map_df(~ shale_data[.x,])

  # find distance to closest shale formation
  closest_shale_dist <-
    closest_shale_values %>%
    apply(1, min) 

  closest_shale <-
    df %>%
    anti_join(select(as.data.frame(onshale), id)) %>%
    bind_cols(tibble(ShaleDist = closest_shale_dist)) %>%
    bind_cols(closest_shale_formation) 

  output <-
    rbind(onshale, closest_shale) %>%
    mutate(OnShale = if_else(ShaleDist == 0, TRUE, FALSE)) %>%
    select(-id) %>%
    st_transform(df_crs) 

  print(paste("Done in", Sys.time() - start))

  return(output)
}


infra_dist <- function(df, inf, var_name, df_id){
  df_int <- 
    df %>%
    st_join(inf, left = FALSE) %>%
    st_set_geometry(NULL) %>%
    select({{ df_id }}) %>%
    mutate("{{var_name}}" := 0)

  no_int <-
    df %>%
    anti_join(df_int, by = as_label(enquo(df_id))) %>%
    st_centroid()

  # first find nearest feature to vectorize distance function 
  indices <- st_nearest_feature(no_int, inf)

  nearest <- inf$geometry[indices]
  dist <- st_distance(no_int$geometry, nearest, by_element = TRUE)

  df_no_int <-
    no_int %>%
    st_set_geometry(NULL) %>%
    as_tibble() %>%
    select({{ df_id }}) %>%
    mutate("{{var_name}}" := as.numeric(dist))

  df_dist <-
    rbind(df_int, df_no_int) %>%
    distinct() %>%
    group_by({{ df_id }}) %>%
    arrange({{ var_name }}) %>%
    filter(row_number() == 1) %>%
    ungroup

  return(df_dist)
}
