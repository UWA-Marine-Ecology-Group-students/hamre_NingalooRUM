
# ASC functions -----------------------------------------------------------


# This script contains functions useful when making an alternative specific constant rum of marine parks.
# ascGrid: makes asc grid, ensuring there is at least one data point in every cell.
# sim_stats: provides basic stats on simulation and associated plots
# mp_stats: provides basic stats on current mp and associated plots


## ASC grid ----------------------------------------------------------------


# This is a function which generate a ASC grid, given a base grid, simulation to be tested and use data. 
# The function ssplits each section of the base grid with vertical/horizontal lines at the minimum
# size to ensure every site has at least one data point. 
# By default saves 7x7 sz_value plot in current wd unless otherwise specified with parameters. 
# Function will crop all sf object to be within poly 

# 
#'[#TODO make it so you can add commonwealth and state sz separately]
#'[#TODO make sure you can get to work with projections with different units]


ASCgrid <- function(poly, #  sf polygon to split
                    # ntz,
                    sz_sim, # sf polygon of sz scenarios to simulate
                    sz_current,# sf polygon with current sz, commonwealth and state
                    point, # spatial use data
                    from, # starting value of distance between grid lines, same units as crs
                    by, # incrementally distance increase to iterate over, same units as crs
                    vert = FALSE, # make line orientation perpendicular to lines in poly
                    crs # vertical or horizontal divisions
) { # height if plots
  
  if (missing(poly) || missing(sz_sim) || missing(point)) {
    stop("Missing at least one sf file at argument, poly, ntz or point")
  }
  
  if (class(poly) != "sf" || class(sz_sim) != "sf" || class(sz_current) != "sf" || class(point) != "sf" ) {
    stop("poly, ntz or point is not an sf object. Change format to sf, see sf::st_as_sf()")
  } 
  
  #'[#TODO: add in ifelse statement so you can run scenarios where there are no current sz] 
  # if (missing(sz_current)) {
  #   stop("Missing sz_current, continuing assuming there no current sz")
  # }
  
  # make crs for every sf object the same 
  poly %<>% st_transform(crs) %>% st_make_valid()
  # ntz %<>% st_transform(crs)
  sz_sim %<>% st_transform(crs) %>% st_crop(poly) %>% st_make_valid()
  sz_current %<>% st_transform(crs) %>% st_crop(poly) %>% st_make_valid()
  point %<>% st_transform(crs) %>% st_crop(poly) %>% st_make_valid()
  
  # add npz col, all should be 1 and filled in with 0 when data expanded
  sz_current <- sz_current %>% 
    mutate(sz_current = 1) %>% 
    dplyr::select(sz_current)
  
  # add sz col all should be 1, and row numbers for loop
  sz_sim %<>% 
    mutate(sz = 1) %>% 
    mutate(id = row_number())
  
  # make a new binary column for each sanctuary zone
  for (i in 1:nrow(sz_sim)) {
    sz = ifelse(sz_sim$id == i, 1, 0)
    sz_sim[,length(sz_sim) + 1 ] <- sz 
    colnames(sz_sim)[ncol(sz_sim)] <- paste0("sz", i)
  }
  
  # making ntz layer: combining npz and current sz with sims
  sim_holes <- st_difference(sz_sim, st_union(st_combine(sz_current))) # cut npz out of sims
  sim_holes %<>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") # separate litte extra bits
  sim_holes$area <- as.numeric(set_units(st_area(sim_holes), km^2)) # calc area
  sim_holes %<>% filter(area > 10) # remove slivers under 10 km2
  sim_holes %<>% dplyr::select(-area)
  
  # joining sims and npz while maintaining attributes
  sim_holes_df <- sim_holes %>% as.data.frame() %>% mutate(geom = as.character(geom))
  sz_current_df <- sz_current %>% as.data.frame() %>% mutate(geom = as.character(geom))
  
  ntz <- full_join(sim_holes_df, sz_current_df) %>% 
    mutate(id = row_number()) %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  sim_holes_geom <- st_as_sf(sim_holes$geom)
  npz_geom <- st_as_sf(sz_current$geom)
  
  ntz_geom <- rbind(sim_holes_geom, npz_geom)
  
  ntz <- cbind(ntz, ntz_geom) %>% 
    dplyr::select(-geom) %>%
    rename(geom = x) %>% 
    st_as_sf() 
  
  # # joining sims and npz while maintaining attributes
  # chunk for when using pre-made sim with holes
  # sz_sim_df <- sz_sim %>% as.data.frame() %>% mutate(geom = as.character(geom))
  # sz_current_df <- sz_current %>% as.data.frame() %>% mutate(geom = as.character(geom))
  # 
  # ntz <- full_join(sz_sim_df, sz_current_df) %>%
  #   mutate(id = row_number()) %>%
  #   mutate_if(is.numeric, ~replace_na(., 0))
  # 
  # sz_sim_geom <- st_as_sf(sz_sim$geom)
  # npz_geom <- st_as_sf(sz_current$geom)
  # 
  # ntz_geom <- rbind(sz_sim_geom, npz_geom)
  # 
  # ntz <- cbind(ntz, ntz_geom) %>%
  #   st_as_sf() %>%
  #   dplyr::select(-geom) %>%
  #   rename(geom = x)
  
  # count number of uses in each site   
  ntz$use_count <- lengths(st_intersects(ntz, point))
  
  # identify sz with and without data/recreational value  
  ntz %<>%
    mutate(sz_value = ifelse(use_count > 0, 1, 0)) %>% 
    mutate(sz = ifelse(sz_current %in% 1, 1, sz)) %>% 
    mutate(id = row_number())
  
  # cutting ntz out of polygon to make grid in
  poly_w_ntz <- st_difference(st_make_valid(poly), st_union(st_combine(ntz))) # crops ntz out of grid
  
  poly_w_ntz %<>% 
    st_cast('MULTIPOLYGON') %>%
    st_cast("POLYGON") 
  
  poly_w_ntz$area <- as.numeric(round(set_units(st_area(poly_w_ntz), km^2), 2))
  poly_w_ntz %<>% filter(area > 1)
  
  bbox <- st_bbox(poly_w_ntz) # make bbox
  
  Grid <- seq(from = from, by = by)
  # makes grid & checks data presents, if not loops over next grid
  acGrids <- data.frame()
  
  for (j in 1:nrow(poly_w_ntz)) {
    
    p <- poly_w_ntz[j,]
    
    print(paste("Making grid for feature", j, sep = " "))
    
    if (vert == FALSE) {
      
      for (i in Grid) {
        grid <- p %>% st_make_grid(cellsize = c(bbox$xmax - bbox$xmin, i))
        grid <- st_intersection(p, grid)
        grid$area <- as.numeric(round(set_units(st_area(grid), km^2), 2))
        # grid %<>% filter(area > min_area)
        temp <- as.data.frame(st_intersects(grid, point, sparse = FALSE)) %>%
          rowwise %>%
          mutate(inp = if_any(starts_with('V'), ~. == TRUE)) %>%
          ungroup()
        grid$data_present <- temp$inp
        grid %<>% mutate(id = row_number())
        
        if (any(grid$data_present == FALSE)) {
          print(paste0("Missing data (", i ,"), making new grid"))
        } else {
          print(paste("Feature", j, "has a", i, "Grid", sep = " "))
          break
        }
      }
    } else {
      
      for (i in Grid) {
        grid <- p %>% st_make_grid(cellsize = c(i, bbox$ymax - bbox$ymin))
        grid <- st_intersection(p, grid)
        grid$area <- as.numeric(round(set_units(st_area(grid), km^2), 2))
        # grid %<>% filter(area > min_area)
        temp <- as.data.frame(st_intersects(grid, point, sparse = FALSE)) %>%
          rowwise %>%
          mutate(inp = if_any(starts_with('V'), ~. == TRUE)) %>%
          ungroup()
        grid$data_present <- temp$inp
        grid %<>% mutate(id = row_number())
        
        if (any(grid$data_present == FALSE)) {
          print(paste0("Missing data (", i ,"), making new grid"))
        } else {
          print(paste(i, "Grid", sep = " "))
          break
        }
        
      }
    }
    acGrids %<>% rbind(grid)
  }
  
  grid <- acGrids %>% mutate(id = row_number()) %>% st_as_sf(crs = crs)
  
  grid_geom <- st_as_sf(grid$geom)
  ntz_geom <- st_as_sf(ntz$geom)
  full_geom <- rbind(grid_geom, ntz_geom)
  
  grid  %<>% as.data.frame() %>% mutate(geom = as.character(geom))
  ntz %<>% as.data.frame() %>% mutate(geom = as.character(geom))
  
  full_grid <- full_join(grid, ntz) %>%
    mutate_if(is.numeric, ~replace_na(., 0))
  
  full_grid <- cbind(full_grid, full_geom) %>%
    st_as_sf() %>%
    dplyr::select(-geom) %>%
    rename(geom = x,
           gridID_alt = id) %>% 
    mutate(gridID_alt = row_number()) %>% 
    mutate(sz_value = ifelse(sz == 0, NA, sz_value)) %>% 
    mutate(sz_type = ifelse(sz_current %in% 1 & sz %in% 1, "Current",
                            ifelse(sz_current %in% 0 & sz %in% 1, "Simulated", NA)))
  
  
  # merging geometries that have no data becayuse of base_grid and szs defined
  st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****") # find neighbour (nb) function
  
  temp_grid <- full_grid %>% mutate(grid_nb = st_rook(.))  # find neighbours (nb) for each grid cell
  merge_from <- full_grid %>% filter(data_present == FALSE) #cells with no data that need merged
  grid0_nb_list <- st_intersects(merge_from, full_grid) %>% unlist() #id list of all empty grid nbs
  
  # df of nb to merge to - this will only merge to areas which are not in a sz so will exclude shore fishing
  # zones surrounded by sz, those area will have to be removed. 
  
  merge_to <- full_grid %>% filter(gridID_alt %in% grid0_nb_list, data_present == TRUE) 
  merge_to_id <- as.character(unique(merge_to$gridID_alt))
  
  if (nrow(merge_to) != 0) {
    temp_grid %<>%
      mutate(merge_to = ifelse(str_detect(grid_nb, merge_to_id), merge_to_id, 0)) %>% # ids all rows nb with merge_to
      mutate(merge_to = ifelse(merge_to > 0 & data_present == TRUE | merge_to > 0 & sz == 1, 
                               0, merge_to)) %>%  # selects only the nbs to be merged
      mutate(gridID_alt = ifelse(merge_to > 0, merge_to, gridID_alt)) %>% 
      group_by(gridID_alt) %>% 
      summarise()
    
    # turn into dfs and left join
    full_grid %<>% as.data.frame() %>% dplyr::select(-geom)
    temp_grid %<>% 
      mutate(gridID_alt = as.numeric(gridID_alt)) %>% 
      rename(geometry = geom) %>% 
      as.data.frame()
    full_grid <- left_join(temp_grid, full_grid, by = "gridID_alt") %>% st_as_sf()
    
  }
  
  full_grid$area <- as.numeric(round(set_units(st_area(full_grid), km^2), 2))
  full_grid$use_count <- lengths(st_intersects(full_grid, point))
  
  
  emp <- full_grid %>% filter(data_present == FALSE)
  
  if (nrow(emp) > 0) {
    print(paste("gridID", emp$gridID_alt, "have no data. Check before removing", sep = " "))
    
    print(
      ggplot() +
        geom_sf(data = emp) +
        ggtitle("Sites with no data")
    )
    
  } else{
    print("All sites contain data.")
  }
  
  for (i in unique(full_grid$gridID_alt)) {
    gridID = ifelse(full_grid$gridID_alt == i, 1, 0)
    full_grid[,length(full_grid) + 1] <- gridID
    colnames(full_grid)[ncol(full_grid)] <- paste0("gridID_", i)
  }
  
  print(
    ggplot() +
      geom_sf(data = full_grid, aes(fill = as.factor(gridID_alt)), lwd = 0.25) +
      ggtitle("Check no cells have the same ID")
  )
  return(full_grid)
}


# sim_stats ---------------------------------------------------------------


# This function produces a table a basic simulation stats; 
#   sz value, number of trips, % of total trips, area, % of mp area, length of beach access, % of MP beach access.
#
# It also produces 2 plots which will be saved in current directory unless otherwise stated; sz_value plot of ascGrid and mp,
# and a validation plot showing how the beach access length has been calculated.
# 
# This function can only be run after ascGrid.

sim_stats <- function(ascGrid, # sf of asc grid
                      point, # sf of point data
                      mp, # sf of mp boundaries
                      coast, # sf object of coast
                      crs = 4283, # crs
                      save = TRUE, # do you want to save sz_value plot
                      sim_name, # to be associated with saved plots
                      path = getwd(), # directory to save plots in
                      width = 7, # width of plots
                      height = 7){ # hieght of plots
  
  if (missing(ascGrid) || missing(mp) || missing(point) || missing(coast)) {
    stop("Missing at least one sf file at argument, ascGrid, point, mp or coast")
  }
  
  if (class(ascGrid) != "sf" || class(mp) != "sf" || class(coast) != "sf" || class(point) != "sf" ) {
    stop("ascGrid, mp, point or coast is not an sf object. Change format to sf, see sf::st_as_sf()")
  } 
  
  if (missing(crs)) {
    message("No specified crs; using default (crs = 4283)")
  } 
  
  if (save == TRUE & missing(sim_name)) {
    stop("Saving with no sim_name; please give your simulation a name or change save to FALSE.")
  }
  
  if (save == TRUE & missing(path)) {
    message("Saving with no specified path; will save in current working directory.")
  }
  
  if (save == TRUE & missing(width) || save == TRUE & missing(height)) {
    message("Saving with no specified dimensions; saving with default dimensions (7 x 7)")
  }
  
  # transform to same crs
  ascGrid %<>% st_transform(crs)
  mp %<>% st_transform(crs)
  coast %<>% st_transform(crs)
  
  # simulated sz
  sim <- ascGrid %>% filter(sz_type %in% "Simulated")
  
  # total calcs
  n_uses <- sum(ascGrid$use_count) # total uses
  mp_area <- as.numeric(round(set_units(st_area(mp), km^2), 2)) # total area of mp
  
  
  # calculating length fo coast
  # mp length
  coast_buff <- st_buffer(coast, dist = 0.001) # buffer coast
  mp_coast <- st_intersection(mp, coast_buff) # get intersection between coast and mp
  mp_coast$length <- st_length(mp_coast) # calc length
  mp_coast <- mp_coast %>% slice(which.max(length)) # select biggest length (sometime harbours get in way)
  mp_length <- round(set_units(mp_coast$length, "km"), 2) # extracting length
  
  # sz length
  sz_coast <- st_intersection(sim, mp_coast) # intersecting with coast
  sz_coast$length <- set_units(st_length(sz_coast), "km") # calc length
  sz_length <- round(sum(sz_coast$length), 2)
  
  # sz stats by value
  t1 <- sim %>%
    group_by(sz_value) %>%
    summarise(area = sum(area),
              permp = round((area/sum(mp_area))*100, 2),
              nTrips = sum(use_count),
              perTrips = round((nTrips/sum(n_uses))*100, 2))
  
  # length of coast
  t2 <- sz_coast %>%
    group_by(sz_value) %>%
    summarise(len = as.numeric(round(sum(length), 2)),
              perCoast = as.numeric(round((len/sum(mp_length))*100, 2)))
  
  
  
  t1$len <- t2$len
  t1$perCoast <- t2$perCoast
  sz_area <- sum(t1$area) # total sz area
  
  t1 <- t1 %>% 
    mutate(sz_value = ifelse(sz_value == 1, "Yes", "No")) %>% 
    relocate(sz_value, nTrips, perTrips, area, permp, len, perCoast) %>% 
    rename(!!paste0("SZ area", " (", sz_area, " km2)") := area,
           !!paste0("% of mp", " (", mp_area, " km2)") := permp,
           "# Trips" = nTrips,
           "Rec. value" = sz_value,
           !!paste0("% of Trips", " (", n_uses, ")") := perTrips,
           !!paste0("Beach Access", " (", sz_length, " km)") := len,
           !!paste0("% of mp", " (", mp_length, " km)") := perCoast) %>% 
    st_drop_geometry()
  
  print(
    ggplot() +
      geom_sf(data = ascGrid, aes(fill = as.factor(sz_value)), lwd = 0.25) +
      geom_sf(data = mp, lwd = 0.5, colour = "darkcyan", fill = NA) +  #'[#TODO: add legend to show mp boundaries]  
      geom_sf(data = point, size = 0.25) +
      guides(fill = guide_legend("", byrow = TRUE)) +
      scale_fill_manual(na.translate = FALSE,
                        values = c("#8fcacaff", "#cce2cbff"), 
                        labels = c('SZ sim with\nno value', 'SZ sim with\nvalue')) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm"))
  )
  
  # save plot
  if (save == TRUE) {
    ggsave(path = path, 
           filename = paste("asc_sz_value_", sim_name, ".png", sep = ''), 
           width = width, height = height)
  }
  
  mp_point <- st_crop(point, mp)
  
  print(
    ggplot() +
      geom_sf(data = mp, lwd = 0.25) +
      geom_sf(data = sim, aes(fill = as.factor(sz_value)), lwd = 0.25) +
      geom_sf(data = mp_point, size = 0.25) +
      scale_fill_manual(na.translate = FALSE,
                        values = c("#8fcacaff", "#cce2cbff"),
                        labels = c('SZ sim with\nno value', 'SZ sim with\nvalue')) +
      guides(fill = guide_legend("", byrow = TRUE)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm"))
  )
  
  # save plot
  if (save == TRUE) {
    ggsave(path = path, 
           filename = paste("mp_sz_value_", sim_name, ".png", sep = ''), 
           width = width, height = height)
  }
  
  # validation plot: length of coast measured
  print(
    ggplot() +
      geom_sf(data = mp, lwd = 0.25) +
      geom_sf(data = sim, lwd = 0.25) +
      geom_sf(data = mp_coast, colour = 'red') +
      geom_sf(data = sz_coast, color = "blue") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm")) +
      ggtitle("Check beach access has been calculated correctly")
  )
  
  return(t1)
}


# mp_stats ----------------------------------------------------------------


# This function produces a table of basic mp stats; 
#   area, % of mp area, length of beach access, % of MP beach access.
#
# It also produces 2 plots which will be saved in current directory unless otherwise stated; sz_value plot of ascGrid and mp,
# and a validation plot showing how the beach access length has been calculated.
# 
# This function can only be run after ascGrid

mp_stats <- function(ascGrid,
                     point, # sf of point data
                     mp, # sf of mp boundaries
                     coast, # sf object of coast
                     crs = 4283, # crs
                     save = TRUE, # do you want to save sz_value plot
                     mp_name, # to be associated with saved plots
                     path = getwd(), # directory to save plots in
                     width = 7, # width of plots
                     height = 7){ # hieght of plots
  
  if (missing(ascGrid) || missing(mp) || missing(point) || missing(coast)) {
    stop("Missing at least one sf file at argument, ascGrid, mp, point or coast")
  }
  
  if (class(ascGrid) != "sf" || class(mp) != "sf" || class(coast) != "sf" || class(point) != "sf" ) {
    stop("ascGrid, point, mp or coast is not an sf object. Change format to sf, see sf::st_as_sf()")
  } 
  
  if (missing(crs)) {
    message("No specified crs; using default (crs = 4283)")
  } 
  
  if (save == TRUE & missing(mp_name)) {
    stop("Saving with no mp_name; please give your marine park a name or change save to FALSE.")
  }
  
  if (save == TRUE & missing(path)) {
    message("Saving with no specified path; will save in current working directory.")
  }
  
  if (save == TRUE & missing(width) || save == TRUE & missing(height)) {
    message("Saving with no specified dimensions; saving with default dimensions (7 x 7)")
  }
  
  # transform to same crs
  ascGrid %<>% st_transform(crs)
  point %<>% st_transform(crs)
  mp %<>% st_transform(crs)
  coast %<>% st_transform(crs)
  
  sz <- ascGrid %>% filter(sz_current == 1)
  
  # total calcs
  n_uses <- sum(ascGrid$use_count) # total uses
  mp_area <- as.numeric(round(set_units(st_area(mp), km^2), 2)) # total area of mp
  
  
  # calculating length fo coast
  # mp length
  coast_buff <- st_buffer(coast, dist = 0.001) # buffer coast
  mp_coast <- st_intersection(mp, coast_buff) # get intersection between coast and mp
  mp_coast$length <- st_length(mp_coast) # calc length
  mp_coast <- mp_coast %>% slice(which.max(length)) # select biggest length (sometime harbours get in way)
  mp_length <- round(set_units(mp_coast$length, "km"), 2) # extracting length
  
  # sz length
  sz_coast <- st_intersection(sz, mp_coast) # intersecting with coast
  sz_coast$length <- set_units(st_length(sz_coast), "km") # calc length
  sz_length <- round(sum(sz_coast$length), 2)
  
  # sz stats by value
  t1 <- sz %>%
    group_by(sz_value) %>%
    summarise(area = sum(area),
              permp = round((area/sum(mp_area))*100, 2),
              nTrips = sum(use_count),
              perTrips = round((nTrips/sum(n_uses))*100, 2))
  
  # length of coast
  t2 <- sz_coast %>%
    group_by(sz_value) %>%
    summarise(len = as.numeric(round(sum(length), 2)),
              perCoast = as.numeric(round((len/sum(mp_length))*100, 2)))
  
  
  
  t1$len <- t2$len
  t1$perCoast <- t2$perCoast
  sz_area <- sum(t1$area) # total sz area
  
  t1 <- t1 %>% 
    mutate(sz_value = ifelse(sz_value == 1, "Yes", "No")) %>% 
    relocate(sz_value, nTrips, perTrips, area, permp, len, perCoast) %>% 
    rename(!!paste0("SZ area", " (", sz_area, " km2)") := area,
           !!paste0("% of mp", " (", mp_area, " km2)") := permp,
           "# Trips" = nTrips,
           "Rec. value" = sz_value,
           !!paste0("% of Trips", " (", n_uses, ")") := perTrips,
           !!paste0("Beach Access", " (", sz_length, " km)") := len,
           !!paste0("% of mp", " (", mp_length, " km)") := perCoast) %>% 
    st_drop_geometry()
  
  print(
    ggplot() +
      geom_sf(data = ascGrid, aes(fill = as.factor(sz_current)), lwd = 0.25) +
      guides(fill = guide_legend("", byrow = TRUE)) +
      scale_fill_manual(values = c(NA, "#85b8a6"), 
                        na.translate = FALSE,
                        labels = c('Not a\ncurrent SZ', 'Current SZ')) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm"))
  )
  
  # save plot
  if (save == TRUE) {
    ggsave(path = path, 
           filename = paste("mp_sz_value_", mp_name, ".png", sep = ''), 
           width = width, height = height)
  }
  
  # validation plot: length of coast measured
  print(
    ggplot() +
      geom_sf(data = mp, lwd = 0.25) +
      geom_sf(data = sz, lwd = 0.25) +
      geom_sf(data = mp_coast, colour = 'red') +
      geom_sf(data = sz_coast, color = "blue") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm")) +
      ggtitle("Check beach access has been calculated correctly")
  )
  
  return(t1)
}


# ascAtt ------------------------------------------------------------------


# ascAtt is a function to calculate the attributes for an ASC RUM.

ascAtt <- function(ascGrid, # sf object of ascGrid (make sure geometry is called geom)
                   grid_id, # col in ascGrid with grid id
                   point, # sf object of point data
                   br, # sf object of boat ramps in study site
                   br_id, # col in BR with ramp id
                   crs) # crs
{
  
  br_id_name <- br_id # value which is name of columns
  br_id <- br[,which(names(br) %in% br_id)] %>%  st_drop_geometry()# the col
  # br %<>% 
  #   mutate(br_lon = st_coordinates(.)[,1]) %>%
  #   mutate(br_lat = st_coordinates(.)[,2]) %>% 
  #   mutate(br_geom = paste0(br_lon, br_lat)) %>% 
  #   dplyr::select(-c(br_lon, br_lat))
  
  grid_id_name <- grid_id # value which is name of columns
  grid_id_vis <- ascGrid[,which(names(ascGrid) == grid_id)]
  grid_id <- ascGrid[,which(names(ascGrid) == grid_id)] %>% st_drop_geometry()# the col
  
  ascGrid %<>% st_transform(crs = crs)
  point %<>% st_transform(crs = crs)
  br %<>% st_transform(crs = crs)
  
  centroid <- st_centroid_within_poly(ascGrid) # gets centroid of every ascGrid cell
  ascGrid$centroid <- centroid$geom # appending centroid
  
  # validation plot: site centroids
  print(
    ggplot() +
      geom_sf(data = ascGrid$geom) +
      geom_sf(data = ascGrid$centroid, color = 'red', size = 1) +
      ggtitle("Check red dots are in centroid of cells") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm"))
  )
  
  # get distance from each ramp to the centroid of the sites
  dist <- as.data.frame(round(set_units(st_distance(ascGrid$centroid, br), km), 2)) # calc dist (km)
  names(dist) <- pull(br_id[,1] %>% st_drop_geometry()) # extract ramp ids
  dist$grid_id <- pull(grid_id)
  names(dist)[names(dist) == "grid_id"] <- grid_id_name # change name to match original
  dist %<>% gather("BR", "site.centroid_km.br", 1:length(dist) - 1) # gather
  names(dist)[names(dist) == "BR"] <- br_id_name # change name to match original
  dist[] <- sapply(dist, as.numeric) # change all cols to numeric
  dist <- dist %>% left_join(br) # join to br data
  ascGrid <- ascGrid %>% left_join(dist, by = grid_id_name) # join to grid
  ascGrid$fac_br_id <- as.factor(pull(br_id))
  ascGrid %<>% rename(asc_geom = geom, br_geom = geometry)
  
  # validation plot: distances to site centroid
  print(
    ggplot(ascGrid) +
      geom_sf(aes(geometry = asc_geom, fill = ascGrid$site.centroid_km.br), lwd = 0.25) +
      geom_sf(aes(geometry = br_geom, colour = as.factor(br_id_name)), size = 3) +
      theme_classic() +
      scale_fill_distiller(palette = "Blues") +
      labs(fill = "km from BR", colour = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.spacing.y = unit(0.75, "cm")) +
      facet_grid(.~fac_br_id) +
      ggtitle("Check distances look accurate")
  )
  
  return(ascGrid)
  stop()
  
  #'[#TODO:Need to join br_geom to point so I can have a common var to join with at expansion.]  
  
  # allocating sites to grids
  # point  %<>% st_join(grid_id_vis, left = T, join = st_intersects)
  # 
  # # selecting attributes, and tidying
  # point %<>% 
  #   rename_at( vars(contains(grid_id_name)), ~paste0("gridID_vis")) %>% 
  #   distinct() %>%
  #   mutate(UseLong = st_coordinates(.)[,1]) %>%
  #   mutate(UseLat = st_coordinates(.)[,2]) %>%
  #   st_drop_geometry()
  # 
  # # checkpoint
  # v1 <- which(is.na(point$gridID_vis) == TRUE)
  # if (identical(v1, integer(0)) == FALSE) {
  #   stop("Not all sites have been assigned a gridID_vis: NAs present in gridID_vis")
  # } else {
  #   message("All sites have been assigned a gridID_vis")
  # }
  # 
  # t1 <- nrow(point)
  
  # point <- inner_join(point, ascGrid, by = br_id_name) # FALSE
  
  # validation: testing expansion
  # if (t1*nrow(grid_id) != nrow(point)) {
  #   warning("Incorrect expansion: number of trips * number of sites != nrow(point)")
  # } else {
  #   message("Correct expansion: number of trips * number of sites == nrow(point)")
  # }
  
  # print(ascGrid)
  # return(point)
  
}
