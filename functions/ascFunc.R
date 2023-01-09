### ASCgrid
### This is a function which generate a ASC grid. 
### 
### TODO make it so you can add commonwelath and state sxz separately
### make it so you can not add any sz or commonwealth mps
### thing about the outputs you might want 

ASCgrid <- function(poly, #  sf polygon to split
                     # ntz,
                     sz_sim, # sf polygon of sz scenarios to simulate
                     sz_current,# sf polygon with current sz, commonwealth and state
                     point, # spatial use data
                     from, # starting value of distance between grid lines
                     by, # incrementally distance increase to iterate over
                     min_area = 20, # min allowable area km2
                     vert = FALSE, # make line orientation perpendicular to lines in poly
                     crs) { # vertical or horizontal divisions

  # if (missing(poly) || missing(sz_sim) || missing(point)) {
  #   stop("Missing at least one sf file at argument, poly, ntz or point")
  # }
  # 
  #   if (class(poly) != "sf" || class(sz_sim) != "sf" || class(sz_current) != "sf" || class(point) != "sf" ) {
  #   stop("Poly, ntz or point is not an sf object. Change format to sf, see sf::st_as_sf()")
  #   }
  # 
  # if (missing(sz_current)) {
  #   stop("Missing sz_current, continuing assuming there none to be itegrated in the grid")
   
  # make crs for every sf object the same 
  poly %<>% st_transform(crs)
  # ntz %<>% st_transform(crs)
  sz_sim %<>% st_transform(crs)
  sz_current %<>% st_transform(crs)
  point %<>% st_transform(crs)
  
  # add npz col, all should be 1 and filled in with 0 when data expanded
  sz_current <- sz_current %>% 
    mutate(sz_current = 1)
  
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
  sim_holes <- st_difference(sz_sim, st_combine(sz_current)) # cut npz out of sims
  
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
    st_as_sf() %>%
    dplyr::select(-geom) %>%
    rename(geom = x)
  
  # chceking to see if there is data in the sims
  # temp <- as.data.frame(st_intersects(ntz, dat, sparse = FALSE)) %>%
  #   rowwise %>%
  #   mutate(inpoly_w_ntz = if_any(starts_with('V'), ~. == TRUE)) %>%
  #   ungroup()
  # 
  # ntz$data_present <- temp$inpoly_w_ntz
  # ntz %<>% mutate(id = row_number())
  # 
  # ntz %<>%
  #   mutate(data_present = temp$inpoly_w_ntz,
  #          id = row_number(),
  #          sz_w_value = ifelse(sz == 1 & data_present == TRUE, 1, 0),
  #          sz_wo_value = ifelse(sz == 1 & data_present == FALSE, 1, 0))
  #          
 ntz$use_count <- lengths(st_intersects(ntz, point))
  
  ntz %<>%
    mutate(sz_w_value = ifelse(use_count > 0, 1, 0),
           sz_wo_value = ifelse(use_count == 0, 1, 0),
           id = row_number())
  
  # cutting ntz out of polygon to make grid in
  poly_w_ntz <- st_difference(st_make_valid(poly), st_combine(ntz)) # crops ntz out of grid
  
  poly_w_ntz %<>% 
    st_cast('MULTIPOLYGON') %>%
    st_cast("POLYGON") 
  
  poly_w_ntz$area <- as.numeric(round(set_units(st_area(poly_w_ntz), km^2), 2))
  poly_w_ntz %<>% filter(area > 1)
  
  bbox <- st_bbox(poly_w_ntz) # make bbox
  
  # Grid <- seq(from = units::set_units(from, units), by = units::set_units(by, units)) # make sequence
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
        grid %<>% filter(area > min_area)
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
        grid %<>% filter(area > min_area)
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
  # }
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
    mutate(gridID_alt = row_number())
  
  full_grid$area <- as.numeric(round(set_units(st_area(full_grid), km^2), 2))
  full_grid$use_count <- lengths(st_intersects(full_grid, point))
  
  for (i in unique(full_grid$gridID_alt)) {
       gridID = ifelse(full_grid$gridID_alt == i, 1, 0)
       full_grid[,length(full_grid) + 1] <- gridID
       colnames(full_grid)[ncol(full_grid)] <- paste0("gridID_", i)
     }
  
  print(
    ggplot() +
      geom_sf(data = full_grid, aes(fill = as.factor(sz_w_value),
                                    color = as.factor(sz_wo_value)), lwd = 0.5) +
      geom_sf(data = point, size = 0.5)
  )
  
  print(
    ggplot() +
      geom_sf(data = full_grid, aes(fill = as.factor(gridID_alt)), lwd = 0.5)
  )
  return(full_grid)
}


## NOT FINISHED
## acAtt is a function to calculate the attributes for an ASC RUM> 
## 
## 
# acAtt <- function(grid, # sf object of acGrid
#                   point, # sf object of point data
#                   BR, # sf object of boat ramps in study site
#                   crs) # crs 
# {
#   grid %<>% st_transform(crs = crs)
#   point %<>% st_transform(crs = crs)
#   BR %<>% st_transform(crs = crs)
#   
#   centroid <- st_centroid_within_poly(grid) # gets centroid of every grid cell
#   grid$centroid <- centroid$geom # appending centroid
#   
#   # getting distance from each boat rampe to the centroid of the sites
#   # BR <- st_transform(BR, crs(grid))
#   # dist <- as.data.frame(set_units(st_distance(grid$centroid, BR), km)) 
#   # names(dist) <- unique(BR$RampID)
#   # gridID <- sites$gridID
#   # dist$gridID <- gridID
#   # dist <- dist %>% gather("BR", "site.centroid_km.BR", 1:length(dist) - 1) 
#   # sites <- sites %>% left_join(dist, by = "gridID") # join 
#   
#   return(grid)
# }
# 
# acAtt(grid, dat, 4283)