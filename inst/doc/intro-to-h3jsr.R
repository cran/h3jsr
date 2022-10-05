## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 6, fig.align = 'center',
  warning = FALSE,
  message = FALSE
)

## ----'pkgs', warning = FALSE, message = FALSE---------------------------------
local_options <- options()
library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)
# for R < 4, since H3 addresses are handled as strings
options(stringsAsFactors = FALSE)

## ----'c1'---------------------------------------------------------------------
# This is the location of the Brisbane Town Hall:
bth <- sf::st_sfc(sf::st_point(c(153.023503, -27.468920)), crs = 4326)

# where is the Brisbane Town Hall at resolution 15?
point_to_cell(bth, res = 15)

## ----'c2'---------------------------------------------------------------------
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_pts <- st_centroid(nc)
nc_pts <- st_transform(nc_pts, crs = 4326)
nc_pts <- dplyr::select(nc_pts, CNTY_ID, NAME)

# Give me the address for the center of each NC county at every resolution
nc_all_res <- point_to_cell(nc_pts, res = seq(0, 15), simple = FALSE)
head(nc_all_res[, c(1:5)])

## ----'c4'---------------------------------------------------------------------
# plot a few
ashe_hexes <- unlist(nc_all_res[1, c(6,7,8,9,10)], use.names = FALSE)
ashe_hexes <- cell_to_polygon(ashe_hexes, simple = FALSE)
ggplot(nc[1,]) +
  geom_sf(fill = NA, colour = 'black') +
  geom_sf(data = ashe_hexes, aes(fill = h3_address), alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('H3 hexagons over County Ashe, NC', subtitle = 'Resolutions 6-10') +
  theme_minimal() +
  coord_sf()

## ----'isval'------------------------------------------------------------------
is_valid(h3_address = '8abe8d12acaffff')
is_valid(h3_address = '8abe8d12aca')

## ----'ispent', fig.height=3, fig.width=3--------------------------------------
# is the following address a pentagon?
is_pentagon(h3_address = '8abe8d12acaffff')

get_pentagons(res = 8)

ggplot() +
  geom_sf(data = cell_to_polygon(get_pentagons(8)[[1]][1]), fill = NA) +
  theme_void()

## ----'isrc3'------------------------------------------------------------------
is_rc3(h3_address = '8abe8d12acaffff')

## ----'getb'-------------------------------------------------------------------
get_base_cell(h3_address = '8abe8d12acaffff')

## ----'getf'-------------------------------------------------------------------
get_faces(h3_address = '8abe8d12acaffff')

## ----'gr'---------------------------------------------------------------------
get_res(h3_address = '8abe8d12acaffff')

## ----'fam', fig.height=3, fig.width=3-----------------------------------------
# input is res 10:
get_parent(h3_address = '8abe8d12acaffff', res = 6)
# input is res 6:
get_children(h3_address = '86be8d12fffffff', res = 7)

ggplot() +
  geom_sf(data = cell_to_polygon('86be8d12fffffff'), fill = NA) +
  geom_sf(data = cell_to_polygon(get_children(h3_address = '86be8d12fffffff',
                                            res = 7)[[1]]),
          fill = 'red', alpha = 0.5 ) +
  theme_void()

## ----'cc', fig.height=3, fig.width=3------------------------------------------
# input is res 6:
get_centerchild(h3_address = '86be8d12fffffff', res = 7)

ggplot() +
  geom_sf(data = cell_to_polygon('86be8d12fffffff'), fill = NA) +
  geom_sf(data = cell_to_polygon(get_centerchild('86be8d12fffffff', 7)),
          fill = 'red') +
  geom_sf(data = cell_to_polygon(get_centerchild('86be8d12fffffff', 8)),
          fill = 'blue') +  
  theme_void()

## ----'disks'------------------------------------------------------------------
get_disk(h3_address = '86be8d12fffffff', ring_size = 2)

get_disk_list(h3_address = '86be8d12fffffff', ring_size = 2)

## ----'ring'-------------------------------------------------------------------
get_ring(h3_address = '86be8d12fffffff', ring_size = 2)

## ----'stmp', fig.height=3, fig.width=3----------------------------------------
disk <- get_disk(h3_address = '86be8d12fffffff', ring_size = 2)

ring <- get_ring(h3_address = '86be8d12fffffff', ring_size = 5)

patch_sf <- cells_to_multipolygon(disk, simple = FALSE)
donut_sf <- cells_to_multipolygon(ring, simple = FALSE)

ggplot() +
  geom_sf(data = patch_sf, alpha = 0.5) +
  theme_minimal() +
  geom_sf(data = donut_sf, alpha = 0.5, fill = 'red') +
  theme_void()

## ----'coolplot', fig.height=3, fig.width=3------------------------------------
disk_singles <- cell_to_polygon(unlist(disk, use.names = FALSE), simple = FALSE)
ring_singles <- cell_to_polygon(unlist(ring, use.names = FALSE), simple = FALSE)

ggplot(disk_singles) +
  geom_sf(aes(fill = 1:nrow(disk_singles)), show.legend = FALSE) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme_void()

ggplot(ring_singles) +
  geom_sf(aes(fill = 1:nrow(ring_singles)), show.legend = FALSE) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme_void()

## ----'pf'---------------------------------------------------------------------
ashe <- st_transform(nc[1, ], crs = 4326)
ashe_7 <- polygon_to_cells(ashe, res = 7, simple = FALSE)
ashe_7 <- cell_to_polygon(unlist(ashe_7$h3_addresses), simple = FALSE)

ggplot() +
  geom_sf(data = ashe, fill = NA) +
  geom_sf(data = ashe_7, fill = NA, colour = 'red') +
  ggtitle('Resolution 7 hexagons', subtitle = 'County Ashe, NC') +
  theme_minimal() +
  coord_sf()

## ----'compact'----------------------------------------------------------------
ashe_comp <- compact(ashe_7$h3_address)
ashe_comp <- cell_to_polygon(ashe_comp, simple = FALSE)

ggplot() +
  geom_sf(data = ashe, fill = NA) +
  geom_sf(data = ashe_comp, fill = NA, colour = 'red') +
  ggtitle('Compacted hexes from resolution 7', subtitle = 'County Ashe, NC') +
  theme_minimal() +
  coord_sf()


## ----'unc'--------------------------------------------------------------------
ashe_comp <- compact(ashe_7$h3_address)
ashe_uncomp <- uncompact(ashe_comp, res = 8)

ashe_uncomp <- cell_to_polygon(ashe_uncomp, simple = FALSE)

ggplot() +
  geom_sf(data = ashe, fill = NA) +
  geom_sf(data = ashe_uncomp, fill = NA, colour = 'red') +
  theme_minimal() +
  ggtitle('Uncompacted hexes to resolution 8', subtitle = 'County Ashe, NC') +
  coord_sf()


## ----'neigbs', fig.height=3, fig.width=3--------------------------------------
# Are the following addresses neighbours?
are_neighbours(origin = '86be8d12fffffff', destination = '86be8d127ffffff')

are_neighbours(origin = '86be8d12fffffff', destination = '86be8d147ffffff')

ggplot() +
  geom_sf(data = cell_to_polygon(c('86be8d12fffffff')),
          fill = c('red'), alpha =  0.5) +
  geom_sf(data = cell_to_polygon(c('86be8d127ffffff')),
          fill = c('blue'), alpha =  0.5) +  
    geom_sf(data = cell_to_polygon(c('86be8d147ffffff')),
          fill = c('green'), alpha =  0.5) +  
  theme_void()

## ----'udg'--------------------------------------------------------------------
# Get me the edge between these two addresses
get_udedge(origin = '86be8d12fffffff', destination = '86be8d127ffffff')

is_valid_edge('166be8d12fffffff')

# not neighbours:
#get_udedge(origin = '86be8d12fffffff', destination = '86be8d147ffffff')

## ----'udg2'-------------------------------------------------------------------
get_udorigin(h3_edge = '166be8d12fffffff')

get_uddest(h3_edge = '166be8d12fffffff')

get_udends(h3_edge = '166be8d12fffffff')

## ----'edges', fig.height=3, fig.width=3---------------------------------------
get_udedges(h3_address = '86be8d12fffffff')

ggplot() +
  geom_sf(data = cell_to_polygon('86be8d12fffffff'), col = NA) +
  geom_sf(data = udedge_to_line(get_udedges(h3_address = '86be8d12fffffff')[[1]]),
          aes(col = seq(6)), size = 2, show.legend = FALSE) +
  scale_color_viridis_c() +
  theme_void()


## -----------------------------------------------------------------------------
vtx0 <- vertex_to_point(get_cell_vertex('86be8d12fffffff', 0), simple = FALSE)
vtxs <- vertex_to_point(get_cell_vertexes('86be8d12fffffff')[[1]], simple = FALSE)
poly <- cell_to_polygon('86be8d12fffffff', simple = FALSE)
is_valid_vertex(get_cell_vertex('86be8d12fffffff', 0))

ggplot() +
  geom_sf(data = poly, col = NA) +
  geom_sf(data = vtxs, aes(col = seq(6)), size = 3, show.legend = FALSE) +
  geom_sf(data = vtx0, col = 'red', size = 5, pch = 1, show.legend = FALSE) +
  scale_color_viridis_c() +
  theme_void()


## ----'locals'-----------------------------------------------------------------
local <- get_local_ij(origin = '86be8d12fffffff', 
                      destination = '86be8d127ffffff')

get_local_cell(origin = '86be8d12fffffff', i = local[, 1], j = local[, 2])


## ----'gdist'------------------------------------------------------------------
nc_pts <- sf::st_centroid(nc[c(1, 2), ])
nc_6 <- point_to_cell(nc_pts, res = 6)
# how far apart are these two addresses?
grid_distance(nc_6[1], nc_6[2])

# find a path between these two addresses:
path <- grid_path(nc_6[1], nc_6[2], simple = TRUE)
path

## ----'h3l'--------------------------------------------------------------------
state_line <- cell_to_line(path)

ggplot() +
  geom_sf(data = nc[c(1,2), ], fill = NA) +
  geom_sf(data = sf::st_centroid(nc[c(1,2), ]), pch = 19, size = 2) +
  geom_sf(data = cell_to_point(nc_6), pch = 19, size = 2, col = 'red') +
  geom_sf(data = cell_to_polygon(nc_6), fill = NA) +
  geom_sf(data = state_line, fill = NA, colour = 'red') +
  theme_minimal() +
  ggtitle('Counties Ashe and Alleghany, NC', subtitle = 'Line connecting hexagons containing centroids at resolution 6') +
  coord_sf()


## ----'info'-------------------------------------------------------------------
res_area(6, 'km2')

res_length(6, 'km')

res_cendist(6, 'km')

num_cells(6)

data("h3_info_table")
str(h3_info_table)

## ----'info2'------------------------------------------------------------------
cell_area(h3_address = '8abe8d12acaffff', 'km2')

edge_length(h3_edge = '166be8d12fffffff', 'km')


## -----------------------------------------------------------------------------
x <- cell_to_splitlong(h3_address = '8abe8d12acaffff')

y <- splitlong_to_cell(split_lower = x[[1]][1], split_upper = x[[1]][2])

x
y


## -----------------------------------------------------------------------------
degs_to_rads(120)

rads_to_degs(1.5)


## -----------------------------------------------------------------------------
# reset local options
options(local_options)

