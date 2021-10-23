library(dplyr)
library(ggplot2)

# Read data ---------------------------------------------------------------
species_data <- readr::read_delim("data/species_data.csv",
                                  delim = "\t")

header_data <- readr::read_delim(
  "data/header_data.csv",
  delim = "\t",
  escape_backslash = F,
  escape_double = F
)


# Convert species data into numeric values --------------------------------
old_table <- species_data[, 1:31]
converted_table <- old_table

converted_table[old_table == 'r' | old_table == 'r '] <- '1'
converted_table[old_table == '+' | old_table == '+ '] <- '2'
converted_table[old_table == '1'] <- '3'
converted_table[old_table == '2'] <- '4'
converted_table[old_table == '3'] <- '5'
converted_table[old_table == '4'] <- '6'
converted_table[old_table == '5'] <- '7'
converted_table[old_table == 'p'] <- '2.5'

converted_table[, 3:31] <-
  sapply(converted_table[, 3:31], as.numeric)
species_data[, 1:31] <- converted_table
rm(old_table, converted_table)



# Calculate indicator values per plot -------------------------------------
landolt <- species_data[, c(1, 2, 32:34)]
data <- species_data[, 3:31]
temp <-
  colSums(data * landolt$temp, na.rm = T) / colSums(data[!is.na(landolt$temp),],
                                                    na.rm = T)
cont <-
  colSums(data *  landolt$cont, na.rm = T) / colSums(data[!is.na(landolt$cont), ], 
                                                     na.rm = T)
light <-
  colSums(data *  landolt$light, na.rm = T) / colSums(data[!is.na(landolt$light), ], 
                                                      na.rm = T)

results <-
  data.frame(orig_num = 1:29, temp, cont, light) %>%
  inner_join(header_data) %>%
  arrange(num) %>%
  as_tibble()

# exposition
results[results$exposition == '-', 'exposition'] <- NA
results$exposition <- as.numeric(results$exposition)

# soil temperature
results[results$soil_temp == "?", 'soil_temp'] <- NA
results$soil_temp <- as.numeric(results$soil_temp)

results
rm(cont, light, temp, data, landolt)


# Calculate forest indicator values ---------------------------------------


### Tree / shrub layer
landolt <- species_data[, c(1, 2, 32:34)]

data <- species_data[, 3:31]
data[!species_data$layer  %in% c('S', 'T'),] <- NA

temp <-
  colSums(data * landolt$temp, na.rm = T) / colSums(data[!is.na(landolt$temp), ], 
                                                    na.rm = T)
cont <-
  colSums(data *  landolt$cont, na.rm = T) / colSums(data[!is.na(landolt$cont), ], 
                                                     na.rm = T)
light <-
  colSums(data *  landolt$light, na.rm = T) / colSums(data[!is.na(landolt$light), ], 
                                                      na.rm = T)

results_tree_shrub <-
  data.frame(orig_num = 1:29, temp, cont, light) %>%
  inner_join(header_data) %>%
  arrange(num) %>%
  filter(type == 'Forest') %>%
  as_tibble()

results_tree_shrub
rm(cont, light, temp, data, landolt)


## all other layers

landolt <- species_data[, c(1, 2, 32:34)]

data <- species_data[, 3:31]
data[species_data$layer  %in% c('S', 'T'),] <- NA

temp <-
  colSums(data * landolt$temp, na.rm = T) / colSums(data[!is.na(landolt$temp), ], 
                                                    na.rm = T)
cont <-
  colSums(data *  landolt$cont, na.rm = T) / colSums(data[!is.na(landolt$cont), ], 
                                                     na.rm = T)
light <-
  colSums(data *  landolt$light, na.rm = T) / colSums(data[!is.na(landolt$light), ], 
                                                      na.rm = T)

results_herb <-
  data.frame(orig_num = 1:29, temp, cont, light) %>%
  inner_join(header_data) %>%
  arrange(num) %>%
  filter(type == 'Forest') %>%
  as_tibble()

rm(cont, light, temp, data, landolt)


# Calculate NMDS ----------------------------------------------------------
nmds_data <-
  species_data %>%
  select(-temp,-cont,-light) %>%
  mutate(name = paste(layer, species, sep = '_')) %>%
  select(-layer,-species) %>%
  as.data.frame()

rownames(nmds_data) <- nmds_data$name
nmds_data$name <- NULL

nmds_data[is.na(nmds_data)] <- 0
nmds_data <- nmds_data[!rowSums(nmds_data) == 0, ]

nmds_data_t <- data.table::transpose(nmds_data)
colnames(nmds_data_t) <- rownames(nmds_data)
rownames(nmds_data_t) <- colnames(nmds_data)

nmds1 <- vegan::metaMDS(nmds_data_t)


# Plot NMDS ---------------------------------------------------------------

nmds_plotting_data <-
  as.data.frame(nmds1$points) %>%
  mutate(orig_num = as.numeric(row.names(nmds1$points))) %>%
  inner_join(results) %>%
  as_tibble()


############## polygons



polygon_floodplain <-
  data.frame(x = c(-2.05,-2.05,-1.05,-1.05),
             y = c(0.6, 0.7, 0.68, 0.55))

polygon_magd <- data.frame(x = c(-0.05,-0.05, 0.65, 0.65),
                           y = c(-1.2,-1,-0.95,-1.12))

forest <-
  filter(nmds_plotting_data,
         own_veg_type == 'Spruce forest &\\nKrummholz')
forest <- forest[chull(forest[, 1:2]), ]
polygon_forest <- data.frame(x = forest$MDS1,
                             y = forest$MDS2)

alpine_grass <-
  filter(nmds_plotting_data,
         own_veg_type == 'Alpine heathland &\\ngrassland')
alpine_grass <- alpine_grass[chull(alpine_grass[, 1:2]),]
polygon_alpine_grass <- data.frame(x = alpine_grass$MDS1,
                                   y = alpine_grass$MDS2)

nardus <-
  filter(nmds_plotting_data, own_veg_type == 'Nardus grassland')
nardus <-  nardus[chull(nardus[, 1:2]),]
polygon_blaser <- data.frame(x = nardus$MDS1,
                             y = nardus$MDS2)

small_sedge <-
  filter(nmds_plotting_data,
         own_veg_type == 'Small sedge community &\\nmountain meadow')
small_sedge <- small_sedge[chull(small_sedge[, 1:2]),]
polygon_laponesalm <- data.frame(x = small_sedge$MDS1,
                                 y = small_sedge$MDS2)
############## end of polygons

text_data <- data.frame(
  x = c(-0.6, -1.5, 1, 0.4,-0.2,-1.1),
  y = c(1.3, 0.85,  1.2,-1.3,-0.74,-1.1),
  label = c(
    'Spruce forest &\nKrummholz\n(Gschnitz)',
    'Floodplain forest\n(Gschnitz)',
    'Alpine heathland &\ngrassland\n(Nösslachjoch,\nTribulaun)',
    'Calcareous grassland\n(Magdalena)',
    'Nardus grassland\n(Blaser)',
    'Small sedge community &\nmountain meadow\n(Laponesalm, Magdalena)'
  )
)

pol_alpha <- 0.1
p1 <-
  nmds_plotting_data %>%
  ggplot() +
  geom_polygon(data = polygon_floodplain,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_forest,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_alpine_grass,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_magd,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_blaser,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_laponesalm,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_point(aes(
    x = MDS1,
    y = MDS2,
    color = temp,
    size = height
  )) +
  scale_color_gradient(name = 'Landolt\ntemperature\nindicator\nvalue',
                       low = 'blue', high = 'orange') +
  scale_size_continuous(name = 'Height [m]',
                        range = c(2, 6)) +
  guides(shape = guide_legend(override.aes = list(size = 3.5))) +
  geom_text(data = text_data, aes(x = x, y = y, label = label)) +
  labs(title = 'Temperature',
       y = 'NMDS2',
       x = 'NMDS1') +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


p2 <-
  nmds_plotting_data %>%
  ggplot() +
  geom_polygon(data = polygon_floodplain,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_forest,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_alpine_grass,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_magd,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_blaser,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_laponesalm,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_point(aes(
    x = MDS1,
    y = MDS2,
    color = light,
    size = height
  )) +
  scale_color_viridis_c(name = 'Landolt\nlight\nindicator\nvalue') +
  scale_size_continuous(name = 'Height [m]',
                        range = c(2, 6)) +
  geom_text(data = text_data, aes(x = x, y = y, label = label)) +
  labs(title = 'Light',
       y = 'NMDS2',
       x = 'NMDS1') +
  guides(colour = guide_colourbar(order = 1),
         size = guide_legend(order = 2)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))


combi_plot <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave(
  'results/nmds.png',
  combi_plot,
  dpi = 600,
  width = 13,
  height = 6
)


# for continentality
nmds_plotting_data %>%
  ggplot() +
  geom_polygon(data = polygon_floodplain,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_forest,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_alpine_grass,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_magd,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_blaser,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_laponesalm,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_point(aes(x = MDS1, y = MDS2,
                 color = cont), size = 4) +
  scale_color_gradient2(
    low = 'gold1',
    mid = 'orange',
    high = 'red4',
    midpoint = 3.1
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3.5))) +
  #geom_text(aes(x=MDS1, y = MDS2 + 0.08, label=Num))+
  geom_text(data = text_data, aes(x = x, y = y, label = label)) +
  labs(title = 'Continentality',
       y = 'NMDS2',
       x = 'NMDS1') +
  theme_classic()



# All indicator values in one ---------------------------------------------
mean_plotting_data <- results %>%
  tidyr::pivot_longer(cols = c(temp, cont, light)) %>%
  mutate(
    name = factor(
      name,
      level = c('temp', 'cont', 'light'),
      labels = c('Temperature', 'Continentality', 'Light')
    ),
    veg_type = factor(
      own_veg_num,
      labels = c(
        'Floodplain forest',
        'Spruce forest &\nKrummholz',
        'Small sedge community &\nmountain meadow',
        'Nardus grassland',
        'Calcareous grassland',
        'Alpine heathland &\ngrassland'
      )
    )
  ) %>%
  group_by(veg_type, name) %>%
  summarise(y = mean(value))

herb_plotting <-
  results_herb %>%
  select(temp, cont, light, own_veg_num) %>%
  tidyr::pivot_longer(cols = c(temp, cont, light)) %>%
  mutate(
    name = factor(
      name,
      level = c('temp', 'cont', 'light'),
      labels = c('Temperature', 'Continentality', 'Light')
    ),
    veg_type = factor(
      own_veg_num,
      labels = c('Floodplain forest',
                 'Spruce forest &\nKrummholz')
    )
  )

tree_plotting <-
  results_tree_shrub %>%
  select(temp, cont, light, own_veg_num) %>%
  tidyr::pivot_longer(cols = c(temp, cont, light)) %>%
  mutate(
    name = factor(
      name,
      level = c('temp', 'cont', 'light'),
      labels = c('Temperature', 'Continentality', 'Light')
    ),
    veg_type = factor(
      own_veg_num,
      labels = c('Floodplain forest',
                 'Spruce forest &\nKrummholz')
    )
  )

plotting_data <-
  results %>%
  select(temp, cont, light, own_veg_num) %>%
  tidyr::pivot_longer(cols = c(temp, cont, light)) %>%
  mutate(
    name = factor(
      name,
      level = c('temp', 'cont', 'light'),
      labels = c('Temperature', 'Continentality', 'Light')
    ),
    veg_type = factor(
      own_veg_num,
      labels = c(
        'Floodplain forest',
        'Spruce forest &\nKrummholz',
        'Small sedge community &\nmountain meadow',
        'Nardus grassland',
        'Calcareous grassland',
        'Alpine heathland &\ngrassland'
      )
    )
  ) %>%
  filter(!own_veg_num %in% 1:2)

ggplot() +
  geom_point(
    data = mean_plotting_data,
    aes(veg_type, y),
    shape = 3,
    size = 3,
    col = 'white'
  ) +
  geom_jitter(
    data = plotting_data,
    aes(veg_type, value),
    width = 0.15,
    size = 1.5,
    col = 'orange'
  ) +
  geom_jitter(data = herb_plotting,
              aes(veg_type, value),
              width = 0.15,
              col = 'darkgreen') +
  geom_jitter(data = tree_plotting,
              aes(veg_type, value),
              width = 0.15,
              col = 'green2') +
  geom_point(
    data = mean_plotting_data,
    aes(veg_type, y),
    shape = 3,
    size = 3,
    col = 'black'
  ) +
  facet_wrap(vars(name)) +
  labs(x = 'Vegetation type', y = 'Landolt indicator value') +
  theme_light() +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 0.95,
    vjust = 0.5
  ),
  text = element_text(size = 14))
ggsave('results/indicator_values.png',
       width = 8,
       height = 6)


# Temperature -------------------------------------------------------------

######  Temperature ~ Height
plot(results$temp, results$height)
cor.test(results$temp, results$height, method = 'spearman')

######  Temperature ~ Soil temperature
plot(results$temp, results$soil_temp)
cor.test(results$temp, results$soil_temp, method = 'spearman')

###### Temperature ~ Exposition
x <- sin(pi * 2 * results$exposition / 360)

y <- cos(pi * 2 * results$exposition / 360)


# quality check
ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_point(aes(x = x, y = y, color = results$loc), size = 3) +
  coord_fixed()

exposition_df <- results %>%
  select(loc, exposition, inclination, temp, soil_temp, light) %>%
  tidyr::drop_na(exposition, inclination) %>%
  #filter(inclination >= 10) %>%
  mutate(exposition_checked = exposition) %>%
  
  # strange Blaser exposition
  mutate(exposition_checked = replace(exposition_checked,
                                      exposition == 17.2,
                                      mean(c(160, 165, 191, 161)))) %>%
  
  # strange Tribulaun exposition
  mutate(exposition_checked = replace(exposition_checked,
                                      exposition == 65,
                                      mean(c(210, 229, 260, 280)))) %>%
  
  # strange Laponesalm exposition
  mutate(exposition_checked = replace(exposition_checked,
                                      exposition == 12.9,
                                      mean(c(150, 135, 130, 100))))


x <- radius * sin(pi * 2 * exposition_df$exposition_checked / 360)
y <- radius * cos(pi * 2 * exposition_df$exposition_checked / 360)

ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_point(aes(x = x, y = y, color = exposition_df$loc), size = 3) +
  coord_fixed()

exposition_df <-
  exposition_df %>%
  mutate(
    dist_to_360 = 360 - exposition_checked,
    dist_to_n = pmin(exposition_checked, dist_to_360)
  )

# Distance to north ~ soil temperature
plot(exposition_df$dist_to_n, exposition_df$soil_temp)
cor.test(exposition_df$dist_to_n, exposition_df$soil_temp, method = 'spearman')

# Distance to north ~ temperature
plot(exposition_df$dist_to_n, exposition_df$temp)
cor.test(exposition_df$dist_to_n, exposition_df$temp, method = 'spearman')


# Light -------------------------------------------------------------------

###### Light ~ type(open/forest)
results %>%
  select(light, type) %>%
  rstatix::t_test(light ~ type)


###### Light ~ height
# everything
plot(results$light, results$height)
cor.test(results$light, results$height, method = 'spearman')

# only open
results %>%
  select(height, type, light) %>%
  filter(type == 'Open') %>%
  rstatix::cor_test(light, height, method = 'spearman')

results %>%
  select(height, type, light, loc) %>%
  filter(type == 'Open') %>%
  ggplot() +
  geom_point(aes(x = height, y = light, col = loc))
# --> increasing variance

# Exposition ~ light
#exposition_df %>%
#  rstatix::cor_test(light, dist_to_n, method='s')


# Continentality ----------------------------------------------------------

###### Continentality ~ height
results %>%
  select(loc, height, cont) %>%
  rstatix::cor_test(cont, height, method = 'spearman')

results %>%
  select(loc, height, cont) %>%
  ggplot() +
  geom_point(aes(x = height, y = cont, col = loc))


# NMDS --------------------------------------------------------------------

###### Temperature
# first axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS1, temp, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS1, temp))

# second axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS2, temp, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS2, temp))

###### Light
# first axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS1, light, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS1, light))

# second axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS2, light, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS2, light))

###### Continentality
# first axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS1, cont, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS1, cont))

# second axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS2, cont, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS2, cont))

###### Height
# first axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS1, height, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS1, height))

# second axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS2, height, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS2, height))

###### Exposition
# first axis
nmds_plotting_data %>%
  left_join(exposition_df) %>%
  select(MDS1, MDS2, dist_to_n) %>%
  rstatix::cor_test(MDS1, dist_to_n, method = 's')

nmds_plotting_data %>%
  left_join(exposition_df) %>%
  select(MDS1, MDS2, dist_to_n) %>%
  ggplot() +
  geom_point(aes(MDS1, dist_to_n))

# second axis
nmds_plotting_data %>%
  left_join(exposition_df) %>%
  select(MDS1, MDS2, dist_to_n) %>%
  rstatix::cor_test(MDS2, dist_to_n, method = 's')

nmds_plotting_data %>%
  left_join(exposition_df) %>%
  select(MDS1, MDS2, dist_to_n) %>%
  ggplot() +
  geom_point(aes(MDS2, dist_to_n))

nmds_plotting_data %>%
  left_join(exposition_df) %>%
  ggplot() +
  geom_polygon(data = polygon_floodplain,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_forest,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_alpine_grass,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_magd,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_blaser,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_laponesalm,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_point(aes(x = MDS1, y = MDS2,
                 color = dist_to_n), size = 4) +
  scale_color_gradient2(
    'Expo',
    low = 'gold1',
    mid = 'orange',
    high = 'red4',
    midpoint = 80
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3.5))) +
  #geom_text(aes(x=MDS1, y = MDS2 + 0.08, label=Num))+
  geom_text(data = text_data, aes(x = x, y = y, label = label)) +
  labs(title = 'Exposition',
       y = 'NMDS2',
       x = 'NMDS1') +
  theme_classic()

###### soil_temp

# first axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS1, soil_temp, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS1, soil_temp))

# second axis
nmds_plotting_data %>%
  rstatix::cor_test(MDS2, soil_temp, method = 'spearman')

nmds_plotting_data %>%
  ggplot() +
  geom_point(aes(MDS2, soil_temp))

nmds_plotting_data %>%
  left_join(exposition_df) %>%
  ggplot() +
  geom_polygon(data = polygon_floodplain,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_forest,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_alpine_grass,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_magd,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_blaser,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_polygon(data = polygon_laponesalm,
               aes(x = x, y = y),
               alpha = pol_alpha,
               fill = 1) +
  geom_point(aes(x = MDS1, y = MDS2,
                 color = soil_temp), size = 4) +
  scale_color_gradient2(
    'Soil temp',
    low = 'gold1',
    mid = 'orange',
    high = 'red4',
    midpoint = 10
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3.5))) +
  #geom_text(aes(x=MDS1, y = MDS2 + 0.08, label=Num))+
  geom_text(data = text_data, aes(x = x, y = y, label = label)) +
  labs(title = 'Soil temperature',
       y = 'NMDS2',
       x = 'NMDS1') +
  theme_classic()
