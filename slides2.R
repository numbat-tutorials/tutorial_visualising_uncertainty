## -------------------------------------------------------------
#| label: setup
#| include: false
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggdibbler)
library(distributional)
library(cartogram)
library(sf)
library(scales)
library(colorspace)
library(kableExtra)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
options(digits=3, 
        repr.plot.width=15,
        repr.plot.height=8)

# ![](images/){fig-align="center"}


## -------------------------------------------------------------
#| label: show-data
temp_data <- toy_temp |>
  as_tibble() |>
  select(scientistID, county_name, recorded_temp) |>
  head(5)
kbl(temp_data)


## -------------------------------------------------------------
#| echo: false
p_data <- ggplot(toy_temp) +
  geom_jitter(aes(x=county_longitude, y=county_latitude, colour=recorded_temp), 
              width=7000, height =7000, alpha=0.7) +
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude",
       colour= "Temperature",
       title = "Citizen Scientist Recordings") +
  scale_colour_distiller(palette = "YlOrRd", direction= 1) +
  theme(aspect.ratio=0.7)
p_data


## -------------------------------------------------------------
#| label: make-map
#| echo: false
p_data_sf <- ggplot(toy_temp) +
  geom_sf(aes(geometry=county_geometry), fill="white") +
  geom_jitter(aes(x=county_longitude, y=county_latitude, colour=recorded_temp), 
              width=7000, height =7000, alpha=0.7) +
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude",
       colour= "Temperature",
       title = "Citizen Scientist Recordings") +
  scale_colour_distiller(palette = "YlOrRd", direction= 1) +
  theme(aspect.ratio=0.7)
p_data_sf


## -------------------------------------------------------------
p_data_sf


## -------------------------------------------------------------
#| label: compute-summaries
#| include: false
# Calculate County Mean
mean_print <- toy_temp |> 
  group_by(county_name) |>
  summarise(temp_mean = mean(recorded_temp),
            temp_se = sd(recorded_temp)/sqrt(n()),
            n = n()) |>
  as_tibble() |>
  select(county_name, temp_mean, temp_se, n) |>
  head(5)

toy_temp_mean <- toy_temp |> 
  group_by(county_name, county_longitude, county_latitude) |>
  summarise(temp_mean = mean(recorded_temp),
            temp_se = sd(recorded_temp)/sqrt(n()),
            n = n()) |>
  ungroup()


## -------------------------------------------------------------
#| label: compute-mean
#| echo: true
#| eval: false
# # Calculate County Mean
# toy_temp |>
#   group_by(county_name) |>
#   summarise(temp_mean = mean(recorded_temp),
#             temp_se = sd(recorded_temp)/sqrt(n()),
#             n = n())


## -------------------------------------------------------------
kbl(mean_print)


## -------------------------------------------------------------
#| label: choropleth
p_choro <- ggplot(toy_temp_mean) +
  geom_sf(aes(geometry=county_geometry, fill=temp_mean), linewidth = 0, alpha=0.9) +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature") +
  theme(aspect.ratio=0.7)
p_choro


## -------------------------------------------------------------
p_choro


## -------------------------------------------------------------
#| label: cartogram
# Make Cartogram using instructions from r graph gallery
toy_merc <- st_transform(toy_temp_mean, 3857)
toy_cartogram <- cartogram_cont(toy_merc, weight = "n", itermax = 5)
toy_cartogram <- st_transform(toy_cartogram, st_crs(toy_temp_mean))
# plot it
p_cartogram <- ggplot(toy_cartogram) +
  geom_sf(aes(fill = temp_mean), linewidth = 0, alpha = 0.9) +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature") +
  theme(aspect.ratio=0.7)
p_cartogram


## -------------------------------------------------------------
#| label: bubble-plot
p_bubble <- ggplot(toy_temp_mean) +
  geom_sf(fill = "white", alpha = 0.9) +
  geom_point(aes(x=county_longitude, county_latitude,
                 colour=temp_mean, size=n), alpha=0.9) +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(colour = "Temperature") +
  scale_colour_distiller(palette = "YlOrRd", direction= 1) +
  theme(aspect.ratio=0.7, legend.position="none")

p_bubble



## -------------------------------------------------------------
#| label: compute-var
#| include: false

# Calculate extra variance
toy_temp_comp <- toy_temp_mean |> 
  mutate(low_temp_se = temp_se,
         high_temp_se = 3*temp_se) |>
  select(-temp_se)
  
comp_print <- toy_temp_comp |>
  select(county_name, temp_mean, low_temp_se,
         high_temp_se, n) |>
  head(3)


## -------------------------------------------------------------
kbl(comp_print)


## -------------------------------------------------------------
#| fig-width: 10
#| fig-height: 7
#| out-width: 100%
p_choro + ggtitle("Low Standard Error")


## -------------------------------------------------------------
#| fig-width: 10
#| fig-height: 7
#| out-width: 100%
p_choro + ggtitle("High Standard Error")


## -------------------------------------------------------------
#| include: false
# Calculate County Distribution
toy_temp_dist <- toy_temp_comp |>
mutate(temp_dist_low = dist_normal(temp_mean, low_temp_se),
temp_dist_high = dist_normal(temp_mean, high_temp_se)
)



## -------------------------------------------------------------
toy_temp_dist |>
  as_tibble() |>
  select(county_name, temp_dist_low, temp_dist_high, n) |>
  head(5) |>
  kbl()


## -------------------------------------------------------------
#| label: ggdibbler1a
#| echo: true
#| fig-align: center 

toy_temp_dist |> 
  ggplot() + 
  geom_sf_sample(aes(geometry = county_geometry,
                     fill=temp_dist_low))


## -------------------------------------------------------------
#| label: ggdibbler1n
#| echo: true
#| fig-align: center 

toy_temp_dist |> 
  ggplot() + 
  geom_sf_sample(aes(geometry = county_geometry,
                     fill=temp_dist_high))


## -------------------------------------------------------------
#| label: ggdibbler2a
#| fig-width: 10
#| fig-height: 7
#| out-width: 100%
#| fig-align: center 

p1 <- ggplot(toy_temp_dist) +
  geom_sf_sample(aes(geometry=county_geometry, fill=temp_dist_low),  linewidth=0, n=7) +
  geom_sf(aes(geometry = county_geometry), fill=NA, linewidth=0.5, colour="white") +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature",
title = "Low variance case")
p1


## -------------------------------------------------------------
#| label: ggdibbler2b
#| fig-width: 10
#| fig-height: 7
#| out-width: 100%
p2 <- ggplot(toy_temp_dist) +
  geom_sf_sample(aes(geometry=county_geometry, fill=temp_dist_high),  linewidth=0, n=7) +
  geom_sf(aes(geometry = county_geometry), fill=NA, linewidth=0.5, colour="white") +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature",
title = "High variance case")
p2


## -------------------------------------------------------------
#| label: ggdibbler3a
#| fig-width: 10
#| fig-height: 7
#| out-width: 100%
#| fig-align: center 

p1 <- ggplot(toy_temp_dist) +
  geom_sf_sample(aes(geometry=county_geometry, fill=temp_dist_low),  linewidth=0, n=7) +
  geom_sf(aes(geometry = county_geometry), fill=NA, linewidth=0.5, colour="white") +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature",
title = "Low variance case")
p1


## -------------------------------------------------------------
#| label: ggdibbler3b
#| fig-width: 10
#| fig-height: 7
#| out-width: 100%
p2 <- ggplot(toy_temp_dist) +
  geom_sf_sample(aes(geometry=county_geometry, fill=temp_dist_high),  linewidth=0, n=7) +
  geom_sf(aes(geometry = county_geometry), fill=NA, linewidth=0.5, colour="white") +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature",
title = "High variance case")
p2


## -------------------------------------------------------------
#| echo: false
countdown::countdown(10, left="10%", bottom="35%",
  color_background="#027EB6",
  color_text="white",
  color_running_background="#027EB6",
  color_running_text="white",
  color_finished_background="#D93F00")


## -------------------------------------------------------------
#| label: example2
#| echo: true
# Transform to a the crs needed to do the cartogram transformation
toy_merc <- st_transform(toy_temp_mean, 3857)
# cartogram transformation
toy_cartogram <- cartogram_cont(toy_merc, weight = "n", itermax = 5)
# Transform back to original crs 
toy_cartogram <- st_transform(toy_cartogram, st_crs(toy_temp_mean))

# Plot cartogram using ggplot2
ggplot(toy_cartogram) +
  geom_sf(aes(fill = temp_mean), linewidth = 0, alpha = 0.9) +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature") +
  theme(aspect.ratio=0.7)


## -------------------------------------------------------------
#| label: example2-sol
#| echo: true

# only change to data is distribution
toy_cartogram |>
  mutate(temp_dist = dist_normal(temp_mean, temp_se^2)) |>
  ggplot() +
  geom_sf_sample(aes(geometry=county_geometry, 
                     fill=temp_dist), linewidth=0) +
   geom_sf(aes(geometry=county_geometry), fill=NA, colour="white") +
  theme_minimal() +
  scale_fill_distiller(palette = "YlOrRd", direction= 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Temperature") +
  theme(aspect.ratio=0.7)

