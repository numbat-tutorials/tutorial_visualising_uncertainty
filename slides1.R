## ----include = FALSE------------------------------------------
#| label: libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(forcats)
library(colorspace)
library(patchwork)
library(ggbeeswarm)
library(ggforce)
library(ggdist)
library(ggridges)
library(nullabor)
library(ozmaps)
library(sf)
library(rmapshaper)
library(brolgar)
library(lme4)
library(modelr)
library(distributional)
library(lubridate)               
library(ggthemes)
library(fable)
library(tsibble)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)


## ----include = FALSE------------------------------------------
#| label: options-for-nice-slides
options(width = 200)
knitr::opts_chunk$set(
  fig.width = 5,
  fig.height = 5,
  fig.align = "center",
  dev.args = list(bg = 'transparent'),
  out.width = "100%",
  fig.retina = 5,
  dpi = 150, 
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE
)


## ----include = FALSE------------------------------------------
#| label: theme-for-nice-plots
theme_set(ggthemes::theme_gdocs() + #base_size = 14) +
  theme(plot.background = 
        element_rect(fill = 'transparent', colour = NA),
        #axis.line.x = element_line(color = "black", 
        #                           linewidth = 0.4),
        #axis.line.y = element_line(color = "black", 
        #                           linewidth = 0.4),
        panel.grid.major = element_line(color = "grey90"),
        axis.ticks = element_line(color = "black"),
        #plot.title.position = "plot",
        #plot.title = element_text(size = 14),
        panel.background  = 
          element_rect(fill = 'transparent', colour = "black"),
        legend.background = 
          element_rect(fill = 'transparent', colour = NA),
        legend.key        = 
          element_rect(fill = 'transparent', colour = NA)
  ) 
)


## -------------------------------------------------------------
#| label: election1
election <- read_csv(here::here("session1/data/election2019.csv"),
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
e_grn <- election |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)

e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))



## -------------------------------------------------------------
#| label: election1
#| echo: false
election <- read_csv(here::here("session1/data/election2019.csv"),
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
e_grn <- election |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)

e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))



## -------------------------------------------------------------
#| label: election1
#| echo: false
#| out-width: 80%
election <- read_csv(here::here("session1/data/election2019.csv"),
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
e_grn <- election |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)

e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))



## -------------------------------------------------------------
#| label: election2
#| out-width: 80%
e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_boxplot(varwidth = TRUE) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))


## -------------------------------------------------------------
#| label: election3
#| out-width: 80%
e_grn |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(x=perc_GRN, y=State)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
      fill="#006dae", alpha=0.5) +
    labs(
      x = "First preference votes %",
      y = ""
    ) +
  xlim(c(0,50))


## -------------------------------------------------------------
#| label: election4
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")
oz_votes <- rmapshaper::ms_simplify(ozmaps::abs_ced)
oz_votes_grn <- full_join(oz_votes, e_grn, by=c("NAME"="DivisionNm"))

ggplot(oz_votes_grn, aes(fill=perc_GRN)) +
  geom_sf(colour="white") +
  scale_fill_viridis_c(direction=-1, trans = "log", 
    guide = "colourbar", 
    labels = scales::label_number(accuracy = 0.1)) +
  theme_map() +
  theme(legend.position = "right", 
    legend.title = element_blank())


## -------------------------------------------------------------
#| eval: false
#| echo: false
# melbtemp <- read_csv(here::here("session1/data/melb_temp.csv")) |>
#   janitor::clean_names() |>
#   dplyr::rename(temp = `maximum_temperature_degree_c`) |>
#   mutate(month = fct_recode(month,
#                             "Jan"="01",
#                             "Feb"="02",
#                             "Mar"="03",
#                             "Apr"="04",
#                             "May"="05",
#                             "Jun"="06",
#                             "Jul"="07",
#                             "Aug"="08",
#                             "Sep"="09",
#                             "Oct"="10",
#                             "Nov"="11",
#                             "Dec"="12")) |>
#   mutate(day = as.numeric(day)) |>
#   filter(year > 1972, year < 2020) |>
#   dplyr::select(year, month, day, temp)
# save(melbtemp, file="session1/data/melbtemp.rda")


## -------------------------------------------------------------
#| label: distributions
#| fig-width: 18
#| fig-height: 8
#| out-width: 80%
load("data/melbtemp.rda")
melbtemp_2019 <- melbtemp |>
  filter(year == 2019)
  
d1 <- ggplot(melbtemp_2019, aes(x=month, y=temp)) +
  geom_quasirandom() + 
  stat_summary(geom="point", fun="median", 
    colour="red", size=3) +
  xlab("") + ylab("Temp (C)") +
  ggtitle("A. ggbeeswarm::geom_quasirandom")
  
library(ggforce)
d2 <- ggplot(melbtemp_2019, aes(x=month, y=temp)) +
  geom_violin(fill = "#6F7C4D", colour=NA, alpha=0.7) +
  geom_sina() +
  xlab("") + ylab("Temp (C)") +
  ggtitle("B. geom_violin + ggforce::geom_sina")

library(ggridges)
d3 <- ggplot(melbtemp_2019, aes(x=temp, y=month)) +
  geom_density_ridges(scale = 1.5, 
                      quantile_lines = TRUE,
                      quantiles = 2,
                      fill = "#6F7C4D") +
  xlab("Temp (C)") + ylab("") + 
  theme_ridges() +
  ggtitle("C. ggridges::geom_density_ridges")

library(ggdist)
d4 <- ggplot(melbtemp_2019, aes(x=temp, y=month)) +
  stat_halfeye(fill="#6F7C4D", alpha=0.7) +
  geom_point(pch = "|", size = 2,
    position = position_nudge(y = -.15)) +
  xlab("Temp (C)") + ylab("") +
  ggtitle("D. ggdist::stat_halfeye")

lout <- c(area(1,2),
          area(3),
          area(4))
lout <- "
AACD
BBCD
"
d1 + d2 + d3 + d4 + plot_layout(design=lout)


## -------------------------------------------------------------
#| echo: false
countdown::countdown(10, left="50%", bottom="40%",
  color_background="#027EB6",
  color_text="white",
  color_running_background="#027EB6",
  color_running_text="white",
  color_finished_background="#D93F00")


## -------------------------------------------------------------
#| label: model
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
data("wages")
wages_fct <- wages |>
  select(id, ln_wages, xp, high_grade) |>
  mutate(high_grade = factor(high_grade))
wages_fit <- lmer(ln_wages~xp + high_grade + (xp|id), data=wages_fct)
wages_fe <- summary(wages_fit)$coefficients
wages_fe_d <- tibble(xp = rep(seq(0, 13, 1), 7),
     high_grade = rep(c(6, 7, 8, 9, 10, 11, 12), rep(14, 7))) |>
  mutate(ln_wages = case_when(
    high_grade == 6 ~ wages_fe[1,1] + wages_fe[2,1]*xp,
    high_grade == 7 ~ wages_fe[1,1] + wages_fe[3,1] + wages_fe[2,1]*xp,
    high_grade == 8 ~ wages_fe[1,1] + wages_fe[4,1]  + wages_fe[2,1]*xp,
    high_grade == 9 ~ wages_fe[1,1] + wages_fe[5,1]  + wages_fe[2,1]*xp,
    high_grade == 10 ~ wages_fe[1,1] + wages_fe[6,1]  + wages_fe[2,1]*xp,
    high_grade == 11 ~ wages_fe[1,1] + wages_fe[7,1]  + wages_fe[2,1]*xp,
    high_grade == 12 ~ wages_fe[1,1] + wages_fe[8,1]  + wages_fe[2,1]*xp)
  ) |>
  mutate(high_grade = factor(high_grade))
ggplot(wages_fe_d) + 
  geom_line(aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade") 


## -------------------------------------------------------------
#| label: modelanddata
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
ggplot() + 
  geom_line(data=wages_fct, aes(x=xp, y=ln_wages, group=id), alpha=0.1) +
  geom_line(data=wages_fe_d, aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade") 


## -------------------------------------------------------------
#| label: SE
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
wages_fe_d <- wages_fe_d |>
  mutate(ln_wages_l = case_when(
    high_grade == 6 ~ wages_fe[1,1] - wages_fe[1,2] +
                      (wages_fe[2,1]-wages_fe[2,2])*xp ,
    high_grade == 7 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[3,1] - wages_fe[3,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 8 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[4,1] - wages_fe[4,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 9 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[5,1] - wages_fe[5,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 10 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[6,1] - wages_fe[6,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 11 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[7,1] - wages_fe[7,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 12 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[8,1] - wages_fe[8,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp)
  ) |>
  mutate(ln_wages_u = case_when(
    high_grade == 6 ~ wages_fe[1,1] + wages_fe[1,2] +
                      (wages_fe[2,1]+wages_fe[2,2])*xp ,
    high_grade == 7 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[3,1] + wages_fe[3,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 8 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[4,1] + wages_fe[4,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 9 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[5,1] + wages_fe[5,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 10 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[6,1] + wages_fe[6,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 11 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[7,1] + wages_fe[7,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 12 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[8,1] + wages_fe[8,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp)
  ) 

ggplot() + 
  geom_ribbon(data=wages_fe_d, 
            aes(x=xp, 
                ymin=ln_wages_l,
                ymax=ln_wages_u,
                fill=high_grade), colour=NA, alpha=0.1) +
  geom_line(data=wages_fe_d, 
            aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_fill_discrete_divergingx(palette = "Zissou 1") +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade", fill="Grade") 



## -------------------------------------------------------------
#| label: indiv-fits
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
wages_full <- wages_fct |>
  add_predictions(wages_fit, 
                  var = "pred") |>
  add_residuals(wages_fit, 
                var = "res")
set.seed(1222)
wages_full |> add_n_obs() |> filter(n_obs > 4) |>
  sample_n_keys(size = 12) |>
  ggplot() + 
  geom_line(aes(x = xp, y = pred, group = id, 
             colour = factor(id))) + 
  geom_point(aes(x = xp, y = ln_wages, 
                 colour = factor(id))) + 
  facet_wrap(~id, ncol=4)  +
  scale_x_continuous("Experience (years)", 
    breaks=seq(0, 12, 2)) +
  ylab("Wages (ln)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| echo: false
V1 = tibble(x = 1:7, 
            native = factor(c("quoll", "emu", "roo", 
            "bilby", "quokka", "dingo", "numbat")))
c1 <- ggplot(V1, aes(x=x, y=1, fill=native)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=native)) +
  ggtitle("qualitative") + 
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))
V2 = tibble(x = 1:7, 
            fill = 1:7)
c2 <- ggplot(V2, aes(x=x, y=1, fill=fill)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=fill)) +
  ggtitle("sequential: emphasise high") + 
  scale_fill_continuous_sequential(palette = "PinkYl") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))

V3 = tibble(x = 1:7, 
            fill = -3:3)
c3 <- ggplot(V3, aes(x=x, y=1, fill=fill)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=fill)) +
  ggtitle("diverging: emphasise high and low") + 
  scale_fill_continuous_divergingx(palette = "ArmyRose") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 2
#| out-width: 100%
c1 + c2 + c3 + plot_layout(ncol=1)


## -------------------------------------------------------------
#| label: SE
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
#| echo: false
wages_fe_d <- wages_fe_d |>
  mutate(ln_wages_l = case_when(
    high_grade == 6 ~ wages_fe[1,1] - wages_fe[1,2] +
                      (wages_fe[2,1]-wages_fe[2,2])*xp ,
    high_grade == 7 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[3,1] - wages_fe[3,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 8 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[4,1] - wages_fe[4,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 9 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[5,1] - wages_fe[5,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 10 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[6,1] - wages_fe[6,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 11 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[7,1] - wages_fe[7,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp,
    high_grade == 12 ~ wages_fe[1,1] - wages_fe[1,2] + 
                      wages_fe[8,1] - wages_fe[8,2] + 
                      (wages_fe[2,1]-wages_fe[2,2])*xp)
  ) |>
  mutate(ln_wages_u = case_when(
    high_grade == 6 ~ wages_fe[1,1] + wages_fe[1,2] +
                      (wages_fe[2,1]+wages_fe[2,2])*xp ,
    high_grade == 7 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[3,1] + wages_fe[3,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 8 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[4,1] + wages_fe[4,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 9 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[5,1] + wages_fe[5,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 10 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[6,1] + wages_fe[6,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 11 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[7,1] + wages_fe[7,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp,
    high_grade == 12 ~ wages_fe[1,1] + wages_fe[1,2] + 
                      wages_fe[8,1] + wages_fe[8,2] + 
                      (wages_fe[2,1]+wages_fe[2,2])*xp)
  ) 

ggplot() + 
  geom_ribbon(data=wages_fe_d, 
            aes(x=xp, 
                ymin=ln_wages_l,
                ymax=ln_wages_u,
                fill=high_grade), colour=NA, alpha=0.1) +
  geom_line(data=wages_fe_d, 
            aes(x=xp, 
                y=ln_wages, 
                colour=high_grade, 
                group=high_grade)) +
  scale_fill_discrete_divergingx(palette = "Zissou 1") +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  labs(x="Experience (years)", y="Wages (ln)", colour="Grade", fill="Grade") 



## -------------------------------------------------------------
#| label: indiv-fits
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
#| echo: false
wages_full <- wages_fct |>
  add_predictions(wages_fit, 
                  var = "pred") |>
  add_residuals(wages_fit, 
                var = "res")
set.seed(1222)
wages_full |> add_n_obs() |> filter(n_obs > 4) |>
  sample_n_keys(size = 12) |>
  ggplot() + 
  geom_line(aes(x = xp, y = pred, group = id, 
             colour = factor(id))) + 
  geom_point(aes(x = xp, y = ln_wages, 
                 colour = factor(id))) + 
  facet_wrap(~id, ncol=4)  +
  scale_x_continuous("Experience (years)", 
    breaks=seq(0, 12, 2)) +
  ylab("Wages (ln)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## -------------------------------------------------------------
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
ggplot(melbtemp_2019, aes(x=month, y=temp)) +
  geom_quasirandom() + 
  stat_summary(geom="point", fun="median", 
    colour="red", size=5) +
  xlab("") + ylab("Temp (C)") +
  ggtitle("A. ggbeeswarm::geom_quasirandom")


## -------------------------------------------------------------
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
ggplot(melbtemp_2019, aes(x=month, y=temp)) +
  geom_violin(fill = "grey80", colour=NA, alpha=0.7) +
  geom_sina() +
  stat_summary(geom="point", fun="median", 
    colour="red", size=5) +
  xlab("") + ylab("Temp (C)") +
  ggtitle("B. geom_violin + ggforce::geom_sina")


## -------------------------------------------------------------
#| echo: false
countdown::countdown(10, left="25%", bottom="40%",
  color_background="#027EB6",
  color_text="white",
  color_running_background="#027EB6",
  color_running_text="white",
  color_finished_background="#D93F00")


## -------------------------------------------------------------
#| eval: false
#| echo: false
# ped <- rwalkr::melb_walk(from = as.Date("2025-08-01"),
#                          to = as.Date("2025-08-31"))
# ped <- ped |>
#   mutate(wday = wday(Date, label=TRUE,
#       abbr=TRUE, week_start=1)) |>
#   mutate(Count = as.numeric(Count))
# save(ped, file="data/ped_Aug2025.rda")


## -------------------------------------------------------------
#| label: pedestrians
#| echo: false
load("data/ped_Aug2025.rda")
ped_sc <- ped |>
  filter(Sensor == "Southern Cross Station") |>
  filter(Date == ymd("2025-08-31")) |>
  group_by(Time) |>
  summarise(Count = sum(Count), .groups = "drop") |>
  mutate(se = sqrt(Count))
b1 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_col(fill = "#20794D") +
  xlab("Hour")
b2 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_col(fill = "#b9ca4a") +
  geom_errorbar(aes(ymin = Count - se, ymax = Count + se),
    width=0.5, colour="#20794D") +
  xlab("Hour")
b3 <- ggplot(ped_sc, aes(x=Time,
    ydist=distributional::dist_normal(Count, se))) +
  stat_pointinterval(colour = "#20794D") +
  xlab("Hour") + ylab("Count")
b4 <- ggplot(ped_sc, aes(x=Time,
    ydist=distributional::dist_normal(Count, se))) +
  stat_gradientinterval(colour = NA, fill="#20794D", 
    .width=1) +
  geom_line(aes(x=Time, y=Count), colour="#20794D") +
  xlab("Hour") + ylab("Count")
b5 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_ribbon(aes(ymin = Count - qnorm(0.975)*se, 
                  ymax = Count + qnorm(0.975)*se),
    fill = "#b9ca4a") +
  geom_line(colour="#20794D") +
  xlab("Hour")
ped_sc_ci <- ped_sc |>
  mutate(l50 = Count - qnorm(0.75)*se,
         u50 = Count + qnorm(0.75)*se,
         l80 = Count - qnorm(0.9)*se,
         u80 = Count + qnorm(0.9)*se,
         l99 = Count - qnorm(0.995)*se,
         u99 = Count + qnorm(0.995)*se
  ) |>
  pivot_longer(cols=l50:u99, names_to = "intprob", 
    values_to="value") |>
  mutate(bound = str_sub(intprob, 1, 1),
       prob = str_sub(intprob, 2, 3)) |>
  select(Time, Count, se, prob, bound, value) |>
  pivot_wider(names_from = bound, values_from = value)
b6 <- ggplot(ped_sc_ci, aes(x=Time, y=Count)) +
  geom_lineribbon(aes(ymin = l, ymax = u, fill = prob)) +
  labs(x="Hour", fill="Confidence") +
  scale_fill_discrete_sequential(palette = "Greens", 
    rev=FALSE, n=5)  
b7 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
  geom_smooth(colour = "#20794D", fill = "#b9ca4a") +
  geom_point(colour = "#20794D") +
  xlab("Hour")


## -------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b1


## -------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b2


## -------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b3


## -------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b4


## -------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b5


## -------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 5
#| out-width: 70%
b6


## -------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
b7


## -------------------------------------------------------------
#| label: pedestrians
#| echo: true
#| eval: false
# load("data/ped_Aug2025.rda")
# ped_sc <- ped |>
#   filter(Sensor == "Southern Cross Station") |>
#   filter(Date == ymd("2025-08-31")) |>
#   group_by(Time) |>
#   summarise(Count = sum(Count), .groups = "drop") |>
#   mutate(se = sqrt(Count))
# b1 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_col(fill = "#20794D") +
#   xlab("Hour")
# b2 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_col(fill = "#b9ca4a") +
#   geom_errorbar(aes(ymin = Count - se, ymax = Count + se),
#     width=0.5, colour="#20794D") +
#   xlab("Hour")
# b3 <- ggplot(ped_sc, aes(x=Time,
#     ydist=distributional::dist_normal(Count, se))) +
#   stat_pointinterval(colour = "#20794D") +
#   xlab("Hour") + ylab("Count")
# b4 <- ggplot(ped_sc, aes(x=Time,
#     ydist=distributional::dist_normal(Count, se))) +
#   stat_gradientinterval(colour = NA, fill="#20794D",
#     .width=1) +
#   geom_line(aes(x=Time, y=Count), colour="#20794D") +
#   xlab("Hour") + ylab("Count")
# b5 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_ribbon(aes(ymin = Count - qnorm(0.975)*se,
#                   ymax = Count + qnorm(0.975)*se),
#     fill = "#b9ca4a") +
#   geom_line(colour="#20794D") +
#   xlab("Hour")
# ped_sc_ci <- ped_sc |>
#   mutate(l50 = Count - qnorm(0.75)*se,
#          u50 = Count + qnorm(0.75)*se,
#          l80 = Count - qnorm(0.9)*se,
#          u80 = Count + qnorm(0.9)*se,
#          l99 = Count - qnorm(0.995)*se,
#          u99 = Count + qnorm(0.995)*se
#   ) |>
#   pivot_longer(cols=l50:u99, names_to = "intprob",
#     values_to="value") |>
#   mutate(bound = str_sub(intprob, 1, 1),
#        prob = str_sub(intprob, 2, 3)) |>
#   select(Time, Count, se, prob, bound, value) |>
#   pivot_wider(names_from = bound, values_from = value)
# b6 <- ggplot(ped_sc_ci, aes(x=Time, y=Count)) +
#   geom_lineribbon(aes(ymin = l, ymax = u, fill = prob)) +
#   labs(x="Hour", fill="Confidence") +
#   scale_fill_discrete_sequential(palette = "Greens",
#     rev=FALSE, n=5)
# b7 <- ggplot(ped_sc, aes(x=Time, y=Count)) +
#   geom_smooth(colour = "#20794D", fill = "#b9ca4a") +
#   geom_point(colour = "#20794D") +
#   xlab("Hour")


## -------------------------------------------------------------
#| label: forecast1
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
tourism_melb <- tourism %>%
  filter(Region == "Melbourne", Purpose == "Business")
fit <- tourism_melb %>%
  model(
    ets = ETS(Trips ~ trend("A"))
  )
fc <- fit |>
  forecast(h = "5 years")
fc |>
  autoplot(tourism_melb) +
    theme(aspect.ratio = 0.6)


## -------------------------------------------------------------
#| label: forecast2
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
fc_b <- fit |>
  forecast(h = "5 years", bootstrap = TRUE)
fc_b_samples <- fit |>
  generate(h = 20, times = 50, bootstrap = TRUE)
fc_b_samples |>
  ggplot() +
    geom_line(aes(x = Quarter, y = .sim, group = .rep),
      colour = "#027EB6", alpha=0.1) +
    geom_line(data=fc_b, aes(x = Quarter, y = .mean), 
      colour = "#027EB6") +
    autolayer(tourism_melb, Trips) +
    ylab("Trips") +
    theme(aspect.ratio = 0.6)


## -------------------------------------------------------------
#| label: simulate-data
#| echo: false
nlev <- 8
x <- 1:nlev
y_large <- 2 + (x-nlev/2)^2
y_large <- max(y_large) - y_large
y_small <- y_large
#y_small[5] <- y_small[5] + 5
#y_small[6] <- y_small[6] - 5
#y_small[7] <- y_small[7] - 6
struct_d <- tibble(x, y=y_small)


## -------------------------------------------------------------
#| label: lineup
#| echo: false
set.seed(130)
noise_param <- 1
noise_scale <- 30
m <- 12 # multiple of 3
noise1 <- tibble(x=rep(x, m), 
                 y=rpois(nlev*m, noise_param)+1, 
                 .sample=rep(1:m, rep(nlev, m))
)
set.seed(258)
pos <- sample(1:m, 1)
noise1_lup <- noise1 |>
  mutate(yn = y*noise_scale) |>
  mutate(y = if_else(.sample == pos, struct_d$y+y, y)) |>
  group_by(.sample) |>
  mutate(y = round((y-min(y))/(max(y)-min(y))*90+10, 0))
l1 <- ggplot(noise1_lup, aes(x=x, y=y,
    ymin = y-yn, ymax = y+yn)) + 
  geom_ribbon(colour=NA, fill="#b9ca4a") +
  geom_line(colour="#20794D") +
  facet_wrap(~.sample, ncol=m/3, scales="free_y") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 0.6)
l2 <- ggplot(noise1_lup, aes(x=x, y=y,
         ymin = y-yn, ymax = y+yn)) +
  geom_pointrange(colour = "#20794D") +
  geom_point(colour="#b9ca4a", size=3) +
  facet_wrap(~.sample, ncol=m/3, scales="free_y") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 0.6)
noise_scale <- 10
set.seed(432)
pos <- sample(1:m, 1)
noise1_lup <- noise1 |>
  mutate(yn = y*noise_scale) |>
  mutate(y = if_else(.sample == pos, struct_d$y+y, y)) |>
  group_by(.sample) |>
  mutate(y = round((y-min(y))/(max(y)-min(y))*90+10, 0))
l3 <- ggplot(noise1_lup, aes(x=x, y=y,
    ymin = y-yn, ymax = y+yn)) + 
  geom_ribbon(colour=NA, fill="#b9ca4a") +
  geom_line(colour="#20794D") +
  facet_wrap(~.sample, ncol=m/3, scales="free_y") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 0.6)
l4 <- ggplot(noise1_lup, aes(x=x, y=y,
         ymin = y-yn, ymax = y+yn)) +
  geom_pointrange(colour = "#20794D") +
  geom_point(colour="#b9ca4a", size=3) +
  facet_wrap(~.sample, ncol=m/3, scales="free_y") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 0.6)


## -------------------------------------------------------------
#| echo: false
countdown::countdown(1, left="0%", top="20%",
  color_background="#027EB6",
  color_text="white",
  color_running_background="#027EB6",
  color_running_text="white",
  color_finished_background="#D93F00")


## -------------------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 6
#| out-width: 80%
l1


## -------------------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 6
#| out-width: 80%
l2


## -------------------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 6
#| out-width: 80%
l4


## -------------------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 6
#| out-width: 80%
l3


## -------------------------------------------------------------
#| label: simulate-data
#| eval: false
# nlev <- 8
# x <- 1:nlev
# y_large <- 2 + (x-nlev/2)^2
# y_large <- max(y_large) - y_large
# y_small <- y_large
# #y_small[5] <- y_small[5] + 5
# #y_small[6] <- y_small[6] - 5
# #y_small[7] <- y_small[7] - 6
# struct_d <- tibble(x, y=y_small)


## -------------------------------------------------------------
#| label: lineup
#| eval: false
#| code-fold: false
# set.seed(130)
# noise_param <- 1
# noise_scale <- 30
# m <- 12 # multiple of 3
# noise1 <- tibble(x=rep(x, m),
#                  y=rpois(nlev*m, noise_param)+1,
#                  .sample=rep(1:m, rep(nlev, m))
# )
# set.seed(258)
# pos <- sample(1:m, 1)
# noise1_lup <- noise1 |>
#   mutate(yn = y*noise_scale) |>
#   mutate(y = if_else(.sample == pos, struct_d$y+y, y)) |>
#   group_by(.sample) |>
#   mutate(y = round((y-min(y))/(max(y)-min(y))*90+10, 0))
# l1 <- ggplot(noise1_lup, aes(x=x, y=y,
#     ymin = y-yn, ymax = y+yn)) +
#   geom_ribbon(colour=NA, fill="#b9ca4a") +
#   geom_line(colour="#20794D") +
#   facet_wrap(~.sample, ncol=m/3, scales="free_y") +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         aspect.ratio = 0.6)
# l2 <- ggplot(noise1_lup, aes(x=x, y=y,
#          ymin = y-yn, ymax = y+yn)) +
#   geom_pointrange(colour = "#20794D") +
#   geom_point(colour="#b9ca4a", size=3) +
#   facet_wrap(~.sample, ncol=m/3, scales="free_y") +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         aspect.ratio = 0.6)
# noise_scale <- 10
# set.seed(432)
# pos <- sample(1:m, 1)
# noise1_lup <- noise1 |>
#   mutate(yn = y*noise_scale) |>
#   mutate(y = if_else(.sample == pos, struct_d$y+y, y)) |>
#   group_by(.sample) |>
#   mutate(y = round((y-min(y))/(max(y)-min(y))*90+10, 0))
# l3 <- ggplot(noise1_lup, aes(x=x, y=y,
#     ymin = y-yn, ymax = y+yn)) +
#   geom_ribbon(colour=NA, fill="#b9ca4a") +
#   geom_line(colour="#20794D") +
#   facet_wrap(~.sample, ncol=m/3, scales="free_y") +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         aspect.ratio = 0.6)
# l4 <- ggplot(noise1_lup, aes(x=x, y=y,
#          ymin = y-yn, ymax = y+yn)) +
#   geom_pointrange(colour = "#20794D") +
#   geom_point(colour="#b9ca4a", size=3) +
#   facet_wrap(~.sample, ncol=m/3, scales="free_y") +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         aspect.ratio = 0.6)

