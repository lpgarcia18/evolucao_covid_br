#Setando ambiente --------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(geobr)
library(gganimate)
library(pals)
#remotes::install_github("connorrothschild/cr")
library(cr)
library(magick)
library(gifski)
library(ggstance)

# Carregando a base -------------------------------------------------------
source("transformacao.R")

# Construindo o mapa ---------------------------------------------------
all_states <- geobr::read_state(
  code_state = "all",
  year = 2018,
  simplified = TRUE,
  showProgress = TRUE
)
estado_ano <- merge(unique(all_states$code_state), c(2010:2019), all = T)
names(estado_ano) <- c("id_uf", "ano")
mapa <- merge(estado_ano, base_ca, by = c("id_uf", "ano"), all = T)
mapa[is.na(mapa)] <- 0

mapa <- left_join(all_states, mapa, by = c("code_state" = "id_uf"))
mapa$ano <- as.integer(mapa$ano)

mapa_obito_gif <- ggplot()+
  geom_sf(data=mapa, aes(fill=tx_padronizada), color = NA)+
  paletteer::scale_fill_paletteer_c(palette = "pals::coolwarm", 
                                    name="Taxa por 100.000 hab", na.value="white")+
  theme_minimal()+
  theme(plot.title = element_text(size=24), plot.subtitle = element_text(size=20))+
  labs(title = "Taxa Padronizada de Óbitos/100.000 hab", subtitle = "Ano: {frame_time}") +
  transition_time(ano)

a_gif <- animate(mapa_obito_gif, 
                 nframes = nrow(mapa),
                 fps = 5, 
                 duration = 14,
                 width = 600, height = 600, 
                 start_pause = 5, end_pause = 5,
                 renderer = gifski_renderer("outputs/animation1.gif"))

# Construindo a linha -----------------------------------------------------
linha_obito_gif <- ggplot(data=mapa, aes(ano, tx_padronizada, group = sigla_uf, color = sigla_uf)) + 
  geom_line(show.legend = FALSE) + 
  geom_segment(aes(xend = 2019, yend = tx_padronizada), linetype = 2, colour = 'grey', show.legend = FALSE) + 
  geom_point(size = 2, show.legend = FALSE) + 
  geom_text(aes(x = 2019.1, label = sigla_uf, color = "#000000"), hjust = 0, show.legend = FALSE) + 
  drop_axis(axis = "y") +
  coord_cartesian(clip = 'off') +
  theme_light() +
  theme(plot.title=element_text(size=25),
        plot.subtitle=element_text(size=18,face = "bold"),
        plot.caption = element_text(size=18,face = "bold"),
        plot.margin = margin(t = 2, r = 2, b = 1, l = 2, unit = "cm"))+  
  transition_reveal(ano)+
  labs(title = "Taxa Padronizada de Óbitos/100.000 hab", subtitle = "Evolução dos Estados",
       y = 'Taxa por 100.000 hab',
       x = element_blank(),
       caption='Fonte: Base dos Dados', size=8)

b_gif <- animate(linha_obito_gif, 
                 nframes = nrow(mapa),
                 fps = 10, 
                 duration = 27,
                 width = 600, height = 600, 
                 start_pause = 15, end_pause = 15,
                 renderer = gifski_renderer("outputs/animation2.gif"))

# Construindo colunas -----------------------------------------------------
mapa_rank <- mapa
mapa_rank$geom <- NULL
mapa_rank$tx_padronizada <- round(mapa_rank$tx_padronizada, 2)

mapa_rank <- mapa_rank %>%
  select(ano, sigla_uf, tx_padronizada) %>%
  group_by(ano) %>%
  mutate(rank = rank(-tx_padronizada),
         tx_rel = tx_padronizada/tx_padronizada[rank == 1],
         tx_lbl = paste0(" ",tx_padronizada,2))%>%
  group_by(sigla_uf)%>%
  ungroup()
mapa_rank$tx_rel <- round(mapa_rank$tx_rel,2)
  
colunas_obito_gif <- ggplot(data=mapa_rank, aes(rank, group = sigla_uf, fill = sigla_uf, color = sigla_uf)) + 
  geom_tile(aes(y = tx_padronizada/2,
            height = tx_padronizada,
            width = 0.9), alpha = 0.8, color = NA)+
  geom_text(aes(y = 0, label = paste(sigla_uf, " ")), vjust = 0.2, hjust = 1, size = 6) +
  geom_text(aes(y = tx_padronizada, label = paste(scales::label_comma(accuracy = .01, decimal.mark = ",", big.mark = ".")(tx_padronizada), " "), 
                hjust=0), size = 6)+
  coord_flip(clip = "off", expand = FALSE)+
  scale_y_continuous()+
  scale_x_reverse() +
  guides(color = "none", fill = "none")+
  theme_light()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25),
        plot.subtitle=element_text(size=18,face = "bold"),
        plot.caption = element_text(size=18,face = "bold"),
        plot.margin = margin(t = 2, r = 3, b = 1, l = 2, unit = "cm"))+
  transition_states(ano, transition_length = 4
                    , state_length = 1)+
  view_follow(fixed_y = TRUE)+
  labs(title = "Taxa Padronizada de Óbitos/100.000 hab", subtitle = "Ano: {closest_state}",
       caption='Fonte: Base dos Dados')

c_gif <- animate(colunas_obito_gif, 
                 nframes = nrow(mapa),
                 fps = 10, 
                 duration = 27,
                 width = 600, height = 600, 
                 start_pause = 5, end_pause = 0,
                 renderer = gifski_renderer("outputs/animation3.gif"))



