library(tidyverse)

# Fonte plot
sysfonts::font_add_google("Roboto", "Roboto", bold.wt = 900, regular.wt = 300)
sysfonts::font_add_google("Roboto", "Roboto Bold", regular.wt = 900)
sysfonts::font_add_google("Roboto", "Roboto Bold Medio", regular.wt = 400)
showtext::showtext_auto()

# Define questões estéticas
tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", family = "Roboto", size = 22, hjust = 0.5),
    plot.subtitle = element_text(family = "Roboto", size = 17, hjust = 0.5, 
                                 lineheight = 0.75),
    legend.position = "top",
    strip.text = element_text(face = "bold", family = "Roboto", size = 16,
                              vjust = 0, lineheight = 0.65),
    axis.text.y = element_text(family = "Roboto", size = 13, color = "black"),
    axis.text.x = element_text(family = "Roboto", size = 13, color = "black",
                               angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_blank(),
    plot.caption = element_text(family = "Roboto", size = 13),
    legend.text = element_text(family = "Roboto", size = 14),
    panel.grid.minor = element_blank()
  )

# Importa
load("dados/swiid9_92/swiid9_92.rda")

rm(swiid)

# bd_swiid <- swiid_summary %>% 
#   mutate(
#     across(
#       .cols = c("gini_mkt", "gini_disp"),
#       .fns = ~ . / 100
#     )
#   )

bd_gini_ipeadata <- readxl::read_xls(here::here("dados/gini_ipeadata.xls")) %>% 
  rename(ano = 1, gini = 2) %>% 
  mutate(gini = gini / 100) %>% 
  mutate(ano = as.Date(paste0(ano, "-01-01"))) %>% 
  add_row(ano = as.Date("2025-01-01"),
          gini = 0.511)

# Plots
# Gráfico 1 - Gráfico de barras no tempo Brasil
bd_seg <- bd_gini_ipeadata %>%
  arrange(ano) %>%
  mutate(
    ano_next  = lead(ano),
    gini_next = lead(gini),
    gap_ano   = as.integer(format(ano_next, "%Y")) - as.integer(format(ano, "%Y")),
    tipo      = if_else(gap_ano > 1, "pontilhada", "continua")
  ) %>%
  filter(!is.na(ano_next))



ggplot(bd_gini_ipeadata, aes(x = ano, y = gini)) +
  geom_segment(
    data = filter(bd_seg, tipo == "continua"),
    aes(x = ano, y = gini, xend = ano_next, yend = gini_next),
    color = "darkblue",
    linewidth = 1.2
  ) +
  geom_segment(
    data = filter(bd_seg, tipo == "pontilhada"),
    aes(x = ano, y = gini, xend = ano_next, yend = gini_next),
    color = "darkblue",
    linewidth = 1.2,
    linetype = "22"
  ) +
  geom_point(color = "darkblue", size = 2.5) +
  geom_text(
    aes(label = round(gini, 3)),
    vjust = -1,
    family = "Roboto Bold Medio",
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 0.7)
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    limits = c(
      min(bd_gini_ipeadata$ano),
      max(bd_gini_ipeadata$ano)
    ), expand = c(0.02,0)
  ) +
  labs(
    title    = "Evolução da desigualdade brasileira",
    subtitle = "Índice de Gini da renda domiciliar per capita de todas as fontes",
    x        = NULL,
    y        = "Índice de Gini",
    caption  = "\nFonte: IPEADATA\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema +
  theme(
    axis.title.y = element_text(family = "Roboto", size = 12, color = "black"),
    axis.text.y = element_text(family = "Roboto", size = 11, color = "black")
  )

# Gráfico 2 - Série histórica Brasil absoluta e relativa

# reorganiza o banco para formato longo
banco_plot <- swiid_summary  %>%
  filter(country=="Brazil")%>%
  filter(year >= 1995) %>% 
  select(year, rel_red, rel_red_se, abs_red, abs_red_se) %>% 
  mutate(
    abs_inf = abs_red - 1.96 * abs_red_se,
    abs_sup = abs_red + 1.96 * abs_red_se,
    rel_inf = rel_red - 1.96 * rel_red_se,
    rel_sup = rel_red + 1.96 * rel_red_se
  ) %>% 
  select(year,
         abs_red, abs_inf, abs_sup,
         rel_red, rel_inf, rel_sup) %>% 
  pivot_longer(
    cols = -year,
    names_to = c("tipo", ".value"),
    names_pattern = "(abs|rel)_(.*)"
  ) %>% 
  mutate(
    tipo = recode(tipo,
                  "abs" = "Variação absoluta",
                  "rel" = "Variação relativa (%)")
  )

ggplot(banco_plot, aes(x = year, y = red)) +
  
  # intervalo de confiança
  geom_ribbon(
    aes(ymin = inf,
        ymax = sup),
    alpha = .25,
    fill = "darkblue"
  ) +
  
  # linha principal
  geom_line(
    color = "darkblue",
    linewidth = 1.2
  ) +
  
  # pontos
  geom_point(
    color = "darkblue",
    size = 2.5
  ) +
  
  
  facet_wrap(~tipo, scales = "free_y") +
  
  ggh4x::facetted_pos_scales(
    y = list(
      tipo == "Variação absoluta" ~ scale_y_continuous(
        labels = function(x) {round(x / 100, 3)},
        limits = c(0, 18),
        breaks = seq(0, 18, 3)
      ),
      
      tipo == "Variação relativa (%)" ~ scale_y_continuous(
        labels = function(x) {paste0(x, "%")}, 
        limits = c(0, 27),
        breaks = seq(0, 100, 5)
      )
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(min(banco_plot$year),
                 max(banco_plot$year), 1)
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    title    = "Evolução do impacto dos impostos e transferências no Gini brasileiro",
    subtitle = "Variação comparativa do Gini pre-tax e post-tax\n",
    caption  = "\nFonte: Standardized World Income Inequality Database\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema +
  theme(panel.spacing.x = unit(2, "cm"))


# Gráfico 3 - Série histórica Brasil x outros países - Relativa
banco_plot <- swiid_summary  %>%
  filter(redist_after <= 1995)%>%
  filter(year >= 1995) %>% 
  filter(year <= 2024) %>% 
  select(country, year, rel_red) %>% 
  mutate(cor = if_else(country == "Brazil", "Brasil", "Outros"))
  
ggplot(banco_plot) +
  aes(x = year, y = rel_red, color = cor, group = country, 
      linewidth = cor, alpha = cor) +
  # geom_point() +
  geom_line() +
  scale_color_manual(
    values = c("Brasil" = "darkblue",
               "Outros" = "grey70")
  ) +
  scale_linewidth_manual(
    values = c("Brasil" = 1.5,
               "Outros" = 1)
  ) +
  scale_alpha_manual(
    values = c("Brasil" = 1,
               "Outros" = 0.7)
  ) +
  labs(
    title    = "Evolução da variação relativa anual no Gini",
    subtitle = "Todos os países com dados de variação desde 1995",
    x        = NULL,
    y        = "Variação relativa\n",
    caption  = "\nFonte: Standardized World Income Inequality Database\nFeito por: Artur Vidaurre de Almeida"
  ) +
  scale_y_continuous(
    labels = function(x) {paste0(x, "%")},
    breaks = seq(-120, 100, 5)
  ) +
  scale_x_continuous(
    breaks = seq(min(banco_plot$year),
                 max(banco_plot$year), 1)
  ) +
  tema +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(family = "Roboto", size = 13, color = "black")
  )


# Gráfico 4 - Scatterplot Brasil x outros países
swiid_summary %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  filter(year >= 2023) %>% 
  mutate(cor = if_else(country == "Brazil", "Brasil", "Outros")) %>% 
  ggplot() +
  aes(x = gini_mkt, y = gini_disp, color = cor, size = cor) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_text(data = swiid_summary %>% 
              group_by(country) %>% 
              filter(year == max(year)) %>% 
              ungroup() %>% 
              filter(year >= 2023) %>% 
              mutate(cor = if_else(country == "Brazil", "Brasil", "Outros")) %>% 
              filter(country == "Brazil") %>% 
              mutate(label = "Brasil"), 
            aes(label = label), family = "Roboto Bold Medio", vjust = -1, size = 6,
            show.legend = FALSE) +
  scale_color_manual(
    values = c("Brasil" = "darkblue",
               "Outros" = "grey70")
  ) +
  scale_size_manual(
    values = c("Brasil" = 7,
               "Outros" = 2)
  ) +
  scale_x_continuous(
    labels = function(x) {round(x / 100, digits = 3)},
    breaks = seq(0, 100, 2)
  ) +
  scale_y_continuous(
    labels = function(x) {round(x / 100, digits = 3)},
    breaks = seq(0, 100, 2)
  ) +
  labs(
    title    = "Distribuição dos países na comparação entre Índices de Gini",
    subtitle = "Todos os países com dados mais recentes disponíveis desde 2023\n",
    x        = "\nGini - Market (pre-tax)",
    y        = "Gini - Disposable (post-tax)\n",
    caption  = "\nFonte: Standardized World Income Inequality Database\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema +
  theme(
    legend.position = "none",
    axis.title = element_text(family = "Roboto", size = 13, color = "black"),
    axis.text.x = element_text(family = "Roboto", size = 13, color = "black", hjust = 0.5, angle = 0)
  )

# Gráfico 5 - Lollipop pretax postax Brasil
bd_arrow <- swiid_summary %>%
  filter(country=="Brazil")%>%
  filter(year >= 1995) %>% 
  select(year, gini_disp, gini_mkt) %>% 
  mutate(year = factor(year,
                       levels = seq(1995, 2024, 1)))

banco_plot <- swiid_summary  %>%
  filter(country=="Brazil")%>%
  filter(year >= 1995) %>% 
  select(year, gini_disp, gini_mkt) %>% 
  pivot_longer(cols = contains("gini"))  %>% 
  mutate(year = factor(year,
                       levels = seq(1995, 2024, 1)))


ggplot() +
  # linha principal
  geom_segment(
    data = bd_arrow,
    aes(x = gini_mkt, xend = gini_disp, y = year, yend = year),
    linewidth = 1,
    color = "black"
  ) +
  
  # seta pequena no meio
  geom_segment(
    data = bd_arrow,
    aes(
      x = gini_mkt + (gini_disp - gini_mkt)*0.45,
      xend = gini_mkt + (gini_disp - gini_mkt)*0.55,
      y = year,
      yend = year
    ),
    arrow = arrow(type = "closed", length = unit(0.18, "cm")),
    linewidth = 1,
    color = "black"
  ) +
  geom_point(
    data = banco_plot,
    aes(x = value, y = year, color = name),
    size = 2.8
  ) +
  geom_text(
    data = banco_plot %>% filter(name == "gini_mkt"),
    aes(x = value, y = year, label = round(value / 100, 3), color = name),
    hjust = -0.25,
    size = 3.5,
    show.legend = FALSE,
    family = "Roboto Bold"
  ) +
  geom_text(
    data = banco_plot %>% filter(name == "gini_disp"),
    aes(x = value, y = year, label = round(value / 100, 3), color = name),
    hjust = 1.25,
    size = 3.5,
    show.legend = FALSE,
    family = "Roboto Bold"
  ) +
  scale_x_continuous(
    labels = function(x) {round(x / 100, digits = 3)},
    breaks = seq(0, 100, 2)
  ) +
  scale_y_discrete(limits = rev(levels(banco_plot$year))) +
  scale_color_manual(
    breaks = c("gini_disp", "gini_mkt"),
    values = c("gini_mkt" = "darkblue", "gini_disp" = "firebrick3"),
    labels = c("Gini - Disposable (post-tax)   ", "Gini - Market (pre-tax)")
  ) +
  labs(
    title    = "Diferença no Gini pré e pós impostos e transferências",
    # subtitle = "BRasil aumentou a capacidade de reduzir as desigualdades com",
    x        = "\nÍndice de Gini",
    y        = NULL,
    caption  = "\nFonte: Standardized World Income Inequality Database\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(family = "Roboto", size = 13, color = "black", hjust = 0.5, angle = 0)
  )


  

# Gráfico 6 - Lollipop pretax postax Brasil x paises
bd_arrow <- swiid_summary %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  filter(year >= 2023) %>% 
  select(country, gini_disp, gini_mkt) 

ordem <- bd_arrow %>% 
  arrange(gini_mkt) %>% 
  pull(country)

banco_plot <- swiid_summary %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  filter(year >= 2023) %>% 
  select(country, gini_disp, gini_mkt) %>% 
  pivot_longer(cols = contains("gini")) %>% 
  mutate(country = factor(country, levels = ordem))

bd_arrow <- bd_arrow  %>% 
  mutate(country = factor(country, levels = ordem)) %>% 
  mutate(posicao = case_when(
    gini_mkt > gini_disp ~ "normal",
    gini_mkt < gini_disp ~ "invertido"
  ))


ggplot() +
  # linha principal
  geom_segment(
    data = bd_arrow,
    aes(x = gini_mkt, xend = gini_disp, y = country, yend = country),
    linewidth = 1,
    color = "black"
  ) +
  
  # seta pequena no meio
  geom_segment(
    data = bd_arrow,
    aes(
      x = gini_mkt + (gini_disp - gini_mkt)*0.45,
      xend = gini_mkt + (gini_disp - gini_mkt)*0.55,
      y = country,
      yend = country
    ),
    arrow = arrow(type = "closed", length = unit(0.18, "cm")),
    linewidth = 1,
    color = "black"
  ) +
  geom_point(
    data = banco_plot,
    aes(x = value, y = country, color = name),
    size = 2.5
  ) +
  geom_text(
    data = bd_arrow %>% 
      filter(posicao == "normal"),
    aes(x = gini_mkt, y = country, label = round(gini_mkt / 100, 3)),
    hjust = -0.25,
    size = 3,
    show.legend = FALSE,
    family = "Roboto Bold", color = "darkblue"
  ) +
  geom_text(
    data = bd_arrow %>% 
      filter(posicao == "normal"),
    aes(x = gini_disp, y = country, label = round(gini_disp / 100, 3)),
    hjust = 1.25,
    size = 3,
    show.legend = FALSE,
    family = "Roboto Bold", color = "firebrick3"
  ) +
  geom_text(
    data = bd_arrow %>% 
      filter(posicao == "invertido"),
    aes(x = gini_mkt, y = country, label = round(gini_mkt / 100, 3)),
    hjust = 1.25,
    size = 3,
    show.legend = FALSE,
    family = "Roboto Bold", color = "darkblue"
  ) +
  geom_text(
    data = bd_arrow %>% 
      filter(posicao == "invertido"),
    aes(x = gini_disp, y = country, label = round(gini_disp / 100, 3)),
    hjust = -0.25,
    size = 3,
    show.legend = FALSE,
    family = "Roboto Bold", color = "firebrick3"
  ) +
  # geom_text(
  #   data = banco_plot %>% filter(name == "gini_disp"),
  #   aes(x = value, y = country, label = round(value / 100, 3), color = name),
  #   hjust = 1.25,
  #   size = 3.5,
  #   show.legend = FALSE,
  #   family = "Roboto Bold"
  # ) +
  scale_x_continuous(
    labels = function(x) {round(x / 100, digits = 3)},
    breaks = seq(0, 100, 2),
    limits = c(22, 59)
  ) +
  scale_color_manual(
    breaks = c("gini_disp", "gini_mkt"),
    values = c("gini_mkt" = "darkblue", "gini_disp" = "firebrick3"),
    labels = c("Gini - Disposable (post-tax)   ", "Gini - Market (pre-tax)")
  ) +
  labs(
    title    = "Diferença no Gini pré e pós impostos e transferências",
    subtitle = "Todos os países com dados mais recentes disponíveis desde 2023",
    x        = "\nÍndice de Gini",
    y        = NULL,
    caption  = "\nFonte: Standardized World Income Inequality Database\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(family = "Roboto", size = 10, color = "black", hjust = 0.5, angle = 0),
    axis.text.y = element_text(family = "Roboto", size = 10, color = "black"),
    plot.caption = element_text(family = "Roboto", size = 11),
    plot.title = element_text(face = "bold", family = "Roboto", size = 19, hjust = 0.5),
    plot.subtitle = element_text(family = "Roboto", size = 15, hjust = 0.5, 
                                 lineheight = 0.75)
  )


  