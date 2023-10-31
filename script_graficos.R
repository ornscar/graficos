
# bibliotecas -------------------------------------------------------------

#dados
library(dplyr)
library(forcats)
#graficos
library(ggplot2)
library(scales)
library(ggpubr)
library(patchwork)


# dados -------------------------------------------------------------------

# diabetes gestacional
d_diab <- readr::read_csv("docs/dados/diabetes.csv")

# pnud
d_pnud <- readxl::read_excel("docs/dados/pnud.xlsx") 


# graficos univariados ----------------------------------------------------

# barras

ggplot(d_diab, aes(x = imc_classe, y = after_stat(count))) +
  # grafico de barras
  geom_bar(color = "blue", fill = "white") +
  # adiciona rotulos em cima das barras
  geom_text(
    stat = "count", 
    aes(label = round(after_stat(count)/sum(after_stat(count)), 1) * 100,
        vjust = -1)
  ) +
  # nomes do titulo e eixos x e y 
  labs(
    title = "Frequência do IMC categórico das gestantes",
    x = "IMC categórico", 
    y = "Frequência" 
  )

# barras ordenadas em ordem decrescente

d_diab |> 
  # agrupa os dados por imc
  group_by(imc_classe) |> 
  # conta numero de casos por categoria de imc
  summarise(n = n()) |> 
  # reordena as frequencias de imc em ordem decrescente
  mutate(imc = forcats::fct_reorder(imc_classe, dplyr::desc(n))) |> 
  ggplot(aes(x = imc, y = n), fill = imc) +
  # grafico de barras
    geom_bar(stat = "identity", color = "blue", fill = "white") +
    # adiciona rotulos em cima das barras
    geom_text(aes(label = n, vjust = -1)) +
    # nomes do titulo e eixos x e y 
    labs(
      title = "Frequência do IMC categórico das gestantes",
      x = "IMC categórico", 
      y = "Frequência" 
    )

# barras com percentuais

ggplot(d_diab, aes(x = imc_classe, y = after_stat(count)/sum(after_stat(count)))) +
  # grafico de barras
  geom_bar(color = "blue", fill = "white") +
  # eixo y em porcentagem
  scale_y_continuous(labels = scales::percent) +
  # adiciona rotulos em cima das barras
  geom_text(
    stat = "count", 
    aes(label = round(after_stat(count)/sum(after_stat(count)), 1) * 100,
        vjust = -1)
  ) +
  # nomes do titulo e eixos x e y 
  labs(
    title = "Frequência do IMC categórico das gestantes",
    x = "IMC categórico", 
    y = "Frequência" 
  )

# histograma

ggplot(d_diab, aes(x = glicemia_jejum)) +
  # histograma
  geom_histogram(color = "white", breaks = seq(90, 125, 5)) + #breaks: tamanho do intervalo igual a 5
  # nomes do titulo e eixos x e y 
  labs(
    title = "Frequência dos valores do exame de glicemia de jejum",
    x = "Glicemia de jejum (em mg/dL)",
    y = "Frequência"
  ) +
  # tema (background) do grafico
  theme_bw() 


# graficos bivariados - qualitativas x qualitativas -----------------------

# barras agrupadas

d_diab |> 
  # filtra casos diferentes de NA
  dplyr::filter(!is.na(cor)) |> 
  ggplot(aes(x = cor, fill = insulina)) + #fill: preenche as barras por grupo de insulina
  # grafico de barras
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), position = "dodge") +
  # eixo y em porcentagem
  scale_y_continuous(labels = scales::percent) +
  # nomes do titulo, eixos x e y e legenda
  labs(
    x = "Raça/cor",
    y = "Frequência",
    fill = "Usou insulina?"
  ) +
  # tema (backgroun) do grafico
  theme_bw()

# barras empilhadas (sem rotulos)

d_diab |> 
  # filtra casos diferentes de NA
  dplyr::filter(!is.na(cor)) |> 
  ggplot(aes(x = cor, fill = insulina)) + #fill: preenche barras por grupo de insulina
  # grafico de barras empilhadas em 100%
  geom_bar(position = "fill") +
  # eixo y em porcentagem
  scale_y_continuous(labels = scales::percent) +
  # nomes do titulo, eixos x e y e legenda
  labs( 
    x = "Raça/cor", 
    y = "Frequência", 
    fill = "Usou insulina?" 
  ) +
  # altera cor das barras
  scale_fill_brewer(palette = "Accent")

# barras empilhadas (com rotulos)

d_diab |> 
  # filtra somente casos validos (diferentes de NA)
  dplyr::filter(!is.na(cor)) |> 
  ggplot(aes(x = cor, fill = insulina)) +
  # grafico de barras
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs( 
    x = "Raça/cor", 
    y = "Frequência", 
    fill = "Usou insulina?" 
  ) +
  scale_fill_brewer(palette = "Accent") -> g #salva grafico num objeto 'g'

g_diab <- g$data |> #acessa os dados (data frame)
  # agrupa os dados por raca/cor e grupo de insulina
  dplyr::group_by(cor, insulina) |>
  # conta quantos casos de insulina 'sim' e 'nao' tem em cada categoria de raca/cor 
  dplyr::summarise(n = dplyr::n()) |> 
  # cria variaveis de proporcao e rotulos
  dplyr::mutate(
    prop = n / sum(n), 
    rotulo = scales::percent(prop)
  )

g_diab |> 
  ggplot(aes(x = cor, y = prop, fill = insulina)) + #fill: preenche por grupo de insulina
  # grafico de barras empilhadas em 100%
  geom_bar(stat = "identity", position = "fill") + #identity: usar as barras para exibir as somas de y
  # eixo y em porcentagem
  scale_y_continuous(labels = scales::percent) +
  # nomes do titulo, eixos x e y e legenda
  labs( 
    x = "Raça/cor", 
    y = "Frequência", 
    fill = "Usou insulina?" 
  ) +
  # altera cor das barras
  scale_fill_brewer(palette = "Accent") +
  # adiciona rotulos no meio de cada categoria
  geom_text(
    aes(label = rotulo), size = 3, 
    position = position_stack(vjust = .5)
  ) 


# graficos bivariados - quantitativas x quantitativas ---------------------

# dispersao

ggplot(d_diab, aes(x = idade, y = glicemia_jejum)) +
  # grafico de pontos
  geom_point(size = 2, alpha = .5) +
  # adiciona valor da correlacao
  ggpubr::stat_cor( 
    aes(label = after_stat(r.label)), method = "pearson",
    label.x = 40, label.y = 120, size = 4.5, color = "red" 
  ) +
  # nomes dos eixos x e y
  labs( 
    x = "Idade (em anos)",
    y = "Valor do exame de glicemia de jejum (em mg/dL)" 
  )

# dispersao (com funcao base do R plot())

plot(d_diab$idade, d_diab$glicemia_jejum)


# graficos bivariados - qualitativas x quantitativas ----------------------

# linhas

d_pnud |> 
  # agrupa dados por ano e agregacao
  dplyr::group_by(ano, agregacao) |> 
  # calcula a media da expectativa de vida por agregacao
  dplyr::summarise(media_espvida = mean(espvida)) |> 
  ggplot(aes(x = ano, y = media_espvida, color = agregacao)) + #color: colore as linhas por agregacao
    # grafico de linhas
    geom_line() +
    # grafico de pontos
    geom_point() +
    # escala de 2012 a 2012 indo de 1 a 1
    scale_x_continuous(breaks = seq(2012, 2021, 1)) +
    # tema (background) do grafico
    theme_bw() +
    # nomes dos eixos x e y e legenda
    labs(
      x = "Ano",
      y = "Expectativa de vida média (em anos)",
      color = "Agregação"
    ) +
    # legenda embaixo do grafico
    theme(legend.position = "bottom")

# boxplot e violino

ggplot(d_diab, aes(x = hb_glicada, y = glicemia_jejum)) + 
  # grafico de violino
  geom_violin(fill = "#add8e6") +
  # grafico de boxplot
  geom_boxplot(width = .2) +
  # nome dos eixos x e y e legenda
  labs(
    x = "Hemoglobina glicada categórica",
    y = "Valor do exame de glicemia de jejum (em mg/dL)", 
    fill = "Usou insulina?"
  ) +
  # tema (background) do grafico
  theme_classic() +
  # divide o grafico em funcao da variavel 'insulina' (ou seja, nas categorias 'sim' e 'nao')
  facet_wrap(. ~ insulina)

# mapa de calor

d_pnud |> 
  # filtra casos que se referem aos estados
  dplyr::filter(agregacao == "UF") |> 
  ggplot(aes(x = ano, y = nome, fill = gini)) + #fill: preenche por indice de gini
    # mapa de calor
    geom_tile() +
    # escala de cor viridis
    scale_fill_viridis_c() +
    # tema (background) do grafico
    theme_bw() +
    # eixo x na escala de 2012 a 2021 indo de 1 a 1
    scale_x_continuous(breaks = seq(2012, 2021, 1), expand = c(0, 0)) + #expand: remove os espacos em branco entre os eixos
    # nome dos eixos x e y e legenda
    labs(
      x = "Ano", 
      y = "Unidade Federativa (UF)", 
      fill = "Expectativa \nde vida"
    ) +
    theme( 
      legend.position = "bottom", #legenda embaixo
      legend.key.width = unit(1.5, 'cm') #aumenta a escala da legenda
    )

# halteres (dumbell)

d_pnud |> 
  # filtra casos que se referem às ufs dos anos 2012 e 2019
  dplyr::filter(agregacao == "UF" & ano %in% c(2012, 2019)) |> 
  ggplot(aes(x = espvida, y = reorder(nome, espvida, max))) + #reorder: ornena a uf em forma decrescente
   # grafico de pontos
   geom_point(aes(color = factor(ano))) + #color: transforma ano em fator e colore os pontos de acordo com os anos
   # grafico de linhas
   geom_line(aes(group = nome)) + #group: colore por uf
   # nomes do titulo, subtitulo e eixos x e y
   labs( 
     title = "Expectativa de vida do brasileiro, por UF", 
     subtitle = "Entre os anos de 2012 a 2019", 
     x = "Expectativa de vida (em anos)", 
     y = "Unidade Federativa (UF)" 
   ) +
   # tema (background) do grafico
   theme_minimal()

# media com barras de erros (errorbar)

d_diab |> 
  # agrupa os dados por imc
  dplyr::group_by(imc_classe) |> 
  dplyr::summarise(
    n = dplyr::n(), #numero de casos por imc
    media = mean(glicemia_jejum), #media por imc
    desvio = sd(glicemia_jejum), #dp por imc
    erro.padrao = desvio / sqrt(n) #erro padrao por imc
  ) |> 
  ggplot(aes(x = imc_classe, y = media)) +
    # grafico de pontos
    geom_point(size = 3) +
    # errorbar
    geom_errorbar( 
      aes(ymin = media - erro.padrao, 
          ymax = media + erro.padrao), 
      width = .1 
    )

# media com barras de erros (errorbar), por grupo de insulina

d_diab |> 
  # agrupa dados por imc e insulina
  dplyr::group_by(imc_classe, insulina) |> 
  dplyr::summarise(
    n = dplyr::n(), #numero de casos de insulina 'sim' e 'nao' por imc
    media = mean(glicemia_jejum), #media de casos de insulina 'sim' e 'nao' por imc
    desvio = sd(glicemia_jejum), #dp dos casos de insulina 'sim' e 'nao' por imc
    erro.padrao = desvio / sqrt(n) #erro padrao dos casos de insulina 'sim' e 'nao' por imc
  ) |> 
  ggplot(aes(x = imc_classe, y = media, group = insulina, color = insulina)) + 
    # grafico de pontos
    geom_point(size = 3) +
    # errorbar
    geom_errorbar( 
      aes(ymin = media - erro.padrao, 
          ymax = media + erro.padrao), 
      width = .1 
    ) +
    # nomes dos eixos x e y e legenda
    labs( 
      x = "IMC categórico", 
      y = "Média do valor do exame de glicemia de jejum (em mg/dL)", 
      fill = "Usou insulina?" 
    ) +
    # tema (background) do grafico
    theme_bw()


# junta graficos numa mesma imagem ----------------------------------------

g1 <- ggplot(d_diab, aes(x = imc_classe, y = after_stat(count)/sum(after_stat(count)))) +
  geom_bar(color = "blue", fill = "white") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(
    stat = "count", 
    aes(label = round(after_stat(count)/sum(after_stat(count)), 1) * 100,
        vjust = -1)
  ) +
  labs(
    title = "Frequência do IMC categórico das gestantes",
    x = "IMC categórico", 
    y = "Frequência" 
  )

g2 <- ggplot(d_diab, aes(x = glicemia_jejum)) +
  geom_histogram(color = "white", breaks = seq(90, 125, 5)) +
  labs(
    title = "Frequência dos valores do exame de glicemia de jejum",
    x = "Glicemia de jejum (em mg/dL)",
    y = "Frequência"
  ) +
  theme_bw() 

# graficos um embaixo do outro (usa-se o caractere /)

g1 / g2

# graficos lado a lado (usa-se o simbolo + ou |)

g1 + g2


# salva grafico -----------------------------------------------------------

ggsave("graficos/univariados.png", width = 16, height = 10)


