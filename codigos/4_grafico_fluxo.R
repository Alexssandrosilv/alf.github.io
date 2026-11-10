# ==============================================================================
# PROJETO ALF — Painel de Gráficos: Empilhado, Linhas e Boxplot
# ==============================================================================
# 3 Visões integradas:
#   1. Empilhado (Barras com cantos arredondados)
#   2. Linhas (Linhas finas + bolinhas pretas ao selecionar)
#   3. Boxplot (Pontos com cor própria + hover Dia/Valor, Outliers destacados
#      com rótulo à direita, e linha/rótulo da Média em cor diferenciada)
# No Mobile: Apenas o Card inferior ao clicar (sem balão em cima do gráfico).
# No PC: Hover estilizado + Card interativo inferior ao clicar na coluna.

# ==============================================================================
# 1. CONFIGURAÇÕES VISUAIS (PADRÃO ALEX + PALETA AZUL/CIANO)
# ==============================================================================

titulo_nome     <- "Fluxo Financeiro: Entradas e Saídas por data de pagamento"
titulo_cor      <- "#03045e"
titulo_tamanho  <- 16

# Cores das séries (Caixas / Barras / Linhas)
cor_entradas         <- "#29b6d8"   # Azul Ciano Vibrante
cor_saidas           <- "#1b78a1"   # Azul Oceano Escuro

# Cores exclusivas para os elementos do Boxplot
cor_ponto_entradas   <- "#005082"   # Azul profundo (diferente da caixa ciano)
cor_ponto_saidas     <- "#03045e"   # Azul marinho escuro (diferente da caixa oceano)
cor_outlier          <- "#e11d48"   # Vermelho/Coral vibrante para Outliers
cor_media_box        <- "#d97706"   # Âmbar/Dourado escuro para a linha e rótulo da Média

# Cor da sombra de seleção no fundo (bem clarinha e suave)
cor_sombra_selecao   <- "rgba(226, 232, 240, 0.45)"

tamanho_rotulos <- 10.5
cor_rotulos     <- "#000000"
tamanho_eixo_x  <- 11
cor_eixo_x      <- "#000000"
tamanho_legenda <- 12.5

# Balão de Hover no PC
hover_bg_cor      <- "#FFFFFF"
hover_txt_cor     <- "#000000"
hover_fonte_tam   <- 12
borda_balao_hover <- "#000000"

raio_cantos           <- 8
altura_grafico        <- 440 # Altura no Notebook/PC
altura_grafico_mobile <- 290 # Altura compacta no Celular

incluir_a_pagar <- TRUE


# ==============================================================================
# 2. CARREGAMENTO DAS FOTOS DOS USUÁRIOS (BASE64)
# ==============================================================================

uri_alex    <- knitr::image_uri(here::here("imagens", "alex_transparente.png"))
uri_leticia <- knitr::image_uri(here::here("imagens", "leticia_transparente.png"))


# ==============================================================================
# 3. PREPARAÇÃO DOS DADOS
# ==============================================================================

meses_pt <- base::c(
  "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
)

formatar_brl <- function(x) {
  base::formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
}

formatar_brl_curto <- function(x) {
  base::ifelse(
    x == 0, "",
    base::paste0("<b>R$ ", base::formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ","), "</b>")
  )
}

padronizar_usuario <- function(u) {
  u_limpo <- base::trimws(base::as.character(u))
  dplyr::case_when(
    base::grepl("let", u_limpo, ignore.case = TRUE) ~ "Leticia",
    base::grepl("alex", u_limpo, ignore.case = TRUE) ~ "Alex",
    TRUE ~ u_limpo
  )
}

entradas_fluxo <- bd_entradas %>%
  dplyr::filter(!base::is.na(data)) %>%
  dplyr::transmute(
    tipo    = "Entradas",
    data    = base::as.Date(data),
    valor   = base::as.numeric(valor),
    usuario = padronizar_usuario(usuario_id),
    grupo_1 = dplyr::coalesce(base::as.character(fonte_renda), "Outras Fontes"),
    grupo_2 = dplyr::coalesce(base::as.character(descricao), "Sem descrição"),
    status  = "Recebido"
  )

saidas_fluxo <- bd_saidas %>%
  dplyr::filter(!base::is.na(data_pagamento)) %>%
  dplyr::transmute(
    tipo    = "Saídas",
    data    = base::as.Date(data_pagamento),
    valor   = base::as.numeric(valor),
    usuario = padronizar_usuario(usuario_id),
    grupo_1 = dplyr::coalesce(base::as.character(forma_pagamento), "Outros"),
    grupo_2 = dplyr::coalesce(base::as.character(tipo_despesa), "variáveis"),
    status  = dplyr::coalesce(base::as.character(status), "A pagar")
  )

if (!incluir_a_pagar) {
  saidas_fluxo <- saidas_fluxo %>% dplyr::filter(status == "Pago")
}

fluxo <- dplyr::bind_rows(entradas_fluxo, saidas_fluxo) %>%
  dplyr::mutate(
    grupo_1   = base::ifelse(base::trimws(grupo_1) == "", "Não informado", grupo_1),
    grupo_2   = base::ifelse(base::trimws(grupo_2) == "", "Sem descrição", grupo_2),
    mes_num   = base::as.integer(base::format(data, "%m")),
    ano_num   = base::as.integer(base::format(data, "%Y")),
    mes_ref   = base::format(data, "%Y-%m"),
    mes_nome  = meses_pt[mes_num],
    mes_label = base::paste0(mes_nome, "/", ano_num)
  )

tabela_meses <- fluxo %>%
  dplyr::distinct(mes_ref, mes_label, mes_nome) %>%
  dplyr::arrange(mes_ref)

meses_disponiveis <- base::c("Todos os Meses", tabela_meses$mes_label)
opcoes_usuario    <- base::c("Todos", "Alex", "Leticia")


# ==============================================================================
# 4. FUNÇÕES DE HOVER (1ª LINHA TODA EM AZUL INCLUINDO O n = X)
# ==============================================================================

montar_hover_inteligente <- function(df_sub, titulo_periodo, tipo_sel, compacto = FALSE) {
  cor_titulo <- base::ifelse(tipo_sel == "Entradas", cor_entradas, cor_saidas)
  n_lanc     <- base::nrow(df_sub)
  tam_topo   <- if (compacto) "12px" else "13px"
  
  # 1ª linha inteira (Data/Mês | R$ Valor (n = X)) toda na cor azul da série!
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0(
      "<b style='color:", cor_titulo, "; font-size:", tam_topo, ";'>",
      titulo_periodo, " | R$ 0,00 (n = 0)</b>"
    ))
  }
  
  val_total <- base::sum(df_sub$valor, na.rm = TRUE)
  val_media <- base::mean(df_sub$valor, na.rm = TRUE)
  val_max   <- base::max(df_sub$valor, na.rm = TRUE)
  val_min   <- base::min(df_sub$valor, na.rm = TRUE)
  
  lim_chars <- if (compacto) 22 else 34
  qtd_top   <- if (compacto) 4 else 5
  
  cabecalho <- base::paste0(
    "<b style='color:", cor_titulo, "; font-size:", tam_topo, ";'>",
    titulo_periodo, " | R$ ", formatar_brl(val_total), " (n = ", n_lanc, ")</b><br>"
  )
  
  if (tipo_sel == "Saídas") {
    rotulo_1 <- "Forma de Pagamento:"
    rotulo_2 <- "Tipo de Despesa:"
  } else {
    rotulo_1 <- "Fonte de Renda:"
    rotulo_2 <- "Descrição:"
  }
  
  res_1 <- df_sub %>%
    dplyr::group_by(grupo_1) %>%
    dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(soma)) %>%
    utils::head(qtd_top) %>%
    dplyr::mutate(
      pct      = base::round((soma / val_total) * 100, 1),
      nome_lim = base::substr(grupo_1, 1, lim_chars),
      linha    = base::paste0("• ", nome_lim, ": R$ ", formatar_brl(soma), " (n = ", qtd, " | ", pct, "%)")
    )
  bloco_1 <- base::paste0("<br><b>", rotulo_1, "</b><br>", base::paste(res_1$linha, collapse = "<br>"))
  
  res_2 <- df_sub %>%
    dplyr::group_by(grupo_2) %>%
    dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(soma)) %>%
    utils::head(qtd_top) %>%
    dplyr::mutate(
      pct      = base::round((soma / val_total) * 100, 1),
      nome_lim = base::substr(grupo_2, 1, lim_chars),
      linha    = if (tipo_sel == "Entradas") {
        base::paste0("• ", nome_lim, ": R$ ", formatar_brl(soma), " (", pct, "%)")
      } else {
        base::paste0("• ", nome_lim, ": R$ ", formatar_brl(soma), " (n = ", qtd, " | ", pct, "%)")
      }
    )
  bloco_2 <- base::paste0("<br><b>", rotulo_2, "</b><br>", base::paste(res_2$linha, collapse = "<br>"))
  
  sep_stats <- if (compacto) "<br>" else " | "
  rodape_stats <- base::paste0(
    "<br><br><span style='color:#444444; font-size:", if (compacto) "10.5px" else "11px", "'>",
    "<b>Média:</b> R$ ", formatar_brl(val_media),
    sep_stats,
    "<b>Máx:</b> R$ ", formatar_brl(val_max),
    " | <b>Mín:</b> R$ ", formatar_brl(val_min),
    "</span>"
  )
  
  base::paste0(cabecalho, bloco_1, bloco_2, rodape_stats)
}

# Hover estatístico da Caixa do Boxplot (também com o n = X em azul)
montar_hover_boxplot <- function(df_sub, titulo_periodo, tipo_sel) {
  cor_titulo <- base::ifelse(tipo_sel == "Entradas", cor_entradas, cor_saidas)
  n_lanc     <- base::nrow(df_sub)
  
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0(
      "<b style='color:", cor_titulo, "; font-size:13px;'>",
      titulo_periodo, " | R$ 0,00 (n = 0)</b>"
    ))
  }
  
  vals      <- df_sub$valor
  val_total <- base::sum(vals, na.rm = TRUE)
  val_med   <- stats::median(vals, na.rm = TRUE)
  val_q1    <- stats::quantile(vals, 0.25, na.rm = TRUE, names = FALSE)
  val_q3    <- stats::quantile(vals, 0.75, na.rm = TRUE, names = FALSE)
  val_iiq   <- val_q3 - val_q1
  val_media <- base::mean(vals, na.rm = TRUE)
  val_max   <- base::max(vals, na.rm = TRUE)
  val_min   <- base::min(vals, na.rm = TRUE)
  
  base::paste0(
    "<b style='color:", cor_titulo, "; font-size:13px;'>",
    titulo_periodo, " | R$ ", formatar_brl(val_total), " (n = ", n_lanc, ")</b><br><br>",
    "<b>Resumo do Boxplot (", tipo_sel, "):</b><br>",
    "• <b>Mediana (Q2):</b> R$ ", formatar_brl(val_med), "<br>",
    "• <b>1º Quartil (25%):</b> R$ ", formatar_brl(val_q1), "<br>",
    "• <b>3º Quartil (75%):</b> R$ ", formatar_brl(val_q3), "<br>",
    "• <b>Amplitude Interquartil (IIQ):</b> R$ ", formatar_brl(val_iiq), "<br><br>",
    "<span style='color:#444444; font-size:11px'>",
    "<b style='color:", cor_media_box, ";'>Média: R$ ", formatar_brl(val_media), "</b>",
    " | <b>Máx:</b> R$ ", formatar_brl(val_max),
    " | <b>Mín:</b> R$ ", formatar_brl(val_min),
    "</span>"
  )
}


# ==============================================================================
# 5. GERADOR DE CAMADAS (BARRAS, LINHAS E BOXPLOT CUSTOMIZADO COM OUTLIERS/MÉDIA)
# ==============================================================================

gerar_par_traces <- function(mes_sel, usuario_sel) {
  
  d <- fluxo
  if (usuario_sel != "Todos") {
    d <- d %>% dplyr::filter(usuario == usuario_sel)
  }
  
  usar_ano <- dplyr::n_distinct(fluxo$ano_num) > 1
  
  escopo_meses <- if (mes_sel == "Todos os Meses") {
    tabela_meses
  } else {
    tabela_meses %>% dplyr::filter(mes_label == mes_sel)
  }
  
  escopo_meses <- escopo_meses %>%
    dplyr::mutate(
      eixo_x = if (usar_ano) base::paste0("<b>", mes_label, "</b>") else base::paste0("<b>", mes_nome, "</b>")
    )
  
  # Vetores para alimentar o Boxplot + Pontos Individuais + Outliers + Média
  box_cats      <- base::as.character(escopo_meses$eixo_x)
  box_e_x       <- base::character(0)
  box_e_y       <- base::numeric(0)
  box_s_x       <- base::character(0)
  box_s_y       <- base::numeric(0)
  
  # Vetores dos pontos individuais do Boxplot (Hover com Dia e Valor + Outliers destacados)
  pts_e_x       <- base::numeric(0)
  pts_e_y       <- base::numeric(0)
  pts_e_hov     <- base::character(0)
  pts_e_cor     <- base::character(0)
  pts_e_tam     <- base::numeric(0)
  pts_e_lbl     <- base::character(0)
  
  pts_s_x       <- base::numeric(0)
  pts_s_y       <- base::numeric(0)
  pts_s_hov     <- base::character(0)
  pts_s_cor     <- base::character(0)
  pts_s_tam     <- base::numeric(0)
  pts_s_lbl     <- base::character(0)
  
  # Vetores para a Linha e Rótulo da Média de cada Boxplot
  med_e_x0      <- base::numeric(0)
  med_e_x1      <- base::numeric(0)
  med_e_xr      <- base::numeric(0)
  med_e_y       <- base::numeric(0)
  med_e_txt     <- base::character(0)
  med_e_hov     <- base::character(0)
  
  med_s_x0      <- base::numeric(0)
  med_s_x1      <- base::numeric(0)
  med_s_xr      <- base::numeric(0)
  med_s_y       <- base::numeric(0)
  med_s_txt     <- base::character(0)
  med_s_hov     <- base::character(0)
  
  box_res_e_y   <- base::numeric(base::nrow(escopo_meses))
  box_res_s_y   <- base::numeric(base::nrow(escopo_meses))
  box_res_e_hov <- base::character(base::nrow(escopo_meses))
  box_res_s_hov <- base::character(base::nrow(escopo_meses))
  
  for (i in base::seq_len(base::nrow(escopo_meses))) {
    idx_zero <- i - 1 # Índice 0-based no eixo X numérico do Boxplot
    m_ref    <- escopo_meses$mes_ref[i]
    m_lbl    <- escopo_meses$mes_label[i]
    cat_x    <- escopo_meses$eixo_x[i]
    
    sub_e <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Entradas")
    sub_s <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Saídas")
    
    box_res_e_y[i]   <- base::sum(sub_e$valor, na.rm = TRUE)
    box_res_s_y[i]   <- base::sum(sub_s$valor, na.rm = TRUE)
    box_res_e_hov[i] <- montar_hover_boxplot(sub_e, m_lbl, "Entradas")
    box_res_s_hov[i] <- montar_hover_boxplot(sub_s, m_lbl, "Saídas")
    
    # --- ENTRADAS NO MÊS i (Centro da caixa em idx_zero - 0.19) ---
    if (base::nrow(sub_e) > 0) {
      vals_e <- sub_e$valor
      q1_e   <- stats::quantile(vals_e, 0.25, na.rm = TRUE, names = FALSE)
      q3_e   <- stats::quantile(vals_e, 0.75, na.rm = TRUE, names = FALSE)
      iiq_e  <- q3_e - q1_e
      lim_sup_e <- q3_e + 1.5 * iiq_e
      lim_inf_e <- q1_e - 1.5 * iiq_e
      eh_out_e  <- (vals_e > lim_sup_e | vals_e < lim_inf_e) & (iiq_e > 0)
      
      # Jitter horizontal determinístico suave para pontos não colidirem
      n_e <- base::nrow(sub_e)
      desloc_e <- if (n_e == 1) 0 else base::seq(-0.045, 0.045, length.out = n_e)
      x_pts_e  <- (idx_zero - 0.19) + base::ifelse(eh_out_e, 0, desloc_e)
      
      hov_ind_e <- base::paste0(
        "<b style='color:", base::ifelse(eh_out_e, cor_outlier, cor_entradas), "; font-size:13px;'>",
        base::format(sub_e$data, "%d/%m/%Y"), " | R$ ", formatar_brl(vals_e),
        base::ifelse(eh_out_e, " (Outlier)", ""), "</b><br>",
        "• <b>Fonte:</b> ", sub_e$grupo_1, "<br>",
        "• <b>Descrição:</b> ", sub_e$grupo_2
      )
      
      lbl_out_e <- base::ifelse(eh_out_e, base::paste0(" <b>R$ ", formatar_brl_curto(vals_e), "</b>"), "")
      lbl_out_e <- base::gsub("<b>R\\$ <b>R\\$ ", "<b>R$ ", lbl_out_e)
      
      box_e_x   <- base::c(box_e_x, base::rep(cat_x, n_e))
      box_e_y   <- base::c(box_e_y, vals_e)
      pts_e_x   <- base::c(pts_e_x, x_pts_e)
      pts_e_y   <- base::c(pts_e_y, vals_e)
      pts_e_hov <- base::c(pts_e_hov, hov_ind_e)
      pts_e_cor <- base::c(pts_e_cor, base::ifelse(eh_out_e, cor_outlier, cor_ponto_entradas))
      pts_e_tam <- base::c(pts_e_tam, base::ifelse(eh_out_e, 8.5, 6.5))
      pts_e_lbl <- base::c(pts_e_lbl, lbl_out_e)
      
      val_med_e <- base::mean(vals_e, na.rm = TRUE)
      med_e_x0  <- base::c(med_e_x0, idx_zero - 0.32)
      med_e_x1  <- base::c(med_e_x1, idx_zero - 0.06)
      med_e_xr  <- base::c(med_e_xr, idx_zero - 0.05)
      med_e_y   <- base::c(med_e_y, val_med_e)
      med_e_txt <- base::c(med_e_txt, base::paste0(" <b>Méd: R$ ", base::formatC(val_med_e, format = "f", digits = 0, big.mark = ".", decimal.mark = ","), "</b>"))
      med_e_hov <- base::c(med_e_hov, base::paste0("<b style='color:", cor_media_box, ";'>Média Entradas (", m_lbl, "): R$ ", formatar_brl(val_med_e), "</b>"))
    }
    
    # --- SAÍDAS NO MÊS i (Centro da caixa em idx_zero + 0.19) ---
    if (base::nrow(sub_s) > 0) {
      vals_s <- sub_s$valor
      q1_s   <- stats::quantile(vals_s, 0.25, na.rm = TRUE, names = FALSE)
      q3_s   <- stats::quantile(vals_s, 0.75, na.rm = TRUE, names = FALSE)
      iiq_s  <- q3_s - q1_s
      lim_sup_s <- q3_s + 1.5 * iiq_s
      lim_inf_s <- q1_s - 1.5 * iiq_s
      eh_out_s  <- (vals_s > lim_sup_s | vals_s < lim_inf_s) & (iiq_s > 0)
      
      n_s <- base::nrow(sub_s)
      desloc_s <- if (n_s == 1) 0 else base::seq(-0.045, 0.045, length.out = n_s)
      x_pts_s  <- (idx_zero + 0.19) + base::ifelse(eh_out_s, 0, desloc_s)
      
      hov_ind_s <- base::paste0(
        "<b style='color:", base::ifelse(eh_out_s, cor_outlier, cor_saidas), "; font-size:13px;'>",
        base::format(sub_s$data, "%d/%m/%Y"), " | R$ ", formatar_brl(vals_s),
        base::ifelse(eh_out_s, " (Outlier)", ""), "</b><br>",
        "• <b>Forma Pgto:</b> ", sub_s$grupo_1, "<br>",
        "• <b>Tipo Despesa:</b> ", sub_s$grupo_2
      )
      
      lbl_out_s <- base::ifelse(eh_out_s, base::paste0(" <b>R$ ", formatar_brl_curto(vals_s), "</b>"), "")
      lbl_out_s <- base::gsub("<b>R\\$ <b>R\\$ ", "<b>R$ ", lbl_out_s)
      
      box_s_x   <- base::c(box_s_x, base::rep(cat_x, n_s))
      box_s_y   <- base::c(box_s_y, vals_s)
      pts_s_x   <- base::c(pts_s_x, x_pts_s)
      pts_s_y   <- base::c(pts_s_y, vals_s)
      pts_s_hov <- base::c(pts_s_hov, hov_ind_s)
      pts_s_cor <- base::c(pts_s_cor, base::ifelse(eh_out_s, cor_outlier, cor_ponto_saidas))
      pts_s_tam <- base::c(pts_s_tam, base::ifelse(eh_out_s, 8.5, 6.5))
      pts_s_lbl <- base::c(pts_s_lbl, lbl_out_s)
      
      val_med_s <- base::mean(vals_s, na.rm = TRUE)
      med_s_x0  <- base::c(med_s_x0, idx_zero + 0.06)
      med_s_x1  <- base::c(med_s_x1, idx_zero + 0.32)
      med_s_xr  <- base::c(med_s_xr, idx_zero + 0.33)
      med_s_y   <- base::c(med_s_y, val_med_s)
      med_s_txt <- base::c(med_s_txt, base::paste0(" <b>Méd: R$ ", base::formatC(val_med_s, format = "f", digits = 0, big.mark = ".", decimal.mark = ","), "</b>"))
      med_s_hov <- base::c(med_s_hov, base::paste0("<b style='color:", cor_media_box, ";'>Média Saídas (", m_lbl, "): R$ ", formatar_brl(val_med_s), "</b>"))
    }
  }
  
  # Dados para Barras Empilhadas e Linhas (Mensal quando "Todos", Diário quando Mês)
  if (mes_sel == "Todos os Meses") {
    n_pts      <- base::nrow(escopo_meses)
    eixo_x_vec <- base::as.character(escopo_meses$eixo_x)
    y_e        <- base::numeric(n_pts)
    y_s        <- base::numeric(n_pts)
    hov_e      <- base::character(n_pts)
    hov_s      <- base::character(n_pts)
    hov_e_mob  <- base::character(n_pts)
    hov_s_mob  <- base::character(n_pts)
    
    for (i in base::seq_len(n_pts)) {
      m_ref <- escopo_meses$mes_ref[i]
      m_lbl <- escopo_meses$mes_label[i]
      
      sub_e <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Entradas")
      sub_s <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Saídas")
      
      y_e[i]       <- base::sum(sub_e$valor, na.rm = TRUE)
      y_s[i]       <- base::sum(sub_s$valor, na.rm = TRUE)
      hov_e[i]     <- montar_hover_inteligente(sub_e, m_lbl, "Entradas", FALSE)
      hov_s[i]     <- montar_hover_inteligente(sub_s, m_lbl, "Saídas",   FALSE)
      hov_e_mob[i] <- montar_hover_inteligente(sub_e, m_lbl, "Entradas", TRUE)
      hov_s_mob[i] <- montar_hover_inteligente(sub_s, m_lbl, "Saídas",   TRUE)
    }
    
  } else {
    d_mes <- d %>% dplyr::filter(mes_label == mes_sel)
    
    dias_ativos <- d_mes %>%
      dplyr::group_by(data) %>%
      dplyr::summarise(movimento = base::sum(valor, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(movimento > 0) %>%
      dplyr::arrange(data) %>%
      dplyr::pull(data)
    
    if (base::length(dias_ativos) == 0) {
      eixo_x_vec <- base::c("<b>Sem lançamentos</b>")
      y_e        <- base::c(0)
      y_s        <- base::c(0)
      hov_e      <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Entradas", FALSE))
      hov_s      <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Saídas",   FALSE))
      hov_e_mob  <- hov_e
      hov_s_mob  <- hov_s
    } else {
      n_pts      <- base::length(dias_ativos)
      eixo_x_vec <- base::paste0("<b>", base::format(dias_ativos, "%d"), "</b>")
      y_e        <- base::numeric(n_pts)
      y_s        <- base::numeric(n_pts)
      hov_e      <- base::character(n_pts)
      hov_s      <- base::character(n_pts)
      hov_e_mob  <- base::character(n_pts)
      hov_s_mob  <- base::character(n_pts)
      
      for (i in base::seq_len(n_pts)) {
        dia_i   <- dias_ativos[i]
        lbl_dia <- base::format(dia_i, "%d/%m/%Y")
        
        sub_e <- d_mes %>% dplyr::filter(data == dia_i, tipo == "Entradas")
        sub_s <- d_mes %>% dplyr::filter(data == dia_i, tipo == "Saídas")
        
        y_e[i]       <- base::sum(sub_e$valor, na.rm = TRUE)
        y_s[i]       <- base::sum(sub_s$valor, na.rm = TRUE)
        hov_e[i]     <- montar_hover_inteligente(sub_e, lbl_dia, "Entradas", FALSE)
        hov_s[i]     <- montar_hover_inteligente(sub_s, lbl_dia, "Saídas",   FALSE)
        hov_e_mob[i] <- montar_hover_inteligente(sub_e, lbl_dia, "Entradas", TRUE)
        hov_s_mob[i] <- montar_hover_inteligente(sub_s, lbl_dia, "Saídas",   TRUE)
      }
    }
  }
  
  soma_topo <- y_e + y_s
  max_bar   <- base::max(base::c(soma_topo, 100), na.rm = TRUE) * 1.22
  max_line  <- base::max(base::c(y_e, y_s, 100), na.rm = TRUE) * 1.28
  max_box   <- base::max(base::c(box_e_y, box_s_y, 100), na.rm = TRUE) * 1.22
  
  txt_e_bar  <- base::ifelse(y_e >= (max_bar * 0.07), formatar_brl_curto(y_e), "")
  txt_s_bar  <- base::ifelse(y_s > 0, formatar_brl_curto(y_s), "")
  txt_e_line <- formatar_brl_curto(y_e)
  txt_s_line <- formatar_brl_curto(y_s)
  
  tot_e <- base::sum(y_e, na.rm = TRUE)
  tot_s <- base::sum(y_s, na.rm = TRUE)
  dif   <- tot_e - tot_s
  sinal <- base::ifelse(dif > 0, "+", base::ifelse(dif < 0, "-", ""))
  
  nome_e   <- base::paste0("<b>Entradas: R$ ", formatar_brl(tot_e), "</b>")
  nome_s   <- base::paste0("<b>Saídas: R$ ", formatar_brl(tot_s), "</b>")
  nome_dif <- base::paste0("<b>Diferença: ", sinal, "R$ ", formatar_brl(base::abs(dif)), "</b>")
  
  base::list(
    x             = base::as.character(eixo_x_vec),
    e_y           = base::as.numeric(y_e),
    s_y           = base::as.numeric(y_s),
    e_txt_bar     = base::as.character(txt_e_bar),
    s_txt_bar     = base::as.character(txt_s_bar),
    e_txt_line    = base::as.character(txt_e_line),
    s_txt_line    = base::as.character(txt_s_line),
    e_hover       = base::as.character(hov_e),
    s_hover       = base::as.character(hov_s),
    e_hover_mob   = base::as.character(hov_e_mob),
    s_hover_mob   = base::as.character(hov_s_mob),
    # Vetores exclusivos do Boxplot
    box_cats      = base::as.character(box_cats),
    box_e_x       = base::as.character(box_e_x),
    box_e_y       = base::as.numeric(box_e_y),
    box_s_x       = base::as.character(box_s_x),
    box_s_y       = base::as.numeric(box_s_y),
    pts_e_x       = base::as.numeric(pts_e_x),
    pts_e_y       = base::as.numeric(pts_e_y),
    pts_e_hov     = base::as.character(pts_e_hov),
    pts_e_cor     = base::as.character(pts_e_cor),
    pts_e_tam     = base::as.numeric(pts_e_tam),
    pts_e_lbl     = base::as.character(pts_e_lbl),
    pts_s_x       = base::as.numeric(pts_s_x),
    pts_s_y       = base::as.numeric(pts_s_y),
    pts_s_hov     = base::as.character(pts_s_hov),
    pts_s_cor     = base::as.character(pts_s_cor),
    pts_s_tam     = base::as.numeric(pts_s_tam),
    pts_s_lbl     = base::as.character(pts_s_lbl),
    med_e_x0      = base::as.numeric(med_e_x0),
    med_e_x1      = base::as.numeric(med_e_x1),
    med_e_xr      = base::as.numeric(med_e_xr),
    med_e_y       = base::as.numeric(med_e_y),
    med_e_txt     = base::as.character(med_e_txt),
    med_e_hov     = base::as.character(med_e_hov),
    med_s_x0      = base::as.numeric(med_s_x0),
    med_s_x1      = base::as.numeric(med_s_x1),
    med_s_xr      = base::as.numeric(med_s_xr),
    med_s_y       = base::as.numeric(med_s_y),
    med_s_txt     = base::as.character(med_s_txt),
    med_s_hov     = base::as.character(med_s_hov),
    box_res_e_y   = base::as.numeric(box_res_e_y),
    box_res_s_y   = base::as.numeric(box_res_s_y),
    box_res_e_hov = base::as.character(box_res_e_hov),
    box_res_s_hov = base::as.character(box_res_s_hov),
    e_nome        = nome_e,
    s_nome        = nome_s,
    dif_nome      = nome_dif,
    dif_cor       = base::ifelse(dif >= 0, "#15803d", "#b91c1c"),
    max_bar       = base::as.numeric(max_bar)[1],
    max_line      = base::as.numeric(max_line)[1],
    max_box       = base::as.numeric(max_box)[1]
  )
}


# ==============================================================================
# 6. PRÉ-CÁLCULO DE TODOS OS ESTADOS (JSON SEGURO)
# ==============================================================================

banco_estados <- base::list()

for (m in meses_disponiveis) {
  banco_estados[[m]] <- base::list()
  for (u in opcoes_usuario) {
    banco_estados[[m]][[u]] <- gerar_par_traces(m, u)
  }
}

json_estados <- jsonlite::toJSON(banco_estados, auto_unbox = FALSE)


# ==============================================================================
# 7. CONSTRUÇÃO DO GRÁFICO PLOTLY + ONRENDER
# ==============================================================================

estado_ini <- banco_estados[["Todos os Meses"]][["Todos"]]

js_callback <- base::paste0("
function(el, x) {
  const DADOS = ", json_estados, ";
  const painel = el.closest('.painel-fluxo-alf');
  if (!painel) return;

  let mesAtual      = 'Todos os Meses';
  let usuarioAtual  = 'Todos';
  let modoAtual     = 'barras'; // 'barras' | 'linhas' | 'boxplot'
  let indiceClicado = null;
  let serieClicada  = 0;

  const selMes        = painel.querySelector('.select-mes-alf');
  const botoesUsuario = painel.querySelectorAll('.btn-usuario-alf');
  const botoesModo    = painel.querySelectorAll('.btn-modo-alf');
  const legEntradas   = painel.querySelector('.leg-texto-entradas');
  const legSaidas     = painel.querySelector('.leg-texto-saidas');
  const legDiferenca  = painel.querySelector('.leg-texto-diferenca');
  const quadDiferenca = painel.querySelector('.quadrado-diferenca');
  const cardDetalhes  = painel.querySelector('.card-hover-mobile-alf');

  function formatarMoedaJS(v) {
    return v.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  }

  function obterCategoriasAtuais(d) {
    return (modoAtual === 'boxplot') ? (d.box_cats || []) : (d.x || []);
  }

  // Abre o Card Interativo com abas Entradas/Saídas tanto no Mobile quanto no PC ao clicar!
  function atualizarCardInterativo(d) {
    if (!cardDetalhes) return;
    const cats = obterCategoriasAtuais(d);

    if (indiceClicado === null || !cats.length || indiceClicado >= cats.length) {
      cardDetalhes.style.display = 'none';
      cardDetalhes.innerHTML = '';
      return;
    }

    const htmlE = (modoAtual === 'boxplot')
      ? (d.box_res_e_hov[indiceClicado] || '')
      : (d.e_hover[indiceClicado] || '');
    const htmlS = (modoAtual === 'boxplot')
      ? (d.box_res_s_hov[indiceClicado] || '')
      : (d.s_hover[indiceClicado] || '');

    const htmlAtivo = (serieClicada === 1) ? htmlS : htmlE;

    cardDetalhes.innerHTML =
      '<div class=\"topo-card-mobile-alf\">' +
        '<div class=\"abas-card-mobile-alf\">' +
          '<button type=\"button\" class=\"aba-mob-alf ' + (serieClicada === 0 ? 'ativa-e' : '') + '\" data-serie=\"0\">Entradas</button>' +
          '<button type=\"button\" class=\"aba-mob-alf ' + (serieClicada === 1 ? 'ativa-s' : '') + '\" data-serie=\"1\">Saídas</button>' +
        '</div>' +
        '<button type=\"button\" class=\"fechar-card-mobile-alf\" title=\"Fechar\">&times;</button>' +
      '</div>' +
      '<div class=\"corpo-card-mobile-alf\">' + htmlAtivo + '</div>';
    cardDetalhes.style.display = 'block';

    cardDetalhes.querySelectorAll('.aba-mob-alf').forEach(function(btnAba) {
      btnAba.addEventListener('click', function() {
        serieClicada = parseInt(this.getAttribute('data-serie'), 10);
        atualizarCardInterativo(d);
      });
    });

    const btnFechar = cardDetalhes.querySelector('.fechar-card-mobile-alf');
    if (btnFechar) {
      btnFechar.addEventListener('click', function() {
        indiceClicado = null;
        atualizarGrafico();
      });
    }
  }

  function atualizarRotulosTopo(d) {
    const cats = obterCategoriasAtuais(d);
    if (indiceClicado !== null && cats.length && indiceClicado < cats.length) {
      const valE  = (modoAtual === 'boxplot') ? (d.box_res_e_y[indiceClicado] || 0) : (d.e_y[indiceClicado] || 0);
      const valS  = (modoAtual === 'boxplot') ? (d.box_res_s_y[indiceClicado] || 0) : (d.s_y[indiceClicado] || 0);
      const dif   = valE - valS;
      const sinal = dif > 0 ? '+' : (dif < 0 ? '-' : '');
      const corD  = dif >= 0 ? '#15803d' : '#b91c1c';
      const rotX  = String(cats[indiceClicado] || '').replace(/<[^>]*>/g, '');

      if (legSaidas)    legSaidas.innerHTML    = '<b>Saídas (' + rotX + '): R$ ' + formatarMoedaJS(valS) + '</b>';
      if (legEntradas)  legEntradas.innerHTML  = '<b>Entradas (' + rotX + '): R$ ' + formatarMoedaJS(valE) + '</b>';
      if (legDiferenca) legDiferenca.innerHTML = '<b>Diferença (' + rotX + '): ' + sinal + 'R$ ' + formatarMoedaJS(Math.abs(dif)) + '</b>';
      if (quadDiferenca) quadDiferenca.style.background = corD;
    } else {
      const nomeE   = Array.isArray(d.e_nome)   ? d.e_nome[0]   : d.e_nome;
      const nomeS   = Array.isArray(d.s_nome)   ? d.s_nome[0]   : d.s_nome;
      const nomeDif = Array.isArray(d.dif_nome) ? d.dif_nome[0] : d.dif_nome;
      const corDif  = Array.isArray(d.dif_cor)  ? d.dif_cor[0]  : d.dif_cor;

      if (legEntradas)  legEntradas.innerHTML  = nomeE;
      if (legSaidas)    legSaidas.innerHTML    = nomeS;
      if (legDiferenca) legDiferenca.innerHTML = nomeDif;
      if (quadDiferenca) quadDiferenca.style.background = corDif;
    }
  }

  function atualizarGrafico() {
    const d = DADOS[mesAtual] && DADOS[mesAtual][usuarioAtual];
    if (!d) return;

    const isMobile  = window.innerWidth <= 768;
    const ehBarra   = (modoAtual === 'barras');
    const ehLinha   = (modoAtual === 'linhas');
    const ehBox     = (modoAtual === 'boxplot');
    const cats      = obterCategoriasAtuais(d);
    const qtdPontos = cats.length || 1;

    const maxBar    = Array.isArray(d.max_bar)  ? d.max_bar[0]  : d.max_bar;
    const maxLine   = Array.isArray(d.max_line) ? d.max_line[0] : d.max_line;
    const maxBox    = Array.isArray(d.max_box)  ? d.max_box[0]  : d.max_box;
    const maxY      = ehBarra ? maxBar : (ehBox ? maxBox : maxLine);

    atualizarRotulosTopo(d);
    atualizarCardInterativo(d);

    const tamRotulo = isMobile ? 8.5 : ", tamanho_rotulos, ";
    const tamEixoX  = isMobile ? (qtdPontos > 6 ? 8.5 : 10) : ", tamanho_eixo_x, ";
    const tamEixoY  = isMobile ? 9 : 11;
    const alturaAtu = isMobile ? ", altura_grafico_mobile, " : ", altura_grafico, ";
    const raioAtu   = isMobile ? 5 : ", raio_cantos, ";

    // Sombra de seleção bem clarinha no fundo + linhas da média no Boxplot
    const shapesLayout = [];
    if (indiceClicado !== null && indiceClicado < qtdPontos) {
      const meiaLargura = ehLinha ? (qtdPontos > 8 ? 0.34 : 0.22) : 0.44;
      shapesLayout.push({
        type: 'rect',
        xref: 'x',
        yref: 'paper',
        x0: indiceClicado - meiaLargura,
        x1: indiceClicado + meiaLargura,
        y0: 0,
        y1: 1,
        fillcolor: '", cor_sombra_selecao, "',
        line: { width: 0 },
        layer: 'below'
      });
    }

    let tracesFinais = [];

    if (ehBox) {
      // Mapa de categorias para índice numérico (permite posicionar pontos, outliers e média com precisão)
      const mapaCat = {};
      cats.forEach(function(c, idx) { mapaCat[c] = idx; });

      const xBoxE = (d.box_e_x || []).map(function(c) { return (mapaCat[c] !== undefined ? mapaCat[c] : 0) - 0.19; });
      const xBoxS = (d.box_s_x || []).map(function(c) { return (mapaCat[c] !== undefined ? mapaCat[c] : 0) + 0.19; });

      // Linhas tracejadas da Média com cor própria diferenciada (#d97706)
      const qtdMedE = (d.med_e_y || []).length;
      for (let i = 0; i < qtdMedE; i++) {
        shapesLayout.push({
          type: 'line',
          xref: 'x',
          yref: 'y',
          x0: d.med_e_x0[i],
          x1: d.med_e_x1[i],
          y0: d.med_e_y[i],
          y1: d.med_e_y[i],
          line: { color: '", cor_media_box, "', width: 2.2, dash: 'dot' }
        });
      }

      const qtdMedS = (d.med_s_y || []).length;
      for (let i = 0; i < qtdMedS; i++) {
        shapesLayout.push({
          type: 'line',
          xref: 'x',
          yref: 'y',
          x0: d.med_s_x0[i],
          x1: d.med_s_x1[i],
          y0: d.med_s_y[i],
          y1: d.med_s_y[i],
          line: { color: '", cor_media_box, "', width: 2.2, dash: 'dot' }
        });
      }

      // 1. Caixas de Entradas e Saídas (hover da caixa só no PC; no mobile usa o Card embaixo)
      const traceBoxE = {
        type: 'box',
        x: xBoxE,
        y: d.box_e_y,
        name: 'Entradas',
        width: 0.26,
        boxpoints: false,
        fillcolor: 'rgba(41, 182, 216, 0.45)',
        line: { color: '", cor_entradas, "', width: 1.8 },
        hoverinfo: isMobile ? 'none' : 'y'
      };

      const traceBoxS = {
        type: 'box',
        x: xBoxS,
        y: d.box_s_y,
        name: 'Saídas',
        width: 0.26,
        boxpoints: false,
        fillcolor: 'rgba(27, 120, 161, 0.45)',
        line: { color: '", cor_saidas, "', width: 1.8 },
        hoverinfo: isMobile ? 'none' : 'y'
      };

      // 2. Pontos individuais com cores diferentes da caixa + Outliers em Vermelho com rótulo à direita + Hover Dia/Valor!
      const tracePtsE = {
        type: 'scatter',
        mode: isMobile ? 'markers' : 'markers+text',
        x: d.pts_e_x,
        y: d.pts_e_y,
        text: d.pts_e_lbl,
        textposition: 'middle right',
        textfont: { family: 'Inter, sans-serif', size: isMobile ? 8 : 9.5, color: '", cor_outlier, "' },
        cliponaxis: false,
        marker: {
          color: d.pts_e_cor,
          size: d.pts_e_tam,
          line: { color: '#ffffff', width: 1 }
        },
        hoverinfo: isMobile ? 'none' : 'text',
        hovertext: d.pts_e_hov
      };

      const tracePtsS = {
        type: 'scatter',
        mode: isMobile ? 'markers' : 'markers+text',
        x: d.pts_s_x,
        y: d.pts_s_y,
        text: d.pts_s_lbl,
        textposition: 'middle right',
        textfont: { family: 'Inter, sans-serif', size: isMobile ? 8 : 9.5, color: '", cor_outlier, "' },
        cliponaxis: false,
        marker: {
          color: d.pts_s_cor,
          size: d.pts_s_tam,
          line: { color: '#ffffff', width: 1 }
        },
        hoverinfo: isMobile ? 'none' : 'text',
        hovertext: d.pts_s_hov
      };

      // 3. Rótulos da Média na direita de cada Boxplot
      const traceMedE = {
        type: 'scatter',
        mode: 'text',
        x: d.med_e_xr,
        y: d.med_e_y,
        text: isMobile ? d.med_e_y.map(function() { return ''; }) : d.med_e_txt,
        textposition: 'middle right',
        textfont: { family: 'Inter, sans-serif', size: 9, color: '", cor_media_box, "' },
        cliponaxis: false,
        hoverinfo: isMobile ? 'none' : 'text',
        hovertext: d.med_e_hov
      };

      const traceMedS = {
        type: 'scatter',
        mode: 'text',
        x: d.med_s_xr,
        y: d.med_s_y,
        text: isMobile ? d.med_s_y.map(function() { return ''; }) : d.med_s_txt,
        textposition: 'middle right',
        textfont: { family: 'Inter, sans-serif', size: 9, color: '", cor_media_box, "' },
        cliponaxis: false,
        hoverinfo: isMobile ? 'none' : 'text',
        hovertext: d.med_s_hov
      };

      tracesFinais = [traceBoxE, traceBoxS, tracePtsE, tracePtsS, traceMedE, traceMedS];

    } else {
      // MODOS 1 E 2: BARRAS EMPILHADAS E LINHAS
      const opacidadeArr = [];
      const coresMarkerE = [];
      const coresMarkerS = [];
      const tamMarkerArr = [];

      for (let i = 0; i < qtdPontos; i++) {
        if (indiceClicado === null) {
          opacidadeArr.push(1);
          coresMarkerE.push('", cor_entradas, "');
          coresMarkerS.push('", cor_saidas, "');
          tamMarkerArr.push(isMobile ? 6 : 7);
        } else if (i === indiceClicado) {
          opacidadeArr.push(1);
          coresMarkerE.push('#000000');
          coresMarkerS.push('#000000');
          tamMarkerArr.push(isMobile ? 7.5 : 8.5);
        } else {
          opacidadeArr.push(0.32);
          coresMarkerE.push('", cor_entradas, "');
          coresMarkerS.push('", cor_saidas, "');
          tamMarkerArr.push(isMobile ? 5.5 : 6.5);
        }
      }

      const posLinhasE = [];
      const posLinhasS = [];
      const txtLinhasE = [];
      const txtLinhasS = [];

      for (let i = 0; i < qtdPontos; i++) {
        const valE = d.e_y[i] || 0;
        const valS = d.s_y[i] || 0;
        const rawE = d.e_txt_line[i] || '';
        const rawS = d.s_txt_line[i] || '';

        if (valE >= valS) {
          posLinhasE.push('top center');
          txtLinhasE.push(rawE ? (rawE + '<br> ') : '');
          if (valS > 0 && Math.abs(valE - valS) < (maxLine * 0.14)) {
            posLinhasS.push('bottom center');
            txtLinhasS.push(rawS ? (' <br>' + rawS) : '');
          } else {
            posLinhasS.push('top center');
            txtLinhasS.push(rawS ? (rawS + '<br> ') : '');
          }
        } else {
          posLinhasS.push('top center');
          txtLinhasS.push(rawS ? (rawS + '<br> ') : '');
          if (valE > 0 && Math.abs(valS - valE) < (maxLine * 0.14)) {
            posLinhasE.push('bottom center');
            txtLinhasE.push(rawE ? (' <br>' + rawE) : '');
          } else {
            posLinhasE.push('top center');
            txtLinhasE.push(rawE ? (rawE + '<br> ') : '');
          }
        }
      }

      const ocultarTxtMobile = (isMobile && qtdPontos > 10);

      const markerEntradas = ehBarra ? {
        color: '", cor_entradas, "',
        opacity: opacidadeArr,
        line: { width: 0 }
      } : {
        color: coresMarkerE,
        size: tamMarkerArr,
        opacity: opacidadeArr
      };

      const markerSaidas = ehBarra ? {
        color: '", cor_saidas, "',
        opacity: opacidadeArr,
        line: { width: 0 }
      } : {
        color: coresMarkerS,
        size: tamMarkerArr,
        opacity: opacidadeArr
      };

      // No Mobile: hoverinfo = 'none' para nunca abrir o balão em cima do gráfico!
      const traceEntradas = {
        x: d.x,
        y: d.e_y,
        type: ehBarra ? 'bar' : 'scatter',
        mode: ehBarra ? undefined : 'lines+markers+text',
        marker: markerEntradas,
        line: { color: '", cor_entradas, "', width: isMobile ? 1.5 : 1.8 },
        text: ocultarTxtMobile ? d.e_y.map(function() { return ''; }) : (ehBarra ? d.e_txt_bar : txtLinhasE),
        textposition: ehBarra ? 'inside' : posLinhasE,
        insidetextanchor: 'middle',
        insidetextorientation: 'horizontal',
        constraintext: 'none',
        cliponaxis: false,
        textfont: {
          family: 'Inter, sans-serif',
          size: tamRotulo,
          color: ehBarra ? '#ffffff' : '", cor_entradas, "'
        },
        hoverinfo: isMobile ? 'none' : 'text',
        hovertext: d.e_hover
      };

      const traceSaidas = {
        x: d.x,
        y: d.s_y,
        type: ehBarra ? 'bar' : 'scatter',
        mode: ehBarra ? undefined : 'lines+markers+text',
        marker: markerSaidas,
        line: { color: '", cor_saidas, "', width: isMobile ? 1.5 : 1.8 },
        text: ocultarTxtMobile ? d.s_y.map(function() { return ''; }) : (ehBarra ? d.s_txt_bar : txtLinhasS),
        textposition: ehBarra ? 'outside' : posLinhasS,
        constraintext: 'none',
        cliponaxis: false,
        textfont: {
          family: 'Inter, sans-serif',
          size: tamRotulo,
          color: ehBarra ? '#000000' : '", cor_saidas, "'
        },
        hoverinfo: isMobile ? 'none' : 'text',
        hovertext: d.s_hover
      };

      tracesFinais = [traceEntradas, traceSaidas];
    }

    const minY = ehBarra ? 0 : -(maxY * 0.06);
    const larguraContainer = painel.clientWidth || window.innerWidth;

    const eixoXConfig = ehBox ? {
      title: '',
      type: 'linear',
      tickmode: 'array',
      tickvals: cats.map(function(_, idx) { return idx; }),
      ticktext: cats,
      range: [-0.55, (cats.length - 1) + 0.65],
      showgrid: false,
      zeroline: false,
      fixedrange: true,
      tickangle: 0,
      tickfont: { family: 'Inter, sans-serif', size: tamEixoX, color: '", cor_eixo_x, "' }
    } : {
      title: '',
      type: 'category',
      categoryorder: 'array',
      categoryarray: cats,
      showgrid: false,
      zeroline: false,
      fixedrange: true,
      tickangle: (isMobile && qtdPontos > 8) ? -45 : 0,
      tickfont: { family: 'Inter, sans-serif', size: tamEixoX, color: '", cor_eixo_x, "' }
    };

    const novoLayout = Object.assign({}, el.layout, {
      width: larguraContainer,
      height: alturaAtu,
      barmode: 'stack',
      barcornerradius: raioAtu,
      bargap: isMobile ? (qtdPontos > 6 ? 0.20 : 0.35) : (qtdPontos > 10 ? 0.22 : 0.32),
      uniformtext: { mode: 'show', minsize: tamRotulo },
      shapes: shapesLayout,
      hovermode: isMobile ? false : 'closest',
      hoverlabel: {
        bgcolor: '", hover_bg_cor, "',
        bordercolor: '", borda_balao_hover, "',
        align: 'left',
        font: {
          family: 'Inter, sans-serif',
          size: ", hover_fonte_tam, ",
          color: '", hover_txt_cor, "'
        }
      },
      margin: isMobile
        ? { t: 22, b: qtdPontos > 8 ? 38 : 30, l: 44, r: 14 }
        : { t: 28, b: 42, l: 62, r: ehBox ? 45 : 15 },
      xaxis: eixoXConfig,
      yaxis: Object.assign({}, el.layout.yaxis, {
        range: [minY, maxY],
        tickfont: { family: 'Inter, sans-serif', size: tamEixoY, color: '#444444' }
      })
    });

    el.style.width  = '100%';
    el.style.height = alturaAtu + 'px';
    Plotly.react(el, tracesFinais, novoLayout, {displayModeBar: false, responsive: true});
  }

  el.on('plotly_click', function(data) {
    if (!data || !data.points || !data.points.length) return;
    const d    = DADOS[mesAtual] && DADOS[mesAtual][usuarioAtual];
    const cats = obterCategoriasAtuais(d);
    const pt   = data.points[0];
    const crv  = (pt.curveNumber !== undefined) ? (pt.curveNumber % 2) : 0;

    let idx = -1;
    if (modoAtual === 'boxplot' && typeof pt.x === 'number') {
      idx = Math.round(pt.x);
    } else {
      idx = cats.indexOf(pt.x);
      if (idx === -1 && typeof pt.pointIndex === 'number') {
        idx = pt.pointIndex;
      }
    }
    if (idx < 0 || idx >= cats.length) return;

    if (indiceClicado === idx && serieClicada === crv) {
      indiceClicado = null;
    } else {
      indiceClicado = idx;
      serieClicada  = crv;
    }
    atualizarGrafico();
  });

  el.addEventListener('click', function(ev) {
    const tickNode = ev.target.closest('.xtick');
    if (!tickNode) return;
    const todosTicks = Array.from(el.querySelectorAll('.xtick'));
    const idx = todosTicks.indexOf(tickNode);
    if (idx !== -1) {
      indiceClicado = (indiceClicado === idx) ? null : idx;
      atualizarGrafico();
    }
  });

  if (selMes) {
    selMes.addEventListener('change', function() {
      mesAtual = this.value;
      indiceClicado = null;
      atualizarGrafico();
    });
  }

  botoesUsuario.forEach(function(btn) {
    btn.addEventListener('click', function() {
      const clicado = this.getAttribute('data-usuario');
      if (usuarioAtual === clicado && clicado !== 'Todos') {
        usuarioAtual = 'Todos';
      } else {
        usuarioAtual = clicado;
      }
      indiceClicado = null;
      botoesUsuario.forEach(function(b) {
        b.classList.toggle('ativo', b.getAttribute('data-usuario') === usuarioAtual);
      });
      atualizarGrafico();
    });
  });

  botoesModo.forEach(function(btn) {
    btn.addEventListener('click', function() {
      modoAtual = this.getAttribute('data-modo');
      indiceClicado = null;
      botoesModo.forEach(function(b) { b.classList.remove('ativo'); });
      this.classList.add('ativo');
      atualizarGrafico();
    });
  });

  atualizarGrafico();
  setTimeout(atualizarGrafico, 120);

  let timerResize;
  window.addEventListener('resize', function() {
    clearTimeout(timerResize);
    timerResize = setTimeout(atualizarGrafico, 120);
  });
}
")

grafico <- plotly::plot_ly(height = altura_grafico) %>%
  plotly::add_bars(
    x                     = estado_ini$x,
    y                     = estado_ini$e_y,
    name                  = estado_ini$e_nome[1],
    marker                = base::list(color = cor_entradas),
    text                  = estado_ini$e_txt_bar,
    textposition          = "inside",
    insidetextanchor      = "middle",
    insidetextorientation = "horizontal",
    constraintext         = "none",
    cliponaxis            = FALSE,
    textfont              = base::list(family = "Inter, sans-serif", size = tamanho_rotulos, color = "#ffffff"),
    hoverinfo             = "text",
    hovertext             = estado_ini$e_hover
  ) %>%
  plotly::add_bars(
    x                     = estado_ini$x,
    y                     = estado_ini$s_y,
    name                  = estado_ini$s_nome[1],
    marker                = base::list(color = cor_saidas),
    text                  = estado_ini$s_txt_bar,
    textposition          = "outside",
    constraintext         = "none",
    cliponaxis            = FALSE,
    textfont              = base::list(family = "Inter, sans-serif", size = tamanho_rotulos, color = cor_rotulos),
    hoverinfo             = "text",
    hovertext             = estado_ini$s_hover
  ) %>%
  plotly::layout(
    autosize        = TRUE,
    font            = base::list(family = "Inter, sans-serif"),
    separators      = ",.",
    barmode         = "stack",
    barcornerradius = raio_cantos,
    bargap          = 0.28,
    uniformtext     = base::list(mode = "show", minsize = tamanho_rotulos),
    
    hoverlabel = base::list(
      bgcolor     = hover_bg_cor,
      font        = base::list(family = "Inter, sans-serif", size = hover_fonte_tam, color = hover_txt_cor),
      bordercolor = borda_balao_hover,
      align       = "left"
    ),
    
    xaxis = base::list(
      title         = "",
      type          = "category",
      categoryorder = "array",
      categoryarray = estado_ini$x,
      showgrid      = FALSE,
      fixedrange    = TRUE,
      tickangle     = 0,
      tickfont      = base::list(family = "Inter, sans-serif", size = tamanho_eixo_x, color = cor_eixo_x)
    ),
    
    yaxis = base::list(
      title         = "",
      tickprefix    = "R$ ",
      tickformat    = ",.0f",
      showgrid      = TRUE,
      gridcolor     = "#e9ecef",
      zeroline      = TRUE,
      zerolinecolor = "#cccccc",
      fixedrange    = TRUE,
      range         = base::list(0, estado_ini$max_bar[1]),
      tickfont      = base::list(family = "Inter, sans-serif", size = 11, color = "#444444")
    ),
    
    showlegend    = FALSE,
    margin        = base::list(t = 28, b = 42, l = 62, r = 12),
    plot_bgcolor  = "white",
    paper_bgcolor = "white"
  ) %>%
  plotly::config(displayModeBar = FALSE, responsive = TRUE) %>%
  htmlwidgets::onRender(js_callback)


# ==============================================================================
# 8. ESTILO CSS E ESTRUTURA HTML (CARD UNIFICADO PC/MOBILE + HOVER ARREDONDADO)
# ==============================================================================

estilo_controles_fluxo <- htmltools::tags$style(base::paste0("
  .painel-fluxo-alf {
    max-width: 950px;
    width: 100%;
    margin: 20px auto;
    font-family: 'Inter', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    background: #ffffff;
    box-sizing: border-box;
  }
  .painel-fluxo-alf .grafico-scroll,
  .painel-fluxo-alf .html-widget,
  .painel-fluxo-alf .plotly,
  .painel-fluxo-alf .js-plotly-plot,
  .painel-fluxo-alf .plot-container,
  .painel-fluxo-alf .svg-container {
    width: 100% !important;
    max-width: 100% !important;
    min-width: 0 !important;
    overflow: visible !important;
    box-sizing: border-box !important;
  }
  /* Deixa o balão de hover do PC com bordas arredondadas e sombra igual ao Card do Mobile */
  .painel-fluxo-alf .hoverlayer .hovertext path {
    rx: 8px !important;
    ry: 8px !important;
    stroke-width: 1.5px !important;
    filter: drop-shadow(0 4px 10px rgba(0,0,0,0.10));
  }
  .painel-fluxo-alf .xtick,
  .painel-fluxo-alf .bars path,
  .painel-fluxo-alf .box {
    cursor: pointer !important;
  }
  .barra-controles-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 12px 16px;
    margin-bottom: 16px;
    background: #fcfcfc;
    border: 1px solid #e0e0e0;
    border-top: 2px solid #000000;
    border-radius: 0 0 10px 10px;
  }
  .cabecalho-grafico-alf {
    text-align: left;
    padding: 0 2px;
    margin-bottom: 6px;
  }
  .titulo-grafico-alf {
    color: ", titulo_cor, ";
    font-size: ", titulo_tamanho, "px;
    font-weight: 700;
    margin: 0 0 10px 0;
    line-height: 1.25;
  }
  .legenda-custom-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 18px;
    margin: 0;
    padding: 0;
  }
  .item-legenda-alf {
    display: inline-flex;
    align-items: center;
    gap: 7px;
    font-size: ", tamanho_legenda, "px;
    color: #000000;
  }
  .quadrado-legenda-alf {
    width: 13px;
    height: 13px;
    border-radius: 2px;
    display: inline-block;
    flex-shrink: 0;
  }
  .grupo-controle-alf {
    display: flex;
    align-items: center;
    gap: 8px;
  }
  .rotulo-controle-alf {
    font-size: 12px;
    font-weight: 600;
    color: #333333;
    margin-right: 2px;
  }
  .select-mes-alf {
    font-family: inherit;
    font-size: 12.5px;
    font-weight: 500;
    color: #111111;
    background-color: #ffffff;
    border: 1px solid #cccccc;
    border-radius: 6px;
    padding: 6px 12px;
    outline: none;
    cursor: pointer;
    transition: all 0.2s ease;
  }
  .select-mes-alf:hover, .select-mes-alf:focus {
    border-color: #000000;
    box-shadow: 0 2px 4px rgba(0,0,0,0.06);
  }
  .btn-usuario-alf {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    border: 2px solid transparent;
    background: transparent;
    padding: 2px;
    border-radius: 50px;
    cursor: pointer;
    transition: all 0.2s ease;
  }
  .btn-usuario-alf.texto-todos {
    padding: 5px 13px;
    font-size: 12px;
    font-weight: 600;
    color: #222222;
    border: 1px solid #d0d0d0;
    border-radius: 20px;
    background: #ffffff;
  }
  .btn-usuario-alf.texto-todos:hover {
    background: #000000;
    color: #ffffff;
    border-color: #000000;
    transform: translateY(-1px);
  }
  .btn-usuario-alf.texto-todos.ativo {
    background: #000000;
    color: #ffffff;
    border-color: #000000;
    box-shadow: 0 2px 5px rgba(0,0,0,0.18);
  }
  .btn-usuario-alf img {
    width: 36px;
    height: 36px;
    border-radius: 50%;
    object-fit: cover;
    border: 2px solid #e0e0e0;
    transition: all 0.2s ease;
    filter: grayscale(20%);
  }
  .btn-usuario-alf:hover img {
    transform: scale(1.12);
    filter: grayscale(0%);
    border-color: #555555;
  }
  .btn-usuario-alf.ativo img {
    border-color: #000000;
    box-shadow: 0 0 0 2px #000000;
    transform: scale(1.08);
    filter: grayscale(0%);
  }
  .btn-modo-alf {
    font-family: inherit;
    font-size: 12px;
    font-weight: 500;
    color: #222222;
    background: #ffffff;
    border: 1px solid #d0d0d0;
    border-radius: 6px;
    padding: 6px 11px;
    cursor: pointer;
    transition: all 0.2s ease;
    display: inline-flex;
    align-items: center;
    gap: 5px;
  }
  .btn-modo-alf:hover:not(.ativo) {
    background: #000000;
    color: #ffffff;
    border-color: #000000;
    transform: translateY(-1px);
  }
  .btn-modo-alf.ativo {
    background: #000000;
    color: #ffffff;
    font-weight: 600;
    border-color: #000000;
    box-shadow: 0 2px 5px rgba(0,0,0,0.15);
  }

  /* Card Interativo de Detalhes (usado no Mobile e ao clicar em uma coluna no PC!) */
  .card-hover-mobile-alf {
    display: none;
    width: 100%;
    box-sizing: border-box;
    margin-top: 10px;
    padding: 12px 14px;
    background: #ffffff;
    border: 1.5px solid #000000;
    border-radius: 10px;
    font-size: 12px;
    line-height: 1.5;
    color: #000000;
    text-align: left;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    word-wrap: break-word;
    overflow-wrap: break-word;
  }
  .topo-card-mobile-alf {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 8px;
    padding-bottom: 6px;
    border-bottom: 1px solid #eaeaea;
  }
  .abas-card-mobile-alf {
    display: inline-flex;
    gap: 6px;
  }
  .aba-mob-alf {
    font-family: inherit;
    font-size: 11.5px;
    font-weight: 600;
    padding: 4px 12px;
    border-radius: 5px;
    border: 1px solid #cccccc;
    background: #f8f9fa;
    color: #333333;
    cursor: pointer;
    transition: all 0.15s ease;
  }
  .aba-mob-alf.ativa-e {
    background: ", cor_entradas, ";
    border-color: ", cor_entradas, ";
    color: #ffffff;
  }
  .aba-mob-alf.ativa-s {
    background: ", cor_saidas, ";
    border-color: ", cor_saidas, ";
    color: #ffffff;
  }
  .fechar-card-mobile-alf {
    background: transparent;
    border: none;
    font-size: 19px;
    line-height: 1;
    color: #555555;
    cursor: pointer;
    padding: 0 4px;
  }

  @media (max-width: 768px) {
    .painel-fluxo-alf {
      margin: 12px auto;
    }
    .barra-controles-alf {
      padding: 9px 11px;
      gap: 7px;
      margin-bottom: 10px;
    }
    .grupo-controle-alf {
      width: 100%;
      justify-content: space-between;
      gap: 6px;
    }
    .rotulo-controle-alf {
      font-size: 11px;
    }
    .select-mes-alf {
      font-size: 11.5px;
      padding: 4px 8px;
      flex: 1;
      max-width: 175px;
    }
    .btn-usuario-alf.texto-todos {
      padding: 4px 10px;
      font-size: 11px;
    }
    .btn-usuario-alf img {
      width: 29px;
      height: 29px;
    }
    .btn-modo-alf {
      font-size: 10.5px;
      padding: 4px 7px;
    }
    .titulo-grafico-alf {
      font-size: 13.5px;
      margin-bottom: 8px;
    }
    .legenda-custom-alf {
      flex-direction: column;
      align-items: flex-start;
      gap: 5px;
    }
    .item-legenda-alf {
      font-size: 11.5px;
      gap: 6px;
    }
    .quadrado-legenda-alf {
      width: 11px;
      height: 11px;
    }
    .card-hover-mobile-alf {
      font-size: 11.5px;
      padding: 10px 12px;
    }
  }
"))

opcoes_html_mes <- purrr::map(meses_disponiveis, function(m) {
  htmltools::tags$option(value = m, m)
})

controles_html <- htmltools::tags$div(
  class = "barra-controles-alf",
  
  # 1. Seletor de Período
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-calendar3'></i> Período:")),
    htmltools::tags$select(class = "select-mes-alf", opcoes_html_mes)
  ),
  
  # 2. Seletor de Usuário (Todos + Fotos Redondas)
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-people'></i> Usuário:")),
    htmltools::tags$div(
      style = "display: inline-flex; align-items: center; gap: 6px;",
      htmltools::tags$button(type = "button", class = "btn-usuario-alf texto-todos ativo", `data-usuario` = "Todos", "Todos"),
      htmltools::tags$button(type = "button", class = "btn-usuario-alf", `data-usuario` = "Alex", htmltools::tags$img(src = uri_alex, alt = "Alex")),
      htmltools::tags$button(type = "button", class = "btn-usuario-alf", `data-usuario` = "Leticia", htmltools::tags$img(src = uri_leticia, alt = "Letícia"))
    )
  ),
  
  # 3. Seletor de Visão (3 Gráficos: Empilhado, Linhas e Boxplot!)
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-bar-chart-line'></i> Visão:")),
    htmltools::tags$div(
      style = "display: inline-flex; align-items: center; gap: 5px; flex-wrap: wrap;",
      htmltools::tags$button(
        type = "button",
        class = "btn-modo-alf ativo",
        `data-modo` = "barras",
        htmltools::HTML("<i class='bi bi-bar-chart-steps'></i> Empilhado")
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn-modo-alf",
        `data-modo` = "linhas",
        htmltools::HTML("<i class='bi bi-graph-up'></i> Linhas")
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn-modo-alf",
        `data-modo` = "boxplot",
        htmltools::HTML("<i class='bi bi-distribute-vertical'></i> Boxplot")
      )
    )
  )
)

cabecalho_html <- htmltools::tags$div(
  class = "cabecalho-grafico-alf",
  htmltools::tags$div(class = "titulo-grafico-alf", titulo_nome),
  htmltools::tags$div(
    class = "legenda-custom-alf",
    htmltools::tags$span(
      class = "item-legenda-alf",
      htmltools::tags$span(class = "quadrado-legenda-alf", style = base::paste0("background:", cor_saidas, ";")),
      htmltools::tags$span(class = "leg-texto-saidas", htmltools::HTML(estado_ini$s_nome[1]))
    ),
    htmltools::tags$span(
      class = "item-legenda-alf",
      htmltools::tags$span(class = "quadrado-legenda-alf", style = base::paste0("background:", cor_entradas, ";")),
      htmltools::tags$span(class = "leg-texto-entradas", htmltools::HTML(estado_ini$e_nome[1]))
    ),
    htmltools::tags$span(
      class = "item-legenda-alf",
      htmltools::tags$span(class = "quadrado-legenda-alf quadrado-diferenca", style = base::paste0("background:", estado_ini$dif_cor[1], ";")),
      htmltools::tags$span(class = "leg-texto-diferenca", htmltools::HTML(estado_ini$dif_nome[1]))
    )
  )
)

# ==============================================================================
# 9. OBJETO FINAL
# ==============================================================================

grafico_fluxo <- htmltools::tagList(
  estilo_controles_fluxo,
  htmltools::tags$div(
    class = "painel-fluxo-alf",
    controles_html,
    cabecalho_html,
    htmltools::tags$div(class = "grafico-scroll", grafico),
    htmltools::tags$div(class = "card-hover-mobile-alf")
  )
)

grafico_fluxo