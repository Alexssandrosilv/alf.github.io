# ==============================================================================
# PROJETO ALF — Painel de Gráficos: Empilhado, Linhas e Boxplot (Versão Final)
# ==============================================================================
# Ajustes implementados:
#   1. Ordenação Hierárquica Inteligente ao clicar nos cabeçalhos da tabela
#      (ordena valores e textos preservando e reconstruindo o rowspan automaticamente,
#      sem setas visuais, com cursor pointer e mantendo a linha Total fixa no rodapé!).
#   2. KPIs minimalistas integrados dentro da barra de filtros.
#   3. Ícones Bootstrap nos cabeçalhos + "%" sem parênteses + Total por Item de Consumo.
#   4. Tabela de Resumo Estatístico compacta e sem sombra/caixa externa duplicada.

# ==============================================================================
# 1. CONFIGURAÇÕES VISUAIS (PADRÃO ALEX + PALETA AZUL/CIANO)
# ==============================================================================

titulo_nome     <- "Fluxo Financeiro: Entradas e Saídas por data de pagamento"
titulo_cor      <- "#03045e"
titulo_tamanho  <- 16

# Cores das séries (Caixas / Barras / Linhas)
cor_entradas       <- "#29b6d8"   # Azul Ciano Vibrante
cor_saidas         <- "#1b78a1"   # Azul Oceano Escuro

# Cores para os elementos do Boxplot
cor_ponto_entradas <- "#0077b6"   # Azul médio para pontos de Entrada
cor_ponto_saidas   <- "#023e8a"   # Azul escuro para pontos de Saída
cor_outlier        <- "#e11d48"   # Coral/Carmim elegante para Outliers
cor_media_box      <- "#03045e"   # Azul Marinho Profundo para a Média

# Cor da sombra de seleção no fundo (bem clarinha)
cor_sombra_selecao <- "rgba(226, 232, 240, 0.50)"

tamanho_rotulos <- 10.5
cor_rotulos     <- "#000000"
tamanho_eixo_x  <- 11
cor_eixo_x      <- "#000000"
tamanho_legenda <- 12.5

# Balão de Hover
hover_bg_cor      <- "#FFFFFF"
hover_txt_cor     <- "#000000"
hover_fonte_tam   <- 12
borda_balao_hover <- "#000000"

raio_cantos           <- 8
altura_grafico        <- 440 # Altura no Notebook/PC
altura_grafico_mobile <- 295 # Altura compacta no Celular

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

# Formata a célula de moeda igual à sua tabela DT (R$ na esquerda e número na direita)
formatar_celula_moeda <- function(x, negrito = FALSE) {
  peso <- if (negrito) "font-weight: 700;" else ""
  base::paste0(
    "<div class='celula-moeda-alf' style='", peso, "'>",
    "<span>R$</span><span>", formatar_brl(x), "</span>",
    "</div>"
  )
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

# Calcula Q1, Mediana (Q2) e Q3 com o mesmo método linear do Plotly (type = 5)
calcular_quartis_plotly <- function(vals) {
  v <- base::sort(stats::na.omit(base::as.numeric(vals)))
  n <- base::length(v)
  if (n == 0) return(base::list(q1 = 0, med = 0, q3 = 0, iiq = 0))
  q1  <- stats::quantile(v, 0.25, type = 5, names = FALSE)
  med <- stats::quantile(v, 0.50, type = 5, names = FALSE)
  q3  <- stats::quantile(v, 0.75, type = 5, names = FALSE)
  base::list(q1 = q1, med = med, q3 = q3, iiq = q3 - q1)
}

entradas_fluxo <- bd_entradas %>%
  dplyr::filter(!base::is.na(data)) %>%
  dplyr::transmute(
    tipo         = "Entradas",
    data         = base::as.Date(data),
    valor        = base::as.numeric(valor),
    usuario      = padronizar_usuario(usuario_id),
    grupo_1      = dplyr::coalesce(base::as.character(fonte_renda), "Outras Fontes"),
    grupo_2      = dplyr::coalesce(base::as.character(descricao), "Sem descrição"),
    item_consumo = "—",
    descricao    = dplyr::coalesce(base::as.character(descricao), "Sem descrição"),
    status       = "Recebido"
  )

saidas_fluxo <- bd_saidas %>%
  dplyr::filter(!base::is.na(data_pagamento)) %>%
  dplyr::transmute(
    tipo         = "Saídas",
    data         = base::as.Date(data_pagamento),
    valor        = base::as.numeric(valor),
    usuario      = padronizar_usuario(usuario_id),
    grupo_1      = dplyr::coalesce(base::as.character(forma_pagamento), "Outros"),
    grupo_2      = dplyr::coalesce(base::as.character(tipo_despesa), "variáveis"),
    item_consumo = dplyr::coalesce(base::as.character(item_consumo), "Outros"),
    descricao    = dplyr::coalesce(base::as.character(descricao), "Sem descrição"),
    status       = dplyr::coalesce(base::as.character(status), "A pagar")
  )

if (!incluir_a_pagar) {
  saidas_fluxo <- saidas_fluxo %>% dplyr::filter(status == "Pago")
}

fluxo <- dplyr::bind_rows(entradas_fluxo, saidas_fluxo) %>%
  dplyr::mutate(
    grupo_1      = base::ifelse(base::trimws(grupo_1) == "", "Não informado", grupo_1),
    grupo_2      = base::ifelse(base::trimws(grupo_2) == "", "Sem descrição", grupo_2),
    item_consumo = base::ifelse(base::trimws(item_consumo) == "", "Outros", item_consumo),
    descricao    = base::ifelse(base::trimws(descricao) == "", "Sem descrição", descricao),
    mes_num      = base::as.integer(base::format(data, "%m")),
    ano_num      = base::as.integer(base::format(data, "%Y")),
    mes_ref      = base::format(data, "%Y-%m"),
    mes_nome     = meses_pt[mes_num],
    mes_label    = base::paste0(mes_nome, "/", ano_num)
  )

tabela_meses <- fluxo %>%
  dplyr::distinct(mes_ref, mes_label, mes_nome) %>%
  dplyr::arrange(mes_ref)

meses_disponiveis <- base::c("Todos os Meses", tabela_meses$mes_label)
opcoes_usuario    <- base::c("Todos", "Alex", "Leticia")


# ==============================================================================
# 4. FUNÇÕES DE HOVER E TABELAS ORDENÁVEIS COM MESCLAGEM DINÂMICA (ROWSPAN)
# ==============================================================================

# 4.1 Hover flutuante (PC) para Barras e Linhas
montar_hover_inteligente <- function(df_sub, titulo_periodo, tipo_sel) {
  cor_titulo <- base::ifelse(tipo_sel == "Entradas", cor_entradas, cor_saidas)
  n_lanc     <- base::nrow(df_sub)
  
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0(
      "<b style='color:", cor_titulo, "; font-size:13px;'>",
      titulo_periodo, " | R$ 0,00 (n = 0)</b>"
    ))
  }
  
  val_total <- base::sum(df_sub$valor, na.rm = TRUE)
  val_media <- base::mean(df_sub$valor, na.rm = TRUE)
  val_max   <- base::max(df_sub$valor, na.rm = TRUE)
  val_min   <- base::min(df_sub$valor, na.rm = TRUE)
  val_amp   <- val_max - val_min
  
  cabecalho <- base::paste0(
    "<b style='color:", cor_titulo, "; font-size:13px;'>",
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
    utils::head(5) %>%
    dplyr::mutate(
      pct      = base::round((soma / val_total) * 100, 1),
      nome_lim = base::substr(grupo_1, 1, 34),
      linha    = base::paste0("• ", nome_lim, ": R$ ", formatar_brl(soma), " (n = ", qtd, " | ", pct, "%)")
    )
  bloco_1 <- base::paste0("<br><b>", rotulo_1, "</b><br>", base::paste(res_1$linha, collapse = "<br>"))
  
  res_2 <- df_sub %>%
    dplyr::group_by(grupo_2) %>%
    dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(soma)) %>%
    utils::head(5) %>%
    dplyr::mutate(
      pct      = base::round((soma / val_total) * 100, 1),
      nome_lim = base::substr(grupo_2, 1, 34),
      linha    = if (tipo_sel == "Entradas") {
        base::paste0("• ", nome_lim, ": R$ ", formatar_brl(soma), " (", pct, "%)")
      } else {
        base::paste0("• ", nome_lim, ": R$ ", formatar_brl(soma), " (n = ", qtd, " | ", pct, "%)")
      }
    )
  bloco_2 <- base::paste0("<br><b>", rotulo_2, "</b><br>", base::paste(res_2$linha, collapse = "<br>"))
  
  rodape_stats <- base::paste0(
    "<br><br><span style='color:#444444; font-size:11px'>",
    "<b>Média:</b> R$ ", formatar_brl(val_media),
    " | <b>Máx:</b> R$ ", formatar_brl(val_max),
    " | <b>Mín:</b> R$ ", formatar_brl(val_min),
    " | <b>Ampl.:</b> R$ ", formatar_brl(val_amp),
    "</span>"
  )
  
  base::paste0(cabecalho, bloco_1, bloco_2, rodape_stats)
}

# 4.2 Resumo Estatístico como Mini-Tabela Compacta
montar_tabela_metricas <- function(vals, extra_quartis = NULL) {
  val_media <- base::mean(vals, na.rm = TRUE)
  val_max   <- base::max(vals, na.rm = TRUE)
  val_min   <- base::min(vals, na.rm = TRUE)
  val_amp   <- val_max - val_min
  
  if (!base::is.null(extra_quartis)) {
    cab_cols <- base::paste0(
      "<th><i class='bi bi-distribute-horizontal'></i> Mediana</th>",
      "<th><i class='bi bi-bounding-box'></i> Q1 (25%)</th>",
      "<th><i class='bi bi-bounding-box'></i> Q3 (75%)</th>",
      "<th><i class='bi bi-arrows-expand'></i> IIQ</th>",
      "<th><i class='bi bi-calculator'></i> Média</th>",
      "<th><i class='bi bi-arrow-up-circle'></i> Máx</th>",
      "<th><i class='bi bi-arrow-down-circle'></i> Mín</th>",
      "<th><i class='bi bi-arrows-vertical'></i> Amplitude</th>"
    )
    val_cols <- base::paste0(
      "<td class='cel-primeira-col'><b>R$ ", formatar_brl(extra_quartis$med), "</b></td>",
      "<td>R$ ", formatar_brl(extra_quartis$q1), "</td>",
      "<td>R$ ", formatar_brl(extra_quartis$q3), "</td>",
      "<td>R$ ", formatar_brl(extra_quartis$iiq), "</td>",
      "<td><b>R$ ", formatar_brl(val_media), "</b></td>",
      "<td>R$ ", formatar_brl(val_max), "</td>",
      "<td>R$ ", formatar_brl(val_min), "</td>",
      "<td>R$ ", formatar_brl(val_amp), "</td>"
    )
  } else {
    cab_cols <- base::paste0(
      "<th><i class='bi bi-calculator'></i> Média</th>",
      "<th><i class='bi bi-arrow-up-circle'></i> Máx</th>",
      "<th><i class='bi bi-arrow-down-circle'></i> Mín</th>",
      "<th><i class='bi bi-arrows-vertical'></i> Amplitude</th>"
    )
    val_cols <- base::paste0(
      "<td class='cel-primeira-col'><b>R$ ", formatar_brl(val_media), "</b></td>",
      "<td>R$ ", formatar_brl(val_max), "</td>",
      "<td>R$ ", formatar_brl(val_min), "</td>",
      "<td>R$ ", formatar_brl(val_amp), "</td>"
    )
  }
  
  base::paste0(
    "<div class='secao-resumo-estatistico'>",
    "<div class='wrapper-tabela-dt-alf'>",
    "<table class='tabela-estilo-dt-alf tabela-metricas-compacta'>",
    "<thead><tr>", cab_cols, "</tr></thead>",
    "<tbody><tr class='linha-total-dt'>", val_cols, "</tr></tbody>",
    "</table>",
    "</div>",
    "</div>"
  )
}

# 4.3 Tabela Principal com atributos de ordenação hierárquica (permite clicar em qualquer coluna!)
montar_card_tabela_unificada <- function(df_sub, titulo_periodo, tipo_sel, incluir_quartis = FALSE) {
  n_lanc <- base::nrow(df_sub)
  
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return("<div class='sem-dados-card'>Nenhum lançamento registrado neste período.</div>")
  }
  
  val_total     <- base::sum(df_sub$valor, na.rm = TRUE)
  vals          <- df_sub$valor
  quartis_lista <- if (incluir_quartis) calcular_quartis_plotly(vals) else NULL
  bloco_resumo  <- montar_tabela_metricas(vals, extra_quartis = quartis_lista)
  
  if (tipo_sel == "Entradas") {
    # ENTRADAS: Fonte de Renda | Descrição | Valor | % | Total | %
    fontes_agr <- df_sub %>%
      dplyr::group_by(grupo_1) %>%
      dplyr::summarise(
        n_fonte   = dplyr::n(),
        tot_fonte = base::sum(valor, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(tot_fonte)) %>%
      dplyr::mutate(
        pct_fonte = base::formatC((tot_fonte / val_total) * 100, format = "f", digits = 1, decimal.mark = ".")
      )
    
    linhas_html <- base::character(0)
    
    for (idx_f in base::seq_len(base::nrow(fontes_agr))) {
      f_atual <- fontes_agr$grupo_1[idx_f]
      n_f     <- fontes_agr$n_fonte[idx_f]
      tot_f   <- fontes_agr$tot_fonte[idx_f]
      pct_f   <- fontes_agr$pct_fonte[idx_f]
      
      itens_f <- df_sub %>%
        dplyr::filter(grupo_1 == f_atual) %>%
        dplyr::group_by(descricao) %>%
        dplyr::summarise(
          val_item = base::sum(valor, na.rm = TRUE),
          qtd_item = dplyr::n(),
          .groups  = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(val_item)) %>%
        dplyr::mutate(
          pct_item = base::formatC((val_item / val_total) * 100, format = "f", digits = 1, decimal.mark = "."),
          desc_lbl = base::ifelse(qtd_item > 1, base::paste0(descricao, " <span class='badge-n'>(", qtd_item, ")</span>"), descricao)
        )
      
      k_rows <- base::nrow(itens_f)
      
      html_col_fonte <- htmltools::htmlEscape(base::paste0("<b>", f_atual, "</b> <span class='badge-n'>(", n_f, ")</span>"), attribute = TRUE)
      html_col_tot   <- htmltools::htmlEscape(formatar_celula_moeda(tot_f, TRUE), attribute = TRUE)
      html_col_pct   <- htmltools::htmlEscape(base::paste0("<b>", pct_f, "%</b>"), attribute = TRUE)
      
      for (j in base::seq_len(k_rows)) {
        html_col_desc <- htmltools::htmlEscape(itens_f$desc_lbl[j], attribute = TRUE)
        html_col_val  <- htmltools::htmlEscape(formatar_celula_moeda(itens_f$val_item[j], FALSE), attribute = TRUE)
        html_col_vpct <- htmltools::htmlEscape(base::paste0(itens_f$pct_item[j], "%"), attribute = TRUE)
        
        atributos_tr <- base::paste0(
          " class='linha-dados-sort'",
          " data-g1='", htmltools::htmlEscape(base::tolower(f_atual), attribute = TRUE), "'",
          " data-desc='", htmltools::htmlEscape(base::tolower(itens_f$descricao[j]), attribute = TRUE), "'",
          " data-val='", itens_f$val_item[j], "'",
          " data-tot='", tot_f, "'",
          " data-h-g1='", html_col_fonte, "'",
          " data-h-desc='", html_col_desc, "'",
          " data-h-val='", html_col_val, "'",
          " data-h-vpct='", html_col_vpct, "'",
          " data-h-tot='", html_col_tot, "'",
          " data-h-tpct='", html_col_pct, "'"
        )
        
        if (j == 1) {
          linhas_html <- base::c(linhas_html, base::paste0(
            "<tr", atributos_tr, ">",
            "<td rowspan='", k_rows, "' class='cel-primeira-col'><b>", f_atual, "</b> <span class='badge-n'>(", n_f, ")</span></td>",
            "<td>", itens_f$desc_lbl[j], "</td>",
            "<td>", formatar_celula_moeda(itens_f$val_item[j], FALSE), "</td>",
            "<td>", itens_f$pct_item[j], "%</td>",
            "<td rowspan='", k_rows, "'>", formatar_celula_moeda(tot_f, TRUE), "</td>",
            "<td rowspan='", k_rows, "'><b>", pct_f, "%</b></td>",
            "</tr>"
          ))
        } else {
          linhas_html <- base::c(linhas_html, base::paste0(
            "<tr", atributos_tr, ">",
            "<td>", itens_f$desc_lbl[j], "</td>",
            "<td>", formatar_celula_moeda(itens_f$val_item[j], FALSE), "</td>",
            "<td>", itens_f$pct_item[j], "%</td>",
            "</tr>"
          ))
        }
      }
    }
    
    linha_total <- base::paste0(
      "<tr class='linha-total-dt'>",
      "<td class='cel-primeira-col'><b>Total (", n_lanc, ")</b></td>",
      "<td>—</td>",
      "<td>", formatar_celula_moeda(val_total, TRUE), "</td>",
      "<td><b>100.0%</b></td>",
      "<td>", formatar_celula_moeda(val_total, TRUE), "</td>",
      "<td><b>100.0%</b></td>",
      "</tr>"
    )
    
    tabela_final <- base::paste0(
      "<div class='wrapper-tabela-dt-alf'>",
      "<table class='tabela-estilo-dt-alf tabela-ordenavel-alf' data-tipo-tab='entradas'>",
      "<thead><tr>",
      "<th class='th-sort-alf' data-col='g1' title='Clique para ordenar por Fonte de Renda'><i class='bi bi-briefcase'></i> Fonte de Renda</th>",
      "<th class='th-sort-alf' data-col='desc' title='Clique para ordenar por Descrição'><i class='bi bi-file-earmark-text'></i> Descrição</th>",
      "<th class='th-sort-alf' data-col='val' title='Clique para ordenar por Valor'><i class='bi bi-cash-stack'></i> Valor</th>",
      "<th class='th-sort-alf' data-col='val' title='Clique para ordenar por %'>%</th>",
      "<th class='th-sort-alf' data-col='tot' title='Clique para ordenar por Total da Fonte'><i class='bi bi-calculator'></i> Total</th>",
      "<th class='th-sort-alf' data-col='tot' title='Clique para ordenar por % do Total'>%</th>",
      "</tr></thead>",
      "<tbody>", base::paste(linhas_html, collapse = ""), linha_total, "</tbody>",
      "</table></div>"
    )
    
  } else {
    # SAÍDAS: Forma de Pagamento | Tipo de Despesa | Item de Consumo | Descrição | Valor | % | Total (por Item) | % (por Item)
    formas_agr <- df_sub %>%
      dplyr::group_by(grupo_1) %>%
      dplyr::summarise(
        n_forma   = dplyr::n(),
        tot_forma = base::sum(valor, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(tot_forma))
    
    linhas_html <- base::character(0)
    
    for (idx_f in base::seq_len(base::nrow(formas_agr))) {
      f_atual   <- formas_agr$grupo_1[idx_f]
      n_f       <- formas_agr$n_forma[idx_f]
      tot_f_num <- formas_agr$tot_forma[idx_f]
      
      df_forma <- df_sub %>% dplyr::filter(grupo_1 == f_atual)
      
      qtd_por_tipo <- df_forma %>%
        dplyr::count(grupo_2, name = "qtd_lanc_tipo")
      
      resumo_por_item <- df_forma %>%
        dplyr::group_by(grupo_2, item_consumo) %>%
        dplyr::summarise(
          qtd_lanc_item = dplyr::n(),
          soma_i        = base::sum(valor, na.rm = TRUE),
          .groups       = "drop"
        ) %>%
        dplyr::mutate(
          pct_i = base::formatC((soma_i / val_total) * 100, format = "f", digits = 1, decimal.mark = ".")
        )
      
      ordem_tipo <- df_forma %>%
        dplyr::group_by(grupo_2) %>%
        dplyr::summarise(soma_t = base::sum(valor, na.rm = TRUE), .groups = "drop")
      
      itens_f <- df_forma %>%
        dplyr::group_by(grupo_2, item_consumo, descricao) %>%
        dplyr::summarise(
          val_desc = base::sum(valor, na.rm = TRUE),
          qtd_desc = dplyr::n(),
          .groups  = "drop"
        ) %>%
        dplyr::left_join(qtd_por_tipo, by = "grupo_2") %>%
        dplyr::left_join(resumo_por_item, by = base::c("grupo_2", "item_consumo")) %>%
        dplyr::left_join(ordem_tipo, by = "grupo_2") %>%
        dplyr::arrange(dplyr::desc(soma_t), grupo_2, dplyr::desc(soma_i), item_consumo, dplyr::desc(val_desc)) %>%
        dplyr::mutate(
          pct_desc = base::formatC((val_desc / val_total) * 100, format = "f", digits = 1, decimal.mark = "."),
          tipo_lbl = base::paste0("<b>", grupo_2, "</b> <span class='badge-n'>(", qtd_lanc_tipo, ")</span>"),
          item_lbl = base::ifelse(qtd_lanc_item > 1, base::paste0(item_consumo, " <span class='badge-n'>(", qtd_lanc_item, ")</span>"), item_consumo),
          desc_lbl = base::ifelse(qtd_desc > 1, base::paste0(descricao, " <span class='badge-n'>(", qtd_desc, ")</span>"), descricao)
        )
      
      linhas_por_tipo <- itens_f %>%
        dplyr::count(grupo_2, name = "n_rows_tipo")
      
      linhas_por_item <- itens_f %>%
        dplyr::count(grupo_2, item_consumo, name = "n_rows_item")
      
      itens_f <- itens_f %>%
        dplyr::left_join(linhas_por_tipo, by = "grupo_2") %>%
        dplyr::left_join(linhas_por_item, by = base::c("grupo_2", "item_consumo")) %>%
        dplyr::group_by(grupo_2) %>%
        dplyr::mutate(prim_tipo = dplyr::row_number() == 1) %>%
        dplyr::group_by(grupo_2, item_consumo) %>%
        dplyr::mutate(prim_item = dplyr::row_number() == 1) %>%
        dplyr::ungroup()
      
      k_rows <- base::nrow(itens_f)
      html_col_forma <- htmltools::htmlEscape(base::paste0("<b>", f_atual, "</b> <span class='badge-n'>(", n_f, ")</span>"), attribute = TRUE)
      
      for (j in base::seq_len(k_rows)) {
        html_col_tipo <- htmltools::htmlEscape(itens_f$tipo_lbl[j], attribute = TRUE)
        html_col_item <- htmltools::htmlEscape(itens_f$item_lbl[j], attribute = TRUE)
        html_col_desc <- htmltools::htmlEscape(itens_f$desc_lbl[j], attribute = TRUE)
        html_col_val  <- htmltools::htmlEscape(formatar_celula_moeda(itens_f$val_desc[j], FALSE), attribute = TRUE)
        html_col_vpct <- htmltools::htmlEscape(base::paste0(itens_f$pct_desc[j], "%"), attribute = TRUE)
        html_col_tot  <- htmltools::htmlEscape(formatar_celula_moeda(itens_f$soma_i[j], TRUE), attribute = TRUE)
        html_col_tpct <- htmltools::htmlEscape(base::paste0("<b>", itens_f$pct_i[j], "%</b>"), attribute = TRUE)
        
        atributos_tr <- base::paste0(
          " class='linha-dados-sort'",
          " data-g1='", htmltools::htmlEscape(base::tolower(f_atual), attribute = TRUE), "'",
          " data-g1-tot='", tot_f_num, "'",
          " data-g2='", htmltools::htmlEscape(base::tolower(itens_f$grupo_2[j]), attribute = TRUE), "'",
          " data-g2-tot='", itens_f$soma_t[j], "'",
          " data-item='", htmltools::htmlEscape(base::tolower(itens_f$item_consumo[j]), attribute = TRUE), "'",
          " data-tot='", itens_f$soma_i[j], "'",
          " data-desc='", htmltools::htmlEscape(base::tolower(itens_f$descricao[j]), attribute = TRUE), "'",
          " data-val='", itens_f$val_desc[j], "'",
          " data-h-g1='", html_col_forma, "'",
          " data-h-g2='", html_col_tipo, "'",
          " data-h-item='", html_col_item, "'",
          " data-h-desc='", html_col_desc, "'",
          " data-h-val='", html_col_val, "'",
          " data-h-vpct='", html_col_vpct, "'",
          " data-h-tot='", html_col_tot, "'",
          " data-h-tpct='", html_col_tpct, "'"
        )
        
        cel_forma <- if (j == 1) {
          base::paste0("<td rowspan='", k_rows, "' class='cel-primeira-col'><b>", f_atual, "</b> <span class='badge-n'>(", n_f, ")</span></td>")
        } else ""
        
        cel_tipo <- if (itens_f$prim_tipo[j]) {
          base::paste0("<td rowspan='", itens_f$n_rows_tipo[j], "'>", itens_f$tipo_lbl[j], "</td>")
        } else ""
        
        cel_item <- if (itens_f$prim_item[j]) {
          base::paste0("<td rowspan='", itens_f$n_rows_item[j], "'>", itens_f$item_lbl[j], "</td>")
        } else ""
        
        cel_tot_item <- if (itens_f$prim_item[j]) {
          base::paste0(
            "<td rowspan='", itens_f$n_rows_item[j], "'>", formatar_celula_moeda(itens_f$soma_i[j], TRUE), "</td>",
            "<td rowspan='", itens_f$n_rows_item[j], "'><b>", itens_f$pct_i[j], "%</b></td>"
          )
        } else ""
        
        linhas_html <- base::c(linhas_html, base::paste0(
          "<tr", atributos_tr, ">",
          cel_forma,
          cel_tipo,
          cel_item,
          "<td>", itens_f$desc_lbl[j], "</td>",
          "<td>", formatar_celula_moeda(itens_f$val_desc[j], FALSE), "</td>",
          "<td>", itens_f$pct_desc[j], "%</td>",
          cel_tot_item,
          "</tr>"
        ))
      }
    }
    
    linha_total <- base::paste0(
      "<tr class='linha-total-dt'>",
      "<td class='cel-primeira-col'><b>Total (", n_lanc, ")</b></td>",
      "<td>—</td>",
      "<td>—</td>",
      "<td>—</td>",
      "<td>", formatar_celula_moeda(val_total, TRUE), "</td>",
      "<td><b>100.0%</b></td>",
      "<td>", formatar_celula_moeda(val_total, TRUE), "</td>",
      "<td><b>100.0%</b></td>",
      "</tr>"
    )
    
    tabela_final <- base::paste0(
      "<div class='wrapper-tabela-dt-alf'>",
      "<table class='tabela-estilo-dt-alf tabela-ordenavel-alf' data-tipo-tab='saidas'>",
      "<thead><tr>",
      "<th class='th-sort-alf' data-col='g1' title='Clique para ordenar por Forma de Pagamento'><i class='bi bi-credit-card'></i> Forma de Pagamento</th>",
      "<th class='th-sort-alf' data-col='g2' title='Clique para ordenar por Tipo de Despesa'><i class='bi bi-tags'></i> Tipo de Despesa</th>",
      "<th class='th-sort-alf' data-col='item' title='Clique para ordenar por Item de Consumo'><i class='bi bi-cart3'></i> Item de Consumo</th>",
      "<th class='th-sort-alf' data-col='desc' title='Clique para ordenar por Descrição'><i class='bi bi-file-earmark-text'></i> Descrição</th>",
      "<th class='th-sort-alf' data-col='val' title='Clique para ordenar por Valor'><i class='bi bi-cash-stack'></i> Valor</th>",
      "<th class='th-sort-alf' data-col='val' title='Clique para ordenar por %'>%</th>",
      "<th class='th-sort-alf' data-col='tot' title='Clique para ordenar por Total do Item'><i class='bi bi-calculator'></i> Total</th>",
      "<th class='th-sort-alf' data-col='tot' title='Clique para ordenar por % do Item'>%</th>",
      "</tr></thead>",
      "<tbody>", base::paste(linhas_html, collapse = ""), linha_total, "</tbody>",
      "</table></div>"
    )
  }
  
  base::paste0(
    bloco_resumo,
    tabela_final
  )
}


# ==============================================================================
# 5. GERADOR DE CAMADAS (BARRAS, LINHAS E BOXPLOT)
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
  
  box_cats        <- base::as.character(escopo_meses$eixo_x)
  box_periodos    <- base::as.character(escopo_meses$mes_label)
  box_e_x         <- base::character(0)
  box_e_y         <- base::numeric(0)
  box_s_x         <- base::character(0)
  box_s_y         <- base::numeric(0)
  
  pts_e_x         <- base::numeric(0)
  pts_e_y         <- base::numeric(0)
  pts_e_hov       <- base::character(0)
  pts_e_cor       <- base::character(0)
  pts_e_tam       <- base::numeric(0)
  
  pts_s_x         <- base::numeric(0)
  pts_s_y         <- base::numeric(0)
  pts_s_hov       <- base::character(0)
  pts_s_cor       <- base::character(0)
  pts_s_tam       <- base::numeric(0)
  
  med_e_x0        <- base::numeric(0)
  med_e_x1        <- base::numeric(0)
  med_e_y         <- base::numeric(0)
  med_s_x0        <- base::numeric(0)
  med_s_x1        <- base::numeric(0)
  med_s_y         <- base::numeric(0)
  
  box_res_e_y     <- base::numeric(base::nrow(escopo_meses))
  box_res_s_y     <- base::numeric(base::nrow(escopo_meses))
  box_card_e_html <- base::character(base::nrow(escopo_meses))
  box_card_s_html <- base::character(base::nrow(escopo_meses))
  
  for (i in base::seq_len(base::nrow(escopo_meses))) {
    idx_zero <- i - 1
    m_ref    <- escopo_meses$mes_ref[i]
    m_lbl    <- escopo_meses$mes_label[i]
    cat_x    <- escopo_meses$eixo_x[i]
    
    sub_e <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Entradas")
    sub_s <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Saídas")
    
    box_res_e_y[i]     <- base::sum(sub_e$valor, na.rm = TRUE)
    box_res_s_y[i]     <- base::sum(sub_s$valor, na.rm = TRUE)
    box_card_e_html[i] <- montar_card_tabela_unificada(sub_e, m_lbl, "Entradas", incluir_quartis = TRUE)
    box_card_s_html[i] <- montar_card_tabela_unificada(sub_s, m_lbl, "Saídas",   incluir_quartis = TRUE)
    
    if (base::nrow(sub_e) > 0) {
      vals_e    <- sub_e$valor
      qs_e      <- calcular_quartis_plotly(vals_e)
      lim_sup_e <- qs_e$q3 + 1.5 * qs_e$iiq
      lim_inf_e <- qs_e$q1 - 1.5 * qs_e$iiq
      eh_out_e  <- (vals_e > lim_sup_e | vals_e < lim_inf_e) & (qs_e$iiq > 0)
      
      n_e      <- base::nrow(sub_e)
      desloc_e <- if (n_e == 1) 0 else base::seq(-0.045, 0.045, length.out = n_e)
      x_pts_e  <- (idx_zero - 0.19) + base::ifelse(eh_out_e, 0, desloc_e)
      
      hov_ind_e <- base::paste0(
        "<b>R$ ", formatar_brl(vals_e), " | ", sub_e$grupo_1, " | ", sub_e$descricao, "</b>"
      )
      
      box_e_x   <- base::c(box_e_x, base::rep(cat_x, n_e))
      box_e_y   <- base::c(box_e_y, vals_e)
      pts_e_x   <- base::c(pts_e_x, x_pts_e)
      pts_e_y   <- base::c(pts_e_y, vals_e)
      pts_e_hov <- base::c(pts_e_hov, hov_ind_e)
      pts_e_cor <- base::c(pts_e_cor, base::ifelse(eh_out_e, cor_outlier, cor_ponto_entradas))
      pts_e_tam <- base::c(pts_e_tam, base::ifelse(eh_out_e, 8.5, 6.5))
      
      val_med_e <- base::mean(vals_e, na.rm = TRUE)
      med_e_x0  <- base::c(med_e_x0, idx_zero - 0.32)
      med_e_x1  <- base::c(med_e_x1, idx_zero - 0.06)
      med_e_y   <- base::c(med_e_y, val_med_e)
    }
    
    if (base::nrow(sub_s) > 0) {
      vals_s    <- sub_s$valor
      qs_s      <- calcular_quartis_plotly(vals_s)
      lim_sup_s <- qs_s$q3 + 1.5 * qs_s$iiq
      lim_inf_s <- qs_s$q1 - 1.5 * qs_s$iiq
      eh_out_s  <- (vals_s > lim_sup_s | vals_s < lim_inf_s) & (qs_s$iiq > 0)
      
      n_s      <- base::nrow(sub_s)
      desloc_s <- if (n_s == 1) 0 else base::seq(-0.045, 0.045, length.out = n_s)
      x_pts_s  <- (idx_zero + 0.19) + base::ifelse(eh_out_s, 0, desloc_s)
      
      hov_ind_s <- base::paste0(
        "<b>R$ ", formatar_brl(vals_s), " | ", sub_s$item_consumo, " | ", sub_s$grupo_1, " | ", sub_s$descricao, "</b>"
      )
      
      box_s_x   <- base::c(box_s_x, base::rep(cat_x, n_s))
      box_s_y   <- base::c(box_s_y, vals_s)
      pts_s_x   <- base::c(pts_s_x, x_pts_s)
      pts_s_y   <- base::c(pts_s_y, vals_s)
      pts_s_hov <- base::c(pts_s_hov, hov_ind_s)
      pts_s_cor <- base::c(pts_s_cor, base::ifelse(eh_out_s, cor_outlier, cor_ponto_saidas))
      pts_s_tam <- base::c(pts_s_tam, base::ifelse(eh_out_s, 8.5, 6.5))
      
      val_med_s <- base::mean(vals_s, na.rm = TRUE)
      med_s_x0  <- base::c(med_s_x0, idx_zero + 0.06)
      med_s_x1  <- base::c(med_s_x1, idx_zero + 0.32)
      med_s_y   <- base::c(med_s_y, val_med_s)
    }
  }
  
  # Dados para Barras Empilhadas e Linhas
  if (mes_sel == "Todos os Meses") {
    n_pts        <- base::nrow(escopo_meses)
    eixo_x_vec   <- base::as.character(escopo_meses$eixo_x)
    periodos_vec <- base::as.character(escopo_meses$mes_label)
    y_e          <- base::numeric(n_pts)
    y_s          <- base::numeric(n_pts)
    hov_e        <- base::character(n_pts)
    hov_s        <- base::character(n_pts)
    card_e_html  <- base::character(n_pts)
    card_s_html  <- base::character(n_pts)
    
    for (i in base::seq_len(n_pts)) {
      m_ref <- escopo_meses$mes_ref[i]
      m_lbl <- escopo_meses$mes_label[i]
      
      sub_e <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Entradas")
      sub_s <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Saídas")
      
      y_e[i]         <- base::sum(sub_e$valor, na.rm = TRUE)
      y_s[i]         <- base::sum(sub_s$valor, na.rm = TRUE)
      hov_e[i]       <- montar_hover_inteligente(sub_e, m_lbl, "Entradas")
      hov_s[i]       <- montar_hover_inteligente(sub_s, m_lbl, "Saídas")
      card_e_html[i] <- montar_card_tabela_unificada(sub_e, m_lbl, "Entradas", incluir_quartis = FALSE)
      card_s_html[i] <- montar_card_tabela_unificada(sub_s, m_lbl, "Saídas",   incluir_quartis = FALSE)
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
      eixo_x_vec   <- base::c("<b>Sem lançamentos</b>")
      periodos_vec <- base::c(mes_sel)
      y_e          <- base::c(0)
      y_s          <- base::c(0)
      hov_e        <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Entradas"))
      hov_s        <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Saídas"))
      card_e_html  <- base::c(montar_card_tabela_unificada(d_mes[0, ], mes_sel, "Entradas", FALSE))
      card_s_html  <- base::c(montar_card_tabela_unificada(d_mes[0, ], mes_sel, "Saídas",   FALSE))
    } else {
      n_pts        <- base::length(dias_ativos)
      eixo_x_vec   <- base::paste0("<b>", base::format(dias_ativos, "%d"), "</b>")
      periodos_vec <- base::format(dias_ativos, "%d/%m/%Y")
      y_e          <- base::numeric(n_pts)
      y_s          <- base::numeric(n_pts)
      hov_e        <- base::character(n_pts)
      hov_s        <- base::character(n_pts)
      card_e_html  <- base::character(n_pts)
      card_s_html  <- base::character(n_pts)
      
      for (i in base::seq_len(n_pts)) {
        dia_i   <- dias_ativos[i]
        lbl_dia <- periodos_vec[i]
        
        sub_e <- d_mes %>% dplyr::filter(data == dia_i, tipo == "Entradas")
        sub_s <- d_mes %>% dplyr::filter(data == dia_i, tipo == "Saídas")
        
        y_e[i]         <- base::sum(sub_e$valor, na.rm = TRUE)
        y_s[i]         <- base::sum(sub_s$valor, na.rm = TRUE)
        hov_e[i]       <- montar_hover_inteligente(sub_e, lbl_dia, "Entradas")
        hov_s[i]       <- montar_hover_inteligente(sub_s, lbl_dia, "Saídas")
        card_e_html[i] <- montar_card_tabela_unificada(sub_e, lbl_dia, "Entradas", incluir_quartis = FALSE)
        card_s_html[i] <- montar_card_tabela_unificada(sub_s, lbl_dia, "Saídas",   incluir_quartis = FALSE)
      }
    }
  }
  
  soma_topo <- y_e + y_s
  max_bar   <- base::max(base::c(soma_topo, 100), na.rm = TRUE) * 1.22
  max_line  <- base::max(base::c(y_e, y_s, 100), na.rm = TRUE) * 1.28
  max_box   <- base::max(base::c(box_e_y, box_s_y, 100), na.rm = TRUE) * 1.18
  
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
    x            = base::as.character(eixo_x_vec),
    periodos     = base::as.character(periodos_vec),
    e_y          = base::as.numeric(y_e),
    s_y          = base::as.numeric(y_s),
    e_txt_bar    = base::as.character(txt_e_bar),
    s_txt_bar    = base::as.character(txt_s_bar),
    e_txt_line   = base::as.character(txt_e_line),
    s_txt_line   = base::as.character(txt_s_line),
    e_hover      = base::as.character(hov_e),
    s_hover      = base::as.character(hov_s),
    card_e       = base::as.character(card_e_html),
    card_s       = base::as.character(card_s_html),
    # Vetores do Boxplot
    box_cats     = base::as.character(box_cats),
    box_periodos = base::as.character(box_periodos),
    box_e_x      = base::as.character(box_e_x),
    box_e_y      = base::as.numeric(box_e_y),
    box_s_x      = base::as.character(box_s_x),
    box_s_y      = base::as.numeric(box_s_y),
    pts_e_x      = base::as.numeric(pts_e_x),
    pts_e_y      = base::as.numeric(pts_e_y),
    pts_e_hov    = base::as.character(pts_e_hov),
    pts_e_cor    = base::as.character(pts_e_cor),
    pts_e_tam    = base::as.numeric(pts_e_tam),
    pts_s_x      = base::as.numeric(pts_s_x),
    pts_s_y      = base::as.numeric(pts_s_y),
    pts_s_hov    = base::as.character(pts_s_hov),
    pts_s_cor    = base::as.character(pts_s_cor),
    pts_s_tam    = base::as.numeric(pts_s_tam),
    med_e_x0     = base::as.numeric(med_e_x0),
    med_e_x1     = base::as.numeric(med_e_x1),
    med_e_y      = base::as.numeric(med_e_y),
    med_s_x0     = base::as.numeric(med_s_x0),
    med_s_x1     = base::as.numeric(med_s_x1),
    med_s_y      = base::as.numeric(med_s_y),
    box_res_e_y  = base::as.numeric(box_res_e_y),
    box_res_s_y  = base::as.numeric(box_res_s_y),
    box_card_e   = base::as.character(box_card_e_html),
    box_card_s   = base::as.character(box_card_s_html),
    e_nome       = nome_e,
    s_nome       = nome_s,
    dif_nome     = nome_dif,
    dif_cor      = base::ifelse(dif >= 0, "#15803d", "#b91c1c"),
    max_bar      = base::as.numeric(max_bar)[1],
    max_line     = base::as.numeric(max_line)[1],
    max_box      = base::as.numeric(max_box)[1]
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
# 7. CONSTRUÇÃO DO GRÁFICO PLOTLY + ONRENDER (COM ORDENAÇÃO HIERÁRQUICA DE TABELAS)
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
  const legBoxplotDir = painel.querySelector('.legenda-boxplot-direita-alf');
  const cardDetalhes  = painel.querySelector('.card-hover-mobile-alf');

  function formatarMoedaJS(v) {
    return v.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  }

  function obterCategoriasAtuais(d) {
    return (modoAtual === 'boxplot') ? (d.box_cats || []) : (d.x || []);
  }

  function obterPeriodosAtuais(d) {
    return (modoAtual === 'boxplot') ? (d.box_periodos || []) : (d.periodos || []);
  }

  // =========================================================================
  // ORDENADOR HIERÁRQUICO INTELIGENTE (ORDENA E RECONSTRÓI ROWSPAN NA HORA!)
  // =========================================================================
  function ativarOrdenacaoTabela(container) {
    const tabela = container.querySelector('table.tabela-ordenavel-alf');
    if (!tabela) return;

    const tipoTab = tabela.getAttribute('data-tipo-tab');
    const ths = tabela.querySelectorAll('th.th-sort-alf');

    ths.forEach(function(th) {
      th.addEventListener('click', function() {
        const col = this.getAttribute('data-col');
        const dirAtual = this.getAttribute('data-dir') || 'desc';
        const novaDir  = (dirAtual === 'asc') ? 'desc' : 'asc';
        const mult     = (novaDir === 'asc') ? 1 : -1;

        ths.forEach(function(t) { t.removeAttribute('data-dir'); });
        this.setAttribute('data-dir', novaDir);

        const tbody    = tabela.querySelector('tbody');
        const trTotal  = tbody.querySelector('tr.linha-total-dt');
        const trsDados = Array.from(tbody.querySelectorAll('tr.linha-dados-sort'));
        if (trsDados.length <= 1) return;

        const linhas = trsDados.map(function(tr) {
          return {
            g1:    tr.getAttribute('data-g1') || '',
            g1Tot: parseFloat(tr.getAttribute('data-g1-tot') || '0'),
            g2:    tr.getAttribute('data-g2') || '',
            g2Tot: parseFloat(tr.getAttribute('data-g2-tot') || '0'),
            item:  tr.getAttribute('data-item') || '',
            tot:   parseFloat(tr.getAttribute('data-tot') || '0'),
            desc:  tr.getAttribute('data-desc') || '',
            val:   parseFloat(tr.getAttribute('data-val') || '0'),
            hG1:   tr.getAttribute('data-h-g1') || '',
            hG2:   tr.getAttribute('data-h-g2') || '',
            hItem: tr.getAttribute('data-h-item') || '',
            hDesc: tr.getAttribute('data-h-desc') || '',
            hVal:  tr.getAttribute('data-h-val') || '',
            hVpct: tr.getAttribute('data-h-vpct') || '',
            hTot:  tr.getAttribute('data-h-tot') || '',
            hTpct: tr.getAttribute('data-h-tpct') || ''
          };
        });

        function cmpTexto(a, b) {
          return a.localeCompare(b, 'pt-BR');
        }

        if (tipoTab === 'entradas') {
          linhas.sort(function(a, b) {
            if (col === 'g1') {
              const c = cmpTexto(a.g1, b.g1) * mult;
              return c !== 0 ? c : (b.val - a.val);
            } else if (col === 'tot') {
              const c = (a.tot - b.tot) * mult;
              if (c !== 0) return c;
              const cg = cmpTexto(a.g1, b.g1);
              return cg !== 0 ? cg : (b.val - a.val);
            } else if (col === 'desc') {
              const cg = cmpTexto(a.g1, b.g1);
              if (cg !== 0) return cg;
              return cmpTexto(a.desc, b.desc) * mult;
            } else {
              // Ordena por Valor mantendo as fontes agrupadas e ordenando também as fontes pelo valor
              if (a.g1 !== b.g1) return (a.tot - b.tot) * mult;
              return (a.val - b.val) * mult;
            }
          });

          // Reconstrói o HTML de Entradas recalculando o rowspan de cada Fonte de Renda
          const contG1 = {};
          linhas.forEach(function(r) { contG1[r.g1] = (contG1[r.g1] || 0) + 1; });

          const vistoG1 = {};
          let htmlNovo  = '';
          linhas.forEach(function(r) {
            const primG1 = !vistoG1[r.g1];
            vistoG1[r.g1] = true;
            const spanG1 = contG1[r.g1];

            htmlNovo += '<tr class=\"linha-dados-sort\"' +
              ' data-g1=\"' + r.g1 + '\" data-desc=\"' + r.desc + '\"' +
              ' data-val=\"' + r.val + '\" data-tot=\"' + r.tot + '\"' +
              ' data-h-g1=\"' + r.hG1.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-desc=\"' + r.hDesc.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-val=\"' + r.hVal.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-vpct=\"' + r.hVpct.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-tot=\"' + r.hTot.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-tpct=\"' + r.hTpct.replace(/\"/g, '&quot;') + '\">' +
              (primG1 ? '<td rowspan=\"' + spanG1 + '\" class=\"cel-primeira-col\">' + r.hG1 + '</td>' : '') +
              '<td>' + r.hDesc + '</td>' +
              '<td>' + r.hVal + '</td>' +
              '<td>' + r.hVpct + '</td>' +
              (primG1 ? '<td rowspan=\"' + spanG1 + '\">' + r.hTot + '</td>' : '') +
              (primG1 ? '<td rowspan=\"' + spanG1 + '\">' + r.hTpct + '</td>' : '') +
              '</tr>';
          });

          tbody.innerHTML = htmlNovo + (trTotal ? trTotal.outerHTML : '');

        } else {
          // SAÍDAS: Ordena respeitando a hierarquia Forma -> Tipo -> Item -> Descrição
          linhas.sort(function(a, b) {
            if (col === 'g1') {
              const c1 = cmpTexto(a.g1, b.g1) * mult;
              if (c1 !== 0) return c1;
            } else {
              if (a.g1 !== b.g1) return b.g1Tot - a.g1Tot;
            }

            if (col === 'g2') {
              const c2 = cmpTexto(a.g2, b.g2) * mult;
              if (c2 !== 0) return c2;
            } else {
              if (a.g2 !== b.g2) return b.g2Tot - a.g2Tot;
            }

            if (col === 'item') {
              const ci = cmpTexto(a.item, b.item) * mult;
              if (ci !== 0) return ci;
            } else if (col === 'tot') {
              const ct = (a.tot - b.tot) * mult;
              if (ct !== 0) return ct;
              const ci = cmpTexto(a.item, b.item);
              if (ci !== 0) return ci;
            } else {
              if (a.item !== b.item) return (a.tot - b.tot) * (col === 'val' ? mult : -1);
            }

            if (col === 'desc') {
              return cmpTexto(a.desc, b.desc) * mult;
            }
            return (a.val - b.val) * mult;
          });

          // Reconstrói o HTML de Saídas recalculando os rowspans de Forma, Tipo e Item
          const contG1 = {};
          const contG2 = {};
          const contIt = {};

          linhas.forEach(function(r) {
            const k1 = r.g1;
            const k2 = r.g1 + '||' + r.g2;
            const k3 = r.g1 + '||' + r.g2 + '||' + r.item;
            contG1[k1] = (contG1[k1] || 0) + 1;
            contG2[k2] = (contG2[k2] || 0) + 1;
            contIt[k3] = (contIt[k3] || 0) + 1;
          });

          const vistoG1 = {};
          const vistoG2 = {};
          const vistoIt = {};
          let htmlNovo  = '';

          linhas.forEach(function(r) {
            const k1 = r.g1;
            const k2 = r.g1 + '||' + r.g2;
            const k3 = r.g1 + '||' + r.g2 + '||' + r.item;

            const primG1 = !vistoG1[k1];
            const primG2 = !vistoG2[k2];
            const primIt = !vistoIt[k3];

            vistoG1[k1] = true;
            vistoG2[k2] = true;
            vistoIt[k3] = true;

            htmlNovo += '<tr class=\"linha-dados-sort\"' +
              ' data-g1=\"' + r.g1 + '\" data-g1-tot=\"' + r.g1Tot + '\"' +
              ' data-g2=\"' + r.g2 + '\" data-g2-tot=\"' + r.g2Tot + '\"' +
              ' data-item=\"' + r.item + '\" data-tot=\"' + r.tot + '\"' +
              ' data-desc=\"' + r.desc + '\" data-val=\"' + r.val + '\"' +
              ' data-h-g1=\"' + r.hG1.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-g2=\"' + r.hG2.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-item=\"' + r.hItem.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-desc=\"' + r.hDesc.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-val=\"' + r.hVal.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-vpct=\"' + r.hVpct.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-tot=\"' + r.hTot.replace(/\"/g, '&quot;') + '\"' +
              ' data-h-tpct=\"' + r.hTpct.replace(/\"/g, '&quot;') + '\">' +
              (primG1 ? '<td rowspan=\"' + contG1[k1] + '\" class=\"cel-primeira-col\">' + r.hG1 + '</td>' : '') +
              (primG2 ? '<td rowspan=\"' + contG2[k2] + '\">' + r.hG2 + '</td>' : '') +
              (primIt ? '<td rowspan=\"' + contIt[k3] + '\">' + r.hItem + '</td>' : '') +
              '<td>' + r.hDesc + '</td>' +
              '<td>' + r.hVal + '</td>' +
              '<td>' + r.hVpct + '</td>' +
              (primIt ? '<td rowspan=\"' + contIt[k3] + '\">' + r.hTot + '</td>' : '') +
              (primIt ? '<td rowspan=\"' + contIt[k3] + '\">' + r.hTpct + '</td>' : '') +
              '</tr>';
          });

          tbody.innerHTML = htmlNovo + (trTotal ? trTotal.outerHTML : '');
        }
      });
    });
  }

  function atualizarCardInterativo(d) {
    if (!cardDetalhes) return;
    const cats     = obterCategoriasAtuais(d);
    const periodos = obterPeriodosAtuais(d);

    if (indiceClicado === null || !cats.length || indiceClicado >= cats.length) {
      cardDetalhes.style.display = 'none';
      cardDetalhes.innerHTML = '';
      return;
    }

    const rotPeriodo = periodos[indiceClicado] || String(cats[indiceClicado] || '').replace(/<[^>]*>/g, '');

    const htmlE = (modoAtual === 'boxplot')
      ? (d.box_card_e[indiceClicado] || '')
      : (d.card_e[indiceClicado] || '');
    const htmlS = (modoAtual === 'boxplot')
      ? (d.box_card_s[indiceClicado] || '')
      : (d.card_s[indiceClicado] || '');

    const htmlAtivo = (serieClicada === 1) ? htmlS : htmlE;

    cardDetalhes.innerHTML =
      '<div class=\"topo-card-mobile-alf\">' +
        '<div class=\"abas-card-mobile-alf\">' +
          '<button type=\"button\" class=\"aba-mob-alf ' + (serieClicada === 0 ? 'ativa-e' : '') + '\" data-serie=\"0\">Entradas ' + rotPeriodo + '</button>' +
          '<button type=\"button\" class=\"aba-mob-alf ' + (serieClicada === 1 ? 'ativa-s' : '') + '\" data-serie=\"1\">Saídas ' + rotPeriodo + '</button>' +
        '</div>' +
        '<button type=\"button\" class=\"fechar-card-mobile-alf\" title=\"Fechar\">&times;</button>' +
      '</div>' +
      '<div class=\"corpo-card-mobile-alf\">' + htmlAtivo + '</div>';
    cardDetalhes.style.display = 'block';

    // Ativa o clique de ordenação nos cabeçalhos da tabela recém-aberta!
    ativarOrdenacaoTabela(cardDetalhes);

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
    if (legBoxplotDir) {
      legBoxplotDir.style.display = (modoAtual === 'boxplot') ? 'inline-flex' : 'none';
    }
    painel.classList.toggle('modo-boxplot-ativo', modoAtual === 'boxplot');

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
      const mapaCat = {};
      cats.forEach(function(c, idx) { mapaCat[c] = idx; });

      const xBoxE = (d.box_e_x || []).map(function(c) { return (mapaCat[c] !== undefined ? mapaCat[c] : 0) - 0.19; });
      const xBoxS = (d.box_s_x || []).map(function(c) { return (mapaCat[c] !== undefined ? mapaCat[c] : 0) + 0.19; });

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

      const traceBoxE = {
        type: 'box',
        x: xBoxE,
        y: d.box_e_y,
        name: 'Entradas',
        width: 0.26,
        quartilemethod: 'linear',
        boxpoints: false,
        hoveron: 'boxes',
        fillcolor: 'rgba(41, 182, 216, 0.42)',
        line: { color: '", cor_entradas, "', width: 1.8 },
        hoverinfo: 'y'
      };

      const traceBoxS = {
        type: 'box',
        x: xBoxS,
        y: d.box_s_y,
        name: 'Saídas',
        width: 0.26,
        quartilemethod: 'linear',
        boxpoints: false,
        hoveron: 'boxes',
        fillcolor: 'rgba(27, 120, 161, 0.42)',
        line: { color: '", cor_saidas, "', width: 1.8 },
        hoverinfo: 'y'
      };

      const tracePtsE = {
        type: 'scatter',
        mode: 'markers',
        x: d.pts_e_x,
        y: d.pts_e_y,
        marker: {
          color: d.pts_e_cor,
          size: isMobile ? d.pts_e_tam.map(function(s) { return s + 1.5; }) : d.pts_e_tam,
          line: { color: '#ffffff', width: 1 }
        },
        hoverinfo: 'text',
        hovertext: d.pts_e_hov
      };

      const tracePtsS = {
        type: 'scatter',
        mode: 'markers',
        x: d.pts_s_x,
        y: d.pts_s_y,
        marker: {
          color: d.pts_s_cor,
          size: isMobile ? d.pts_s_tam.map(function(s) { return s + 1.5; }) : d.pts_s_tam,
          line: { color: '#ffffff', width: 1 }
        },
        hoverinfo: 'text',
        hovertext: d.pts_s_hov
      };

      tracesFinais = [traceBoxE, traceBoxS, tracePtsE, tracePtsS];

    } else {
      const coresMarkerE = [];
      const coresMarkerS = [];
      const bordaMarkerE = [];
      const bordaMarkerS = [];
      const largBordaM   = [];
      const tamMarkerArr = [];

      for (let i = 0; i < qtdPontos; i++) {
        if (indiceClicado !== null && i === indiceClicado) {
          coresMarkerE.push('#000000');
          coresMarkerS.push('#000000');
          bordaMarkerE.push('#000000');
          bordaMarkerS.push('#000000');
          largBordaM.push(2);
          tamMarkerArr.push(isMobile ? 7.5 : 8.5);
        } else {
          coresMarkerE.push('", cor_entradas, "');
          coresMarkerS.push('", cor_saidas, "');
          bordaMarkerE.push('", cor_entradas, "');
          bordaMarkerS.push('", cor_saidas, "');
          largBordaM.push(1);
          tamMarkerArr.push(isMobile ? 6 : 7);
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
        opacity: 1,
        line: { width: 0 }
      } : {
        color: coresMarkerE,
        size: tamMarkerArr,
        opacity: 1,
        line: { color: bordaMarkerE, width: largBordaM }
      };

      const markerSaidas = ehBarra ? {
        color: '", cor_saidas, "',
        opacity: 1,
        line: { width: 0 }
      } : {
        color: coresMarkerS,
        size: tamMarkerArr,
        opacity: 1,
        line: { color: bordaMarkerS, width: largBordaM }
      };

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
        hoverinfo: 'text',
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
        hoverinfo: 'text',
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
      range: [-0.55, (cats.length - 1) + 0.55],
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
      hovermode: 'closest',
      hoverdistance: isMobile ? 35 : 20,
      hoverlabel: {
        bgcolor: '", hover_bg_cor, "',
        bordercolor: '", borda_balao_hover, "',
        align: 'left',
        font: {
          family: 'Inter, sans-serif',
          size: isMobile ? 10.5 : ", hover_fonte_tam, ",
          color: '", hover_txt_cor, "'
        }
      },
      margin: isMobile
        ? { t: 22, b: qtdPontos > 8 ? 38 : 30, l: 44, r: 10 }
        : { t: 28, b: 42, l: 62, r: 15 },
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

  let tsUltimoClique = 0;

  el.on('plotly_click', function(data) {
    if (!data || !data.points || !data.points.length) return;
    tsUltimoClique = Date.now();
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

    if (modoAtual === 'boxplot' && window.innerWidth <= 768) {
      indiceClicado = idx;
      serieClicada  = crv;
      atualizarRotulosTopo(d);
      atualizarCardInterativo(d);
      return;
    }

    if (indiceClicado === idx && serieClicada === crv) {
      indiceClicado = null;
    } else {
      indiceClicado = idx;
      serieClicada  = crv;
    }
    atualizarGrafico();
  });

  el.addEventListener('pointerup', function(ev) {
    if (Date.now() - tsUltimoClique < 250) return;

    const tickNode = ev.target.closest('.xtick');
    if (tickNode) {
      const todosTicks = Array.from(el.querySelectorAll('.xtick'));
      const idx = todosTicks.indexOf(tickNode);
      if (idx !== -1) {
        indiceClicado = (indiceClicado === idx) ? null : idx;
        atualizarGrafico();
      }
      return;
    }

    if (window.innerWidth <= 768 && modoAtual !== 'boxplot') {
      const bgRect = el.querySelector('.nsewdrag');
      if (!bgRect) return;
      const rect = bgRect.getBoundingClientRect();
      if (ev.clientX >= rect.left && ev.clientX <= rect.right && ev.clientY >= rect.top && ev.clientY <= rect.bottom) {
        const d    = DADOS[mesAtual] && DADOS[mesAtual][usuarioAtual];
        const cats = obterCategoriasAtuais(d);
        if (!cats.length) return;
        const propX = (ev.clientX - rect.left) / rect.width;
        const idx   = Math.min(cats.length - 1, Math.max(0, Math.floor(propX * cats.length)));
        indiceClicado = (indiceClicado === idx) ? null : idx;
        atualizarGrafico();
      }
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
# 8. ESTILO CSS E ESTRUTURA HTML (KPIS MINIMALISTAS + ORDENAÇÃO CLICÁVEL)
# ==============================================================================

estilo_controles_fluxo <- htmltools::tags$style(base::paste0("
  @import url('https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css');

  .painel-fluxo-alf {
    max-width: 950px;
    width: 100%;
    margin: 20px auto;
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
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

  /* 1. Título Principal ACIMA da linha preta da barra de controles */
  .titulo-grafico-alf {
    color: ", titulo_cor, ";
    font-size: ", titulo_tamanho, "px;
    font-weight: 700;
    text-align: left;
    margin: 0 0 8px 2px;
    line-height: 1.25;
  }

  /* 2. Caixa Única de Controles + KPIs Minimalistas na Linha Inferior */
  .barra-controles-alf {
    display: flex;
    flex-direction: column;
    gap: 10px;
    padding: 12px 16px;
    margin-bottom: 12px;
    background: #fcfcfc;
    border: 1px solid #e0e0e0;
    border-top: 2px solid #000000;
    border-radius: 0 0 10px 10px;
  }
  .linha-filtros-sup-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    width: 100%;
  }
  .linha-legendas-inf-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    width: 100%;
    padding-top: 10px;
    border-top: 1px solid #eaeaea;
  }

  /* KPIs Minimalistas e Limpos (sem caixotes pesados ao redor) */
  .legenda-custom-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 20px;
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

  .legenda-boxplot-direita-alf {
    display: none;
    flex-wrap: wrap;
    align-items: center;
    gap: 14px;
    background: #ffffff;
    border: 1px solid #e4e4e7;
    border-radius: 8px;
    padding: 4px 12px;
    font-size: 11.5px;
    color: #1e293b;
  }
  .item-leg-box-alf {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    font-weight: 600;
  }
  .bolinha-leg-box {
    width: 9px;
    height: 9px;
    border-radius: 50%;
    display: inline-block;
  }
  .traco-media-leg-box {
    width: 18px;
    height: 0;
    border-top: 2.5px dotted ", cor_media_box, ";
    display: inline-block;
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

  /* =========================================================
     ÁREA INFERIOR DAS TABELAS (SEM SOMBRA/CAIXA EXTERNA DUPLICADA)
     ========================================================= */
  .card-hover-mobile-alf {
    display: none;
    width: 100%;
    box-sizing: border-box;
    margin-top: 10px;
    padding: 4px 0;
    background: transparent;
    border: none;
    box-shadow: none;
    font-size: 12px;
    line-height: 1.45;
    color: #000000;
    text-align: left;
  }
  .topo-card-mobile-alf {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 10px;
    padding-bottom: 6px;
  }
  .abas-card-mobile-alf {
    display: inline-flex;
    flex-wrap: wrap;
    gap: 8px;
  }
  .aba-mob-alf {
    font-family: inherit;
    font-size: 12px;
    font-weight: 600;
    padding: 6px 14px;
    border-radius: 6px;
    border: 1px solid #d0d0d0;
    background: #ffffff;
    color: #222222;
    cursor: pointer;
    transition: all 0.15s ease;
  }
  .aba-mob-alf:hover:not(.ativa-e):not(.ativa-s) {
    border-color: #000000;
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
    font-size: 20px;
    line-height: 1;
    color: #555555;
    cursor: pointer;
    padding: 0 4px;
  }

  /* =========================================================
     TABELA DE RESUMO ESTATÍSTICO COMPACTA
     ========================================================= */
  .secao-resumo-estatistico {
    margin-bottom: 12px;
  }
  table.tabela-metricas-compacta {
    width: auto !important;
    min-width: 420px;
    max-width: 100%;
    margin: 0 !important;
    font-size: 11.5px !important;
  }
  table.tabela-metricas-compacta thead th {
    font-size: 11.5px !important;
    padding: 4px 14px 10px 14px !important;
  }
  table.tabela-metricas-compacta tbody tr td {
    padding: 6px 14px !important;
    white-space: nowrap !important;
  }

  /* =========================================================
     TABELA PRINCIPAL IDÊNTICA À SUA TABELA DT (COM ORDENAÇÃO INVISÍVEL)
     ========================================================= */
  .wrapper-tabela-dt-alf {
    width: 100%;
    overflow-x: auto;
  }
  table.tabela-estilo-dt-alf {
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
    font-size: 12px !important;
    border-collapse: separate !important;
    border-spacing: 0 !important;
    width: 100% !important;
  }
  table.tabela-estilo-dt-alf thead th {
    border: none !important;
    text-transform: none !important;
    font-weight: 600 !important;
    font-size: 12.5px !important;
    color: #111111 !important;
    position: relative !important;
    padding: 8px 8px 16px 8px !important;
    background: transparent !important;
    text-align: center !important;
    white-space: nowrap !important;
    user-select: none !important;
  }
  /* Mãozinha ao passar o mouse nos cabeçalhos ordenáveis (igual à sua tabela DT!) */
  table.tabela-estilo-dt-alf thead th.th-sort-alf {
    cursor: pointer !important;
  }
  table.tabela-estilo-dt-alf thead th i {
    margin-right: 5px !important;
    font-size: 13.5px !important;
    color: #444444 !important;
  }
  /* A barra preta individual de 80% embaixo de cada cabeçalho */
  table.tabela-estilo-dt-alf thead th::after {
    content: '' !important;
    position: absolute !important;
    left: 10% !important;
    bottom: 0 !important;
    width: 80% !important;
    height: 2px !important;
    background-color: #000000 !important;
    border-radius: 4px !important;
  }

  /* Células do corpo com grade fina #e0e0e0 e fundo branco puro */
  table.tabela-estilo-dt-alf tbody tr td {
    padding: 8px 10px !important;
    border-top: none !important;
    border-bottom: 1px solid #e0e0e0 !important;
    border-right: 1px solid #e0e0e0 !important;
    border-left: none !important;
    background-color: #ffffff !important;
    color: #222222 !important;
    text-align: center !important;
    vertical-align: middle !important;
  }
  /* Apenas a 1ª coluna da esquerda leva border-left para não duplicar bordas */
  table.tabela-estilo-dt-alf tbody tr td.cel-primeira-col {
    border-left: 1px solid #e0e0e0 !important;
  }
  table.tabela-estilo-dt-alf tbody tr:hover td {
    background-color: #f9f9f9 !important;
  }
  /* Linha final (ou linha única da tabela de resumo) com cantos arredondados de 12px embaixo */
  table.tabela-estilo-dt-alf tbody tr.linha-total-dt td {
    background-color: #fcfcfc !important;
    border-bottom: 1px solid #e0e0e0 !important;
    color: #000000 !important;
  }
  table.tabela-estilo-dt-alf tbody tr.linha-total-dt td:first-child {
    border-bottom-left-radius: 12px !important;
  }
  table.tabela-estilo-dt-alf tbody tr.linha-total-dt td:last-child {
    border-bottom-right-radius: 12px !important;
  }

  /* Formatação de moeda R$ alinhada igual à coluna Valor da sua tabela DT */
  .celula-moeda-alf {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 6px;
    width: 100%;
    min-width: 74px;
    white-space: nowrap;
  }
  .badge-n {
    color: #64748b;
    font-weight: 600;
    font-size: 11px;
  }

  /* No Celular: esconde o balão flutuante em Barras/Linhas, mas LIBERA no modo Boxplot */
  @media (max-width: 768px) {
    .painel-fluxo-alf:not(.modo-boxplot-ativo) .hoverlayer {
      display: none !important;
    }
    .painel-fluxo-alf {
      margin: 12px auto;
    }
    .titulo-grafico-alf {
      font-size: 13.5px;
      margin-bottom: 6px;
    }
    .barra-controles-alf {
      padding: 9px 11px;
      gap: 8px;
      margin-bottom: 10px;
    }
    .linha-filtros-sup-alf {
      gap: 7px;
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
    .linha-legendas-inf-alf {
      padding-top: 8px;
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
    .legenda-boxplot-direita-alf {
      width: 100%;
      justify-content: flex-start;
      font-size: 10.5px;
      padding: 5px 9px;
    }
    .aba-mob-alf {
      font-size: 11px;
      padding: 5px 10px;
    }
    table.tabela-estilo-dt-alf {
      font-size: 11px !important;
      white-space: nowrap !important;
    }
    table.tabela-estilo-dt-alf thead th {
      font-size: 11.5px !important;
      padding: 7px 6px 14px 6px !important;
    }
    table.tabela-estilo-dt-alf tbody tr td {
      padding: 6px 8px !important;
    }
    table.tabela-metricas-compacta {
      min-width: 100%;
    }
  }
"))

opcoes_html_mes <- purrr::map(meses_disponiveis, function(m) {
  htmltools::tags$option(value = m, m)
})

# 1. Título Principal ACIMA da linha preta
titulo_topo_html <- htmltools::tags$div(class = "titulo-grafico-alf", titulo_nome)

# 2. Caixa Única: Filtros na linha de cima + KPIs Minimalistas na linha de baixo!
controles_e_legenda_html <- htmltools::tags$div(
  class = "barra-controles-alf",
  
  # Linha Superior: Período | Usuário | Visão
  htmltools::tags$div(
    class = "linha-filtros-sup-alf",
    
    htmltools::tags$div(
      class = "grupo-controle-alf",
      htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-calendar3'></i> Período:")),
      htmltools::tags$select(class = "select-mes-alf", opcoes_html_mes)
    ),
    
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
  ),
  
  # Linha Inferior Minimalista (dentro da mesma caixa): Saídas | Entradas | Diferença
  htmltools::tags$div(
    class = "linha-legendas-inf-alf",
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
    ),
    htmltools::tags$div(
      class = "legenda-boxplot-direita-alf",
      htmltools::tags$span(
        class = "item-leg-box-alf",
        htmltools::tags$span(class = "bolinha-leg-box", style = base::paste0("background:", cor_ponto_entradas, ";")),
        "Lançamento"
      ),
      htmltools::tags$span(
        class = "item-leg-box-alf",
        htmltools::tags$span(class = "bolinha-leg-box", style = base::paste0("background:", cor_outlier, ";")),
        "Outlier"
      ),
      htmltools::tags$span(
        class = "item-leg-box-alf",
        htmltools::tags$span(class = "traco-media-leg-box"),
        "Média"
      )
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
    titulo_topo_html,
    controles_e_legenda_html,
    htmltools::tags$div(class = "grafico-scroll", grafico),
    htmltools::tags$div(class = "card-hover-mobile-alf")
  )
)

grafico_fluxo