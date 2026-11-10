# ==============================================================================
# PROJETO ALF — Painel de Gráficos: Empilhado, Linhas e Boxplot (Versão Final)
# ==============================================================================
# Ajustes Finais:
#   1. Indicadores de Saídas, Entradas e Diferença em Cards KPI elegantes
#      (com barra superior na cor da série, título no topo e valor em destaque),
#      100% dinâmicos ao clicar nos meses/dias ou trocar filtros.
#   2. Barra de controles superior e interações mantidas intactas.
#   3. Quartis sincronizados (type = 5 / linear) e tabelas consolidadas com rowspan.

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

# Limite máximo de descrições abertas por Item de Consumo antes de agrupar em "+ X outros"
max_desc_por_item     <- 3

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

# Função que calcula Q1, Mediana (Q2) e Q3 usando EXATAMENTE o algoritmo linear do Plotly (type = 5)
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
# 4. FUNÇÕES DE HOVER E TABELA ÚNICA CONSOLIDADA COM AGRUPAMENTO INTELIGENTE
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

# 4.2 Barra de estatísticas estilizada (Quartis 100% sincronizados com o Plotly!)
montar_barra_metricas <- function(vals, extra_quartis = NULL) {
  val_media <- base::mean(vals, na.rm = TRUE)
  val_max   <- base::max(vals, na.rm = TRUE)
  val_min   <- base::min(vals, na.rm = TRUE)
  val_amp   <- val_max - val_min
  
  bloco_q <- ""
  if (!base::is.null(extra_quartis)) {
    bloco_q <- base::paste0(
      "<div class='pilula-metrica-card'><span>Mediana (Q2):</span> <b>R$ ", formatar_brl(extra_quartis$med), "</b></div>",
      "<div class='pilula-metrica-card'><span>Q1 / Q3:</span> <b>R$ ", formatar_brl(extra_quartis$q1), " / R$ ", formatar_brl(extra_quartis$q3), "</b></div>",
      "<div class='pilula-metrica-card'><span>IIQ:</span> <b>R$ ", formatar_brl(extra_quartis$iiq), "</b></div>"
    )
  }
  
  base::paste0(
    "<div class='barra-metricas-card'>",
    bloco_q,
    "<div class='pilula-metrica-card'><span>Média:</span> <b>R$ ", formatar_brl(val_media), "</b></div>",
    "<div class='pilula-metrica-card'><span>Máx:</span> <b>R$ ", formatar_brl(val_max), "</b></div>",
    "<div class='pilula-metrica-card'><span>Mín:</span> <b>R$ ", formatar_brl(val_min), "</b></div>",
    "<div class='pilula-metrica-card'><span>Amplitude:</span> <b>R$ ", formatar_brl(val_amp), "</b></div>",
    "</div>"
  )
}

# 4.3 Tabela Única Consolidada com Agrupamento Top-N por Item
montar_card_tabela_unificada <- function(df_sub, titulo_periodo, tipo_sel, incluir_quartis = FALSE) {
  cor_titulo <- base::ifelse(tipo_sel == "Entradas", cor_entradas, cor_saidas)
  n_lanc     <- base::nrow(df_sub)
  
  cabecalho_card <- base::paste0(
    "<div class='titulo-card-tab' style='color:", cor_titulo, ";'>",
    titulo_periodo, "</div>"
  )
  
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0(
      cabecalho_card,
      "<div class='sem-dados-card'>Nenhum lançamento registrado neste período.</div>"
    ))
  }
  
  val_total <- base::sum(df_sub$valor, na.rm = TRUE)
  vals      <- df_sub$valor
  
  quartis_lista <- if (incluir_quartis) calcular_quartis_plotly(vals) else NULL
  
  if (tipo_sel == "Entradas") {
    # ENTRADAS: Fonte de Renda | Descrição | Valor | (%) | Total | (%)
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
      f_atual   <- fontes_agr$grupo_1[idx_f]
      n_f       <- fontes_agr$n_fonte[idx_f]
      tot_f     <- fontes_agr$tot_fonte[idx_f]
      pct_f     <- fontes_agr$pct_fonte[idx_f]
      
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
      
      for (j in base::seq_len(k_rows)) {
        borda_fim_grupo <- if (j == k_rows) " class='fim-grupo-row'" else ""
        
        if (j == 1) {
          linhas_html <- base::c(linhas_html, base::paste0(
            "<tr", borda_fim_grupo, ">",
            "<td rowspan='", k_rows, "' class='cel-mesclada'><b>", f_atual, "</b> <span class='badge-n'>(", n_f, ")</span></td>",
            "<td>", itens_f$desc_lbl[j], "</td>",
            "<td class='col-num'>R$ ", formatar_brl(itens_f$val_item[j]), "</td>",
            "<td class='col-num'>", itens_f$pct_item[j], "%</td>",
            "<td rowspan='", k_rows, "' class='col-num cel-mesclada'><b>R$ ", formatar_brl(tot_f), "</b></td>",
            "<td rowspan='", k_rows, "' class='col-num cel-mesclada'><b>", pct_f, "%</b></td>",
            "</tr>"
          ))
        } else {
          linhas_html <- base::c(linhas_html, base::paste0(
            "<tr", borda_fim_grupo, ">",
            "<td>", itens_f$desc_lbl[j], "</td>",
            "<td class='col-num'>R$ ", formatar_brl(itens_f$val_item[j]), "</td>",
            "<td class='col-num'>", itens_f$pct_item[j], "%</td>",
            "</tr>"
          ))
        }
      }
    }
    
    linha_total <- base::paste0(
      "<tr class='linha-total-card'>",
      "<td><b>Total (", n_lanc, ")</b></td>",
      "<td>—</td>",
      "<td class='col-num'><b>R$ ", formatar_brl(val_total), "</b></td>",
      "<td class='col-num'><b>100.0%</b></td>",
      "<td class='col-num'><b>R$ ", formatar_brl(val_total), "</b></td>",
      "<td class='col-num'><b>100.0%</b></td>",
      "</tr>"
    )
    
    tabela_final <- base::paste0(
      "<div class='wrapper-tabela-card'>",
      "<table class='tabela-resumo-card'>",
      "<thead><tr>",
      "<th>Fonte de Renda</th>",
      "<th>Descrição</th>",
      "<th class='col-num'>Valor</th>",
      "<th class='col-num'>(%)</th>",
      "<th class='col-num'>Total</th>",
      "<th class='col-num'>(%)</th>",
      "</tr></thead>",
      "<tbody>", base::paste(linhas_html, collapse = ""), linha_total, "</tbody>",
      "</table></div>"
    )
    
  } else {
    # SAÍDAS CONSOLIDADAS COM COMPACTAÇÃO INTELIGENTE DE DESCRIÇÕES LONGAS
    formas_agr <- df_sub %>%
      dplyr::group_by(grupo_1) %>%
      dplyr::summarise(
        n_forma   = dplyr::n(),
        tot_forma = base::sum(valor, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(tot_forma)) %>%
      dplyr::mutate(
        pct_forma = base::formatC((tot_forma / val_total) * 100, format = "f", digits = 1, decimal.mark = ".")
      )
    
    linhas_html <- base::character(0)
    
    for (idx_f in base::seq_len(base::nrow(formas_agr))) {
      f_atual   <- formas_agr$grupo_1[idx_f]
      n_f       <- formas_agr$n_forma[idx_f]
      tot_f     <- formas_agr$tot_forma[idx_f]
      pct_f     <- formas_agr$pct_forma[idx_f]
      
      df_forma <- df_sub %>% dplyr::filter(grupo_1 == f_atual)
      
      qtd_por_tipo <- df_forma %>%
        dplyr::count(grupo_2, name = "qtd_lanc_tipo")
      
      qtd_por_item <- df_forma %>%
        dplyr::count(grupo_2, item_consumo, name = "qtd_lanc_item")
      
      itens_brutos <- df_forma %>%
        dplyr::group_by(grupo_2, item_consumo, descricao) %>%
        dplyr::summarise(
          val_desc = base::sum(valor, na.rm = TRUE),
          qtd_desc = dplyr::n(),
          .groups  = "drop"
        ) %>%
        dplyr::arrange(grupo_2, item_consumo, dplyr::desc(val_desc))
      
      itens_f <- itens_brutos %>%
        dplyr::group_by(grupo_2, item_consumo) %>%
        dplyr::mutate(
          ordem_desc = dplyr::row_number(),
          total_desc = dplyr::n()
        ) %>%
        dplyr::ungroup()
      
      top_itens <- itens_f %>%
        dplyr::filter(total_desc <= max_desc_por_item | ordem_desc <= max_desc_por_item) %>%
        dplyr::mutate(
          desc_lbl = base::ifelse(
            qtd_desc > 1,
            base::paste0(descricao, " <span class='badge-n'>(", qtd_desc, ")</span>"),
            descricao
          )
        ) %>%
        dplyr::select(grupo_2, item_consumo, desc_lbl, val_desc, ordem_desc)
      
      resto_itens <- itens_f %>%
        dplyr::filter(total_desc > max_desc_por_item & ordem_desc > max_desc_por_item) %>%
        dplyr::group_by(grupo_2, item_consumo) %>%
        dplyr::summarise(
          qtd_extra   = dplyr::n(),
          lanc_extra  = base::sum(qtd_desc),
          lista_extra = base::paste0(
            descricao,
            base::ifelse(qtd_desc > 1, base::paste0(" (", qtd_desc, "x)"), ""),
            ": R$ ", formatar_brl(val_desc),
            collapse = " &#10;• "
          ),
          val_desc    = base::sum(val_desc, na.rm = TRUE),
          ordem_desc  = 999L,
          .groups     = "drop"
        ) %>%
        dplyr::mutate(
          desc_lbl = base::paste0(
            "<details class='detalhes-extras-card'>",
            "<summary>+ ", qtd_extra, " outras descrições <span class='badge-n'>(", lanc_extra, ")</span></summary>",
            "<div class='lista-oculta-card'>• ", base::gsub(" &#10;• ", "<br>• ", lista_extra), "</div>",
            "</details>"
          )
        ) %>%
        dplyr::select(grupo_2, item_consumo, desc_lbl, val_desc, ordem_desc)
      
      itens_compactos <- dplyr::bind_rows(top_itens, resto_itens) %>%
        dplyr::left_join(qtd_por_tipo, by = "grupo_2") %>%
        dplyr::left_join(qtd_por_item, by = base::c("grupo_2", "item_consumo")) %>%
        dplyr::arrange(grupo_2, item_consumo, ordem_desc) %>%
        dplyr::mutate(
          pct_desc = base::formatC((val_desc / val_total) * 100, format = "f", digits = 1, decimal.mark = "."),
          tipo_lbl = base::paste0("<b>", grupo_2, "</b> <span class='badge-n'>(", qtd_lanc_tipo, ")</span>"),
          item_lbl = base::ifelse(qtd_lanc_item > 1, base::paste0(item_consumo, " <span class='badge-n'>(", qtd_lanc_item, ")</span>"), item_consumo)
        )
      
      linhas_por_tipo <- itens_compactos %>%
        dplyr::count(grupo_2, name = "n_rows_tipo")
      
      linhas_por_item <- itens_compactos %>%
        dplyr::count(grupo_2, item_consumo, name = "n_rows_item")
      
      itens_compactos <- itens_compactos %>%
        dplyr::left_join(linhas_por_tipo, by = "grupo_2") %>%
        dplyr::left_join(linhas_por_item, by = base::c("grupo_2", "item_consumo")) %>%
        dplyr::group_by(grupo_2) %>%
        dplyr::mutate(prim_tipo = dplyr::row_number() == 1) %>%
        dplyr::group_by(grupo_2, item_consumo) %>%
        dplyr::mutate(
          prim_item = dplyr::row_number() == 1,
          ult_item  = dplyr::row_number() == dplyr::n()
        ) %>%
        dplyr::ungroup()
      
      k_rows <- base::nrow(itens_compactos)
      
      for (j in base::seq_len(k_rows)) {
        cls_tr <- if (j == k_rows) {
          " class='fim-grupo-row'"
        } else if (itens_compactos$ult_item[j]) {
          " class='fim-subitem-row'"
        } else {
          ""
        }
        
        cel_forma <- if (j == 1) {
          base::paste0("<td rowspan='", k_rows, "' class='cel-mesclada'><div class='sticky-cel'><b>", f_atual, "</b> <span class='badge-n'>(", n_f, ")</span></div></td>")
        } else ""
        
        cel_tipo <- if (itens_compactos$prim_tipo[j]) {
          base::paste0("<td rowspan='", itens_compactos$n_rows_tipo[j], "' class='cel-mesclada-sub'><div class='sticky-cel'>", itens_compactos$tipo_lbl[j], "</div></td>")
        } else ""
        
        cel_item <- if (itens_compactos$prim_item[j]) {
          base::paste0("<td rowspan='", itens_compactos$n_rows_item[j], "' class='cel-mesclada-item'><div class='sticky-cel'>", itens_compactos$item_lbl[j], "</div></td>")
        } else ""
        
        cel_tot_forma <- if (j == 1) {
          base::paste0(
            "<td rowspan='", k_rows, "' class='col-num cel-mesclada'><div class='sticky-cel'><b>R$ ", formatar_brl(tot_f), "</b></div></td>",
            "<td rowspan='", k_rows, "' class='col-num cel-mesclada'><div class='sticky-cel'><b>", pct_f, "%</b></div></td>"
          )
        } else ""
        
        linhas_html <- base::c(linhas_html, base::paste0(
          "<tr", cls_tr, ">",
          cel_forma,
          cel_tipo,
          cel_item,
          "<td>", itens_compactos$desc_lbl[j], "</td>",
          "<td class='col-num'>R$ ", formatar_brl(itens_compactos$val_desc[j]), "</td>",
          "<td class='col-num'>", itens_compactos$pct_desc[j], "%</td>",
          cel_tot_forma,
          "</tr>"
        ))
      }
    }
    
    linha_total <- base::paste0(
      "<tr class='linha-total-card'>",
      "<td><b>Total (", n_lanc, ")</b></td>",
      "<td>—</td>",
      "<td>—</td>",
      "<td>—</td>",
      "<td class='col-num'><b>R$ ", formatar_brl(val_total), "</b></td>",
      "<td class='col-num'><b>100.0%</b></td>",
      "<td class='col-num'><b>R$ ", formatar_brl(val_total), "</b></td>",
      "<td class='col-num'><b>100.0%</b></td>",
      "</tr>"
    )
    
    tabela_final <- base::paste0(
      "<div class='wrapper-tabela-card'>",
      "<table class='tabela-resumo-card'>",
      "<thead><tr>",
      "<th>Forma de Pagamento</th>",
      "<th>Tipo de Despesa</th>",
      "<th>Item de Consumo</th>",
      "<th>Descrição</th>",
      "<th class='col-num'>Valor</th>",
      "<th class='col-num'>(%)</th>",
      "<th class='col-num'>Total</th>",
      "<th class='col-num'>(%)</th>",
      "</tr></thead>",
      "<tbody>", base::paste(linhas_html, collapse = ""), linha_total, "</tbody>",
      "</table></div>"
    )
  }
  
  base::paste0(
    cabecalho_card,
    tabela_final,
    montar_barra_metricas(vals, extra_quartis = quartis_lista)
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
  
  # Vetores para alimentar o Boxplot
  box_cats        <- base::as.character(escopo_meses$eixo_x)
  box_e_x         <- base::character(0)
  box_e_y         <- base::numeric(0)
  box_s_x         <- base::character(0)
  box_s_y         <- base::numeric(0)
  
  # Pontos individuais do Boxplot
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
  
  # Linha tracejada da Média de cada Boxplot
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
    
    # --- ENTRADAS NO MÊS i (Quartis com type = 5 idênticos ao Plotly) ---
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
    
    # --- SAÍDAS NO MÊS i (Quartis com type = 5 idênticos ao Plotly) ---
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
    n_pts       <- base::nrow(escopo_meses)
    eixo_x_vec  <- base::as.character(escopo_meses$eixo_x)
    y_e         <- base::numeric(n_pts)
    y_s         <- base::numeric(n_pts)
    hov_e       <- base::character(n_pts)
    hov_s       <- base::character(n_pts)
    card_e_html <- base::character(n_pts)
    card_s_html <- base::character(n_pts)
    
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
      eixo_x_vec  <- base::c("<b>Sem lançamentos</b>")
      y_e         <- base::c(0)
      y_s         <- base::c(0)
      hov_e       <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Entradas"))
      hov_s       <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Saídas"))
      card_e_html <- base::c(montar_card_tabela_unificada(d_mes[0, ], mes_sel, "Entradas", FALSE))
      card_s_html <- base::c(montar_card_tabela_unificada(d_mes[0, ], mes_sel, "Saídas",   FALSE))
    } else {
      n_pts       <- base::length(dias_ativos)
      eixo_x_vec  <- base::paste0("<b>", base::format(dias_ativos, "%d"), "</b>")
      y_e         <- base::numeric(n_pts)
      y_s         <- base::numeric(n_pts)
      hov_e       <- base::character(n_pts)
      hov_s       <- base::character(n_pts)
      card_e_html <- base::character(n_pts)
      card_s_html <- base::character(n_pts)
      
      for (i in base::seq_len(n_pts)) {
        dia_i   <- dias_ativos[i]
        lbl_dia <- base::format(dia_i, "%d/%m/%Y")
        
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
  
  val_e_fmt   <- base::paste0("R$ ", formatar_brl(tot_e))
  val_s_fmt   <- base::paste0("R$ ", formatar_brl(tot_s))
  val_dif_fmt <- base::paste0(sinal, "R$ ", formatar_brl(base::abs(dif)))
  
  nome_e   <- base::paste0("<b>Entradas: ", val_e_fmt, "</b>")
  nome_s   <- base::paste0("<b>Saídas: ", val_s_fmt, "</b>")
  nome_dif <- base::paste0("<b>Diferença: ", val_dif_fmt, "</b>")
  
  base::list(
    x           = base::as.character(eixo_x_vec),
    e_y         = base::as.numeric(y_e),
    s_y         = base::as.numeric(y_s),
    e_txt_bar   = base::as.character(txt_e_bar),
    s_txt_bar   = base::as.character(txt_s_bar),
    e_txt_line  = base::as.character(txt_e_line),
    s_txt_line  = base::as.character(txt_s_line),
    e_hover     = base::as.character(hov_e),
    s_hover     = base::as.character(hov_s),
    card_e      = base::as.character(card_e_html),
    card_s      = base::as.character(card_s_html),
    # Vetores do Boxplot
    box_cats    = base::as.character(box_cats),
    box_e_x     = base::as.character(box_e_x),
    box_e_y     = base::as.numeric(box_e_y),
    box_s_x     = base::as.character(box_s_x),
    box_s_y     = base::as.numeric(box_s_y),
    pts_e_x     = base::as.numeric(pts_e_x),
    pts_e_y     = base::as.numeric(pts_e_y),
    pts_e_hov   = base::as.character(pts_e_hov),
    pts_e_cor   = base::as.character(pts_e_cor),
    pts_e_tam   = base::as.numeric(pts_e_tam),
    pts_s_x     = base::as.numeric(pts_s_x),
    pts_s_y     = base::as.numeric(pts_s_y),
    pts_s_hov   = base::as.character(pts_s_hov),
    pts_s_cor   = base::as.character(pts_s_cor),
    pts_s_tam   = base::as.numeric(pts_s_tam),
    med_e_x0    = base::as.numeric(med_e_x0),
    med_e_x1    = base::as.numeric(med_e_x1),
    med_e_y     = base::as.numeric(med_e_y),
    med_s_x0    = base::as.numeric(med_s_x0),
    med_s_x1    = base::as.numeric(med_s_x1),
    med_s_y     = base::as.numeric(med_s_y),
    box_res_e_y = base::as.numeric(box_res_e_y),
    box_res_s_y = base::as.numeric(box_res_s_y),
    box_card_e  = base::as.character(box_card_e_html),
    box_card_s  = base::as.character(box_card_s_html),
    e_nome      = nome_e,
    s_nome      = nome_s,
    dif_nome    = nome_dif,
    e_val_fmt   = val_e_fmt,
    s_val_fmt   = val_s_fmt,
    dif_val_fmt = val_dif_fmt,
    dif_cor     = base::ifelse(dif >= 0, "#15803d", "#b91c1c"),
    max_bar     = base::as.numeric(max_bar)[1],
    max_line    = base::as.numeric(max_line)[1],
    max_box     = base::as.numeric(max_box)[1]
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
# 7. CONSTRUÇÃO DO GRÁFICO PLOTLY + ONRENDER (COM CARDS KPI DINÂMICOS)
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

  const selMes         = painel.querySelector('.select-mes-alf');
  const botoesUsuario  = painel.querySelectorAll('.btn-usuario-alf');
  const botoesModo     = painel.querySelectorAll('.btn-modo-alf');
  
  // Elementos dos 3 Cards KPI (Saídas, Entradas, Diferença)
  const kpiTitSaidas   = painel.querySelector('.kpi-titulo-saidas');
  const kpiValSaidas   = painel.querySelector('.kpi-valor-saidas');
  const kpiTitEntradas = painel.querySelector('.kpi-titulo-entradas');
  const kpiValEntradas = painel.querySelector('.kpi-valor-entradas');
  const kpiTitDif      = painel.querySelector('.kpi-titulo-diferenca');
  const kpiValDif      = painel.querySelector('.kpi-valor-diferenca');
  const kpiCardDif     = painel.querySelector('.kpi-card-diferenca');
  const quadDiferenca  = painel.querySelector('.quadrado-diferenca');
  
  const legBoxplotDir  = painel.querySelector('.legenda-boxplot-direita-alf');
  const cardDetalhes   = painel.querySelector('.card-hover-mobile-alf');

  function formatarMoedaJS(v) {
    return v.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  }

  function obterCategoriasAtuais(d) {
    return (modoAtual === 'boxplot') ? (d.box_cats || []) : (d.x || []);
  }

  function atualizarCardInterativo(d) {
    if (!cardDetalhes) return;
    const cats = obterCategoriasAtuais(d);

    if (indiceClicado === null || !cats.length || indiceClicado >= cats.length) {
      cardDetalhes.style.display = 'none';
      cardDetalhes.innerHTML = '';
      return;
    }

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

      if (kpiTitSaidas)   kpiTitSaidas.textContent   = 'Saídas (' + rotX + ')';
      if (kpiValSaidas)   kpiValSaidas.textContent   = 'R$ ' + formatarMoedaJS(valS);
      if (kpiTitEntradas) kpiTitEntradas.textContent = 'Entradas (' + rotX + ')';
      if (kpiValEntradas) kpiValEntradas.textContent = 'R$ ' + formatarMoedaJS(valE);
      if (kpiTitDif)      kpiTitDif.textContent      = 'Diferença (' + rotX + ')';
      if (kpiValDif)      kpiValDif.textContent      = sinal + 'R$ ' + formatarMoedaJS(Math.abs(dif));
      if (kpiValDif)      kpiValDif.style.color      = corD;
      if (quadDiferenca)  quadDiferenca.style.background = corD;
      if (kpiCardDif)     kpiCardDif.style.borderTopColor = corD;
    } else {
      const valE   = Array.isArray(d.e_val_fmt)   ? d.e_val_fmt[0]   : d.e_val_fmt;
      const valS   = Array.isArray(d.s_val_fmt)   ? d.s_val_fmt[0]   : d.s_val_fmt;
      const valDif = Array.isArray(d.dif_val_fmt) ? d.dif_val_fmt[0] : d.dif_val_fmt;
      const corDif = Array.isArray(d.dif_cor)     ? d.dif_cor[0]     : d.dif_cor;

      if (kpiTitSaidas)   kpiTitSaidas.textContent   = 'Saídas';
      if (kpiValSaidas)   kpiValSaidas.textContent   = valS;
      if (kpiTitEntradas) kpiTitEntradas.textContent = 'Entradas';
      if (kpiValEntradas) kpiValEntradas.textContent = valE;
      if (kpiTitDif)      kpiTitDif.textContent      = 'Diferença';
      if (kpiValDif)      kpiValDif.textContent      = valDif;
      if (kpiValDif)      kpiValDif.style.color      = corDif;
      if (quadDiferenca)  quadDiferenca.style.background = corDif;
      if (kpiCardDif)     kpiCardDif.style.borderTopColor = corDif;
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
# 8. ESTILO CSS E ESTRUTURA HTML (COM CARDS KPI MODERNOS E DINÂMICOS)
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
    margin-bottom: 10px;
  }
  .titulo-grafico-alf {
    color: ", titulo_cor, ";
    font-size: ", titulo_tamanho, "px;
    font-weight: 700;
    margin: 0 0 12px 0;
    line-height: 1.25;
  }
  .linha-legendas-topo-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 14px;
    width: 100%;
  }

  /* =========================================================
     CARDS KPI (CAIXINHAS ELEGANTES DE SAÍDAS, ENTRADAS E DIFERENÇA)
     ========================================================= */
  .grade-kpis-alf {
    display: grid;
    grid-template-columns: repeat(3, minmax(165px, 215px));
    gap: 12px;
    margin: 0;
    padding: 0;
  }
  .kpi-card-alf {
    background: #ffffff;
    border: 1px solid #e2e8f0;
    border-radius: 10px;
    padding: 10px 14px;
    box-shadow: 0 2px 8px rgba(15, 23, 42, 0.04);
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: flex-start;
    gap: 4px;
    transition: transform 0.18s ease, box-shadow 0.18s ease;
  }
  .kpi-card-alf:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(15, 23, 42, 0.08);
  }
  .kpi-card-saidas {
    border-top: 3.5px solid ", cor_saidas, ";
  }
  .kpi-card-entradas {
    border-top: 3.5px solid ", cor_entradas, ";
  }
  .kpi-card-diferenca {
    border-top: 3.5px solid ", estado_ini$dif_cor[1], ";
  }
  .kpi-topo-alf {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    font-size: 11.5px;
    font-weight: 600;
    color: #64748b;
    letter-spacing: 0.01em;
  }
  .quadrado-legenda-alf {
    width: 10px;
    height: 10px;
    border-radius: 2.5px;
    display: inline-block;
    flex-shrink: 0;
  }
  .kpi-valor-alf {
    font-size: 16.5px;
    font-weight: 800;
    color: #0f172a;
    line-height: 1.2;
    letter-spacing: -0.02em;
    white-space: nowrap;
  }

  .legenda-boxplot-direita-alf {
    display: none;
    flex-wrap: wrap;
    align-items: center;
    gap: 14px;
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 8px;
    padding: 7px 13px;
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
     CARD INFERIOR COM TABELA COMPACTA E DETALHES EXPANSÍVEIS
     ========================================================= */
  .card-hover-mobile-alf {
    display: none;
    width: 100%;
    box-sizing: border-box;
    margin-top: 12px;
    padding: 14px 18px;
    background: #ffffff;
    border: 1.5px solid #000000;
    border-radius: 12px;
    font-size: 12px;
    line-height: 1.45;
    color: #000000;
    text-align: left;
    box-shadow: 0 4px 14px rgba(0,0,0,0.07);
  }
  .topo-card-mobile-alf {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 10px;
    padding-bottom: 8px;
    border-bottom: 1px solid #e5e7eb;
  }
  .abas-card-mobile-alf {
    display: inline-flex;
    gap: 6px;
  }
  .aba-mob-alf {
    font-family: inherit;
    font-size: 12px;
    font-weight: 600;
    padding: 5px 14px;
    border-radius: 6px;
    border: 1px solid #d1d5db;
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
    font-size: 20px;
    line-height: 1;
    color: #555555;
    cursor: pointer;
    padding: 0 4px;
  }
  .titulo-card-tab {
    font-size: 14px;
    font-weight: 700;
    margin-bottom: 10px;
    letter-spacing: -0.01em;
  }
  .wrapper-tabela-card {
    width: 100%;
    max-height: 360px;
    overflow-y: auto;
    overflow-x: auto;
    margin-bottom: 12px;
    border-bottom: 1.5px solid #000000;
  }
  .tabela-resumo-card {
    width: 100%;
    border-collapse: collapse;
    font-size: 12px;
  }
  .tabela-resumo-card thead th {
    position: sticky;
    top: 0;
    z-index: 2;
    text-align: center;
    font-weight: 700;
    color: #0f172a;
    background: #f8fafc;
    padding: 8px 10px;
    border-top: 1.5px solid #000000;
    border-bottom: 1.5px solid #000000;
    white-space: nowrap;
  }
  .tabela-resumo-card td {
    text-align: center;
    padding: 6px 10px;
    border-bottom: 1px solid #f1f5f9;
    color: #1e293b;
    vertical-align: middle;
  }
  .tabela-resumo-card .cel-mesclada {
    background: #fcfcfd;
    border-right: 1px solid #e2e8f0;
    border-left: 1px solid #e2e8f0;
    vertical-align: top;
    padding-top: 10px;
  }
  .tabela-resumo-card .cel-mesclada-sub {
    background: #ffffff;
    border-right: 1px solid #e2e8f0;
    vertical-align: top;
    padding-top: 10px;
    color: #1e293b;
  }
  .tabela-resumo-card .cel-mesclada-item {
    background: #ffffff;
    border-right: 1px solid #f1f5f9;
    vertical-align: top;
    padding-top: 8px;
    font-weight: 600;
    color: #334155;
  }
  .sticky-cel {
    position: sticky;
    top: 38px;
  }
  .tabela-resumo-card tr.fim-subitem-row td {
    border-bottom: 1px solid #cbd5e1;
  }
  .tabela-resumo-card tr.fim-grupo-row td {
    border-bottom: 1.5px solid #94a3b8;
  }
  .tabela-resumo-card tr.linha-total-card td {
    position: sticky;
    bottom: 0;
    z-index: 2;
    background: #f8fafc;
    border-top: 1.5px solid #000000;
    border-bottom: 1.5px solid #000000;
    font-weight: 700;
    color: #000000;
    padding: 8px 10px;
  }
  .detalhes-extras-card {
    display: inline-block;
    text-align: left;
  }
  .detalhes-extras-card summary {
    cursor: pointer;
    color: #0369a1;
    font-weight: 600;
    font-size: 11px;
    list-style: none;
    background: #f0f9ff;
    border: 1px solid #bae6fd;
    padding: 2px 8px;
    border-radius: 12px;
    display: inline-block;
  }
  .detalhes-extras-card summary::-webkit-details-marker {
    display: none;
  }
  .lista-oculta-card {
    margin-top: 5px;
    padding: 6px 8px;
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    font-size: 11px;
    color: #334155;
    line-height: 1.4;
  }
  .badge-n {
    color: #64748b;
    font-weight: 600;
    font-size: 11px;
  }
  .tabela-resumo-card .col-num {
    text-align: center;
    white-space: nowrap;
  }
  .barra-metricas-card {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 10px;
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 8px;
    padding: 8px 12px;
    font-size: 11.5px;
    color: #1e293b;
  }
  .pilula-metrica-card {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    background: #ffffff;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    padding: 4px 10px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.02);
  }
  .pilula-metrica-card span {
    color: #64748b;
    font-weight: 600;
  }
  .pilula-metrica-card b {
    color: #0f172a;
    font-weight: 700;
  }

  /* Responsividade Mobile para os Cards KPI e Tabelas */
  @media (max-width: 768px) {
    .painel-fluxo-alf:not(.modo-boxplot-ativo) .hoverlayer {
      display: none !important;
    }
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
    .grade-kpis-alf {
      grid-template-columns: repeat(3, 1fr);
      gap: 6px;
      width: 100%;
    }
    .kpi-card-alf {
      padding: 7px 8px;
      border-radius: 8px;
    }
    .kpi-topo-alf {
      font-size: 10px;
      gap: 4px;
    }
    .quadrado-legenda-alf {
      width: 8px;
      height: 8px;
    }
    .kpi-valor-alf {
      font-size: 12px;
    }
    .legenda-boxplot-direita-alf {
      width: 100%;
      justify-content: flex-start;
      font-size: 10.5px;
      padding: 5px 9px;
    }
    .card-hover-mobile-alf {
      padding: 10px 12px;
    }
    .tabela-resumo-card {
      font-size: 11px;
    }
    .tabela-resumo-card th,
    .tabela-resumo-card td {
      padding: 5px 7px;
    }
    .barra-metricas-card {
      gap: 6px;
      padding: 6px 8px;
    }
    .pilula-metrica-card {
      padding: 3px 7px;
      font-size: 10.5px;
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

# Cabeçalho com os 3 Cards KPI (Saídas, Entradas e Diferença) e Legenda lateral do Boxplot
cabecalho_html <- htmltools::tags$div(
  class = "cabecalho-grafico-alf",
  htmltools::tags$div(class = "titulo-grafico-alf", titulo_nome),
  htmltools::tags$div(
    class = "linha-legendas-topo-alf",
    htmltools::tags$div(
      class = "grade-kpis-alf",
      
      # Card 1: Saídas
      htmltools::tags$div(
        class = "kpi-card-alf kpi-card-saidas",
        htmltools::tags$div(
          class = "kpi-topo-alf",
          htmltools::tags$span(class = "quadrado-legenda-alf", style = base::paste0("background:", cor_saidas, ";")),
          htmltools::tags$span(class = "kpi-titulo-saidas", "Saídas")
        ),
        htmltools::tags$div(class = "kpi-valor-alf kpi-valor-saidas", estado_ini$s_val_fmt[1])
      ),
      
      # Card 2: Entradas
      htmltools::tags$div(
        class = "kpi-card-alf kpi-card-entradas",
        htmltools::tags$div(
          class = "kpi-topo-alf",
          htmltools::tags$span(class = "quadrado-legenda-alf", style = base::paste0("background:", cor_entradas, ";")),
          htmltools::tags$span(class = "kpi-titulo-entradas", "Entradas")
        ),
        htmltools::tags$div(class = "kpi-valor-alf kpi-valor-entradas", estado_ini$e_val_fmt[1])
      ),
      
      # Card 3: Diferença (Dinâmico Verde/Vermelho)
      htmltools::tags$div(
        class = "kpi-card-alf kpi-card-diferenca",
        htmltools::tags$div(
          class = "kpi-topo-alf",
          htmltools::tags$span(class = "quadrado-legenda-alf quadrado-diferenca", style = base::paste0("background:", estado_ini$dif_cor[1], ";")),
          htmltools::tags$span(class = "kpi-titulo-diferenca", "Diferença")
        ),
        htmltools::tags$div(
          class = "kpi-valor-alf kpi-valor-diferenca",
          style = base::paste0("color:", estado_ini$dif_cor[1], ";"),
          estado_ini$dif_val_fmt[1]
        )
      )
    ),
    
    # Legenda lateral do Boxplot
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
    controles_html,
    cabecalho_html,
    htmltools::tags$div(class = "grafico-scroll", grafico),
    htmltools::tags$div(class = "card-hover-mobile-alf")
  )
)

grafico_fluxo