# ==============================================================================
# PROJETO ALF — Gráfico: Fluxo de Entradas e Saídas (Mensal e Diário)
# ==============================================================================
# Controles HTML customizados com fotos redondas (Alex / Letícia),
# cabeçalho 100% alinhado à esquerda, 3º rótulo de Diferença (Entradas - Saídas),
# eixo X diário enxuto (apenas dias com movimento), rótulos com tamanho fixo
# padronizado e hover analítico com (n = ...) por Tipo de Despesa e Forma de Pagamento.

# ==============================================================================
# 1. CONFIGURAÇÕES VISUAIS (PADRÃO ALEX + PALETA AZUL/CIANO)
# ==============================================================================

titulo_nome     <- "Fluxo Financeiro: Entradas e Saídas"
subtitulo_texto <- "Visão consolidada mensal e detalhamento diário (saídas pela data de pagamento)"

titulo_cor      <- "#03045e"
titulo_tamanho  <- 16
sub_cor         <- "#555555"
sub_tamanho     <- 13

# Cores das séries (Paleta Azul/Ciano do Modelo)
cor_entradas    <- "#29b6d8"   # Azul Ciano Vibrante (Base)
cor_saidas      <- "#1b78a1"   # Azul Oceano Escuro (Topo)
cor_diferenca   <- "#03045e"   # Azul Marinho Profundo (3º Rótulo: Diferença)

tamanho_rotulos <- 10.5
cor_rotulos     <- "#000000"
tamanho_eixo_x  <- 11
cor_eixo_x      <- "#000000"
tamanho_legenda <- 12.5

# Balão de Hover (Fundo branco e borda preta padrão ABNT/Alex)
hover_bg_cor      <- "#FFFFFF"
hover_txt_cor     <- "#000000"
hover_fonte_tam   <- 12
borda_balao_hover <- "#000000"

raio_cantos           <- 8
altura_grafico        <- 460 # Altura no Notebook/PC
altura_grafico_mobile <- 310 # Altura reduzida e proporcional no Celular

incluir_a_pagar    <- TRUE
descricao_no_hover <- FALSE


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
    tipo            = "Entradas",
    data            = base::as.Date(data),
    valor           = base::as.numeric(valor),
    usuario         = padronizar_usuario(usuario_id),
    fonte_ou_tipo   = dplyr::coalesce(base::as.character(fonte_renda), "Outras Fontes"),
    forma_pagamento = dplyr::coalesce(base::as.character(frequencia_recebimento), "Única"),
    item_consumo    = dplyr::coalesce(base::as.character(fonte_renda), "Entrada"),
    descricao       = base::as.character(descricao),
    status          = "Recebido"
  )

saidas_fluxo <- bd_saidas %>%
  dplyr::filter(!base::is.na(data_pagamento)) %>%
  dplyr::transmute(
    tipo            = "Saídas",
    data            = base::as.Date(data_pagamento),
    valor           = base::as.numeric(valor),
    usuario         = padronizar_usuario(usuario_id),
    fonte_ou_tipo   = dplyr::coalesce(base::as.character(tipo_despesa), "variáveis"),
    forma_pagamento = dplyr::coalesce(base::as.character(forma_pagamento), "Outros"),
    item_consumo    = dplyr::coalesce(base::as.character(item_consumo), "Outros"),
    descricao       = base::as.character(descricao),
    status          = dplyr::coalesce(base::as.character(status), "A pagar")
  )

if (!incluir_a_pagar) {
  saidas_fluxo <- saidas_fluxo %>% dplyr::filter(status == "Pago")
}

fluxo <- dplyr::bind_rows(entradas_fluxo, saidas_fluxo) %>%
  dplyr::mutate(
    descricao     = dplyr::coalesce(descricao, "Sem descrição"),
    mes_num       = base::as.integer(base::format(data, "%m")),
    ano_num       = base::as.integer(base::format(data, "%Y")),
    mes_ref       = base::format(data, "%Y-%m"),
    mes_nome      = meses_pt[mes_num],
    mes_label     = base::paste0(mes_nome, "/", ano_num)
  )

tabela_meses <- fluxo %>%
  dplyr::distinct(mes_ref, mes_label, mes_nome) %>%
  dplyr::arrange(mes_ref)

meses_disponiveis <- base::c("Todos os Meses", tabela_meses$mes_label)
opcoes_usuario    <- base::c("Todos", "Alex", "Leticia")


# ==============================================================================
# 4. FUNÇÃO DE HOVER INTELIGENTE (COM n = , TIPO DE DESPESA E FORMA DE PAGAMENTO)
# ==============================================================================

montar_hover_inteligente <- function(df_sub, titulo_periodo, tipo_sel) {
  n_lanc <- base::nrow(df_sub)
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0("<b>", titulo_periodo, "</b><br>", tipo_sel, ": R$ 0,00 (n = 0)"))
  }
  
  val_total <- base::sum(df_sub$valor, na.rm = TRUE)
  val_media <- base::mean(df_sub$valor, na.rm = TRUE)
  val_maior <- base::max(df_sub$valor, na.rm = TRUE)
  
  # Cabeçalho com (n = ...)
  cabecalho <- base::paste0(
    "<b>", titulo_periodo, "</b><br>",
    "<b>", tipo_sel, ": R$ ", formatar_brl(val_total), "</b> (n = ", n_lanc, ")<br>"
  )
  
  if (tipo_sel == "Saídas") {
    # 1. Agrupamento por Forma de Pagamento (credito, corrente, etc.) com n =
    res_pgto <- df_sub %>%
      dplyr::group_by(forma_pagamento) %>%
      dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(soma)) %>%
      dplyr::mutate(
        pct   = base::round((soma / val_total) * 100, 1),
        linha = base::paste0("• ", forma_pagamento, ": R$ ", formatar_brl(soma), " (", pct, "% | n = ", qtd, ")")
      )
    bloco_1 <- base::paste0("<br><b>Por Forma de Pagamento:</b><br>", base::paste(res_pgto$linha, collapse = "<br>"))
    
    # 2. Agrupamento por Tipo de Despesa (variáveis, fixas, etc.) com n =
    res_tipo <- df_sub %>%
      dplyr::group_by(fonte_ou_tipo) %>%
      dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(soma)) %>%
      dplyr::mutate(
        pct   = base::round((soma / val_total) * 100, 1),
        linha = base::paste0("• ", fonte_ou_tipo, ": R$ ", formatar_brl(soma), " (", pct, "% | n = ", qtd, ")")
      )
    bloco_2 <- base::paste0("<br><b>Por Tipo de Despesa:</b><br>", base::paste(res_tipo$linha, collapse = "<br>"))
    
  } else {
    # Para Entradas: Fonte de Renda e Frequência com n =
    res_fonte <- df_sub %>%
      dplyr::group_by(fonte_ou_tipo) %>%
      dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(soma)) %>%
      dplyr::mutate(
        pct   = base::round((soma / val_total) * 100, 1),
        linha = base::paste0("• ", fonte_ou_tipo, ": R$ ", formatar_brl(soma), " (", pct, "% | n = ", qtd, ")")
      )
    bloco_1 <- base::paste0("<br><b>Por Fonte de Renda:</b><br>", base::paste(res_fonte$linha, collapse = "<br>"))
    
    res_freq <- df_sub %>%
      dplyr::group_by(forma_pagamento) %>%
      dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(soma)) %>%
      dplyr::mutate(
        pct   = base::round((soma / val_total) * 100, 1),
        linha = base::paste0("• ", forma_pagamento, ": R$ ", formatar_brl(soma), " (", pct, "% | n = ", qtd, ")")
      )
    bloco_2 <- base::paste0("<br><b>Por Frequência:</b><br>", base::paste(res_freq$linha, collapse = "<br>"))
  }
  
  rodape_stats <- base::paste0(
    "<br><br><span style='color:#444444; font-size:11px'>",
    "Média/lanç.: R$ ", formatar_brl(val_media), " | Maior: R$ ", formatar_brl(val_maior),
    "</span>"
  )
  
  base::paste0(cabecalho, bloco_1, bloco_2, rodape_stats)
}


# ==============================================================================
# 5. GERADOR DE PARES DE CAMADAS (SINCRONIZA EIXO X E DIAS COM MOVIMENTO)
# ==============================================================================

gerar_par_traces <- function(mes_sel, usuario_sel) {
  
  d <- fluxo
  if (usuario_sel != "Todos") {
    d <- d %>% dplyr::filter(usuario == usuario_sel)
  }
  
  if (mes_sel == "Todos os Meses") {
    usar_ano <- dplyr::n_distinct(fluxo$ano_num) > 1
    
    esqueleto <- tabela_meses %>%
      dplyr::mutate(
        eixo_x = if (usar_ano) base::paste0("<b>", mes_label, "</b>") else base::paste0("<b>", mes_nome, "</b>")
      )
    
    n_pts <- base::nrow(esqueleto)
    y_e   <- base::numeric(n_pts)
    y_s   <- base::numeric(n_pts)
    hov_e <- base::character(n_pts)
    hov_s <- base::character(n_pts)
    
    for (i in base::seq_len(n_pts)) {
      m_ref <- esqueleto$mes_ref[i]
      m_lbl <- esqueleto$mes_label[i]
      
      sub_e <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Entradas")
      sub_s <- d %>% dplyr::filter(mes_ref == m_ref, tipo == "Saídas")
      
      y_e[i]   <- base::sum(sub_e$valor, na.rm = TRUE)
      y_s[i]   <- base::sum(sub_s$valor, na.rm = TRUE)
      hov_e[i] <- montar_hover_inteligente(sub_e, m_lbl, "Entradas")
      hov_s[i] <- montar_hover_inteligente(sub_s, m_lbl, "Saídas")
    }
    
    eixo_x_vec <- base::as.character(esqueleto$eixo_x)
    
  } else {
    # Visão Diária: mostra no eixo X APENAS os dias que possuem dados (Entradas ou Saídas > 0)
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
      hov_e      <- base::c(base::paste0("<b>", mes_sel, "</b><br>Entradas: R$ 0,00 (n = 0)"))
      hov_s      <- base::c(base::paste0("<b>", mes_sel, "</b><br>Saídas: R$ 0,00 (n = 0)"))
    } else {
      n_pts      <- base::length(dias_ativos)
      eixo_x_vec <- base::paste0("<b>", base::format(dias_ativos, "%d"), "</b>")
      y_e        <- base::numeric(n_pts)
      y_s        <- base::numeric(n_pts)
      hov_e      <- base::character(n_pts)
      hov_s      <- base::character(n_pts)
      
      for (i in base::seq_len(n_pts)) {
        dia_i   <- dias_ativos[i]
        lbl_dia <- base::format(dia_i, "%d/%m/%Y")
        
        sub_e <- d_mes %>% dplyr::filter(data == dia_i, tipo == "Entradas")
        sub_s <- d_mes %>% dplyr::filter(data == dia_i, tipo == "Saídas")
        
        y_e[i]   <- base::sum(sub_e$valor, na.rm = TRUE)
        y_s[i]   <- base::sum(sub_s$valor, na.rm = TRUE)
        hov_e[i] <- montar_hover_inteligente(sub_e, lbl_dia, "Entradas")
        hov_s[i] <- montar_hover_inteligente(sub_s, lbl_dia, "Saídas")
      }
    }
  }
  
  # Cálculo do teto do eixo Y (Barras Empilhadas vs Linhas)
  soma_topo <- y_e + y_s
  max_bar   <- base::max(base::c(soma_topo, 100), na.rm = TRUE) * 1.22
  max_line  <- base::max(base::c(y_e, y_s, 100), na.rm = TRUE) * 1.28
  
  # Rótulos das Barras: se a barra de Entrada tiver menos de 7% da altura do gráfico,
  # oculta o texto interno para nunca encolher a fonte ou sair da barra; mantém no modo Linhas!
  txt_e_bar  <- base::ifelse(y_e >= (max_bar * 0.07), formatar_brl_curto(y_e), "")
  txt_s_bar  <- base::ifelse(y_s > 0, formatar_brl_curto(y_s), "")
  txt_e_line <- formatar_brl_curto(y_e)
  txt_s_line <- formatar_brl_curto(y_s)
  
  tot_e <- base::sum(y_e, na.rm = TRUE)
  tot_s <- base::sum(y_s, na.rm = TRUE)
  dif   <- tot_e - tot_s
  sinal <- base::ifelse(dif > 0, "+", base::ifelse(dif < 0, "-", ""))
  
  nome_e   <- base::paste0("<b>Entradas (Total: R$ ", formatar_brl(tot_e), ")</b>")
  nome_s   <- base::paste0("<b>Saídas (Total: R$ ", formatar_brl(tot_s), ")</b>")
  nome_dif <- base::paste0("<b>Diferença (Total: ", sinal, "R$ ", formatar_brl(base::abs(dif)), ")</b>")
  
  base::list(
    x          = base::as.character(eixo_x_vec),
    e_y        = base::as.numeric(y_e),
    s_y        = base::as.numeric(y_s),
    e_txt_bar  = base::as.character(txt_e_bar),
    s_txt_bar  = base::as.character(txt_s_bar),
    e_txt_line = base::as.character(txt_e_line),
    s_txt_line = base::as.character(txt_s_line),
    e_hover    = base::as.character(hov_e),
    s_hover    = base::as.character(hov_s),
    e_nome     = nome_e,
    s_nome     = nome_s,
    dif_nome   = nome_dif,
    dif_cor    = base::ifelse(dif >= 0, "#15803d", "#b91c1c"),
    max_bar    = base::as.numeric(max_bar)[1],
    max_line   = base::as.numeric(max_line)[1]
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
# 7. CONSTRUÇÃO DO GRÁFICO PLOTLY + ONRENDER (TAMANHO DE FONTE FIXO E LINHAS LIMPAS)
# ==============================================================================

estado_ini <- banco_estados[["Todos os Meses"]][["Todos"]]

grafico <- plotly::plot_ly(height = altura_grafico) %>%
  # Série 0: Entradas (Base Azul Ciano)
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
  # Série 1: Saídas (Topo Azul Oceano)
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
    font            = base::list(family = "Inter, sans-serif"),
    separators      = ",.",
    barmode         = "stack",
    barcornerradius = raio_cantos,
    bargap          = 0.25,
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
    margin        = base::list(t = 28, b = 45, l = 65, r = 15),
    plot_bgcolor  = "white",
    paper_bgcolor = "white"
  ) %>%
  plotly::config(displayModeBar = FALSE, responsive = TRUE) %>%
  htmlwidgets::onRender(base::paste0("
    function(el, x) {
      const DADOS = ", json_estados, ";
      const painel = el.closest('.painel-fluxo-alf');
      if (!painel) return;

      let mesAtual     = 'Todos os Meses';
      let usuarioAtual = 'Todos';
      let modoAtual    = 'barras';

      const selMes        = painel.querySelector('.select-mes-alf');
      const botoesUsuario = painel.querySelectorAll('.btn-usuario-alf');
      const botoesModo    = painel.querySelectorAll('.btn-modo-alf');
      const legEntradas   = painel.querySelector('.leg-texto-entradas');
      const legSaidas     = painel.querySelector('.leg-texto-saidas');
      const legDiferenca  = painel.querySelector('.leg-texto-diferenca');
      const quadDiferenca = painel.querySelector('.quadrado-diferenca');

      function atualizarGrafico() {
        const d = DADOS[mesAtual] && DADOS[mesAtual][usuarioAtual];
        if (!d) return;

        const isMobile  = window.innerWidth <= 768;
        const ehBarra   = (modoAtual === 'barras');
        const ehDiario  = (mesAtual !== 'Todos os Meses');
        const qtdPontos = d.x ? d.x.length : 1;

        const maxBar    = Array.isArray(d.max_bar)  ? d.max_bar[0]  : d.max_bar;
        const maxLine   = Array.isArray(d.max_line) ? d.max_line[0] : d.max_line;
        const maxY      = ehBarra ? maxBar : maxLine;

        const nomeE     = Array.isArray(d.e_nome)   ? d.e_nome[0]   : d.e_nome;
        const nomeS     = Array.isArray(d.s_nome)   ? d.s_nome[0]   : d.s_nome;
        const nomeDif   = Array.isArray(d.dif_nome) ? d.dif_nome[0] : d.dif_nome;
        const corDif    = Array.isArray(d.dif_cor)  ? d.dif_cor[0]  : d.dif_cor;

        // Atualiza os 3 rótulos do cabeçalho (Saídas, Entradas e Diferença) dinamicamente!
        if (legEntradas)  legEntradas.innerHTML  = nomeE;
        if (legSaidas)    legSaidas.innerHTML    = nomeS;
        if (legDiferenca) legDiferenca.innerHTML = nomeDif;
        if (quadDiferenca) quadDiferenca.style.background = corDif;

        const tamRotulo = isMobile ? 9 : ", tamanho_rotulos, ";
        const tamEixoX  = isMobile ? (ehDiario ? 9.5 : 10.5) : ", tamanho_eixo_x, ";
        const tamEixoY  = isMobile ? 9.5 : 11;
        const alturaAtu = isMobile ? ", altura_grafico_mobile, " : ", altura_grafico, ";
        const raioAtu   = isMobile ? 5 : ", raio_cantos, ";

        // No modo Linhas: posiciona cada rótulo de forma inteligente (quem é maior fica bem acima da bolinha,
        // quem é menor fica abaixo ou acima com folga) para NUNCA ficar em cima da linha!
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

        const ocultarTxtMobile = (isMobile && qtdPontos > 12);

        const traceEntradas = {
          x: d.x,
          y: d.e_y,
          name: nomeE,
          type: ehBarra ? 'bar' : 'scatter',
          mode: ehBarra ? undefined : 'lines+markers+text',
          marker: { color: '", cor_entradas, "', size: isMobile ? 7 : 8 },
          line: { color: '", cor_entradas, "', width: isMobile ? 2.2 : 3 },
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
          name: nomeS,
          type: ehBarra ? 'bar' : 'scatter',
          mode: ehBarra ? undefined : 'lines+markers+text',
          marker: { color: '", cor_saidas, "', size: isMobile ? 7 : 8 },
          line: { color: '", cor_saidas, "', width: isMobile ? 2.2 : 3 },
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

        const minY = ehBarra ? 0 : -(maxLine * 0.08);

        const novoLayout = Object.assign({}, el.layout, {
          height: alturaAtu,
          barmode: 'stack',
          barcornerradius: raioAtu,
          bargap: isMobile ? (qtdPontos > 8 ? 0.18 : 0.30) : (qtdPontos > 10 ? 0.22 : 0.30),
          uniformtext: { mode: 'show', minsize: tamRotulo },
          margin: isMobile
            ? { t: 24, b: qtdPontos > 10 ? 40 : 32, l: 48, r: 10 }
            : { t: 30, b: 45, l: 65, r: 15 },
          xaxis: Object.assign({}, el.layout.xaxis, {
            type: 'category',
            categoryorder: 'array',
            categoryarray: d.x,
            tickangle: (isMobile && qtdPontos > 10) ? -45 : 0,
            tickfont: { family: 'Inter, sans-serif', size: tamEixoX, color: '", cor_eixo_x, "' }
          }),
          yaxis: Object.assign({}, el.layout.yaxis, {
            range: [minY, maxY],
            tickfont: { family: 'Inter, sans-serif', size: tamEixoY, color: '#444444' }
          })
        });

        el.style.height = alturaAtu + 'px';
        Plotly.react(el, [traceEntradas, traceSaidas], novoLayout, {displayModeBar: false, responsive: true});
      }

      atualizarGrafico();

      let timerResize;
      window.addEventListener('resize', function() {
        clearTimeout(timerResize);
        timerResize = setTimeout(atualizarGrafico, 150);
      });

      if (selMes) {
        selMes.addEventListener('change', function() {
          mesAtual = this.value;
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
          botoesUsuario.forEach(function(b) {
            b.classList.toggle('ativo', b.getAttribute('data-usuario') === usuarioAtual);
          });
          atualizarGrafico();
        });
      });

      botoesModo.forEach(function(btn) {
        btn.addEventListener('click', function() {
          modoAtual = this.getAttribute('data-modo');
          botoesModo.forEach(function(b) { b.classList.remove('ativo'); });
          this.classList.add('ativo');
          atualizarGrafico();
        });
      });
    }
  "))


# ==============================================================================
# 8. ESTILO CSS E ESTRUTURA HTML (COM O 3º RÓTULO DE DIFERENÇA)
# ==============================================================================

estilo_controles_fluxo <- htmltools::tags$style(base::paste0("
  .painel-fluxo-alf {
    max-width: 950px;
    width: 100%;
    margin: 25px auto;
    font-family: 'Inter', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    background: #ffffff;
    box-sizing: border-box;
  }
  .painel-fluxo-alf .grafico-scroll {
    width: 100% !important;
    min-width: 0 !important;
    overflow-x: hidden !important;
  }
  .barra-controles-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 14px;
    padding: 12px 16px;
    margin-bottom: 18px;
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
    margin: 0 0 4px 0;
    line-height: 1.25;
  }
  .subtitulo-grafico-alf {
    color: ", sub_cor, ";
    font-size: ", sub_tamanho, "px;
    font-weight: 400;
    margin: 0 0 10px 0;
  }
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
    padding: 6px 12px;
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
     AJUSTES EXCLUSIVOS PARA CELULAR (MOBILE <= 768px)
     ========================================================= */
  @media (max-width: 768px) {
    .painel-fluxo-alf {
      margin: 15px auto;
    }
    .barra-controles-alf {
      padding: 10px 12px;
      gap: 8px;
      margin-bottom: 12px;
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
      max-width: 180px;
    }
    .btn-usuario-alf.texto-todos {
      padding: 4px 10px;
      font-size: 11px;
    }
    .btn-usuario-alf img {
      width: 30px;
      height: 30px;
    }
    .btn-modo-alf {
      font-size: 11px;
      padding: 4px 9px;
    }
    .titulo-grafico-alf {
      font-size: 14px;
    }
    .subtitulo-grafico-alf {
      font-size: 11.5px;
      margin-bottom: 8px;
    }
    .legenda-custom-alf {
      gap: 10px;
    }
    .item-legenda-alf {
      font-size: 11px;
      gap: 5px;
    }
    .quadrado-legenda-alf {
      width: 11px;
      height: 11px;
    }
  }
"))

opcoes_html_mes <- purrr::map(meses_disponiveis, function(m) {
  htmltools::tags$option(value = m, m)
})

controles_html <- htmltools::tags$div(
  class = "barra-controles-alf",
  
  # 1. Seletor de Mês
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-calendar3'></i> Período:")),
    htmltools::tags$select(
      class = "select-mes-alf",
      opcoes_html_mes
    )
  ),
  
  # 2. Seletor de Usuário com Fotos Redondas
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-people'></i> Usuário:")),
    htmltools::tags$div(
      style = "display: inline-flex; align-items: center; gap: 6px;",
      htmltools::tags$button(
        type = "button",
        class = "btn-usuario-alf texto-todos ativo",
        `data-usuario` = "Todos",
        title = "Mostrar Todos",
        "Todos"
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn-usuario-alf",
        `data-usuario` = "Alex",
        title = "Filtrar: Alex",
        htmltools::tags$img(src = uri_alex, alt = "Alex")
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn-usuario-alf",
        `data-usuario` = "Leticia",
        title = "Filtrar: Letícia",
        htmltools::tags$img(src = uri_leticia, alt = "Letícia")
      )
    )
  ),
  
  # 3. Seletor de Visão (Empilhado vs Linhas)
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-bar-chart-line'></i> Visão:")),
    htmltools::tags$div(
      style = "display: inline-flex; align-items: center; gap: 6px;",
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
      )
    )
  )
)

# Cabeçalho com os 3 Rótulos alinhados à esquerda: Saídas, Entradas e Diferença!
cabecalho_html <- htmltools::tags$div(
  class = "cabecalho-grafico-alf",
  htmltools::tags$div(class = "titulo-grafico-alf", titulo_nome),
  htmltools::tags$div(class = "subtitulo-grafico-alf", subtitulo_texto),
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
    htmltools::tags$div(class = "grafico-scroll", grafico)
  )
)

grafico_fluxo