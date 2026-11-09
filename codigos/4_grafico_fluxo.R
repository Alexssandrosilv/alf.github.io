# ==============================================================================
# PROJETO ALF — Gráfico: Fluxo de Entradas e Saídas (Mensal e Diário)
# ==============================================================================
# Controles HTML customizados com fotos redondas (Alex / Letícia),
# cabeçalho alinhado à esquerda, hover analítico e ajuste automático Mobile/Desktop.

# ==============================================================================
# 1. CONFIGURAÇÕES VISUAIS (PADRÃO ALEX + PALETA AZUL/CIANO)
# ==============================================================================

titulo_nome     <- "Fluxo Financeiro: Entradas e Saídas"
subtitulo_texto <- "Visão consolidada mensal e detalhamento diário (saídas pela data de pagamento)"

titulo_cor      <- "#03045e"
titulo_tamanho  <- 16
sub_cor         <- "#555555"
sub_tamanho     <- 13

# Cores inspiradas no modelo de barras empilhadas (tons de azul/ciano)
cor_entradas    <- "#29b6d8"   # Azul Ciano Vibrante (Base)
cor_saidas      <- "#1b78a1"   # Azul Oceano Escuro (Topo)

tamanho_rotulos <- 10.5
cor_rotulos     <- "#000000"
tamanho_eixo_x  <- 11
cor_eixo_x      <- "#000000"
tamanho_legenda <- 13

# Balão de Hover (Fundo branco e borda preta padrão ABNT/Alex)
hover_bg_cor      <- "#FFFFFF"
hover_txt_cor     <- "#000000"
hover_fonte_tam   <- 12
borda_balao_hover <- "#000000"

raio_cantos           <- 8
altura_grafico        <- 460 # Altura no Notebook/PC
altura_grafico_mobile <- 300 # Altura reduzida e proporcional no Celular

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
    tipo      = "Entradas",
    data      = base::as.Date(data),
    valor     = base::as.numeric(valor),
    usuario   = padronizar_usuario(usuario_id),
    categoria = dplyr::coalesce(base::as.character(fonte_renda), "Outras Fontes"),
    descricao = base::as.character(descricao),
    status    = "Recebido"
  )

saidas_fluxo <- bd_saidas %>%
  dplyr::filter(!base::is.na(data_pagamento)) %>%
  dplyr::transmute(
    tipo      = "Saídas",
    data      = base::as.Date(data_pagamento),
    valor     = base::as.numeric(valor),
    usuario   = padronizar_usuario(usuario_id),
    categoria = dplyr::coalesce(base::as.character(tipo_despesa), "Outras Despesas"),
    descricao = base::as.character(descricao),
    status    = dplyr::coalesce(base::as.character(status), "A pagar")
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
    mes_label     = base::paste0(mes_nome, "/", ano_num),
    sufixo_status = base::ifelse(tipo == "Entradas", "", base::paste0(" (", status, ")")),
    item_individual = if (descricao_no_hover) {
      base::paste0("• ", descricao, sufixo_status, ": R$ ", formatar_brl(valor))
    } else {
      base::paste0("• R$ ", formatar_brl(valor), " — ", categoria, sufixo_status)
    }
  )

tabela_meses <- fluxo %>%
  dplyr::distinct(mes_ref, mes_label, mes_nome) %>%
  dplyr::arrange(mes_ref)

meses_disponiveis <- base::c("Todos os Meses", tabela_meses$mes_label)
opcoes_usuario    <- base::c("Todos", "Alex", "Leticia")


# ==============================================================================
# 4. FUNÇÃO DE HOVER INTELIGENTE (RESUMO EXECUTIVO VS LISTA CURTA)
# ==============================================================================

montar_hover_inteligente <- function(df_sub, titulo_periodo, tipo_sel, modo_mensal = TRUE) {
  n_lanc <- base::nrow(df_sub)
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0("<b>", titulo_periodo, "</b><br>", tipo_sel, ": R$ 0,00"))
  }
  
  val_total <- base::sum(df_sub$valor, na.rm = TRUE)
  val_media <- base::mean(df_sub$valor, na.rm = TRUE)
  val_maior <- base::max(df_sub$valor, na.rm = TRUE)
  
  cabecalho <- base::paste0(
    "<b>", titulo_periodo, "</b><br>",
    "<b>", tipo_sel, ": R$ ", formatar_brl(val_total), "</b> (", n_lanc,
    base::ifelse(n_lanc == 1, " lançamento)", " lançamentos)"), "<br>"
  )
  
  if (!modo_mensal && n_lanc <= 5) {
    corpo <- base::paste(df_sub$item_individual, collapse = "<br>")
    return(base::paste0(cabecalho, "<br>", corpo))
  }
  
  bloco_status <- ""
  if (tipo_sel == "Saídas") {
    res_status <- df_sub %>%
      dplyr::group_by(status) %>%
      dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(soma)) %>%
      dplyr::mutate(
        pct   = base::round((soma / val_total) * 100, 1),
        linha = base::paste0("• ", status, ": R$ ", formatar_brl(soma), " (", pct, "% | ", qtd, "x)")
      )
    bloco_status <- base::paste0("<br><b>Por Status:</b><br>", base::paste(res_status$linha, collapse = "<br>"))
  }
  
  rotulo_cat <- base::ifelse(tipo_sel == "Entradas", "Por Fonte de Renda:", "Por Tipo de Despesa:")
  res_cat <- df_sub %>%
    dplyr::group_by(categoria) %>%
    dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(soma)) %>%
    utils::head(4) %>%
    dplyr::mutate(
      pct   = base::round((soma / val_total) * 100, 1),
      linha = base::paste0("• ", categoria, ": R$ ", formatar_brl(soma), " (", pct, "%)")
    )
  bloco_cat <- base::paste0("<br><b>", rotulo_cat, "</b><br>", base::paste(res_cat$linha, collapse = "<br>"))
  
  rodape_stats <- base::paste0(
    "<br><br><span style='color:#444444; font-size:11px'>",
    "Média/lanç.: R$ ", formatar_brl(val_media), " | Maior: R$ ", formatar_brl(val_maior),
    "</span>"
  )
  
  base::paste0(cabecalho, bloco_status, bloco_cat, rodape_stats)
}


# ==============================================================================
# 5. GERADOR DE CAMADAS POR FILTRO (MÊS X USUÁRIO)
# ==============================================================================

gerar_dados_trace <- function(mes_sel, usuario_sel, tipo_sel) {
  
  d <- fluxo %>% dplyr::filter(tipo == tipo_sel)
  if (usuario_sel != "Todos") {
    d <- d %>% dplyr::filter(usuario == usuario_sel)
  }
  
  if (mes_sel == "Todos os Meses") {
    usar_ano <- dplyr::n_distinct(fluxo$ano_num) > 1
    
    esqueleto_meses <- tabela_meses %>%
      dplyr::mutate(
        eixo_x = if (usar_ano) base::paste0("<b>", mes_label, "</b>") else base::paste0("<b>", mes_nome, "</b>")
      )
    
    valores_y   <- base::numeric(base::nrow(esqueleto_meses))
    textos_bar  <- base::character(base::nrow(esqueleto_meses))
    hovers_bar  <- base::character(base::nrow(esqueleto_meses))
    
    for (i in base::seq_len(base::nrow(esqueleto_meses))) {
      m_ref <- esqueleto_meses$mes_ref[i]
      m_lbl <- esqueleto_meses$mes_label[i]
      d_sub <- d %>% dplyr::filter(mes_ref == m_ref)
      
      soma_m         <- base::sum(d_sub$valor, na.rm = TRUE)
      valores_y[i]   <- soma_m
      textos_bar[i]  <- formatar_brl_curto(soma_m)
      hovers_bar[i]  <- montar_hover_inteligente(d_sub, m_lbl, tipo_sel, modo_mensal = TRUE)
    }
    
    return(base::list(
      x     = base::as.character(esqueleto_meses$eixo_x),
      y     = base::as.numeric(valores_y),
      text  = base::as.character(textos_bar),
      hover = base::as.character(hovers_bar),
      nome  = base::paste0("<b>", tipo_sel, " (Total: R$ ", formatar_brl(base::sum(valores_y)), ")</b>")
    ))
    
  } else {
    d_mes      <- d %>% dplyr::filter(mes_label == mes_sel)
    ref_str    <- tabela_meses$mes_ref[tabela_meses$mes_label == mes_sel][1]
    inicio_mes <- base::as.Date(base::paste0(ref_str, "-01"))
    fim_mes    <- lubridate::ceiling_date(inicio_mes, "month") - lubridate::days(1)
    todos_dias <- base::seq(inicio_mes, fim_mes, by = "day")
    
    eixo_x     <- base::paste0("<b>", base::format(todos_dias, "%d"), "</b>")
    valores_y  <- base::numeric(base::length(todos_dias))
    textos_bar <- base::character(base::length(todos_dias))
    hovers_bar <- base::character(base::length(todos_dias))
    
    for (i in base::seq_along(todos_dias)) {
      dia_i <- todos_dias[i]
      d_sub <- d_mes %>% dplyr::filter(data == dia_i)
      
      soma_d        <- base::sum(d_sub$valor, na.rm = TRUE)
      valores_y[i]  <- soma_d
      textos_bar[i] <- formatar_brl_curto(soma_d)
      hovers_bar[i] <- montar_hover_inteligente(d_sub, base::format(dia_i, "%d/%m/%Y"), tipo_sel, modo_mensal = FALSE)
    }
    
    return(base::list(
      x     = base::as.character(eixo_x),
      y     = base::as.numeric(valores_y),
      text  = base::as.character(textos_bar),
      hover = base::as.character(hovers_bar),
      nome  = base::paste0("<b>", tipo_sel, " (Total: R$ ", formatar_brl(base::sum(valores_y)), ")</b>")
    ))
  }
}


# ==============================================================================
# 6. PRÉ-CÁLCULO DE TODOS OS ESTADOS (JSON SEGURO COM ARRAYS GARANTIDOS)
# ==============================================================================

banco_estados <- base::list()

for (m in meses_disponiveis) {
  banco_estados[[m]] <- base::list()
  for (u in opcoes_usuario) {
    dados_e <- gerar_dados_trace(m, u, "Entradas")
    dados_s <- gerar_dados_trace(m, u, "Saídas")
    
    soma_topo <- dados_e$y + dados_s$y
    max_y     <- base::max(base::c(soma_topo, 100), na.rm = TRUE) * 1.25
    
    banco_estados[[m]][[u]] <- base::list(
      e     = dados_e,
      s     = dados_s,
      max_y = base::as.numeric(max_y)[1]
    )
  }
}

json_estados <- jsonlite::toJSON(banco_estados, auto_unbox = FALSE)


# ==============================================================================
# 7. CONSTRUÇÃO DO GRÁFICO PLOTLY + ONRENDER (COM ADAPTAÇÃO AUTOMÁTICA MOBILE)
# ==============================================================================

estado_ini <- banco_estados[["Todos os Meses"]][["Todos"]]

grafico <- plotly::plot_ly(height = altura_grafico) %>%
  # Série 0: Entradas (Base Azul Ciano)
  plotly::add_bars(
    x                = estado_ini$e$x,
    y                = estado_ini$e$y,
    name             = estado_ini$e$nome[1],
    marker           = base::list(color = cor_entradas),
    text             = estado_ini$e$text,
    textposition     = "inside",
    insidetextanchor = "middle",
    cliponaxis       = FALSE,
    textfont         = base::list(family = "Inter, sans-serif", size = tamanho_rotulos, color = "#ffffff"),
    hoverinfo        = "text",
    hovertext        = estado_ini$e$hover
  ) %>%
  # Série 1: Saídas (Topo Azul Oceano)
  plotly::add_bars(
    x            = estado_ini$s$x,
    y            = estado_ini$s$y,
    name         = estado_ini$s$nome[1],
    marker       = base::list(color = cor_saidas),
    text         = estado_ini$s$text,
    textposition = "outside",
    cliponaxis   = FALSE,
    textfont     = base::list(family = "Inter, sans-serif", size = tamanho_rotulos, color = cor_rotulos),
    hoverinfo    = "text",
    hovertext    = estado_ini$s$hover
  ) %>%
  plotly::layout(
    font            = base::list(family = "Inter, sans-serif"),
    separators      = ",.",
    barmode         = "stack",
    barcornerradius = raio_cantos,
    bargap          = 0.25,
    
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
      categoryarray = estado_ini$e$x,
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
      range         = base::list(0, estado_ini$max_y[1]),
      tickfont      = base::list(family = "Inter, sans-serif", size = 11, color = "#444444")
    ),
    
    showlegend    = FALSE,
    margin        = base::list(t = 25, b = 45, l = 65, r = 15),
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

      function atualizarGrafico() {
        const d = DADOS[mesAtual] && DADOS[mesAtual][usuarioAtual];
        if (!d) return;

        const isMobile  = window.innerWidth <= 768;
        const ehBarra   = (modoAtual === 'barras');
        const ehDiario  = (mesAtual !== 'Todos os Meses');
        const maxY      = Array.isArray(d.max_y) ? d.max_y[0] : d.max_y;
        const nomeE     = Array.isArray(d.e.nome) ? d.e.nome[0] : d.e.nome;
        const nomeS     = Array.isArray(d.s.nome) ? d.s.nome[0] : d.s.nome;

        const tamRotulo = isMobile ? 8.5 : ", tamanho_rotulos, ";
        const tamEixoX  = isMobile ? (ehDiario ? 8.5 : 10) : ", tamanho_eixo_x, ";
        const tamEixoY  = isMobile ? 9.5 : 11;
        const alturaAtu = isMobile ? ", altura_grafico_mobile, " : ", altura_grafico, ";
        const raioAtu   = isMobile ? 5 : ", raio_cantos, ";

        if (legEntradas) legEntradas.innerHTML = nomeE;
        if (legSaidas)   legSaidas.innerHTML   = nomeS;

        const traceEntradas = {
          x: d.e.x,
          y: d.e.y,
          name: nomeE,
          type: ehBarra ? 'bar' : 'scatter',
          mode: ehBarra ? undefined : 'lines+markers+text',
          marker: { color: '", cor_entradas, "', size: isMobile ? 6 : 8 },
          line: { color: '", cor_entradas, "', width: isMobile ? 2.2 : 3 },
          text: (isMobile && ehDiario) ? d.e.y.map(function() { return ''; }) : d.e.text,
          textposition: ehBarra ? 'inside' : 'top center',
          insidetextanchor: 'middle',
          cliponaxis: false,
          textfont: {
            family: 'Inter, sans-serif',
            size: tamRotulo,
            color: ehBarra ? '#ffffff' : '", cor_entradas, "'
          },
          hoverinfo: 'text',
          hovertext: d.e.hover
        };

        const traceSaidas = {
          x: d.s.x,
          y: d.s.y,
          name: nomeS,
          type: ehBarra ? 'bar' : 'scatter',
          mode: ehBarra ? undefined : 'lines+markers+text',
          marker: { color: '", cor_saidas, "', size: isMobile ? 6 : 8 },
          line: { color: '", cor_saidas, "', width: isMobile ? 2.2 : 3 },
          text: (isMobile && ehDiario) ? d.s.y.map(function() { return ''; }) : d.s.text,
          textposition: ehBarra ? 'outside' : 'bottom center',
          cliponaxis: false,
          textfont: {
            family: 'Inter, sans-serif',
            size: tamRotulo,
            color: ehBarra ? '#000000' : '", cor_saidas, "'
          },
          hoverinfo: 'text',
          hovertext: d.s.hover
        };

        const novoLayout = Object.assign({}, el.layout, {
          height: alturaAtu,
          barmode: 'stack',
          barcornerradius: raioAtu,
          bargap: isMobile ? (ehDiario ? 0.15 : 0.30) : 0.25,
          margin: isMobile
            ? { t: 18, b: ehDiario ? 42 : 32, l: 48, r: 8 }
            : { t: 25, b: 45, l: 65, r: 15 },
          xaxis: Object.assign({}, el.layout.xaxis, {
            type: 'category',
            categoryorder: 'array',
            categoryarray: d.e.x,
            tickangle: (isMobile && ehDiario) ? -45 : 0,
            nticks: (isMobile && ehDiario) ? 11 : undefined,
            tickfont: { family: 'Inter, sans-serif', size: tamEixoX, color: '", cor_eixo_x, "' }
          }),
          yaxis: Object.assign({}, el.layout.yaxis, {
            range: [0, maxY],
            tickfont: { family: 'Inter, sans-serif', size: tamEixoY, color: '#444444' }
          })
        });

        el.style.height = alturaAtu + 'px';
        Plotly.react(el, [traceEntradas, traceSaidas], novoLayout, {displayModeBar: false, responsive: true});
      }

      // Aplica o dimensionamento correto logo ao abrir (seja no celular ou no PC)
      atualizarGrafico();

      // Reajusta automaticamente se girar a tela do celular ou redimensionar a janela
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
# 8. ESTILO CSS E ESTRUTURA HTML (ALINHAMENTO À ESQUERDA + MOBILE COMPACTO)
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
    gap: 22px;
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

cabecalho_html <- htmltools::tags$div(
  class = "cabecalho-grafico-alf",
  htmltools::tags$div(class = "titulo-grafico-alf", titulo_nome),
  htmltools::tags$div(class = "subtitulo-grafico-alf", subtitulo_texto),
  htmltools::tags$div(
    class = "legenda-custom-alf",
    htmltools::tags$span(
      class = "item-legenda-alf",
      htmltools::tags$span(class = "quadrado-legenda-alf", style = base::paste0("background:", cor_saidas, ";")),
      htmltools::tags$span(class = "leg-texto-saidas", htmltools::HTML(estado_ini$s$nome[1]))
    ),
    htmltools::tags$span(
      class = "item-legenda-alf",
      htmltools::tags$span(class = "quadrado-legenda-alf", style = base::paste0("background:", cor_entradas, ";")),
      htmltools::tags$span(class = "leg-texto-entradas", htmltools::HTML(estado_ini$e$nome[1]))
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