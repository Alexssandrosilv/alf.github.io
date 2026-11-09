# ==============================================================================
# PROJETO ALF — Gráfico: Fluxo de Entradas e Saídas (Mensal e Diário)
# ==============================================================================
# Controles HTML customizados com fotos redondas (Alex / Letícia),
# alternância entre Barras Empilhadas e Linhas, e drill-down de Meses para Dias.

# ==============================================================================
# 1. CONFIGURAÇÕES VISUAIS (PADRÃO ALEX + PALETA AZUL/CIANO DO MODELO)
# ==============================================================================

titulo_nome     <- "Fluxo Financeiro: Entradas e Saídas"
subtitulo_texto <- "Visão consolidada mensal e detalhamento diário (saídas pela data de pagamento)"

titulo_cor      <- "#03045e"
titulo_tamanho  <- 15
sub_cor         <- "#555555"
sub_tamanho     <- 12.5

# Cores inspiradas no modelo de barras empilhadas (tons de azul/ciano)
cor_entradas    <- "#29b6d8"   # Azul Ciano Vibrante (Base)
cor_saidas      <- "#1b78a1"   # Azul Oceano Escuro (Topo)

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

raio_cantos    <- 8   # Cantos arredondados no topo das barras como na imagem
altura_grafico <- 500

# Opções de regra de negócio
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

# Padroniza o nome do usuário para bater com os botões das fotos
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
    descricao = base::as.character(descricao),
    status    = NA_character_
  )

saidas_fluxo <- bd_saidas %>%
  dplyr::filter(!base::is.na(data_pagamento)) %>%
  dplyr::transmute(
    tipo      = "Saídas",
    data      = base::as.Date(data_pagamento),
    valor     = base::as.numeric(valor),
    usuario   = padronizar_usuario(usuario_id),
    descricao = base::as.character(descricao),
    status    = base::as.character(status)
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
    sufixo_status = base::ifelse(base::is.na(status), "", base::paste0(" (", status, ")")),
    item = if (descricao_no_hover) {
      base::paste0("• ", descricao, sufixo_status, ": R$ ", formatar_brl(valor))
    } else {
      base::paste0("• R$ ", formatar_brl(valor), sufixo_status)
    }
  )

# Lista de meses existentes ordenados cronologicamente
tabela_meses <- fluxo %>%
  dplyr::distinct(mes_ref, mes_label, mes_nome) %>%
  dplyr::arrange(mes_ref)

meses_disponiveis <- base::c("Todos os Meses", tabela_meses$mes_label)
opcoes_usuario    <- base::c("Todos", "Alex", "Leticia")


# ==============================================================================
# 4. FUNÇÃO GERADORA DE CAMADAS (MENSAL QUANDO "TODOS", DIÁRIO QUANDO MÊS)
# ==============================================================================

gerar_dados_trace <- function(mes_sel, usuario_sel, tipo_sel) {
  
  d <- fluxo %>% dplyr::filter(tipo == tipo_sel)
  if (usuario_sel != "Todos") {
    d <- d %>% dplyr::filter(usuario == usuario_sel)
  }
  
  # CASO 1: "Todos os Meses" -> Eixo X com os Nomes dos Meses e valores totais
  if (mes_sel == "Todos os Meses") {
    
    # Se houver mais de um ano nos dados, exibe Mês/Ano; se for mesmo ano, exibe só o nome do Mês
    usar_ano <- dplyr::n_distinct(fluxo$ano_num) > 1
    
    esqueleto_meses <- tabela_meses %>%
      dplyr::mutate(
        eixo_x = if (usar_ano) base::paste0("<b>", mes_label, "</b>") else base::paste0("<b>", mes_nome, "</b>")
      )
    
    por_mes <- d %>%
      dplyr::group_by(mes_ref, mes_label) %>%
      dplyr::summarise(
        valor_total = base::sum(valor, na.rm = TRUE),
        qtd_lanc    = dplyr::n(),
        detalhes    = if (dplyr::n() <= 8) {
          base::paste(item, collapse = "<br>")
        } else {
          base::paste0(base::paste(utils::head(item, 8), collapse = "<br>"), "<br>• ... (+", dplyr::n() - 8, " lançamentos)")
        },
        .groups = "drop"
      )
    
    res <- esqueleto_meses %>%
      dplyr::left_join(por_mes, by = base::c("mes_ref", "mes_label")) %>%
      dplyr::mutate(
        valor_final = base::ifelse(base::is.na(valor_total), 0, valor_total),
        texto_barra = formatar_brl_curto(valor_final),
        hover = base::ifelse(
          valor_final == 0,
          base::paste0("<b>", mes_label, "</b><br>", tipo_sel, ": R$ 0,00"),
          base::paste0(
            "<b>", mes_label, "</b><br>",
            "<b>", tipo_sel, ": R$ ", formatar_brl(valor_final), "</b> (", qtd_lanc, " lanç.)<br><br>",
            detalhes
          )
        )
      )
    
    return(base::list(
      x     = base::as.list(res$eixo_x),
      y     = base::as.list(res$valor_final),
      text  = base::as.list(res$texto_barra),
      hover = base::as.list(res$hover),
      total = base::sum(res$valor_final),
      nome  = base::paste0("<b>", tipo_sel, " (Total: R$ ", formatar_brl(base::sum(res$valor_final)), ")</b>")
    ))
    
  } else {
    # CASO 2: Mês Específico -> Eixo X com os dias 01 até o final do mês
    d_mes <- d %>% dplyr::filter(mes_label == mes_sel)
    
    ref_str    <- tabela_meses$mes_ref[tabela_meses$mes_label == mes_sel][1]
    inicio_mes <- base::as.Date(base::paste0(ref_str, "-01"))
    fim_mes    <- lubridate::ceiling_date(inicio_mes, "month") - lubridate::days(1)
    todos_dias <- base::seq(inicio_mes, fim_mes, by = "day")
    
    por_dia <- d_mes %>%
      dplyr::group_by(data) %>%
      dplyr::summarise(
        valor_total = base::sum(valor, na.rm = TRUE),
        detalhes    = base::paste(item, collapse = "<br>"),
        .groups     = "drop"
      )
    
    res <- dplyr::tibble(data = todos_dias) %>%
      dplyr::left_join(por_dia, by = "data") %>%
      dplyr::mutate(
        eixo_x      = base::paste0("<b>", base::format(data, "%d"), "</b>"),
        valor_final = base::ifelse(base::is.na(valor_total), 0, valor_total),
        texto_barra = formatar_brl_curto(valor_final),
        hover = base::ifelse(
          valor_final == 0,
          base::paste0("<b>", base::format(data, "%d/%m/%Y"), "</b><br>", tipo_sel, ": R$ 0,00"),
          base::paste0(
            "<b>", base::format(data, "%d/%m/%Y"), "</b><br>",
            "<b>", tipo_sel, ": R$ ", formatar_brl(valor_final), "</b><br><br>",
            detalhes
          )
        )
      )
    
    return(base::list(
      x     = base::as.list(res$eixo_x),
      y     = base::as.list(res$valor_final),
      text  = base::as.list(res$texto_barra),
      hover = base::as.list(res$hover),
      total = base::sum(res$valor_final),
      nome  = base::paste0("<b>", tipo_sel, " (Total: R$ ", formatar_brl(base::sum(res$valor_final)), ")</b>")
    ))
  }
}


# ==============================================================================
# 5. PRÉ-CÁLCULO DO DICIONÁRIO DE ESTADOS PARA O JAVASCRIPT (ZERO BUG)
# ==============================================================================

banco_estados <- base::list()

for (m in meses_disponiveis) {
  banco_estados[[m]] <- base::list()
  for (u in opcoes_usuario) {
    dados_e <- gerar_dados_trace(m, u, "Entradas")
    dados_s <- gerar_dados_trace(m, u, "Saídas")
    
    # Calcula o teto do eixo Y (soma das barras empilhadas no ponto mais alto + folga de 25%)
    soma_topo <- base::unlist(dados_e$y) + base::unlist(dados_s$y)
    max_y     <- base::max(base::c(soma_topo, 100), na.rm = TRUE) * 1.25
    
    banco_estados[[m]][[u]] <- base::list(
      e     = dados_e,
      s     = dados_s,
      max_y = max_y
    )
  }
}

json_estados <- jsonlite::toJSON(banco_estados, auto_unbox = TRUE)


# ==============================================================================
# 6. GRÁFICO INICIAL (PLOTLY)
# ==============================================================================

estado_ini <- banco_estados[["Todos os Meses"]][["Todos"]]
id_grafico <- "plotly_fluxo_alf"

grafico <- plotly::plot_ly(height = altura_grafico, elementId = id_grafico) %>%
  # Série 0: Entradas (Base Azul Ciano)
  plotly::add_bars(
    x            = base::unlist(estado_ini$e$x),
    y            = base::unlist(estado_ini$e$y),
    name         = estado_ini$e$nome,
    marker       = base::list(color = cor_entradas, line = base::list(color = cor_entradas, width = 0)),
    line         = base::list(color = cor_entradas, width = 3),
    text         = base::unlist(estado_ini$e$text),
    textposition = "inside",
    insidetextanchor = "middle",
    cliponaxis   = FALSE,
    textfont     = base::list(family = "Inter, sans-serif", size = tamanho_rotulos, color = "#ffffff"),
    hoverinfo    = "text",
    hovertext    = base::unlist(estado_ini$e$hover)
  ) %>%
  # Série 1: Saídas (Topo Azul Escuro)
  plotly::add_bars(
    x            = base::unlist(estado_ini$s$x),
    y            = base::unlist(estado_ini$s$y),
    name         = estado_ini$s$nome,
    marker       = base::list(color = cor_saidas, line = base::list(color = cor_saidas, width = 0)),
    line         = base::list(color = cor_saidas, width = 3),
    text         = base::unlist(estado_ini$s$text),
    textposition = "outside",
    cliponaxis   = FALSE,
    textfont     = base::list(family = "Inter, sans-serif", size = tamanho_rotulos, color = cor_rotulos),
    hoverinfo    = "text",
    hovertext    = base::unlist(estado_ini$s$hover)
  ) %>%
  plotly::layout(
    font            = base::list(family = "Inter, sans-serif"),
    separators      = ",.",
    barmode         = "stack",
    barcornerradius = raio_cantos,
    bargap          = 0.25,
    
    title = base::list(
      text = base::paste0(
        "<b style='color:", titulo_cor, "; font-size:", titulo_tamanho, "px'>", titulo_nome, "</b>",
        "<br><span style='color:", sub_cor, "; font-size:", sub_tamanho, "px'>", subtitulo_texto, "</span>"
      ),
      x = 0, y = 0.97, xanchor = "left", xref = "paper"
    ),
    
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
      categoryarray = base::unlist(estado_ini$e$x),
      showgrid      = FALSE,
      fixedrange    = TRUE,
      tickangle     = 0,
      tickfont      = base::list(family = "Inter, sans-serif", size = tamanho_eixo_x, color = cor_eixo_x)
    ),
    
    yaxis = base::list(
      title      = "",
      tickprefix = "R$ ",
      tickformat = ",.0f",
      showgrid   = TRUE,
      gridcolor  = "#e9ecef",
      zeroline   = TRUE,
      zerolinecolor = "#cccccc",
      fixedrange = TRUE,
      range      = base::list(0, estado_ini$max_y),
      tickfont   = base::list(family = "Inter, sans-serif", size = 11, color = "#444444")
    ),
    
    showlegend = TRUE,
    legend = base::list(
      orientation = "h",
      x = 0, xanchor = "left",
      y = 1.11, yanchor = "bottom",
      font = base::list(family = "Inter, sans-serif", size = tamanho_legenda, color = "#000000")
    ),
    
    margin        = base::list(t = 105, b = 50, l = 65, r = 20),
    plot_bgcolor  = "white",
    paper_bgcolor = "white"
  ) %>%
  plotly::config(displayModeBar = FALSE, responsive = TRUE)


# ==============================================================================
# 7. BARRA DE CONTROLES ESTILO ABNT (COM FOTOS REDONDAS E BOTÕES ELEGANTES)
# ==============================================================================

estilo_controles_fluxo <- htmltools::tags$style("
  .painel-fluxo-alf {
    max-width: 950px;
    margin: 25px auto;
    font-family: 'Inter', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    background: #ffffff;
  }
  .barra-controles-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 14px;
    padding: 12px 16px;
    margin-bottom: 8px;
    background: #fcfcfc;
    border: 1px solid #e0e0e0;
    border-top: 2px solid #000000; /* Barra superior preta padrão ABNT */
    border-radius: 0 0 10px 10px;
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
  /* Select de Mês */
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
  /* Botões de Usuário (Todos + Fotos Redondas) */
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
  /* Botões de Tipo de Gráfico (Empilhado vs Linhas) */
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
  @media (max-width: 768px) {
    .barra-controles-alf {
      flex-direction: column;
      align-items: stretch;
      gap: 10px;
    }
    .grupo-controle-alf {
      justify-content: space-between;
    }
  }
")

# Monta o HTML dos controles
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
      id = "filtro_mes_fluxo",
      class = "select-mes-alf",
      opcoes_html_mes
    )
  ),
  
  # 2. Seletor de Usuário com as Fotos Redondas (Todos / Alex / Letícia)
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-people'></i> Usuário:")),
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
  ),
  
  # 3. Seletor de Visualização (Barras Empilhadas vs Gráfico de Linhas)
  htmltools::tags$div(
    class = "grupo-controle-alf",
    htmltools::tags$span(class = "rotulo-controle-alf", htmltools::HTML("<i class='bi bi-bar-chart-line'></i> Visão:")),
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

# Script JS que sincroniza os 3 controles sem nunca bugar
script_interatividade <- htmltools::tags$script(htmltools::HTML(base::paste0("
(function() {
  const DADOS_FLUXO = ", json_estados, ";
  let mesAtual     = 'Todos os Meses';
  let usuarioAtual = 'Todos';
  let modoAtual    = 'barras';

  function atualizarGraficoFluxo() {
    const el = document.getElementById('", id_grafico, "');
    if (!el || !el.data) return;

    const d = DADOS_FLUXO[mesAtual][usuarioAtual];
    if (!d) return;

    const ehBarra = (modoAtual === 'barras');

    // Atualiza dados e tipo (Barras Empilhadas ou Linhas)
    const updateTraces = {
      x:         [d.e.x, d.s.x],
      y:         [d.e.y, d.s.y],
      text:      [d.e.text, d.s.text],
      hovertext: [d.e.hover, d.s.hover],
      name:      [d.e.nome, d.s.nome],
      type:      ehBarra ? ['bar', 'bar'] : ['scatter', 'scatter'],
      mode:      ehBarra ? [null, null] : ['lines+markers+text', 'lines+markers+text'],
      textposition: ehBarra ? ['inside', 'outside'] : ['top center', 'top center'],
      textfont:  ehBarra 
        ? [{family: 'Inter, sans-serif', size: 10.5, color: '#ffffff'}, {family: 'Inter, sans-serif', size: 10.5, color: '#000000'}]
        : [{family: 'Inter, sans-serif', size: 10.5, color: '#1b78a1'}, {family: 'Inter, sans-serif', size: 10.5, color: '#03045e'}]
    };

    const updateLayout = {
      'xaxis.categoryarray': d.e.x,
      'yaxis.range': [0, d.max_y],
      'barmode': 'stack'
    };

    Plotly.update(el, updateTraces, updateLayout, [0, 1]);
  }

  document.addEventListener('DOMContentLoaded', function() {
    // Evento de mudança de Mês
    const selMes = document.getElementById('filtro_mes_fluxo');
    if (selMes) {
      selMes.addEventListener('change', function() {
        mesAtual = this.value;
        atualizarGraficoFluxo();
      });
    }

    // Evento de clique nas Fotos de Usuário
    const botoesUsuario = document.querySelectorAll('.btn-usuario-alf');
    botoesUsuario.forEach(function(btn) {
      btn.addEventListener('click', function() {
        const clicado = this.getAttribute('data-usuario');
        // Se clicar na foto já ativa, volta para 'Todos' (igual na tabela!)
        if (usuarioAtual === clicado && clicado !== 'Todos') {
          usuarioAtual = 'Todos';
        } else {
          usuarioAtual = clicado;
        }
        botoesUsuario.forEach(b => {
          b.classList.toggle('ativo', b.getAttribute('data-usuario') === usuarioAtual);
        });
        atualizarGraficoFluxo();
      });
    });

    // Evento de clique em Empilhado vs Linhas
    const botoesModo = document.querySelectorAll('.btn-modo-alf');
    botoesModo.forEach(function(btn) {
      btn.addEventListener('click', function() {
        modoAtual = this.getAttribute('data-modo');
        botoesModo.forEach(b => b.classList.remove('ativo'));
        this.classList.add('ativo');
        atualizarGraficoFluxo();
      });
    });
  });
})();
")))


# ==============================================================================
# 8. OBJETO FINAL PARA O INDEX.QMD
# ==============================================================================

grafico_fluxo <- htmltools::tagList(
  estilo_controles_fluxo,
  htmltools::tags$div(
    class = "painel-fluxo-alf",
    controles_html,
    htmltools::tags$div(class = "grafico-scroll", grafico)
  ),
  script_interatividade
)

grafico_fluxo