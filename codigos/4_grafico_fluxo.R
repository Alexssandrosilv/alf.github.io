# ==============================================================================
# PROJETO ALF — Gráfico: Fluxo Financeiro (Entradas e Saídas)
# ==============================================================================
# Cabeçalho do hover no formato: Mês/Ano | R$ Valor (n = X)
# Descrição sem (n = ), mantendo apenas (%).
# No Notebook: Hover flutuante original do Plotly.
# No Mobile: Card responsivo 100% encaixado na largura da tela (nunca corta!).

# ==============================================================================
# 1. CONFIGURAÇÕES VISUAIS (PADRÃO ALEX + PALETA AZUL/CIANO)
# ==============================================================================

titulo_nome     <- "Fluxo Financeiro: Entradas e Saídas (por data de pagamento)"
titulo_cor      <- "#03045e"
titulo_tamanho  <- 16

# Cores das séries
cor_entradas    <- "#29b6d8"   # Azul Ciano Vibrante (Base)
cor_saidas      <- "#1b78a1"   # Azul Oceano Escuro (Topo)

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
altura_grafico_mobile <- 275 # Altura compacta no Celular

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
# 4. FUNÇÃO DE HOVER INTELIGENTE: Mês/Ano | R$ Valor (n = X) e Descrição sem n
# ==============================================================================

montar_hover_inteligente <- function(df_sub, titulo_periodo, tipo_sel) {
  cor_titulo <- base::ifelse(tipo_sel == "Entradas", cor_entradas, cor_saidas)
  n_lanc     <- base::nrow(df_sub)
  
  if (n_lanc == 0 || base::sum(df_sub$valor, na.rm = TRUE) == 0) {
    return(base::paste0(
      "<b style='color:", cor_titulo, "; font-size:13px;'>", titulo_periodo, "</b>",
      " <b>| R$ 0,00</b> (n = 0)"
    ))
  }
  
  val_total <- base::sum(df_sub$valor, na.rm = TRUE)
  val_media <- base::mean(df_sub$valor, na.rm = TRUE)
  val_max   <- base::max(df_sub$valor, na.rm = TRUE)
  val_min   <- base::min(df_sub$valor, na.rm = TRUE)
  
  # Cabeçalho em linha única: Mês/Ano | R$ Valor (n = X)
  cabecalho <- base::paste0(
    "<b style='color:", cor_titulo, "; font-size:13px;'>", titulo_periodo, "</b>",
    " <b>| R$ ", formatar_brl(val_total), "</b> (n = ", n_lanc, ")<br>"
  )
  
  if (tipo_sel == "Saídas") {
    rotulo_1 <- "Forma de Pagamento:"
    rotulo_2 <- "Tipo de Despesa:"
  } else {
    rotulo_1 <- "Fonte de Renda:"
    rotulo_2 <- "Descrição:"
  }
  
  # Bloco 1: Forma de Pagamento (Saídas) ou Fonte de Renda (Entradas) -> mantém (n = X | Y%)
  res_1 <- df_sub %>%
    dplyr::group_by(grupo_1) %>%
    dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(soma)) %>%
    utils::head(5) %>%
    dplyr::mutate(
      pct   = base::round((soma / val_total) * 100, 1),
      linha = base::paste0("• ", grupo_1, ": R$ ", formatar_brl(soma), " (n = ", qtd, " | ", pct, "%)")
    )
  bloco_1 <- base::paste0("<br><b>", rotulo_1, "</b><br>", base::paste(res_1$linha, collapse = "<br>"))
  
  # Bloco 2: Tipo de Despesa (com n = ) ou Descrição nas Entradas (SEM n = , apenas %)
  res_2 <- df_sub %>%
    dplyr::group_by(grupo_2) %>%
    dplyr::summarise(soma = base::sum(valor, na.rm = TRUE), qtd = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(soma)) %>%
    utils::head(5) %>%
    dplyr::mutate(
      pct   = base::round((soma / val_total) * 100, 1),
      linha = if (tipo_sel == "Entradas") {
        base::paste0("• ", grupo_2, ": R$ ", formatar_brl(soma), " (", pct, "%)")
      } else {
        base::paste0("• ", grupo_2, ": R$ ", formatar_brl(soma), " (n = ", qtd, " | ", pct, "%)")
      }
    )
  bloco_2 <- base::paste0("<br><b>", rotulo_2, "</b><br>", base::paste(res_2$linha, collapse = "<br>"))
  
  # Rodapé Estatístico: Média, Máx e Mín
  rodape_stats <- base::paste0(
    "<br><br><span style='color:#444444; font-size:11px'>",
    "<b>Média:</b> R$ ", formatar_brl(val_media),
    " | <b>Máx:</b> R$ ", formatar_brl(val_max),
    " | <b>Mín:</b> R$ ", formatar_brl(val_min),
    "</span>"
  )
  
  base::paste0(cabecalho, bloco_1, bloco_2, rodape_stats)
}


# ==============================================================================
# 5. GERADOR DE CAMADAS (SINCRONIZA EIXO X E DIAS COM MOVIMENTO)
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
      hov_e      <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Entradas"))
      hov_s      <- base::c(montar_hover_inteligente(d_mes[0, ], mes_sel, "Saídas"))
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
  
  soma_topo <- y_e + y_s
  max_bar   <- base::max(base::c(soma_topo, 100), na.rm = TRUE) * 1.22
  max_line  <- base::max(base::c(y_e, y_s, 100), na.rm = TRUE) * 1.28
  
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
# 7. CONSTRUÇÃO DO GRÁFICO PLOTLY + ONRENDER (COM CARD DE HOVER PARA MOBILE)
# ==============================================================================

estado_ini <- banco_estados[["Todos os Meses"]][["Todos"]]

js_callback <- base::paste0("
function(el, x) {
  const DADOS = ", json_estados, ";
  const painel = el.closest('.painel-fluxo-alf');
  if (!painel) return;

  let mesAtual      = 'Todos os Meses';
  let usuarioAtual  = 'Todos';
  let modoAtual     = 'barras';
  let indiceClicado = null;
  let serieClicada  = 0; // 0 = Entradas, 1 = Saídas

  const selMes        = painel.querySelector('.select-mes-alf');
  const botoesUsuario = painel.querySelectorAll('.btn-usuario-alf');
  const botoesModo    = painel.querySelectorAll('.btn-modo-alf');
  const legEntradas   = painel.querySelector('.leg-texto-entradas');
  const legSaidas     = painel.querySelector('.leg-texto-saidas');
  const legDiferenca  = painel.querySelector('.leg-texto-diferenca');
  const quadDiferenca = painel.querySelector('.quadrado-diferenca');
  const cardMobile    = painel.querySelector('.card-hover-mobile-alf');

  function formatarMoedaJS(v) {
    return v.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  }

  function atualizarCardMobile(d, isMobile) {
    if (!cardMobile) return;
    if (!isMobile || indiceClicado === null || !d.x || indiceClicado >= d.x.length) {
      cardMobile.style.display = 'none';
      cardMobile.innerHTML = '';
      return;
    }
    const htmlE = d.e_hover[indiceClicado] || '';
    const htmlS = d.s_hover[indiceClicado] || '';
    // Mostra tanto a série clicada em destaque quanto o botão para alternar Entradas/Saídas no card mobile
    const htmlAtivo = (serieClicada === 1) ? htmlS : htmlE;
    cardMobile.innerHTML =
      '<div class=\"topo-card-mobile-alf\">' +
        '<div class=\"abas-card-mobile-alf\">' +
          '<button type=\"button\" class=\"aba-mob-alf ' + (serieClicada === 0 ? 'ativa-e' : '') + '\" data-serie=\"0\">Entradas</button>' +
          '<button type=\"button\" class=\"aba-mob-alf ' + (serieClicada === 1 ? 'ativa-s' : '') + '\" data-serie=\"1\">Saídas</button>' +
        '</div>' +
        '<button type=\"button\" class=\"fechar-card-mobile-alf\" title=\"Fechar\">&times;</button>' +
      '</div>' +
      '<div class=\"corpo-card-mobile-alf\">' + htmlAtivo + '</div>';
    cardMobile.style.display = 'block';

    cardMobile.querySelectorAll('.aba-mob-alf').forEach(function(btnAba) {
      btnAba.addEventListener('click', function() {
        serieClicada = parseInt(this.getAttribute('data-serie'), 10);
        atualizarCardMobile(d, isMobile);
      });
    });

    const btnFechar = cardMobile.querySelector('.fechar-card-mobile-alf');
    if (btnFechar) {
      btnFechar.addEventListener('click', function() {
        indiceClicado = null;
        atualizarGrafico();
      });
    }
  }

  function atualizarRotulosTopo(d) {
    if (indiceClicado !== null && d.x && indiceClicado < d.x.length) {
      const valE  = d.e_y[indiceClicado] || 0;
      const valS  = d.s_y[indiceClicado] || 0;
      const dif   = valE - valS;
      const sinal = dif > 0 ? '+' : (dif < 0 ? '-' : '');
      const corD  = dif >= 0 ? '#15803d' : '#b91c1c';
      const rotX  = String(d.x[indiceClicado] || '').replace(/<[^>]*>/g, '');

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
    const qtdPontos = d.x ? d.x.length : 1;

    const maxBar    = Array.isArray(d.max_bar)  ? d.max_bar[0]  : d.max_bar;
    const maxLine   = Array.isArray(d.max_line) ? d.max_line[0] : d.max_line;
    const maxY      = ehBarra ? maxBar : maxLine;

    atualizarRotulosTopo(d);
    atualizarCardMobile(d, isMobile);

    const tamRotulo = isMobile ? 8.5 : ", tamanho_rotulos, ";
    const tamEixoX  = isMobile ? (qtdPontos > 6 ? 8.5 : 10) : ", tamanho_eixo_x, ";
    const tamEixoY  = isMobile ? 9 : 11;
    const alturaAtu = isMobile ? ", altura_grafico_mobile, " : ", altura_grafico, ";
    const raioAtu   = isMobile ? 5 : ", raio_cantos, ";

    const opacidadeArr = [];
    const bordaLargArr = [];
    const bordaCorArr  = [];

    for (let i = 0; i < qtdPontos; i++) {
      if (indiceClicado === null) {
        opacidadeArr.push(1);
        bordaLargArr.push(0);
        bordaCorArr.push('rgba(0,0,0,0)');
      } else if (i === indiceClicado) {
        opacidadeArr.push(1);
        bordaLargArr.push(2.2);
        bordaCorArr.push('#03045e');
      } else {
        opacidadeArr.push(0.32);
        bordaLargArr.push(0);
        bordaCorArr.push('rgba(0,0,0,0)');
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

    // No Notebook: usa o hover flutuante perfeito. No Mobile: desativa o balão que corta e abre o Card Mobile!
    const traceEntradas = {
      x: d.x,
      y: d.e_y,
      type: ehBarra ? 'bar' : 'scatter',
      mode: ehBarra ? undefined : 'lines+markers+text',
      marker: ehBarra
        ? { color: '", cor_entradas, "', opacity: opacidadeArr, line: { color: bordaCorArr, width: bordaLargArr } }
        : { color: '", cor_entradas, "', size: isMobile ? 6 : 7, opacity: opacidadeArr },
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
      marker: ehBarra
        ? { color: '", cor_saidas, "', opacity: opacidadeArr, line: { color: bordaCorArr, width: bordaLargArr } }
        : { color: '", cor_saidas, "', size: isMobile ? 6 : 7, opacity: opacidadeArr },
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

    const minY = ehBarra ? 0 : -(maxLine * 0.08);
    const larguraContainer = painel.clientWidth || window.innerWidth;

    const novoLayout = Object.assign({}, el.layout, {
      width: larguraContainer,
      height: alturaAtu,
      barmode: 'stack',
      barcornerradius: raioAtu,
      bargap: isMobile ? (qtdPontos > 6 ? 0.20 : 0.35) : (qtdPontos > 10 ? 0.22 : 0.32),
      uniformtext: { mode: 'show', minsize: tamRotulo },
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
        ? { t: 22, b: qtdPontos > 8 ? 38 : 30, l: 44, r: 10 }
        : { t: 28, b: 42, l: 62, r: 12 },
      xaxis: Object.assign({}, el.layout.xaxis, {
        type: 'category',
        categoryorder: 'array',
        categoryarray: d.x,
        tickangle: (isMobile && qtdPontos > 8) ? -45 : 0,
        tickfont: { family: 'Inter, sans-serif', size: tamEixoX, color: '", cor_eixo_x, "' }
      }),
      yaxis: Object.assign({}, el.layout.yaxis, {
        range: [minY, maxY],
        tickfont: { family: 'Inter, sans-serif', size: tamEixoY, color: '#444444' }
      })
    });

    el.style.width  = '100%';
    el.style.height = alturaAtu + 'px';
    Plotly.react(el, [traceEntradas, traceSaidas], novoLayout, {displayModeBar: false, responsive: true});
  }

  el.on('plotly_click', function(data) {
    if (!data || !data.points || !data.points.length) return;
    const pt  = data.points[0];
    const idx = pt.pointIndex;
    const crv = pt.curveNumber || 0;

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
# 8. ESTILO CSS E ESTRUTURA HTML (COM CARD DE HOVER PARA CELULAR)
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
  .painel-fluxo-alf .xtick,
  .painel-fluxo-alf .bars path {
    cursor: pointer !important;
  }
  .barra-controles-alf {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 14px;
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

  /* Card de Hover Mobile (100% dentro da tela, sem nunca cortar) */
  .card-hover-mobile-alf {
    display: none;
    width: 100%;
    box-sizing: border-box;
    margin-top: 8px;
    padding: 10px 12px;
    background: #ffffff;
    border: 1.5px solid #000000;
    border-radius: 8px;
    font-size: 11.5px;
    line-height: 1.45;
    color: #000000;
    text-align: left;
    box-shadow: 0 3px 10px rgba(0,0,0,0.08);
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
    font-size: 11px;
    font-weight: 600;
    padding: 3px 10px;
    border-radius: 4px;
    border: 1px solid #cccccc;
    background: #f8f9fa;
    color: #333333;
    cursor: pointer;
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
    font-size: 18px;
    line-height: 1;
    color: #555555;
    cursor: pointer;
    padding: 0 4px;
  }

  /* =========================================================
     AJUSTES EXCLUSIVOS PARA CELULAR (MOBILE <= 768px)
     ========================================================= */
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
      font-size: 11px;
      padding: 4px 9px;
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