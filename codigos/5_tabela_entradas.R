# ==============================================================================
# --- TABELA DE ENTRADAS (ORDENAÇÃO INVISÍVEL + BARRA PRETA INTACTA) ---
# ==============================================================================

# 1. LER AS IMAGENS E CONVERTER PARA BASE64
# A função knitr::image_uri() pega no ficheiro físico da imagem e transforma-o 
# num código de texto (Base64). Isto garante que a foto apareça sempre, 
# sem links partidos, mesmo que mude o site de servidor.


# uri_alex <- knitr::image_uri("imagens/alex_transparente.png")
# uri_leticia <- knitr::image_uri("imagens/leticia_transparente.png")

# 1. LER AS IMAGENS E CONVERTER PARA BASE64
# Usamos o here::here() para garantir que o R procura as imagens a partir
# da pasta raiz do projeto (onde está o seu ficheiro .Rproj), independentemente
# de onde este script .R esteja guardado.
uri_alex <- knitr::image_uri(here::here("imagens", "alex_transparente.png"))
uri_leticia <- knitr::image_uri(here::here("imagens", "leticia_transparente.png"))


# 2. CSS CUSTOMIZADO DA TABELA
# Aqui definimos toda a "maquilhagem" da tabela.
estilo_abnt <- htmltools::tags$style("
  /* Importa a biblioteca de ícones minimalistas (Bootstrap Icons) da internet */
  @import url('https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css');

  /* --- ESTILO GERAL E LARGURA DA TABELA --- */
  .dataTables_wrapper {
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
    max-width: 900px !important; /* Limita a largura da tabela para um visual compacto e elegante */
    width: 100% !important;
    margin: 30px auto !important; /* O 'auto' nas margens centraliza a tabela na página */
    overflow: hidden !important; 
  }
  
  table.dataTable {
    font-size: 13px !important; /* Tamanho da fonte de todos os dados dentro da tabela */
    border-collapse: separate !important; /* <--- ALTERADO AQUI: 'separate' é necessário para os cantos arredondados aparecerem (antes era 'collapse') */
    border-spacing: 0 !important;         /* <--- NOVO: sem espaço entre as células, para o visual continuar igual ao de antes */
    width: 100% !important;
  }
  
  /* Define o espaço em branco dentro de cada célula (cima/baixo, esquerda/direita) */
  table.dataTable tbody th, table.dataTable tbody td,
  table.dataTable thead th, table.dataTable thead td {
    padding: 12px 12px !important; 
  }
  
  /* --- ESTILO DE CABEÇALHO --- */
  table.dataTable thead th, table.dataTable thead td {
    border: none !important; /* Remove as bordas normais do cabeçalho */
    text-transform: none !important; 
    font-weight: 600 !important; /* Deixa o texto em negrito suave (semi-bold) */
    font-size: 14px !important;
    position: relative !important; /* Necessário para desenharmos a barra preta depois */
    padding-bottom: 22px !important; /* Cria um espaço no fundo da célula para caber a barra preta */
    
    /* CRÍTICO: Bloqueia a formatação nativa do DataTables.
       Remove o fundo cinzento e as imagens nativas ao ativar a ordenação. */
    background-image: none !important;
    background-color: transparent !important;
  }
  
  /* Estiliza os ícones do Bootstrap dentro do cabeçalho (ex: o calendário, a mala) */
  table.dataTable thead th i {
    margin-right: 6px !important; /* Dá um espacinho entre o ícone e a palavra */
    font-size: 15px !important;   /* Tamanho do ícone */
    color: #444444 !important;    /* Cor cinzento escuro */
  }
  
  /* =======================================================================
     A NOSSA BARRA PRETA (Desenhada com CSS ::after)
     Mantida intacta e colada ao fundo, ignorando a formatação do DataTables.
     ======================================================================= */
  table.dataTable thead th::after,
  table.dataTable thead th.sorting::after,
  table.dataTable thead th.sorting_asc::after,
  table.dataTable thead th.sorting_desc::after {
    content: '' !important;
    position: absolute !important;
    left: 10% !important;         /* Inicia a barra aos 10% da largura, criando o espaço em branco entre as colunas */
    right: auto !important;
    bottom: 0 !important;         /* Cola a barra à base do cabeçalho */
    top: auto !important;         /* CRÍTICO: Impede o DataTables de puxar a barra para o centro da célula (top: 50%) */
    margin: 0 !important;         /* CRÍTICO: Remove margens ocultas que o DataTables injeta */
    width: 80% !important;        /* A barra ocupa 80% da célula (10% de folga de cada lado) */
    height: 2px !important;       /* Espessura da barra preta */
    background-color: #000000 !important; /* Cor da barra */
    border-radius: 4px !important; /* Arredonda as pontinhas da barra */
    opacity: 1 !important;        /* Garante que a barra nunca fica transparente */
    color: transparent !important;
  }
  
  /* =======================================================================
     OCULTAÇÃO DAS SETAS DE ORDENAÇÃO NATIVAS
     Mantém o cabeçalho clicável, mas esconde completamente qualquer símbolo visual.
     ======================================================================= */
  table.dataTable thead th.sorting, 
  table.dataTable thead th.sorting_asc, 
  table.dataTable thead th.sorting_desc {
    cursor: pointer !important; /* Mostra a 'mãozinha' ao passar o rato para indicar interatividade */
  }
  
  /* Bloqueia o aparecimento de qualquer seta gerada pelo DataTables via ::before */
  table.dataTable thead th.sorting::before,
  table.dataTable thead th.sorting_asc::before,
  table.dataTable thead th.sorting_desc::before {
    content: none !important;
    display: none !important;
  }

  /* --- CORPO DA TABELA --- */
  table.dataTable tbody tr { background-color: transparent !important; }
  
  table.dataTable tbody tr td {
    border-top: none !important;                 /* <--- ALTERADO AQUI: Remove a linha que fica colada ao cabeçalho */
    # border: 1px solid #e0e0e0 !important;      /* Restaura as linhas finas cinzentas (tipo Excel) */
    # border-top: 1px solid #e0e0e0 !important;  /* Linha de CIMA */
    border-bottom: 1px solid #e0e0e0 !important; /* Linha de BAIXO */
    border-left: 1px solid #e0e0e0 !important;   /* Linha da ESQUERDA */
    border-right: 1px solid #e0e0e0 !important;  /* Linha da DIREITA */
    
    background-color: #ffffff !important; /* Fundo branco puro para tapar os pontos flutuantes do site */
    box-shadow: none !important;
  }
  
  /* <--- NOVO: com 'separate' as bordas de células vizinhas deixam de se fundir numa só.
     Só a 1ª coluna mantém a linha da esquerda, para as linhas verticais não ficarem duplicadas. */
  table.dataTable tbody tr td:not(:first-child) { border-left: none !important; }
  
  /* Borda final em baixo da tabela inteira */
  /* <--- ALTERADO AQUI: agora quem desenha a linha de baixo é a última linha de células (que também fica arredondada). */
  table.dataTable.no-footer { border-bottom: none !important; }
  
  /* =======================================================================
     <--- NOVO: CANTOS ARREDONDADOS APENAS EM BAIXO
     A última linha visível da página (a 1ª e a última célula dela) ganha
     o raio nos cantos de baixo. Para mudar o quanto arredonda, altere o 12px.
     ======================================================================= */
  table.dataTable tbody tr:last-child td:first-child { border-bottom-left-radius: 12px !important; }
  table.dataTable tbody tr:last-child td:last-child  { border-bottom-right-radius: 12px !important; }
  
  /* Efeito Hover (passar o rato) e Selected (clicar na linha) */
  table.dataTable tbody tr:hover td { background-color: #f9f9f9 !important; /* Fica cinzento muito clarinho */ }
  table.dataTable tbody tr.selected td { background-color: #eaeaea !important; color: #000000 !important; }
  
  /* --- RODAPÉ E CONTROLES (Paginação, Mostrar X registos) --- */
  /* Substitui os 'floats' antigos por Flexbox para evitar que os elementos vazem da tabela */
  .dataTables_wrapper .bottom {
    display: flex !important; flex-wrap: wrap !important; align-items: center !important;
    justify-content: space-between !important; padding-top: 15px !important;
    margin-top: 5px !important; width: 100% !important;
  }
  
  .dataTables_info, .dataTables_length, .dataTables_paginate {
    float: none !important; margin: 0 !important; padding: 0 !important;
    font-size: 12px !important; color: #555555 !important;
  }
  
  .dataTables_length { margin-right: 20px !important; }
  .dataTables_info { margin-right: auto !important; } /* O 'auto' empurra a paginação toda para o lado direito */
  
  /* Formatação da Barra de Pesquisa Minimalista */
  .dataTables_filter { float: right !important; text-align: right !important; margin-bottom: 15px !important; font-size: 11px !important; }
  .dataTables_filter input { 
    width: 160px !important; 
    border: 1px solid #cccccc 
    !important; border-radius: 4px !important; 
    padding: 4px 8px !important; 
    background-color: #ffffff !important; 
    outline: none !important; 
    font-size: 15px !important; }
  .dataTables_filter input:focus { 
    border-color: #000000 !important; /* A borda da caixa fica preta ao digitar */ }
  .dataTables_length select { border: 1px solid #cccccc !important; 
    border-radius: 4px !important; 
    padding: 2px 6px !important; 
    background-color: #ffffff !important; 
    outline: none !important; }
  
  /* Estilização dos Botões de Paginação (1, 2, Próximo, Anterior) */
  .dataTables_wrapper .dataTables_paginate .paginate_button { color: #000000 !important; border: 1px solid transparent !important; border-radius: 4px !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background: #f9f9f9 !important; border: 1px solid #cccccc !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current, 
  .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover { background: #000000 !important; color: #ffffff !important; border: 1px solid #000000 !important; }
  
  /* =======================================================================
     <--- NOVO: CELULAR E TABLET (até 768px de largura)
     Em tela pequena as 6 colunas não cabem lado a lado. Em vez de cortar as
     colunas da direita (Descrição e Usuário), a tabela mantém uma largura mínima
     legível e rola para o lado. A pesquisa e a paginação ficam fixas na
     esquerda, sem rolar junto. No computador nada muda.
     ======================================================================= */
  @media (max-width: 768px) {
    .dataTables_wrapper { overflow-x: auto !important; overflow-y: hidden !important; }
    table.dataTable { min-width: 640px !important; }
    .dataTables_wrapper .top, .dataTables_wrapper .bottom { position: sticky !important; left: 0 !important; }
  }
")


# 3. PREPARAÇÃO DOS DADOS (COM LÓGICA DE ORDENAÇÃO OCULTA)
bd_entradas_formatado <- dplyr::mutate(
  bd_entradas,
  # Se a data for NA (vazia), coloca um '-', se não, transforma o dado em texto.
  data = base::ifelse(base::is.na(data), "-", base::as.character(data)),
  
  # TRUQUE DE ORDENAÇÃO NUMÉRICA DO VALOR:
  # Como adicionámos um 'R$' via HTML, a tabela tentaria ordenar tudo alfabeticamente pela letra 'R'.
  # A solução: Injetamos o valor puro (ex: 000001000.00) invisível (<span display:none>) ANTES do Flexbox visual.
  # A tabela lê o número escondido e ordena matematicamente da forma correta.
  valor = base::paste0(
    "<span style='display: none;'>", base::sprintf("%015.2f", base::as.numeric(valor)), "</span>",
    "<div style='display: flex; justify-content: space-between; width: 100%; min-width: 75px;'>",
    "<span>R$</span> <span>", base::format(valor, nsmall = 2, big.mark = ".", decimal.mark = ","), "</span>",
    "</div>"
  ),
  
  # LÓGICA DO USUÁRIO: Verifica primeiro se está vazio. Se não estiver, verifica quem é.
  usuario_id = base::ifelse(
    base::is.na(usuario_id) | base::trimws(base::as.character(usuario_id)) == "",
    "<div style='display: flex; justify-content: center; align-items: center; height: 35px; color: #cccccc;'>-</div>",
    base::ifelse(
      base::grepl("Leticia", usuario_id, ignore.case = TRUE),
      base::paste0("<div style='display: flex; justify-content: center;'><span style='display: none;'>Letícia</span><img src='", uri_leticia, "' style='width: 35px; height: 35px; border-radius: 50%; object-fit: cover;'></div>"),
      base::paste0("<div style='display: flex; justify-content: center;'><span style='display: none;'>Alex</span><img src='", uri_alex, "' style='width: 35px; height: 35px; border-radius: 50%; object-fit: cover;'></div>")
    )
  )
  
  # TRUQUE DE ORDENAÇÃO E FILTRO ALFABÉTICO DO UTILIZADOR:
  # O mesmo princípio de cima. O nome real (Letícia ou Alex) é colocado de forma invisível
  # no início da célula. Assim, a tabela consegue ordenar os utilizadores por A-Z e a barra
  # de pesquisa continua a encontrá-los quando escrevemos, mesmo só exibindo a foto.
  
  # usuario_id = base::ifelse(
  #   base::grepl("Leticia", usuario_id, ignore.case = TRUE),
  #   base::paste0("<div style='display: flex; justify-content: center;'><span style='display: none;'>Letícia</span><img src='", uri_leticia, "' style='width: 35px; height: 35px; border-radius: 50%; object-fit: cover;'></div>"),
  #   base::paste0("<div style='display: flex; justify-content: center;'><span style='display: none;'>Alex</span><img src='", uri_alex, "' style='width: 35px; height: 35px; border-radius: 50%; object-fit: cover;'></div>")
  # )
)

# 4. TRADUÇÃO MANUAL DA TABELA
# O pacote DT gera a tabela em Inglês por defeito. Aqui traduzimos os avisos e botões para PT-PT.
traducao_pt_pt <- base::list(
  sEmptyTable = "Nenhum registo encontrado",
  sInfo = "A mostrar de _START_ até _END_ de _TOTAL_ registos",
  sInfoEmpty = "A mostrar 0 até 0 de 0 registos",
  sInfoFiltered = "(Filtrados de _MAX_ registos)",
  sLengthMenu = "Mostrar _MENU_ resultados", 
  sLoadingRecords = "A carregar...",
  sProcessing = "A processar...",
  sZeroRecords = "Nenhum registo encontrado",
  sSearch = "<b>Pesquisa:</b>",
  oPaginate = base::list(
    sNext = "Próximo",
    sPrevious = "Anterior",
    sFirst = "Primeiro",
    sLast = "Último"
  )
)

# 5. GERAÇÃO DA TABELA DT
tabela_dt <- DT::datatable(
  bd_entradas_formatado,
  rownames = FALSE,   # Esconde a coluna lateral inútil com a numeração das linhas
  escape = FALSE,     # CRÍTICO: Permite que o código HTML (fotos, ícones, spans ocultos) seja renderizado e não lido como texto cru
  
  class = 'hover',    # Ativa a classe nativa do DataTables que destaca a linha ao passar o rato
  
  # Injecta o HTML dos ícones do Bootstrap diretamente nos nomes dos cabeçalhos
  colnames = base::c(
    "<i class='bi bi-calendar-event'></i> Data", 
    "<i class='bi bi-cash-stack'></i> Valor", 
    "<i class='bi bi-briefcase'></i> Fonte Renda", 
    "<i class='bi bi-arrow-repeat'></i> Frequência", 
    "<i class='bi bi-file-earmark-text'></i> Descrição", 
    "<i class='bi bi-people'></i> Usuário"
  ),
  
  selection = 'single', # Permite que se clique para selecionar apenas 1 linha de cada vez
  
  options = base::list(
    pageLength = 5,     # Mostra apenas 5 registos por página
    autoWidth = FALSE,  # Desliga a largura automática do DT (para respeitar o nosso CSS de max-width 900px)
    scrollX = FALSE,    # Remove a barra de scroll horizontal
    ordering = TRUE,    # ATIVA A ORDENAÇÃO NA TABELA (Clique no cabeçalho)
    
    # DOM: Define onde as peças da tabela ficam no ecrã.
    # f = filtro (Pesquisa) na <div class="top">
    # rt = Tabela em si (row table)
    # ilp = Info (Mostrar x a y), Length (Menu de 5, 10...) e Pagination na <div class="bottom">
    dom = '<"top"f>rt<"bottom"ilp><"clear">', 
    
    columnDefs = base::list(
      base::list(orderable = TRUE, targets = "_all"),                  # Garante que TODAS as colunas podem ser ordenadas clicando no cabeçalho
      base::list(className = 'dt-head-center', targets = "_all"),      # Centraliza os títulos (Cabeçalho)
      base::list(className = 'dt-center', targets = base::c(0, 2, 3)), # Centraliza as colunas: Data, Fonte Renda, Frequência
      base::list(className = 'dt-left', targets = base::c(4, 5)),      # Alinha à esquerda: Descrição, Usuário
      base::list(width = '90px', targets = 1)                          # Força uma largura fixa para a coluna do Valor (coluna de índice 1)
    ),
    language = traducao_pt_pt,
    
    # 6. INTERATIVIDADE JAVASCRIPT (O FILTRO DA FOTO)
    # Usamos htmlwidgets::JS para injetar código JavaScript na inicialização da tabela.
    initComplete = htmlwidgets::JS(
      "function(settings, json) {",
      "  var table = this.api();",
      "  ",
      "  // PARTE A: Cria o Efeito Visual de 'Hover' apenas na coluna do utilizador (6ª coluna)",
      "  // Quando o rato entra (mouseenter), a imagem aumenta 15% (scale 1.15) e o cursor vira mãozinha.",
      "  // Quando o rato sai (mouseleave), a imagem volta ao tamanho normal (scale 1).",
      "  $(table.table().body()).on('mouseenter', 'td:nth-child(6)', function() {",
      "    $(this).css('cursor', 'pointer');",
      "    $(this).find('img').css({'transform': 'scale(1.15)', 'transition': 'all 0.2s ease'});",
      "  }).on('mouseleave', 'td:nth-child(6)', function() {",
      "    $(this).find('img').css('transform', 'scale(1)');",
      "  });",
      "  ",
      "  // PARTE B: Função ao clicar na foto (Coluna 6) para filtrar a tabela.",
      "  $(table.table().body()).on('click', 'td:nth-child(6)', function() {",
      "    var nome = $(this).text().trim(); // Lê o nome invisível (Alex ou Letícia) que deixámos na célula.",
      "    ",
      "    // Verifica se a caixa de pesquisa geral JÁ CONTÉM o nome que foi clicado",
      "    if (table.search() === nome) {",
      "      table.search('').draw(); // Se sim, esvazia a pesquisa, desenha a tabela novamente e mostra todos.",
      "    } else {",
      "      table.search(nome).draw(); // Se não, aplica o nome na pesquisa e desenha a tabela filtrada.",
      "    }",
      "  });",
      "}"
    )
  )
)

# 7. RENDERIZAÇÃO FINAL (A JUNÇÃO)
# A função htmltools::tagList é a "cola" que permite juntar uma folha de estilos (estilo_abnt)
# com o nosso widget HTML gerado (tabela_dt). 
# A htmltools::browsable diz ao RStudio/Quarto que isto é para ser exibido como uma página da web interativa.
tabela_final <- htmltools::browsable(
  htmltools::tagList(
    estilo_abnt,
    tabela_dt
  )
)
