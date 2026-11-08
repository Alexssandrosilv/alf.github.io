# ==============================================================================
# PROJETO ALF — Leitura dos dados (Google Sheets)
# ==============================================================================

# Pega o ID da planilha a partir da variável de ambiente
# (nunca fixo no código — configurado no .Renviron local ou como secret no CI)
spreadsheet_id <- Sys.getenv("SPREADSHEET_ID")

# Para com uma mensagem clara se o ID não estiver configurado
if (!nzchar(spreadsheet_id)) {
  stop("SPREADSHEET_ID não configurado (.Renviron local ou secret no GitHub).")
}

# Monta a URL completa da planilha a partir do ID
url_mestre <- paste0("https://docs.google.com/spreadsheets/d/", spreadsheet_id, "/edit")

# Lê cada aba da planilha como um data frame separado (dados ainda brutos, sem tratamento)
bd_entradas      <- googlesheets4::read_sheet(url_mestre, sheet = "entradas")
bd_saidas        <- googlesheets4::read_sheet(url_mestre, sheet = "saidas")
bd_lista_desejos <- googlesheets4::read_sheet(url_mestre, sheet = "lista_desejos")
