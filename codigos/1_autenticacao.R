# ==============================================================================
# PROJETO ALF — Autenticação (Google Sheets)
# ==============================================================================

# Permissão só de leitura: o projeto nunca escreve na planilha
escopo_leitura <- "https://www.googleapis.com/auth/spreadsheets.readonly"

# Verifica se está rodando no GitHub Actions (essa variável só existe lá)
if (nzchar(Sys.getenv("GCP_CREDENTIALS_JSON"))) {

  # Ambiente CI: grava a credencial do secret num arquivo temporário
  writeLines(Sys.getenv("GCP_CREDENTIALS_JSON"), "credentials_temp.json")
  googlesheets4::gs4_auth(path = "credentials_temp.json", scopes = escopo_leitura)

  # O token já está na memória; o arquivo com a chave não precisa continuar no disco
  unlink("credentials_temp.json")

} else {

  # Ambiente local: usa o caminho do .json configurado no .Renviron
  caminho_credencial <- Sys.getenv("GCP_CREDENTIALS")

  if (!nzchar(caminho_credencial) || !file.exists(caminho_credencial)) {
    stop("GCP_CREDENTIALS não configurada ou arquivo não encontrado. Verifique o .Renviron.")
  }

  googlesheets4::gs4_auth(path = caminho_credencial, scopes = escopo_leitura)
}
