# ==============================================================================
# PROJETO ALF — Bibliotecas
# ==============================================================================
# Configura o repositório do CRAN, evitando erro de "pacote não encontrado"
# em máquinas onde isso ainda não foi definido.
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Padrão usado abaixo: if(!require("pacote")) install.packages("pacote") ; library("pacote")
# Ou seja: só instala o pacote se ele ainda não estiver presente na máquina,
# e carrega em seguida. Evita reinstalar toda vez que o script roda.

# --- A. Importação de dados ---
if(!require("gsheet"))  install.packages("gsheet", quiet = TRUE)  ; library("gsheet")   # Leitura de planilhas Google Sheets (link público)
if(!require("readxl"))  install.packages("readxl", quiet = TRUE)  ; library("readxl")   # Leitura de arquivos Excel (.xlsx)
if(!require("readr"))   install.packages("readr", quiet = TRUE)   ; library("readr")    # Leitura rápida de CSV/dados tabulares

# --- B. Manipulação e visualização ---
if(!require("tidyverse"))    install.packages("tidyverse", quiet = TRUE)    ; library("tidyverse")    # Conjunto de pacotes (dplyr, ggplot2, tidyr, etc.)
if(!require("janitor"))      install.packages("janitor", quiet = TRUE)      ; library("janitor")      # Limpeza de nomes de colunas e dados
if(!require("lubridate"))    install.packages("lubridate", quiet = TRUE)    ; library("lubridate")    # Manipulação de datas
if(!require("stringr"))      install.packages("stringr", quiet = TRUE)      ; library("stringr")      # Manipulação de texto/strings
if(!require("stringi"))      install.packages("stringi", quiet = TRUE)      ; library("stringi")      # Manipulação de texto (motor usado pelo stringr)
if(!require("magrittr"))     install.packages("magrittr", quiet = TRUE)     ; library("magrittr")     # Operador pipe (%>%)
if(!require("reshape2"))     install.packages("reshape2", quiet = TRUE)     ; library("reshape2")     # Reorganização de dados (formato longo/largo)
if(!require("ggrepel"))      install.packages("ggrepel", quiet = TRUE)      ; library("ggrepel")      # Rótulos de texto sem sobreposição nos gráficos
if(!require("geomtextpath")) install.packages("geomtextpath", quiet = TRUE) ; library("geomtextpath") # Texto acompanhando curvas em gráficos
if(!require("ggtext"))       install.packages("ggtext", quiet = TRUE)       ; library("ggtext")       # Formatação rica de texto (markdown/html) em gráficos
if(!require("patchwork"))    install.packages("patchwork", quiet = TRUE)    ; library("patchwork")    # Combina múltiplos gráficos em um painel
if(!require("corrplot"))     install.packages("corrplot", quiet = TRUE)     ; library("corrplot")     # Visualização de matriz de correlação
if(!require("viridis"))      install.packages("viridis", quiet = TRUE)      ; library("viridis")      # Paletas de cores para gráficos
if(!require("showtext"))     install.packages("showtext", quiet = TRUE)     ; library("showtext")     # Fontes customizadas em gráficos

# --- C. Modelagem estatística ---
if(!require("stats"))     install.packages("stats", quiet = TRUE)     ; library("stats")     # Funções estatísticas básicas (nativo do R)
if(!require("nlme"))      install.packages("nlme", quiet = TRUE)      ; library("nlme")      # Modelos mistos/não-lineares
if(!require("broom"))     install.packages("broom", quiet = TRUE)     ; library("broom")     # Transforma resultados de modelos em data frames organizados
if(!require("bizdays"))   install.packages("bizdays", quiet = TRUE)   ; library("bizdays")   # Cálculo de dias úteis
if(!require("survival"))  install.packages("survival", quiet = TRUE)  ; library("survival")  # Análise de sobrevivência
if(!require("survminer")) install.packages("survminer", quiet = TRUE) ; library("survminer") # Gráficos para análise de sobrevivência
if(!require("forecast"))  install.packages("forecast", quiet = TRUE)  ; library("forecast")  # Previsão de séries temporais
if(!require("psych"))     install.packages("psych", quiet = TRUE)     ; library("psych")     # Estatística descritiva e psicometria
if(!require("skimr"))     install.packages("skimr", quiet = TRUE)     ; library("skimr")     # Resumo rápido/visual de data frames
if(!require("Hmisc"))     install.packages("Hmisc", quiet = TRUE)     ; library("Hmisc")     # Funções diversas de estatística/manipulação

# --- D. Relatórios e interatividade ---
if(!require("DT"))         install.packages("DT", quiet = TRUE)         ; library("DT")         # Tabelas interativas (JavaScript)
if(!require("reactable"))  install.packages("reactable", quiet = TRUE)  ; library("reactable")  # Tabelas interativas alternativas
if(!require("kableExtra")) install.packages("kableExtra", quiet = TRUE) ; library("kableExtra") # Tabelas formatadas para relatórios estáticos
if(!require("flextable"))  install.packages("flextable", quiet = TRUE)  ; library("flextable")  # Tabelas formatadas para Word/PowerPoint/HTML
if(!base::require("here")) utils::install.packages("here", quiet = TRUE) ; base::library("here") # Resolve caminhos de ficheiros a partir da raiz do projeto

# Dependências exigidas pelo flextable (sem elas, o Quarto pode falhar ao renderizar)
if(!require("gdtools"))     install.packages("gdtools", quiet = TRUE)     ; library("gdtools")     # Fontes e gráficos vetoriais para flextable
if(!require("systemfonts")) install.packages("systemfonts", quiet = TRUE) ; library("systemfonts") # Acesso às fontes instaladas no sistema

if(!require("officer"))    install.packages("officer", quiet = TRUE)    ; library("officer")    # Geração de documentos Word/PowerPoint
if(!require("htmltools"))  install.packages("htmltools", quiet = TRUE)  ; library("htmltools")  # Construção de elementos HTML
if(!require("sjPlot"))     install.packages("sjPlot", quiet = TRUE)     ; library("sjPlot")     # Visualização de modelos estatísticos
if(!require("fdth"))       install.packages("fdth", quiet = TRUE)       ; library("fdth")       # Tabelas de distribuição de frequência
if(!require("cli"))        install.packages("cli", quiet = TRUE)        ; library("cli")        # Mensagens formatadas no console
if(!require("googlesheets4")) install.packages("googlesheets4", quiet = TRUE) ; library("googlesheets4")
if(!require("here")) install.packages("here", quiet = TRUE) ; library("here")

# --- Gráfico dinâmico ---
if(!require("plotly")) install.packages("plotly", quiet = TRUE) ; library("plotly") # Gráficos interativos (zoom, hover, etc.)
if(!require("tidyr"))  install.packages("tidyr", quiet = TRUE)  ; library("tidyr")  # Reorganização de dados (pivot_longer/wider)
if(!require("dplyr"))  install.packages("dplyr", quiet = TRUE)  ; library("dplyr")  # Manipulação de data frames (filter, mutate, etc.)