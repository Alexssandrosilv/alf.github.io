# ==============================================================================
# PROJETO ALF — Tratamento dos dados
# ==============================================================================
# Cada bloco abaixo pega a base bruta (vinda do 2_leitura.R) e sobrescreve
# a mesma variável já tratada:
# 1. Remove linhas totalmente vazias
# 2. Converte datas de texto para Date
# 3. Converte valores em formato brasileiro (vírgula decimal) para numeric
# 4. Converte colunas categóricas para factor

# --- BASE DE ENTRADAS (bd_entradas: bruta -> tratada) ---

bd_entradas <- bd_entradas %>%
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !base::is.na(.))) %>%
  dplyr::mutate(
    # data: texto "dd/mm/aaaa" -> Date
    data = base::as.Date(data),
    
    # valor: "1566,19" (formato BR, vírgula decimal) -> número
    valor = base::as.numeric(valor),
    
    # colunas categóricas -> factor
    fonte_renda            = base::as.factor(fonte_renda),
    frequencia_recebimento = base::as.factor(frequencia_recebimento),
    usuario_id             = base::as.factor(usuario_id)
  )

# --- BASE DE SAÍDAS (bd_saidas: bruta -> tratada) ---

bd_saidas <- bd_saidas %>%
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !base::is.na(.))) %>%
  dplyr::mutate(
    # datas: texto "dd/mm/aaaa" -> Date
    data_compra    = base::as.Date(data_compra),
    data_pagamento = base::as.Date(data_pagamento),
    
    # valor: formato BR -> número
    valor = base::as.numeric(valor),
    
    # colunas categóricas -> factor
    item_consumo    = base::as.factor(item_consumo),
    tipo_despesa    = base::as.factor(tipo_despesa),
    forma_pagamento = base::as.factor(forma_pagamento),
    status          = base::as.factor(status),
    usuario_id      = base::as.factor(usuario_id),
    mes_extenso_pag = base::as.factor(mes_extenso_pag)
  )

# --- BASE DE LISTA DE DESEJOS (bd_lista_desejos: bruta -> tratada) ---

# bd_lista_desejos <- bd_lista_desejos %>%
#   dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !base::is.na(.))) %>%
#   dplyr::mutate(
#     itens = base::as.factor(itens),
#     
#     # fator ORDENADO: baixa < média < alta (permite comparação/ordenação)
#     prioridade = base::factor(prioridade, levels = base::c("baixa", "média", "alta"), ordered = TRUE),
#     
#     # texto, não numérico: valores são intervalos ("600 a 800") ou abertos ("2000+")
#     faixa_preco = base::as.factor(faixa_preco),
#     
#     usuario_id = base::as.factor(usuario_id)
#     
#     # descricao permanece como texto livre (character), sem conversão
#   )

# --- BASE DE ACOMPANHAMENTO (bd_acompanhamento: bruta -> tratada) ---

bd_acompanhamento <- bd_acompanhamento %>%
  # Ignora linhas que só tenham o usuario_id pré-selecionado na planilha, mas sem registro real
  dplyr::filter(!base::is.na(compromisso) | !base::is.na(horas) | !base::is.na(data)) %>%
  dplyr::mutate(
    # data: texto "dd/mm/aaaa" -> Date
    data = base::as.Date(data),
    
    # horas: garante formato numérico decimal (ex: 1.5, 2.0)
    horas = base::as.numeric(horas),
    
    # colunas categóricas -> factor (identifica os níveis automaticamente)
    compromisso = base::as.factor(compromisso),
    categoria   = base::as.factor(categoria),
    usuario_id  = base::as.factor(usuario_id)
    
    # descricao permanece como texto livre (character), sem conversão
  ) %>%
  # Garante a ordem das colunas para a tabela DT (usuario_id na 6ª coluna para o clique na foto)
  dplyr::select(data, compromisso, categoria, horas, descricao, usuario_id)