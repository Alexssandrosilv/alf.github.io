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
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) %>%
  dplyr::mutate(
    # data: texto "dd/mm/aaaa" -> Date
    data = as.Date(data),
    
    # valor: "1566,19" (formato BR, vírgula decimal) -> número
    valor = as.numeric(valor),
    
    # colunas categóricas -> factor
    fonte_renda            = as.factor(fonte_renda),
    frequencia_recebimento = as.factor(frequencia_recebimento),
    usuario_id             = as.factor(usuario_id)
  )

# --- BASE DE SAÍDAS (bd_saidas: bruta -> tratada) ---

bd_saidas <- bd_saidas %>%
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) %>%
  dplyr::mutate(
    # datas: texto "dd/mm/aaaa" -> Date
    data_compra    = as.Date(data_compra),
    data_pagamento = as.Date(data_pagamento),
    
    # valor: formato BR -> número
    valor = as.numeric(valor),
    
    # colunas categóricas -> factor
    item_consumo    = as.factor(item_consumo),
    tipo_despesa    = as.factor(tipo_despesa),
    forma_pagamento = as.factor(forma_pagamento),
    status          = as.factor(status),
    usuario_id      = as.factor(usuario_id),
    mes_extenso_pag = as.factor(mes_extenso_pag)
  )

# --- BASE DE LISTA DE DESEJOS (bd_lista_desejos: bruta -> tratada) ---

# bd_lista_desejos <- bd_lista_desejos %>%
#   dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) %>%
#   dplyr::mutate(
#     itens = as.factor(itens),
#     
#     # fator ORDENADO: baixa < média < alta (permite comparação/ordenação)
#     prioridade = factor(prioridade, levels = c("baixa", "média", "alta"), ordered = TRUE),
#     
#     # texto, não numérico: valores são intervalos ("600 a 800") ou abertos ("2000+")
#     faixa_preco = as.factor(faixa_preco),
#     
#     usuario_id = as.factor(usuario_id)
#     
#     # descricao permanece como texto livre (character), sem conversão
#   )