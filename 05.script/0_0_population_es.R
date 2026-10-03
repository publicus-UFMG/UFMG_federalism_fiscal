# Este codigo visa obter os dados populacionais da espanha para os três entes federativos 
# (união, comunidade autonomas e municipios).


# limpar objetos salvos:
rm(list = ls())

# Carregar pacotes necessários:

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, stringr, readr, data.table, ggplot2, httr2, jsonlite, purrr, rio)

# ============================================================
# Fonte: API Tempus do INE, tabela 29005 
# "Cifras oficiales del padrón por municipio" (DPOP)
# ============================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

ANO_INI <- 2010
ANO_FIM <- as.integer(format(Sys.Date(), "%Y"))   # a API devolve só o que existir
TABELA  <- 29005
BASE    <- "https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/"

# Download da API do INE (JSON) com filtro de sexo (total) e anos desejados:

baixar <- function(filtrar_sexo = TRUE) {
  q <- sprintf("?tip=AM&date=%d0101:%d1231", ANO_INI, ANO_FIM)
  if (filtrar_sexo) q <- paste0(q, "&tv=18:62")    # variável Sexo (18) = Total (62)
  request(paste0(BASE, TABELA, q)) |>
    req_user_agent("R-httr2 coleta INE") |>
    req_timeout(600) |>
    req_retry(max_tries = 4, backoff = ~ 10) |>
    req_perform() |>
    resp_body_string() |>
    fromJSON(simplifyVector = FALSE)
}

message("Baixando tabela ", TABELA, " do INE...")
series <- tryCatch(baixar(TRUE), error = function(e) {
  message("Filtro de sexo falhou (", conditionMessage(e), "); baixando sem filtro...")
  baixar(FALSE)
})
message("Séries recebidas: ", length(series))

# Parsing do JSON para formato longo (long):

eh_sexo <- function(m) {
  grepl("^(total|ambos sexos|hombres|mujeres)$", trimws(m$Nombre %||% ""), ignore.case = TRUE)
}

parse_serie <- function(s) {
  meta <- s$MetaData
  if (length(meta) == 0) return(NULL)

  is_sexo <- vapply(meta, eh_sexo, logical(1))
  sexo <- meta[is_sexo]
  muni <- meta[!is_sexo]
  if (length(sexo) == 0 || length(muni) == 0) return(NULL)

  # só "Total" (ambos os sexos)
  if (!grepl("^(total|ambos)", sexo[[1]]$Nombre, ignore.case = TRUE)) return(NULL)
  muni <- muni[[1]]

  nome_bruto <- muni$Nombre
  cod <- muni$Codigo %||% ""
  if (!nzchar(cod)) cod <- str_extract(nome_bruto, "^\\d{5}") %||% NA_character_

  tibble(
    codigo_ine    = cod,
    codigo_tempus = as.character(muni$Id),
    nome          = str_remove(nome_bruto, "^\\d{5}\\s+"),
    ano           = map_int(s$Data, ~ as.integer(.x$Anyo)),
    valor         = map_dbl(s$Data, ~ as.numeric(.x$Valor %||% NA))
  )
}

long <- map(series, parse_serie) |> compact() |> bind_rows()

# diagnóstico: se vier vazio, mostra a estrutura real da primeira série
if (nrow(long) == 0) {
  str(series[[1]], max.level = 3)
  stop("Parsing não encontrou séries; envie a saída do str() acima.")
}

long <- long |> filter(ano >= ANO_INI, ano <= ANO_FIM)

anos <- sort(unique(long$ano))
message("Anos disponíveis: ", min(anos), "–", max(anos))

para_largo <- function(df, chaves) {
  df |>
    pivot_wider(id_cols = all_of(chaves), names_from = ano, values_from = valor) |>
    select(all_of(chaves), all_of(as.character(anos)))
}

# Banco dos municípios (códigos INE 01001–52999) com anos como colunas:

municipios <- long |>
  distinct(codigo_ine, codigo_tempus, nome, ano, valor) |>
  para_largo(c("codigo_ine", "codigo_tempus", "nome")) |>
  arrange(codigo_ine)

# Banco das comunidades autônomas (códigos INE 01–19) com anos como colunas:

ccaa_tab <- tribble(
  ~cod_ccaa, ~nome_ccaa,                       ~provincias,
  "01", "Andalucía",                           c("04","11","14","18","21","23","29","41"),
  "02", "Aragón",                              c("22","44","50"),
  "03", "Asturias, Principado de",             c("33"),
  "04", "Balears, Illes",                      c("07"),
  "05", "Canarias",                            c("35","38"),
  "06", "Cantabria",                           c("39"),
  "07", "Castilla y León",                     c("05","09","24","34","37","40","42","47","49"),
  "08", "Castilla - La Mancha",                c("02","13","16","19","45"),
  "09", "Cataluña",                            c("08","17","25","43"),
  "10", "Comunitat Valenciana",                c("03","12","46"),
  "11", "Extremadura",                         c("06","10"),
  "12", "Galicia",                             c("15","27","32","36"),
  "13", "Madrid, Comunidad de",                c("28"),
  "14", "Murcia, Región de",                   c("30"),
  "15", "Navarra, Comunidad Foral de",         c("31"),
  "16", "País Vasco",                          c("01","20","48"),
  "17", "Rioja, La",                           c("26"),
  "18", "Ceuta",                               c("51"),
  "19", "Melilla",                             c("52")
) |>
  unnest(provincias) |> rename(cod_prov = provincias)

soma <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

ccaa <- long |>
  mutate(cod_prov = substr(codigo_ine, 1, 2)) |>
  left_join(ccaa_tab, by = "cod_prov") |>
  group_by(cod_ccaa, nome_ccaa, ano) |>
  summarise(valor = soma(valor), .groups = "drop") |>
  mutate(codigo_secundario = NA_character_) |>
  rename(codigo_ine = cod_ccaa, nome = nome_ccaa) |>
  para_largo(c("codigo_ine", "codigo_secundario", "nome")) |>
  arrange(codigo_ine) |> 
  select(-codigo_secundario)

# Banco da União (código INE 00) com anos como colunas:
uniao <- long |>
  group_by(ano) |>
  summarise(valor = soma(valor), .groups = "drop") |>
  mutate(codigo_ine = "00", codigo_secundario = NA_character_, nome = "Total Nacional") |>
  para_largo(c("codigo_ine", "codigo_secundario", "nome"))

# Conferência de consistência: todos os municípios devem ter código INE válido (não NA):

stopifnot(all(is.na(ccaa$codigo_ine) == FALSE))
message("Municípios: ", nrow(municipios), " | CCAA: ", nrow(ccaa))
message("Total nacional no último ano: ", format(uniao[[as.character(max(anos))]], big.mark = "."))
message("Soma das CCAA = total nacional? ",
        all.equal(sum(ccaa[[as.character(max(anos))]]), uniao[[as.character(max(anos))]]))

# Exportação com o pacote rio:

export(uniao, "02.metadata/pop_uniao_es.rds")
export(ccaa, "02.metadata/pop_regional_es.rds")
export(municipios, "02.metadata/pop_local_es.rds")

