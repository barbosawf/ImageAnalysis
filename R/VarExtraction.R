
# Pacotes em uso ----------------------------------------------------------

library('pliman')
library('tidyverse')


# Arquivos para serem analisados ------------------------------------------

# Salva a primeira data de processamento dos dados.
# Também lista e salva a primeira lista de imagens a serem processadas.
if (!file.exists('initial_data.rds')) {

  # Salva a data de primeiro uso do algorítmo.
  saveRDS(Sys.time(), file = 'initial_data.rds')

  # Cria um vetor com os nomes das imagens dentro do diretório ImagesProj.
  img_files <- list.files("ImagesProj")

  # Cria uma lista para que no máximo 150 imagens sejam analizadas por vez.
  # O valor de 150 pode ser modificado de acordo com o usuário.
  split_file_list <-
    split(img_files,
          ((seq_along(img_files) - 1) %/% 150) + 1)

  # Salva a lista para ser posteriormente utilzada.
  saveRDS(split_file_list, file = 'split_file_list.rds')

}


# Obtém a diferença entre a atual lista de imagens e a anterior.
# Também junta as últimas imagens no final da lista anterior de images.
if (Sys.time() > readRDS('initial_data.rds')) {

  # Lê a lista de imagens salva anteriormente.
  split_file_list <- readRDS("split_file_list.rds")

  # Obtém um vetor das imagens listadas
  last_imgs <- as.vector(unlist(split_file_list))

  # Obtém o comprimento da lista de imagens anterior
  l <- length(split_file_list)

  # obtém a diferença
  img_diff <- setdiff(list.files('ImagesProj'), last_imgs)

  # Cria uma lista para que no máximo 150 imagens sejam analizadas por vez.
  # O valor de 150 pode ser modificado de acordo com o usuário.
  split_file_diff <-
    split(img_diff,
          ((seq_along(img_diff) - 1) %/% 150) + 1 + l)

  # Concatena a lista anterior com a nova obtida da diferença
  split_file_list <- c(split_file_list, split_file_diff)

  # Salva a lista para ser posteriormente utilzada.
  saveRDS(split_file_list, file = "split_file_list.rds")

}


# Função analyse_objects() modificada -------------------------------------

# Obtenção dos conjuntos de variáveis -------------------------------------

# Diretório do projeto necessário internamente à rotina do for().
project_path <- rstudioapi::getActiveProject()

# Cria a pasta "Features" para armazenar os objetos salvos
# na rotina criada no for().
dir.create("Features")

# Se após rodar a rotina pela primeira vez e houver imagens que falharam
# na contagem de objetos em estudo, gere o objeto "failed_imgs_list",
# volte novamente neste ponto, remova o hashtag da linha abaixo e
# execute novamente a rotina com diferente argumentos na função
# analyse_objects() para que se possa possam melhorar a identificação
# dos objetos.
# split_file_list <- failed_imgs_list

# Rotina para obtenção dos conjuntos de variáveis.
# Modificar o objeto "split_file_list" quando necessário.
for (i in seq_len(length(split_file_list))) {

  # Item da lista em execução
  list_item <- names(split_file_list[i])

  # Condição que controla a criação dos objetos provenientes
  # da função analyse_objects_mod(). Neste caso, para i >= 1, todos os
  # ítens da lista "split_file_list" serão utilizados. Se i > 1
  # (desde que < que o tamanho da lista), nem todos eles serão utilizados.
  if (i >= 1) {

    # Condição que ajuda no ordenamento dos objetos salvos a partir
    # da função analyse_objects_mod().
    if (i < 10) {
      object_name <- paste0('features_0', list_item)
    } else {
      object_name <- paste0('features_', list_item)
    }

    # Cria uma pasta para cópia temporária de 150 imagens a serem
    # processadas por vez. Este número é definido em "split_file_list".
    dir_ <- "ImagesProj/tmp"
    dir.create(dir_)

    # Copia 150 imagens para a pasta tmp criada.
    # Modificar o objeto "split_file_list" quando necessário.
    p = paste0(project_path, '/ImagesProj/', split_file_list[[i]])
    file.copy(p, dir_)

    analyze_objects(
      # pattern = "" captura todas as imagens.
      pattern = '',
      # Diretório de onde as imagens serão analisadas.
      dir_original = dir_,
      # Diretório para onde as imagens processadas serão salvas.
      dir_processed = 'Proc_ImagesProj',
      # Define se a imagem a ser processada possuir uma referência.
      reference = TRUE,
      # Área da referência.
      reference_area = 18,
      # Índice para remover apenas o fundo.
      # Neste trabalho, ele foi encontrado com invert = FALSE.
      back_fore_index = "SCI",
      # Índice para remover o fundo e os objetos em estudo.
      # Neste trabalho, ele foi encontrado com invert = TRUE.
      fore_ref_index = "HUE2",
      # watershed = FALSE é melhor quando os objetos na
      # imagem não estão muito próximos. Caso estejam muito próximos,
      # ou ligeiramente sobrepostos, mudar para TRUE.
      watershed = TRUE,
      # Adiciona uma identificação nos objetos em estudo.
      marker = "id",
      # Filtro utilizado;
      filter = 4,
      # Preenchimento de buracos nos objetos em estudo.
      fill_hull = TRUE,
      # Executa o processamento em paralelo.
      parallel = TRUE,
      # Não mostrar a imagem processada (o padrão da função é TRUE).
      plot = FALSE,
      # Salva a imagem.
      save_image = TRUE,
      # Vetor de inversão. FALSE para "SCI" e TRUE para "HUE2".
      invert = c(FALSE, TRUE),
      # Cor do fundo da figura salva.
      col_background = 'white',
      # Contorno dos objetos em estudo nas imagens salvas.
      contour_size = 2,
      # Índices utilizados como conjunto de variáveis.
      # É um vetor de IVs que, neste caso, foi definido pela
      # função pliman_indexes() do pacote pliman.
      object_index = pliman_indexes(),
      # Define se as características de textura serão computadas.
      haralick = TRUE,
      # Extração dos coeficientes dos DEF.
      efourier = TRUE,
      # Número de harmônicas dos DEF.
      nharm = 10
    ) -> features

    # Salva o objeto proveniente da função na pasta Features
    saveRDS(get('features'), file = paste0('Features/', object_name, '.rds'))

    # Remove o último objeto criado pela função analyse_objects_mod().
    rm('features')

    # Remove o diretório "tmp"
    unlink(dir_, recursive = TRUE)

    # Faz uma limpeza na memória do sistema durante a rotina do for().
    gc()

  }
}

# Função auxiliar para criar de vetores numéricos como caracteres ---------

str_add_char <- function(x,
                         c1 = "0",
                         c2 = "",
                         n1 = 0,
                         n2 = 0,
                         prefix = "",
                         suffix = "") {
  l <- str_length(x)
  r <- max(l) - l + n1
  map2_vec(r, x, \(r, x) str_c(c(prefix, rep(c1, r), x,
                                 rep(c2, n2), suffix), collapse = ''))


}


# Figuras que falharam em obter o número de objetos em estudo  ------------

# Lista as imagens em que houve falha no número de objetos em estudo.
# Neste trabalho, a falha aconteceu quando um número diferente de 9
# folíolos foi identificado.

# Cria uma lista que pode substituir a lista "split_file_list"
# na rotina do for().
failed_imgs_list <- list()

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("statistics") |>
  bind_rows() |>
  pivot_wider(values_from = value,
              names_from = stat) |>
  filter(n != '9') -> failed_imgs

failed_imgs

# Salva a tabela "failed_imgs" para que posteriormente seja
# lida se necessário.
saveRDS(failed_imgs, "failed_imgs.rds")

# Conta o número de imagens que falharam.
failed_imgs$id |> length()

# Armazena a listagem de imagens que falharam para que sejam novamente
# inseridas na rotina for() diferenciando alguns argumentos de modo que
# possa se obter o número de objetos em estudo contidos nas imagens.
failed_imgs$id |> sort() |> paste0('.jpg') ->
  failed_imgs_list[['0']]

failed_imgs_list

# Salva a lista
saveRDS(failed_imgs_list, "failed_imgs_list.rds")


# Características de forma e textura das imagens sem falha ----------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("results") |>
  bind_rows() |>
  filter(!img %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> results_imgs

results_imgs


# Estatística descritiva da área das imagens sem falha --------------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("statistics") |>
  bind_rows() |>
  pivot_wider(values_from = value,
              names_from = stat) |>
  filter(!id %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(id = fct_relevel(id, \(x) str_sort(x, numeric = T))) |>
  arrange(id) -> statistics_imgs

statistics_imgs


# Média dos IVs de cada objeto em estudo dentro de cada imagem ------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("object_index") |>
  bind_rows() |>
  filter(!img %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> object_index_imgs

object_index_imgs


# Coeficientes de Fourier -------------------------------------------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("efourier") |>
  bind_rows() |>
  filter(!img %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_imgs

efourier_imgs


# Coeficientes de Fourier -------------------------------------------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("efourier_norm") |>
  bind_rows() |>
  filter(!img %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_norm_imgs

efourier_norm_imgs


# Erro entre o contorno original e o reconstruído pelo DEF ----------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("efourier_error") |>
  bind_rows() |>
  filter(!img %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_error_imgs

efourier_error_imgs


# Mínimo de harmônicas ----------------------------------------------------

str_add_char(1:8, n1 = 1,
             prefix = "Features/features_",
             suffix = ".rds") |>
  map(readRDS) |>
  map("efourier_minharm") |>
  bind_rows() |>
  filter(!img %in% c(failed_imgs$id)) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_minharm_imgs

efourier_minharm_imgs


# Imagens que haviam falhado em obter o número de objetos em estudo -------

# ATENÇÂO! Esta etapa deve ser executada apenas se o reprocessamento
# das imagens que haviam falhado em identificar o número de objetos
# em estudo, agora identificaram corretamente. Uma nova filtragem
# também é feita caso haja necessidade de reprocessar novas fallhas.

# Obtém os dados das imagens que haviam faladho.
'Features/features_00.rds' |>
  readRDS() -> features_00

# Lista aquelas imagens em que não houve falha de contagem.
# Neste trabalho o número deveria ser 9.
features_00$count |>
  filter(Objects != 9) -> new_failed_imgs

new_failed_imgs

# Os novos dados após o reprocessamento das imagens são obtidos abaixo.
features_00$results |>
  filter(!img %in% new_failed_imgs$Image) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> results_remaning_imgs

features_00$statistics |>
  filter(!id %in% new_failed_imgs$Image) |>
  pivot_wider(values_from = value,
              names_from = stat) |>
  as_tibble() |>
  mutate(id = fct_relevel(id, \(x) str_sort(x, numeric = T))) |>
  arrange(id) -> statistics_remaining_imgs

features_00$object_index |>
  filter(!img %in% new_failed_imgs$Image) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> object_index_remaning_imgs

features_00$efourier |>
  filter(!img %in% new_failed_imgs$Image) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_remaning_imgs

features_00$efourier_norm |>
  filter(!img %in% new_failed_imgs$Image) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_norm_remaning_imgs

features_00$efourier_error |>
  filter(!img %in% new_failed_imgs$Image) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_error_remaning_imgs

features_00$efourier_minharm |>
  filter(!img %in% new_failed_imgs$Image) |>
  as_tibble() |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_minharm_remaning_imgs


# Novas tabelas com os dados das imagens que haviam falhado ---------------

# As imagens que haviam falhado foram reprocessadas e agora estão certas.

# ATENÇÂO! Esta etapa deve ser executada apenas se houver necessidade
# de adicionar os dados das imagens que haviam anteriormente falhado
# em identificar todos os objetos em estudo, mas que depois de
# reprocessados, houve a identificação correta~.

bind_rows(results_imgs, results_remaning_imgs) |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> results_imgs

bind_rows(statistics_imgs, statistics_remaining_imgs) |>
  mutate(id = fct_relevel(id, \(x) str_sort(x, numeric = T))) |>
  arrange(id) -> statistics_imgs

bind_rows(object_index_imgs, object_index_remaning_imgs) |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> object_index_imgs

bind_rows(efourier_imgs, efourier_remaning_imgs) |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_imgs

bind_rows(efourier_norm_imgs, efourier_norm_remaning_imgs) |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_norm_imgs

bind_rows(efourier_error_imgs, efourier_error_remaning_imgs) |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_error_imgs

bind_rows(efourier_minharm_imgs, efourier_minharm_remaning_imgs) |>
  mutate(img = fct_relevel(img, \(x) str_sort(x, numeric = T)),
         id = as_factor(id)) |>
  arrange(img) -> efourier_minharm_imgs


# Salvando os conjuntos de variáveis --------------------------------------

# Lista os diferentes conjuntos de variáveis
set_var <- list(
  'results_imgs',
  'statistics_imgs',
  'object_index_imgs',
  'efourier_imgs',
  'efourier_norm_imgs',
  'efourier_error_imgs',
  'efourier_minharm_imgs'
)

# Cria a pasta para salvamento
dir.create("Sheets")

# Salva as tabelas em formato xlsx
set_var |>
  map(\(x) writexl::write_xlsx(get(x),
                               path = paste0("Sheets/",
                                             x,
                                             ".xlsx")))



