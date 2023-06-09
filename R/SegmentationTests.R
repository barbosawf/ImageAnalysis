# Pacotes em uso ----------------------------------------------------------

library("pliman")
library("tidyverse")


# Encontrando os melhores IVs para segmentação ----------------------------

# Importação da imagem para testes de segmentação. Deve-se escolher uma
# imagem na pasta "ImagesProj e substituir o nome "figure_name.jpg"
# na função image_import()
img <- image_import("figure_name.jpg", path = "ImagesProj")

# Segmentação com todos os IVs e falso para inversão (invert = F).
# Buscar IVs que segmentam tanto os objetos em estudo quanto a referência.
# O plano de fundo ficará branco. Os objetos em estudo e a referência
# ficarão em destaque.
image_segment(img, index = 'all', show_image = T, invert = F)


# Segmentação com todos os IVs e verdadeiro para inversão (invert = T).
# Buscar IVs que segmentam tanto os objetos em estudo quanto a referência.
# O plano de fundo ficará branco. Os objetos em estudo e a referência
# ficarão em destaque.
image_segment(img, index = 'all', show_image = T, invert = T)


# IVs para segmentação dos objetos em estudo e da referência
# quando a inversão é falsa (invert = F).
IVs_obj_ref_inv_false <- c('SCI', 'HUE', 'R-G', 'RGRI','MyIndex')


# Mostra a segmentação dos objetos em estudo (folíolos) e da referência
# de forma ampla quando a inversão é falsa (invert = F).
map(IVs_obj_ref_inv_false,
    ~ image_segment(
      img,
      index = .x,
      show_image = T,
      filter = 4,
      invert = F
    ))

# IVs para segmentação dos objetos em estudo e da referência
# quando a inversão é verdadeira (invert = T).
IVs_obj_ref_inv_true <- c('NG', 'GLI', 'NGRDI', 'VARI', 'GLAI', 'G-R',
                          'G/(G+R)', 'MNGRDI', 'ExG', 'VEG')


# Mostra a segmentação dos objetos em estudo (folíolos) e da referência
# de forma ampla quando a inversão é falsa (invert = F).
map(IVs_obj_ref_inv_true,
    ~ image_segment(
      img,
      index = .x,
      show_image = T,
      filter = 4,
      invert = T
    ))


# Encontrando a melhor segmentação apenas na referência -------------------

# Necessário segmentar primeiro os objetos em estudo e a referência
# Utilizar um dos índices em que isso foi possível (ex. SCI)
# Definir se a inversão foi verdadeira ou falsa com o índice escolhido.
seg_obj_ref <-
  image_segment(
    img,
    index = "SCI",
    show_image = T,
    filter = 4,
    invert = F
  )

# Verifica quais dos IVs pode segmentar apenas a referência na
# função image_binary() para a inversão igual a falsa (invert = F).
# A referência deve ficar preta e restante da imagem branca.
seg_only_ref_inv_false <- map(
  pliman_indexes(),
  \(x)
  try(image_binary(
    seg_obj_ref,
    index = x,
    show_image = T,
    filter = 4,
    parallel = T,
    invert = F
  ))
)

names(seg_only_ref_inv_false) <- pliman_indexes()

# Verifica quais dos IVs pode segmentar apenas a referência na
# função image_binary() para a inversão igual a verdadeira (invert = T).
# A referência deve ficar preta e restante da imagem branca.
seg_only_ref_inv_true <- map(
  pliman_indexes(),
  \(x)
  try(image_binary(
    seg_obj_ref,
    index = x,
    show_image = T,
    filter = 4,
    parallel = T,
    invert = T
  ))
)

names(seg_only_ref_inv_true) <- pliman_indexes()

# Número de pixels na referência ------------------------------------------

# Segmentação da referência com o índice R-B para invert = F
length(which(seg_only_ref_inv_false$`R-B`$`R-B` != 1))

# Segmentação da referência com o índice B-R para invert = T
length(which(seg_only_ref_inv_true$`B-R`$`B-R` != 1))

# Segmentação da referência com o índice HUE2 para invert = T
length(which(seg_only_ref_inv_true$HUE2$HUE2 != 1))



