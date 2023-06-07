# Instala o pacote googledrive necessário para acessar as imagens hospedadas
# em um diretório do GoogleDrive. Deve ser executado apenas uma vez.
# install.packages("googledrive")

# Instala a coleção de pacotes tidyverse que é útil para ciência de dados.
# install.packages("tidyverse")

# Carrega o pacote googledrive com suas funções.
library(googledrive)

# Carrega o pacote tidyverse com suas funções.
library(tidyverse)

# Função necessária para autorizar o acesso ao GoogleDrive. Utilize um e-mail
# de acesso ao Google. A autorização é necessária apenas no primeiro acesso.
drive_auth(email = "imagens.plantas@gmail.com")

# Após o primeiro acesso, caso seja necessário acessar repetidas vezes o
# GoogleDrive, execute apenas a função de autorização sem especificar o e-mail
# de acesso. A função automaticamente requererá ao usuário a escolha da opção
# de acesso anterior.
drive_auth()

# Cria uma pasta de nome "ImagesProj" dentro do diretório do projeto para
# hospedar as imagens baixadas do GoogleDrive.
dir.create("ImagesProj")

# Lista e armazena os nomes das imagens dentro da pasta "ImagesProj"
HD_files <- list.files("ImagesProj", pattern = ".jpg")

# Lista e armazena os nomes das imagens dentro da pasta "ImagesGoogle".
# OBS.: "ImagesGoogle" é uma sugestão de nome de pasta para armazenar as
# imagens dentro do GoogleDrive.
Google_files <- drive_ls("ImagesGoogle")
GD_files <- Google_files[[1]]

# Obtém a diferença entre os arquivos de imagens armazenados na pasta
# "ImagesGoogle" do GoogleDrive e na pasta "Images_Proj" do projeto.
dif <- setdiff(GD_files, HD_files)

dif_in_Google_files <- Google_files |>
  dplyr::filter(name %in% dif)

# Define a pasta dentro do projeto onde as imagens serão baixadas
setwd("ImagesProj")

# Baixa os arquivos de imagens do GoogleDrive.
purrr::walk(dif_in_Google_files$id,
            ~ drive_download(as_id(.x),
                             overwrite = TRUE))

# Redefine o diretório para a pasta do projeto
dir_path <- rstudioapi::getActiveProject()
setwd(dir_path)

# Confere se todos os arquivos de imagens presentes no GoogleDrive foram
# baixados para pasta "ImagesProj" definido no diretório do projeto.
# Caso verdadeiro, a execução da função setequal retornará TRUE.
HD_files_2 <- list.files("ImagesProj", pattern = ".jpg")
setequal(GD_files, HD_files_2)


