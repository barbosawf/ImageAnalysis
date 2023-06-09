
# Packages ----------------------------------------------------------------

library('pliman')
library('tidyverse')


# Files to import and analyze ---------------------------------------------

project_path <- dirname(rstudioapi::documentPath())


soybean_files <- readRDS('soybean_files.rds')
length(soybean_files)

if (Sys.time() > "2023-01-30 19:31:32 -03") {
  d <- setdiff(list.files('Soybean_Imgs'), soybean_files)
  soybean_files <- c(soybean_files, d)
}

length(soybean_files)
saveRDS(soybean_files, file = 'soybean_files.rds')


split_file_list <-
  split(soybean_files,
        ((seq_along(soybean_files) - 1) %/% 150) + 1)



# Adding image files ------------------------------------------------------

for (i in seq_len(length(split_file_list))) {

  # Condition to control the file creation
  if (i > 0) {

    if (i < 10) {
      object_name <- paste0('img_0', i)
    } else {
      object_name <- paste0('img_', i)
    }

    dir_ <- "Soybean_Imgs/tmp"
    dir.create(dir_)

    p = paste0(project_path, '/Soybean_Imgs/', split_file_list[[i]])
    file.copy(p, dir_)

    image_import(pattern = '.jpg',
                 path = dir_,
                 plot = F) -> imgs

    saveRDS(get('imgs'), file = paste0('Imgs/', object_name, '.rds'))
    rm('imgs')

    unlink(dir_, recursive = TRUE) # remove dir
    gc()

  }
}


# Obtaining features ------------------------------------------------------

for (i in seq_len(length(split_file_list))) { # Change list when necessary

  # Condition to control the file creation
  if (i > 0) {

    if (i < 10) {
      object_name <- paste0('features_0', i)
    } else {
      object_name <- paste0('features_', i)
    }

    dir_ <- "Soybean_Imgs/tmp"
    dir.create(dir_)

    # change list when necessary
    p = paste0(project_path, '/Soybean_Imgs/', split_file_list[[i]])
    file.copy(p, dir_)

    analyze_objects(
      pattern = '',
      dir_original = dir_,
      dir_processed = 'Processed_Soybean_Imgs',
      reference = TRUE,
      reference_area = 18,
      back_fore_index = "SCI",
      fore_ref_index = "HUE2",
      # watersehed = FALSE is better (change for the remaining imgs)
      watershed = FALSE,
      marker = "id",
      filter = 4,
      fill_hull = TRUE,
      parallel = TRUE,
      show_image = FALSE,
      save_image = TRUE,
      invert = c(FALSE, TRUE),
      col_background = 'white',
      # col_foreground = 'blue',
      contour_size = 2,
      # object_index = c('R', 'G', 'B', 'DGCI')
    ) -> features

    # Change name when necessary
    saveRDS(get('features'), file = paste0('Features/', object_name, '.rds'))
    rm('features')

    unlink(dir_, recursive = TRUE) # remove dir
    gc()

  }
}



# Figures that failed -----------------------------------------------------

diff_from_9_list <- list()

paste0('Features/features_0', 1:8, '.rds') |>
  map(readRDS) |>
  map(2) |>
  bind_rows() |>
  filter(Objects != '9') -> diff_from_9

diff_from_9$Image  |> length()

diff_from_9$Image |> as.numeric() |> sort() |> paste0('.jpg') ->
  diff_from_9_list[['1']]
# substitute in previous section and change
#


# Figures with nine objects (filtering) -----------------------------------

paste0('Features/features_0', 1:8, '.rds') |>
  map(readRDS) |>
  map(1) |>
  bind_rows() |>
  pivot_wider(values_from = value,
              names_from = stat) |>
  filter(n == '9') |>
  filter(min_area < 5)


# Figure features with nine objects ---------------------------------------

paste0('Features/features_0', 1:8, '.rds') |>
  map(readRDS) |>
  map(3) |>
  bind_rows() |>
  filter(!img %in% c(diff_from_9$Image)) |> # filter for imgs with 9 leaflets
  as_tibble() -> features_all_imgs


# Remaining figures with nine objects -------------------------------------

'Features/features_00.rds' |>
  readRDS() -> features_00

features_00$count |>
  filter(Objects != 9) -> new_diff_from_9

features_00$results |>
  filter(!img %in% new_diff_from_9$Image) |>
  as_tibble() -> features_remaning_imgs

features_remaning_imgs |>
  count(img) |> as.data.frame()


# New figures with nine objects -------------------------------------------

add_parents <- readRDS('add_parents.rds')

rbind(features_all_imgs, features_remaning_imgs) |>
  add_parents() ->
  features_all_imgs_new

saveRDS(features_all_imgs_new, file = 'features_all_imgs_new.rds')

readRDS('features_all_imgs_new.rds') |>
  count(Linhagem) |>
  pull(Linhagem) |> length()

list.files('Soybean_Imgs') |> length()


# Adding categories in variables ------------------------------------------

plus_or_minus_sd <- readRDS(file = 'plus_or_minus_sd.rds')


# Feature values categorized using 1 sd
readRDS('features_all_imgs_new.rds') |>
  mutate(id = as.character(id)) |>
  mutate_at(vars(8:last_col()), list(cat = plus_or_minus_sd)) ->
  feature_values_cat_1sd

saveRDS(feature_values_cat_1sd, file = 'feature_values_cat_1sd.rds')


# Feature means categorized using 1 sd
readRDS('features_all_imgs_new.rds') |>
  group_by(Linhagem, `Genitor Paterno`, `Genitor Materno`, Genitores) |>
  select(-c('id', 'x', 'y')) |>
  summarise_if(is.numeric, mean) |>
  ungroup() |>
  mutate_if(is.numeric, list(cat = plus_or_minus_sd)) ->
  feature_means_cat_1sd

saveRDS(feature_means_cat_1sd, file = 'feature_means_cat_1sd.rds')


# Checking for anomalies in leaflet identity ------------------------------

feature_values_cat_1sd |>
  filter(!id %in% c(1:9)) |>
  select(Linhagem) |> pull() |> unique() -> id_anomalies

feature_values_cat_1sd |>
  filter(Linhagem %in% id_anomalies) |>
  view()
