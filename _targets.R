library(targets)

# set options
tar_option_set(packages = c("readr", "dplyr", "sentimentr", "here"))

# functions to be used

# load starwars data
load_data = function(file = here::here('materials', 'data', 'starwars_text.csv')) {
  
  read_csv(file,
           show_col_types = F)
  
}

# prepare data
clean_data = function(data) {
  
  data |>
    mutate(episode = case_when(document == 'a new hope' ~ 'iv',
                               document == 'the empire strikes back' ~ 'v',
                               document == 'return of the jedi' ~ 'vi')) |>
    mutate(character = case_when(character == 'BERU' ~ 'AUNT BERU',
                                 character == 'LURE' ~ 'LUKE',
                                 TRUE ~ character)) |>
    select(episode, everything())
}

# calculate sentiment
calculate_sentiment = function(data,
                               by = c("document", "character", "line_number")) {
  
  data |>
    sentiment_by(by = by) |>
    sentimentr::uncombine()
}
# define targets
list(
  tar_target(
    name = starwars,
    command = 
      load_data() |>
      clean_data()
  ),
  tar_target(
    name = sentences,
    command = 
      starwars |>
      get_sentences()
  ),
  tar_target(
    name = sentiment,
    command = 
      sentences |>
      calculate_sentiment()
  )
)
