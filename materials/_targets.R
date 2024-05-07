library(targets)
tar_option_set(packages = c("readr", "dplyr", "sentimentr", "here"))
load_data = function(file = here::here("materials", "data", "starwars_text.csv")) {
    read_csv(file)
}
clean_data = function(data) {
    select(mutate(mutate(data, episode = case_when(document == 
        "a new hope" ~ "iv", document == "the empire strikes back" ~ 
        "v", document == "return of the jedi" ~ "vi")), character = case_when(character == 
        "BERU" ~ "AUNT BERU", character == "LURE" ~ "LUKE", TRUE ~ 
        character)), episode, everything())
}
calculate_sentiment = function(data, by = c("document", "character", 
    "line_number")) {
    sentimentr::uncombine(sentiment_by(data, by = by))
}
list(tar_target(name = starwars_raw, command = load_data()), 
    tar_target(name = starwars, command = clean_data(starwars_raw)), 
    tar_target(name = sentences, command = get_sentences(starwars)), 
    tar_target(name = sentiment, command = calculate_sentiment(sentences)))
tar_make()
