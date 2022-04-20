
#################################################################
##                 Clean JSON data and Analyse                 ##
##                    Analyst: Joel Larwood                    ##
#################################################################


# Load Pacakges -----------------------------------------------------------

library(tidyverse)
library(janitor)


# Load data ---------------------------------------------------------------

load_pilot_data <- function(file){
  pilot_read <- jsonlite::stream_in(
    file(
      file
    )
  )
  

# Clean data for analysis -------------------------------------------------

# data is in wide form. I need to make long with the variables I am interested in 

# loop where the rows are taken and then rbind to initiallt created dataframe 

pilot_jatos <- pilot_read %>% 
  select(
    contains("test_part"),
    contains("node"),
    contains("stimulus"),
    contains("target"),
    contains("key_press"),
    contains("responses")
  ) %>% 
  rename_with(.cols = !contains("."),
              ~paste0(.x, ".0") # add .0 to the rows without a postscript
  ) 

pilot_loop <- data.frame()


for (i in 0:29){
  pid <- i 
  tmp <- select(
    pilot_jatos,
    paste(c(
      "test_part",
      "internal_node_id",
      "stimulus",
      "target",
      "key_press",
      "responses"
    ),
    pid,
    sep = ".")
  )
  tmp <- rename_with(
    tmp,
    ~gsub(".[[:digit:]]{1,2}",
          "",
          .x)
  )
  tmp["pid"] <- pid
  pilot_loop <- bind_rows(pilot_loop, tmp)
}

pilot_long <- pilot_loop %>% 
  select(test_part,
         internal_node_id,
         stimulus,
         target,
         key_press,
         responses,
         pid) %>% 
  dplyr::filter(
    test_part %in% c("song", 
                     "emotion_rating",
                     "valence rating", 
                     "arousal rating")
  ) %>% 
  tidyr::separate(
    col = internal_node_id, 
    into = c("node_1", "node_2", "node_3"),
    sep = "-",
  ) %>% 
  tidyr::separate(
    col = node_3, 
    into = c("node_3_lead", "trial_index"), 
    remove = FALSE
  ) %>% 
  dplyr::select(
    -node_1, 
    -node_2, 
    -node_3, 
    -node_3_lead
  )

processed <- pilot_long %>% 
  pivot_wider(names_from = test_part, 
              values_from = c(
                stimulus, 
                target, 
                responses,
                key_press
              )
  ) %>% 
  select(
    pid,
    trial_index,
    stimulus_song,
    target_song,
    responses_emotion_rating,
    `key_press_arousal rating`,
    `key_press_valence rating`
  ) %>% 
  rename(
    emotion = responses_emotion_rating,
    valence = `key_press_valence rating`,
    arousal = `key_press_arousal rating`
  ) %>% 
  tidyr::separate(
    emotion, 
    into = c("QO", "emotion"), 
    sep = '\":"'
  ) %>% 
  dplyr::select(
    -QO
  ) %>% 
  dplyr::mutate(
    emotion = tolower(as.factor(gsub('(["]})', "", emotion)))
  ) %>% 
  dplyr::mutate(
    correct = if_else(
      emotion == target_song, 
      1, 
      0
    )
  ) %>% 
  mutate(
    valence = as.numeric(
      as.character(
        recode(
          valence, 
          "49" = "-3",
          "50" = "-2",
          "51" = "-1",
          "52" = "0",
          "53" = "1",
          "54" = "2",
          "55" = "3")
      )
  ),
  arousal = as.numeric(
    as.character(
      recode(
        arousal, 
        "49" = "-3",
        "50" = "-2",
        "51" = "-1",
        "52" = "0",
        "53" = "1",
        "54" = "2",
        "55" = "3")
    )
  )
  ) %>% 
  mutate(
    across(
      where(is.character),
      as.factor
    ),
    pid = as.factor(pid)
  )

return(processed)
}


