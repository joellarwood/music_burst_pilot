
##################################################################
##                    Analyse Data for Paper                    ##
##################################################################


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(lmerTest)
library(lme4)
library(emmeans)
library(tidymodels)
library(themis)
library(exboost)
library(kknn)
source("analysis/load_pilot_data.R")
source("analysis/chi_sq_helpers.R")


# Load data ---------------------------------------------------------------

pilot_data <- load_pilot_data("analysis/pilot_data_raw.txt")

visdat::vis_dat(pilot_data) + 
  ggplot2::ggtitle("Data Strucutre")

glimpse(pilot_data)

# Create lookup table of probabilities ------------------------------------

prob_table <- pilot_data %>% 
  group_by(target_song) %>% 
  summarise(
    instances = n()
  ) %>% 
  mutate(
    unweighted_prob = 1/n(),
    weighted_prob = instances/sum(instances)
  ) %>% 
  select(-instances)


# Category Level Summary --------------------------------------------------

cat_sum <- pilot_data %>% 
  group_by(target_song) %>% 
  summarise(
    correct = sum(correct),
    instances = n()
  ) %>% 
  left_join(prob_table) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    unweighted_z = z_prop_test(x = correct, n = instances, p = unweighted_prob),
    unweighted_sig = is_sig(x = correct, n = instances, p = unweighted_prob),
    weighted_z = z_prop_test(x = correct, n = instances, p = weighted_prob),
    weighted_sig = is_sig(x = correct, n = instances, p = weighted_prob)
  )


# Compare Accuracy Across Categories --------------------------------------
pairwise_emotion <- cat_sum %>% 
  transmute(
    target_song = target_song,
    correct = correct,
    incorrect = instances - correct
  ) %>% 
  tibble::column_to_rownames("target_song") %>% 
  as.matrix() %>% 
  pairwise.prop.test(p.adjust.method = "none") 


pairwise_emotion <- pairwise_emotion$p.value %>% 
  data.frame() %>% 
  tibble::rownames_to_column() %>% 
  mutate(
    across(
      where(is.numeric),
      ~ if_else(.x < .001,
                "< .001", 
                as.character(
                  round(.x, 3)
                )
      )
    )
  )


write_delim(
  pairwise_emotion,
  "output/pairwise_emotion.txt",
  delim = "~"
)


# Compare counts within categories ----------------------------------------

counts <- table(pilot_data$target_song, 
                forcats::fct_relevel(
                  pilot_data$emotion,
                  "none of these", 
                  after = Inf
                )
) %>% 
  as.data.frame() %>% 
  filter(Var2 != "") %>% 
  group_by(Var1) %>% 
  arrange(Var1) %>% 
  group_split(.keep = TRUE)

names(counts) <- unique(pilot_data$target_song)

comparisons <- lapply(
  counts, 
  function(x){
    round(as.data.frame(RVAideMemoire::chisq.multcomp(as.vector(x[["Freq"]]))$p.value),2)
  }
)


# Confusion Matrix ---------------------------------------------------------

conf_mat <- table(pilot_data$target_song, 
                  forcats::fct_relevel(
                    pilot_data$emotion,
                    "none of these", 
                    after = Inf
                    )
                  ) %>% 
  as.data.frame() %>% 
  rename(
    target = Var1,
    response = Var2
  ) %>% 
  group_by(target) %>% 
  arrange(target) %>% 
  mutate(
    n = sum(Freq),
    prop = Freq/n,
    se = sqrt(prop * (1 - prop) / 30)
  ) %>% 
  select(
    target,
    response, 
    prop, 
    se
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ round(.x * 100, 2)
    ),
    result = as.factor(paste0(prop, "% (", se, ")"))
  ) %>% 
  filter(
    response != ""
  ) %>%
  select(
    -prop, - se
  ) %>% 
  # pivot_longer(
  #   result,
  #   names_to = "measure",
  #   values_to = "result"
  # ) %>%
  pivot_wider(
    names_from = response,
    values_from = result
  )
 

write_delim(
  conf_mat, 
  path="output/conf_mat.txt",
  delim = "~"
  )


# Song level summary ------------------------------------------------------


song_sum <- pilot_data %>% 
  group_by(stimulus_song, target_song) %>% 
  summarise(
    correct = sum(correct),
    instances = n()
  ) %>% 
  left_join(prob_table) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    unweighted_z = z_prop_test(x = correct, n = instances, p = unweighted_prob),
    unweighted_sig = is_sig(x = correct, n = instances, p = unweighted_prob),
    weighted_z = z_prop_test(x = correct, n = instances, p = weighted_prob),
    weighted_sig = is_sig(x = correct, n = instances, p = weighted_prob),
    song = gsub("audio/", "", stimulus_song)
  ) 

 


# Song level significance counts ---------------------------------------------------------

song_sig <- song_sum %>% 
  mutate(
    acc = correct/instances
  ) %>% 
  group_by(target_song) %>% 
  summarise(
    songs = n(),
    sig_weighted = sum(weighted_sig),
    sig_unweighted = sum(unweighted_sig),
    min = min(acc),
    mean = mean(acc),
    max = max(acc)
  ) %>% 
  mutate(
    weighted_song_prop = paste(sig_weighted, songs, sep = "/"),
    unweighted_song_prop = paste(sig_unweighted, songs, sep = "/")
  )


# Compare percentage of songs performing above chance --------

compare_counts <- song_sig %>% 
  transmute(
    target = target_song, 
    correct = sig_unweighted,
    incorrect = songs - sig_unweighted
  ) %>% 
  tibble::column_to_rownames("target") %>% 
  as.matrix() %>% 
  pairwise.prop.test() 

compare_counts <- compare_counts$p.value %>% 
  data.frame() %>% 
  tibble::rownames_to_column() %>% 
  mutate(
    across(
      where(is.numeric),
      round, 3
      )
  )


write_delim(
  compare_counts,
  "output/comparison_of_sig_songs.txt",
  delim = "~"
)


# Create discriptives table -----------------------------------------------

describe_emotion <- cat_sum %>% 
  full_join(
    song_sig
  ) #%>% 
  transmute(
    `Target Emotion` = target_song,
    `Proportion Correct` = round(correct/instances, 2),
    `Mean Accuracy within category (range)` = paste0(round(mean, 2), " (", round(min, 2), ",", round(max, 2), ")"),
    `One proportion Z test` = unweighted_z,
    `Proportion of excerpts supported` = unweighted_song_prop,
  )



describe_emotion_va <- pilot_data %>% 
  group_by(target_song) %>% 
  summarise(    
    mean_v = mean(valence),
    sd_v = sd(valence),
    mean_a = mean(arousal),
    sd_a = sd(arousal)
  ) %>% 
  mutate(
    across(
      where(
        is.numeric
      ),
      ~ round(.x), 3
    )
  ) %>% 
  transmute(
    `Target Emotion` = target_song,
    `Valence Mean (SD)` = glue::glue("{mean_v} ({sd_v})"),
    `Arousal Mean (SD)` = glue::glue("{mean_a} ({sd_a})")
  )

describe_emotion_join <- left_join(describe_emotion, describe_emotion_va)
                                   
describe_emotion_t <-as_tibble(cbind(nms = names(describe_emotion_join), t(describe_emotion_join)))

write_delim(describe_emotion_t,
          "output/emotion_summary.txt",
          delim = "~")



# Model Valence and Arousal Ratings ---------------------------------------

##Valence
valence_lm <- lmerTest::lmer(valence ~ target_song + (1 + target_song | pid) + (1 | stimulus_song),
                             data = pilot_data,
                             control=lmerControl(optimizer="bobyqa"),
                             contrasts = list(target_song = "contr.sum"))

valence_emmeans <- emmeans(
  valence_lm,
  pairwise ~ target_song,
  infer = TRUE
)

valence_contrasts <- valence_emmeans$contrasts %>% 
  data.frame() %>% 
  transmute(
    contrast = contrast,
    valence = glue::glue("est = {round(estimate, 2)}, t({round(df, 2)}) = {round(t.ratio, 2)}, p = {if_else(p.value < .001, '<.001', as.character(round(p.value, 3)))}, 95% CI [{round(lower.CL, 2)}, {round(upper.CL, 2)}]" )
  )

##Arousal 
arousal_lm <- lmerTest::lmer(arousal ~ target_song + (1 + target_song | pid) + (1 | stimulus_song),
                             data = pilot_data,
                             control=lmerControl(optimizer="bobyqa"),
                             contrasts = list(target_song = "contr.sum"))

arousal_emmeans <- emmeans(
  arousal_lm,
  pairwise ~ target_song,
  infer = TRUE
)

arousal_contrasts <- arousal_emmeans$contrasts %>% 
  data.frame() %>% 
  transmute(
    contrast = contrast,
    arousal = glue::glue("est = {round(estimate, 2)}, t({round(df, 2)}) = {round(t.ratio, 2)}, p = {if_else(p.value < .001, '<.001', as.character(round(p.value, 3)))}, 95% CI [{round(lower.CL, 2)}, {round(upper.CL, 2)}]" )
  )


contrasts <- left_join(valence_contrasts, arousal_contrasts)

write_delim(contrasts,
            "output/contrasts.txt",
            delim = "~")


# Summarise Valence and Arousal -------------------------------------------

summarise_affect <- pilot_data %>% 
  group_by(target_song) %>% 
  summarise(
    across(
      c("valence", "arousal"),
      list(mean = mean,
           sd = sd))
  )


# Plot Valence and Arousal ------------------------------------------------


plot_affect <- pilot_data %>% 
  group_by(stimulus_song, target_song) %>% 
  summarise(
    valence_sd = sd(valence),
    arousal_sd = sd(arousal),
    valence = mean(valence),
    arousal = mean(arousal),
  ) %>% 
  mutate(
    across(
      contains("sd"), 
      list(se = ~ (.x)/sqrt(30))
    ),
    `Target Emotion` = stringr::str_to_sentence(target_song)
  ) %>% 
  ggplot2::ggplot(
    aes(x = valence,
        y = arousal,
        fill = `Target Emotion`,
        color = `Target Emotion`)
  ) + 
  geom_point() +
  geom_errorbar(
    aes(
      ymin = arousal - arousal_sd_se,
      ymax = arousal + arousal_sd_se),
    alpha = .5
  ) +
  geom_errorbarh(
    aes(
      xmin = valence - valence_sd_se,
      xmax = valence + valence_sd_se
    ),
    alpha = .5
  ) +
  coord_cartesian(xlim = c(-2,2),
                  ylim = c(-2, 2)) + 
  geom_hline(
    aes(
      yintercept = 0), 
    linetype = "dashed",
    alpha = .3
  )+ 
  geom_vline(
    aes(xintercept = 0), 
    linetype = "dashed",
    alpha = .3
  )+
  ylab("Rated Arousal") + 
  xlab("Rated Valence")+
  theme_classic() + 
  theme(text =  element_text(
    family = "Times New Roman",
    size = 12),
    legend.position = c(.9,.8)
  )

plot_affect

ggsave("output/affect_plot.png")



# Machine Leanring Predictions --------------------------------------------
set.seed(01062021)
## Use KNN to model classify the 4 emotions
## Cross validate due to small number of observations
## Subsample for class imbalance 

# Create test/trail split
train_test_split <- rsample::initial_split(pilot_data, prop = 3/4, strata = target_song)

# Get training sample

train <- rsample::training(train_test_split)

# Get folds using V folds
folds <- rsample::vfold_cv(train, folds = 10, strata = target_song)

# Get test sample
test <- rsample::testing(train_test_split)

## Define the recipe 
### This is the same for all engines
rec <- recipes::recipe(
  target_song ~ valence + arousal + stimulus_song,
  data = train
) %>% 
  recipes::update_role(stimulus_song, new_role = "ID") %>% 
  recipes::step_center(all_predictors()) %>% 
  recipes::step_normalize(all_predictors()) %>% 
  themis::step_smote(target_song)

### Define explicit interaction term
rec_int <- recipes::recipe(
  target_song ~ valence + arousal + stimulus_song,
  data = train
) %>% 
  recipes::update_role(stimulus_song, new_role = "ID") %>% 
  recipes::step_interact(~ valence*arousal) %>% 
  recipes::step_center(all_predictors()) %>% 
  recipes::step_normalize(all_predictors()) %>% 
  themis::step_smote(target_song) 
  


## Define models
###xgBoost
boost <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

###random forest 
forest <- rand_forest() %>% 
  set_engine("ranger", importance = 'impurity') %>% 
  set_mode("classification")

### Neural Network 
neural_network <- mlp() %>% 
  set_engine("keras") %>% 
  set_mode("classification")

## Define workflow 

workflow_all <- workflow_set(
  preproc = list(no_int = rec,
                 int = rec_int),
  models = list(
    xgboost = boost,
    random_forest = forest, 
    knn = knn
  )
)

## Fit each model 
models <- workflow_all %>% 
  workflow_map(
    "fit_resamples",
    seed = 999,
    verbose = T,
    resamples = folds,
    control = control_resamples(save_pred = T))

beepr::beep()

## Get metrics
metrics <- models %>% 
  collect_metrics() %>% 
  arrange(desc(.metric), mean)

## Fit random forest model 
forest_fit <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(forest) %>% 
  last_fit(train_test_split)
  

## Get RF metrics
rf_metrics <- collect_metrics(
  forest_fit
)


rf_metrics

## Get confusion matrix 

rf_conf_mat <- forest_fit %>% 
  collect_predictions() %>% 
  conf_mat(target_song, .pred_class)

### Express confusion matrix as percentages 

rf_conf_mat_pct <- rf_conf_mat$table %>% 
  data.frame() %>% 
  group_by(Truth) %>% 
  mutate(
    pct = paste0(round(Freq/sum(Freq) * 100, 2), "%")
  ) %>% 
  select(-Freq) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = Prediction,
    values_from = pct
  )

## Write conf matrix 
write_delim(rf_conf_mat_pct,
            "output/random_forest_matrix.txt",
            delim = "~")


### Get feature importance
importance <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(forest) %>% 
  fit(test) %>% 
  pull_workflow_fit() %>% 
  .$fit


importance$variable.importance

# Appendix A --------------------------------------------------------------

appendix_a <- pilot_data %>% 
  group_by(stimulus_song, target_song) %>% 
  summarise(
    correct = sum(correct),
    instances = n()
  ) %>%
  left_join(centroid,
            by = c("stimulus_song" = "Song")) %>% 
  ungroup() %>% 
  transmute(
    Target = stringr::str_to_title(Target),
    `Accuracy` = paste0(correct, "/", instances),
    Song = stringr::str_remove(str_remove(stimulus_song, "audio/"), ".mp3"),
    `Valence Mean (SD range)` =  glue::glue("{round(Valence_mean, 2)} ({Valence_lower}, {Valence_upper})"),
    `Arousal Mean (SD range)` =  glue::glue("{round(arousal_mean, 2)} ({arousal_lower}, {arousal_upper})")
  ) %>% 
  arrange(Target)
    
  
write_delim(appendix_a,
            "output/appendix_a.txt",
            delim = "~")
