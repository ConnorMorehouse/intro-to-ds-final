#####
# packages 
#####
library(tidyverse)
library(baseballr)
library(caret)
library(fastDummies)

#####
# functions
#####

scrape_statcast <- function(season) {
  
  # create weeks of dates for season from mar - nov
  # includes spring training + postseason
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  # create 'safe' version of scrape_statcast_savant in case week doesn't process
  safe_savant <- safely(scrape_statcast_savant)
  
  # loop over each row of date_grid, and collect each week in a df
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  # eliminate results with an empty dataframe
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
}

data_format <- function(data) {
  
  df <- data %>%
    mutate(pitch_type = ifelse(pitch_type == "", "UN", pitch_type),
           # expand game_type codes for clarity
           game_type = case_when(
             game_type == "E" ~ "Exhibition",
             game_type == "S" ~ "Spring Training",
             game_type == "R" ~ "Regular Season",
             game_type == "F" ~ "Wild Card",
             game_type == "D" ~ "Divisional Series",
             game_type == "L" ~ "League Championship Series",
             game_type == "W" ~ "World Series"),
           # create binary handedness indicators
           is_lhb = ifelse(stand == "L", "1", "0"),
           is_lhp = ifelse(p_throws == "L", "1", "0"),
           # create fielderid to accompany hit_location, giving the
           fielderid = case_when(
             hit_location == "1" ~ as.character(pitcher),
             hit_location == "2" ~ as.character(fielder_2),
             hit_location == "3" ~ as.character(fielder_3),
             hit_location == "4" ~ as.character(fielder_4),
             hit_location == "5" ~ as.character(fielder_5),
             hit_location == "6" ~ as.character(fielder_6),
             hit_location == "7" ~ as.character(fielder_7),
             hit_location == "8" ~ as.character(fielder_8),
             hit_location == "9" ~ as.character(fielder_9)),
           fielderid = as.numeric(fielderid),
           # binary inning half indicator
           is_bottom = ifelse(inning_topbot == "Bot", "1", "0"),
           # add spray angle 
           spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
           # standardize team abbreviations (some deprecated)
           home_team = case_when(
             home_team == "FLA" ~ "MIA",
             home_team == "KC" ~ "KCR",
             home_team == "SD" ~ "SDP",
             home_team == "SF" ~ "SFG",
             home_team == "TB" ~ "TBR",
             TRUE ~ home_team),
           away_team = case_when(
             away_team == "FLA" ~ "MIA",
             away_team == "KC" ~ "KCR",
             away_team == "SD" ~ "SDP",
             away_team == "SF" ~ "SFG",
             away_team == "TB" ~ "TBR",
             TRUE ~ away_team),
           # runner status
           run_on_1b = ifelse(on_1b == "0", NA, on_1b),
           run_on_2b = ifelse(on_2b == "0", NA, on_2b),
           run_on_3b = ifelse(on_3b == "0", NA, on_3b),
           # pitch information
           is_bip = ifelse(type == "X", 1, 0),
           is_stk = ifelse(type == "S", 1, 0),
           # baseout state before PA event
           basecode_before = case_when(
             is.na(run_on_1b) & is.na(run_on_2b) & is.na(run_on_3b) ~ "000",
             is.na(run_on_1b) & is.na(run_on_2b) ~ "001",
             is.na(run_on_1b) & is.na(run_on_3b) ~ "010",
             is.na(run_on_2b) & is.na(run_on_3b) ~ "100",
             is.na(run_on_3b) ~ "110",
             is.na(run_on_2b) ~ "101",
             is.na(run_on_1b) ~ "011",
             TRUE ~ "111")) %>%
    rename(vis_team = away_team,
           batterid = batter,
           pitcherid = pitcher,
           event_type = events,
           event_description = des,
           pitch_description = description,
           outs_before = outs_when_up,
           hit_distance = hit_distance_sc,
           pa_number = at_bat_number,
           bat_score_before = bat_score,
           gameid = game_pk,
           field_score = fld_score,
           release_spin = release_spin_rate) %>%
    arrange(game_date, gameid, pa_number, pitch_number) 
  
  register <- read_csv("people.csv") %>%
    filter(!is.na(key_mlbam)) %>%
    mutate(name = paste(name_first, name_last))
  
  df <- df %>%
    # correct pitch numbering, ball, and strike counts
    group_by(gameid, pa_number, batterid) %>%
    mutate(pitch_number = row_number(),
           is_last_pitch = ifelse(row_number() == n(), 1, 0),
           balls = pmin(pitch_number - cumsum(lag(is_stk, default = 0)) - 1, 3),
           strikes = pmin(cumsum(lag(is_stk, default = 0)), 2)) %>%
    # post PA information
    group_by(gameid, inning, is_bottom) %>%
    mutate(outs_after = ifelse(row_number() == n() & 
                                 inning == 9 & is_bottom == 1 & 
                                 str_detect(pitch_description, "score"),
                               outs_before,
                               lead(outs_before, default = 3)),
           basecode_after = ifelse(row_number() == n() & 
                                     inning == 9 & is_bottom == 1 & 
                                     str_detect(pitch_description, "score"),
                                   basecode_before,
                                   lead(basecode_before, default = "000")),
           baseout_state_before = paste(basecode_before, outs_before),
           baseout_state_after = paste(basecode_after, outs_after)) %>%
    # correct PA number, get runs scored from PA
    group_by(gameid) %>%
    mutate(pa_number = cumsum(lag(is_last_pitch, default = 0)) + 1,
           bat_score_after = 
             ifelse(row_number() == n(),
                    ifelse(is_bottom == 1, 
                           ifelse(str_detect(pitch_description, "score"), 
                                  away_score + 1, home_score), away_score),
                    ifelse(is_bottom == 1, lead(home_score), 
                           lead(away_score)))) %>%
    ungroup() %>% 
    # add player names
    left_join(select(register, key_mlbam, name), 
              by = c("batterid" = "key_mlbam")) %>%
    rename(batter_name = name) %>%
    left_join(select(register, key_mlbam, name), 
              by = c("pitcherid" = "key_mlbam")) %>%
    rename(pitcher_name = name) %>%
    left_join(select(register, key_mlbam, name), 
              by = c("fielderid" = "key_mlbam")) %>%
    rename(fielder_name = name) %>%
    arrange(game_date, gameid, pa_number, pitch_number)
  
  re24 <- df %>%
    group_by(game_date, gameid, inning, is_bottom) %>%
    mutate(runs_roi = max(bat_score_after) - bat_score_before) %>%
    ungroup() %>%
    group_by(baseout_state_before) %>%
    summarise(mean_roi = mean(runs_roi)) %>%
    rename(state = baseout_state_before) %>%
    rbind(c("000 3", 0)) %>%
    mutate(mean_roi = as.numeric(mean_roi))
  
  re288 <- df %>%
    group_by(game_date, gameid, inning, is_bottom) %>%
    mutate(runs_roi = max(bat_score_after) - bat_score_before,
           full_state_before = paste0(baseout_state_before, " ", 
                                      balls, "-", strikes)) %>%
    ungroup() %>%
    group_by(full_state_before) %>%
    summarise(mean_roi = mean(runs_roi)) %>%
    rename(state = full_state_before) %>%
    rbind(c("000 3 0-0", 0)) %>%
    mutate(mean_roi = as.numeric(mean_roi))
  
  df <- df %>%
    mutate(full_state_before = paste0(baseout_state_before, " ", balls, "-", 
                                      strikes),
           full_state_after = case_when(
             is_last_pitch == 1 ~ paste0(baseout_state_after, " 0-0"),
             strikes == 2 & is_stk == 1 ~ paste0(baseout_state_before, " ", 
                                                 balls, "-", strikes),
             is_stk == 1 ~ paste0(baseout_state_before, " ", balls, "-", 
                                  strikes + 1),
             is_stk == 0 ~ paste0(baseout_state_before, " ", balls + 1, "-", 
                                  strikes),
             TRUE ~ "ERROR")) %>%
    left_join(re24, by = c("baseout_state_before" = "state"), keep = TRUE) %>%
    rename(re24_before = mean_roi) %>%
    left_join(re24, by = c("baseout_state_after" = "state"), keep = TRUE) %>%
    rename(re24_after = mean_roi) %>%
    mutate(re24_before = ifelse(is_last_pitch == 1, re24_before, NA),
           re24_after = ifelse(is_last_pitch == 1, re24_after, NA),
           cdrv_24 = ifelse(is_last_pitch == 1, 
                            bat_score_after - bat_score_before + 
                              re24_after - re24_before, NA)) %>%
    group_by(event_type) %>%
    mutate(cnrv_24 = mean(cdrv_24)) %>%
    left_join(re288, by = c("full_state_before" = "state"), keep = TRUE) %>%
    rename(re288_before = mean_roi) %>%
    left_join(re288, by = c("full_state_after" = "state"), keep = TRUE) %>%
    rename(re288_after = mean_roi) %>%
    mutate(cn_event = ifelse(is_last_pitch == 1, paste0(balls, "-", strikes, 
                                                        " ", event_type),
                             ifelse(is_stk == 1, paste0(balls, "-", strikes, 
                                                        " strike"),
                                    paste0(balls, "-", strikes, " ball"))),
           cdrv_288 = ifelse(is_last_pitch == 1, 
                             bat_score_after - bat_score_before + 
                               re288_after - re288_before, 
                             re288_after - re288_before)) %>%
    group_by(cn_event) %>%
    mutate(cnrv_288 = mean(cdrv_288)) %>%
    ungroup()
  
  df <- df %>%
    mutate(rel_plate_x = plate_x / ((17/2 + 1.456) / 12),
           rel_plate_z = (plate_z - (sz_bot - 1.456/12)) /
             (sz_top - sz_bot + 1.456*2/12) * 2 - 1,
           attack_region = case_when(
             abs(rel_plate_x) < 0.67 & abs(rel_plate_z) < 0.67 ~ "Heart",
             abs(rel_plate_x) < 1.33 & abs(rel_plate_z) < 1.33 ~ "Shadow",
             abs(rel_plate_x) < 2.00 & abs(rel_plate_z) < 2.00 ~ "Chase",
             TRUE ~ "Waste"),
           attack_region = ifelse(is.na(plate_x) | is.na(plate_z), NA, attack_region)) %>%
    select(gameid, game_year, game_date, game_type, home_team, vis_team, 
           pa_number, pitch_number, inning, is_bottom, batterid, batter_name, 
           is_lhb, pitcherid, pitcher_name, is_lhp, bat_score_before, 
           bat_score_after, field_score, baseout_state_before, 
           baseout_state_after, event_type, event_description, 
           pitch_description, balls, strikes, is_last_pitch, is_bip, is_stk, 
           pitch_type, attack_region, plate_x, plate_z, sz_top, sz_bot, 
           rel_plate_x, rel_plate_z, bb_type, estimated_ba_using_speedangle, 
           estimated_woba_using_speedangle, woba_value, woba_denom, 
           babip_value, iso_value, launch_speed, launch_angle,
           launch_speed_angle, hc_x, hc_y, hit_distance, spray_angle, cdrv_24, 
           cnrv_24, cdrv_288, cnrv_288, release_speed, effective_speed, 
           release_extension, pfx_x, pfx_z, vx0, vy0, vz0, ax, ay, az, 
           hit_location, run_on_1b, run_on_2b, run_on_3b, fielderid, 
           fielder_name, fielder_2, fielder_3, fielder_4, fielder_5, fielder_6, 
           fielder_7, fielder_8, fielder_9, if_fielding_alignment, 
           of_fielding_alignment)
  
  # balls in play only
  # adds fielding team column
  # drop na's in if_fielding_alignemnt and of_fielding_alignment
  # add abid (gameid + pa_number) 
  # add is_shift
  # add is_pulled
  # add is_gb
  # add is_pulled_gb
  
  df <- df %>% 
    filter(is_bip == 1) %>% 
    drop_na(if_fielding_alignment,of_fielding_alignment)
  
  df$field_team <- ifelse(df$is_bottom == 1, df$vis_team, df$home_team)
  
  df$abid <- paste(df$gameid,df$pa_number,sep = '_')
  
  df$is_shift <- ifelse(df$if_fielding_alignment == 'Infield shift', 1, 0)
  
  df$is_pulled <- ifelse(df$is_lhb == 0, 
                         ifelse(df$spray_angle < 0, 1, 0),
                         ifelse(df$spray_angle > 0, 1, 0))
  
  df$is_gb <- ifelse(df$bb_type == 'ground_ball', 1, 0)
  
  df$is_pgb <- ifelse(df$is_gb == 1 & df$is_pulled, 1, 0)
  
  return(df)
}

model_prep <- function(dat) {
  
  # creates the to-date features 
  dat <- dat %>% 
    arrange(batterid, game_date, pa_number) %>% 
    group_by(batterid) %>% 
    mutate(gb_rate_to_date = cummean(is_gb),
           pull_rate_to_date = cummean(is_pulled),
           pgb_rate_to_date = cummean(is_pgb)) %>% 
    drop_na(is_pgb)
  
  # convert to factor for the model
  dat$is_pgb <- as.factor(dat$is_pgb)
  
  # dummy variables 
  dat <- dummy_cols(dat, select_columns = 'pitch_type')
  
  # normalize 
  dat <- dat %>% 
    mutate_each_(list(~scale(.) %>% as.vector),
                 vars = c('release_speed','pfx_x','pfx_z'))
  
}

metrics <- function(mod, newdat, real) {
  
  pred <- predict(mod, newdata = newdat, na.action = na.pass)
  
  results <- data.frame(predicted = pred,
                        real = real)
  
  tab <- table(results)
  tab <- tab[nrow(tab):1,][,ncol(tab):1]
  
  perc <- round(precision(tab),2)
  recall <- round(recall(tab),2)
  f_score <- round(F_meas(tab),2)
  acc <- round((tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2]),2)
  # 
  # print(c('precision:', perc))
  # print(c('recall:', recall))
  # print(c('f1:', f_score))
  # print(c('accuracy:', acc))
  
  return(c(perc, recall, f_score, acc))
  
}

#####
# sample of one year of gathering and preparing data 
#####

pbp15 <- scrape_statcast(2015)
df15 <- data_format(pbp15)
rm(pbp15)
df15_prep <- model_prep(df15)
rm(df15)

#####
# model testing 
#####

# for models that need y ~ x formula
{
  models <- c('ctree','glmboost','knn')
  
  train_control <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)
  
  mod_results <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(mod_results) <- c('mod','precision', 'recall', 'f score', 'accuracy')
  
  
  
  for (x in 1:length(models)) {
    
    print(models[x])
    
    test_mod <- train(is_pgb ~ rel_plate_x + rel_plate_z + release_speed + gb_rate_to_date + pull_rate_to_date + pfx_x + pfx_z +
                        pitch_type_CH + pitch_type_CS + pitch_type_CU + pitch_type_FC + pitch_type_FF + pitch_type_FS +
                        pitch_type_KC + pitch_type_SI + pitch_type_SL,
                      data = testdf,
                      trControl = train_control,
                      method = models[x],
                      na.action = na.pass)
    
    mets <- c(models[x], metrics(test_mod, testdf, testdf$is_pgb))
    mod_results[nrow(mod_results)+1,] <- mets
    
  }
  beep()
  view(mod_results)
}

# for models that need x,y formula

{
  models <- c('LogitBoost')
  
  train_control <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)
  
  # mod_results <- data.frame(matrix(ncol = 5, nrow = 0))
  # colnames(mod_results) <- c('mod','precision', 'recall', 'f score', 'accuracy')
  
  trainx <- c('rel_plate_x', 'rel_plate_z', 'release_speed', 'gb_rate_to_date', 'pull_rate_to_date', 'pfx_x','pfx_z',
              'pitch_type_CH', 'pitch_type_CS', 'pitch_type_CU', 'pitch_type_FC', 'pitch_type_FF', 'pitch_type_FS',
              'pitch_type_KC', 'pitch_type_SI', 'pitch_type_SL')
  
  for (x in 1:length(models)) {
    
    print(models[x])
    
    test_mod <- train(x = testdf[,trainx],
                      y = testdf$is_pgb,
                      trControl = train_control,
                      method = models[x],
                      na.action = na.pass)
    
    mets <- c(models[x], metrics(test_mod, testdf, testdf$is_pgb))
    mod_results[nrow(mod_results)+1,] <- mets
    
  }
  beep()
  view(mod_results)
}

# for a neural net
{
  test_mod <- train(is_pgb ~ rel_plate_x + rel_plate_z + release_speed + gb_rate_to_date + pull_rate_to_date + pfx_x + pfx_z +
                      pitch_type_CH + pitch_type_CS + pitch_type_CU + pitch_type_FC + pitch_type_FF + pitch_type_FS +
                      pitch_type_KC + pitch_type_SI + pitch_type_SL,
                    data = testdf,
                    trControl = train_control,
                    method = 'nnet',
                    na.action = na.pass)
  
  mets <- c('nnet', metrics(test_mod, testdf, testdf$is_pgb))
  mod_results[nrow(mod_results)+1,] <- mets
  beep()
}

# xgbtree
{
  test_mod <- train(is_pgb ~ rel_plate_x + rel_plate_z + release_speed + gb_rate_to_date + pull_rate_to_date + pfx_x + pfx_z +
                      pitch_type_CH + pitch_type_CS + pitch_type_CU + pitch_type_FC + pitch_type_FF + pitch_type_FS +
                      pitch_type_KC + pitch_type_SI + pitch_type_SL,
                    data = testdf,
                    trControl = train_control,
                    method = 'xgbTree',
                    na.action = na.pass,
                    verbosity = 0)
  
  mets <- c('xgbTree', metrics(test_mod, testdf, testdf$is_pgb))
  mod_results[nrow(mod_results)+1,] <- mets
  beep()
}

#####
# tuning 
#####

# grid of hyperparameters 
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = c(1,5,25,50),
  colsample_bytree = c(.25,.5,.75),
  min_child_weight = c(1,5,25,50),
  subsample = seq(0,1,.1)
)

# train function controls 
tune_train_control <- trainControl(method = 'repeatedcv', 
                                   number = 3, 
                                   repeats = 3,
                                   allowParallel = T,
                                   savePredictions = T)

# iterating over all hyperparameter combinations 
tune_mod <- train(is_pgb ~ rel_plate_x + rel_plate_z + release_speed + gb_rate_to_date + pull_rate_to_date + pfx_x + pfx_z +
                    pitch_type_CH + pitch_type_CU + pitch_type_FC + pitch_type_FF + pitch_type_FS +
                    pitch_type_KC + pitch_type_SI + pitch_type_SL, 
                  data = testdf2, 
                  trControl = tune_train_control, 
                  method = 'xgbTree',
                  tuneGrid = tune_grid,
                  na.action = na.pass,
                  verbosity = 0)

# helper function for plots 
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy, na.rm = T))) +
    theme_bw()
}

# best tune grid 
best_tune <- expand.grid(
  nrounds = tune_mod$bestTune$nrounds,
  eta = tune_mod$bestTune$eta,
  max_depth = tune_mod$bestTune$max_depth,
  gamma = tune_mod$bestTune$gamma,
  colsample_bytree = tune_mod$bestTune$colsample_bytree,
  min_child_weight = tune_mod$bestTune$min_child_weight,
  subsample = tune_mod$bestTune$subsample
)

#####
# training all years #
#####

# xgbtree function 
run_mod_xgbtree <- function(pyear, cyear) {
  
  xgbtree_train_control <- trainControl(method = 'repeatedcv', 
                                        number = 10, 
                                        repeats = 5,
                                        allowParallel = T,
                                        savePredictions = T)
  
  mod <- train(is_pgb ~ rel_plate_x + rel_plate_z + release_speed + gb_rate_to_date + pull_rate_to_date + pfx_x + pfx_z +
                 pitch_type_CH + pitch_type_CU + pitch_type_FC + pitch_type_FF + pitch_type_FS +
                 pitch_type_KC + pitch_type_SI + pitch_type_SL, 
               data = pyear, 
               tuneGrid = best_tune,
               trControl = xgbtree_train_control, 
               method = 'xgbTree',
               na.action = na.pass,
               verbosity = 0)
  mod
  
  pred <- predict(mod, newdata = cyear, na.action = na.pass)
  
  metrics <- metrics(mod, cyear, cyear$is_pgb)
  
  return(metrics)
  
}

{
  # xgbtree for all years 
  years <- list(df15_prep, df16_prep, df17_prep, df18_prep, df19_prep, df20_prep, df21_prep)
  results_xgbtree <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results_xgbtree) <- c('year','precision', 'recall', 'f score', 'accuracy')
  
  for(x in 1:length(years)) {
    
    result <- c(years[[x+1]][1,'game_year'], run_mod_xgbtree(years[[x]], years[[x+1]]))
    results_xgbtree[nrow(results_xgbtree)+1,] <- result
    
    
  }
  beep()
}

view(results_xgbtree)

# simple model for all years 
int_results <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(int_results) <- c('year','precision', 'recall', 'f score', 'accuracy')

for (x in 1:length(years)) {
  
  pred <- fifelse(years[[x]][,'pgb_rate_to_date'] >= .5, 1, 0)
  
  comp <- data.frame(pred = pred,
                     real = years[[x]][,'is_pgb'])
  
  tab <- table(comp)
  tab <- tab[nrow(tab):1,][,ncol(tab):1]
  
  perc <- round(precision(tab),2)
  recall <- round(recall(tab),2)
  f_score <- round(F_meas(tab),2)
  acc <- round((tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2]),2)
  
  int_results[nrow(int_results)+1,] <- c(years[[x]][1,'game_year'],
                                         perc, 
                                         recall,
                                         f_score,
                                         acc)
  
}

