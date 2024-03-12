library(cluster)
library(ggplot2)
library(factoextra)
library(dplyr)
library(tidyr)
library(here)
library(tidyverse)
library(lme4)
library(sjPlot)       # clean tables
##### Line 8-46 correspond to raw data preprocessing
# Raw data paths
dir_analysis <- "C:/Users/user/Desktop/Proposal/Experiments/Moral_Ultimatum_Game_Analysis/article1/publication/analysis" # should be where analysis script is stored
dir_parent <- dir_analysis %>% str_remove("/analysis")
dir_data <- str_c(dir_parent, "/data")
dir_graphs <- str_c(dir_parent, "/graphs")

# Reward acceptance & Expected reward trajectories of all participants
reward_accept_traj <- read_csv(str_c(dir_data, "/reward_accept_trajectory_n476.csv"))
reward_accept_traj <- reward_accept_traj[, -1]
reward_expect_traj <- read_csv(str_c(dir_data, "/reward_expect_trajectory_n476.csv"))
reward_expect_traj <- reward_expect_traj[, -1]

# Emotion experience & expectation trajectories of all participants
emo_experience_traj <- read_csv(str_c(dir_data, "/emotion_trajectory_experiece_org_AE_label_clinical_ratings_n476.csv"))
emo_expect_traj <- read_csv(str_c(dir_data, "/emotion_trajectory_expect_org_AE_label_clinical_ratings_n476.csv"))

columns_valence <- c()
columns_arousal <- c()
columns_focus <- c()
columns_dominance <- c()
for (i in 1:30) {
  columns_valence[i] <- 4 * i - 2
  columns_arousal[i] <- 4 * i - 1
  columns_focus[i] <- 4 * i
  columns_dominance[i] <- 4 * i + 1
}

emotion_experience_group <- emo_experience_traj[, "AE_cluster_experience"]

valence_traj_experience <- emo_experience_traj[, columns_valence]
arousal_traj_experience <- emo_experience_traj[, columns_arousal]
focus_traj_experience <- emo_experience_traj[, columns_focus]
dominance_traj_experience <- emo_experience_traj[, columns_dominance]

valence_traj_experience[, "emotion_experience_group"] <- emotion_experience_group
arousal_traj_experience[, "emotion_experience_group"] <- emotion_experience_group
focus_traj_experience[, "emotion_experience_group"] <- emotion_experience_group
dominance_traj_experience[, "emotion_experience_group"] <- emotion_experience_group

valence_expect <- emo_expect_traj[, columns_valence]
arousal_expect <- emo_expect_traj[, columns_arousal]
focus_expect <- emo_expect_traj[, columns_focus]
dominance_expect <- emo_expect_traj[, columns_dominance]

valence_expect[, "emotion_experience_group"] <- emotion_experience_group
arousal_expect[, "emotion_experience_group"] <- emotion_experience_group
focus_expect[, "emotion_experience_group"] <- emotion_experience_group
dominance_expect[, "emotion_experience_group"] <- emotion_experience_group

##### Line 48-58 correspond to deciding the optimal K for reward group
### Figure S1-2 : Extract only time series portion and find optimal number of clusters by k-means
reward_accept_traj_times <- reward_accept_traj %>% select(c(2:31))
reward_expect_traj_times <- reward_expect_traj %>% select(c(2:31))
reward_given <- c(0, 0, 2, 1, 0, 3, 0, 1, 1, 1, 2, 3, 3, 4, 2, 3, 2, 2, 3, 4, 6, 5, 4, 4, 5, 4, 7, 5, 5, 4) ### Actual proposer offer
reward_given <- data.frame(reward_given = reward_given)
reward_given$time_point <- seq_len(nrow(reward_given))

# Figure S1
fviz_nbclust(reward_expect_traj_times, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) 

# Figure S2
fviz_nbclust(reward_accept_traj_times, kmeans, method = "wss") + 
  geom_vline(xintercept = 4, linetype = 2) 
##### Line 60 - 106 correspond to 1) K-means clustering or reward acceptance trajectories,
##### and then 2) pivoting the trajectory to a long dataframe
# K-means clustering of reward acceptance trajectories with k = 4
set.seed(123)
km.acc <- kmeans(reward_accept_traj_times, 4, nstart = 25)
kmeans_group_acc <- km.acc$cluster #Cluster allocation for each participant
cluster_centroids <- km.acc$centers #Centroid for each cluster
cluster_centroids <- as.data.frame(cluster_centroids)

km.exp <- kmeans(reward_accept_traj_times[, 1:30], 4, nstart = 25)
kmeans_group_exp <- km.exp$cluster #Cluster allocation for each participant
cluster_centroids_expect <- km.exp$centers #Centroid for each cluster
cluster_centroids_expect <- as.data.frame(cluster_centroids_expect)
reward_accept_traj_times[, "kmeans_group"] <- kmeans_group_acc
reward_expect_traj_times[, "rewexpect_group"] <- kmeans_group_exp
reward_accept_traj_times[, "participant"] <- as.integer(1:476)
reward_expect_traj_times[, "participant"] <- as.integer(1:476)

write.csv(reward_accept_traj_times, 
          file='reward_accept_trajectory_n476.csv',
          fileEncoding='UTF-8',
          row.names=TRUE)

write.csv(reward_expect_traj_times, 
          file='reward_expect_trajectory_n476.csv',
          fileEncoding='UTF-8',
          row.names=TRUE)

# Standard deviation of reward acceptance at each time point for each group
standard_deviation <- data.frame()
for (i in 1:4) {
  for (j in 1:30) {
    standard_deviation[i, j] <- reward_accept_traj_times %>%
      filter(kmeans_group == i) %>%
      summarise(sd = sd(.[[j]])) %>%
      pull(sd)
  }
}

cluster_centroids$group <- 1:nrow(cluster_centroids)
cluster_centroids <- t(cluster_centroids)
colnames(standard_deviation) <- as.integer(1:30)
standard_deviation$group <- 1:nrow(standard_deviation)

# Pivot dataframe to make it a long form : reward acceptance
acc_long <- pivot_longer(reward_accept_traj_times,
                         col = 1:30,
                         names_to = "time_point",
                         values_to = "decision")
acc_long$time_point <- as.numeric(as.character(acc_long$time_point))


cluster_centroids <- t(cluster_centroids)
cluster_centroids <- as.data.frame(cluster_centroids)
mean_long <- pivot_longer(cluster_centroids, 
                          cols = 1:30,  # Assuming you want to pivot all 30 columns
                          names_to = "time_point", 
                          values_to = "mean_response")

sd_long <- pivot_longer(standard_deviation, 
                          cols = 1:30,  # Assuming you want to pivot all 30 columns
                          names_to = "time_point", 
                          values_to = "sd_response")

merged_df <- merge(mean_long, sd_long, by = c("group", "time_point"))
merged_df$time_point <- as.numeric(as.character(merged_df$time_point))

##### Line 110 - 257 correspond to 1) extracting central tendency of experience emotion trajectories,
##### and then 2) pivoting the trajectories to long dataframes
# Mean and standard deviations of experienced valence at each time point for each group
standard_deviation_val <- data.frame()
centroid_val <- data.frame()
for (i in 1:4) {
  for (j in 1:30) {
    standard_deviation_val[i, j] <- valence_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(sd = sd(.[[j]])) %>%
      pull(sd)
    centroid_val[i, j] <- valence_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(mean = mean(.[[j]])) %>%
      pull(mean)
  }
}

colnames(standard_deviation_val) <- as.integer(1:30)
colnames(centroid_val) <- as.integer(1:30)

standard_deviation_val$group <- 1:nrow(standard_deviation_val)
centroid_val$group <- 1:nrow(centroid_val)

# Pivot dataframe to make it a long form : valence experience
val_long <- pivot_longer(centroid_val,
                         col = 1:30,
                         names_to = "time_point",
                         values_to = "valence")


val_long_sd <- pivot_longer(standard_deviation_val,
                            col = 1:30,
                            names_to = "time_point",
                            values_to = "valence_sd")

merged_df_val <- merge(val_long, val_long_sd, by = c("group", "time_point"))
merged_df_val$time_point <- as.numeric(as.character(merged_df_val$time_point))

# Mean and standard deviations of experienced arousal at each time point for each group
standard_deviation_aro <- data.frame()
centroid_aro <- data.frame()
for (i in 1:4) {
  for (j in 1:30) {
    standard_deviation_aro[i, j] <- arousal_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(sd = sd(.[[j]])) %>%
      pull(sd)
    centroid_aro[i, j] <- arousal_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(mean = mean(.[[j]])) %>%
      pull(mean)
  }
}

colnames(standard_deviation_aro) <- as.integer(1:30)
colnames(centroid_aro) <- as.integer(1:30)

standard_deviation_aro$group <- 1:nrow(standard_deviation_aro)
centroid_aro$group <- 1:nrow(centroid_aro)

# Pivot dataframe to make it a long form : arousal experience
aro_long <- pivot_longer(centroid_aro,
                         col = 1:30,
                         names_to = "time_point",
                         values_to = "arousal")


aro_long_sd <- pivot_longer(standard_deviation_aro,
                            col = 1:30,
                            names_to = "time_point",
                            values_to = "arousal_sd")

merged_df_aro <- merge(aro_long, aro_long_sd, by = c("group", "time_point"))
merged_df_aro$time_point <- as.numeric(as.character(merged_df_aro$time_point))

# Mean and standard deviations of experienced focus at each time point for each group
standard_deviation_foc <- data.frame()
centroid_foc <- data.frame()
for (i in 1:4) {
  for (j in 1:30) {
    standard_deviation_foc[i, j] <- focus_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(sd = sd(.[[j]])) %>%
      pull(sd)
    centroid_foc[i, j] <- focus_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(mean = mean(.[[j]])) %>%
      pull(mean)
  }
}

colnames(standard_deviation_foc) <- as.integer(1:30)
colnames(centroid_foc) <- as.integer(1:30)

standard_deviation_foc$group <- 1:nrow(standard_deviation_foc)
centroid_foc$group <- 1:nrow(centroid_foc)

# Pivot dataframe to make it a long form : focus experience
foc_long <- pivot_longer(centroid_foc,
                         col = 1:30,
                         names_to = "time_point",
                         values_to = "focus")


foc_long_sd <- pivot_longer(standard_deviation_foc,
                            col = 1:30,
                            names_to = "time_point",
                            values_to = "focus_sd")

merged_df_foc <- merge(foc_long, foc_long_sd, by = c("group", "time_point"))
merged_df_foc$time_point <- as.numeric(as.character(merged_df_foc$time_point))

###
# Mean and standard deviations of experienced dominance at each time point for each group
standard_deviation_dom <- data.frame()
centroid_dom <- data.frame()
for (i in 1:4) {
  for (j in 1:30) {
    standard_deviation_dom[i, j] <- dominance_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(sd = sd(.[[j]])) %>%
      pull(sd)
    centroid_dom[i, j] <- dominance_traj_experience %>%
      filter(emotion_experience_group == i - 1) %>%
      summarise(mean = mean(.[[j]])) %>%
      pull(mean)
  }
}

colnames(standard_deviation_dom) <- as.integer(1:30)
colnames(centroid_dom) <- as.integer(1:30)

standard_deviation_dom$group <- 1:nrow(standard_deviation_dom)
centroid_dom$group <- 1:nrow(centroid_dom)

# Pivot dataframe to make it a long form : dominance experience
dom_long <- pivot_longer(centroid_dom,
                         col = 1:30,
                         names_to = "time_point",
                         values_to = "dominance")


dom_long_sd <- pivot_longer(standard_deviation_dom,
                            col = 1:30,
                            names_to = "time_point",
                            values_to = "dominance_sd")

merged_df_dom <- merge(dom_long, dom_long_sd, by = c("group", "time_point"))
merged_df_dom$time_point <- as.numeric(as.character(merged_df_dom$time_point))

##### Lines 260 - 307 correspond to Figures 2 and 3
# Define a color palette
# Adjust the number of colors based on the number of groups you have
color_palette <- c("blue", "red", "green", "orange")  # Example colors for 4 groups

# Figure 2) Plot with manually specified colors : reward acceptance
ggplot(merged_df, aes(x = time_point, y = mean_response, group = group)) +
  geom_point(aes(color = as.factor(group), shape = "circle"), size = 8, alpha = 0.7) + 
  geom_smooth(aes(color = as.factor(group)), se = TRUE, method = "gam", size = 2, orientation = 'x', span = 2, level = 0.95) +  # Add smoothed mean lines
  scale_fill_manual(values = color_palette) +
  coord_cartesian(ylim = c(0.0, 1.0)) + 
  theme_minimal() +
  labs(x = "Time Point", y = "Mean Response", color = "Group", fill = "Group")
# Figure 3A) Plot with manually specified colors : experienced valence
ggplot(merged_df_val, aes(x = time_point, y = valence, group = group)) +
  geom_point(aes(color = as.factor(group), shape = "circle"), size = 8, alpha = 0.7) + 
  geom_smooth(aes(color = as.factor(group)), se = TRUE, method = "gam", size = 2, orientation = 'x', span = 2, level = 0.95) +  # Add smoothed mean lines
  scale_fill_manual(values = color_palette) +
  coord_cartesian(ylim = c(-250, 250)) + 
  theme_minimal() +
  labs(x = "Time Point", y = "Valence Centroid", color = "Group", fill = "Group")
# Figure 3B) Plot with manually specified colors : experienced arousal
ggplot(merged_df_aro, aes(x = time_point, y = arousal, group = group)) +
  geom_point(aes(color = as.factor(group), shape = "circle"), size = 8, alpha = 0.7) + 
  geom_smooth(aes(color = as.factor(group)), se = TRUE, method = "gam", size = 2, orientation = 'x', span = 2, level = 0.95) +  # Add smoothed mean lines
  scale_fill_manual(values = color_palette) +
  coord_cartesian(ylim = c(-250, 250)) + 
  theme_minimal() +
  labs(x = "Time Point", y = "Arousal Centroid", color = "Group", fill = "Group")
# Figure 3C) Plot with manually specified colors : experienced focus
ggplot(merged_df_foc, aes(x = time_point, y = focus, group = group)) +
  geom_point(aes(color = as.factor(group), shape = "circle"), size = 8, alpha = 0.7) + 
  geom_smooth(aes(color = as.factor(group)), se = TRUE, method = "gam", size = 2, orientation = 'x', span = 2, level = 0.95) +  # Add smoothed mean lines
  scale_fill_manual(values = color_palette) +
  coord_cartesian(ylim = c(1, 9)) + 
  scale_y_continuous(breaks = seq(1, 9, by = 2),
                     labels = seq(-4, 4, by = 2)) + 
  theme_minimal() +
  labs(x = "Time Point", y = "Focus Centroid", color = "Group", fill = "Group")
# Figure 3D) Plot with manually specified colors : experienced dominance
ggplot(merged_df_dom, aes(x = time_point, y = dominance, group = group)) +
  geom_point(aes(color = as.factor(group), shape = "circle"), size = 8, alpha = 0.7) + 
  geom_smooth(aes(color = as.factor(group)), se = TRUE, method = "gam", size = 2, orientation = 'x', span = 2, level = 0.95) +  # Add smoothed mean lines
  scale_fill_manual(values = color_palette) +
  coord_cartesian(ylim = c(1, 9)) + 
  scale_y_continuous(breaks = seq(1, 9, by = 2),
                     labels = seq(-4, 4, by = 2)) + 
  theme_minimal() +
  labs(x = "Time Point", y = "Dominance Centroid", color = "Group", fill = "Group")

# Significance of centroid mean comparison
set.seed(123)
#reward_accept_traj_times
model_acc <- aov(mean_response~as.factor(group), data=merged_df) #one-way ANOVA for existence of group difference
model_acc2<- aov(mean_response~group, data=reward_accept_traj_times) 
summary(model_acc)

TukeyHSD(model_acc, conf.level=.95)

##### GLMM analyses on relationship between reward PE, emotion PEs and decisions to accept
### Preprocessing
valence_traj_experience_ <- valence_traj_experience[, 1:30]
arousal_traj_experience_ <- arousal_traj_experience[, 1:30]
focus_traj_experience_ <- focus_traj_experience[, 1:30]
dominance_traj_experience_ <- dominance_traj_experience[, 1:30]

colnames(valence_traj_experience_) <- as.integer(1:30)
colnames(arousal_traj_experience_) <- as.integer(1:30)
colnames(focus_traj_experience_) <- as.integer(1:30)
colnames(dominance_traj_experience_) <- as.integer(1:30)

valence_traj_experience_[, "emotion_experience_group"] <- emotion_experience_group
arousal_traj_experience_[, "emotion_experience_group"] <- emotion_experience_group
focus_traj_experience_[, "emotion_experience_group"] <- emotion_experience_group
dominance_traj_experience_[, "emotion_experience_group"] <- emotion_experience_group

valence_traj_experience_[, "participant"] <- as.integer(1:476)
arousal_traj_experience_[, "participant"] <- as.integer(1:476)
focus_traj_experience_[, "participant"] <- as.integer(1:476)
dominance_traj_experience_[, "participant"] <- as.integer(1:476)

valence_expect_ <- valence_expect[, 1:30]
arousal_expect_ <- arousal_expect[, 1:30]
focus_expect_ <- focus_expect[, 1:30]
dominance_expect_ <- dominance_expect[, 1:30]

colnames(valence_expect_) <- as.integer(1:30)
colnames(arousal_expect_) <- as.integer(1:30)
colnames(focus_expect_) <- as.integer(1:30)
colnames(dominance_expect_) <- as.integer(1:30)

valence_expect_[, "emotion_experience_group"] <- emotion_experience_group
arousal_expect_[, "emotion_experience_group"] <- emotion_experience_group
focus_expect_[, "emotion_experience_group"] <- emotion_experience_group
dominance_expect_[, "emotion_experience_group"] <- emotion_experience_group

valence_expect_[, "participant"] <- as.integer(1:476)
arousal_expect_[, "participant"] <- as.integer(1:476)
focus_expect_[, "participant"] <- as.integer(1:476)
dominance_expect_[, "participant"] <- as.integer(1:476)

valence_pe <- valence_traj_experience_[, 1:30] - valence_expect_[, 1:30]
arousal_pe <- arousal_traj_experience_[, 1:30] - arousal_expect_[, 1:30]
focus_pe <- focus_traj_experience_[, 1:30] - focus_expect_[, 1:30]
dominance_pe <- dominance_traj_experience_[, 1:30] - dominance_expect_[, 1:30]

valence_pe[, "emotion_experience_group"] <- emotion_experience_group
arousal_pe[, "emotion_experience_group"] <- emotion_experience_group
focus_pe[, "emotion_experience_group"] <- emotion_experience_group
dominance_pe[, "emotion_experience_group"] <- emotion_experience_group

valence_pe[, "participant"] <- as.integer(1:476)
arousal_pe[, "participant"] <- as.integer(1:476)
focus_pe[, "participant"] <- as.integer(1:476)
dominance_pe[, "participant"] <- as.integer(1:476)

rew_exp_long <- pivot_longer(reward_expect_traj_times,
                             col = 1:30,
                             names_to = "time_point",
                             values_to = "RPRE")
rew_acc_long <- pivot_longer(reward_accept_traj_times,
                             col = 1:30,
                             names_to = "time_point",
                             values_to = "acceptance")
vpost_long <- pivot_longer(valence_traj_experience_,
                         col = 1:30,
                         names_to = "time_point",
                         values_to = "VPOST")
apost_long <- pivot_longer(arousal_traj_experience_,
                             col = 1:30,
                             names_to = "time_point",
                             values_to = "APOST")
fpost_long <- pivot_longer(focus_traj_experience_,
                             col = 1:30,
                             names_to = "time_point",
                             values_to = "FPOST")
dpost_long <- pivot_longer(dominance_traj_experience_,
                             col = 1:30,
                             names_to = "time_point",
                             values_to = "DPOST")
vpre_long <- pivot_longer(valence_expect_,
                           col = 1:30,
                           names_to = "time_point",
                           values_to = "VPRE")
apre_long <- pivot_longer(arousal_expect_,
                           col = 1:30,
                           names_to = "time_point",
                           values_to = "APRE")
fpre_long <- pivot_longer(focus_expect_,
                           col = 1:30,
                           names_to = "time_point",
                           values_to = "FPRE")
dpre_long <- pivot_longer(dominance_expect_,
                           col = 1:30,
                           names_to = "time_point",
                           values_to = "DPRE")

df_all <-  merge(rew_exp_long, rew_acc_long, by = c("participant", "time_point"))
df_all <-  merge(df_all, vpost_long, by = c("participant", "time_point"))
df_all <-  merge(df_all, apost_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, fpost_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, dpost_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, vpre_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, apre_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, fpre_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, dpre_long[, 2:4], by = c("participant", "time_point"))
df_all <-  merge(df_all, reward_given, by = "time_point")
df_all[, "RPE"] <- df_all[, "reward_given"] - df_all[, "RPRE"]
df_all[, "VPE"] <- df_all[, "VPOST"] - df_all[, "VPRE"]
df_all[, "APE"] <- df_all[, "APOST"] - df_all[, "APRE"]
df_all[, "FPE"] <- df_all[, "FPOST"] - df_all[, "FPRE"]
df_all[, "DPE"] <- df_all[, "DPOST"] - df_all[, "DPRE"]
df_all$FPRE <- df_all$FPRE - 5
df_all$DPRE <- df_all$DPRE - 5
df_all$FPOST <- df_all$FPOST - 5
df_all$DPOST <- df_all$DPOST - 5

write.csv(df_all, 
          file='emotion_alltime_melt_N476.csv',
          fileEncoding='UTF-8',
          row.names=TRUE)

### Scaling each predictors
df_all$RPRE <- scale(df_all$RPRE, center = F)
df_all$VPRE <- scale(df_all$VPRE, center = F)
df_all$APRE <- scale(df_all$APRE, center = F)
df_all$FPRE <- scale(df_all$FPRE, center = F)
df_all$DPRE <- scale(df_all$DPRE, center = F)

df_all$VPOST <- scale(df_all$VPOST, center = F)
df_all$APOST <- scale(df_all$APOST, center = F)
df_all$FPOST <- scale(df_all$FPOST, center = F)
df_all$DPOST <- scale(df_all$DPOST, center = F)

df_all$RPE <- scale(df_all$RPE, center = F)
df_all$VPE <- scale(df_all$VPE, center = F)
df_all$APE <- scale(df_all$APE, center = F)
df_all$FPE <- scale(df_all$FPE, center = F)
df_all$DPE <- scale(df_all$DPE, center = F)

df_all_group1 <- df_all %>% filter(emotion_experience_group == 0)
df_all_group2 <- df_all %>% filter(emotion_experience_group == 1)
df_all_group3 <- df_all %>% filter(emotion_experience_group == 2)
df_all_group4 <- df_all %>% filter(emotion_experience_group == 3)

# Table 2
table_PE_m5 <- glmer(acceptance ~ RPE + VPE + APE + FPE + DPE + (1 + RPE + VPE + APE + FPE + DPE| participant), 
                     data = df_all,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

tab_model(table_PE_m5, transform = NULL, title = "Contribution of RPE and EPEs on social decision : All groups M5", 
          pred.labels = c("Intercept", "Reward PE", "Valence PE", "Arousal PE", "Focus PE", "Dominance PE"),
          dv.labels = c("Estimates"), string.est = "Log-Odds", string.se = "SE", string.stat = "Z", 
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, 
          show.re.var = FALSE, show.aic = FALSE, show.dev = FALSE, 
          show.r2 = FALSE, show.icc = FALSE, show.obs = TRUE,
          CSS = css_theme("regression"), file = str_c(dir_graphs, "/table_PE_m5.html"))

table_PE_m1a <- glmer(acceptance ~ VPE + (1 + VPE | participant), 
                     data = df_all,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
table_PE_m2a <- glmer(acceptance ~ VPE + RPE + (1 + VPE + RPE | participant), 
                      data = df_all,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
table_PE_m3a <- glmer(acceptance ~ VPE + RPE + DPE + (1 + VPE + RPE + DPE | participant), 
                      data = df_all,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
table_PE_m4a <- glmer(acceptance ~ VPE + RPE + DPE + FPE + (1 + VPE + RPE + DPE + FPE | participant), 
                      data = df_all,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
# Table S6
rmcorr::rmcorr(participant, RPE, VPE, df_all)
rmcorr::rmcorr(participant, RPE, APE, df_all)
rmcorr::rmcorr(participant, RPE, FPE, df_all)
rmcorr::rmcorr(participant, RPE, DPE, df_all)
rmcorr::rmcorr(participant, VPE, APE, df_all)
rmcorr::rmcorr(participant, VPE, FPE, df_all)
rmcorr::rmcorr(participant, VPE, DPE, df_all)
rmcorr::rmcorr(participant, APE, FPE, df_all)
rmcorr::rmcorr(participant, APE, DPE, df_all)
rmcorr::rmcorr(participant, FPE, DPE, df_all)

### Table S7 : Checking multicollinearity
round(car::vif(table_PE_m5), 2)

### Table S4 : Model comparison using AIC
AIC(table_PE_m1a, table_PE_m2a, table_PE_m3a, table_PE_m4a, table_PE_m5)
### Model comparison significance test using likelihood ratio test
anova(table_PE_m1a, table_PE_m2a)
anova(table_PE_m2a, table_PE_m3a)
anova(table_PE_m3a, table_PE_m4a)
anova(table_PE_m4a, table_PE_m5)


# Table 3
table_PE_m5_group1 <- glmer(acceptance ~ RPE + VPE + APE + FPE + DPE + (1 + RPE + VPE + APE + FPE + DPE| participant), 
                            data = df_all_group1,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))

table_PE_m5_group2 <- glmer(acceptance ~ RPE + VPE + APE + FPE + DPE + (1 + RPE + VPE + APE + FPE + DPE| participant), 
                            data = df_all_group2,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))

table_PE_m5_group3 <- glmer(acceptance ~ RPE + VPE + APE + FPE + DPE + (1 + RPE + VPE + APE + FPE + DPE| participant), 
                            data = df_all_group3,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))

table_PE_m5_group4 <- glmer(acceptance ~ RPE + VPE + APE + FPE + DPE + (1 + RPE + VPE + APE + FPE + DPE| participant), 
                            data = df_all_group4,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))

tab_model(table_PE_m5_group1, transform = NULL, title = "Contribution of RPE and EPEs on social decision : NON", 
          pred.labels = c("Intercept", "Reward PE", "Valence PE", "Arousal PE", "Focus PE", "Dominance PE"),
          dv.labels = c("Estimates"), string.est = "Log-Odds", string.se = "SE", string.stat = "Z", 
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, 
          show.re.var = FALSE, show.aic = FALSE, show.dev = FALSE, 
          show.r2 = FALSE, show.icc = FALSE, show.obs = TRUE,
          CSS = css_theme("regression"), file = str_c(dir_graphs, "/table_PE_m5_group1.html"))

tab_model(table_PE_m5_group2, transform = NULL, title = "Contribution of RPE and EPEs on social decision : IND", 
          pred.labels = c("Intercept", "Reward PE", "Valence PE", "Arousal PE", "Focus PE", "Dominance PE"),
          dv.labels = c("Estimates"), string.est = "Log-Odds", string.se = "SE", string.stat = "Z", 
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, 
          show.re.var = FALSE, show.aic = FALSE, show.dev = FALSE, 
          show.r2 = FALSE, show.icc = FALSE, show.obs = TRUE,
          CSS = css_theme("regression"), file = str_c(dir_graphs, "/table_PE_m5_group2.html"))

tab_model(table_PE_m5_group3, transform = NULL, title = "Contribution of RPE and EPEs on social decision : REC", 
          pred.labels = c("Intercept", "Reward PE", "Valence PE", "Arousal PE", "Focus PE", "Dominance PE"),
          dv.labels = c("Estimates"), string.est = "Log-Odds", string.se = "SE", string.stat = "Z", 
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, 
          show.re.var = FALSE, show.aic = FALSE, show.dev = FALSE, 
          show.r2 = FALSE, show.icc = FALSE, show.obs = TRUE,
          CSS = css_theme("regression"), file = str_c(dir_graphs, "/table_PE_m5_group3.html"))

tab_model(table_PE_m5_group4, transform = NULL, title = "Contribution of RPE and EPEs on social decision : RAT", 
          pred.labels = c("Intercept", "Reward PE", "Valence PE", "Arousal PE", "Focus PE", "Dominance PE"),
          dv.labels = c("Estimates"), string.est = "Log-Odds", string.se = "SE", string.stat = "Z", 
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, 
          show.re.var = FALSE, show.aic = FALSE, show.dev = FALSE, 
          show.r2 = FALSE, show.icc = FALSE, show.obs = TRUE,
          CSS = css_theme("regression"), file = str_c(dir_graphs, "/table_PE_m5_group4.html"))
