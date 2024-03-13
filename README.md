# Manual for Replicating Analyses in "Emotion Dynamics in Reciprocity: Individual Differences in Social Decision-making and the Role of Prosocial Emotions"
### Jaewon Kim
### 13 March, 2024

## Table of Contents (in order of appearance on main script)
<Code script in R "edr_main_analyses.R">
1. Figure S1-2
2. Figure 2
3. Figure 3A-D
4. Table 2
5. Table S6
6. Table S7
7. Table S4
8. Table 3
9. Table S3

<<Code script in python "edr_main_analyses_python.ipynb">
1. Figure 4A-B
2. Figure S9

## Setup for python codes
1. To run this analysis, you'll need to have the packages installed in conda virtual environment ("requirements_python.txt")
- conda create --name jaewonml python=3.8.16
- conda activate jaewonml
- pip install -r .\requirements_python.txt

## Setup for R script
1. To run this analysis, you'll need to have the following packages installed:
- cluster
- ggplot2
- factoextra
- dplyr
- tidyr
- here
- tidyverse
- lme4
- sjPlot

## Folders
This repo contains the following folders : analysis, data
1. anaylsis
- "edr_main_analyses.R" : ab R script which contains part of main analyses and supplementary analyses
- "edr_main_analyses_python.ipynb" : a python script which contains part of main analyses and supplementary analyses

2. data
- agree_index_emo.csv : a csv file that contains information about which participant during ECT provided consent upon completing the experiments
- emotion_alltime_melt_N476.csv : a dataframe that contains all independent & dependent variables for GLMM analyses and UMAP embedding. Each row corresponds to a single trial for a given participant.
- emotion_trajectory_expect_org_AE_label_clinical_ratings_n476.csv : a dataframe that contains expected emotion trajectory of participants. Each row corresponds to a single participant.
- emotion_trajectory_experiece_org_AE_label_clinical_ratings_n476.csv : a dataframe that contains experienced emotion trajectory of participants. Each row corresponds to a single participant.
- reward_accept_trajectory_n476.csv : a dataframe that contains reward acceptance trajectory of participants. Each row corresponds to a single participant.
- reward_expect_trajectory_n476.csv : a dataframe that contains expected reward trajectory of participants. Each row corresponds to a single participant.

**The following 4 csv files** contain dataframes that contain ECT responses for each emotion label.
- emotions_arousal_imp.csv
- emotions_focus_imp.csv
- emotions_dominance_imp.csv
- emotions_valence_imp.csv

3. graphs<br>
Graphs produced for manuscripts from analysis folder. Some style changes have been made to generate the final graphs in the manuscript using Illustrator only for the purpose of better visualization.

## Contact
If you have questions, please contact me at jwk1921@kaist.ac.kr
