# YouTube’s Adpocalypse - Study of creators’ participation on the platform after the implementation of a demonetisation policy.

## Paper abstract
This work examines the causal effect of the implementation of a demonetisation rule on content supply of 51,583 English-speaking channels on YouTube. With a difference-in-differences (DiD) model, we estimated the impact of an economic sanction on creators’ production strategies on two levels: the number of weekly posted videos and the proportion of not-suitable-for-advertisers content in the weekly video supply.  
First, the adjustment in the demonetisation rule led to a decrease in the volume of weekly video supply by non-advertiser-friendly content creators compared to brand-safe ones. Second, the impact of such moderation action is heterogeneous among the channel’s main content categories and its audience size. Third, the introduction of this new rule led to a reduction of the share of non-advertiser-friendly videos in the volume of weekly supply. These findings provide additional evidence on the importance of extrinsic motivations and financial incentives in the creators’ level of participation on the platform. These results illustrate the platform’s ability to control the flux of content. Moreover, it highlights the limits of a uniform moderation action on the reduction of harmful content and as a platform governance strategy.  

The econ job market paper is available [here](https://www.dropbox.com/scl/fi/qecvotx5uqebxufnopm7x/Econ_Job_paper_RASSKAZOVA_V3.pdf?rlkey=d45tshfycw69xwsbxu8h4lc5i&st=uha88v14&dl=0).

## Requirements
**Python**: numpy, pandas, gzip, re (regular expression).  
**R**: tidyr; dplyr;

## Data
🚀 This paper draw on data gathered in the [YouNiverse: Large-Scale Channel and Video Metadata from English YouTube](https://github.com/epfl-dlab/YouNiverse) collected and presented by Manoel Horta Ribeiro and Robert West. The whole collection is available on [Zenodo](https://zenodo.org/records/4650046).   

📑For this paper, we sampled channels created before January 1, 2016 and active between January 2016 and March 2017. Moreover, we selected channels whose videos are not mainly associated with traditional media (films and animation, music, news and politics, and sports). From these sampled channels, we retrieved videos uploaded between **August 2016 and October 2017**. As the YouNiverse dataset provides information at the video level, we constructed a panel at the channel-week level for the observed periods. The final sample is composed of **51,859 channels and 5,596,628 videos.**

## Presentation of the folders
### 1. Creating the dataset.
The first folder, "*1_Creating_sample*", contains all the scripts (mainly written in Python in Jupyter Notebook) that clean the initial dataset and create the studied video sample. Optimization could be done as some selection criteria were introduced later in the analysis (smaller time window).   

### 2. Creating the independent variables.
In this folder, we regrouped the scripts used to create the independant variables of our DiD model: channels' main video genre, average subscribers count, treatment/control group.

### 3. Econometric analysis folder
This folder contains the scripts to estimate the main DiD model with the three variations (general, per video genre and per audience size). It also gathers the notebooks for the additional analysis on the effect of the policy on the users' toxicity level. 
