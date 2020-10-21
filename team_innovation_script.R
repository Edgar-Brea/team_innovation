library(RMySQL)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(cluster)
library(factoextra)
library(viridis)
library(psych)
library(gplots)
library(plotrix)
library(tm)
library(ggpubr)
library(npmv)
library(readr)

#********************************************************************************************************************************************************************************************************
# Code to investigate the different typologies of team composition in open source software development projects, and then assess the effects of typologies on the innovativeness of the project outputs.
# It builds on Github data (sourced from GHTorrent, dump: mysql-2015-09-25)
# STEP 1:
#   1A - Get github  data from DB, and sequentially filter each data coming from db's table based on the projects in the sample, to minimise timely querying of unrelated data.
#   1B - Inspect data for missing values, zeros, extreme outliers or erroneous characters
# STEP 2 - Construct variables (remember projects is the unit of analysis)
# STEP 3 - Run hierarchical and k-means clustering
# STEP 4 - Run MANOVA (and also construction of additional DV more related with novelty)
# STEP 5 - Visualization and results
#********************************************************************************************************************************************************************************************************

options(scipen=999) #disable scientific notation


#===STEP 1 (get data and inspect) =============================================================================================================================================================
#1A. Get data from DB
psswd <- .rs.askForPassword("Database password:") #activate a pop up asking for the password, which is then stored in this variable
mydb <- RMySQL::dbConnect(MySQL(), user = 'root', password = psswd, dbname = 'ghtorrent') # connect with MySQL database
query <- function(...) dbGetQuery(mydb, ...) #---Function to make queries faster and easier
projects_sample <- query("SELECT id, owner_id, name, description, language, created_at FROM projects WHERE forked_from IS NULL AND deleted = 0 AND year(created_at) = 2011 AND description IS NOT NULL AND description <> '' AND owner_id IS NOT NULL;")

#Filter 1: bring projects with more than 1 member (real teams):
project_members_full <- query("SELECT repo_id, user_id, created_at FROM ghtorrent.project_members;")
list_of_users_one <- project_members_full[project_members_full$repo_id %in% projects_sample$id,2] # vector of all users associated with projects_sample
project_members <- project_members_full[project_members_full$user_id %in% list_of_users_one, ] #leave only those members associated with projects_sample

#count by projects:
temp1 <- project_members %>% 
  group_by(repo_id) %>% 
  summarise(n=n())

temp1 <- temp1[temp1$n>1,1] #create vector for filtering
projects_sample <- projects_sample[projects_sample$id %in% temp1[[1]],] #filter
projects_sample$created_at <- as.POSIXct(projects_sample$created_at,format="%Y-%m-%d %H:%M:%S") #turn into proper datetime

#filter 2: user data only for the projects_sample:
users <- query("SELECT id, name, company, location, created_at, type, fake, deleted FROM ghtorrent.users;")
temp2 <- c(projects_sample$owner_id, project_members$user_id)#vector of project members + project owners (note: there are cases where a project owner is not in project_members)
temp2 <- unique(temp2)
users <- users[users$id %in% temp2,] #filter users leaving only those associated with the sample projects

#Filter 3: Bring data from other projects but leave only those relating to users associated with the sample projects or that are forks from the sample projects:
projects_full_orig <- query("SELECT id, owner_id, name, description, language, created_at, forked_from, deleted FROM projects;")
temp3 <- project_members_full[project_members_full$user_id %in% temp2,] #df with only projects members data that is associated with the users associated with the sample projects
projects_full <- projects_full_orig[projects_full_orig$id %in% temp3$repo_id | projects_full_orig$forked_from %in% projects_sample$id,] #leave only projects associated with the users associated with the sample projects OR that are forks of the sample projects
projects_full$created_at <- as.POSIXct(projects_full$created_at,format="%Y-%m-%d %H:%M:%S") #turn into proper datetime

#Filter 4: Bring project followers data but leave only those watching the sample projects:
watchers <- query("SELECT * FROM ghtorrent.watchers;")
watchers <- watchers[watchers$repo_id %in% projects_sample$id,] #filter to those watching the sample projects

rm(temp1, temp2, temp3, project_members_full, projects_full_orig) #remove temporal dataframes from memory

#1B. Inspect and clean data:
  #done in previous steps and also when constructing variables
#===End STEP 1 =================================================================================================================================================================================



#===STEP 2 (construct variables) =============================================================================================================================================================
#2A: create a dataframe with user and user project's data associated with each focal project ----------------------------------------------------------------------------------------------
data_users <- data.frame(focal_proj_id=as.character(), user_id=as.character(), user_company=as.character(), user_type =as.character(), user_is_owner=as.character(), user_location=as.character(),
                         user_created_at=as.character(), proj_id=as.character(),proj_name=as.character(), proj_description=as.character(), project_language=as.character(), project_created_at=as.character(), stringsAsFactors = F)

#loop through each project in projects sample dataframe. The result will be a dataframe (data_users) with all the users associated with the sample of projects
pb <- txtProgressBar(min = 0, max = nrow(projects_sample), initial = 0, style = 3) #progress bar
for (i in 1:nrow(projects_sample)) {
  focal_proj <- projects_sample[i,] #vector with project under analysis
  temp_u_owner <- users[users$id==focal_proj[[2]],] #vector with owner's user data
  temp_u_owner$owner <- "yes"
  temp_u_members <- project_members[project_members$repo_id==focal_proj[[1]],2] #vector with list of members of the project
  temp_u_members <- users[users$id %in% temp_u_members,]# dataframe with members' users data
  temp_u_members$owner <- "no"
  temp_u <- rbind(temp_u_owner, temp_u_members)
  temp_u <- temp_u[!duplicated(temp_u),] #delete duplicates, if any
  temp_u <- temp_u[,c(1,3,6,9,4,5)] #order cols
  colnames(temp_u) <- c("user_id", "user_company", "user_type", "user_is_owner", "user_location", "user_created_at")
  
  temp_p_owner <- project_members[project_members$user_id==focal_proj[[2]],-3] #df with list of projects in which the member worked
  colnames(temp_p_owner)[1] <- "id"
  temp_p_owner <- merge(temp_p_owner, projects_full, by = "id") #dataframe of projects in which the owner has worked on
  temp_p_members <- project_members[project_members$repo_id==focal_proj[[1]],2] #vector with list of members of the project
  temp_p_members <- project_members[project_members$user_id %in% temp_p_members,-3] # df with projects in which each member has worked on
  colnames(temp_p_members)[1] <- "id"
  temp_p_members <- merge(temp_p_members, projects_full, by = "id") #dataframe of projects in which each member has worked on
  temp_p <- rbind(temp_p_owner, temp_p_members)
  temp_p <- temp_p[!duplicated(temp_p),] #delete duplicates, if any
  temp_p <- temp_p[,c(2,1,4:7)] #order cols
  colnames(temp_p) <- c("user_id", "proj_id", "proj_name", "proj_description", "project_language", "project_created_at")
  
  temp <- merge(temp_u, temp_p, by = "user_id", all = T)
  temp$focal_proj_id <- focal_proj[[1]]
  temp <- temp[,c(12,1:11)]
  data_users <- rbind(data_users,temp)
  setTxtProgressBar(pb,i) #update progress bar
}
data_users$user_created_at <- as.POSIXct(data_users$user_created_at,format="%Y-%m-%d %H:%M:%S") #turn into proper datetime
data_users$project_created_at <- as.POSIXct(data_users$project_created_at,format="%Y-%m-%d %H:%M:%S") #turn into proper datetime

#Some descriptive data exploration:
data_users_desc <- Hmisc::describe(data_users)
data_users_desc
#---end

##2B: construct the variables for each focal project ----------------------------------------------------------------------------------------------------------------------------------------
data_projects <- data.frame(focal_proj_id = as.character(), v1_no_organisation_members = as.numeric(), v2_geo_dispersion = as.numeric(), v3_experience_variety = as.numeric(),
                            v3_experience_diversity = as.numeric(), v4_avg_team_workload = as.numeric(), v5_no_users = as.numeric(), v6_prog_lang_variety = as.numeric(), 
                            v6_prog_lang_diversity_project = as.numeric(), v6_prog_lang_diversity_members = as.numeric(), v7_max_member_tenure = as.numeric(), 
                            v8_org_is_owner = as.numeric(), v8_prop_members_same_company = as.numeric(), dv_1_no_watchers = as.numeric(), dv_2_no_forkers = as.numeric(), stringsAsFactors = F)

list_focal_proj <- unique(data_users$focal_proj_id) #get list of projects associated with the users obtained in previous step

# loop through all the projects (not just the projects in the sample) associated with the users obtained in previous step. The result is a dataframe (data_projects) with the variables for each project as columns
pb <- txtProgressBar(min = 0, max = length(list_focal_proj), initial = 0, style = 3) #progress bar
for (j in 1:length(list_focal_proj)) {
  temp <- subset(data_users, data_users$focal_proj_id==list_focal_proj[[j]])
  temp_user_only <- temp[,2:7]
  temp_user_only <- temp_user_only[!duplicated(temp_user_only),]
  
  #Independent variables:
  v1_no_organisation_members <- length(temp_user_only[temp_user_only$user_type=="ORG",1]) #no. unique users for which type is "ORG"
  
  owner_loc <- temp_user_only[temp_user_only$user_is_owner=="yes",5] #location of the owner
  if(!is.na(owner_loc)) { #if owner location is not NA
    v2_geo_dispersion <- 1 - (nrow(subset(temp_user_only, temp_user_only$user_location==owner_loc)) / nrow(temp_user_only)) # 1 - proportion users from same location as the owner
  } else {
    v2_geo_dispersion <- NA
  }
  
  v3_experience_variety <- temp %>% group_by(user_id) %>% summarise(n = sum(project_created_at < as.POSIXct("2011-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), na.rm = T)) #by user, count their past projects
  v3_experience_variety <- length(unique(v3_experience_variety$n)) #no different experiences (measured by no. of past projects)
  
  v3_experience_diversity <- temp %>% group_by(user_id) %>% summarise(no_proj = sum(project_created_at < as.POSIXct("2011-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), na.rm = T)) #by user, count their past projects
  v3_experience_diversity <- v3_experience_diversity %>% group_by(no_proj) %>% summarise(n=n()) # count no. users with similar project counts
  v3_experience_diversity <- v3_experience_diversity[complete.cases(v3_experience_diversity),] %>% mutate(p = n/sum(n)) %>% mutate(p_sq = p^2) #calculate proportion and proportion squared for HHI
  v3_experience_diversity <- 1-sum(v3_experience_diversity$p_sq) #calculate Blau's index: 1 - HHI, where HHI is the sum of the (squared) proportions of each category from total
  
  proj_start_date <- projects_sample[projects_sample$id == list_focal_proj[[j]], 6] #starting date of focal project
  
  v4_avg_team_workload <- temp %>% 
    group_by(user_id) %>% 
    summarise(n = sum(project_created_at > proj_start_date, na.rm = T)) #by user, count their projects that started after the starting date of the focal one
  
  v4_avg_team_workload <- mean(v4_avg_team_workload$n, na.rm = T)
  
  v5_no_users <- nrow(temp_user_only)
  
  v6_prog_lang_variety <- temp[complete.cases(temp$project_language),] %>% 
    group_by(project_language) %>% 
    summarise(n=n()) #different languages per user
  
  v6_prog_lang_variety <- nrow(v6_prog_lang_variety)
  
  v6_prog_lang_diversity_project <- temp[complete.cases(temp$project_language),] %>% group_by(project_language) %>% summarise(n=n()) #count of different languages
  v6_prog_lang_diversity_project <- v6_prog_lang_diversity_project %>% mutate(p = n/sum(n)) %>% mutate(p_sq = p^2) #calculate proportion and proportion squared for HHI
  v6_prog_lang_diversity_project <- 1 - sum(v6_prog_lang_diversity_project$p_sq) #Blau's index of diversity: 1 - HHI, where HHI is the sum of the (squared) proportions of each category from total
  
  v6_prog_lang_diversity_members <- temp[complete.cases(temp$project_language),] %>% group_by(user_id, project_language) %>% summarise(n=n()) # count of projects per language per user
  v6_prog_lang_diversity_members <- v6_prog_lang_diversity_members %>% group_by(user_id) %>% summarise(n=n()) # count of different languages per user
  v6_prog_lang_diversity_members <- v6_prog_lang_diversity_members %>% mutate(p = n/sum(n)) %>% mutate(p_sq = p^2) #calculate proportion and proportion squared for HHI
  v6_prog_lang_diversity_members <- 1 - sum(v6_prog_lang_diversity_members$p_sq) #Blau's index of diversity: 1 - HHI, where HHI is the sum of the (squared) proportions of each category from total
  
  v7_max_member_tenure <-temp_user_only %>% mutate(user_age = difftime(as.POSIXct("2015-09-24 11:34:32", format = "%Y-%m-%d %H:%M:%S"), user_created_at, units = "weeks") / 4.3) # time from last day of data to user creation date in months (in weeks divided by 4.3)
  v7_max_member_tenure <- max(as.numeric(v7_max_member_tenure$user_age))
  
  v8_org_is_owner <- ifelse(temp_user_only[temp_user_only$user_is_owner=="yes",3]=="ORG", 1, 0) #1=if owner is ORG, 0 otherwise
  
  v8_prop_members_same_company <- temp_user_only[complete.cases(temp_user_only$user_company),] %>% group_by(user_company) %>% summarise(n = n())
  v8_prop_members_same_company <- ifelse(nrow(v8_prop_members_same_company)==0, NA, max(v8_prop_members_same_company$n) / sum(v8_prop_members_same_company$n)) #if there's no members with company diff from NA, put 0
  
  #dependent variables:
  dv_1_no_watchers <- subset(watchers, watchers$repo_id==list_focal_proj[[j]])
  dv_1_no_watchers <- nrow(dv_1_no_watchers)
  
  dv_2_no_forkers <- subset(projects_full, projects_full$forked_from==list_focal_proj[[j]])
  dv_2_no_forkers <- nrow(dv_2_no_forkers)
  
  data_projects[nrow(data_projects)+1, ] <- list(as.character(list_focal_proj[[j]]), as.numeric(v1_no_organisation_members), as.numeric(v2_geo_dispersion), as.numeric(v3_experience_variety),
                                                 as.numeric(v3_experience_diversity), as.numeric(v4_avg_team_workload), as.numeric(v5_no_users), as.numeric(v6_prog_lang_variety), 
                                                 as.numeric(v6_prog_lang_diversity_project), as.numeric(v6_prog_lang_diversity_members), as.numeric(v7_max_member_tenure), 
                                                 as.numeric(v8_org_is_owner), as.numeric(v8_prop_members_same_company), as.numeric(dv_1_no_watchers), as.numeric(dv_2_no_forkers))
  setTxtProgressBar(pb,j) #update progress bar
}

#Some descriptive data exploration:
data_projects_desc <- Hmisc::describe(data_projects)
data_projects_desc
#===End STEP 2 =================================================================================================================================================================================




#===STEP 3  (run hierarchical and k-means clustering) ==========================================================================================================================================
data_projects_cleaned <- data_projects[complete.cases(data_projects),] #remove NAs (essentially in v2_geo_dispersion and v8_prop_members_same_company due to missing location and company data)

#EXPLORE AND PREPARE DATA:--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Visual exploration
data_to_plot <- reshape2::melt(data_projects_cleaned[,-c(14:15)], id.vars=c("focal_proj_id")) #transform to long format
p <- ggplot(data_to_plot, aes(x = variable, y = value, color = variable)) +
  geom_boxplot(colour = "grey50", alpha = 0.66, fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), # this is to change the middle line in the boxplot from median to mean
               width = 0.75, size = 1, linetype = "solid", colour = "grey50") +
  geom_jitter(alpha = .3, width = .3) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
p

#Visual exploration again, this time with logs for v4 and v5, and scaling
data_projects_cleaned_norm <- data_projects_cleaned
data_projects_cleaned_norm$v4_avg_team_workload <- log(data_projects_cleaned_norm$v4_avg_team_workload+0.1) #log transformation due to extreme skewness (sum 0.1 to avoid 0s)
data_projects_cleaned_norm$v5_no_users <- log(data_projects_cleaned_norm$v5_no_users+0.1) #log transformation due to extreme skewness (sum 0.1 to avoid 0s)
data_projects_cleaned_norm[,2:13] <- scale(data_projects_cleaned_norm[,2:13]) #scale numeric cols (IVs only)
data_projects_cleaned_norm[,2:13] <- sapply(data_projects_cleaned_norm[,2:13], function(x) (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm=T))) #normalise [0-1] numeric cols (IVs only)
data_to_plot <- reshape2::melt(data_projects_cleaned_norm[,-c(14:15)], id.vars=c("focal_proj_id")) #transform to long format
p <- ggplot(data_to_plot, aes(x = variable, y = value, color = variable)) +
  geom_boxplot(colour = "grey50", alpha = 0.66, fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), # this is to change the middle line in the boxplot from median to mean
               width = 0.75, size = 1, linetype = "solid", colour = "grey50") +
  geom_jitter(alpha = .3, width = .3) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
p

#Result of exploration:
  # drop cols 2 & 12 as they are binary (or pseudo) and won't work well with clustering (several readings support this)
  # drop cols 4, 8, 9 as their other versions show a richer variance
data_def <- data_projects_cleaned_norm[,-c(2,4,8,9,12)]
row.names(data_def) <- data_def$focal_proj_id
data_def <- data_def[,-1]

#CLUSTERING:--------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_def.x <- as.matrix(data_def[,-c(8,9)])
d <- dist(data_def.x, method = "euclidean") #remove from data the proj_ids, and the two DVs
h <- hclust(d, method = "ward.D2") #after some reading on the package's description, forums and the paper "https://arxiv.org/pdf/1111.6285.pdf" ward.d turns out NOT to be the by-the-book implementation of Wards method, ward.d2 is.
plot(h) #visualise dendrogram

#how many clusters?:
fviz_nbclust(data_def.x, FUN = hcut, method = "wss") # elbow method (it seems to indicate 4)
fviz_nbclust(data_def.x, FUN = hcut, method = "silhouette") # average silhouette (2, but 3 and 4 are close)
#more detailed look into silhouette:
sil <- cluster::silhouette(cutree(h, k=4), d)
plot(sil, col = 1:4, border = NA, main = "Silhouette plot: hierarchical clustering")
# Objects with negative silhouette (to know which obs has been placed in a wrong cluster and what's its neighbouring cluster):
neg_sil_index <- which(sil[, 'sil_width'] < 0)
df_sil <- sil[neg_sil_index, , drop = FALSE]
  #Res: From silhouette analysis: 2 indicated many negative obs; 3 uneven clusters and many negatives; 4 sim avg silhouette than 2, even clusters and slighter less negative values than 2.
  #(cont) 5, 6 and 7 have less average silhouette and/or more uneven clusters and/or negatives. Best option seems to be 4.

#perform k-means using centroinds from hierarchical clustering:
# Reference: https://stat.ethz.ch/pipermail/r-help/2006-May/105328.html (Book: Venables and Ripley's Modern Applied Statistics with S (4th Ed))
initial <- tapply(data_def.x, list(rep(cutree(h, k=4), ncol(data_def.x)), col(data_def.x)), mean)
dimnames(initial) <- list(NULL, dimnames(data_def.x)[[2]])
km <- kmeans(data_def.x, initial)
#re-check silhouette to see if any improvement:
sil <- cluster::silhouette(km$cluster, d)
plot(sil, col = 1:4, border = NA, main = "Silhouette plot: k-means clustering") #improved considerably from hierarchical!

fviz_cluster(km, geom = "point", data = data_def.x) #Map of clusters using two first principal components automatically calculated by this function. Compared to the one for clusters 6, this was an improvement!

tmp <- data.frame(keyName=names(km$cluster), value=km$cluster, row.names=NULL)
colnames(tmp) <- c("focal_proj_id", "cluster")
data_def$focal_proj_id <- row.names(data_def)
data_def <- data_def[,c(10,1:9)]
data_def <- merge(data_def, tmp, by = "focal_proj_id")
data_def$cluster <- factor(data_def$cluster)
#write.csv(data_def, paste0("data_def_", Sys.Date(),".csv"))
#===End STEP 3 =================================================================================================================================================================================




#===STEP 5 (run MANOVA + additional DV) ========================================================================================================================================================
manova <- manova(cbind(dv_1_no_watchers, dv_2_no_forkers) ~ cluster, data = data_def)
summary(manova) #p<0.05 --> Different team configurations is associated with project innovation
summary.aov(manova) # It's actually no. watchers the DV related with the team composition clusters

#Introducing a truly measure of project novelty:
  #Project novelty = novelty of combination of words from project description, based on (1) Boudreau et al. (2016), "Looking across and looking beyond the knowledge frontier:", Mgt.Sci....
  #(2) Criscuolo et al (2017), "Evaluating novelty:", AMJ. 
find_word_combinations <- function(x) { #function to calculate frequencies of each pairwise combination of words in a vector of strings
  pr <- unlist(lapply(strsplit(x, ' '), function(i) combn(sort(i), 2, paste, collapse=' ')))
  tbl <- table(pr)
  d <- do.call(rbind.data.frame, strsplit(names(tbl), ' '))
  names(d) <- c('word1', 'word2')
  d$Freq <- tbl
  d
}

#find_word_combinations(x)-----------------------------------------------------------------------------------
tmp <- projects_sample
colnames(tmp)[1] <- "focal_proj_id"
tmp <- subset(tmp, tmp$focal_proj_id %in% data_def$focal_proj_id)
tmp$description <- gsub("[[:punct:] ]+"," ",tmp$description) # remove punctuations
tmp$description <- gsub("[^[:graph:]]", " ",tmp$description) #get rid of unvalid characters by removing all non graphical characters
tmp$description <- gsub(" +"," ",tmp$description) #remove more than two consecutive white spaces
tmp$description <- ifelse(tmp$description==" " | tmp$description=="", tmp$name, tmp$description) #for cases of empty descriptions ('' or ' '), copy the project name into description

#build and clean/tidy corpus that will be passed to the 'find_word_combinations' function:
my_corpus <- Corpus(VectorSource(tmp[,4]))
my_corpus <- tm_map(my_corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, content_transformer(removeNumbers))
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
my_corpus <- tm_map(my_corpus, removeWords, stopwords("spanish"))
my_corpus <- tm_map(my_corpus, removeWords, "a")
my_corpus <- tm_map(my_corpus, content_transformer(stripWhitespace))
cleaned_words_vector <- sapply(my_corpus, identity)

#call the function and store output in data frame:
#for cases with no. words = 1, repeat the word so that the function that comes after this runs without issues:
for (v in 1:length(cleaned_words_vector)) {
  if(sapply(strsplit(cleaned_words_vector[[v]], " "), length) < 2 | (sapply(strsplit(cleaned_words_vector[[v]], " "), length) == 2 & "" %in% strsplit(cleaned_words_vector[[173]], " ")[[1]])) { 
    cleaned_words_vector[[v]] <- paste(cleaned_words_vector[[v]], cleaned_words_vector[[v]]) #if there's only one word OR  there are 2 but one is actually empty "", repeat it
  }
  if(grepl("^\\s*$", cleaned_words_vector[[v]])) { cleaned_words_vector[[v]] <- paste(tmp[v,3], tmp[v,3]) } #if there are cases with just white spaces ("  ", "   "), bring project name (twice)
}
tmp$description_cleaned <- cleaned_words_vector #return all the cleaning work done to the data frame to use it afterwards
full_word_combo_table <- data.frame(find_word_combinations(cleaned_words_vector)) #call the function
full_word_combo_table$word1 <- as.character(full_word_combo_table$word1)
full_word_combo_table$word2 <- as.character(full_word_combo_table$word2)
full_word_combo_table$Freq <- as.numeric(full_word_combo_table$Freq)
full_word_combo_table <- full_word_combo_table[full_word_combo_table$word1 != "" & full_word_combo_table$word2 != "",] #remove rows with "" in either word1 or word2

#Loop through each project and find word combinations for each one, and compare to word combo table to create the novelty measure---------------------------------------------
tmp$avg_frequency <- NA
for (z in 1:nrow(tmp)) {
  #z <- 4
  indiv_word_combo_table <- data.frame(find_word_combinations(tmp[z,7]))
  indiv_word_combo_table$word1 <- as.character(indiv_word_combo_table$word1)
  indiv_word_combo_table$word2 <- as.character(indiv_word_combo_table$word2)
  indiv_word_combo_table$Freq <- as.numeric(indiv_word_combo_table$Freq)
  indiv_word_combo_table <- indiv_word_combo_table[indiv_word_combo_table$word1 != "" & indiv_word_combo_table$word2 != "",]
  accum <- 0
  for (yz in 1:nrow(indiv_word_combo_table)) {
    #yz <- 1
    accum <- accum + full_word_combo_table[full_word_combo_table$word1==indiv_word_combo_table[yz,1] & full_word_combo_table$word2==indiv_word_combo_table[yz,2],3] #accum + the freq number from the full table
  }
  accum <- accum / nrow(indiv_word_combo_table) #the avg of all the frequency counts
  tmp[z,8] <- accum
}
data_def <- merge(data_def, tmp[,c(1,8)], by = "focal_proj_id")
colnames(data_def)[12] <- "dv_3_avg_freq_words_desc"

#Non-parametric multivariate test statistics:--------------------------------------------------------------------------------------------------------------------------------
  #Note: MANOVA was the origial choice, but after visually checking for non-normality [i.e. ggqqplot(data_def$dv_1_no_watchers)], all three DVs are non-normal
data_def2 <- data_def
data_def2$cluster <- as.numeric(data_def2$cluster)
kruskal.test(dv_1_no_watchers ~ cluster, data = data_def2)
kruskal.test(dv_2_no_forkers ~ cluster, data = data_def2)
kruskal.test(dv_3_avg_freq_words_desc ~ cluster, data = data_def2)
#The actual multivariate test:
nonpartest(dv_1_no_watchers | dv_2_no_forkers | dv_3_avg_freq_words_desc ~ cluster, data = data_def2, permreps = 1000, plots = F)
#nonpartest(dv_1_no_watchers | dv_2_no_forkers | dv_3_avg_freq_words_desc ~ cluster, data = data_def2[sample(nrow(data_def2), 200),], permreps = 1000, plots = F)
# Results: Each of the four tests show extremely significant mean differences for the three DV across the 4 clusters, meaning that team composition influences project novelty. 
  # As a sort of robust test to control for the ~ large size of the sample, I've taken samples of 200 projs from the 4500 projs and re-ran the analysis. In most runs the p values hold.
  # It's with samples of around ~100 that I start to see half of the runs with non-significant p's, but this is significantly smaller than the actual sample
#End step 5 =================================================================================================================================================================================




# Step 6 (Visualization) =============================================================================================================================================================
#Some descriptives for the paper tables first:
output_1 <- data_def %>% 
  group_by(cluster) %>% 
  summarise(n=n())

output_2 <- data_def %>% 
  group_by(cluster) %>% 
  summarise(dv1_mean = mean(dv_1_no_watchers), dv1_med = median(dv_1_no_watchers), dv2_mean = mean(dv_2_no_forkers), dv2_med = median(dv_2_no_forkers), 
                                                         dv3_mean = mean(dv_3_avg_freq_words_desc), dv3_med <- median(dv_3_avg_freq_words_desc))

#6A: Map of clusters using two first principal components:
fviz_cluster(km, data = data_def.x, geom = "point", palette = "Dark2", ggtheme = theme_light(base_family = "sans", base_size = 22), ellipse.alpha = 0, 
             show.clust.cent = F, shape = 16, pointsize = 2.5, main = "")

#6B: Heatmap of project scores across variables grouped by clusters:
data_def_heatmap <- data_def
row.names(data_def_heatmap) <- data_def_heatmap$focal_proj_id
data_def_heatmap <- data_def_heatmap[order(data_def_heatmap$cluster), -1]
my_group <- as.numeric(data_def_heatmap$cluster)
colSide <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")[my_group] #these are specific colours from RColorBrewer, palette Dark2
data_def_heatmap_plot <- as.matrix(data_def_heatmap[,-c(8:11)])

dev.off()
heatmap.2(data_def_heatmap_plot, dendrogram = "none", margin = c(17,18), Rowv = NULL, Colv="Rowv", trace="none", scale = "none", offsetRow = .1, offsetCol = .1,
          breaks=c(-1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), colsep=c(1:7), col=viridis(11), RowSideColors = colSide, labRow = NA, labCol = NA, xlab = "",ylab = "", key = F)
gradient.rect(0.96,0.15,0.97,0.55, nslices = 11, border=F, gradient="y", col = viridis(11)) #legend colours
text(x=rep(0.975,11),y=seq(0.17,0.53,length.out = 11), adj = 0, cex = 0.85, labels=c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"), family = "sans") #legend values
text(seq(0.16,0.88,length.out = 7), -.06, srt = 45, adj = 1, cex = 1.2, labels = c("Geographic dispersion", "Experience diversity", "Average workload", "Size (no.members)", "Skills diversity", "Maximum tenure", "Centralized coordination"), xpd = TRUE, family = "sans") #var labels
text(x=rep(0.07,4),y=c(0.01,0.25,0.49,0.71), adj = 0, cex = 1.2, srt = 90, col = "white", labels=c("Cluster 4","Cluster 3","Cluster 2","Cluster 1"), family = "sans") #clusters labels
#End step 6 =================================================================================================================================================================================

lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect) # close all opened connections to db

