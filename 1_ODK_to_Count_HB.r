### HB_to_counts fonction convert data from interviews of human behavior (see MOIROUX et al. 2014) to counts of individuals in each compartiments (indoor, outdoors, under nets)
## works with several places (villages) and age classes but for only one survey
## compute stats for the overall population based on the recorded number of peoples censored in the household

# from the output of the ODK app :
# spaces in col 'rcpnomprenomrepdt' should be replaced by underscore '_' previously or removed using the read.table function
# see file 'Hum_Behav_PANIC.txt' for the correct structure



#' Count individuals in different states (indoors, outdoors, sleeping) for both user and non-users of nets at each hour (from 0h to 23h)
#'
#' `summarise_HB` returns hourly counts of individual humans in 3 compartiments (indoor, outdoors, under nets) from individual data (hours) or behavior.
#' Used in `HB_to_counts` function
#'
#' @param data a data frame of individual behavior data with the following fields:
#' `dormirssmoust`, did the individual used a mosquito net ? (value = "oui" or "non"),
#' `hintmaison`, the hour the individual went indoors in the evening, in HH:MM:SS character format (for ex. "20:30:00")
#' `hsortiemaison`, the hour the individual went outdoors in the morning
#' `hcoucher`, the hour the individual went to sleep
#' `hlever`, the hour the individual wake up
#' @keywords
#' @return a dataframe with 6 columns of counts (N_Ind, N_User, N_In, N_Sleep, N_Mild, N_User_In) and 24 lines (one per hours)
#' @export
#' @examples
#' subset_hb <- subset(ODK_HB_R,rcpcodevillage == "DOG")
#' summarise_HB(subset_hb)

summarise_HB <- function(data){
	require(lubridate)

	# Convert hours in HH:MM:SS format to decimal days (0 to 1 with 0h = 0, 12h = 0.5 and 24h = 1)
	data$hintmaison <- as.numeric(as.duration(hms(data$hintmaison)), "days")
	data$hcoucher <- as.numeric(as.duration(hms(data$hcoucher)), "days")
	data$hlever <- as.numeric(as.duration(hms(data$hlever)), "days")
	data$hsortiemaison <- as.numeric(as.duration(hms(data$hsortiemaison)), "days")

	# change the referential of hours (from 0h to 12h, i.e, in the new referential: 12h = 0 and 0h = 0.5)
	data$HIn <- c()					# new column for hour of going indoor
	data$HCou <- c()				# new column for hour of going to bed
	data$HLev <- c()				# new column for hour of waking up
	data$HExt <- c()				# new column for hour of going outdoor

	for (i in 1:nrow(data)) {		# boucle pour la conversion des horaires dans le nouveau referentiel
		if (data$hintmaison[i] > 0.5) {data$HIn[i] <- data$hintmaison[i] - 0.5} else {data$HIn[i] <- data$hintmaison[i] + 0.5}
		if (data$hcoucher[i] > 0.5) {data$HCou[i] <- data$hcoucher[i] - 0.5} else {data$HCou[i] <- data$hcoucher[i] + 0.5}
		if (data$hlever[i] > 0.5) {data$HLev[i] <- data$hlever[i] - 0.5} else {data$HLev[i] <- data$hlever[i] + 0.5}
		if (data$hsortiemaison[i] > 0.5) {data$HExt[i] <- data$hsortiemaison[i] - 0.5} else {data$HExt[i] <- data$hsortiemaison[i] + 0.5}
	}

	N_Ind <- rep(nrow(data), 24)									# vector of hourly Number of individuals
	N_User <-  rep(length(which(data$dormirssmoust == "oui" )), 24)	# vector of hourly Number of individuals having used an LLIN
	N_In <- c()															# vector of hourly number of people being indoor
	N_Sleep <- c()														# vector of hourly number of people being asleep
	N_Mild <- c()														# vector of hourly number of people being asleep AND under an LLIN
	N_User_In <- c()													# vector of hourly number of LLIN user being indoor

	for (i in 0:23){					# 5 correspond à 17h dans le nouveau ref. et 20 correspond à 8h
		N_In[i+1] <- length(which(data$HIn < (i+1)/24 & data$HExt > (i+1)/24))   # pour être à l'intérieur, il faut être entré avant la fin de la tranche horaire (i+1) et sorti après
		N_Sleep[i+1] <- length(which(data$HCou < (i+1)/24 & data$HLev > (i+1)/24))
		N_Mild[i+1] <- length(which(data$HCou < (i+1)/24 & data$HLev > (i+1)/24 & data$dormirssmoust == "oui"))
		N_User_In[i+1] <- length(which(data$HIn < (i+1)/24 & data$HExt > (i+1)/24 & data$dormirssmoust == "oui" ))
	}
	ODK_HB_R_extr_Count_HB <- as.data.frame(cbind(N_Ind, N_User, N_In, N_Sleep, N_Mild, N_User_In))
	return(ODK_HB_R_extr_Count_HB)
}

#' Execute function `summarise_HB` on subsets (age classes and village) of a dataframe with data from interviews of 1 survey of human behavior (see MOIROUX et al. 2014)
#' Also weight count for all age classes according to household census to compute predicted count in the whole population
#' Also counts number of user outdoors and user indoors but not under net
#'
#' `HB_to_counts` returns hourly counts of individual humans in each compartiments (indoor, outdoors, under nets) from a data frame specifying (see MOIROUX et al. 2014)
#' @param data a data frame of individual behavior data with the following fields with these seven first column (order doesnt matter):
#' `rcpcodevillage`    village code (factor)
#' `rcpdateenquete` survey date
#' `rcpcodemenage`    household code
#' `rcpnomprenomrepdt` household respondent
#' `rcpnbrenfant1`     nb of children (0-6y) in the household
#' `rcpnbrenfant2`    nb of children (6-18y) in the household
#' `rcpnbradulte`			nb of adults in the household
#' followed by these columns (order doesnt matter):
#' `rcptrcheage` individual age classes (factor)
#' `sexe` sexe of the individual (f or m)
#' `age` age in years of the individual
#' `dormirssmoust`, did the individual used a mosquito net ? (value = "oui" or "non"),
#' `hintmaison`, the hour the individual went indoors in the evening, in HH:MM:SS character format (for ex. "20:30:00")
#' `hsortiemaison`, the hour the individual went outdoors in the morning
#' `hcoucher`, the hour the individual went to sleep
#' `hlever`, the hour the individual wake up
#' @keywords
#' @return a data frame of counts of individuals in various compartment per hours according to the age classes and the village (to be used for bite exposure calculation)
#' @export
#' @examples
#' HB_to_counts(ODK_HB_R)

HB_to_counts <- function(data = data){
	require(tidyverse)
	# extraire les proportion de chaque classe d'âge par villages
	ODK_HB_R_Menage <- unique(data[,1:7])   # création d'une table "Ménages" contenant une ligne par ménage et ses attributs

	# faire des statistiques (somme d'individus dans chaque classe d'âge) par village
	ODK_HB_R_Village <- ODK_HB_R_Menage %>% group_by(rcpcodevillage) %>% summarise_at(c("rcpnbrenfant1","rcpnbrenfant2","rcpnbradulte"),sum)
	colnames(ODK_HB_R_Village)[2:4] <- c("Total_0_5", "Total_6_17", "Total_Ad")

	ODK_HB_R_Village$sum <- ODK_HB_R_Village$Total_0_5 + ODK_HB_R_Village$Total_6_17 + ODK_HB_R_Village$Total_Ad	# nombre total d'individu dans les ménages recensés
	ODK_HB_R_Village$P_0_5 <- ODK_HB_R_Village$Total_0_5 / ODK_HB_R_Village$sum 									# proportion d'ind. de chaque classe d'âge
	ODK_HB_R_Village$P_6_17 <- ODK_HB_R_Village$Total_6_17 / ODK_HB_R_Village$sum
	ODK_HB_R_Village$P_Ad <- ODK_HB_R_Village$Total_Ad / ODK_HB_R_Village$sum


	# data extraction, per villages and per ages
	Vil <- levels(data$rcpcodevillage)
	Age <- levels(data$rcptrcheage)

	for (v in Vil) {
		ODK_HB_R_Sub<-subset(data, rcpcodevillage == v)
		Count_HB_all <- cbind(summarise_HB(ODK_HB_R_Sub), rep(v,24), rep("all",24))
		colnames(Count_HB_all)[7:8]<- c("Vil","Age")
		if (v == Vil[1]) {Count_HB <- Count_HB_all} else {Count_HB <- rbind(Count_HB, Count_HB_all)}

		for (a in Age) {
			ODK_HB_R_Sub<-subset(data, rcpcodevillage == v & rcptrcheage == a)
			Count_HB_age <- cbind(summarise_HB(ODK_HB_R_Sub), rep(v,24), rep(a,24))
			colnames(Count_HB_age)[7:8]<- c("Vil","Age")
			Count_HB <- rbind(Count_HB, Count_HB_age)
		}
	}

	Count_HB$t <- rep(1:24,length(Vil)*(length(Age)+1))				# Ajout d'une colonne temps (tranche horaire)


	##### Predicted number of individual in each age classes at the village scale according to the weight of each age classe (in surveyed housholds).

	for (v in Vil) {# pour chaque village
		for (t in 1:24){# à chaque heure
			r1 <- which(Count_HB$t == t & Count_HB$Age == Age[1] & Count_HB$Vil == v) # recherche l'index de la ligne dans Count_HB correspondant à Age[1] : 18ansetplus
			r2 <- which(Count_HB$t == t & Count_HB$Age == Age[2] & Count_HB$Vil == v)	# de6ansa17ans
			r3 <- which(Count_HB$t == t & Count_HB$Age == Age[3] & Count_HB$Vil == v) #	moinsde6ans

			tab1 <- t(Count_HB[c(r1,r2,r3), 1:6])			# groupe les résultats dans une table avec chaque age en colonne et "N_Ind","N_User","N_In","N_Sleep","N_Mild" et "N_User_In" en ligne
			tab2 <- t(ODK_HB_R_Village[which(ODK_HB_R_Village$rcpcodevillage == v),6:8]) # récupère dans la table ODK_HB_R_Village les proportion de chaque tranche d'age dans le village

			pop <- t(tab1 %*% tab2)    # weigthed number of individuals
			pop <- cbind(pop, Count_HB[r1,7:9])

			if (t==1 & v == Vil[1]){Count_HB_pop <- pop} else {Count_HB_pop <- rbind(Count_HB_pop,pop)}
		}
	}

	Count_HB_pop$Age <- rep("pop", 24*length(Vil))


	# buiding the final tab
	Count_HB <- bind_rows(Count_HB,Count_HB_pop)
	colnames(Count_HB)[5] <- "U_nets"									# N people under an LLIN
	Count_HB$U_outdoors <- Count_HB$N_User - Count_HB$N_User_In					# N user outdoor
	Count_HB$U_indoors <- Count_HB$N_User - Count_HB$U_outdoors - Count_HB$U_nets		# N user indoor but not under an LLIN
	# convert t into the good referential
	Count_HB$t <- Count_HB$t - 1
	#'Count_HB' is used in the code 'Exposure_calc_graph' that calculate exposure value and produce graph of exposure to bite
	return(Count_HB)
}
