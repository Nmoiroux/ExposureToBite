###### Exposure calculations (for graphical purposes)

#' Calculate hourly exposure to bite indices based on human and vector behaviors data
#' the generated dataframe can be used with functions `plot_exposure`, ` plot_behaviors` and `plot_behavior_H`for plotting purpose
#' and with `daily_exp_stat` and `simul_IC_exp` to calculate daily statistics of exposure
#'
#' @param Count_HB a dataframe resulting from function `HB_to_counts` with an additional column specifying the survey number ("Enq", integer)
#' @param COunt_ento a dataframe resulting from function `Entomo_PHP_to_counts`
#' @param p the personal protection provided by an LLIN (default = 0.92 for Permanet 2 according to Corbel et al. 2010)
#' @keywords
#' @return a dataframe joining Count_HB and COunt_ento plus the following calculmated fields:
#' `pui`` proportion of LLIN user being indoor (rounded to deal with small neagtive value of U_outdoors)
#' `puo` proportion of LLIN user being outdoor
#' `pup` proportion of LLIN user being to bed and protected by a LLIN
#' `eui` user exposuExposure indoor
#' `euo` user exposuExposure outdoor
#' `eun` user exposuExposure under an LLIN
#' `eup` user exposure prevented by LLIN
#' @export
#' @examples
#' Count_HB <- HB_to_counts(ODK_HB_R)
#' Count_HB$Enq <- 1
#' Data_Entomo <- Entomo_PHP_to_counts(Entomo_PHP)
#' exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#' 
exposure_dat <- function(Count_HB, Count_ento, p = 0.92){
	require(dplyr)
	
	Exposure <- left_join(Count_HB, Count_ento, by=c('Vil','Enq','t'))		# join the table Count_HB and Count_ento based on Vil, Enq and t
	
	Exposure$pui <- round(Exposure$U_indoors/Exposure$N_User, 10)		# proportion of LLIN user being indoor (rounded to deal with small neagtive value of U_outdoors)
	Exposure$puo <- round(Exposure$U_outdoors/Exposure$N_User, 10)		# proportion of LLIN user being outdoor
	Exposure$pup <- round(Exposure$U_nets/Exposure$N_User, 10)			# proportion of LLIN user being to bed and protected by a LLIN
	
	Exposure$eui<-Exposure$Ni/Exposure$d*Exposure$pui			# user exposuExposure indoor (Bi,t*(It-St))
	Exposure$euo<-Exposure$No/Exposure$d*Exposure$puo			# user exposuExposure outdoor (Bo,t*(1-It))
	Exposure$eun<-Exposure$Ni/Exposure$d*Exposure$pup*(1-p)		# user exposuExposure under an LLIN (Bi,t*St*(1-P))
	Exposure$eup<-Exposure$Ni/Exposure$d*Exposure$pup*p			# user exposure prevented by LLIN (Bi,t*St*P)
	
	return(Exposure)
}	


#### making graph of exposure to bite

#' making multi panel plot of exposure to bite (one panel per entomological survey for one village and one age classes)
#'
#' @param Exposure a dataframe obtained by the use of function `exposure_dat`
#' @param vil one village code (string) from Exposure$Vil
#' @param age one age classes (string) from Exposure$Age
#'
#' @return a ggplot object 
#' @export
#'
#' @examples
#' Exposure <- exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#' plot_exposure(Exposure, vil= "DOG", age = "pop")
#' 
plot_exposure <- function(Exposure, vil, age = "pop"){
	
	require(tidyr)
	require(dplyr)
	require(ggplot2)
	sub_Exposure_graph <- subset(Exposure, Vil == vil & Age == age)# & Enq == enq)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour
	
	###### Making Graphs of exposure
	data_graph <- sub_Exposure_graph %>% gather("exposure","value",c("eui", "euo", "eun", "eup"))
	data_graph$exposure <- as.factor(data_graph$exposure)
	
	# reorder levels for graph purpose
	data_graph$exposure <- factor(data_graph$exposure, levels=c("eup", "eun", "eui", "euo"))
	
	# plot exposure
	plot <- ggplot(data_graph, aes(x=t2, y=value, fill=exposure)) +
		geom_area(colour="black", size=.2, alpha=.4) +
		scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$exposure))) +
		facet_wrap(~Enq) +
		scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))
	### ADD Sunset and sunrise (To do)
	return(plot)
}

###### making graph of human and vector behaviors

plot_behaviors <- function(Exposure, vil, age){
	
	sub_Exposure_graph <- subset(Exposure, Vil == vil & Age == age)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour
	
	sub_Exposure_graph$pNet <- sub_Exposure_graph$U_nets / sub_Exposure_graph$N_Ind
	sub_Exposure_graph$pIn <- sub_Exposure_graph$N_In / sub_Exposure_graph$N_Ind - (sub_Exposure_graph$pNet)
	maImax <- max(with(sub_Exposure_graph,Ni/d), na.rm=T)
	maOmax <- max(with(sub_Exposure_graph,No/d), na.rm=T)
	mamax <- max(maImax,maOmax)
	sub_Exposure_graph$ma_in <- with(sub_Exposure_graph,Ni/d)/mamax
	sub_Exposure_graph$ma_ou <- with(sub_Exposure_graph,No/d)/mamax
	
	data_graph <- sub_Exposure_graph %>% gather("people","value",33:34)
	data_graph$people <- as.factor(data_graph$people)
	
	
	ggplot(data_graph, aes(x=t2, y=value, fill=people)) +
		geom_area(colour="grey", size=.3, alpha=.4) +
		scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$people))) +
		facet_wrap(~Enq) +
		scale_y_continuous(sec.axis = sec_axis(~.*mamax, name = "hourly biting rate (bites/human/hour)"), name = "% of people indoor or under net") +
		geom_line(aes(y = ma_in, linetype ="indoor"), size=0.6) +
		geom_line(aes(y = ma_ou, linetype ="outdoor"), size=0.6) +
		scale_linetype_manual(values = c(
			'indoor' = 1,
			'outdoor' = 4)) +
		labs(linetype = 'anopheles') +
		scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))

}

plot_behavior_H <- function(Exposure, vil = c("DOG"), enq = 1, nc = 6){
	
	sub_Exposure_graph <- subset(Exposure, Vil %in% vil & !(Age %in% c("pop","all")) & Enq == enq)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour
	
	sub_Exposure_graph$pNet <- sub_Exposure_graph$U_nets / sub_Exposure_graph$N_Ind
	sub_Exposure_graph$pIn <- sub_Exposure_graph$N_In / sub_Exposure_graph$N_Ind - (sub_Exposure_graph$pNet)

	data_graph <- sub_Exposure_graph %>% gather("people","value",33:34)
	data_graph$people <- as.factor(data_graph$people)
	
	
	ggplot(data_graph, aes(x=t2, y=value, fill=people)) +
		geom_area(colour="grey", size=.3, alpha=.4) +
		scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$people))) +
		facet_wrap(~Vil+Age, ncol=nc) +
		scale_y_continuous(name = "% of people indoor or under net") +
		scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))
	
}


###### calculation of Exposure at the night level and other interesting indicators

daily_exp_stat <- function(Exposure, tE = 10, tM = 18){
	require(dplyr)
	expE <- Exposure %>% filter(t < 10)										# select row only for hour before 22h (for evening exposure)
	expM <- Exposure %>% filter(t >= 18)									# select row only for hour after 6h (for morning exposure)
	
	# sum hourly exposure (total, evening and morning)
	sumExp <- Exposure %>% group_by(Vil, Enq, Age) %>% summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T) # daily sums
	names(sumExp)[-c(1:3)] <- paste0("S",names(sumExp)[-c(1:3)])				# rename variables
	sumExpE <- expE %>% group_by(Vil, Enq, Age) %>% summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T)
	names(sumExpE)[-c(1:3)] <- paste0("S",names(sumExpE)[-c(1:3)],"E")				# rename variables
	sumExpM <- expM %>% group_by(Vil, Enq, Age) %>% summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T)
	names(sumExpM)[-c(1:3)] <- paste0("S",names(sumExpM)[-c(1:3)],"M")				# rename variables
	sumExp <- bind_cols(sumExp,sumExpE[,-c(1:3)],sumExpM[,-c(1:3)])								# groups all data in one table
	
	sumExp$PeuE <- 	with(sumExp,(SeuiE+SeuoE+SeunE)/(Seui+Seuo+Seun))						# Proportion of exposure in the Evening for users
	sumExp$PenuE <- with(sumExp,(SeuiE+SeuoE+SeunE+SeupE)/(Seui+Seuo+Seun+Seup))			# Proportion of exposure in the Evening for non users
	sumExp$PeuM <- with(sumExp,(SeuiM+SeuoM+SeunM)/(Seui+Seuo+Seun))						# Proportion of exposure in the Morning for users
	sumExp$PenuM <- with(sumExp,(SeuiM+SeuoM+SeunM+SeupM)/(Seui+Seuo+Seun+Seup))			# Proportion of exposure in the Morning for non users
	sumExp$Peui <- with(sumExp,(Seui+Seun)/(Seui+Seun+Seuo))								# Proportion of exposure indoor for user
	sumExp$Penui <- with(sumExp,(Seui+Seun+Seup)/(Seui+Seun+Seuo+Seup))						# Proportion of exposure indoor for non user
	sumExp$Eff <- with(sumExp,1-((Seui+Seun+Seuo)/(Seui+Seun+Seuo+Seup)))					# True protective efficacy (P*)
	
	return(sumExp)
}

