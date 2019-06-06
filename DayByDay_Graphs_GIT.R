###############################################
## Parliaments Day by Day: Graphs for Paper ##
###############################################

# Note: This script serves to create the graphs that are presented in the article
# Parliaments Day by Day: The PCC Core Data. 

# For the script that generates the data frames released released in conjunction
# with the article, see latest file of DFs_Generation_[year]-[month-[day]_[hour][minute]_OH


###############
# Preparation #
###############

# change the language and date formatting to English if it is not already
	Sys.setenv(LANG = "EN")
	Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
	Sys.getlocale(category = "LC_ALL")


# set working directory
	#setwd("C:/Users/freche/Dropbox/Data_Paper_DFs")
	#setwd("C:/Users/huwylero/Basel Powi Dropbox/Data_Paper_DFs")
	#setwd("E:/Basel Powi Dropbox/Data_Paper_DFs")
	setwd("C:/Users/turnerzw/Basel Powi Dropbox/Data_Paper_DFs")
	
	detach("package:googleVis", unload=TRUE)
	install.packages("googleVis")

# load libraries
	library(ggplot2)
	library(doParallel)
	library(stringr)
	library(gridExtra)
	library(ggpubr)
	library(googleVis) # for Sankey Diagram (https://databreadandbutter.wordpress.com/2017/09/25/erster-blogbeitrag/)
	library(reshape2) #to reshape data
	library(sqldf)
	library(plotly)
	library(grDevices)

#############
# Load data #
#############

# Goal: Load the latest version of the Rdata file "Day-By-Day_Paper_DFs_..."

	# 1) List all files in the directory
		FILES <- data.frame(list.files(path = "./dfs_for_release/"))
		colnames(FILES)[match("list.files.path......dfs_for_release...",colnames(FILES))] <- "file_names"

	# 2) Extract the dates and times from the file names
		FILES$rawdate <- as.character(str_extract_all(FILES$file_names, "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}"))

	# 3) Order the raw dates with the latest file on the top of the list
		FILES <- FILES[order(FILES$rawdate, decreasing=TRUE),]
		FILES<-FILES[!(FILES$rawdate=="character(0)"),] # Get rid of empty rows

	# 4) Complete file to import with file path
		# filetoimport <- as.character(FILES$file_names[1])
		filetoimport <- paste("./dfs_for_release/", as.character(FILES$file_names[1]), sep = "")

	# 5) Load all 12 data frames
		load(filetoimport)

	# 6) Remove files used for loading:
		rm(availablefiles, filetoimport, FILES)

	######### inspect these files now they have been loaded
		head(PARLDAILY) # so this already contains Oliver his aggregates.
		
		
		# Tomas: I am going to play with PART aggregated data a bit, so some inspections here
			head(PARTDAILY)
			
			PARLDAILY[which(PARLDAILY$day == as.Date("1995-12-04",origin="1970-01-01")),]
			PARTDAILY[which(PARTDAILY$day == as.Date("1995-12-04",origin="1970-01-01")),]
			
			# alright, lets check out what the seat distribution looks like
			table(is.na(PARTDAILY$seats))
			table(PARTDAILY$seats)
			hist(PARTDAILY$seats)
			# so, where do all these 'single seat' parties come from?
			table(PARTDAILY[which(PARTDAILY$seats == 1),]$party_id) # for the cases I know this looks legitimate? 
			# Although. PvdA is here as well?
			PARTDAILY[which(PARTDAILY$seats == 1 & PARTDAILY$party_id == "NL_PVV_NT"),] # this is one person in 1945, so not relevant over our general observation period
			
# also get some of the general PCC data-frames in

		# import and inspect PARL and get the election date into the r-format
		PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
		summary(PARL)
		names(PARL)
		PARL$election_date_asdate <- as.Date(as.character(PARL$election_date),format=c("%d%b%Y"))
		PARL$leg_period_start_asdate <- as.Date(as.character(PARL$leg_period_start),format=c("%d%b%Y"))
		PARL$leg_period_end_asdate <- as.Date(as.character(PARL$leg_period_end),format=c("%d%b%Y"))
		head(PARL)
		
		# and reduce to only the national parliament to avoid mistakes
		PARL <- PARL[which(PARL$level == "NT"),]

# and the % of women in parliament data from IPU / worldbank

	# "Data Source","World Development Indicators" / "Last Updated Date","2019-04-24",
	# resources here (https://data.worldbank.org/indicator/SG.GEN.PARL.ZS?locations=DE-NL-CH) say " General cut off date is end-December."
	
	# Import
		IPU = read.csv("API_SG.GEN.PARL.ZS_DS2_en_csv_v2_10576742.csv", header = TRUE, sep = ",")
		head(IPU)
		table(IPU$Country.Code)
		IPU <- IPU[which(IPU$Country.Code == "CHE"|IPU$Country.Code == "DEU"|IPU$Country.Code == "NLD"),]
		head(IPU)
	
	#lets melt this data
		TEMP <- melt(IPU)
		IPU_M <- cbind.data.frame(TEMP$Country.Code,TEMP$variable,TEMP$value)
		colnames(IPU_M) <- c("country","year","value")
		head(IPU_M)
		
	# and get a proper time-stamp in
		
		# a function that I will use for this
			substrRight <- function(x, n)
				{
					substr(x, nchar(x)-n+1, nchar(x))
				}
	
		# putting it all together
			IPU_M$rformateddate <- as.Date(paste(substrRight(as.character(IPU_M$year),4),"-12-31",sep=""),origin="1970-01-01")
		
		# transforming the percentage to scale we use
			IPU_M$propwomen <- IPU_M$value / 100
		
		# and country specif datasets
		IPU_CH <- IPU_M[which(IPU_M$country == "CHE"),]
		IPU_DE <- IPU_M[which(IPU_M$country == "DEU"),]
		IPU_NL <- IPU_M[which(IPU_M$country == "NLD"),]

##########
# Graphs #
##########

###############################################################################
# Gender Daily #
###############################################################################

	# merge the election date in (we need this below in the graphs)
	
		PARLDAILY$parliament_id <- as.character(PARLDAILY$parliament_id)
		PARLDAILY <- sqldf("SELECT PARLDAILY.*, PARL.election_date_asdate, PARL.assembly_abb
						   FROM PARLDAILY LEFT JOIN PARL
						   WHERE PARLDAILY.parliament_id = PARL.parliament_id
						  ")

#Split parliament id of Parldaily into components # #note to Elena/Oliver, this bit needs cleaning up / comments it is rather unclear at the moment what is exactly happening here

	ParlNew <- strsplit(as.character(PARLDAILY$parliament_id), "_")
#	do.call(rbind, ParlNew)
	PARLDAILY2 <- data.frame(PARLDAILY, do.call(rbind, ParlNew))
	ParlNew2 <- strsplit(as.character(PARLDAILY2$X2), "-")
#	do.call(rbind, ParlNew2)
	PARLDAILY_wide <- data.frame(PARLDAILY2, do.call(rbind, ParlNew2))
	colnames(PARLDAILY_wide)[colnames(PARLDAILY_wide)=="X1"] <- "country"
	colnames(PARLDAILY_wide)[colnames(PARLDAILY_wide)=="X3"] <- "year"
	colnames(PARLDAILY_wide)[colnames(PARLDAILY_wide)=="X2.1"] <- "parl"

#split Parldayly into country / house (for CH) based dfs
	PARLDAILY_countries <- split(PARLDAILY_wide, PARLDAILY_wide$country)
	PARLDAILY_CH<-data.frame(PARLDAILY_countries$CH)
	dfCH <- split(PARLDAILY_CH, PARLDAILY_CH$parl)
	
	PARLDAILY_CHSR <-data.frame(dfCH$SR)
	PARLDAILY_CHNR <-data.frame(dfCH$NR)
	PARLDAILY_DE <-data.frame(PARLDAILY_countries$DE)
	PARLDAILY_NL <-data.frame(PARLDAILY_countries$NL)
	
	head(PARLDAILY_DE) 
	
# some country specific inspections

	# time range?
	range(PARLDAILY_CHSR$day)
	range(PARLDAILY_CHNR$day)
	range(PARLDAILY_DE$day)
	range(PARLDAILY_NL$day)
	
	# seat numbers?
	table(PARLDAILY_CHNR$seats)
	table(PARLDAILY_DE$seats)
	table(PARLDAILY_NL$seats)
	
	
	ggplot(NULL) +  geom_line(data=PARLDAILY_CHNR, aes(x=day, y=seats))
	ggplot(NULL) +  geom_line(data=PARLDAILY_DE, aes(x=day, y=seats))
	ggplot(NULL) +  geom_line(data=PARLDAILY_NL, aes(x=day, y=seats))


	# you have also checked missingness on the gender data in POLI for Dutch MPS in other scripts ('control' and oliver' script.. this all looks good).
	
	# so according to the ipu data on the 15th of March 2017 there should be 54 women in the Dutch parliament and 150 members
	PARLDAILY_NL[which(PARLDAILY_NL$day == as.Date("2017-03-15",origin="1970-01-01")),]
	PARLDAILY_NL[which(PARLDAILY_NL$day == as.Date("2017-03-15",origin="1970-01-01")),]$gender * 150 # occurding to our data there where 58.
	
	
	
	## #discuss with Oliver: any idea why the time-ranges might differ between countries. Shall we make this consistent (for the comparability of the graphs) ##

## so Elena did not yet implement to script to get the proper vertical lines

#now do 3 graphs, using ggplot, one for each Parliament
	
	# this data is needed to get the vertical lines in
		UNI_CHSR <- as.data.frame(unique(PARLDAILY_CHSR$election_date_asdate))
		colnames(UNI_DE) <- "election_date_asdate"	
		
		UNI_CHNR <- as.data.frame(unique(PARLDAILY_CHNR$election_date_asdate))
		colnames(UNI_CHNR) <- "election_date_asdate"	
		
		UNI_DE <- as.data.frame(unique(PARLDAILY_DE$election_date_asdate))
		colnames(UNI_DE) <- "election_date_asdate"	
		
		UNI_NL <- as.data.frame(unique(PARLDAILY_NL$election_date_asdate))
		colnames(UNI_NL) <- "election_date_asdate"	
	
	# these are the 'matching vectors' from the IPU data
		min(which(names(IPU) == "X1960"))
		max(which(names(IPU) == "X1960"))

	
	# some vectors with ranges e.t.c. that can be used in all the graphs, done here centrally to force consistency between the graphs
	yname <- c("% Women")
	ybreaks <- c(0,0.1,0.2,0.3,0.4,0.5)
	ylabels <- c(0,10,20,30,40,50)
	yrange <- c(0,0.5)
		
	xrange <- c(as.Date("1950-01-01",origin="1970-01-01"),as.Date("2016-12-31",origin="1970-01-01"))
	
	# the breaks and labels depend on when the elections are
		xbreaks_CHNR <- UNI_CHNR$election_date_asdate
		xlabels_CHNR <- substr(as.character(UNI_CHNR$election_date_asdate),0,4)
		
		xbreaks_DE <- UNI_DE$election_date_asdate
		xlabels_DE <- substr(as.character(UNI_DE$election_date_asdate),0,4)
		
		xbreaks_NL <- UNI_NL$election_date_asdate
		xlabels_NL <- substr(as.character(UNI_NL$election_date_asdate),0,4)
	
	#CH
	# genderdaily_CH <-
		ggplot(NULL) +
		  geom_line(data=PARLDAILY_CHNR, aes(x=day, y=gender)) +
		  geom_point(data=IPU_CH, aes(x=rformateddate, y=propwomen),color="green",size=2) +
		  scale_y_continuous(name=yname,breaks=ybreaks,labels=ylabels,limits=yrange) +
		  scale_x_date(name="Swiss Nationalrat Day by Day",breaks=xbreaks_CHNR,labels=xlabels_CHNR,limits=xrange) +
		  geom_vline(aes(xintercept=UNI_CHNR$election_date_asdate), linetype=4, colour="black") +
		  theme_grey(base_size = 15) +
		  theme(axis.text.x = element_text(angle = 45, hjust = 1))

	
	#DE
	#genderdaily_DE <- 
		ggplot(NULL) +
		  geom_line(data=PARLDAILY_DE, aes(x=day, y=gender)) +
		  geom_point(data=IPU_DE, aes(x=rformateddate, y=propwomen),color="green",size=2) +
		  scale_y_continuous(name=yname,breaks=ybreaks,labels=ylabels,limits=yrange) +
		  scale_x_date(name="German Bundestag Day by Day",breaks=xbreaks_DE,labels=xlabels_DE,limits=xrange) +
		  geom_vline(aes(xintercept=UNI_DE$election_date_asdate), linetype=4, colour="black") +
		  theme_grey(base_size = 15) +
		  theme(axis.text.x = element_text(angle = 45, hjust = 1))
	  
	#NL
	# genderdaily_NL 
		ggplot(NULL) +
		  geom_line(data=PARLDAILY_NL, aes(x=day, y=gender)) +
		  geom_point(data=IPU_NL, aes(x=rformateddate, y=propwomen),color="green") +
		  scale_y_continuous(name=yname,breaks=ybreaks,labels=ylabels,limits=yrange) +
		  scale_x_date(name="Dutch Tweede Kamer Day by Day",breaks=xbreaks_NL,labels=xlabels_NL,limits=xrange) +
		  geom_vline(aes(xintercept=UNI_NL$election_date_asdate), linetype=4, colour="black") +
		  theme_grey(base_size = 15) +
		  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#put all together and save

	#ggarrange(genderdaily_DE, genderdaily_NL, genderdaily_CH, ncol=1, nrow = 3, common.legend = TRUE, legend="right")
	#pdf("/Users/freche/Dropbox/Data_Paper_DFs/r_scripts/graphs_for_paper/GenderDayly4.pdf") 
	 grid.arrange(genderdaily_DE, genderdaily_NL, genderdaily_CH, nrow = 3)
	 #dev.off() 







 ###############################################################################################################
 # Age per year #
 ###############################################################################################################
 
 ### first, just age for the entire parliament
 
		# merge in the leg_period_startdate
			head(PARLDAILY)
			nrow(PARLDAILY)
			TEMP11 <- sqldf("SELECT PARLDAILY.*, PARL.leg_period_start_asdate
										   FROM PARLDAILY LEFT JOIN PARL
										   WHERE
										   (
										   PARLDAILY.parliament_id = PARL.parliament_id
											)
										  ")
			nrow(TEMP11)
			head(TEMP11)
			PARLDAILY <- TEMP11
 
		# build up country / parliament specific dataframes
			PARLDAILY$country_abb <- substr(PARLDAILY$parliament_id,0,2)
			table(PARLDAILY$country_abb)
	 
			head(PARLDAILY)
			table(PARLDAILY$country_abb,PARLDAILY$assembly_abb)
			
			PARLDAILY_CH <- PARLDAILY[which(PARLDAILY$country_abb == "CH"),]
			nrow(PARLDAILY_CH)
			PARLDAILY_CHNR <- PARLDAILY_CH[which(PARLDAILY_CH$assembly_abb == "NR"),]
			nrow(PARLDAILY_CHNR)
			
			PARLDAILY_DE <- PARLDAILY[which(PARLDAILY$country_abb == "DE"),]
			nrow(PARLDAILY_DE)
			PARLDAILY_NL <- PARLDAILY[which(PARLDAILY$country_abb == "NL"),]
			nrow(PARLDAILY_NL)
		
		# get reduced 'first day in session' versions of these data-frames
			
			# CH NR
				PARLDAILY_CHNR_RED <- sqldf("SELECT PARLDAILY_CHNR.*
											 FROM PARLDAILY_CHNR
											 WHERE 
											 day = leg_period_start_asdate
											")
				nrow(PARLDAILY_CHNR_RED) == length(unique(PARLDAILY_CHNR$parliament_id))
			
			# DE
				PARLDAILY_DE_RED <- sqldf("SELECT PARLDAILY_DE.*
											 FROM PARLDAILY_DE
											 WHERE 
											 day = leg_period_start_asdate
											")
				nrow(PARLDAILY_DE_RED) == length(unique(PARLDAILY_DE$parliament_id))
			
			# NL
				PARLDAILY_NL_RED <- sqldf("SELECT PARLDAILY_NL.*
											 FROM PARLDAILY_NL
											 WHERE 
											 day = leg_period_start_asdate
											")
				nrow(PARLDAILY_NL_RED) == length(unique(PARLDAILY_NL$parliament_id))
			
 ### second, lets also try to breakdown Oliver suggested and Stefanie asked for to look at newcommers
 
	head(INDIVIDUAL)
	nrow(INDIVIDUAL)
	length(unique(INDIVIDUAL$pers_id))
	
	# get the day of the month in so we can reduce the individual level data to the first day of the month to speed up plotting
	INDIVIDUAL$weekday <- weekdays(INDIVIDUAL$day)
	INDIVIDUAL$monthday <- days(INDIVIDUAL$day)
	
	head(INDIVIDUAL)
	
	
	# get a data frame for the NEWcomers, and their age
		NEWC <- sqldf("SELECT INDIVIDUAL.*, MIN(INDIVIDUAL.day)
					   FROM INDIVIDUAL 
					   GROUP BY pers_id
					  ")
		nrow(NEWC)
		head(NEWC)

		# and the country level version of these data
		NEWC_CHNR <- NEWC[which(NEWC$country == "CH" & NEWC$chamber == "NR"),]
		NEWC_DE <- NEWC[which(NEWC$country == "DE"),]
		NEWC_NL <- NEWC[which(NEWC$country == "NL"),]
		head(NEWC_CHNR)
		head(NEWC_DE)
		head(NEWC_NL)

		# get a data frame for the LEAVers, and their age
		LEAV <- sqldf("SELECT INDIVIDUAL.*, MAX(INDIVIDUAL.day)
					   FROM INDIVIDUAL 
					   GROUP BY pers_id
					  ")
		nrow(LEAV)
		head(LEAV)
		
		LEAV_CHNR <- LEAV[which(LEAV$country == "CH" & LEAV$chamber == "NR"),]
		LEAV_DE <- LEAV[which(LEAV$country == "DE"),]
		LEAV_NL <- LEAV[which(LEAV$country == "NL"),]
		head(LEAV_CHNR)
		head(LEAV_DE)
		head(LEAV_NL)
		
		# CH
			LEAVE_CHNR_PAR <- sqldf("SELECT parliament_id, MIN(day) as 'day', AVG(age) as 'age'
									FROM LEAV_CHNR
									GROUP BY parliament_id
									")
			nrow(LEAVE_CHNR_PAR) == length(unique(PARLDAILY_CHNR$parliament_id))
			head(LEAVE_CHNR_PAR)
			
		# DE
			LEAVE_DE_PAR <- sqldf("SELECT parliament_id, MIN(day) as 'day', AVG(age) as 'age'
									FROM LEAV_DE
									GROUP BY parliament_id
									")
			nrow(LEAVE_DE_PAR) == length(unique(PARLDAILY_DE$parliament_id))
			head(LEAVE_DE_PAR)
		
		# NL
			LEAVE_NL_PAR <- sqldf("SELECT parliament_id, MIN(day) as 'day', AVG(age) as 'age'
									FROM LEAV_NL
									GROUP BY parliament_id
									")
			nrow(LEAVE_NL_PAR) == length(unique(PARLDAILY_NL$parliament_id))
			head(LEAVE_NL_PAR)

		# and the country level version of these data
		NEWC_CHNR <- NEWC[which(NEWC$country == "CH" & NEWC$chamber == "NR"),]
		NEWC_DE <- NEWC[which(NEWC$country == "DE"),]
		NEWC_NL <- NEWC[which(NEWC$country == "NL"),]
		head(NEWC_CHNR)
		head(NEWC_DE)
		head(NEWC_NL)
	
	
	INDIVIDUAL_CHNR <- INDIVIDUAL[which(INDIVIDUAL$country == "CH" & INDIVIDUAL$chamber == "NR"),]
	INDIVIDUAL_DE <- INDIVIDUAL[which(INDIVIDUAL$country == "DE"),]
	INDIVIDUAL_NL <- INDIVIDUAL[which(INDIVIDUAL$country == "NL"),]
	head(INDIVIDUAL_CHNR)
	head(INDIVIDUAL_DE)
	head(INDIVIDUAL_NL)

 # and now aggregating to parliament_level again
	
	# CH
		NEWC_CHNR_PAR <- sqldf("SELECT parliament_id, MIN(day) as 'day', AVG(age) as 'age'
								FROM NEWC_CHNR
								GROUP BY parliament_id
								")
		nrow(NEWC_CHNR_PAR) == length(unique(PARLDAILY_CHNR$parliament_id))
		head(NEWC_CHNR_PAR)
		
	# DE
		NEWC_DE_PAR <- sqldf("SELECT parliament_id, MIN(day) as 'day', AVG(age) as 'age'
								FROM NEWC_DE
								GROUP BY parliament_id
								")
		nrow(NEWC_DE_PAR) == length(unique(PARLDAILY_DE$parliament_id))
		head(NEWC_DE_PAR)
	
	# NL
		NEWC_NL_PAR <- sqldf("SELECT parliament_id, MIN(day) as 'day', AVG(age) as 'age'
								FROM NEWC_NL
								GROUP BY parliament_id
								")
		nrow(NEWC_NL_PAR) == length(unique(PARLDAILY_NL$parliament_id))
		head(NEWC_NL_PAR)
	
 ### putting all of this together in graphs
	newyname <- c("Average age of parliamentarians")
	newyrange <- c(35,70) 
	
	# xbreaks are taken from above!
	
	# also borrowing some of the stuff that was put in above already
 
	# CH
		ggplot(NULL) +
			#  geom_point(data=INDIVIDUAL_CHNR, aes(x=day, y=age),size=0.2,color="blue",position="jitter") +
			  geom_line(data=PARLDAILY_CHNR, aes(x=day, y=age)) +
			  geom_line(data=PARLDAILY_CHNR_RED, aes(x=day, y=age),color="blue",size=1.2) +
			  geom_line(data=NEWC_CHNR_PAR, aes(x=day, y=age),color="green",size=1.1) +
			  geom_line(data=LEAVE_CHNR_PAR, aes(x=day, y=age),color="brown",size=1.1) +
			#  geom_point(data=NEWC, aes(x=day, y=age),size=2,color="green",position="jitter") +
			  scale_y_continuous(name=newyname,limits=newyrange) +
			  scale_x_date(name="Swiss Nationalrat Day by Day",breaks=xbreaks_CHNR,labels=xlabels_CHNR,limits=xrange) +
			  geom_vline(aes(xintercept=UNI_CHNR$election_date_asdate), linetype=4, colour="black") +
			  theme_grey(base_size = 15) +
			  theme(axis.text.x = element_text(angle = 45, hjust = 1))
	
	# DE
		ggplot(NULL) +
			  geom_line(data=PARLDAILY_DE, aes(x=day, y=age),size=1) +
			  geom_line(data=PARLDAILY_DE_RED, aes(x=day, y=age),color="blue",size=1.3) +
			  geom_line(data=NEWC_DE_PAR, aes(x=day, y=age),color="green",size=1.1) +
			  geom_line(data=LEAVE_DE_PAR, aes(x=day, y=age),color="brown",size=1.1) +
			  scale_y_continuous(name=newyname,limits=newyrange) +
			  scale_x_date(name="German Bundestag Day by Day",breaks=xbreaks_DE,labels=xlabels_DE,limits=xrange) +
			  geom_vline(aes(xintercept=UNI_DE$election_date_asdate), linetype=4, colour="black") +
			  theme_grey(base_size = 15) +
			  theme(axis.text.x = element_text(angle = 45, hjust = 1))

	# NL
		ggplot(NULL) +
			  geom_line(data=PARLDAILY_NL, aes(x=day, y=age)) +
			  geom_line(data=PARLDAILY_NL_RED, aes(x=day, y=age),color="blue",size=1.2) +
			  geom_line(data=NEWC_NL_PAR, aes(x=day, y=age),color="green",size=1.1) +
			  geom_line(data=LEAVE_NL_PAR, aes(x=day, y=age),color="brown",size=1.1) +
			  scale_y_continuous(name=newyname,limits=newyrange) +
			  scale_x_date(name="Dutch Tweede Kamer Day by Day",breaks=xbreaks_NL,labels=xlabels_NL,limits=xrange) +
			  geom_vline(aes(xintercept=UNI_NL$election_date_asdate), linetype=4, colour="black") +
			  theme_grey(base_size = 15) +
			  theme(axis.text.x = element_text(angle = 45, hjust = 1))


 #### ELENA her old script below!
 
 #Split parliament id of Parldaily into components - 
 ParlNewY <- strsplit(as.character(PARLYEARLY$parliament_id), "_")
 do.call(rbind, ParlNewY)
 PARLYEARLY2 <- data.frame(PARLYEARLY, do.call(rbind, ParlNewY))
 ParlNewY2 <- strsplit(as.character(PARLYEARLY2$X2), "-")
 do.call(rbind, ParlNewY2)
 PARLYEARLY_wide <- data.frame(PARLYEARLY2, do.call(rbind, ParlNewY2))
 colnames(PARLYEARLY_wide)[colnames(PARLYEARLY_wide)=="X1"] <- "country"
 colnames(PARLYEARLY_wide)[colnames(PARLYEARLY_wide)=="X3"] <- "term"
 colnames(PARLYEARLY_wide)[colnames(PARLYEARLY_wide)=="X2.1"] <- "parl"
 
 #split Parldayly into Parliamentbased based dfs
 (parlyearly_countries <- split(PARLYEARLY_wide, PARLYEARLY_wide$country))
 PARLYEARLY_CH<-data.frame(parlyearly_countries$CH)
 dfCH <- split(PARLYEARLY_CH, PARLYEARLY_CH$parl)
 PARLYEARLY_CHSR<-data.frame(dfCH$SR)
 PARLYEARLY_CHNR<-data.frame(dfCH$NR)
 PARLYEARLY_DE<-data.frame(parlyearly_countries$DE)
 PARLYEARLY_NL<-data.frame(parlyearly_countries$NL)

 #now do 3 graphs, one for each Parliament
 #DE
 ageyearly_DE <- ggplot(NULL,
                          aes(x=year, y=age)) +
   geom_jitter(data=PARLYEARLY_DE) +
   geom_line(data = PARLYEARLY_DE)+
   xlab("German Bundestag Year by Year") +
   ylab("Average Age")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
 scale_x_discrete(breaks=seq(1945, 2017, 5))+
   ylim(40,65)
 
 
 
 #NL
ageyearly_NL <- ggplot(NULL,
                          aes(x=year, y=age)) +
   geom_jitter(data=PARLYEARLY_NL) +
  geom_line(data = PARLYEARLY_NL)+
   xlab("Dutch Tweede Kamer Year by Year") +
   ylab("Average Age")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(breaks=seq(1945, 2017, 5))+
  ylim(40,65)
 
 
 #CH
 ageyearly_CH <- ggplot(NULL, aes(x=year, y=age)) +
   geom_jitter(data = PARLYEARLY_CHSR, color="darkgrey") +
   geom_line(data = PARLYEARLY_CHSR, color="darkgrey")+
   geom_jitter(data = PARLYEARLY_CHNR) +
   geom_line(data = PARLYEARLY_CHNR)+
   xlab("Swiss Nationalrat (Staenderat=grey) Day by Day") +
   ylab("Average Age") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   scale_x_discrete(breaks=seq(1945, 2017, 5))+
   ylim(40,65)

#put all together 
 pdf("/Users/freche/Dropbox/Data_Paper_DFs/r_scripts/graphs_for_paper/AgeYearly3.pdf") 
grid.arrange(ageyearly_DE, ageyearly_NL, ageyearly_CH, nrow = 3)
dev.off() 

#########################################################################################################
# TENURE, disaggregated on the party level #
#########################################################################################################

	#### so, for this Oliver his PARTDAILY data-frame can be used
	
		
	
		## step 1: define what parties are 'established' when. 
			# So, lets start with simply saying that a party is established when it had seats in the last two parliaments. 
			# This can be checked on basis of PARTDAILY itself?
		
			# step 1.1 get a country variable
				PARTDAILY$country_abb <- substr(PARTDAILY$party_id_national,0,2)
				table(PARTDAILY$country_abb)
				table(PARTDAILY$parliament_id)
			
			# step 1.1 get the national parliament on basis of the date
				nrow(PARTDAILY)
				head(PARTDAILY)
				TEMP <- sqldf("SELECT PARTDAILY.*, PARL.previous_parliament, PARL.assembly_abb
							  FROM PARTDAILY LEFT JOIN PARL
							  ON 
							  PARTDAILY.parliament_id = PARL.parliament_id
							  ")
				nrow(TEMP)
				head(TEMP)
				PARTDAILY <- TEMP
			
			# get rid of the CH standerat entries!
				table(PARTDAILY$assembly_abb)
				head(PARTDAILY[which(PARTDAILY$parliament_id == "CH_NT-SR_1947"),])
				PARTDAILY <- PARTDAILY[which(!PARTDAILY$assembly_abb == "SR"),]
				nrow(PARTDAILY)
		
			# step 1.2 get also the previous, previous parliament
				TEMP2 <- sqldf("SELECT PARTDAILY.*, PARL.previous_parliament  as 'previous_previous_parliament'
							  FROM PARTDAILY LEFT JOIN PARL
							  ON 
								(
									PARTDAILY.previous_parliament = PARL.parliament_id
								)
							  ")
				nrow(TEMP2) # couple of cases are dropped because not all parliaments have previous, previous parliaments?
				head(TEMP2)
				PARTDAILY <- TEMP2
							
			# step 1.3 get a party its pre-assesor parties
			
				# for the we need PART to be loaded as well
				
					# import and inspect PARL and get the election date into the r-format
					PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
					summary(PART)
					names(PART)
					table(PART$ancestor_party_id == "")
					PART$ancestor_party_id <- ifelse(nchar(as.character(PART$ancestor_party_id)) == 0,NA,as.character(PART$ancestor_party_id)) # to avoid issues with matches on empty cells
					table(PART$ancestor_party_id)
					head(PART)
			
				TEMP3 <- sqldf("SELECT PARTDAILY.*, PART.ancestor_party_id
								FROM PARTDAILY LEFT JOIN PART
								ON
								PARTDAILY.party_id_national = PART.party_id
							")
				nrow(TEMP3)
				PARTDAILY <- TEMP3
			
			# step 1.3. get a dummy to check if both of these cases occured as well, 
			# also count preassesor parties as a valid occurence
			
				# lets get reduced data-frame to do this on to save time
				PARTDAILY$party_and_parliament <- paste(PARTDAILY$party_id_national, PARTDAILY$parliament_id,sep="__")
				table(PARTDAILY$party_and_parliament)
				duplicated(PARTDAILY$party_and_parliament)
				
				PDRED <- PARTDAILY[which(!duplicated(PARTDAILY$party_and_parliament)),]
				nrow(PDRED)
			
			pb <- txtProgressBar(min = 1, max = nrow(PDRED), style = 3)
			resvec <- vector()
			for(i in 1:nrow(PDRED))
			{
				# for example, the very first rows
				# PARTDAILY[which(PARTDAILY$party_id_national == "NL_CDA_NT" & PARTDAILY$parliament_id == "NL_NT-TK_1981"),]
				# PARTDAILY[518585,]
				mypreviousparliament <- PDRED$previous_parliament[i]
				mypreviouspreviousparliament <- PDRED$previous_previous_parliament[i]
				myparty = PDRED$party_id_national[i]
				
				# ancestor parties
				myancestorpartyarray <- as.vector(strsplit(PDRED$ancestor_party_id[i],";"))[[1]]
				
				# splitting this array so check can done on each element below
					# maxancesarraylength <- as.numeric(max(names(table(c)))) #how many columns do I need?  currently 3, lets make it work until 5
					ancestor_1 <- myancestorpartyarray[1]
					ancestor_2 <- myancestorpartyarray[2]
					ancestor_3 <- myancestorpartyarray[3]
					ancestor_4 <- myancestorpartyarray[4]
					ancestor_5 <- myancestorpartyarray[5]
				
				# all booleans by default false
					
					iamestablised <- FALSE
				
					seatinpreviouspar <- FALSE
					seatinpreviouspreviouspar <- FALSE
					seatinpreviousparpreass <- FALSE
					seatinpreviouspreviousparpreass <- FALSE
					hit1a = FALSE
					hit2a = FALSE
					hit3a = FALSE
					hit4a = FALSE
					hit5a = FALSE
					
					hit1b = FALSE
					hit2b = FALSE
					hit3b = FALSE
					hit4b = FALSE
					hit5b = FALSE
				
				# did your party have a seat in the previous parliament?
					seatinpreviouspar = nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviousparliament & PARTDAILY$party_id_national == myparty),]) > 0

				# and the one before that?
					seatinpreviouspreviouspar = nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == myparty),]) > 0

				# maybe one of your ancestor parties? # this looks promissing
					hit1a <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_1),]) > 0
					hit2a <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_2),]) > 0
					hit3a <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_3),]) > 0
					hit4a <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_4),]) > 0
					hit5a <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_5),]) > 0
				
					hit1b <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_1),]) > 0
					hit2b <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_2),]) > 0
					hit3b <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_3),]) > 0
					hit4b <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_4),]) > 0
					hit5b <- nrow(PARTDAILY[which(PARTDAILY$parliament_id == mypreviouspreviousparliament & PARTDAILY$party_id_national == ancestor_5),]) > 0
				
				# now, am I established?
					iamestablised <- ifelse(((seatinpreviouspar & seatinpreviouspreviouspar) | (hit1a & hit1b) | (hit2a & hit2b) | (hit3a & hit3b) | (hit4a & hit4b) | (hit5a & hit5b) | (seatinpreviouspar & hit1b) | (seatinpreviouspar & hit2b) | (seatinpreviouspar & hit3b) | (seatinpreviouspar & hit4b) |(seatinpreviouspar & hit5b)),TRUE,FALSE) # set at the end if because it is possible that your most recent parliament you had a seat and the one before was one of your ancestor parties, around transition years
				
				resvec[i] <- iamestablised
				setTxtProgressBar(pb, i)
				}
				close(pb)
				PDRED$iamestablised <- resvec
				table(PDRED$iamestablised) # in general this looks very good
				
				# merge the conclusions back in
				nrow(PARTDAILY)
				TEMP5 <- sqldf("SELECT PARTDAILY.*, PDRED.iamestablised
								FROM PARTDAILY LEFT JOIN PDRED
								ON
								(PARTDAILY.party_id_national = PDRED.party_id_national)
								AND
								(PARTDAILY.parliament_id = PDRED.parliament_id)
								")
				nrow(TEMP5)
				head(TEMP5)
				PARTDAILY <- TEMP5
				table(PARTDAILY$iamestablised)
		
		# step 2: aggregate to the parliament level - with a weighted average (on seats!) - and distinquish between establised and none established parties
		
				# just checking a vector with all days
					uniquedaysvec <- unique(PARTDAILY$day)
					length(table(PARTDAILY$day))
					length(uniquedaysvec)
					min(PARTDAILY$day)
					max(PARTDAILY$day)
					max(PARTDAILY$day) - min(PARTDAILY$day) # looks good

			# get an array with all the unique days per country (is also needed for the loop below!)
					PARTDAILY$daycountry <- paste(PARTDAILY$day,PARTDAILY$country_abb,sep="__")		
					length(table(PARTDAILY$daycountry))
					uniquedaycountryvec <- unique(PARTDAILY$daycountry)
					length(uniquedaycountryvec)
					head(uniquedaycountryvec)
			
			# example
				weighted.mean(c(1,1,2,2),c(1,1,1,10000000))			
			
			# for the entire parliament 
			
				# get the mean for one day
					MEANDAT <- PARTDAILY[which(PARTDAILY$daycountry == uniquedaycountryvec[1]),]
					weighted.mean(c(MEANDAT$tenure),c(MEANDAT$seats))
					
					# checking this
					head(PARLDAILY)
					PARLDAILY$country_abb <- substr(as.character(PARLDAILY$parliament_id),0,2)
					table(PARLDAILY$country_abb)
					PARLDAILY[which(PARLDAILY$country_abb == "CH" & PARLDAILY$day == as.Date("1995-12-04",origin="1970-01-01")),] # not exactly the same number!
				
				# in a loop 
				
					uniquedaycountryvec <- unique(PARTDAILY$daycountry)
					resvec2 <- vector()
					pb <- txtProgressBar(min = 1, max = length(uniquedaycountryvec), style = 3)
					for(i in 1:length(uniquedaycountryvec))
					{
						MEANDAT <- PARTDAILY[which(PARTDAILY$daycountry == uniquedaycountryvec[i]),]
						resvec2[i] <- weighted.mean(c(MEANDAT$tenure),c(MEANDAT$seats))
					setTxtProgressBar(pb, i)
					}
					close(pb)
					summary(resvec2)
					table(is.na(resvec2))
					
					TOT <- as.data.frame(cbind(uniquedaycountryvec,resvec2))
					colnames(TOT) <- c("daycountry","averagetenure")
					head(TOT)
					TOT$day <- as.Date(substr(TOT$daycountry,0,10),origin="1970-01-01")
					TOT$country <- substrRight(as.character(TOT$daycountry),2)
					table(TOT$country) # looks familiair?
					table(PARLDAILY$country) # yes, an exact match!
					
			# for only the established parties
	
				# get a reduced data-frame - and the unique day count vector to match it 
				PARTDAILY_EST <- PARTDAILY[which(PARTDAILY$iamestablised),]
				uniquedaycountryvec2 <- unique(PARTDAILY_EST$daycountry)
				length(uniquedaycountryvec)
				uniquedaycountryvec == uniquedaycountryvec2 # (any none existent days?), yes about 6000 in fact....' let find one so I can check!
				uniquedaycountryvec[which(!uniquedaycountryvec %in% uniquedaycountryvec2)] # right, so this is really an earlier year problem, makes sense. Membership data for CH and DE is missing for these earlier years
				
					resvec3 <- vector()
					pb <- txtProgressBar(min = 1, max = length(uniquedaycountryvec2), style = 3)
					for(i in 1:length(uniquedaycountryvec2))
					{
						MEANDAT2 <- PARTDAILY_EST[which(PARTDAILY_EST$daycountry == uniquedaycountryvec2[i]),]
						resvec3[i] <- weighted.mean(c(MEANDAT2$tenure),c(MEANDAT2$seats))
					setTxtProgressBar(pb, i)
					}
					close(pb)
					summary(resvec3)
					table(is.na(resvec3))
					
					TOT2 <- as.data.frame(cbind(uniquedaycountryvec2,resvec3))
					colnames(TOT2) <- c("daycountry","averagetenure")
					head(TOT2)
					TOT2$day <- as.Date(substr(TOT2$daycountry,0,10),origin="1970-01-01")
					TOT2$country <- substrRight(as.character(TOT2$daycountry),2)
					head(TOT2)
				
			# for the not establised parties
				
				PARTDAILY_NEST <- PARTDAILY[which(!PARTDAILY$iamestablised),]
				uniquedaycountryvec3 <- unique(PARTDAILY_NEST$daycountry)
				length(uniquedaycountryvec)
				uniquedaycountryvec == uniquedaycountryvec3 # (any none existent days?), yes about 19000 in fact....
				uniquedaycountryvec[which(!uniquedaycountryvec %in% uniquedaycountryvec3)] # right, so this is ..
				
					resvec4 <- vector()
					pb <- txtProgressBar(min = 1, max = length(uniquedaycountryvec3), style = 3)
					for(i in 1:length(uniquedaycountryvec3))
					{
						MEANDAT3 <- PARTDAILY_NEST[which(PARTDAILY_NEST$daycountry == uniquedaycountryvec3[i]),]
						resvec4[i] <- weighted.mean(c(MEANDAT3$tenure),c(MEANDAT3$seats))
					setTxtProgressBar(pb, i)
					}
					close(pb)
					summary(resvec4)
					table(is.na(resvec4))
					
					TOT3 <- as.data.frame(cbind(uniquedaycountryvec3,resvec4))
					colnames(TOT3) <- c("daycountry","averagetenure")
					TOT3$day <- as.Date(substr(TOT3$daycountry,0,10),origin="1970-01-01")
					TOT3$country <- substrRight(as.character(TOT3$daycountry),2)
					head(TOT3)
			
			# merging all of these together so that ggplot can start doing its magic
			
				# merging tot2 in
				GGDAT <- sqldf("SELECT TOT.*, TOT2.averagetenure as 'averagetenure_est'
								FROM TOT LEFT JOIN TOT2
								ON
								(
								TOT.day = TOT2.day
								AND
								TOT.country = TOT2.country
								)
								")
				
				# merging tot3 in
				GGDAT <- sqldf("SELECT GGDAT.*, TOT3.averagetenure as 'averagetenure_nest'
								FROM GGDAT LEFT JOIN TOT3
								ON
								(
								GGDAT.day = TOT3.day
								AND
								GGDAT.country = TOT3.country
								)
								")
			
				# lets merge the election dates in here as well, because 'WHERE' is used here instead of 'ON' this also reduces the data to these dates
				PARLDAILY$parliament_id <- as.character(PARLDAILY$parliament_id)
				GGDATRED <- sqldf("SELECT GGDAT.*, PARL.leg_period_start_asdate, PARL.parliament_id
						   FROM GGDAT LEFT JOIN PARL
						   WHERE
						   (
						   GGDAT.country = PARL.country_abb
						   AND
						   GGDAT.day = PARL.leg_period_start_asdate
							)
						  ")
				nrow(GGDATRED)
			
				# set the data-types proper
				head(GGDATRED)
				GGDATRED$averagetenure <- as.numeric(GGDATRED$averagetenure)
				GGDATRED$averagetenure_est <- as.numeric(GGDATRED$averagetenure_est)
				GGDATRED$averagetenure_nest <- as.numeric(GGDATRED$averagetenure_nest)
				
				GGDAT_CH <- GGDATRED[which(GGDATRED$country == "CH"),]
				GGDAT_DE <- GGDATRED[which(GGDATRED$country == "DE"),]
				GGDAT_NL <- GGDATRED[which(GGDATRED$country == "NL"),] 
				
				yrangehere <- c(0,12)
				
				# CH
					ggplot(NULL) +
					  geom_line(data=PARLDAILY_CHNR, aes(x=day, y=tenure,color="from parldaily",size=1))  +
					  geom_line(data=GGDAT_CH, aes(x=day, y=averagetenure,color="all",size=1.5))  +
					  geom_line(data=GGDAT_CH, aes(x=day, y=averagetenure_est,color="established parties",size=1.1))  +
				#	  geom_line(data=GGDAT_CH, aes(x=day, y=averagetenure_nest,color="not established parties",size=1.01)) +
					  scale_y_continuous(name="Average years in parliament before",limits=yrangehere) +
					  scale_x_date(name="Swiss Nationalrat (day by day / at first day in session)",breaks=xbreaks_CHNR,labels=xlabels_CHNR,limits=xrange) +
					  geom_vline(aes(xintercept=UNI_CHNR$election_date_asdate), linetype=4, colour="black") +
					  theme_grey(base_size = 15) +
					  theme(axis.text.x = element_text(angle = 45, hjust = 1))
					  
				# DE # for Germany the line match with PARLDAILY is bang on!
					ggplot(NULL) +
					  geom_line(data=PARLDAILY_DE, aes(x=day, y=tenure,color="from parldaily",size=1))  +
					  geom_line(data=GGDAT_DE, aes(x=day, y=averagetenure,color="all",size=1.5))  +
					  geom_line(data=GGDAT_DE, aes(x=day, y=averagetenure_est,color="established parties",size=1.1))  +
				#	  geom_line(data=GGDAT_DE, aes(x=day, y=averagetenure_nest,color="not established parties")) +
					  scale_y_continuous(name="Average years in parliament before",limits=yrangehere) +
					  scale_x_date(name="German Bundestag at first day in session",breaks=xbreaks_DE,labels=xlabels_DE,limits=xrange) +
					  geom_vline(aes(xintercept=UNI_DE$election_date_asdate), linetype=4, colour="black") +
					  theme_grey(base_size = 15) +
					  theme(axis.text.x = element_text(angle = 45, hjust = 1))	
					  
				# NL
					ggplot(NULL) +
					  geom_line(data=PARLDAILY_NL, aes(x=day, y=tenure,color="from parldaily",size=1))  +
					  geom_line(data=GGDAT_NL, aes(x=day, y=averagetenure,color="all",size=1.5))  +
					  geom_line(data=GGDAT_NL, aes(x=day, y=averagetenure_est,color="established parties",size=1.1))  +
					  geom_line(data=GGDAT_NL, aes(x=day, y=averagetenure_nest,color="not established parties")) +
				#	  scale_y_continuous(name="Average years in parliament before",limits=yrangehere) +
					  scale_x_date(name="Dutch Tweede-Kamer at first day in session",breaks=xbreaks_NL,labels=xlabels_NL,limits=xrange) +
					  geom_vline(aes(xintercept=UNI_NL$election_date_asdate), linetype=4, colour="black") +
					  theme_grey(base_size = 15) +
					  theme(axis.text.x = element_text(angle = 45, hjust = 1))	
				
		# for Germany (just for Germany for now) lets also try a disaggregation to the party level?
				 
			head(PARTDAILY)
			table(PARTDAILY$country_abb)
			PARTDAILY_DE <- PARTDAILY[which(PARTDAILY$country_abb == "DE"),]
			nrow(PARTDAILY_DE)
			
			# melting the data
			PARTDAILY_DE_RED <- PARTDAILY_DE[c("day","party_id_national")]
			PARTDAILY_DE_MELT <- melt(PARTDAILY_DE_RED)
			head(PARTDAILY_DE_MELT)
			PARTDAILY_DE_MELT$day <- as.Date(PARTDAILY_DE_MELT$value,origin="1970-01-01")
			
			ggplot(data=PARTDAILY_DE_MELT,aes(x=day, y=tenure,color="from parldaily"))) +
		

###############################a
# party switching #
###############################


# output format needed
	# vector with sources
	# vector with targets
	# vector with values
	
	# can all be derived from a transition matrix
	
# we need MEME and PART for this

		MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")
		summary(MEME)
		names(MEME)
		head(MEME)
		nrow(MEME)
		
		PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
		summary(PART)
		names(PART)
		head(PART)
		nrow(PART)
	
	# lets start with creating a variable that indicates that you are the source of 'a' transition
	
		# for this we first need the dates properly
			# a) Remove information on left ("[[lcen]]") and right censoring ("[[rcen]]")
			MEME$memep_startdate <- gsub("[[lcen]]", "", MEME$memep_startdate, fixed = TRUE, useBytes = TRUE)
			MEME$memep_enddate <- gsub("[[rcen]]", "", MEME$memep_enddate, fixed = TRUE, useBytes = TRUE)
		
			MEME$memep_startdate_dateformat <- as.Date(as.character(MEME$memep_startdate),format=c("%d%b%Y"),origin="1970-01-01")
			MEME$memep_enddate_dateformat <- as.Date(as.character(MEME$memep_enddate),format=c("%d%b%Y"),origin="1970-01-01")
			head(MEME)
		
		# lets make a column with the 'loweststartdate'
			LSD <- sqldf("SELECT pers_id, min(memep_startdate_dateformat) as 'lowest_start_date'
						  FROM MEME
						  GROUP BY pers_id
						")
			nrow(LSD) == length(unique(MEME$pers_id))
			LSD$lowest_start_date = as.Date(LSD$lowest_start_date,origin="1970-01-01")
			head(LSD) 
			
			nrow(MEME)
			MEME <- sqldf("SELECT MEME.*, LSD.lowest_start_date
						   FROM MEME LEFT JOIN LSD
						   ON 
						   MEME.pers_id = LSD.pers_id
						  ")
			nrow(MEME)
			head(MEME)

		# if the party is not a national party, get the national party equivalent
		
			TEMP21 <- sqldf("SELECT MEME.*, PART.mother_party_id
							 FROM MEME LEFT JOIN PART
							 ON MEME.party_id = PART.party_id
							")
			nrow(TEMP21)
			nrow(PART) # some double party ids exist in PART, will discuss this with Adrian and Oliver tomorrow.
			MEMET <- TEMP21
		
			# if you do not have a mother party specified, then you youself should be the national party
				table(is.na(MEMET$mother_party_id)) # only 2, so lets of emtpy?
				table(MEMET$mother_party_id == "") # exactly
				MEMET[which(MEMET$mother_party_id == ""),]
				table(substrRight(MEMET[which(MEMET$mother_party_id == ""),]$party_id,2)) # all national parties indeed
			
			# so then this fix is good
				MEMET$nat_party_equiv <- ifelse((MEMET$mother_party_id == ""| is.na(MEMET$mother_party_id) | MEMET$mother_party_id == "none"),MEMET$party_id,MEMET$mother_party_id)
				table(MEMET$nat_party_equiv) # looks pretty good
			
		# so, now, you are the source of a transition when there is another date for the same person that is higher then yours and the national party id of that one is not the same as yours
		# lets do this in a loop
			
			# step 1: get a set of other eppisodes with later startdates
			MEMET[582,]
			mypersid <- MEMET$pers_id[582]
			mycurrentstartdate <- MEMET$memep_startdate_dateformat[582]
			mycurrentenddate <- MEMET$memep_enddate_dateformat[582]
			mycurrentparty <- MEMET$nat_party_equiv[582]
			
			MEMET[9,]
			mypersid <- MEMET$pers_id[9]
			mycurrentstartdate <- MEMET$memep_startdate_dateformat[9]
			mycurrentenddate <- MEMET$memep_enddate_dateformat[9]
			mycurrentparty <- MEMET$nat_party_equiv[9]
			
			getmytargetparty <- function(mypersid,mycurrentstartdate,mycurrentenddate,mycurrentparty)
			{
				MEMETLOC  <- MEMET
				MEMETLOC$mypersid <- mypersid
				MEMETLOC$mycurrentstartdate <- mycurrentstartdate
				MEMETLOC$mycurrentparty <- mycurrentparty
				
				# restriction to these actually being within parliament is a good idea?
				
				LATERTHENME <- sqldf("SELECT MEMETLOC.*
									 FROM
									 MEMETLOC 
									 WHERE
									 pers_id = mypersid
									 AND
									 memep_startdate_dateformat > mycurrentstartdate
									 AND
									 NOT (nat_party_equiv = mycurrentparty)
									")
				
				# if there is more then one, select the one with the lowest date
				MYTARGET <- sqldf("SELECT LATERTHENME.*, MIN(memep_startdate_dateformat) as 'min_date'
									FROM LATERTHENME
									")
			
			 if (!is.na(MYTARGET$min_date))
				{				
					# check if what is left is maybe a 'none' party, in less then a year from leaving, in that case remove that one from 'later then me' and try again - only if anything is left then however
					if((MYTARGET$nat_party_equiv == "DE_none_NT" | MYTARGET$nat_party_equiv == "CH_none_NT" | MYTARGET$nat_party_equiv == "NL_none_NT") & ((MYTARGET$memep_enddate_dateformat - mycurrentenddate) < 365))
					{
						REMAINERS <- LATERTHENME[which(!LATERTHENME$memep_id == MYTARGET$memep_id),]
						
						MYTARGET <- sqldf("SELECT REMAINERS.*, MIN(memep_startdate_dateformat) as 'min_date'
										FROM REMAINERS
										")
					}
				}
				
			return(MYTARGET$nat_party_equiv)
			}
			
			# testing
			getmytargetparty(MEMET$pers_id[582],MEMET$memep_startdate_dateformat[582],MEMET$memep_enddate_dateformat[582],MEMET$nat_party_equiv[582]) # should be CDA
			getmytargetparty(MEMET$pers_id[9],MEMET$memep_startdate_dateformat[9],MEMET$memep_enddate_dateformat[9],MEMET$nat_party_equiv[9]) # should be NA
			
			# run as a loop
			pb <- txtProgressBar(min = 1, max = nrow(MEMET), style = 3)
			resvectarget <- vector()
			for(i in 1:nrow(MEMET))
			{
				resvectarget[i] <- getmytargetparty(MEMET$pers_id[i],MEMET$memep_startdate_dateformat[i],MEMET$memep_enddate_dateformat[i],MEMET$nat_party_equiv[i])
				setTxtProgressBar(pb, i)
			}
			close(pb)
	
		MEMET$id_target_party <- resvectarget
	
	## this now needs to be aggregated for each source party, we need to know how often what parties are the target parties, this is country specific
		
		# first, get a dataframe per country 
			MEMET$country_abb <- substr(MEMET$memep_id,0,2)
			
			# NL
				MEMET_NL <- MEMET[which(MEMET$country_abb == "NL"),]
				# within this, we basically just need a table, the rows have the source, the columns the target and the count the value
				
					transtabNL <- table(MEMET_NL$nat_party_equiv,MEMET_NL$id_target_party)
				
					# we can then melt this to get the format that plotly / sanky diagram likes
					transtabNL_melted <- melt(transtabNL)
					names(transtabNL_melted) <- c("source","target","transitioncount")
					transtabNL_melted_red <- transtabNL_melted[which(transtabNL_melted$transitioncount > 0),]
					
			# DE
				MEMET_DE <- MEMET[which(MEMET$country_abb == "DE"),]
				# within this, we basically just need a table, the rows have the source, the columns the target and the count the value
				
					transtabDE <- table(MEMET_DE$nat_party_equiv,MEMET_DE$id_target_party)
				
					# we can then melt this to get the format that plotly / sanky diagram likes
					transtabDE_melted <- melt(transtabDE)
					names(transtabDE_melted) <- c("source","target","transitioncount")
					transtabDE_melted_red <- transtabDE_melted[which(transtabDE_melted$transitioncount > 0),]
					
			# CH
				MEMET_CH <- MEMET[which(MEMET$country_abb == "CH"),]
				# within this, we basically just need a table, the rows have the source, the columns the target and the count the value
				
					transtabCH <- table(MEMET_CH$nat_party_equiv,MEMET_CH$id_target_party)
				
					# we can then melt this to get the format that plotly / sanky diagram likes
					transtabCH_melted <- melt(transtabCH)
					names(transtabCH_melted) <- c("source","target","transitioncount")
					transtabCH_melted_red <- transtabCH_melted[which(transtabCH_melted$transitioncount > 0),]
			
			
	## and then the plot!
	
		# get the colors per party in PART into the proper format
		
			table(PART$RGB)
			library(purrr)
			PART$RGB <- as.character(PART$RGB)
			PART$RGB <- ifelse(PART$RGB == "","rgb(999,999,999)",PART$RGB)
			
			cleaned <- gsub(")","",gsub("rgb(","",PART$RGB,fixed=TRUE),fixed=TRUE)
			
			PART$color_R <- unlist(map(strsplit(cleaned,","),1))
			table(PART$color_R)
			PART$color_R <- as.numeric(ifelse(PART$color_R == "999",149,PART$color_R)) # 149 is grey
			
			PART$color_G <- unlist(map(strsplit(cleaned,","),2))
			table(PART$color_G)
			PART$color_G <- as.numeric(ifelse(PART$color_G == "999",149,PART$color_G))
			
			PART$color_B <- unlist(map(strsplit(cleaned,","),3))
			table(PART$color_B)
			PART$color_B <- as.numeric(ifelse(PART$color_B == "999",149,PART$color_B))
		
			PART$RGB_int <- rgb(PART$color_R/255,PART$color_G/255,PART$color_B/255)

	# sankey diagram for NL
	
		## for later, this code could maybe be used to set the colors
			
			# get the color codes merged in from PART
			if(FALSE)
			{
					partyids <- sort(unique(rownames(transtabNL),colnames(transtabNL)))
					party_abbs <- gsub("_NT","",gsub("NL_","",partyids))
					
					PD <- as.data.frame(cbind(partyids,party_abbs))
					PD <- sqldf("SELECT PD.*, PART.RGB_int as 'colorforparty'
						   FROM PD LEFT JOIN PART
						   ON PD.partyids = PART.party_id
						  ")
						  
					PD <- PD[which(!PD$colorforparty == "<NA>"),]
			
				TEMP <- sqldf("SELECT PBNL.*, PD.colorforparty
								FROM PBNL LEFT JOIN PD
								ON PBNL.source = PD.partyids
								")
				TEMP$colorforparty[which(is.na(TEMP$colorforparty))] <- "#E7D031"
			{
			
		# getting vectors into the format we need them in
			
			# get from the table above
			PBNL <- transtabNL_melted_red
			sum(PBNL$transitioncount)
			length(unique(INDIVIDUAL$pers_id[which(INDIVIDUAL$country=="NL")])) # of x MPS
			sum(PBNL$transitioncount) / length(unique(INDIVIDUAL$pers_id[which(INDIVIDUAL$country=="NL")]))
			
			# clean and set the correct data types
			PBNL$source <- as.character(PBNL$source)
			PBNL$target <- as.character(PBNL$target)
			PBNL$target <- paste(as.character(PBNL$target)," ",sep="")
			PBNL$source <- gsub("_NT","",gsub("NL_","",PBNL$source))
			PBNL$target <- gsub("_NT","",gsub("NL_","",PBNL$target))
			PBNL$transitioncount <- as.numeric(PBNL$transitioncount)
			
			#Plotting
				# (color) options e.t.c.
						opts = paste0("{
						link: { colorMode: 'gradient'},
						TextStyle: {fontSize:16}
						}" )
			
			# and the plot
				p <- gvisSankey(PBNL,from="source",to="target", weight="transitioncount", options = list(sankey=opts,width=150,height=500))
				plot(p)
				
			## plotly alternative! - does not work
			
					PBNL <- transtabNL_melted_red
					partyids <- sort(unique(rownames(transtabNL),colnames(transtabNL)))
					party_abbs <- gsub("_NT","",gsub("NL_","",partyids))
					
					PD <- as.data.frame(cbind(partyids,party_abbs))
					PD <- sqldf("SELECT PD.*, PART.RGB_int as 'colorforparty'
						   FROM PD LEFT JOIN PART
						   ON PD.partyids = PART.party_id
						  ")
						  
						PBNL$source <- as.character(PBNL$source)
						PBNL$target <- as.character(PBNL$target)
						PBNL$target <- paste(as.character(PBNL$target)," ",sep="")
					
						PBNL$transitioncount <- as.numeric(PBNL$transitioncount)
			
					COLDAT <- as.data.frame(cbind(names(table(c(PBNL$source,PBNL$target)))))
					colnames(COLDAT) <- "label"
					COLDAT$id <- str_trim(COLDAT$label)
					
					# now merge the color in
					COLDAT <- sqldf("SELECT COLDAT.*, PD.colorforparty
						   FROM COLDAT LEFT JOIN PD
						   ON COLDAT.id = PD.partyids
						   ")
					
					COLDAT$label <- gsub("_NT","",gsub("NL_","",COLDAT$label))
					
					PBNL$source <- gsub("_NT","",gsub("NL_","",PBNL$source))
					PBNL$target <- gsub("_NT","",gsub("NL_","",PBNL$target))
					
					labelvec <- COLDAT$label
					labelcolorvec <- COLDAT$colorforparty
					sourcevec <- PBNL$source
					targetvec <- PBNL$target
					valuevec <- PBNL$transitioncount

	# sankey diagram for DE
		
			PBDE <- transtabDE_melted_red
			sum(PBDE$transitioncount)
			length(unique(INDIVIDUAL$pers_id[which(INDIVIDUAL$country=="DE")])) # of x MPS
			sum(PBDE$transitioncount) / length(unique(INDIVIDUAL$pers_id[which(INDIVIDUAL$country=="DE")]))
			
			PBDE$source <- as.character(PBDE$source)
			PBDE$target <- as.character(PBDE$target)
			PBDE$target <- paste(as.character(PBDE$target)," ",sep="")
			PBDE$source <- gsub("_NT","",gsub("DE_","",PBDE$source))
			PBDE$target <- gsub("_NT","",gsub("DE_","",PBDE$target))
			
			# and the plot
				p <- gvisSankey(PBDE,from="source",to="target", weight="transitioncount", options = list(sankey=opts,width=150,height=500))
				plot(p)
	
	# sankey diagram for CH
		
			PBCH <- transtabCH_melted_red
			sum(PBCH$transitioncount)
			length(unique(INDIVIDUAL$pers_id[which(INDIVIDUAL$country=="CH")])) # of x MPS
			sum(PBCH$transitioncount) / length(unique(INDIVIDUAL$pers_id[which(INDIVIDUAL$country=="CH")]))
			
			PBCH$source <- as.character(PBCH$source)
			PBCH$target <- as.character(PBCH$target)
			PBCH$target <- paste(as.character(PBCH$target)," ",sep="")
			PBCH$source <- gsub("_NT","",gsub("CH_","",PBCH$source))
			PBCH$target <- gsub("_NT","",gsub("CH_","",PBCH$target))
			
			# and the plot
				p <- gvisSankey(PBCH,from="source",to="target", weight="transitioncount", options = list(sankey=opts,width=150,height=500))
				plot(p)
	
###############################a
# tenure, elena's old script #
###############################


#Split parliament id of Parldaily into components
ParlNewT <- strsplit(as.character(PARLTERM$parliament_id), "_")
do.call(rbind, ParlNewT)
PARLTERM2 <- data.frame(PARLTERM, do.call(rbind, ParlNewT))
ParlNewT2 <- strsplit(as.character(PARLTERM2$X2), "-")
do.call(rbind, ParlNewT2)
PARLTERM_wide <- data.frame(PARLTERM2, do.call(rbind, ParlNewT2))
colnames(PARLTERM_wide)[colnames(PARLTERM_wide)=="X1"] <- "country"
colnames(PARLTERM_wide)[colnames(PARLTERM_wide)=="X3"] <- "term"
colnames(PARLTERM_wide)[colnames(PARLTERM_wide)=="X2.1"] <- "parl"

#split Parldayly into Parliamentbased based dfs
(parlterm_countries <- split(PARLTERM_wide, PARLTERM_wide$country))
PARLTERM_CH<-data.frame(parlterm_countries$CH)
dfCH <- split(PARLTERM_CH, PARLTERM_CH$parl)
PARLTERM_CHSR<-data.frame(dfCH$SR)
PARLTERM_CHNR<-data.frame(dfCH$NR)
PARLTERM_DE<-data.frame(parlterm_countries$DE)
PARLTERM_NL<-data.frame(parlterm_countries$NL)

#now do 3 graphs, one for each Parliament
#DE
#tenureterm_DE <-ggplot(NULL,
 #                      aes(x=term, y=tenure)) +
#  geom_jitter(data=PARLTERM_DE) +
#   xlab("German Bundestag Term by Term") +
#   ylab("Average Tenure")

#NL
# tenureterm_NL <- ggplot(NULL,
#                        aes(x=term, y=tenure)) +
#  geom_jitter(data=PARLTERM_NL) +
#   xlab("Dutch Tweede Kamer Term by Term") +
#   ylab("Average Tenure")

#CH
# tenureterm_CH <- ggplot(NULL, aes(x=term, y=tenure)) +
#  geom_jitter(data = PARLTERM_CHSR, color="darkgrey") +
#  geom_jitter(data = PARLTERM_CHNR) +
#  xlab("Swiss Nationalrat (Staenderat=grey) Term by Term") +
#   ylab("Average Tenure") 

#put all together 
# grid.arrange(tenureterm_DE, tenureterm_NL, tenureterm_CH, nrow = 3)

#put all three countries in the same graph
tenuregraph <-  ggplot(NULL, aes(x=term, y=tenure)) +
  #geom_jitter(data = PARLTERM_CHSR, color="darkgrey") +
  geom_jitter(data=PARLTERM_NL, aes(color="Netherlands")) +
  geom_jitter(data=PARLTERM_DE, aes(color="Germany")) +
  geom_jitter(data = PARLTERM_CHNR, aes(color="Switzerland (NR)")) +
   # guides(fill=guide_legend(title="New Legend Title"))+
  xlab("Tenure Term by Term") +
  ylab("Average Tenure in leg. Terms") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   #scale_x_discrete(breaks=seq(1945, 2017, 1))
  

#put all together 
pdf("/Users/freche/Dropbox/Data_Paper_DFs/r_scripts/graphs_for_paper/TenureTermly2.pdf") 
plot(tenuregraph)
dev.off() 


##########
# Fact/party Composition monthly
##########

#Split fact id of FACTMONTHLY into components
factmonthlyNew <- strsplit(as.character(FACTMONTHLY$faction_id_core_complete), "_")
do.call(rbind, factmonthlyNew)
FACTMONTHLY2 <- data.frame(FACTMONTHLY, do.call(rbind, factmonthlyNew))
colnames(FACTMONTHLY2)[colnames(FACTMONTHLY2)=="X1"] <- "country"
colnames(FACTMONTHLY2)[colnames(FACTMONTHLY2)=="X3"] <- "term"
colnames(FACTMONTHLY2)[colnames(FACTMONTHLY2)=="X2.1"] <- "parl"
colnames(FACTMONTHLY2)[colnames(FACTMONTHLY2)=="X6"] <- "party"

#factmonthlyNew2 <- strsplit(as.character(FACTMONTHLY2$X2), "-")
#do.call(rbind, factmonthlyNew2)


#split Parldayly into Parliamentbased based dfs
FACTMONTHLY_countries <- split(FACTMONTHLY2, FACTMONTHLY2$country)
FACTMONTHLY_DE<-data.frame(FACTMONTHLY_countries$DE)
FACTMONTHLY_NL<-data.frame(FACTMONTHLY_countries$NL)

#plot DE first only to test

  ggplot() + geom_bar(aes(y = seats, x = month, fill = party), data = FACTMONTHLY_DE,
                         stat="identity"
                          #xlab("The Composition of the Bundestag Month by Month") +
                           # ylab("Average Number of Seats") +
                           )
  #fill <- c("#5F9EA0", "#E1B378")
  #p4 <- p4 + scale_fill_manual(values=fill)