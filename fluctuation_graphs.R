# packages

	#install.packages("ggpubr")
	#install.packages("tidyverse")
	#install.packages("purrr")

	library(sqldf)
	library(lubridate)
	library(ggplot2)
	library(ggpubr)
	library(stringi)
	library(purrr)
	library(tidyverse)
	library(data.table)
	
# change the language and date formatting to English if it is not already
	Sys.setenv(LANG = "EN")
	Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
	Sys.getlocale(category = "LC_ALL")

 # working directory
	setwd("C:/Users/turnerzw/Basel Powi Dropbox/Data_Paper_DFs")
	
 # a function that is used below
	substrRight <- function(x, n)
	{
		substr(x, nchar(x)-n+1, nchar(x))
	}	
		


 # load the data
 if(FALSE)
 {
	load("Complete_Daily_MP_Data_2019-11-26_1232.Rdata") # this was the older version 'Complete_Daily_MP_Data_2019-06-04_1444.Rdata' before
#	head(MPDAYLONG)
	head(INDIVIDUAL)
}
	
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

	
	# Load PARL
	PARL = read.csv("./source_csvs/PARL.csv", header = TRUE, sep = ";")
	head(PARL)

# first lets get rid of all the Steanderat entries < this needs to change, 
	nrow(INDIVIDUAL)
#	INDIVIDUAL <- INDIVIDUAL[which(!grepl("CH_NT-SR",MPDAYLONG$parliament_id,fixed=TRUE)),]
	nrow(INDIVIDUAL)
	object.size(INDIVIDUAL)
	head(INDIVIDUAL)
	table(INDIVIDUAL$parliament_id)
	
	table(is.na(INDIVIDUAL$party_id_national))
	
 # merge in the leg_period_start for each parliament
 
	BU <- sqldf("SELECT INDIVIDUAL.pers_id, INDIVIDUAL.parliament_id, INDIVIDUAL.day, INDIVIDUAL.party_id_national, PARL.leg_period_start, PARL.assembly_abb
			   FROM INDIVIDUAL LEFT JOIN PARL
			   ON INDIVIDUAL.parliament_id = PARL.parliament_id
			   ORDER BY pers_id, day
			 ")
	head(BU)
	
	# get in the country because it is needed below
		BU$country <- substr(BU$pers_id,0,2)
		
	# get in NR or SR
		BU$house <- ifelse((BU$assembly_abb == "TK" | BU$assembly_abb == "BT" | BU$assembly_abb == "NR"),"lower house","upper house")
		table(BU$house)
		table(BU$assembly_abb,BU$house)
	
	# get the correct internal r-date format
	#	BU$leg_period_start_posix <- as.Date(as.POSIXct(as.character(BU$leg_period_start),format=c("%d%b%Y")))
		BU$leg_period_start_posix <- as.Date(fast_strptime(as.character(BU$leg_period_start),"%d%b%Y")) # this one works faster, with a better result rom the lubridate package
		table(BU$leg_period_start_posix == BU$leg_period_start_posix2) # indeed, exactly the same, and much faster! - and actuall without this one day issue, so lets implement this everywhere

	# now, get rid off all cases that do not meet the following criteria: a row exists in the data-frame where the day is the legistlative period its start date, if this is not the case you where not 'there on the first day'
	
	# this reduces the data to a list of pers_ids and parliament_ids only for those that where there at the first day
	TEMP <-  	  sqldf("SELECT BU.* 
					 FROM BU
					 WHERE BU.day = BU.leg_period_start_posix
					")
	TEMP$persandparliamentid <- stri_join(TEMP$pers_id,TEMP$parliament_id,sep="__")
	gc()
	head(TEMP)
					
	# now we can simpy only include lines of the main data that occur in TEMP as well (if they don't occur in TEMP they are not people that where there on the first day).
	BU$persandparliamentid <- stri_join(BU$pers_id,BU$parliament_id,sep="__")
	gc()
	head(BU)
	
	BURED <- BU[which(BU$persandparliamentid %in% TEMP$persandparliamentid),]
	BURED$country <- substr(BURED$pers_id,0,2)
	head(BURED)
	rm(TEMP) ## %% ##
	
	# get the daytotals for 'core members'
	#	DAYTOTALS <- as.data.frame(table(BURED$day,BURED$country)) ### THIS NEEDS TO CHANGE
		TESTING <- as.data.frame(BURED %>% count(day, country, house)) ## DOES THIS GIVE THE SAME RESULT? - yes, it seems to, and it also runs a lot faster!
		
		head(DAYTOTALS)
		head(TESTING)
		DAYTOTALS <- TESTING
		
		DAYTOTALS[1000:10010,]
		tail(DAYTOTALS)
		
		colnames(DAYTOTALS) <- c("date","country","house","frequency")
		head(DAYTOTALS)
		DAYTOTALS$date <- as.Date(DAYTOTALS$date)
		
		
		
	
	# also get the daytotals for all members
	#	EVERYBODYCOUNTS <- as.data.frame(table(BU$day,BU$country)) ### THIS NEEDS TO CHANGE
		TESTINGTWO <- as.data.frame(BU %>% count(day, country, house))
		head(EVERYBODYCOUNTS)
		head(TESTINGTWO)
		EVERYBODYCOUNTS <- TESTINGTWO
		
		colnames(EVERYBODYCOUNTS) <- c("date","country","house","numberofmps")
		EVERYBODYCOUNTS$date <- as.Date(EVERYBODYCOUNTS$date)
		EVERYBODYCOUNTS[20000:20010,]
		EVERYBODYCOUNTS[50000:50010,]
		EVERYBODYCOUNTS[70000:70010,]
		tail(EVERYBODYCOUNTS) # this does not look good, what is up with these last NL entries?
		
	
	# merge these two together
		TEMP2 <- sqldf("SELECT DAYTOTALS.*, EVERYBODYCOUNTS.numberofmps
						FROM DAYTOTALS LEFT JOIN EVERYBODYCOUNTS
						WHERE 
							(
							DAYTOTALS.date = EVERYBODYCOUNTS.date 
							AND
							DAYTOTALS.country = EVERYBODYCOUNTS.country
							AND
							DAYTOTALS.house = EVERYBODYCOUNTS.house
							)
						")
		
	# calculate the percentage
		TEMP2$percentagecoremembers <- (TEMP2$frequency / TEMP2$numberofmps) * 100
		summary(TEMP2$percentagecoremembers)
		hist(TEMP2$percentagecoremembers)
	
	# reduce to drop very recent observations
		TEMP3 <- TEMP2[which(TEMP2$date < as.Date("2017-12-31")),] # this takes care of some of the issues that where identified above with [[rcen]] RESE dates on parliamentary membership
		nrow(TEMP3)
		head(TEMP3)
		
	ggplot(TEMP3, aes(date, percentagecoremembers, linetype= house)) + 
       geom_line(size=1.03)+
	   facet_grid(country ~ .) +
	   ylab("%  MPs also there at the first day of parliament") +
	   theme_pubr(base_size = 18,
					base_family = "",
					border = FALSE,
					margin = TRUE,
					legend = c(0.895,0.8),
					x.text.angle = 0) +
		grids(linetype = "dashed",axis="y")

	#########################################
	### party membership
	#########################################
	

	# so now we have done this... how to do this for party membership?
	
		# I would say: for each day we merge in the current party, and then we filter - and this is a little different ..
		 # ..if your party is still them same party as it was when you entered (does not need to be the startdate of the parliament!) then you can stay in the data, 
		 # then - as above - we can use an %in% to for each day only select people that meet this criteria

		# step 1: get a data-frame that contains - for each parliament(!) - the first day you entered this parliament
		
			head(BU)
			BU <- data.frame(BU)
			BU$fake_parl_episode_id <- paste0(BU$pers_id,BU$parliament_id)
		
			FIRSTPARLDAY <- sqldf("SELECT BU.*, MIN(day) as 'first_day'
								   FROM BU
								   GROUP BY fake_parl_episode_id
								 ")
			
			FIRSTPARLDAY$first_day_asdate <- as.Date(FIRSTPARLDAY$first_day,origin="1970-01-01")
			head(FIRSTPARLDAY)
			nrow(FIRSTPARLDAY)
		
		# step 2: merge this into BU
			
			TEMPA <- sqldf("SELECT BU.*, FIRSTPARLDAY.first_day_asdate
						   FROM BU LEFT JOIN FIRSTPARLDAY
						   ON
						   BU.fake_parl_episode_id = FIRSTPARLDAY.fake_parl_episode_id
						 ")		
			head(TEMPA)
			tail(TEMPA)
			nrow(BU)
			nrow(TEMPA)
			BU <- TEMPA
			TEMPA <- BU
			head(BU)
			
		# step 3: get the party ON THE FIRST DAY
		
			# for this we first need to read in MEME
					MEME = read.csv("./source_csvs/MEME.csv", header = TRUE, sep = ";")
					head(MEME)
		
			# get the data that I need for this cleaned
			
				# deal with left and right censored dates
					MEME$memep_startdate_cleaned <- gsub("[[rcen]]","",MEME$memep_startdate,fixed=TRUE)
					MEME$memep_startdate_cleaned <- gsub("[[lcen]]","",MEME$memep_startdate_cleaned,fixed=TRUE)
					MEME$memep_enddate_cleaned <- gsub("[[rcen]]","",MEME$memep_enddate,fixed=TRUE)
					MEME$memep_enddate_cleaned <- gsub("[[lcen]]","",MEME$memep_enddate_cleaned,fixed=TRUE)
					
					table(nchar(MEME$memep_startdate_cleaned)) # so, no issue yet, but will be when new IMPORT has been added
					table(nchar(MEME$memep_enddate_cleaned)) # so, no issue yet, but will be when new IMPORT has been added
					
				# deal with dates that are only years (select 1th of June)			
					MEME$memep_startdate_cleaned <- ifelse(nchar(MEME$memep_startdate_cleaned) == 4,paste("01jun",MEME$memep_startdate_cleaned,sep=""),MEME$memep_startdate_cleaned)
					MEME$memep_enddate_cleaned <- ifelse(nchar(MEME$memep_enddate_cleaned) == 4,paste("01jun",MEME$memep_enddate_cleaned,sep=""),MEME$memep_enddate_cleaned)
				
				# deal with dates that are only months (select 1th of this month)
					MEME$memep_startdate_cleaned <- ifelse(nchar(MEME$memep_startdate_cleaned) == 7,paste("15",MEME$memep_startdate_cleaned,sep=""),MEME$memep_startdate_cleaned)
					MEME$memep_enddate_cleaned <- ifelse(nchar(MEME$memep_enddate_cleaned) == 7,paste("15",MEME$memep_enddate_cleaned,sep=""),MEME$memep_enddate_cleaned)
				
					table(nchar(MEME$memep_startdate_cleaned)) 
					table(nchar(MEME$memep_enddate_cleaned))
					
					MEME[which(nchar(MEME$memep_enddate_cleaned) == 2),] # so these are NA's... lets make these NA
					
					MEME$memep_startdate_cleaned[which(MEME$memep_startdate_cleaned == "NC")] <- NA
					MEME$memep_enddate_cleaned[which(MEME$memep_enddate_cleaned == "NC")] <- NA
				
				# when the start data of an MP its first date is NA, use the birthday
				
					# OK, so the clean that needs to happen here really is quite complicated... We have the following scenarios
						# scenario A: there is only one line for an MP and both the start and the end date are NA : we use the 18th birthday and the 65th birthday
						# scenario B1: the start date of the first entry is missing#
						# scenario B2: the end of the last entry is missing
						# scenario C1: the start date is missing, but there is an earlier entry with an end date
						# scenario C1a: the start date is missing, and there is an earlier entry but it does not have an end date
						
						# scenario C2: the end date is missing, but there is an later entry with an start date
						# scenario C2a: the end date is missing, yet there NO later entry with an start date
						
						#  > please not that in Oliver' integrity scripts all missing start and end dates are simply set the 18th and 65th birthday.. potential issue with that is that MPs can then have more then one party membership at the same time, lets see how many cases would not deal well with this solution?
						
						if(FALSE)
						{
							# how many are their of me?
							MEME[which(is.na(MEME$memep_enddate_cleaned)),]
							
							resvec <- vector()
							pb <- txtProgressBar(min = 1, max = nrow(MEME), style = 3)
							for(i in 1:nrow(MEME))
							{
								mypersid <- MEME$pers_id[i]
								resvec[i] <- length(MEME$pers_id[which(MEME$pers_id == mypersid)])
								setTxtProgressBar(pb, i)
							}
							close(pb)
							# resvec
							MEME$howmanyofme <- resvec
							head(MEME)
						
							# among these, select some cases with NA'safe
							MEME$anydateNAS <- ifelse(is.na(MEME$memep_startdate_cleaned)|is.na(MEME$memep_enddate_cleaned),TRUE,FALSE)
						
							# display potential cases for which this kind of date fixing might not be great
							MEME[which(MEME$howmanyofme > 1 & MEME$anydateNAS),]
							
							# what do we see?
								
								# fist, a lot of this seems to be due to the lack of a right censored date for a bunch of people in CH - new data from Elena?
								
									# lets focus this on people that are actually in the the BU data
									CHECKME <- MEME[which(MEME$howmanyofme > 2 & MEME$anydateNAS & MEME$pers_id %in% BU$pers_id),]
									
									# in most of the swss cases, taking the day before the start date of the next entry seems to do a lot (this could be step one)
									
										# make an internal R-date
											MEME$memep_startdate_cleaned_dateformat <- as.Date(fast_strptime(as.character(MEME$memep_startdate_cleaned),"%d%b%Y"))
											MEME$memep_enddate_cleaned_dateformat <- as.Date(fast_strptime(as.character(MEME$memep_enddate_cleaned),"%d%b%Y"))
									
									
										# some vars that will be needed
											MEME$birthyear <- substrRight(as.character(MEME$pers_id),4)
											head(MEME)
									
										# set the '18' if there are no earlier entries
										i = 1
										pb <- txtProgressBar(min = 1, max = nrow(MEME), style = 3)
										for(i in 1:nrow(MEME))
										{

											# if my start is NA, and there are no earlier entries
											
												# how do I know if there are earlier entries? : if there are any other entries with earlier start or end dates
												
												# get a data-frame with just me and just me without the current line
													MEHERE <- MEME[which(MEME$pers_id == MEME$pers_id[i]),]
													MEHEREWITHOUTME <- MEHERE[which(!MEHERE$memep_id == MEHERE$memep_id[i]),]
												
												
												# get my own end and the lowest dates elsewhere
													myenddate <- MEME$memep_enddate_cleaned_dateformat[i]
													loweststartdatefromotherentries <- min(MEHEREWITHOUTME$memep_startdate_cleaned_dateformat,na.rm=TRUE)
													lowestenddatefromotherentries <- min(MEHEREWITHOUTME$memep_enddate_cleaned_dateformat,na.rm=TRUE)
													lowestdateboth <- min(loweststartdatefromotherentries,lowestenddatefromotherentries)
												
												# check conditions: are their any other entries with earlier start or end dates
												if (!is.na(myenddate) | is.na(lowestdateboth)) # can we do the check in the first place?
												{
													if(is.na(myenddate) & myenddate < lowestdateboth)
													{
														MEME$memep_startdate_cleaned[i] <-  paste("01jan",as.character(as.numeric(as.character(MEME$birthyear[i]))+18),sep="")
													}
												}
										setTxtProgressBar(pb, i) 
										}
										close(pb)
						}
				
				# make an internal R-date
					MEME$memep_startdate_cleaned_dateformat <- as.Date(fast_strptime(as.character(MEME$memep_startdate_cleaned),"%d%b%Y"))
					MEME$memep_enddate_cleaned_dateformat <- as.Date(fast_strptime(as.character(MEME$memep_enddate_cleaned),"%d%b%Y"))
					head(MEME)
			
			# !! please note!! for a final version a bit more data-cleaning e.t.c. should be done here to reduce missingness! -- currently only based on MEME.. is that the same for Oliver his 'party_id_national' variable?
			TEMPB <- sqldf("
						SELECT BU.*, MEME.party_id as 'party_at_start'
						FROM BU LEFT JOIN MEME
						ON 
							BU.pers_id = MEME.pers_id
							AND
								(
								BU.first_day_asdate >= MEME.memep_startdate_cleaned_dateformat
								AND
								BU.first_day_asdate <= MEME.memep_enddate_cleaned_dateformat
								)
						")
			head(TEMPB)
			tail(TEMPB)
			table(TEMPB$party_at_start)
			table(is.na(TEMPB$party_at_start)) # ! so ! note for later that there is a bit of missingness here that needs to be dealth with, ! (excluded these cases) below already for the normalisation accross countries! ## this needs to be brought up with Oliver and Elena!  -- for CH this includes regional variations as well!
			
			# and how does this lool like in Oliver' data?
			head(TEMPB)
			table(is.na(TEMPB$party_id_national)) # missing for almost the exact some cases it seems, let inspect some of these
			
			head(TEMPB[is.na(TEMPB$party_at_start),])
			tail(TEMPB[is.na(TEMPB$party_at_start),])
			
			head(TEMPB[is.na(TEMPB$party_id_national),])
			tail(TEMPB[is.na(TEMPB$party_id_national),])
			
			# how many different people are we actuallt talking about?
			
			missingpersvec <- unique(TEMPB[is.na(TEMPB$party_at_start),]$pers_id) # alright, so about 130 people, all Dutch! -- interesting they do all not seem to have any MEME entries at all?! -- they do not even occur in POLI ?! -- are these people that where somehow missed in Tomas' origional data?! and later only added to RESE but not anywhere else it seems! 
			
				# so, what needs to be added for these people? - for the day by day paper that is!
				
					# 1. their POLI entry!
					# 2. their MEME entry! > looks like this can be taken from the file below!
					
					# taking this vector so I can focus on updating these cases
					library("xlsx")
					write.xlsx(as.data.frame(missingpersvec), "missings_pers.xlsx", sheetName = "missing_pers",  col.names = FALSE, row.names = FALSE, append = FALSE)
					
					# see C:\Users\turnerzw\Basel Powi Dropbox\NL_data\Additional PCC updates from PDC data for day by day paper <-- suggestion is we did do MEME updates?! They are there, so why where they never added to MEME in PCC?
			
					POLI = read.csv("./source_csvs/POLI.csv", header = TRUE, sep = ";")
					head(POLI)
					missingpersvec[which(!missingpersvec %in% POLI$pers_id)] # OK, so they all exist in POLI, and in MEME?
					missingpersvec[which(!missingpersvec %in% MEME$pers_id)] # all, expect one(!) do NOT exist in MEME, so we can focus on preparing an IMPORT for that file now!
					
					
					
			
			unique(TEMPB[is.na(TEMPB$party_id_national),]$pers_id) # alright, so about 130 people, all Dutch!
			
			# lets also see below what this looks like with the further data limitations!
			
			nrow(BU)
			nrow(TEMPB)
			
			BU <- TEMPB
			
			# is it possible that at this stage some MPs get assigned multiple parties? - yes that does seem to happen, do I deal with this below?
			
		# step 4: get the party ON THE DAY THAT SPECIFIES THE ROW in BU itself
		
			TEMPC <- sqldf("
						SELECT BU.*, MEME.party_id as 'party_now'
						FROM BU LEFT JOIN MEME
						ON 
							BU.pers_id = MEME.pers_id
							AND
								(
								BU.day >= MEME.memep_startdate_cleaned_dateformat
								AND
								BU.day <= MEME.memep_enddate_cleaned_dateformat
								)
						")
			head(TEMPC)
			tail(TEMPC)
			table(TEMPC$party_now)
			#table(is.na(TEMPC))
			nrow(BU)
			nrow(TEMPC)
			BU <- TEMPC
			#rm(TEMPC)
			table(is.na(BU$party_at_start))
			table(is.na(BU$party_now)) # the missingness number is very simular here!
			
		# step 5: select the stable party members (i.e.: people that have the same party on this day as they had when they entered parliament)
			#	STAPAME <- sqldf("SELECT BU.*
			#				      FROM BU
			#					  WHERE BU.party_now = BU.party_at_start
			#					")
			#	nrow(STAPAME) # same result as below!
				# or is this faster? > yes it is
				STAPAME <- BU[which(BU$party_now == BU$party_at_start),]
				nrow(STAPAME)
				head(STAPAME)
				nrow(STAPAME)
				
			# I am going to only save what is needed here, and reset R.
				
				# so, we need to safe STAPAME, BU (and that's it?)
					save(STAPAME, BU, file = "stuff.RData")
					
					rm(list = ls())
					.rs.restartR()
					
					# reload again
					
						library(sqldf)
						library(lubridate)
						library(ggplot2)
						library(ggpubr)
						
						library(stringi)
						library(purrr)
						library(tidyverse)
						
						setwd("C:/Users/turnerzw/Basel Powi Dropbox/Data_Paper_DFs")
						load("stuff.RData")			
			
			# there are probably double party memberships in here?!
			
			#	STAPAME$pers_id_day <- paste(STAPAME$pers_id,STAPAME$day,sep="_") # do get a memory exhausted error here
				gc()
				STAPAME <- data.table(STAPAME)
				STAPAME$pers_id_day <- paste0(STAPAME$pers_id,STAPAME$day)	
			#	STAPAME %>% unite("pers_id_day",pers_id:day,sep="_")
				head(STAPAME)
				STAPAME <- as.data.frame(STAPAME)
				head(STAPAME)
						
			if(false) # commenting this out so we are not wasting memory space on it.
			{
				# investigating this issues
					head(STAPAME)
					
					gc()
					length(STAPAME$pers_id_day)
					length(unique(STAPAME$pers_id_day)) # so, there are indeed doubles, lets find one:
					
					STAPAMEDB <- STAPAME[duplicated(STAPAME$pers_id_day),]
					head(STAPAMEDB)
					tail(STAPAMEDB)
					
					# are the parties always the same? > good integrity check
					pers_id_day_vec <- unique(STAPAMEDB$pers_id_day)
					length(pers_id_day_vec)
					
					party_at_start_error_vec <- rep(0,length(pers_id_day_vec))
					party_now_error_vec <- rep(0,length(pers_id_day_vec))
					
					pb <- txtProgressBar(min = 1, max = length(pers_id_day_vec), style = 3)
					for(i in 1:length(pers_id_day_vec))
					{
						# get subset
						RED <- STAPAMEDB[which(STAPAMEDB$pers_id_day == pers_id_day_vec[i]),]
						rm(STAPAMEDB)
						
						# check of more then one party at start occurs
						if (length(table(RED$party_at_start))>1) 
						{
							party_at_start_error_vec[i] <- 1
						}
						
						# check if more then one different party now occurs
						if (length(table(RED$party_now))>1) 
						{
							party_now_error_vec[i] <- 1
						}
					setTxtProgressBar(pb, i)
					}
					close(pb)
					table(party_at_start_error_vec) # always the same
					table(party_now_error_vec) # always the same
					# conclusion: we can just use the first occurence
			}
			
				# fixing the issue of double party membership days by just selecting the first party and seeing if that changed, if not it is fine for us to count this as 'has not changed'
					nrow(STAPAME)
					length(unique(STAPAME$pers_id_day))
					STAPAME <- STAPAME[!duplicated(STAPAME$pers_id_day), ]
					nrow(STAPAME)
					length(unique(STAPAME$pers_id_day)) # looking good
					
		# and a check
		
			nrow(BU[which(BU$pers_id == "NL_Wilders_Geert_1963"),])
			nrow(STAPAME[which(STAPAME$pers_id == "NL_Wilders_Geert_1963"),]) # should be less, and it is
			nrow(STAPAME)
		
		# step 6. tabulate the whole thing
			STAPAME$country <- substr(STAPAME$pers_id,0,2)
			# SPMTAB <- as.data.frame(table(STAPAME$day,STAPAME$country)) # actually get a memory allocation error with this older setup, so good to have the new tidyverse way
			AA <- data.table(as.data.frame(STAPAME %>% count(day, country, house)))
			
			# does his look the same?
			head(STAPAME)
			head(AA)
			SPMTAB <- AA
			
			colnames(SPMTAB) <- c("date","country","house","numberofpartystablemps")
			SPMTAB$date <- as.Date(SPMTAB$date)
			head(SPMTAB)
			tail(SPMTAB)
			SPMTAB[20000:20010,]
			SPMTAB[50000:50010,]
			SPMTAB[70000:70010,]
			
			# inspect the to high counts in the later years
			
			if(FALSE)
			{
				# is the number of stable party members to high? > yes it is!
				gc()
				SPMTABCHINSP # so, the numbers are indeed to high here (larger then 200 -- why?!)
				
				# and/or is the number of all party members to small? > this looks alright!
				head(BU)
				CHECKING <- as.data.frame(table(BU$day,BU$country))
				colnames(CHECKING) <- c("date","country","number")
				CHECKING$date <- as.Date(CHECKING$date)
				head(CHECKING)
				TEMPRES <- CHECKING[which(CHECKING$country == "CH" & CHECKING$date > as.Date("2001-01-01") & CHECKING$date < as.Date("2002-01-01")),]
				TEMPRES # this looks fine!
			}
		# step 7, and a reduced EVERYBODYCOUNTS data-frame that does not include days for which either the party at start of party now is missing in MPDAYLONG
		
			MPDAYLONGMISSINGCASES <- BU[which(is.na(BU$party_at_start) | is.na(BU$party_now)),]
			# MPDAYLONGMISSINGCASES$persidandday <- stri_join(MPDAYLONGMISSINGCASES$pers_id,as.character(MPDAYLONGMISSINGCASES$day),sep="") # should we add the house here as well?
			MPDAYLONGMISSINGCASES <- data.table(MPDAYLONGMISSINGCASES)
			MPDAYLONGMISSINGCASES$persidandday <- paste0(MPDAYLONGMISSINGCASES$pers_id,MPDAYLONGMISSINGCASES$day)
			
			gc()
			head(MPDAYLONGMISSINGCASES)
			nrow(MPDAYLONGMISSINGCASES)
			gc()
			
			BU <- data.table(BU)
			BU$persidandday <- paste0(BU$pers_id,BU$day)
			# BU$persidandday <- stri_join(BU$pers_id,as.character(BU$day),sep="")
			gc()
			head(BU)
			
			nrow(BU)
			BURED2 <- BU[which(!(BU$persidandday %in% MPDAYLONGMISSINGCASES$persidandday)),] #  I guess we have to make this reduction standerat specific 
			nrow(BURED2) # indeed some reduction
			rm(MPDAYLONGMISSINGCASES)
			gc()
			
			# issue with Staenderat cases still left in here? 
			table(BURED2$parliament_id) # nope, does not seem to be the problem
			
			# create new totals
				# EVERYBODYCOUNTSRED <- as.data.frame(table(BURED2$day,BURED2$country))
				EVERYBODYCOUNTSRED <- as.data.frame(BURED2 %>% count(day, country, house))
				
				colnames(EVERYBODYCOUNTSRED) <- c("date","country","house","numberofmps")
				EVERYBODYCOUNTSRED$date <- as.Date(EVERYBODYCOUNTSRED$date)
				EVERYBODYCOUNTSRED[20000:20010,]
				EVERYBODYCOUNTSRED[50000:50010,]
				EVERYBODYCOUNTSRED[70000:70010,]
				tail(EVERYBODYCOUNTSRED)
				head(EVERYBODYCOUNTSRED)
			#	rm(BURED2)
				gc()
			
			
			# inspect how complete this data is
				ggplot(NULL) +  geom_line(data=EVERYBODYCOUNTSRED, aes(x=date, y=numberofmps, linetype=house)) + facet_grid(country ~ .)
				
			# merge these in also here to calculate percentages
					TEMPD <- sqldf("SELECT SPMTAB.*, EVERYBODYCOUNTSRED.numberofmps 	
						FROM SPMTAB LEFT JOIN EVERYBODYCOUNTSRED
						WHERE 
							(
							SPMTAB.date = EVERYBODYCOUNTSRED.date 
							AND
							SPMTAB.country = EVERYBODYCOUNTSRED.country
							AND
							SPMTAB.house = EVERYBODYCOUNTSRED.house
							)
						")
				#	rm(EVERYBODYCOUNTSRED)
					gc()
					head(TEMPD)
					tail(TEMPD)
			
			# calculate the percentage
				TEMPD$percentagestayingpartymembers <- (TEMPD$numberofpartystablemps / TEMPD$numberofmps) * 100
				summary(TEMPD$percentagestayingpartymembers)
				hist(TEMPD$percentagestayingpartymembers)
				TEMPD[which(TEMPD$percentagestayingpartymembers > 200),]
			
			# reduce to drop very recent and very old observations
				TEMPE <- TEMPD[which(TEMPD$date < as.Date("2017-12-31") & TEMPD$date > as.Date("1950-01-01")),]
				rm(TEMPD)
				gc()
				nrow(TEMPE)
				head(TEMPE)
				tail(TEMPE)
				
			ggplot(TEMPE, aes(date, percentagestayingpartymembers, linetype = house)) + 
			   geom_line(size=1.03)+
			   facet_grid(country ~ .) +
			   facet_grid(country ~ .) +
			   ylab("%  MPs in same party as when they entered") +
			   ylim(c(60,100)) +
			   theme_pubr(base_size = 18,
					base_family = "",
					border = FALSE,
					margin = TRUE,
					legend = c(0.075,0.8),
					x.text.angle = 0) +
					grids(linetype = "dashed",axis="y")
			   
			   
			   coord_trans(y="log2")
			   
			 	ggplot(TEMP3, aes(date, percentagecoremembers, linetype= house)) + 
       geom_line(size=1.03)+
	   facet_grid(country ~ .) +
	   ylab("%  MPs also there at the first day of parliament") +
	   theme_pubr(base_size = 18,
					base_family = "",
					border = FALSE,
					margin = TRUE,
					legend = c(0.895,0.8),
					x.text.angle = 0) +
		grids(linetype = "dashed",axis="y")
				
	#########################################
	### party group / faction membership
	#########################################
	
		# we already have a data-frame with the first day of the parliament
			head(TEMPA)
			
		# lets get a 'faction at start' variable
		
			# I am afraid that how to do this differs between countries? - in the Netherlands this is just the party membership?!
			
				# we just copy the party membership data and r-bind it at some point
			
			# In Germany and Switserland, all of this is in RESE
				
				# step 1: get a version of RESE with only the faction cases, prepare dates e.t.c.
				
					# Load RESE
					RESE = read.csv("./source_csvs/RESE.csv", header = TRUE, sep = ";")
					summary(RESE)
					names(RESE)
					head(RESE)
				
					# 
					nrow(RESE)
					RESEFAC <- RESE[which(nchar(as.character(RESE$faction_id_core)) > 0),]
					nrow(RESEFAC)
					table(RESEFAC$faction_id_core)
					
					# lets get a simplified version of these ids, and if there are multiple collapse them
					
						# get rid of the first parliament id
							RESEFAC$faction_id_core_simple <- gsub("^.*?__","",RESEFAC$faction_id_core,perl=T)
							table(RESEFAC$faction_id_core_simple)
							
						# and subsequent ones
							RESEFAC$faction_id_core_simple <- gsub("(?<=;).*?__","",RESEFAC$faction_id_core_simple,perl=T)
							table(RESEFAC$faction_id_core_simple)
						
						# get rid of the __ at the end e.t.c.
							RESEFAC$faction_id_core_simple <- gsub("__[0-9]","",RESEFAC$faction_id_core_simple,perl=T)
							table(RESEFAC$faction_id_core_simple)
						
						# collapse values if they are the same
							
							# example
							AAA <- RESEFAC[which(grepl(";",RESEFAC$faction_id_core_simple)),]
							head(AAA)
							
							strsplit(RESEFAC$faction_id_core_simple[which(RESEFAC$res_entry_id == "CH_Amstutz_Adrian_1953__23")],";",fixed=TRUE)
							table(strsplit(RESEFAC$faction_id_core_simple[which(RESEFAC$res_entry_id == "CH_Amstutz_Adrian_1953__23")],";",fixed=TRUE))
							length(table(strsplit(RESEFAC$faction_id_core_simple[which(RESEFAC$res_entry_id == "CH_Amstutz_Adrian_1953__23")],";",fixed=TRUE))) # are there sometimes combined values?
							names(table(strsplit(RESEFAC$faction_id_core_simple[which(RESEFAC$res_entry_id == "CH_Amstutz_Adrian_1953__23")],";",fixed=TRUE)))[1] # select the first value
							
							# in a loop
							
							length_vec <- vector()
							faction_simplest_vec <- vector()
							
							for(i in 1:nrow(RESEFAC))
							{
								length_vec[i] <- length(table(strsplit(RESEFAC$faction_id_core_simple[i],";",fixed=TRUE))) # are there combined values?
								faction_simplest_vec[i] <- names(table(strsplit(RESEFAC$faction_id_core_simple[i],";",fixed=TRUE)))[1] # select the first value
							}
							table(length_vec) # only one problem case
							table(faction_simplest_vec)

							# write this simplified verson away
							RESEFAC$faction_simplest <- faction_simplest_vec
														
				# step 2: for the pers_id and day combinations in TEMPA (MPDAYLONG), get the 'faction_id_core' they where in
					
					# get the data that I need for this cleaned
					
						# and deal with censoring, incomplete dates and make internal R-date
							RESEFAC$res_entry_end <- gsub("[[rcen]]","",RESEFAC$res_entry_end,fixed=TRUE)
							RESEFAC$res_entry_start <- gsub("[[lcen]]","",RESEFAC$res_entry_start,fixed=TRUE)
							RESEFAC$res_entry_start_dateformat <- as.Date(as.POSIXct(RESEFAC$res_entry_start,format=c("%d%b%Y"),tz="CET"))
							RESEFAC$res_entry_end_dateformat <- as.Date(as.POSIXct(RESEFAC$res_entry_end,format=c("%d%b%Y"),tz="CET"))
						
							# later dealing with incomplete dates should be added here!
							table(is.na(RESEFAC$res_entry_start_dateformat))
							table(is.na(RESEFAC$res_entry_end_dateformat))
							dput(RESEFAC$res_entry_start_dateformat[1])
							dput(RESEFAC$res_entry_end_dateformat[1])
							dput(TEMPA$first_day_asdate[1])

							RESEFAC[which(is.na(RESEFAC$res_entry_end_dateformat)),]
							
							head(RESEFAC)
							
							# temp fix
							
							TEMPA$first_day_asdate <- as.Date(TEMPA$first_day_asdate)
							TEMPA$pers_id <- as.character(TEMPA$pers_id)
							RESEFAC$pers_id <- as.character(RESEFAC$pers_id) 
							
							typeof(TEMPA$first_day_asdate)
							typeof(RESEFAC$res_entry_start_dateformat)
							typeof(RESEFAC$res_entry_end_dateformat)
							
							typeof(TEMPA$pers_id)
							typeof(RESEFAC$pers_id)
					
				###
				## get the faction at the start
				###
				
					PGST <- sqldf("
								SELECT TEMPA.*, RESEFAC.faction_simplest as 'faction_at_start'
								FROM TEMPA LEFT JOIN RESEFAC
								ON 
									TEMPA.pers_id = RESEFAC.pers_id
									AND
										(
										TEMPA.first_day_asdate >= RESEFAC.res_entry_start_dateformat
										AND
										TEMPA.first_day_asdate <= RESEFAC.res_entry_end_dateformat
										)
								",dbname=tempfile())
					nrow(PGST)
					head(PGST)
				#	table(PGST$faction_at_start)
				#	table(is.na(PGST$faction_at_start)) # quite a bit is missing cases still (in percentages:)
				#	table(is.na(PGST$faction_at_start))[2] / (table(is.na(PGST$faction_at_start))[2]+table(is.na(PGST$faction_at_start))[1]) # about 17%
					
					
					# lets try one manual match
				#	RESEFAC[which(RESEFAC$pers_id == "CH_Abate_Fabio_1966"),]
				#	RESEFAC[which(RESEFAC$pers_id == "CH_Abate_Fabio_1966" & as.Date("2000-09-25") >= RESEFAC$res_entry_start_dateformat & as.Date("2000-09-25") <= RESEFAC$res_entry_end_dateformat),]
				
				###
				## get the faction now
				###
				
					PGST <- sqldf("
								SELECT PGST.*, RESEFAC.faction_simplest as 'faction_now'
								FROM PGST LEFT JOIN RESEFAC
								ON 
									PGST.pers_id = RESEFAC.pers_id
									AND
										(
										PGST.day >= RESEFAC.res_entry_start_dateformat
										AND
										PGST.day <= RESEFAC.res_entry_end_dateformat
										)
								")
					head(PGST)
					nrow(PGST)

				## get rid of potential duplicates (just as above)
					
					# fixing the issue of double party membership days
					
						# inspect
						#	PGST$pers_id_day <- paste(PGST$pers_id,PGST$day,sep="_")
							PGST$pers_id_day <- stri_join(PGST$pers_id,PGST$day,sep="_")
							gc()
							nrow(PGST)
							length(unique(PGST$pers_id_day)) # couple of cases
					
						# fix
							PGST <- PGST[!duplicated(PGST$pers_id_day), ]
							nrow(PGST)
							
				## get a data-frame with only these days in which faction_as_start and faction_now are the same
					STAFAME <- PGST[which(PGST$faction_now == PGST$faction_at_start),]
					nrow(STAFAME)
					head(STAFAME)
				
				#################
				## step 6. tabulate the whole thing to get numbers per day for the stable faction members as well as for a (reduced because of missing faction membership information!) sample of all of the members
				#################
				
					# get the counts tabulated (per day and country) for the stables members
					STAFAME$country <- substr(STAFAME$pers_id,0,2)
					FAMETAB <- as.data.frame(table(STAFAME$day,STAFAME$country))
					gc()
					colnames(FAMETAB) <- c("date","country","numberofpartygroupstablemps")
					FAMETAB$date <- as.Date(FAMETAB$date)
					head(FAMETAB)
					tail(FAMETAB)
					FAMETAB[20000:20010,]
					FAMETAB[50000:50010,]
					
					# get the count tabulated (per day and country) for the all the members
					
						##  first we need tp get 'all mps' data with only the none-missing cases
				
							# get a data-frame with only those cases for which we do NOT have this info
							MPDAYLONGMISSINGFAMECASES <- PGST[which(is.na(PGST$faction_at_start) | is.na(PGST$faction_now)),]
							MPDAYLONGMISSINGFAMECASES$persidandday <- stri_join(MPDAYLONGMISSINGFAMECASES$pers_id,as.character(MPDAYLONGMISSINGFAMECASES$day),sep="")
							gc()
							head(MPDAYLONGMISSINGFAMECASES)
							nrow(MPDAYLONGMISSINGFAMECASES)
							
							# now get rid of these cases
								
								# we first need to be able to match them
									PGST$persidandday <- stri_join(PGST$pers_id,as.character(PGST$day),sep="")
									gc()
									head(PGST)
							
								# then we can get rid of them
									nrow(PGST)
									PGSTRED <- PGST[which(!(PGST$persidandday %in% MPDAYLONGMISSINGFAMECASES$persidandday)),]
									nrow(PGSTRED) # indeed some reduction
					
						## then we can calculate the counts
							PGSTRED$country <- substr(PGSTRED$pers_id,0,2)
							table(PGSTRED$country)
							FAMETABALL <- as.data.frame(table(PGSTRED$day,PGSTRED$country))
							gc()
							colnames(FAMETABALL) <- c("date","country","totalnumberofmps")
							FAMETABALL$date <- as.Date(FAMETABALL$date)
							head(FAMETABALL)
							tail(FAMETABALL)
							FAMETABALL[20000:20010,]
							FAMETABALL[50000:50010,]
				
				#################
				## step 7. merge these two data together
				#################				
				
				# merge these in also here to calculate percentages
					nrow(FAMETAB)
					head(FAMETAB)
					head(FAMETABALL)
					
					dput(FAMETAB$date[1])
					dput(FAMETABALL$date[1])
				
					FAMECOUNTS <- sqldf("SELECT FAMETAB.*, FAMETABALL.totalnumberofmps 	
							FROM FAMETAB LEFT JOIN FAMETABALL
							WHERE 
							(
							FAMETAB.date = FAMETABALL.date 
							AND
							FAMETAB.country = FAMETABALL.country
							)
						")
		
					nrow(FAMECOUNTS) # same number
						
				#################
				## step 8. graphic!
				#################			
						
						# calculate the percentage
							FAMECOUNTS$percentagestayingpartygroupmembers <- (FAMECOUNTS$numberofpartygroupstablemps / FAMECOUNTS$totalnumberofmps) * 100
							summary(FAMECOUNTS$percentagestayingpartygroupmembers)
							hist(FAMECOUNTS$percentagestayingpartygroupmembers)
						
						# reduce to drop very recent and very old observations
							FAMECOUNTSREC <- FAMECOUNTS[which(FAMECOUNTS$date < as.Date("2017-12-31") & FAMECOUNTS$date > as.Date("1950-01-01")),]
							nrow(FAMECOUNTSREC)
							head(FAMECOUNTSREC)
							tail(FAMECOUNTSREC)
							
						# add the dutch party data in here!
						
							head(TEMPE)
							table(TEMPE$country)
							TEMPENL <- TEMPE[which(TEMPE$country == "NL"),]
							nrow(TEMPENL)
							head(TEMPENL)
							
							FAMECOUNTSRECWITHNL <- rbind(FAMECOUNTSREC,TEMPENL)
							
						ggplot(FAMECOUNTSREC, aes(date, percentagestayingpartygroupmembers, color = country)) + 
						   geom_line()+
						   facet_grid(country ~ .) +
						   ylab("%  MPs in same party group as when they entered") +
						   theme_pubclean(base_size = 20) 
				