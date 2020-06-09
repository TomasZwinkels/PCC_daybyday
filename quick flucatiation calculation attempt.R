# packages

	#install.packages("ggpubr")
	#install.packages("tidyverse")
	install.packages("purrr")

	library(sqldf)
	library(lubridate)
	library(ggplot2)
	library(ggpubr)
	library(stringi)
	library(purrr)
	library(tidyverse)
	
# change the language and date formatting to English if it is not already
	Sys.setenv(LANG = "EN")
	Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
	Sys.getlocale(category = "LC_ALL")

 # working directory
	setwd("C:/Users/turnerzw/Basel Powi Dropbox/Data_Paper_DFs")

 # load the data
	load("Complete_Daily_MP_Data_2019-06-04_1444.Rdata")
	head(MPDAYLONG)
	
	# Load PARL
	PARL = read.csv("./source_csvs/PARL.csv", header = TRUE, sep = ";")
	head(PARL)

# first lets get rid of all the Steanderat entries < this needs to change, 
	nrow(MPDAYLONG)
#	MPDAYLONG <- MPDAYLONG[which(!grepl("CH_NT-SR",MPDAYLONG$parliament_id,fixed=TRUE)),]
	nrow(MPDAYLONG)
	object.size(MPDAYLONG)
	
 # merge in the leg_period_start for each parliament
 
	BU <- sqldf("SELECT MPDAYLONG.*, PARL.leg_period_start, PARL.assembly_abb
			   FROM MPDAYLONG LEFT JOIN PARL
			   ON MPDAYLONG.parliament_id = PARL.parliament_id
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
		BU$leg_period_start_posix <- as.Date(as.POSIXct(as.character(BU$leg_period_start),format=c("%d%b%Y")))
		BU$leg_period_start_posix <- BU$leg_period_start_posix + days(1) # quick fix
		BU[22700:22720,] # got a range where the first day can be seen, finally

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
		DAYTOTALS <- as.data.frame(table(BURED$day,BURED$country)) ### THIS NEEDS TO CHANGE
		TESTING <- as.data.frame(BURED %>% count(day, country, house)) ## DOES THIS GIVE THE SAME RESULT? - yes, it seems to, and it also runs a lot faster!
		
		head(DAYTOTALS)
		head(TESTING)
		
		DAYTOTALS[1000:10010,]
		tail(DAYTOTALS)
		
		colnames(DAYTOTALS) <- c("date","country","house","frequency")
		head(DAYTOTALS)
		DAYTOTALS$date <- as.Date(DAYTOTALS$date)
	
	# also get the daytotals for all members
		EVERYBODYCOUNTS <- as.data.frame(table(BU$day,BU$country)) ### THIS NEEDS TO CHANGE
		colnames(EVERYBODYCOUNTS) <- c("date","country","house","numberofmps")
		EVERYBODYCOUNTS$date <- as.Date(EVERYBODYCOUNTS$date)
		EVERYBODYCOUNTS[20000:20010,]
		EVERYBODYCOUNTS[50000:50010,]
		EVERYBODYCOUNTS[70000:70010,]
		tail(EVERYBODYCOUNTS)
		head(EVERYBODYCOUNTS)
	
	# merge these two together
		TEMP2 <- sqldf("SELECT DAYTOTALS.*, EVERYBODYCOUNTS.numberofmps 	
						FROM DAYTOTALS LEFT JOIN EVERYBODYCOUNTS
						WHERE 
							(
							DAYTOTALS.date = EVERYBODYCOUNTS.date 
							AND
							DAYTOTALS.country = EVERYBODYCOUNTS.country
							)
						")
		
	# calculate the percentage
		TEMP2$percentagecoremembers <- (TEMP2$frequency / TEMP2$numberofmps) * 100
		summary(TEMP2$percentagecoremembers)
		hist(TEMP2$percentagecoremembers)
	
	# reduce to drop very recent observations
		TEMP3 <- TEMP2[which(TEMP2$date < as.Date("2017-12-31")),]
		nrow(TEMP3)
		head(TEMP3)
		
	ggplot(TEMP3, aes(date, percentagecoremembers, color = country)) + 
       geom_line()+
	   facet_grid(country ~ .) +
	   ylab("%  MPs also there at the first day of parliament") +
	   theme_pubclean(base_size = 18)
	

	#########################################
	### party membership
	#########################################
	

	# so now we have done this... how to do this for party membership?
	
		# I would say: for each day we merge in the current party, and then we filter - and this is a little different ..
		 # ..if your party is still them same party as it was when you entered (does not need to be the startdate of the parliament!) then you can stay in the data, 
		 # then - as above - we can use an %in% to for each day only select people that meet this criteria

		# step 1: get a data-frame that contains - for each parliament(!) - the first day you entered this parliament
			head(MPDAYLONG)
			MPDAYLONG$fake_parl_episode_id <- stri_join(MPDAYLONG$pers_id,MPDAYLONG$parliament_id,sep="__")
		
			FIRSTPARLDAY <- sqldf("SELECT MPDAYLONG.*, MIN(day) as 'first_day'
								   FROM MPDAYLONG
								   GROUP BY fake_parl_episode_id
								 ")
			
			FIRSTPARLDAY$first_day_asdate <- as.Date(FIRSTPARLDAY$first_day,origin="1970-01-01")
			head(FIRSTPARLDAY)
			nrow(FIRSTPARLDAY)
		
		# step 2: merge this into MPDAYLONG
			
			TEMPA <- sqldf("SELECT MPDAYLONG.*, FIRSTPARLDAY.first_day_asdate
						   FROM MPDAYLONG LEFT JOIN FIRSTPARLDAY
						   ON
						   MPDAYLONG.fake_parl_episode_id = FIRSTPARLDAY.fake_parl_episode_id
						 ")		
			head(TEMPA)
			tail(TEMPA)
			MPDAYLONG <- TEMPA
			TEMPA <- MPDAYLONG
			head(MPDAYLONG)
			
		# step 3: get the party on this first day
		
			# get the data that I need for this cleaned
			MEME$memep_startdate_dateformat <- as.Date(as.POSIXct(MEME$memep_startdate,format=c("%d%b%Y"),tz="CET"))
			MEME$memep_enddate_dateformat <- as.Date(as.POSIXct(MEME$memep_enddate,format=c("%d%b%Y"),tz="CET"))
			
			# !! please note!! for a final version a bit more data-cleaning e.t.c. should be done here to reduce missingness!
			TEMPB <- sqldf("
						SELECT MPDAYLONG.*, MEME.party_id as 'party_at_start'
						FROM MPDAYLONG LEFT JOIN MEME
						ON 
							MPDAYLONG.pers_id = MEME.pers_id
							AND
								(
								MPDAYLONG.first_day_asdate >= MEME.memep_startdate_dateformat
								AND
								MPDAYLONG.first_day_asdate <= MEME.memep_enddate_dateformat
								)
						")
			head(TEMPB)
			tail(TEMPB)
			table(TEMPB$party_at_start)
			table(is.na(TEMPB$party_at_start)) # ! so ! note for later that there is a bit of missingness here that needs to be dealth with, ! (excluded these cases) below already for the normalisation accross countries!
			MPDAYLONG <- TEMPB
		#	rm(TEMPB)
			
			# is it possible that at this stage some MPs get described mutiple parties?
			
		# step 4: get the party on the day that specificies the row in MPDAYLONG itself
		
			TEMPC <- sqldf("
						SELECT MPDAYLONG.*, MEME.party_id as 'party_now'
						FROM MPDAYLONG LEFT JOIN MEME
						ON 
							MPDAYLONG.pers_id = MEME.pers_id
							AND
								(
								MPDAYLONG.day >= MEME.memep_startdate_dateformat
								AND
								MPDAYLONG.day <= MEME.memep_enddate_dateformat
								)
						")
			head(TEMPC)
			tail(TEMPC)
			table(TEMPC$party_now)
			#table(is.na(TEMPC))
			MPDAYLONG <- TEMPC
		#	rm(TEMPC)
		
		# step 5: select the stable party members (i.e.: people that have the same party on this day as they had when they entered parliament)
			#	STAPAME <- sqldf("SELECT MPDAYLONG.*
			#				      FROM MPDAYLONG
			#					  WHERE MPDAYLONG.party_now = MPDAYLONG.party_at_start
			#					")
			#	nrow(STAPAME) # same result as below!
				# or is this faster? > yes it is
				STAPAME <- MPDAYLONG[which(MPDAYLONG$party_now == MPDAYLONG$party_at_start),]
				nrow(STAPAME)
				head(STAPAME)
				
			# there are probably double party memberships in here?!
			
				# investigating this issues
					head(STAPAME)
					STAPAME$pers_id_day <- stri_join(STAPAME$pers_id,STAPAME$day,sep="_")
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
				
				# fixing the issue of double party membership days
					nrow(STAPAME)
					STAPAME <- STAPAME[!duplicated(STAPAME$pers_id_day), ]
					nrow(STAPAME)
					length(unique(STAPAME$pers_id_day)) # looking good
					
		# and a check
		
			nrow(MPDAYLONG[which(MPDAYLONG$pers_id == "NL_Wilders_Geert_1963"),])
			nrow(STAPAME[which(STAPAME$pers_id == "NL_Wilders_Geert_1963"),]) # should be less, and it is
		
		# step 6. tabulate the whole thing
			STAPAME$country <- substr(STAPAME$pers_id,0,2)
			SPMTAB <- as.data.frame(table(STAPAME$day,STAPAME$country))
			colnames(SPMTAB) <- c("date","country","numberofpartystablemps")
			SPMTAB$date <- as.Date(SPMTAB$date)
			head(SPMTAB)
			tail(SPMTAB)
			SPMTAB[20000:20010,]
			SPMTAB[50000:50010,]
			SPMTAB[70000:70010,]
			
			# inspect the to high counts in the later years
				
				# is the number of stable party members to high? > yes it is!
				SPMTABCHINSP <- SPMTAB[which(SPMTAB$country == "CH" & SPMTAB$date > as.Date("2001-01-01") & SPMTAB$date < as.Date("2002-01-01")),]
				rm(SPMTAB)
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
			
		# step 7, and a reduced EVERYBODYCOUNTS data-frame that does not include days for which either the party at start of party now is missing in MPDAYLONG
		
			MPDAYLONGMISSINGCASES <- MPDAYLONG[which(is.na(MPDAYLONG$party_at_start) | is.na(MPDAYLONG$party_now)),]
			MPDAYLONGMISSINGCASES$persidandday <- stri_join(MPDAYLONGMISSINGCASES$pers_id,as.character(MPDAYLONGMISSINGCASES$day),sep="")
			gc()
			head(MPDAYLONGMISSINGCASES)
			nrow(MPDAYLONGMISSINGCASES)
			rm(MPDAYLONG)
			gc()
			
			BU$persidandday <- stri_join(BU$pers_id,as.character(BU$day),sep="")
			gc()
			head(BU)
			
			nrow(BU)
			BURED2 <- BU[which(!(BU$persidandday %in% MPDAYLONGMISSINGCASES$persidandday)),]
			nrow(BURED2) # indeed some reduction
			rm(MPDAYLONGMISSINGCASES)
			gc()
			
			# issue with Staenderat cases still left in here? 
			table(BURED2$parliament_id) # nope, does not seem to be the problem
			
			# create new totals
				EVERYBODYCOUNTSRED <- as.data.frame(table(BURED2$day,BURED2$country))
				colnames(EVERYBODYCOUNTSRED) <- c("date","country","numberofmps")
				EVERYBODYCOUNTSRED$date <- as.Date(EVERYBODYCOUNTSRED$date)
				EVERYBODYCOUNTSRED[20000:20010,]
				EVERYBODYCOUNTSRED[50000:50010,]
				EVERYBODYCOUNTSRED[70000:70010,]
				tail(EVERYBODYCOUNTSRED)
				head(EVERYBODYCOUNTSRED)
				rm(BURED2)
				gc()
				
			# merge these in also here to calculate percentages
					TEMPD <- sqldf("SELECT SPMTAB.*, EVERYBODYCOUNTSRED.numberofmps 	
						FROM SPMTAB LEFT JOIN EVERYBODYCOUNTSRED
						WHERE 
							(
							SPMTAB.date = EVERYBODYCOUNTSRED.date 
							AND
							SPMTAB.country = EVERYBODYCOUNTSRED.country
							)
						")
					rm(EVERYBODYCOUNTSRED)
					gc()
		
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
				
			ggplot(TEMPE, aes(date, percentagestayingpartymembers, color = country)) + 
			   geom_line()+
			   facet_grid(country ~ .) +
			   ylab("%  MPs in same party as when they entered") +
			   theme_pubclean(base_size = 20) +
			   coord_trans(y="log2")
				
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
				