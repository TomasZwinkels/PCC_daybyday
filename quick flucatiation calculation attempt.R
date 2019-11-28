# packages

	library(sqldf)
	library(lubridate)
	library(ggplot2)
	library(ggpubr)
	

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
	summary(PARL)
	names(PARL)
	head(PARL)


 # merge in the leg_period_start for each parliament
 
	BU <- sqldf("SELECT MPDAYLONG.*, PARL.leg_period_start
			   FROM MPDAYLONG LEFT JOIN PARL
			   ON MPDAYLONG.parliament_id = PARL.parliament_id
			   ORDER BY pers_id, day
			 ")
	head(BU)
	
	BU[22700:22720,] # got one, finally
	
	tail(BU)
	
	# get in the country because it is needed below
		BU$country <- substr(BU$pers_id,0,2)
	
	# get the right date format
		BU$leg_period_start_posix <- as.Date(as.POSIXct(as.character(BU$leg_period_start),format=c("%d%b%Y")))
		BU$leg_period_start_posix <- BU$leg_period_start_posix + days(1) # quick fix
		BU[22700:22720,] # got a range where the first day can be seen, finally

	# now, get rid off all cases that do not meet the following criteria: a row exists in the data-frame where the day is the legistlative period its start date, if this is not the case you where not 'there on the first day'
	
	# this reduces the data to a list of pers_ids and parliament_ids only for those that where there at the first day
	TEMP <-  	  sqldf("SELECT BU.* 
					 FROM BU
					 WHERE BU.day = BU.leg_period_start_posix
					")
	TEMP$persandparliamentid <- paste(TEMP$pers_id,TEMP$parliament_id,sep="__")
	head(TEMP)
					
	# now we can simpy only include lines of the main data that occur in TEMP as well (if they don't occur in TEMP they are not people that where there on the first day).
	BU$persandparliamentid <- paste(BU$pers_id,BU$parliament_id,sep="__")
	head(BU)
	
	BURED <- BU[which(BU$persandparliamentid %in% TEMP$persandparliamentid),]
	BURED$country <- substr(BURED$pers_id,0,2)
	head(BURED)
	
	# get the daytotals for 'core members'
		DAYTOTALS <- as.data.frame(table(BURED$day,BURED$country))
		DAYTOTALS[1000:10010,]
		tail(DAYTOTALS)
		
		colnames(DAYTOTALS) <- c("date","country","frequency")
		head(DAYTOTALS)
		DAYTOTALS$date <- as.Date(DAYTOTALS$date)
	
	# also get the daytotals for all members
		EVERYBODYCOUNTS <- as.data.frame(table(BU$day,BU$country))
		colnames(EVERYBODYCOUNTS) <- c("date","country","numberofmps")
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
	
	   
	# so now we have done this... how to do this for party membership?
	
		# I would say: for each day we merge in the current party, and then we filter - and this is a little different ..
		 # ..if you party is still them same party as it was when you entered (does not need to be the startdate of the parliament!) then you can stay in the data, 
		 # then - as above - we can use an %in% to for each day only select people that meet this criteria

		# step 1: get a data-frame that contains - for each parliament - the first day you entered this parliament
			head(MPDAYLONG)
			MPDAYLONG$fake_parl_episode_id <- paste(MPDAYLONG$pers_id,MPDAYLONG$parliament_id,sep="__")
		
			FIRSTPARLDAY <- sqldf("SELECT MPDAYLONG.*, MIN(day) as 'first_day'
								   FROM MPDAYLONG
								   GROUP BY fake_parl_episode_id
								 ")
			
			FIRSTPARLDAY$first_day_asdate <- as.Date(FIRSTPARLDAY$first_day,origin="1970-01-01")
			head(FIRSTPARLDAY)
			nrow(FIRSTPARLDAY)
		
		# step 2: merge this into MPDAYLONG
			
			TEMP <- sqldf("SELECT MPDAYLONG.*, FIRSTPARLDAY.first_day_asdate
						   FROM MPDAYLONG LEFT JOIN FIRSTPARLDAY
						   ON
						   MPDAYLONG.fake_parl_episode_id = FIRSTPARLDAY.fake_parl_episode_id
						 ")		
			head(TEMP)
			tail(TEMP)
			MPDAYLONG <- TEMP
		
		# step 3: get the party on this first day
			MEME$memep_startdate_dateformat <- as.Date(as.POSIXct(MEME$memep_startdate,format=c("%d%b%Y"),tz="CET"))
			MEME$memep_enddate_dateformat <- as.Date(as.POSIXct(MEME$memep_enddate,format=c("%d%b%Y"),tz="CET"))
			
			# !! please note!! for a final version a bit more data-cleaning e.t.c. should be done here to reduce missingness!
			

			TEMP <- sqldf("
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
			head(TEMP)
			tail(TEMP)
			table(TEMP$party_at_start)
			table(is.na(TEMP$party_at_start)) # ! so ! not for later that there is a bit of missingness here that needs to be dealth with, !below already for the normalisation accross countries!
			MPDAYLONG <- TEMP
			
		# step 4: get the party on the day that specificies the row in MPDAYLONG itself
		
			TEMP2 <- sqldf("
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
			head(TEMP2)
			tail(TEMP2)
			table(TEMP2$party_now)
			table(is.na(TEMP2$party_now))
			MPDAYLONG <- TEMP2
		
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
		
		# and a check
		
			nrow(MPDAYLONG[which(MPDAYLONG$pers_id == "NL_Wilders_Geert_1963"),])
			nrow(STAPAME[which(STAPAME$pers_id == "NL_Wilders_Geert_1963"),]) # should be less, and it is
		
		# step 6. tabulate the whole thing
			STAPAME$country <- substr(STAPAME$pers_id,0,2)
			SPMTAB <- as.data.frame(table(STAPAME$day,STAPAME$country))
			SPMTAB$date <- as.Date(SPMTAB$date)
			head(SPMTAB)
			tail(SPMTAB)
			SPMTAB[20000:20010,]
			SPMTAB[50000:50010,]
			SPMTAB[70000:70010,]
			colnames(SPMTAB) <- c("date","country","numberofpartystablemps")
			
		# step 7, and a reduced EVERYBODYCOUNTS data-frame that does not include days for which either the party at start of party now is missing in MPDAYLONG
		
			MPDAYLONGMISSINGCASES <- MPDAYLONG[which(is.na(MPDAYLONG$party_at_start) | is.na(MPDAYLONG$party_now)),]
			MPDAYLONGMISSINGCASES$persidandday <- paste(MPDAYLONGMISSINGCASES$pers_id,as.character(MPDAYLONGMISSINGCASES$day),sep="")
			head(MPDAYLONGMISSINGCASES)
			
			BU$persidandday <- paste(BU$pers_id,as.character(BU$day),sep="")
			head(BU)
			
			nrow(BU)
			BURED2 <- BU[which(!(BU$persidandday %in% MPDAYLONGMISSINGCASES$persidandday)),]
			nrow(BURED2)
			
			# create new totals
				EVERYBODYCOUNTSRED <- as.data.frame(table(BURED2$day,BURED2$country))
				colnames(EVERYBODYCOUNTSRED) <- c("date","country","numberofmps")
				EVERYBODYCOUNTSRED$date <- as.Date(EVERYBODYCOUNTSRED$date)
				EVERYBODYCOUNTSRED[20000:20010,]
				EVERYBODYCOUNTSRED[50000:50010,]
				EVERYBODYCOUNTSRED[70000:70010,]
				tail(EVERYBODYCOUNTSRED)
				head(EVERYBODYCOUNTSRED)
				
			
				
			   ggplot(SPMTAB, aes(date, numberofpartystablemps, color = country)) + 
			   geom_line()
			   
			   ggplot(EVERYBODYCOUNTS, aes(date, numberofmps, color = country)) + 
			   geom_line()

				sapply(EVERYBODYCOUNTS, class)
				sapply(SPMTAB, class)