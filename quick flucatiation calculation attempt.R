# packages

	library(sqldf)
	library(lubridate)
	library(ggplot2)

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
	BU[22700:22720,] # got one, finally

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
		EVERYBODYCOUNTS[20000:20010,]
		EVERYBODYCOUNTS[50000:50010,]
		EVERYBODYCOUNTS[70000:70010,]
		tail(EVERYBODYCOUNTS)
	
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
		TEMP2$percentagecoremembers <- (TEMP2&frequency / TEMP2$numberofmps) * 100

	ggplot(DAYTOTALS, aes(date, frequency, color = country)) + 
       geom_line()	
	