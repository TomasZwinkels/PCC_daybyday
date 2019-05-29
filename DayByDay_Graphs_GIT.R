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

# load libraries
	library(ggplot2)
	library(doParallel)
	library(stringr)
	library(gridExtra)
	library(ggpubr)
	library(googleVis) # for Sankey Diagram (https://databreadandbutter.wordpress.com/2017/09/25/erster-blogbeitrag/)
	library(reshape2) #to reshape data
	library(sqldf)

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

# also get some of the general PCC data-frames in

		# import and inspect PARL and get the election date into the r-format
		PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
		summary(PARL)
		names(PARL)
		PARL$election_date_asdate <- as.Date(as.character(PARL$election_date),format=c("%d%b%Y"))
		head(PARL)

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
		PARLDAILY <- sqldf("SELECT PARLDAILY.*, PARL.election_date_asdate
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
	
	PARLDAILY_CHSR<-data.frame(dfCH$SR)
	PARLDAILY_CHNR<-data.frame(dfCH$NR)
	PARLDAILY_DE<-data.frame(PARLDAILY_countries$DE)
	PARLDAILY_NL<-data.frame(PARLDAILY_countries$NL)
	
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
	
	ggplot(NULL) +
		  geom_line(data=PARLDAILY_NL, aes(x=day, y=seats))
		  
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
		  geom_point(data=IPU_CH, aes(x=rformateddate, y=propwomen),color="green") +
		  scale_y_continuous(name=yname,breaks=ybreaks,labels=ylabels,limits=yrange) +
		  scale_x_date(name="Swiss Nationalrat Day by Day",breaks=xbreaks_CHNR,labels=xlabels_CHNR,limits=xrange) +
		  geom_vline(aes(xintercept=UNI_CHNR$election_date_asdate), linetype=4, colour="black") +
		  theme_grey(base_size = 15) +
		  theme(axis.text.x = element_text(angle = 45, hjust = 1))

	
	#DE
	#genderdaily_DE <- 
		ggplot(NULL) +
		  geom_line(data=PARLDAILY_DE, aes(x=day, y=gender)) +
		  geom_point(data=IPU_DE, aes(x=rformateddate, y=propwomen),color="green") +
		  scale_y_continuous(name=yname,breaks=ybreaks,labels=ylabels,limits=yrange) +
		  scale_x_date(name="German Bundestag Day by Day",breaks=xbreaks_DE,labels=xlabels_DE,limits=xrange) +
		  geom_vline(aes(xintercept=UNI_DE$election_date_asdate), linetype=4, colour="black") +
		  theme_grey(base_size = 15) +
		  theme(axis.text.x = element_text(angle = 45, hjust = 1))
	  

	#NL
	# genderdaily_NL 
		ggplot(NULL) +
		  geom_line(data=PARLDAILY_NL, aes(x=day, y=gender)) +
		  geom_point(data=IPU_DE, aes(x=rformateddate, y=propwomen),color="green") +
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







 ##########
 # Age per year #
 ###############################################################################################################
 #Split parliament id of Parldaily into components
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

##########
# tenure by term #
#########################################################################################################Ñ
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