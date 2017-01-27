
MyMachine <- Sys.info()["nodename"]

if (MyMachine=="P37") {MyDisk="G"
} else {MyDisk="D"
}

MyFile <- paste(MyDisk,":/Dissertation/DataSets/Pruned.csv",sep="")

setwd(paste(MyDisk,":/Dissertation/DataSets",sep=""))
getwd()

#install.packages("readr")
library(readr)
features <- read_csv("pruned.csv",col_names=TRUE)

#rename column
library(plyr)
features <- rename(features,c("SourceFieldClassification"="Class"))
names_features <- names(features)

attach(features)
table(features$Class, FieldClass)
features$Class[features$Class=="ALPHA"]<-"Alpha"
features$Class[features$Class=="NUMERIC"]<-"Numeric"


# 
# 
# 
# ## ===================================================
# ## Basic info and data shape
# ## ===================================================
# 
# # get info about the track data
# dim(features)
# str(features)
# names_features <- names(features)
# 
# mytable <- table(features$SourceFieldClassification, features$PropAlpha)
# 
# mytable
# #write_csv(cbind(names_track), "names_track.csv")
# 
# # # get info about the tv data
# # dim(tv)
# # str(tv)
# # names_tv <- names(tv)
# # #write_csv(cbind(names_tv),"names_tv.csv")
# # 
# # ## ===================================================
# # ## Track ids
# # ## ===================================================
# # # summary of the track_id variable
# # summary(track$track_id)
# # # unique values
# # unique(track$track_id)
# # # frequency count of track_id
# # suz.freq.track_id <- cbind(sort(table(track$track_id), decreasing=T))
# # # create a numeric and do a histogram
# # suz.track_id <- as.numeric(track$track_id)
# # hist(suz.track_id)
# # 
# # # ====================================================
# # # Subset clean track_ids
# # # ====================================================
# # # convert character NULL to R na
# # track$track_id[track$track_id == "NULL"] <- NA
# # # make a subset of the track_id=na
# # tracknulls <- subset(track, is.na(track$track_id))
# # # Method 1. make a subset of the track_ids is not na
# # trackvalid <- subset(track, !is.na(track$track_id))
# # # Method 2. create a dataset filtering out the NULL track_ids
# # trackclean <- track[track$track_id!="NULL",]
# # 
# # # see what these objects look like
# # class(trackclean)
# # class(trackvalid)
# # summary(trackclean$track_id)
# # summary(trackvalid$track_id)
# # 
# # # create a clean numeric and do a histogram
# # suz.track_id <- as.numeric(trackclean$track_id)
# # hist(suz.track_id)
# # 
# # # ====================================================
# # # Subset top 3 most frequent track_ids
# # # ====================================================
# # 
# # # frequency count of track_id, sort descending, top 3 values
# # suz.track_id.top3 <- head(sort(table(trackclean$track_id), decreasing=T),3)
# # 
# # # get the values of the row in the table like this
# # # x <- names(suz.track_id.top3)
# # #tracktop3 <- subset(trackclean, trackclean$track_id %in% c('20', '21', '31'))
# # 
# # # use the result directly to subset on the top 3 values
# # tracktop3 <- subset(trackclean, trackclean$track_id %in% names(suz.track_id.top3))
# # 
# # # Date is in unix UTC format
# # tracktop3$dateHuman <- as.Date(as.POSIXct(tracktop3$date, origin="1970-01-01"))
# # 
# # # write to a csv
# # write_csv(tracktop3,"track_top3.csv")
# # 
# # # just look at track 20
# # track20 <- subset(track, track$track_id==20)
# # track20$dateHuman <- as.Date(as.POSIXct(track20$date, origin="1970-01-01"))
# # tv20 <- subset(tv, tv$track_id==20)
# # 
# # # write csv
# # write_csv(track20,"track_20.csv")
# # write_csv(tv20,"tv_20.csv")
# # 
# # # track20 sales, youtube
# # 
# # mycols <- c("id","dateHuman","preorders","sales","youtube_views","youtube_likes","youtube_subscribers","paid_youtube_impressions","paid_youtube_views","youtube_streams")
# # track20youtube <- track20[mycols]
# # write_csv(track20youtube,"track20youtube.csv")
# # 
# # # youtube cols
# # #suz.cols <- names(track20)
# # #class(suz.cols)
# # #any(suz.cols=="dateHuman")