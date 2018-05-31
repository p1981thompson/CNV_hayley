#Little routine to recode IDs for SLIC_df.txt
#DVM Bishop, 31 May 2018

CNV_dat<-read.table('SLIC_df.txt',header=TRUE,stringsAsFactors=FALSE)

#http://www.endmemo.com/program/R/gsub.php explains gsub
myid<-gsub("CONT", "", CNV_dat$id) #remove CONT
mymember <- gsub(".*_","",myid) #retain number after _
myfam<-gsub("_.*","",myid) #retain material before _

w<-which(CNV_dat$Group=='SLIC') #treat SLIC cases differently so get their row nums in w

CNV_dat$fam.member <- NA
CNV_dat$fam.member[-w]<-1 #all controls coded 1
CNV_dat$fam.member[w]<- mymember[w]

CNV_dat$family <- NA #initialise new column

idlist<-unique(myfam) #everyone in same family assigned same family number
for (i in 1: length(idlist)){
  w<-which(myfam==idlist[i])
  CNV_dat$family[w] <- i
}
CNV_anon<-CNV_dat[,c(13,12,2:11)] #reorder the columns
write.table(CNV_anon,'SLIC_df_anon.txt',col.names=TRUE,row.names=FALSE)