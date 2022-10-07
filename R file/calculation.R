data = read.csv(file = "D:/Sem2/Data Mining/Data/dataset.csv")
names(data)

attach(data)

Z1.09. =Z1.09.*-1
Z1.10. =Z1.10.*-1
M1.09. =M1.09.*-1
M1.10. =M1.10.*-1
G1.10. =G1.10.*-1
G1.09. =G1.09.*-1

attrackdiffZoom = cbind(Z1.01., Z1.02., Z1.03., Z1.04., Z1.05., Z1.06., Z1.07., Z1.08., Z1.09., Z1.10.)
attrackdiffTeam = cbind(M1.01., M1.02., M1.03., M1.04., M1.05., M1.06., M1.07., M1.08., M1.09., M1.10.)
attrackdiffMeet = cbind(G1.01., G1.02., G1.03., G1.04., G1.05., G1.06., G1.07., G1.08., G1.09., G1.10.)

pragmatic_zoom = rowSums(attrackdiffZoom[,1:4])/4
identity_zoom = rowSums(attrackdiffZoom[,6:7])/2
attractive_zoom = rowSums(attrackdiffZoom[,9:10])/2
stimulation_zoom = (attrackdiffZoom[,5]+attrackdiffZoom[,8])/2

attrackdiffZoom = attrackdiffZoom+4
attrackdiffMeet = attrackdiffMeet+4
attrackdiffTeam = attrackdiffTeam+4

pragmatic_zoom = rowSums(attrackdiffZoom[,1:4])/4
identity_zoom = rowSums(attrackdiffZoom[,6:7])/2
attractive_zoom = rowSums(attrackdiffZoom[,9:10])/2
stimulation_zoom = (attrackdiffZoom[,5]+attrackdiffZoom[,8])/2

zoom = cbind(pragmatic_zoom, identity_zoom, stimulation_zoom, attractive_zoom)

pragmatic_team = rowSums(attrackdiffTeam[,1:4])/4
identity_team = rowSums(attrackdiffTeam[,6:7])/2
attractive_team = rowSums(attrackdiffTeam[,9:10])/2
stimulation_team = (attrackdiffTeam[,5]+attrackdiffTeam[,8])/2

team = cbind(pragmatic_team, identity_team, stimulation_team, attractive_team)

pragmatic_meet = rowSums(attrackdiffMeet[,1:4])/4
attractive_meet = rowSums(attrackdiffMeet[,9:10])/2
identity_meet = rowSums(attrackdiffMeet[,6:7])/2
stimulation_meet = (attrackdiffTeam[,5]+attrackdiffMeet[,8])/2

meet = cbind(pragmatic_meet, identity_meet, stimulation_meet, attractive_meet)

acceptability_zoom = (Z2.1. + Z4.1. + Z6.1. + Z8.1. + Z10.1.)/5
acceptability_team = (M2.1. + M4.1. + M6.1. + M8.1. + M10.1.)/5
acceptability_meet = (G2.1. + G4.1. + G6.1. + G8.1. + G10.1.)/5

zoom = cbind(pragmatic_zoom, identity_zoom, stimulation_zoom, attractive_zoom, acceptability_zoom)
team = cbind(pragmatic_team, identity_team, stimulation_team, attractive_team, acceptability_team)
meet = cbind(pragmatic_meet, identity_meet, stimulation_meet, attractive_meet, acceptability_meet)


#get indexes for specific age group categories

cat_25_34 <- which(demoQuestion2=="15-24")
cat_25_34 <- which(demoQuestion2=="25-34")
cat_35_44<-which(demoQuestion2=="35-44")
cat_45_54<-which(demoQuestion2=="45-54")
cat_55_64<-which(demoQuestion2=="55-64")
cat_65_84<-which(demoQuestion2=="65-84")

column_names <- c("pragmatic","identity","stimulation","attractive","acceptability")

#Get date based on agegroup 

#zoom

zoomdata_15_24 <- zoom[cat_15_24,]
zoomdata_25_34 <- zoom[cat_25_34,]
zoomdata_35_44 <- zoom[cat_35_44,]
zoomdata_45_54 <- zoom[cat_45_54,]
zoomdata_55_64 <- zoom[cat_55_64,]
zoomdata_65_84 <- zoom[cat_65_84,]

#team

teamdata_15_24 <- team[cat_15_24,]
teamdata_25_34 <- team[cat_25_34,]
teamdata_35_44 <- team[cat_35_44,]
teamdata_45_54 <- team[cat_45_54,]
teamdata_55_64 <- team[cat_55_64,]
teamdata_65_84 <- team[cat_65_84,]

#meet

 meetdata_15_24 <- meet[cat_15_24,]
 meetdata_25_34 <- meet[cat_25_34,]
 meetdata_35_44 <- meet[cat_35_44,]
 meetdata_45_54 <- meet[cat_45_54,]
 meetdata_55_64 <- meet[cat_55_64,]
 meetdata_65_84 <- meet[cat_65_84,]

#data frame for different categories

#zoom
zoomdata_15_24_frame <- data.frame( name=column_names ,value=c(mean(zoomdata_15_24[,1]),mean(zoomdata_15_24[,2]),mean(zoomdata_15_24[,3]),mean(zoomdata_15_24[,4]),mean(zoomdata_15_24[,5])))
zoomdata_25_34_frame <- data.frame(name=column_names ,value= c(mean(zoomdata_25_34[,1]),mean(zoomdata_25_34[,2]),mean(zoomdata_25_34[,3]),mean(zoomdata_25_34[,4]),mean(zoomdata_25_34[,5])))
zoomdata_35_44_frame <- data.frame(name=column_names ,value= c(mean(zoomdata_35_44[,1]),mean(zoomdata_35_44[,2]),mean(zoomdata_35_44[,3]),mean(zoomdata_35_44[,4]),mean(zoomdata_35_44[,5])))
zoomdata_45_54_frame <- data.frame(name=column_names ,value= c(mean(zoomdata_45_54[,1]),mean(zoomdata_45_54[,2]),mean(zoomdata_45_54[,3]),mean(zoomdata_45_54[,4]),mean(zoomdata_45_54[,5])))
zoomdata_55_64_frame <- data.frame(name=column_names ,value= c(mean(zoomdata_55_64[,1]),mean(zoomdata_55_64[,2]),mean(zoomdata_55_64[,3]),mean(zoomdata_55_64[,4]),mean(zoomdata_55_64[,5])))
zoomdata_65_84_frame <- data.frame(name=column_names ,value= c(mean(zoomdata_65_84[,1]),mean(zoomdata_65_84[,2]),mean(zoomdata_65_84[,3]),mean(zoomdata_65_84[,4]),mean(zoomdata_65_84[,5])))


zoomdata_15_24_frame$agegroup <- "15-24"
zoomdata_25_34_frame$agegroup <- "25-34"
zoomdata_35_44_frame$agegroup <- "35-44"
zoomdata_45_54_frame$agegroup <- "45-54"
zoomdata_55_64_frame$agegroup <- "55-64"
zoomdata_65_84_frame$agegroup <- "65-84"

#team

teamdata_15_24_frame <- data.frame( name=column_names ,value=c(mean(teamdata_15_24[,1]),mean(teamdata_15_24[,2]),mean(teamdata_15_24[,3]),mean(teamdata_15_24[,4]),mean(teamdata_15_24[,5])))
teamdata_25_34_frame <- data.frame(name=column_names ,value= c(mean(teamdata_25_34[,1]),mean(teamdata_25_34[,2]),mean(teamdata_25_34[,3]),mean(teamdata_25_34[,4]),mean(teamdata_25_34[,5])))
teamdata_35_44_frame <- data.frame(name=column_names ,value= c(mean(teamdata_35_44[,1]),mean(teamdata_35_44[,2]),mean(teamdata_35_44[,3]),mean(teamdata_35_44[,4]),mean(teamdata_35_44[,5])))
teamdata_45_54_frame <- data.frame(name=column_names ,value= c(mean(teamdata_45_54[,1]),mean(teamdata_45_54[,2]),mean(teamdata_45_54[,3]),mean(teamdata_45_54[,4]),mean(teamdata_45_54[,5])))
teamdata_55_64_frame <- data.frame(name=column_names ,value= c(mean(teamdata_55_64[,1]),mean(teamdata_55_64[,2]),mean(teamdata_55_64[,3]),mean(teamdata_55_64[,4]),mean(teamdata_55_64[,5])))
teamdata_65_84_frame <- data.frame(name=column_names ,value= c(mean(teamdata_65_84[,1]),mean(teamdata_65_84[,2]), mean(teamdata_65_84[,3]),mean(teamdata_65_84[,4]),mean(teamdata_65_84[,5])))

teamdata_15_24_frame$agegroup <- "15-24"
teamdata_25_34_frame$agegroup <- "25-34"
teamdata_35_44_frame$agegroup <- "35-44"
teamdata_45_54_frame$agegroup <- "45-54"
teamdata_55_64_frame$agegroup <- "55-64"
teamdata_65_84_frame$agegroup <- "65-84"

#meet
meetdata_15_24_frame <- data.frame(name=column_names ,value=c(mean(meetdata_15_24[,1]),mean(meetdata_15_24[,2]),mean(meetdata_15_24[,3]),mean(meetdata_15_24[,4]),mean(meetdata_15_24[,5])))
meetdata_25_34_frame <- data.frame(name=column_names ,value= c(mean(meetdata_25_34[,1]),mean(meetdata_25_34[,2]),mean(meetdata_25_34[,3]),mean(meetdata_25_34[,4]),mean(meetdata_25_34[,5])))
meetdata_35_44_frame <- data.frame(name=column_names ,value= c(mean(meetdata_35_44[,1]),mean(meetdata_35_44[,2]),mean(meetdata_35_44[,3]),mean(meetdata_35_44[,4]),mean(meetdata_35_44[,5])))
meetdata_45_54_frame <- data.frame(name=column_names ,value= c(mean(meetdata_45_54[,1]),mean(meetdata_45_54[,2]),mean(meetdata_45_54[,3]),mean(meetdata_45_54[,4]),mean(meetdata_45_54[,5])))
meetdata_55_64_frame <- data.frame(name=column_names ,value= c(mean(meetdata_55_64[,1]),mean(meetdata_55_64[,2]),mean(meetdata_55_64[,3]),mean(meetdata_55_64[,4]),mean(meetdata_55_64[,5])))
meetdata_65_84_frame <- data.frame(name=column_names ,value= c(mean(meetdata_65_84[,1]),mean(meetdata_65_84[,2]),mean(meetdata_65_84[,3]),mean(meetdata_65_84[,4]),mean(meetdata_65_84[,5])))

meetdata_15_24_frame$agegroup <- "15-24"
meetdata_25_34_frame$agegroup <- "25-34"
meetdata_35_44_frame$agegroup <- "35-44"
meetdata_45_54_frame$agegroup <- "45-54"
meetdata_55_64_frame$agegroup <- "55-64"
meetdata_65_84_frame$agegroup <- "65-84"

#graph data frame
zoom.data.graph <- rbind(zoomdata_15_24_frame,zoomdata_25_34_frame,zoomdata_35_44_frame,zoomdata_45_54_frame,zoomdata_55_64_frame,zoomdata_65_84_frame)
team.data.graph <- rbind(teamdata_15_24_frame,teamdata_25_34_frame,teamdata_35_44_frame,teamdata_45_54_frame,teamdata_55_64_frame,teamdata_65_84_frame)
meet.data.graph <- rbind( meetdata_15_24_frame,meetdata_25_34_frame,meetdata_35_44_frame,meetdata_45_54_frame,meetdata_55_64_frame,meetdata_65_84_frame)


#uncomment the foolowing codes  to see the graphical representation of the results 


#zoom score distribution plot by age-group

#ggplot(zoom.data.graph,aes(name,value,fill=agegroup))+geom_bar(stat="identity",position="dodge")+labs(x="Metrics",y="Subjective rating")+geom_text(aes(label =value,vjust= - 0.5),position = position_dodge(0.9))+scale_fill_brewer(palette="Blues")+coord_cartesian(ylim=c(1,7))+   scale_y_continuous(breaks = c(1:7))

#team score distribution plot

#ggplot(team.data.graph,aes(name,value,fill=agegroup))+geom_bar(stat="identity",position="dodge")+labs(x="Metrics",y="Subjective rating")+geom_text(aes(label =value,vjust= - 0.5),position = position_dodge(0.9))+scale_fill_brewer(palette="Blues")+coord_cartesian(ylim=c(1,7))+   scale_y_continuous(breaks = c(1:7))

#meet score distribution plot
#ggplot(meet.data.graph,aes(name,value,fill=agegroup))+geom_bar(stat="identity",position="dodge")+labs(x="Metrics",y="Subjective rating")+geom_text(aes(label =value,vjust= - 0.5),position = position_dodge(0.9))+scale_fill_brewer(palette="Blues")+coord_cartesian(ylim=c(1,7))+   scale_y_continuous(breaks = c(1:7))









