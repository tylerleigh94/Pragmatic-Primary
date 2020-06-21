####Libraries####
library(easypackages)
libs<-c("tidyverse", "haven", "naniar", "ggplot2", "car", "psych", "labelled", "PKPDmisc", "lubridate", 
        "stargazer", "plm")
libraries(libs)
####Import the Data####
dat.4 <- read_sav("Data/s7991_UPenn_LongPollW4_FINAL_WEIGHTED_client_7.11.2019.sav")
dat.5 <- read_sav("Data/s7991_UPenn_LongPollW5_Final_unweighted_client.sav")

# Add Wave indicator
dat.4$wave45_W4 <- 1
dat.5$wave45 <- 1

#Append _W5 to dat.5 column names for consistency
col.names<-colnames(dat.5)
col.names.replace <- c(col.names[1], paste(col.names[-1], "_W5", sep=""))
colnames(dat.5) <- col.names.replace

####Cleaning and Merging####
dat.4$CaseId<-as.numeric(dat.4$CaseId)
dat.5$CaseId<-as.numeric(dat.5$CaseId)

dat<- dat.4 %>%
  full_join(dat.5, by="CaseId")

# Set up values for NA
na.values.2<-c(77, 98, 99)
na.values.3<- c(777, 998, 999)

# Indices for therms
therms<-grep("THERM", colnames(dat), fixed=T)

# Vector of columns with 2-digit NAs
clean.length2<-colnames(dat[c(-1,-therms)])

# Vector of columns with 3-digit NAs
clean.length3<- colnames(dat[therms])

# Remove NAs
dat <- dat %>%
  replace_with_na_at(.vars=clean.length2, condition = ~.x %in% na.values.2)
dat <- dat %>%
  replace_with_na_at(.vars=clean.length3, condition = ~.x %in% na.values.3)

# Drop Non-Democrats

##Three-value PID variable (1=D, 2=I, 3=R)
dat$party3_4<-factor(ifelse(dat$P_PARTYID7_W4<=3, 1, 
                            ifelse(dat$P_PARTYID7_W4>=5, 3, 2)), levels=c(1,2,3), labels=c("1", "2", "3"))

dat$party3_5<-factor(ifelse(dat$P_PARTYID7_W5<=3, 1, 
                            ifelse(dat$P_PARTYID7_W5>=5, 3, 2)), levels=c(1,2,3), labels=c("1", "2", "3"))


## PID dummies
dat$dem_4 <- ifelse(dat$P_PARTYID7_W4<=3, 1, 0)
dat$rep_4 <- ifelse(dat$P_PARTYID7_W4>=5, 1, 0)
dat$ind_4 <- ifelse(dat$P_PARTYID7_W4==4, 1, 0)

dat$dem_5 <- ifelse(dat$P_PARTYID7_W5<=3, 1, 0)
dat$rep_5 <- ifelse(dat$P_PARTYID7_W5>=5, 1, 0)
dat$ind_5 <- ifelse(dat$P_PARTYID7_W5==4, 1, 0)

##Drop 'em
dat<-dat[(dat$dem_4==1 | dat$dem_5==1),]

####Recodes####

# Party Extremity (0=moderate, 3=Extremely partisan)

dat <- dat %>%
  mutate(party.x_4=abs(P_PARTYID7_W4-4), party.x_5=abs(P_PARTYID7_W5-4))

# Perceived Polarization (0= Perceive no polarization; 1=A Lot of polarization)

dat$ppol1_4<-dat$PP1_W4
dat$ppol2_4<-dat$PP2_W4
dat$ppol3_4<-recode(dat$PP3_W4, "1=4; 2=3; 3=2; 4=1")
dat$ppol4_4<-recode(dat$PP4_W4, "1=4; 2=3; 3=2; 4=1")

dat$ppol1_5<-dat$PP1_W5
dat$ppol2_5<-dat$PP2_W5
dat$ppol3_5<-recode(dat$PP3_W5, "1=4; 2=3; 3=2; 4=1")
dat$ppol4_5<-recode(dat$PP4_W5, "1=4; 2=3; 3=2; 4=1")

##Single index and reliability

ppol.index_4<-which(colnames(dat) %in% c("ppol1_4", "ppol2_4", "ppol3_4", "ppol4_4"))
dat$ppol.ind_4<-(rowMeans(dat[,ppol.index_4])-1)/3
alpha.ppol_4<-psych::alpha(dat[,ppol.index_4])

ppol.index_5<-which(colnames(dat) %in% c("ppol1_5", "ppol2_5", "ppol3_5", "ppol4_5"))
dat$ppol.ind_5<-(rowMeans(dat[,ppol.index_5])-1)/3
alpha.ppol_5<-psych::alpha(dat[,ppol.index_5])

# Threat to American Way of Life (0-3; 3=high threat)
dat$way.life.threat_4<-recode(dat$PT4_W4, "4=0; 3=1; 2=2; 1=3")
dat$way.life.threat_5<-recode(dat$PT4_W5, "4=0; 3=1; 2=2; 1=3")

# Affective polarization (0-1; 1=high aff pol)
dat <- dat %>%
  mutate(aff.pol_5=abs(THERM3_W5-THERM4_W5)/100)

#Gender and Race Recodes

##Gender: 1=male

dat$male.dum_4<-ifelse(dat$GENDER_W4==1,1,0)
dat$male.dum_5<-ifelse(dat$GENDER_W5==1,1,0)

##Race: 1=white

dat$white.dum_4<-ifelse(dat$RACETHNICITY_W4==1,1,0)
dat$white.dum_5<-ifelse(dat$RACETHNICITY_W5==1,1,0)


### Minority group dummies (identifying as specified race = 1)

dat$black.dum_4<-ifelse(dat$RACETHNICITY_W4==2, 1, 0)
dat$hispanic.dum_4<-ifelse(dat$RACETHNICITY_W4==4, 1, 0)
dat$asian.dum_4<-ifelse(dat$RACETHNICITY_W4==6, 1, 0)

dat$black.dum_5<-ifelse(dat$RACETHNICITY_W5==2, 1, 0)
dat$hispanic.dum_5<-ifelse(dat$RACETHNICITY_W5==4, 1, 0)
dat$asian.dum_5<-ifelse(dat$RACETHNICITY_W5==6, 1, 0)

# Immigration Index (0=Anti-immigration; 4=pro-immigration)

dat$imm1_4<-recode(dat$IM1_W4, "5=1;4=2;3=3;2=4;1=5")
dat$imm2_4<-dat$IM2_W4
dat$imm3_4<-dat$IM3_W4

dat$imm1_5<-recode(dat$IM1_W5, "5=1;4=2;3=3;2=4;1=5")
dat$imm2_5<-dat$IM2_W5
dat$imm3_5<-dat$IM3_W5

##Collapsed into Scale and Standardized. Alpha reported

imm.index_4<-which(colnames(dat) %in% c("imm1_4", "imm2_4", "imm3_4"))
dat$imm.ind_4 <- rowMeans(dat[,imm.index_4])-1
alpha.imm_4<-psych::alpha(dat[imm.index_4])

imm.index_5<-which(colnames(dat) %in% c("imm1_5", "imm2_5", "imm3_5"))
dat$imm.ind_5 <- rowMeans(dat[,imm.index_5])-1
alpha.imm_5<-psych::alpha(dat[imm.index_5])

#Issue Polarization Scale; (0,1), 1=highly polarized, 0= middle of road/no polarization

dat$iss1_4<-recode(dat$GUNS_W4, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss2_4<-recode(dat$TSN1_W4, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss3_4<-recode(dat$TSN2_W4, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss4_4<-recode(dat$RUS1_W4, "3=0;1=1;5=1;2=.5;4=.5")

dat$iss1_5<-recode(dat$GUNS_W5, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss2_5<-recode(dat$TSN1_W5, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss3_5<-recode(dat$TSN2_W5, "3=0;1=1;5=1;2=.5;4=.5")
dat$iss4_5<-recode(dat$RUS1_W5, "3=0;1=1;5=1;2=.5;4=.5")

##Transformation of Immigration index

dat <- dat %>%
  mutate(iss5_4=abs(imm.ind_4-2)/2, iss5_5=abs(imm.ind_5-2)/2)

##Index construction and alpha

iss.pol.index_4<-which(colnames(dat) %in% c("iss1_4", "iss2_4", "iss3_4", "iss4_4", "iss5_4"))
alpha.iss.pol_4<-psych::alpha(dat[iss.pol.index_4])
dat$iss.pol_4 <- rowMeans(dat[,iss.pol.index_4])

iss.pol.index_5<-which(colnames(dat) %in% c("iss1_5", "iss2_5", "iss3_5", "iss4_5", "iss5_5"))
alpha.iss.pol_5<-psych::alpha(dat[iss.pol.index_5])
dat$iss.pol_5 <- rowMeans(dat[,iss.pol.index_5])

# Perceived Elite Polarization (0,1); 1=high perceived elite polarization
dat <- dat %>%
  mutate(imm.diff_5=abs(IM1A_W5-IM1B_W5)/4, tr.diff_5=abs(TR1A_W5-TR1B_W5)/4, 
         chi.diff_5=abs(CHI3_W5-CHI4_W5)/4, rs.diff_5=abs(RS1A_W5-RS1B_W5)/3,
         hc.diff_5=abs(HC5A_W5-HC5B_W5)/3, guns.diff_5=abs(GUNSREP_W5-GUNSDEM_W5)/4)

##Index and reliability
index.diff<-grep("\\.diff_5", colnames(dat))
alpha.diff<-psych::alpha(dat[,index.diff])
dat$elite.ppolar_5 <- rowMeans(dat[,index.diff])

# Polarization index (Wave 5 only)
##Wave 5
index.pol_5<-which(colnames(dat) %in% c("aff.pol_5", "iss.pol_5", "elite.ppolar_5", "ppol.ind_5"))
dat$polar.ind_5<-rowMeans(dat[,index.pol_5], na.rm=T)
alpha.polar.ind_5 <- alpha(dat[,index.pol_5])

#Extreme Ideology; (0,3) 3=extreme, 0=moderate

dat$ideo.x_4<-recode(dat$P_IDEO_W4, "1=3;7=3;2=2;6=2;3=1;5=1;4=0;8=0;else=NA")
dat$ideo.x_5<-recode(dat$P_IDEO_W5, "1=3;7=3;2=2;6=2;3=1;5=1;4=0;8=0;else=NA")

#Civility Index; (0,1), high=civil

dat$civil.1_4<-dat$CIVIL1_W4
dat$civil.2_4<-recode(dat$CIVIL2_W4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.3_4<-dat$CIVIL3_W4
dat$civil.4_4<-recode(dat$CIVIL4_W4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.5_4<-dat$CIVIL5_W4

dat$civil.1_5<-dat$CIVIL1_W5
dat$civil.2_5<-recode(dat$CIVIL2_W5, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.3_5<-dat$CIVIL3_W5
dat$civil.4_5<-recode(dat$CIVIL4_W5, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat$civil.5_5<-dat$CIVIL5_W5

##Create scale, standardized (0,1), and alpha

civil.index_4<-which(colnames(dat) %in% c("civil.1_4", "civil.2_4", "civil.3_4", "civil.4_4", "civil.5_4"))
dat$civil.ind_4 <- (rowMeans(dat[,civil.index_4])-1)/9
alpha.civil_4<-psych::alpha(dat[civil.index_4])

civil.index_5<-which(colnames(dat) %in% c("civil.1_5", "civil.2_5", "civil.3_5", "civil.4_5", "civil.5_5"))
dat$civil.ind_5 <- (rowMeans(dat[,civil.index_5])-1)/9
alpha.civil_5<-psych::alpha(dat[civil.index_5])

# Political Interest recoded and standardized (0-1); high=interested

dat$interest_4<-recode(dat$Q16_W4, "1=3; 2=2; 3=1; 4=0")/3
dat$interest_5<-recode(dat$Q16_W5, "1=3; 2=2; 3=1; 4=0")/3

# Ideology: Recode 8 = "Haven't thought much about it" to NA; (1,7) high=extremely conservative
dat$ideo_4 <- ifelse(dat$P_IDEO_W4==8, NA, dat$P_IDEO_W4)
dat$ideo_5 <- ifelse(dat$P_IDEO_W5==8, NA, dat$P_IDEO_W4)

# Indicator for voting for most viable candidate (1: First choice vote = First choice viable)
dat$vote.viable_4 <- ifelse(dat$DEM1_W4==dat$DEM3_W4, 1, 0)
dat$vote.viable_5 <- ifelse(dat$DEM1_W5==dat$DEM3_W5, 1, 0)

# Indicator for voting for second most viable (1: First choice vote = Second choice viable)
dat$vote.viable2_4 <- ifelse(dat$DEM1_W4==dat$DEM4_W4, 1, 0)
dat$vote.viable2_5 <- ifelse(dat$DEM1_W5==dat$DEM4_W5, 1, 0)

# Combine two viability indicators into a single factor (1: Voted for most viable; 
# .5: Voted for second-most viable; 0: Voted for neither)
dat$vote.via.c_4 <- ifelse(dat$vote.viable_4==1, 1,
                         ifelse(dat$vote.viable2_4==1, .5, 0))
dat$vote.via.c_5 <- ifelse(dat$vote.viable_5==1, 1,
                           ifelse(dat$vote.viable2_5==1, .5, 0))

# Indicator for voting for most electable
dat$vote.electable_4 <- ifelse(dat$DEM1_W4==dat$DEM5_W4, 1, 0)
dat$vote.electable_5 <- ifelse(dat$DEM1_W5==dat$DEM5_W5, 1, 0)

# Indicator for voting for second-most electable (only possible for wave 5)
dat$vote.electable2_5<-ifelse(dat$DEM1_W5==dat$DEM6_W5, 1, 0)


# Combine two electability indicators into a single factor (1: Voted for most electable; 
# .5: Voted for second-most electable; 0: Voted for neither)
dat$vote.elect.c_4 <- ifelse(dat$vote.electable_4==1, 1, 0)

dat$vote.elect.c_5 <- ifelse(dat$vote.electable_5==1, 1,
                             ifelse(dat$vote.electable2_5==1, .5, 0))

# Recode therms so NAs are treated as 0
dat$therm.biden_4 <- dat$THERM5_W4 %>% replace_na(0)
dat$therm.sanders_4 <- dat$THERMNEW3_W4 %>% replace_na(0)
dat$therm.harris_4 <- dat$THERMNEW2_W4 %>% replace_na(0)
dat$therm.warren_4 <- dat$THERMNEW1_W4 %>% replace_na(0)
dat$therm.trump_4 <- dat$THERMTRUMP_W4 %>% replace_na(0)

dat$therm.biden_5 <- dat$THERM5_W5 %>% replace_na(0)
dat$therm.sanders_5 <- dat$THERMNEW3_W5 %>% replace_na(0)
dat$therm.warren_5 <- dat$THERMNEW1_W5 %>% replace_na(0)
dat$therm.buttigieg_5 <- dat$THERMNEW4_W5 %>% replace_na(0)
dat$therm.trump_5 <- dat$THERMTRUMP_W5 %>% replace_na(0)
dat$therm.bloomberg_5 <- dat$THERMNEW5_W5 %>% replace_na(0)

# Add therm recodes where NAs are NAs
dat$thermr.biden_4 <- dat$THERM5_W4 
dat$thermr.sanders_4 <- dat$THERMNEW3_W4 
dat$thermr.harris_4 <- dat$THERMNEW2_W4 
dat$thermr.warren_4 <- dat$THERMNEW1_W4 
dat$thermr.trump_4 <- dat$THERMTRUMP_W4 

dat$thermr.biden_5 <- dat$THERM5_W5
dat$thermr.sanders_5 <- dat$THERMNEW3_W5
dat$thermr.warren_5 <- dat$THERMNEW1_W5
dat$thermr.buttigieg_5 <- dat$THERMNEW4_W5
dat$thermr.trump_5 <- dat$THERMTRUMP_W5
dat$thermr.bloomberg_5 <- dat$THERMNEW5_W5

# Bin therms in 10s for analysis
dat$therm10.biden_4 <- as.numeric(cut(dat$THERM5_W4, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.sanders_4 <- as.numeric(cut(dat$THERMNEW3_W4, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.harris_4 <- as.numeric(cut(dat$THERMNEW2_W4, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.warren_4 <- as.numeric(cut(dat$THERMNEW1_W4, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.trump_4 <- as.numeric(cut(dat$THERMTRUMP_W4, seq(0,100, 10), include.lowest=T, labels=c(1:10)))

dat$therm10.biden_5 <- as.numeric(cut(dat$THERM5_W5, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.sanders_5 <- as.numeric(cut(dat$THERMNEW3_W5, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.warren_5 <- as.numeric(cut(dat$THERMNEW1_W5, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.buttigieg_5 <- as.numeric(cut(dat$THERMNEW4_W5, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.trump_5 <- as.numeric(cut(dat$THERMTRUMP_W5, seq(0,100, 10), include.lowest=T, labels=c(1:10)))
dat$therm10.bloomberg_5 <- as.numeric(cut(dat$THERMNEW5_W5, seq(0,100, 10), include.lowest=T, labels=c(1:10)))

# Column representing highest therm value
index.dem.therms_4 <- which(colnames(dat) %in% c("therm.biden_4", "therm.sanders_4", "therm.harris_4", 
                                             "therm.warren_4"))
dat$therm.max.val_4 <- apply(dat[,index.dem.therms_4], 1, max)

index.dem.therms_5 <- which(colnames(dat) %in% c("therm.biden_5", "therm.sanders_5", "therm.warren_5", "therm.buttigieg_5", "therm.bloomberg_5"))
dat$therm.max.val_5 <- apply(dat[,index.dem.therms_5], 1, max)

# Dummmy indicating whether each of Top 4 (or 5) candidates was most-liked or tied for most-liked
dat$therm.highest.biden_4 <- ifelse(dat$therm.biden_4 == dat$therm.max.val_4, 1, 0)
dat$therm.highest.sanders_4 <- ifelse(dat$therm.sanders_4 == dat$therm.max.val_4, 1, 0)
dat$therm.highest.harris_4 <- ifelse(dat$therm.harris_4 == dat$therm.max.val_4, 1, 0)
dat$therm.highest.warren_4 <- ifelse(dat$therm.warren_4 == dat$therm.max.val_4, 1, 0)

dat$therm.highest.biden_5 <- ifelse(dat$therm.biden_5 == dat$therm.max.val_5, 1, 0)
dat$therm.highest.sanders_5 <- ifelse(dat$therm.sanders_5 == dat$therm.max.val_5, 1, 0)
dat$therm.highest.buttigieg_5 <- ifelse(dat$therm.buttigieg_5 == dat$therm.max.val_5, 1, 0)
dat$therm.highest.warren_5 <- ifelse(dat$therm.warren_5 == dat$therm.max.val_5, 1, 0)
dat$therm.highest.bloomberg_5 <- ifelse(dat$therm.bloomberg_5 == dat$therm.max.val_5, 1, 0)

# Indicator for voting for most-liked candidate, allowing for duplicate values at max (WAVE 4)
dat$dem1r_4<-ifelse(is.na(dat$DEM1_W4), 0, dat$DEM1_W4) # Replace NAs with 0
vote.therm_4 <- vector()

for (row in 1:nrow(dat)) {
  vote <- dat[row, "dem1r_4"]
  therm.highest.biden <- dat[row, "therm.highest.biden_4"]
  therm.highest.sanders <- dat[row, "therm.highest.sanders_4"]
  therm.highest.harris <- dat[row, "therm.highest.harris_4"]
  therm.highest.warren <- dat[row, "therm.highest.warren_4"]
  
  if ((vote == 1) & (therm.highest.biden == 1)) {
    vote.therm_4 <- c(vote.therm_4, c(1))
  } else if ((vote == 2) & (therm.highest.sanders == 1)) {
    vote.therm_4 <- c(vote.therm_4, c(1))
  } else if ((vote == 3) & (therm.highest.harris == 1)) {
    vote.therm_4 <- c(vote.therm_4, c(1))
  } else if ((vote == 4) && (therm.highest.warren == 1)) {
    vote.therm_4 <- c(vote.therm_4, c(1))
  } else {
    vote.therm_4 <- c(vote.therm_4, c(0))
  }
  
}

dat$vote.therm_4 <- vote.therm_4

# Indicator for voting for most-liked candidate, allowing for duplicate values at max (WAVE 5)
dat$dem1r_5<- ifelse(is.na(dat$DEM1_W5), 0, dat$DEM1_W5) # Replace NAs with 0
vote.therm_5 <- vector()

for (row in 1:nrow(dat)) {
  vote <- dat[row, "dem1r_5"]
  therm.highest.biden <- dat[row, "therm.highest.biden_5"]
  therm.highest.sanders <- dat[row, "therm.highest.sanders_5"]
  therm.highest.buttigieg <- dat[row, "therm.highest.buttigieg_5"]
  therm.highest.warren <- dat[row, "therm.highest.warren_5"]
  therm.highest.bloomberg <- dat[row, "therm.highest.bloomberg_5"]
  
  if ((vote == 1) & (therm.highest.biden == 1)) {
    vote.therm_5 <- c(vote.therm_5, c(1))
  } else if ((vote == 2) & (therm.highest.sanders == 1)) {
    vote.therm_5 <- c(vote.therm_5, c(1))
  } else if ((vote == 4) & (therm.highest.buttigieg == 1)) {
    vote.therm_5 <- c(vote.therm_5, c(1))
  } else if ((vote == 3) && (therm.highest.warren == 1)) {
    vote.therm_5 <- c(vote.therm_5, c(1))
  } else if ((vote == 5) && (therm.highest.bloomberg == 1)) {
    vote.therm_5 <- c(vote.therm_5, c(1))
  } else {
    vote.therm_5 <- c(vote.therm_5, c(0))
  }
  
}

dat$vote.therm_5 <- vote.therm_5

# Ideological distance from candidate (Negative: Candidate more liberal than respondent; 
#Postive: Candidate more conservative than respondent)
dat <- dat %>% 
  mutate(biden.dist_4=CANSCALE_2_W4-ideo_4, sanders.dist_4=CANSCALE_3_W4-ideo_4,
         harris.dist_4=CANSCALE_4_W4-ideo_4, warren.dist_4=CANSCALE_5_W4-ideo_4)

dat <- dat %>% 
  mutate(biden.dist_5=CANSCALE_2_W5-ideo_5, sanders.dist_5=CANSCALE_3_W5-ideo_5,
         warren.dist_5=CANSCALE_5_W5-ideo_5, buttigieg.dist_5=CANSCALE_6_W5-ideo_5, 
         bloomberg.dist_5=CANSCALE_7_W5-ideo_5)

# Top-vote variables for logit models
dat$vote.top.biden_4<-ifelse(dat$DEM1_W4==1, 1, 0)
dat$vote.top.sanders_4<-ifelse(dat$DEM1_W4==2, 1, 0)
dat$vote.top.harris_4<-ifelse(dat$DEM1_W4==3, 1, 0)
dat$vote.top.warren_4<-ifelse(dat$DEM1_W4==4, 1, 0)

dat$vote.top.biden_5<-ifelse(dat$DEM1_W5==1, 1, 0)
dat$vote.top.sanders_5<-ifelse(dat$DEM1_W5==2, 1, 0)
dat$vote.top.buttigieg_5<-ifelse(dat$DEM1_W5==4, 1, 0)
dat$vote.top.warren_5<-ifelse(dat$DEM1_W5==3, 1, 0)
dat$vote.top.bloomberg_5<-ifelse(dat$DEM1_W5==5, 1, 0)

# Three-level vote choice variable for Top 4 candidates 
# (1: First choice; .5: Second choice; 0: Neither)
dat$vote.biden_4<-ifelse(dat$DEM1_W4==1, 1, 
                       ifelse(dat$DEM2_W4==1, .5, 0))
dat$vote.sanders_4<-ifelse(dat$DEM1_W4==2, 1, 
                         ifelse(dat$DEM2_W4==2, .5, 0))
dat$vote.warren_4<-ifelse(dat$DEM1_W4==4, 1, 
                        ifelse(dat$DEM2_W4==4, .5, 0))
dat$vote.harris_4<-ifelse(dat$DEM1_W4==3, 1, 
                        ifelse(dat$DEM2_W4==3, .5, 0))

dat$vote.biden_5<-ifelse(dat$DEM1_W5==1, 1, 
                       ifelse(dat$DEM2_W5==1, .5, 0))
dat$vote.sanders_5<-ifelse(dat$DEM1_W5==2, 1, 
                         ifelse(dat$DEM2_W5==2, .5, 0))
dat$vote.warren_5<-ifelse(dat$DEM1_W5==3, 1, 
                        ifelse(dat$DEM2_W5==3, .5, 0))
dat$vote.buttigieg_5<-ifelse(dat$DEM1_W5==4, 1, 
                           ifelse(dat$DEM2_W5==4, .5, 0))
dat$vote.bloomberg_5<-ifelse(dat$DEM1_W5==5, 1, 
                           ifelse(dat$DEM2_W5==5, .5, 0))

# Three-level viability variable for Top 4/5 candidates (1: First choice; .5: Second choice; 0: Neither)
dat$via.biden_4<-ifelse(dat$DEM3_W4==1, 1, 
                      ifelse(dat$DEM4_W4==1, .5, 0))
dat$via.sanders_4<-ifelse(dat$DEM3_W4==2, 1, 
                        ifelse(dat$DEM4_W4==2, .5, 0))
dat$via.warren_4<-ifelse(dat$DEM3_W4==4, 1, 
                       ifelse(dat$DEM4_W4==4, .5, 0))
dat$via.harris_4<-ifelse(dat$DEM3_W4==3, 1, 
                       ifelse(dat$DEM4_W4==3, .5, 0))

dat$via.biden_5<-ifelse(dat$DEM3_W5==1, 1, 
                      ifelse(dat$DEM4_W5==1, .5, 0))
dat$via.sanders_5<-ifelse(dat$DEM3_W5==2, 1, 
                        ifelse(dat$DEM4_W5==2, .5, 0))
dat$via.warren_5<-ifelse(dat$DEM3_W5==3, 1, 
                       ifelse(dat$DEM4_W5==3, .5, 0))
dat$via.buttigieg_5<-ifelse(dat$DEM3_W5==4, 1, 
                          ifelse(dat$DEM4_W5==4, .5, 0))
dat$via.bloomberg_5<-ifelse(dat$DEM3_W5==5, 1, 
                          ifelse(dat$DEM4_W5==5, .5, 0))

# Binary electability variable for Top 4/5 candidates (1: Most electable, 0: Not) (WAVE 4)
dat$elect.biden_4<-ifelse(dat$DEM5_W4==1, 1, 0)
dat$elect.sanders_4<-ifelse(dat$DEM5_W4==2, 1, 0)
dat$elect.harris_4<-ifelse(dat$DEM5_W4==3, 1, 0)
dat$elect.warren_4<-ifelse(dat$DEM5_W4==4, 1, 0)

dat$elect2.biden_5<-ifelse(dat$DEM5_W5==1, 1, 0)
dat$elect2.sanders_5<-ifelse(dat$DEM5_W5==2, 1, 0)
dat$elect2.warren_5<-ifelse(dat$DEM5_W5==3, 1, 0)
dat$elect2.buttigieg_5<-ifelse(dat$DEM5_W4==4, 1, 0)
dat$elect2.bloomberg_5<-ifelse(dat$DEM5_W4==5, 1, 0)

# Electability variable for Top 5 candidates (1: Most electable, .5: second most electable, 0: Not)
dat$elect.biden_5<-ifelse(dat$DEM5_W5==1, 1, 
                        ifelse(dat$DEM6_W5==1, .5, 0))
dat$elect.sanders_5<-ifelse(dat$DEM5_W5==2, 1, 
                          ifelse(dat$DEM6_W5==2, .5, 0))
dat$elect.warren_5<-ifelse(dat$DEM5_W5==3, 1, 
                         ifelse(dat$DEM6_W5==3, .5, 0))
dat$elect.buttigieg_5<-ifelse(dat$DEM5_W5==4, 1, 
                            ifelse(dat$DEM6_W5==4, .5, 0))
dat$elect.bloomberg_5<-ifelse(dat$DEM5_W5==5, 1, 
                            ifelse(dat$DEM6_W5==5, .5, 0))

# Candidate ideology renames
dat$ideo.trump_4<-(dat$CANSCALE_1_W4-4)
dat$ideo.biden_4<-(dat$CANSCALE_2_W4-4)
dat$ideo.sanders_4<-(dat$CANSCALE_3_W4-4)
dat$ideo.harris_4<-(dat$CANSCALE_4_W4-4)
dat$ideo.warren_4<-(dat$CANSCALE_5_W4-4)

dat$ideo.trump_5<-(dat$CANSCALE_1_W5-4)
dat$ideo.biden_5<-(dat$CANSCALE_2_W5-4)
dat$ideo.sanders_5<-(dat$CANSCALE_3_W5-4)
dat$ideo.buttigieg_5<-(dat$CANSCALE_6_W5-4)
dat$ideo.warren_5<-(dat$CANSCALE_5_W5-4)
dat$ideo.bloomberg_5<-(dat$CANSCALE_7_W5-4)

# Code respondents as voting for ideologically closest candidate (WAVE 4)

## Get out min distance

index.dist_4<-grep(".dist_4", colnames(dat))
dat$min.dist_4<-NA
for(x in 1:nrow(dat)){
  dat$min.dist_4[x]<-min(abs(dat[x,index.dist_4]), na.rm=T)
}

### Remove cases where no candidate ideology is provided

dat$min.dist_4<-ifelse(dat$min.dist_4=='Inf', NA, dat$min.dist_4)

## Compare minimum distance to vote choice

dat$vote.dist_4<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM1_W4[x]) | is.na(dat$min.dist_4[x])){dat$vote.dist_4[x]=NA}
  else if(dat$DEM1_W4[x]==1 & dat$min.dist_4[x] %in% abs(dat$biden.dist_4[x])){
    dat$vote.dist_4[x]=1}
  else if(dat$DEM1_W4[x]==2 & dat$min.dist_4[x] %in% abs(dat$sanders.dist_4[x])){
    dat$vote.dist_4[x]=1}
  else if(dat$DEM1_W4[x]==3 & dat$min.dist_4[x] %in% abs(dat$harris.dist_4[x])){
    dat$vote.dist_4[x]=1}
  else if(dat$DEM1_W4[x]==4 & dat$min.dist_4[x] %in% abs(dat$warren.dist_4[x])){
    dat$vote.dist_4[x]=1}
  else {dat$vote.dist_4[x]=0}
}

# Code respondents as voting for ideologically closest candidate (WAVE 5)

## Get out min distance

index.dist_5<-grep(".dist_5", colnames(dat))
dat$min.dist_5<-NA
for(x in 1:nrow(dat)){
  dat$min.dist_5[x]<-min(abs(dat[x,index.dist_5]), na.rm=T)
}

### Remove cases where no candidate ideology is provided

dat$min.dist_5<-ifelse(dat$min.dist_5=='Inf', NA, dat$min.dist_5)

## Compare minimum distance to vote choice

dat$vote.dist_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM1_W5[x]) | is.na(dat$min.dist_5[x])){dat$vote.dist_5[x]=NA}
  else if(dat$DEM1_W5[x]==1 & dat$min.dist_5[x] %in% abs(dat$biden.dist_5[x])){
    dat$vote.dist_5[x]=1}
  else if(dat$DEM1_W5[x]==2 & dat$min.dist_5[x] %in% abs(dat$sanders.dist_5[x])){
    dat$vote.dist_5[x]=1}
  else if(dat$DEM1_W5[x]==4 & dat$min.dist_5[x] %in% abs(dat$buttigieg.dist_5[x])){
    dat$vote.dist_5[x]=1}
  else if(dat$DEM1_W5[x]==3 & dat$min.dist_5[x] %in% abs(dat$warren.dist_5[x])){
    dat$vote.dist_5[x]=1}
  else if(dat$DEM1_W5[x]==5 & dat$min.dist_5[x] %in% abs(dat$bloomberg.dist_5[x])){
    dat$vote.dist_5[x]=1}
  else {dat$vote.dist_5[x]=0}
}

# Thermometer rating for top vote choice

dat$top.vote.therm_4<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W4[x] %in% c(1:4))
  {dat$top.vote.therm_4[x]=dat[x, index.dem.therms_4[dat$DEM1_W4[x]]]}
  else{dat$top.vote.therm_4[x]=NA}
}

dat$top.vote.therm_4<-as.numeric(unlist(dat$top.vote.therm_4))

dat$top.vote.therm_5<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W5[x] %in% c(1:5))
  {dat$top.vote.therm_5[x]=dat[x, index.dem.therms_5[dat$DEM1_W5[x]]]}
  else{dat$top.vote.therm_5[x]=NA}
}

dat$top.vote.therm_5<-as.numeric(unlist(dat$top.vote.therm_5))

# Viability top vote choice

dat$via.top_4<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W4[x] %in% dat$DEM3_W4[x]){dat$via.top_4[x]=1}
  else if(dat$DEM1_W4[x] %in% dat$DEM4_W4[x]){dat$via.top_4[x]=.5}
  else{dat$via.top_4[x]=0}
}

dat$via.top_5<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W5[x] %in% dat$DEM3_W5[x]){dat$via.top_5[x]=1}
  else if(dat$DEM1_W5[x] %in% dat$DEM4_W5[x]){dat$via.top_5[x]=.5}
  else{dat$via.top_5[x]=0}
}

# Electability top vote choice

dat$elec.top_4<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W4[x] %in% dat$DEM5_W4[x]){dat$elec.top_4[x]=1}
  else{dat$elec.top_4[x]=0}
}

dat$elec.top3_5<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W5[x] %in% dat$DEM5_W5[x]){dat$elec.top3_5[x]=1}
  else if(dat$DEM1_W5[x] %in% dat$DEM6_W5[x]){dat$elec.top3_5[x]=.5}
  else{dat$elec.top3_5[x]=0}
}

dat$elec.top_5<-NA
for(x in 1:nrow(dat)){
  if(dat$DEM1_W5[x] %in% dat$DEM5_W5[x]){dat$elec.top_5[x]=1}
  else{dat$elec.top_5[x]=0}
}
# Distance top vote choice

dat$dist.top_4<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM1_W4[x])) {dat$dist.top_4[x]<-NA}
  else if(dat$DEM1_W4[x]==1){dat$dist.top_4=abs(dat$biden.dist_4)}
  else if (dat$DEM1_W4[x]==2){dat$dist.top_4=abs(dat$sanders.dist_4)}
  else if (dat$DEM1_W4[x]==3){dat$dist.top_4=abs(dat$harris.dist_4)}
  else if(dat$DEM1_W4[x]==4){dat$dist.top_4=abs(dat$warren.dist_4)}
  else {dat$dist.top_4<-NA}
}

dat$dist.top_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM1_W5[x])) {dat$dist.top_5[x]<-NA}
  else if(dat$DEM1_W5[x]==1){dat$dist.top_5[x]=abs(dat$biden.dist_5[x])}
  else if (dat$DEM1_W5[x]==2){dat$dist.top_5[x]=abs(dat$sanders.dist_5[x])}
  else if (dat$DEM1_W5[x]==3){dat$dist.top_5[x]=abs(dat$warren.dist_5[x])}
  else if(dat$DEM1_W5[x]==4){dat$dist.top_5[x]=abs(dat$buttigieg.dist_5[x])}
  else if(dat$DEM1_W5[x]==5){dat$dist.top_5[x]=abs(dat$bloomberg.dist_5[x])}
  else {dat$dist.top_5[x]<-NA}
}

# Indicator for thermometer ties

dat$therm.tie_4<-NA
for(x in 1:nrow(dat)){
  n<-0
  if(dat$therm.biden_4[x]==dat$therm.max.val_4[x])
  {n<-n+1}
  if(dat$therm.sanders_4[x]==dat$therm.max.val_4[x])
  {n<-n+1}
  if(dat$therm.harris_4[x]==dat$therm.max.val_4[x])
  {n<-n+1}
  if(dat$therm.warren_4[x]==dat$therm.max.val_4[x])
  {n<-n+1}
  dat$therm.tie_4[x]<-n
}

dat$therm.tie_5<-NA
for(x in 1:nrow(dat)){
  n<-0
  if(dat$therm.biden_5[x]==dat$therm.max.val_5[x])
  {n<-n+1}
  if(dat$therm.sanders_5[x]==dat$therm.max.val_5[x])
  {n<-n+1}
  if(dat$therm.buttigieg_5[x]==dat$therm.max.val_5[x])
  {n<-n+1}
  if(dat$therm.bloomberg_5[x]==dat$therm.max.val_5[x])
  {n<-n+1}
  if(dat$therm.warren_5[x]==dat$therm.max.val_5[x])
  {n<-n+1}
  dat$therm.tie_5[x]<-n
}

# Voting strategically (electability)

## Wave 4
###Get out most liked candidate electability
dat$elect.mostliked_4<-NA
for(x in 1:nrow(dat)){
  if(dat$therm.highest.biden_4[x]==1) {dat$elect.mostliked_4[x]<-dat$elect.biden_4[x]}
  else if (dat$therm.highest.sanders_4[x]==1) {dat$elect.mostliked_4[x]<-dat$elect.sanders_4[x]}
  else if (dat$therm.highest.warren_4[x]==1) {dat$elect.mostliked_4[x]<-dat$elect.warren_4[x]}
  else if (dat$therm.highest.harris_4[x]==1) {dat$elect.mostliked_4[x]<-dat$elect.harris_4[x]}
  else{dat$elect.mostliked_4[x]<-NA}
}

### Electability Strategic voting
dat$strat.elect_4<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$elec.top_4[x]) | is.na(dat$elect.mostliked_4[x]) | is.na(dat$therm.tie_4[x])) {dat$strat.elect_4[x]<-NA}
  else if(dat$elec.top_4[x]>dat$elect.mostliked_4[x]) {dat$strat.elect_4[x]<-1}
  else if(dat$elec.top_4[x]==dat$elect.mostliked_4[x] & dat$therm.tie_4[x]>1) {dat$strat.elect_4[x]<-1}
  else {dat$strat.elect_4[x]<-0}
}

###Remove anyone who can't be definitively classified
dat$strat.elect_4<-ifelse(dat$DEM1_W4<=4, dat$strat.elect_4, NA)

##Wave 5--Using Binary Electability
##Get out most liked candidate electability
dat$elect2.mostliked_5<-NA
for(x in 1:nrow(dat)){
  if(dat$therm.highest.biden_5[x]==1) {dat$elect2.mostliked_5[x]<-dat$elect2.biden_5[x]}
  else if (dat$therm.highest.sanders_5[x]==1) {dat$elect2.mostliked_5[x]<-dat$elect2.sanders_5[x]}
  else if (dat$therm.highest.warren_5[x]==1) {dat$elect2.mostliked_5[x]<-dat$elect2.warren_5[x]}
  else if (dat$therm.highest.buttigieg_5[x]==1) {dat$elect2.mostliked_5[x]<-dat$elect2.buttigieg_5[x]}
  else if (dat$therm.highest.bloomberg_5[x]==1) {dat$elect2.mostliked_5[x]<-dat$elect2.bloomberg_5[x]}
  else{dat$elect2.mostliked_5[x]<-NA}
}

dat$strat.elect_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$elec.top_5[x]) | is.na(dat$elect2.mostliked_5[x]) | is.na(dat$therm.tie_5[x])) {dat$strat.elect_5[x]<-NA}
  else if(dat$elec.top_5[x]>dat$elect2.mostliked_5[x]) {dat$strat.elect_5[x]<-1}
  else if(dat$elec.top_5[x]==dat$elect2.mostliked_5[x] & dat$therm.tie_5[x]>1) {dat$strat.elect_5[x]<-1}
  else {dat$strat.elect_5[x]<-0}
}

###Remove anyone who can't be definitively classified
dat$strat.elect_5<-ifelse(dat$DEM1_W5<=5, dat$strat.elect_5, NA)

##Wave 5--Using 3pt Electability
##Get out most liked candidate electability
dat$elect.mostliked_5<-NA
for(x in 1:nrow(dat)){
  if(dat$therm.highest.biden_5[x]==1) {dat$elect.mostliked_5[x]<-dat$elect.biden_5[x]}
  else if (dat$therm.highest.sanders_5[x]==1) {dat$elect.mostliked_5[x]<-dat$elect.sanders_5[x]}
  else if (dat$therm.highest.warren_5[x]==1) {dat$elect.mostliked_5[x]<-dat$elect.warren_5[x]}
  else if (dat$therm.highest.buttigieg_5[x]==1) {dat$elect.mostliked_5[x]<-dat$elect.buttigieg_5[x]}
  else if (dat$therm.highest.bloomberg_5[x]==1) {dat$elect.mostliked_5[x]<-dat$elect.bloomberg_5[x]}
  else{dat$elect.mostliked_5[x]<-NA}
}

dat$strat.elect3_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$elec.top3_5[x]) | is.na(dat$elect.mostliked_5[x]) | is.na(dat$therm.tie_5[x])) {dat$strat.elect3_5[x]<-NA}
  else if(dat$elec.top3_5[x]>dat$elect.mostliked_5[x]) {dat$strat.elect3_5[x]<-1}
  else if(dat$elec.top3_5[x]==dat$elect.mostliked_5[x] & dat$therm.tie_5[x]>1) {dat$strat.elect3_5[x]<-1}
  else {dat$strat.elect3_5[x]<-0}
}

###Remove anyone who can't be definitively classified
dat$strat.elect3_5<-ifelse(dat$DEM1_W5<=5, dat$strat.elect3_5, NA)

# Viability Strategic Voting
##Wave 4
###Get out most liked candidate viability
dat$via.mostliked_4<-NA
for(x in 1:nrow(dat)){
  if(dat$therm.highest.biden_4[x]==1) {dat$via.mostliked_4[x]<-dat$via.biden_4[x]}
  else if (dat$therm.highest.sanders_4[x]==1) {dat$via.mostliked_4[x]<-dat$via.sanders_4[x]}
  else if (dat$therm.highest.warren_4[x]==1) {dat$via.mostliked_4[x]<-dat$via.warren_4[x]}
  else if (dat$therm.highest.harris_4[x]==1) {dat$via.mostliked_4[x]<-dat$via.harris_4[x]}
  else{dat$via.mostliked_4[x]<-NA}
}

### Viability Strategic voting
dat$strat.via_4<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$via.top_4[x]) | is.na(dat$via.mostliked_4[x]) | is.na(dat$therm.tie_4[x])) {dat$strat.via_4[x]<-NA}
  else if(dat$via.top_4[x]>dat$via.mostliked_4[x]) {dat$strat.via_4[x]<-1}
  else if(dat$via.top_4[x]==dat$via.mostliked_4[x] & dat$therm.tie_4[x]>1) {dat$strat.via_4[x]<-1}
  else {dat$strat.via_4[x]<-0}
}

###Remove anyone who can't be definitively classified
dat$strat.via_4<-ifelse(dat$DEM1_W4<=4, dat$strat.via_4, NA)

##Wave 5
##Get out most liked candidate viability
dat$via.mostliked_5<-NA
for(x in 1:nrow(dat)){
  if(dat$therm.highest.biden_5[x]==1) {dat$via.mostliked_5[x]<-dat$via.biden_5[x]}
  else if (dat$therm.highest.sanders_5[x]==1) {dat$via.mostliked_5[x]<-dat$via.sanders_5[x]}
  else if (dat$therm.highest.warren_5[x]==1) {dat$via.mostliked_5[x]<-dat$via.warren_5[x]}
  else if (dat$therm.highest.buttigieg_5[x]==1) {dat$via.mostliked_5[x]<-dat$via.buttigieg_5[x]}
  else if (dat$therm.highest.bloomberg_5[x]==1) {dat$via.mostliked_5[x]<-dat$via.bloomberg_5[x]}
  else{dat$via.mostliked_5[x]<-NA}
}

dat$strat.via_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$via.top_5[x]) | is.na(dat$via.mostliked_5[x]) | is.na(dat$therm.tie_5[x])) {dat$strat.via_5[x]<-NA}
  else if(dat$via.top_5[x]>dat$via.mostliked_5[x]) {dat$strat.via_5[x]<-1}
  else if(dat$via.top_5[x]==dat$via.mostliked_5[x] & dat$therm.tie_5[x]>1) {dat$strat.via_5[x]<-1}
  else {dat$strat.via_5[x]<-0}
}

###Remove anyone who can't be definitively classified
dat$strat.via_5<-ifelse(dat$DEM1_W5<=5, dat$strat.via_5, NA)

# Indicator for being both viability and electability strategic voter
dat$strat.ev_4<-ifelse(dat$strat.via_4==1 & dat$strat.elect_4==1, 1, 0)
dat$strat.ev_5<-ifelse(dat$strat.via_5==1 & dat$strat.elect_5==1, 1, 0)

#Indicator for liking Trump more than any of the Dem candidates (qualifies one as an outlier)
dat$outlier_4<-ifelse(dat$therm.max.val_4<=dat$thermr.trump_4, 1, 0)
dat$outlier_5<-ifelse(dat$therm.max.val_5<=dat$thermr.trump_5, 1, 0)

# Indicator for voting for someone not in the top 4 candidates (1= voted for someone not in top 4) (WAVE 4)
dat$not.top4_4<-ifelse(dat$DEM1_W4>4, 1, 0)

# Indicator for voting for someone not in the top 5 candidates (1= voted for someone not in top 5) (WAVE 5)
dat$not.top5_5<-ifelse(dat$DEM1_W5>5, 1, 0)

# Ideological Distance from most electable candidate 
dat$elect.dist_4<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM5_W4[x]) | dat$DEM5_W4[x]>4) {dat$elect.dist_4[x]=NA}
  else {dat$elect.dist_4[x]<-unlist(dat[x, index.dist_4[dat$DEM5_W4[x]]])}
}

dat$elect.dist_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM5_W5[x]) | dat$DEM5_W5[x]>5) {dat$elect.dist_5[x]=NA}
  else {dat$elect.dist_5[x]<-unlist(dat[x, index.dist_5[dat$DEM5_W5[x]]])}
}

# Ideological distance from most viable candidate
dat$via.dist_4<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM3_W4[x]) | dat$DEM3_W4[x]>4) {dat$via.dist_4[x]=NA}
  else {dat$via.dist_4[x]<-unlist(dat[x, index.dist_4[dat$DEM3_W4[x]]])}
}

dat$via.dist_5<-NA
for(x in 1:nrow(dat)){
  if(is.na(dat$DEM3_W5[x]) | dat$DEM3_W5[x]>5) {dat$via.dist_5[x]=NA}
  else {dat$via.dist_5[x]<-unlist(dat[x, index.dist_5[dat$DEM3_W5[x]]])}
}

# Did the person vote for the same candidate in wave 4 and wave 5?

dat$vote.r_4 <- dat$DEM1_W4
dat$vote.r_5 <- car::recode(dat$DEM1_W5, "1=1; 2=2; 3=4; 4=6; 5=11; 6=12; 7=8; 8=13; 9=14")

dat$vote.change <- ifelse(dat$vote.r_4==dat$vote.r_5, 0, 1)

# Separate sample halves by date
dat$end.date_4 <- ymd_hms(dat$ENDDT_W4)
dat$end.date_5 <- ymd_hms(dat$ENDDT_W5)

dat$second.half_5 <- ifelse(dat$wave45_W5==1 & dat$end.date_5<ymd("2020-03-04"), 0, 
                            ifelse(dat$wave45_W5==1 & dat$end.date_5>ymd("2020-03-04"), 1, NA))

# Identify electability/Viability conflicts
##Wave 4
dat$ev.conflict_4<-0
dat$ev.conflict_4<-ifelse(dat$elect.biden_4==dat$via.biden_4, dat$ev.conflict_4, 1)
dat$ev.conflict_4<-ifelse(dat$elect.harris_4==dat$via.harris_4, dat$ev.conflict_4, 1)
dat$ev.conflict_4<-ifelse(dat$elect.sanders_4==dat$via.sanders_4, dat$ev.conflict_4, 1)
dat$ev.conflict_4<-ifelse(dat$elect.warren_4==dat$via.warren_4, dat$ev.conflict_4, 1)

## Wave 5
dat$ev.conflict_5<-0
dat$ev.conflict_5<-ifelse(dat$elect2.biden_5==dat$via.biden_5, dat$ev.conflict_5, 1)
dat$ev.conflict_5<-ifelse(dat$elect2.buttigieg_5==dat$via.buttigieg_5, dat$ev.conflict_5, 1)
dat$ev.conflict_5<-ifelse(dat$elect2.sanders_5==dat$via.sanders_5, dat$ev.conflict_5, 1)
dat$ev.conflict_5<-ifelse(dat$elect2.warren_5==dat$via.warren_5, dat$ev.conflict_5, 1)
dat$ev.conflict_5<-ifelse(dat$elect2.bloomberg_5==dat$via.bloomberg_5, dat$ev.conflict_5, 1)

# Dichotomize Education
dat$college_4<-ifelse(dat$EDUC4_W4==4, 1, 0)
dat$college_5<-ifelse(dat$EDUC4_W5==4, 1, 0)

####Analyses####

#Percentage of Respondents voting Strategically
##Electability
mean(dat$strat.elect_4, na.rm=T)
mean(dat$strat.elect_5[dat$second.half_5==0], na.rm=T)
mean(dat$strat.elect_5[dat$second.half_5==1], na.rm=T)

### 3pt electability
mean(dat$strat.elect3_5[dat$second.half_5==0], na.rm=T)
mean(dat$strat.elect3_5[dat$second.half_5==1], na.rm=T)

##Viability
mean(dat$strat.via_4, na.rm=T)
mean(dat$strat.via_5[dat$second.half_5==0], na.rm=T)
mean(dat$strat.via_5[dat$second.half_5==1], na.rm=T)

##Combined E/V
mean(dat$strat.ev_4, na.rm=T)
mean(dat$strat.ev_5[dat$second.half_5==0], na.rm=T)
mean(dat$strat.ev_5[dat$second.half_5==1], na.rm=T)

##Percent of Electability nd Viability Strat Voters who are Combined EV Voters
mean(dat$strat.ev_4[dat$strat.elect_4==1], na.rm=T)
mean(dat$strat.ev_5[(dat$strat.elect_5==1 & dat$second.half_5==0)], na.rm=T)
mean(dat$strat.ev_5[(dat$strat.elect_5==1 & dat$second.half_5==1)], na.rm=T)

mean(dat$strat.ev_4[dat$strat.via_4==1], na.rm=T)
mean(dat$strat.ev_5[(dat$strat.via_5==1 & dat$second.half_5==0)], na.rm=T)
mean(dat$strat.ev_5[(dat$strat.via_5==1 & dat$second.half_5==1)], na.rm=T)

#Who are strategic voters voting for?
prop.table(table(dat$strat_4[dat$wave45_W4==1], dat$DEM1_W4[dat$wave45_W4==1]), 1)
prop.table(table(dat$strat_5[dat$wave45_W5==1], dat$DEM1_W5[dat$wave45_W5==1]), 1)

# Prep data for export to stata for fixed effects
dat.out5a<-dat[dat$second.half_5==0,] %>%
  dplyr::select(CaseId, strat_4, strat_5, dist.top_5, dist.top_4, elec.top_4, 
                elec.top_5, via.top_4, via.top_5, therm.max.val_4, therm.max.val_5, ppol.ind_4,
                ppol.ind_5, vote.via.c_4, vote.via.c_5, ideo_4, ideo_5, interest_5, interest_4, 
                civil.ind_4, civil.ind_5, ideo.x_4, ideo.x_5, iss.pol_4, iss.pol_5) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep = "_")

dat.out5a <- dat.out5a[!is.na(dat.out5a$CaseId),]

dat.out5b<-dat[dat$second.half_5==1,] %>%
  dplyr::select(CaseId, strat_4, strat_5, dist.top_5, dist.top_4, elec.top_4, 
                elec.top_5, via.top_4, via.top_5, therm.max.val_4, therm.max.val_5, ppol.ind_4,
                ppol.ind_5, vote.via.c_4, vote.via.c_5, ideo_4, ideo_5, interest_5, interest_4, 
                civil.ind_4, civil.ind_5, ideo.x_4, ideo.x_5, iss.pol_4, iss.pol_5) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep = "_")

dat.out5b <- dat.out5b[!is.na(dat.out5b$CaseId),]

colnames(dat.out5a) <- gsub("\\.", "_", colnames(dat.out5a))
foreign::write.dta(dat.out5a, file="Prag Primary 4-5a.dta")

colnames(dat.out5b) <- gsub("\\.", "_", colnames(dat.out5b))
foreign::write.dta(dat.out5b, file="Prag Primary 4-5b.dta")

# Are people who change strategic voting behavior changing their votes?

prop.table(table(dat$strat.change, dat$vote.change),1)
prop.table(table(dat$strat.change[dat$strat_5==1]))
prop.table(table(dat$strat.change[dat$strat_4==1]))


# What led to increase in Biden support?

dat.biden5a <- dat[dat$second.half_5==0,] %>%
  select(CaseId, interest_4, interest_5, elect.biden_4, elect.biden_5, via.biden_4, via.biden_5, 
         therm10.biden_4, therm10.biden_5, biden.dist_4, biden.dist_5, via.top_4, via.top_5, elect.sanders_4,
         elect.sanders_5, via.sanders_5, via.sanders_4, thermr.sanders_4, thermr.sanders_5, sanders.dist_4,
         sanders.dist_5, vote.biden_4, vote.biden_5) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep="_")

dat.biden5a <- dat.biden5a[!is.na(dat.biden5a$CaseId),]


dat.biden5b <- dat[dat$second.half_5==1,] %>%
  select(CaseId, interest_4, interest_5, elect.biden_4, elect.biden_5, via.biden_4, via.biden_5, 
         therm10.biden_4, therm10.biden_5, biden.dist_4, biden.dist_5, via.top_4, via.top_5, elect.sanders_4,
         elect.sanders_5, via.sanders_5, via.sanders_4, thermr.sanders_4, thermr.sanders_5, sanders.dist_4,
         sanders.dist_5, vote.biden_4, vote.biden_5) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep="_")

dat.biden5b <- dat.biden5b[!is.na(dat.biden5b$CaseId),]

m.biden5a<-plm(formula=vote.biden~elect.biden+via.biden+therm10.biden+biden.dist, data=dat.biden5a,
    model='within', index='CaseId', type='individual')

m.biden5a2<-plm(formula=therm10.biden~elect.biden+via.biden+biden.dist, data=dat.biden5a,
               model='within', index='CaseId', type='individual')

m.biden5b<-plm(formula=vote.biden~elect.biden+via.biden+therm10.biden+biden.dist, data=dat.biden5b,
               model='within', index='CaseId', type='individual')

m.biden5b2<-plm(formula=therm10.biden~elect.biden+via.biden+biden.dist, data=dat.biden5b,
               model='within', index='CaseId', type='individual')

summary(m.biden5a)
summary(m.biden5b)

stargazer(m.biden5a, m.biden5b, star.cutoffs = c(.05, .01, .001), 
          covariate.labels = c("Biden Electability", "Biden Viability", "Biden FT", "Biden Ideo Distance"))

stargazer(m.biden5a2, m.biden5b2, star.cutoffs = c(.05, .01, .001), 
          covariate.labels = c("Biden Electability", "Biden Viability", "Biden Ideo Distance"))


colnames(dat.biden5a) <- gsub("\\.", "_", colnames(dat.biden5a))
foreign::write.dta(dat.biden5a, file="Biden 4-5a.dta")

colnames(dat.biden5b) <- gsub("\\.", "_", colnames(dat.biden5b))
foreign::write.dta(dat.biden5b, file="Biden 4-5b.dta")

# Does threat, disliking trump, or polarization predict electability-based strategic voting?

m.elect.strat_4<-glm(strat.elect_4~AGE7_W4+INCOME_W4+white.dum_4+male.dum_4+EDUC4_W4+interest_4+iss.pol_4+therm10.trump_4+way.life.threat_4, 
                      family='binomial', data=dat[(dat$wave45_W4==1  & dat$outlier_4==0),])

m.elect.strat2_4<-glm(strat.elect_4~AGE7_W4+INCOME_W4+white.dum_4+male.dum_4+EDUC4_W4+interest_4+iss.pol_4+therm10.trump_4, 
                     family='binomial', data=dat[(dat$wave45_W4==1  & dat$outlier_4==0),])

m.elect.strat_5<-glm(strat.elect_5~AGE7_W5+INCOME_W5+white.dum_5+male.dum_5+EDUC4_W5+interest_5+iss.pol_5+aff.pol_5+elite.ppolar_5+therm10.trump_5+way.life.threat_5, 
                     family='binomial', data=dat[(dat$wave45_W5==1 & dat$outlier_5==0),])

m.elect.strat2_5<-glm(strat.elect_5~AGE7_W5+INCOME_W5+white.dum_5+male.dum_5+EDUC4_W5+interest_5+iss.pol_5+therm10.trump_5, 
                     family='binomial', data=dat[(dat$wave45_W5==1 & dat$outlier_5==0),])

stargazer(m.elect.strat2_4, star.cutoffs = c(.05, .01, .001), 
          covariate.labels = c("Age (7pt)", "Income", "White", "Male", "Edu (4pt)", "Interest",
                               "Issue Polar", "Trump Therm (10pt)"))

stargazer(m.elect.strat2_5, star.cutoffs = c(.05, .01, .001), 
          covariate.labels = c("Age (7pt)", "Income", "White", "Male", "Edu (4pt)", "Interest",  
                               "Issue Polar", "Trump Therm (10pt)"))

##Bivariate correlations with the DV
###Wave 4

index.biv.cor_4<-c("AGE7_W4", "INCOME_W4", "white.dum_4", "male.dum_4", "EDUC4_W4", "interest_4", 
                   "iss.pol_4", "therm10.trump_4", "way.life.threat_4")
dat.biv.cor_4<-as_tibble(matrix(NA, ncol=3, nrow=length(index.biv.cor_4)))
colnames(dat.biv.cor_4)<-c("Variable", "Correlation", "P-Value")
dat.biv.cor_4[,1]<-index.biv.cor_4

for(x in 1:length(index.biv.cor_4)){
  txt<-paste("cor.test(dat$strat.elect_4, dat$", index.biv.cor_4[x], ")", sep="")
  result<-eval(parse(text=txt))
  dat.biv.cor_4[x,2]<-result$estimate
  dat.biv.cor_4[x,3]<-result$p.value
}

cor.test(dat$college_4, dat$strat.elect_4)

###Wave 5
index.biv.cor_5<-c("AGE7_W5", "INCOME_W5", "white.dum_5", "male.dum_5", "EDUC4_W5", 
                   "interest_5", "iss.pol_5", "aff.pol_5", "elite.ppolar_5", "therm10.trump_5", 
                   "way.life.threat_5")
dat.biv.cor_5<-as_tibble(matrix(NA, ncol=3, nrow=length(index.biv.cor_5)))
colnames(dat.biv.cor_5)<-c("Variable", "Correlation", "P-Value")
dat.biv.cor_5[,1]<-index.biv.cor_5

for(x in 1:length(index.biv.cor_5)){
  txt<-paste("cor.test(dat$strat.elect_5, dat$", index.biv.cor_5[x], ")", sep="")
  result<-eval(parse(text=txt))
  dat.biv.cor_5[x,2]<-result$estimate
  dat.biv.cor_5[x,3]<-result$p.value
}

cor.test(dat$college_5, dat$strat.elect_5)

# Therms with Outliers Removed
hist(dat$therm10.trump_4[dat$outlier_4==0], ylab="Count", xlab="FT Score", main="Trump Therm (W4)")
hist(dat$therm10.trump_5[dat$outlier_5==0 & dat$second.half_5==0], ylab="Count", xlab="FT Score", main="Trump Therm (W5A)")
hist(dat$therm10.trump_5[dat$outlier_5==0 & dat$second.half_5==1], ylab="Count", xlab="FT Score", main="Trump Therm (W5B)")

hist(dat$therm10.biden_4[dat$outlier_4==0], ylab="Count", xlab="FT Score", main="Biden Therm (W4)")
hist(dat$therm10.biden_5[dat$outlier_5==0 & dat$second.half_5==0], ylab="Count", xlab="FT Score", main="Biden Therm (W5A)")
hist(dat$therm10.biden_5[dat$outlier_5==0 & dat$second.half_5==1], ylab="Count", xlab="FT Score", main="Biden Therm (W5B)")

hist(dat$therm10.sanders_4[dat$outlier_4==0], ylab="Count", xlab="FT Score", main="Sanders Therm (W4)")
hist(dat$therm10.sanders_5[dat$outlier_5==0 & dat$second.half_5==0], ylab="Count", xlab="FT Score", main="Sanders Therm (W5A)")
hist(dat$therm10.sanders_5[dat$outlier_5==0 & dat$second.half_5==1], ylab="Count", xlab="FT Score", main="Sanders Therm (W5B)")

hist(dat$therm10.harris_4[dat$outlier_4==0], ylab="Count", xlab="FT Score", main="Harris Therm (W4)")

hist(dat$therm10.warren_4[dat$outlier_4==0], ylab="Count", xlab="FT Score", main="Warren Therm (W4)")
hist(dat$therm10.warren_5[dat$outlier_5==0 & dat$second.half_5==0], ylab="Count", xlab="FT Score", main="Warren Therm (W5A)")
hist(dat$therm10.warren_5[dat$outlier_5==0 & dat$second.half_5==1], ylab="Count", xlab="FT Score", main="Warren Therm (W5B)")

hist(dat$therm10.buttigieg_5[dat$outlier_5==0 & dat$second.half_5==0], ylab="Count", xlab="FT Score", main="Buttigieg Therm (W5A)")
hist(dat$therm10.buttigieg_5[dat$outlier_5==0 & dat$second.half_5==1], ylab="Count", xlab="FT Score", main="Buttigieg Therm (W5B)")

hist(dat$therm10.bloomberg_5[dat$outlier_5==0 & dat$second.half_5==0], ylab="Count", xlab="FT Score", main="Bloomberg Therm (W5A)")
hist(dat$therm10.bloomberg_5[dat$outlier_5==0 & dat$second.half_5==1], ylab="Count", xlab="FT Score", main="Bloomberg Therm (W5B)")

# Outlier Descriptives
summary(dat$white.dum_4[dat$outlier_4==1], na.rm=T)
summary(dat$white.dum_5[dat$outlier_5==1 & dat$second.half_5==0], na.rm=T)
summary(dat$white.dum_5[dat$outlier_5==1 & dat$second.half_5==1], na.rm=T)

summary(dat$male.dum_4[dat$outlier_4==1], na.rm=T)
summary(dat$male.dum_5[dat$outlier_5==1 & dat$second.half_5==0], na.rm=T)
summary(dat$male.dum_5[dat$outlier_5==1 & dat$second.half_5==1], na.rm=T)

summary(dat$P_PARTYID7_W4[dat$outlier_4==1], na.rm=T)
summary(dat$P_PARTYID7_W5[dat$outlier_5==1 & dat$second.half_5==0], na.rm=T)
summary(dat$P_PARTYID7_W5[dat$outlier_5==1 & dat$second.half_5==1], na.rm=T)

# Prevalence of electability/viability conflicts
prop.table(table(dat$ev.conflict_4))
prop.table(table(dat$ev.conflict_5[dat$second.half_5==0]))
prop.table(table(dat$ev.conflict_5[dat$second.half_5==1]))

#################################################
####NOT USED####
################
# What Predicts Change in Strategic Voting?

m.strat5a<-plm(formula=strat~dist_top+elec_top+via_top+therm_max_val+ppol_ind+vote_via_c+ideo+interest+civil_ind+ideo_x+iss_pol,
               data=dat.out5a, model='within', index='CaseId', type='individual')

m.strat5b<-plm(formula=strat~dist_top+elec_top+via_top+therm_max_val+ppol_ind+vote_via_c+ideo+interest+civil_ind+ideo_x+iss_pol,
               data=dat.out5b, model='within', index='CaseId', type='individual')

stargazer(m.strat5a, m.strat5b)
stargazer(m.strat5b)

# PRedictors of change in support for Biden
m2.biden5a<-plm(formula=vote.biden~interest+elect.biden+via.biden+thermr.biden+biden.dist+via.top+elect.sanders+via.sanders+thermr.sanders+sanders.dist, 
                data=dat.biden5a, model='within', index='CaseId', type='individual')

m2.biden5b<-plm(formula=vote.biden~interest+elect.biden+via.biden+thermr.biden+biden.dist+via.top+elect.sanders+via.sanders+thermr.sanders+sanders.dist, 
                data=dat.biden5b, model='within', index='CaseId', type='individual')


# Cross sectional prevalence of Strategic Voting
t.test(dat$strat_4[dat$wave45_W4==1])
t.test(dat$strat_5[dat$wave45_W5==1])
t.test(dat$strat_4[dat$wave45_W4==1], dat$strat_5[dat$wave45_W5==1])

t.test(dat$strat_5[dat$second.half_5==0])
t.test(dat$strat_5[dat$second.half_5==1])
t.test(dat$strat_5[dat$second.half_5==0], dat$strat_5[dat$second.half_5==1])

t.test(dat$strat_4[dat$wave45_W4==1], dat$strat_5[dat$second.half_5==0])
t.test(dat$strat_4[dat$wave45_W4==1], dat$strat_5[dat$second.half_5==1])

#What predicts Strategic Voting?

m.predictors_4<-glm(strat_4~AGE7_W4+INCOME_W4+white.dum_4+male.dum_4+EDUC4_W4+ideo.x_4+P_PARTYID7_W4+interest_4+factor(DEM1_W4), 
                    family='binomial', data=dat[dat$wave45_W4==1,])

m.predictors_4.2<-glm(strat_4~AGE7_W4+INCOME_W4+white.dum_4+male.dum_4+EDUC4_W4+ideo.x_4+P_PARTYID7_W4+interest_4+factor(DEM1_W4)+ppol.ind_4+thermr.trump_4+way.life.threat_4, 
                      family='binomial', data=dat[dat$wave45_W4==1,])

m.predictors_5a<-glm(strat_5~AGE7_W5+INCOME_W5+white.dum_5+male.dum_5+EDUC4_W5+ideo.x_5+P_PARTYID7_W5+interest_5+factor(DEM1_W5), 
                     family='binomial', data=dat[dat$second.half_5==0,])

m.predictors_5a.2<-glm(strat_5~AGE7_W5+INCOME_W5+white.dum_5+male.dum_5+EDUC4_W5+ideo.x_5+P_PARTYID7_W5+interest_5+factor(DEM1_W5)+ppol.ind_5+aff.pol_5+elite.ppolar_5+thermr.trump_5+way.life.threat_5, 
                       family='binomial', data=dat[dat$second.half_5==0,])

m.predictors_5b<-glm(strat_5~AGE7_W5+INCOME_W5+white.dum_5+male.dum_5+EDUC4_W5+ideo.x_5+P_PARTYID7_W5+interest_5+factor(DEM1_W5), 
                     family='binomial', data=dat[dat$second.half_5==1,])

m.predictors_5b.2<-glm(strat_5~AGE7_W5+INCOME_W5+white.dum_5+male.dum_5+EDUC4_W5+ideo.x_5+P_PARTYID7_W5+interest_5+factor(DEM1_W5)+ppol.ind_5+aff.pol_5+elite.ppolar_5+thermr.trump_5+way.life.threat_5, 
                       family='binomial', data=dat[dat$second.half_5==1,])

stargazer(m.predictors_4)
stargazer(m.predictors_5a)
stargazer(m.predictors_5b)
stargazer(m.predictors_4, m.predictors_5a, m.predictors_5b)
stargazer(m.predictors_4.2)
stargazer(m.predictors_5a.2, m.predictors_5b.2)

# Biden's relative ideological distance 

##Wave 4 (1,4; 4=most distant)
dat$biden.dist.rel_4<-NA
for(x in 1:nrow(dat)){
  working.vect<-unlist(matrix(dat[x,index.dist_4], ncol=1))
  
  if(length(which(dat$biden.dist_4[x]==sort(working.vect)))==0)
  {dat$biden.dist.rel_4[x]<-NA}
  else{dat$biden.dist.rel_4[x]<-which(dat$biden.dist_4[x]==sort(working.vect))}
  
}

## Wave 5 (1,5; 5=most distant)
dat$biden.dist.rel_5<-NA
for(x in 1:nrow(dat)){
  working.vect<-unlist(matrix(dat[x,index.dist_5], ncol=1))
  
  if(length(which(dat$biden.dist_5[x]==sort(working.vect)))==0)
  {dat$biden.dist.rel_5[x]<-NA}
  else{dat$biden.dist.rel_5[x]<-which(dat$biden.dist_5[x]==sort(working.vect))}
  
}

