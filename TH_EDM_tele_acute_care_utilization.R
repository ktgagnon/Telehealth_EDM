library(data.table)
library(lubridate)
library(rEDM)
library(plotrix)
setwd("C:/Users/KG050542/OneDrive - Cerner Corporation/Desktop")
rawed <- fread("ED_daily_stats_mood_disorders.csv")
rawna <- fread("non_acute_daily_stats_mood_disorders.csv")
#rawinp <- fread("Inp_daily_stats_mood_disorders.csv")

#summary(rawed)
#str(rawded)

# convert day date field to date type and standardize
rawed <- rawed[,"date":=as.Date(from_date_of_service, format="%m/%d/%Y")]
#rawinp <- rawinp[,"date":=as.Date(from_begin_date, format="%m/%d/%Y")]
rawna <- rawna[,"date":=as.Date(dt, format="%m/%d/%Y")]

# filter down to dates of service within time of interest
ed <- rawed[date>"2018-05-31" & date<"2021-03-01",]
#inp <- rawinp[date>"2018-05-31" & date<"2021-03-01",]
na <- rawna[date>"2018-05-31" & date<"2021-03-01",]

# Remove raw data to save on memory
rm(rawna, rawed, rawinp)

# Make some plots of ED visits by avoidable type
plot(ed[av_ed_ind=="ALL",date], ed[av_ed_ind=="ALL",member_cnt], type="l", ylim=c(0,400), xlab="Date", ylab="Distinct Person Count", main="Daily ED Visits")
plot(ed[av_ed_ind=="A",date], ed[av_ed_ind=="A",member_cnt], type="l")
plot(ed[av_ed_ind=="P",date], ed[av_ed_ind=="P",member_cnt], type="l")
plot(ed[av_ed_ind=="N",date], ed[av_ed_ind=="N",member_cnt], type="l")

lines(ed[av_ed_ind=="A",date], ed[av_ed_ind=="A",member_cnt], col="red")
lines(ed[av_ed_ind=="P",date], ed[av_ed_ind=="P",member_cnt], col="orange")
lines(ed[av_ed_ind=="N",date], ed[av_ed_ind=="N",member_cnt], col="blue")
lines(ed[av_ed_ind=="Unclassified",date], ed[av_ed_ind=="Unclassified",member_cnt], col="dark green")
lines(as.Date(c("2020-03-12","2020-03-12")),c(0,500), col="black", lwd=3)
legend("topleft", 
       legend = c("All ED", "Avoid ED", "Potentially Avoid ED", "Not Avoid ED", "Unclassified"), 
       col = c("black", 
               "red",
               "orange",
               "blue",
               "dark green"), 
       lwd=1,
       pt.cex = 1, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0.01, 0.01))

# Need to replace some NAs in inpatient with zeroes, and maybe make combined ED and Inp acute care utilization member count and cost total
#nacols <- c(3,16,17,18,19,20,21,22,23,24,25,26)
#  for (j in nacols){
#    set(inp,which(is.na(inp[[j]])),j,0)
#  }

# Create dummy codes off of avoidable ed and summarize to get totals, including across POS
na <- na[grep("- Avoidable ED", na[,avoidable_ed_indicator]),'av_ed_ind':="A"]
na <- na[grep("- Potentially", na[,avoidable_ed_indicator]),'av_ed_ind':="P"]
na <- na[grep("- Not", na[,avoidable_ed_indicator]),'av_ed_ind':="N"]
na <- na[avoidable_ed_indicator=="Unclassified",'av_ed_ind':="UNC"]

na_total_nTH <- na[cde_pos != 2,.("member_cnt"=sum(member_cnt)
                            ,"claim_cnt"=sum(claim_cnt)
                            ,"paid_amount_total"=sum(paid_amount_total))
                         ,by=c("date")][
                           ,`:=`("TH_ind"=0
                           ,"av_ed_ind"="ALL")]
na_total_TH <- na[cde_pos == 2,.("member_cnt"=sum(member_cnt)
                                            ,"claim_cnt"=sum(claim_cnt)
                                            ,"paid_amount_total"=sum(paid_amount_total))
                             ,by=c("date")][
                               ,`:=`("TH_ind"=1
                               ,"av_ed_ind"="ALL")]
na_aed_nTH <- na[cde_pos != 2,.("member_cnt"=sum(member_cnt)
                                          ,"claim_cnt"=sum(claim_cnt)
                                          ,"paid_amount_total"=sum(paid_amount_total))
                           ,by=c("date","av_ed_ind")][
                             ,"TH_ind":=0]
na_aed_TH <- na[cde_pos == 2,.("member_cnt"=sum(member_cnt)
                                         ,"claim_cnt"=sum(claim_cnt)
                                         ,"paid_amount_total"=sum(paid_amount_total))
                          ,by=c("date","av_ed_ind")][
                            ,"TH_ind":=1]

setcolorder(na_aed_nTH, neworder=c("date","member_cnt","claim_cnt","paid_amount_total","TH_ind","av_ed_ind"))
setcolorder(na_aed_TH, neworder=c("date","member_cnt","claim_cnt","paid_amount_total","TH_ind","av_ed_ind"))


na_sum <- funion(na_total_nTH,na_total_TH)
na_sum <- funion(na_sum,na_aed_nTH)
na_sum <- funion(na_sum,na_aed_TH)

# Next steps, plot non-acute care data
# Aggregate non-acute care data by POS and and avoidable ed indicators
# plot aggregate non-acute care data
plot(na_sum[av_ed_ind=="ALL" & TH_ind==0, date],na_sum[av_ed_ind=="ALL" & TH_ind==0, member_cnt], col="blue", type="l",  ylim=c(0,12000), xlab="Date", ylab="Distinct Person Count", main="Daily Office & Telehealth Visits")
lines(na_sum[av_ed_ind=="ALL" & TH_ind==1, date],na_sum[av_ed_ind=="ALL" & TH_ind==1, member_cnt], col="magenta", type="l")
lines(ed[av_ed_ind=="ALL",date], ed[av_ed_ind=="ALL",member_cnt], col="red", type="l")

plot(na_sum[av_ed_ind=="ALL" & TH_ind==0, date],scale(na_sum[av_ed_ind=="ALL" & TH_ind==0, member_cnt], center=TRUE, scale=TRUE), col="blue", type="l", ylim=c(-5,5))
lines(na_sum[av_ed_ind=="ALL" & TH_ind==1, date],scale(na_sum[av_ed_ind=="ALL" & TH_ind==1, member_cnt], center=TRUE, scale=TRUE), col="green", type="l")
lines(ed[av_ed_ind=="ALL",date], scale(ed[av_ed_ind=="ALL",member_cnt], center=TRUE, scale=TRUE), col="red", type="l")

plot(ed[av_ed_ind=="A",date], ed[av_ed_ind=="A",member_cnt], type="l")
lines(na_sum[av_ed_ind=="A" & TH_ind==1, date],na_sum[av_ed_ind=="A" & TH_ind==1, member_cnt], col="purple", type="l")


plot(na_sum[av_ed_ind=="A" & TH_ind==0, date],scale(na_sum[av_ed_ind=="A" & TH_ind==0, member_cnt], center=TRUE, scale=TRUE), col="blue", type="l", ylim=c(-5,5))
lines(na_sum[av_ed_ind=="A" & TH_ind==1, date],scale(na_sum[av_ed_ind=="A" & TH_ind==1, member_cnt], center=TRUE, scale=TRUE), col="green", type="l")
lines(ed[av_ed_ind=="A",date], scale(ed[av_ed_ind=="A",member_cnt], center=TRUE, scale=TRUE), col="red", type="l")

plot(na_sum[av_ed_ind=="ALL" & TH_ind==0, date],scale(na_sum[av_ed_ind=="ALL" & TH_ind==0, member_cnt], center=TRUE, scale=TRUE), col="blue", type="l", ylim=c(-5,5), xlab="Date", ylab="Distinct Person Count")
lines(na_sum[av_ed_ind=="ALL" & TH_ind==1, date],scale(na_sum[av_ed_ind=="ALL" & TH_ind==1, member_cnt], center=TRUE, scale=TRUE), col="green", type="l")
lines(ed[av_ed_ind=="A",date], scale(ed[av_ed_ind=="A",member_cnt], center=TRUE, scale=TRUE), col="red", type="l")
lines(ed[av_ed_ind=="Unclassified",date], scale(ed[av_ed_ind=="Unclassified",member_cnt], center=TRUE, scale=TRUE), col="magenta", type="l")

par(mfrow=c(2,1))
plot(na_sum[av_ed_ind=="ALL" & TH_ind==0, date],scale(na_sum[av_ed_ind=="ALL" & TH_ind==0, member_cnt], center=TRUE, scale=TRUE), col="blue", type="l", ylim=c(-5,5), xlab="Date", ylab="Distinct Person Count", main="Office & Telehealth Visits")
lines(na_sum[av_ed_ind=="ALL" & TH_ind==1, date],scale(na_sum[av_ed_ind=="ALL" & TH_ind==1, member_cnt], center=TRUE, scale=TRUE), col="orange", type="l")
plot(ed[av_ed_ind=="A",date], scale(ed[av_ed_ind=="A",member_cnt], center=TRUE, scale=TRUE), col="dark red", type="l", ylim=c(-5,5), xlab="Date", ylab="Distinct Person Count", main="Avoidable & Unclassified ED Visits")
lines(ed[av_ed_ind=="Unclassified",date], scale(ed[av_ed_ind=="Unclassified",member_cnt], center=TRUE, scale=TRUE), col="dark green", type="l")

lines(as.Date(c("2020-03-12","2020-03-12")),c(0,12000), col="black", lwd=3)
legend("topleft", 
       legend = c("Telehealth","In-Person"), 
       col = c("blue", 
               "magenta"), 
       lwd=1,
       pt.cex = 1, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0.01, 0.01))

# Proceed with EDM analysis
# First prep data by pulling together each variable as a column in a single dataframe
edm_data_nt <- setkeyv(na_sum[TH_ind==0 & av_ed_ind=="ALL",.("date"=date, "nonTH_mem_cnt" = member_cnt,"nonTH_cost"=paid_amount_total,"nonTH_clm_cnt"=claim_cnt)], "date")
edm_data_t <- setkeyv(na_sum[TH_ind==1 & av_ed_ind=="ALL",.("date"=date, "TH_mem_cnt" = member_cnt,"TH_cost"=paid_amount_total,"TH_clm_cnt"=claim_cnt)], "date")
edm_data_e <- setkeyv(ed[av_ed_ind=="ALL",.("date"=date, "ed_mem_cnt" = member_cnt,"ed_cost"=paid_amount_total,"ed_claim_cnt"=claim_cnt, "ed_adm_cnt"=admission_cnt)], "date")
edm_data_av_ed <- setkeyv(ed[av_ed_ind=="A",.("date"=date, "av_ed_mem_cnt" = member_cnt,"av_ed_cost"=paid_amount_total,"av_ed_claim_cnt"=claim_cnt, "av_ed_adm_cnt"=admission_cnt)], "date")
edm_data_pav_ed <- setkeyv(ed[av_ed_ind=="P",.("date"=date, "pav_ed_mem_cnt" = member_cnt,"pav_ed_cost"=paid_amount_total,"pav_ed_claim_cnt"=claim_cnt, "pav_ed_adm_cnt"=admission_cnt)], "date")
edm_data_pvav_ed <- setkeyv(ed[av_ed_ind!="N"&av_ed_ind!="ALL",.("pvav_ed_mem_cnt" = sum(member_cnt),"pvav_ed_cost"=sum(paid_amount_total),"pvav_ed_claim_cnt"=sum(claim_cnt), "pvav_ed_adm_cnt"=sum(admission_cnt)), by="date"], "date")
edm_data_unclass_ed <- setkeyv(ed[av_ed_ind=="Unclassified",.("date"=date, "unc_ed_mem_cnt" = member_cnt,"unc_ed_cost"=paid_amount_total,"unc_ed_claim_cnt"=claim_cnt, "unc_ed_adm_cnt"=admission_cnt)], "date")


edm_data <- edm_data_e[edm_data_nt, nomatch=0]
edm_data <- edm_data[edm_data_t, nomatch=0]
edm_data <- edm_data[edm_data_av_ed, nomatch=0]
edm_data <- edm_data[edm_data_pav_ed, nomatch=0]
edm_data <- edm_data[edm_data_pvav_ed, nomatch=0]
edm_data <- edm_data[edm_data_unclass_ed, nomatch=0]

# Create some index, descriptions, and cyclical trends (quadratic terms)
setorderv(edm_data, "date", order=1)
edm_data[,"time":=seq(0,nrow(edm_data)-1, by=1)]
edm_data[date>"2020-03-12", "phe_ind":=1]
edm_data[date<"2020-03-13", "phe_ind":=0]
edm_data[,"DoW":=rep_len(c(seq(4,6,1),seq(0,3,1)),length.out=nrow(edm_data))] # June 1st 2018 was a Friday, so make Monday = 0 through Sunday = 6
edm_data[,"Month":=month(edm_data$date)-1]
edm_data[,"DoW2":= (DoW-3)^2] # Create DoW quadratic term (centered to reduce multicollinearity)
edm_data[,"Month2":= (Month-6)^2] # Create Month quadratic term (centered to reduce mulicollinearity)
edm_data[DoW==4,"DoW_Name":="Friday"]
edm_data[DoW==5,"DoW_Name":="Saturday"]
edm_data[DoW==6,"DoW_Name":="Sunday"]
edm_data[DoW==0,"DoW_Name":="Monday"]
edm_data[DoW==1,"DoW_Name":="Tuesday"]
edm_data[DoW==2,"DoW_Name":="Wednesday"]
edm_data[DoW==3,"DoW_Name":="Thursday"]

# Don't need anymore, I think
# edm_data[,"ed_mem_cnt_lead7":=shift(ed_mem_cnt,7, type="lead")]
# edm_data[,"ed_mem_cnt_wk_adj_diff":=ed_mem_cnt-ed_mem_cnt_lead7]
# edm_data[,"TH_mem_cnt_lead7":=shift(TH_mem_cnt,7, type="lead")]
# edm_data[,"TH_mem_cnt_wk_adj_diff":=TH_mem_cnt-TH_mem_cnt_lead7]
# edm_data[,"nonTH_mem_cnt_lead7":=shift(nonTH_mem_cnt,7, type="lead")]
# edm_data[,"nonTH_mem_cnt_wk_adj_diff":=nonTH_mem_cnt-nonTH_mem_cnt_lead7]
# plot(edm_data[,ed_mem_cnt_wk_adj_diff],type="l",col="red")
# lines(edm_data[,nonTH_mem_cnt_wk_adj_diff],col="blue")
# lines(edm_data[,TH_mem_cnt_wk_adj_diff],col="green")

# Create library and prediction ts segments for EDM cross validation.
lib_pre <- c(1,round(nrow(edm_data[phe_ind==0,])/2)) 
lib_phe <- c(min(edm_data[phe_ind==1, which=TRUE]), min(edm_data[phe_ind==1, which=TRUE]) + round((max(edm_data[phe_ind==1, which=TRUE]) - min(edm_data[phe_ind==1, which=TRUE]))/2))
pred_pre <- c(round(nrow(edm_data[phe_ind==0,])/2)+1, nrow(edm_data[phe_ind==0,]))
pred_phe <- c(min(edm_data[phe_ind==1, which=TRUE]) + round((max(edm_data[phe_ind==1, which=TRUE]) - min(edm_data[phe_ind==1, which=TRUE]))/2)+1, max(edm_data[phe_ind==1, which=TRUE]))
lib <- c(1,round(nrow(edm_data)/2)) 
pred <- c(round(nrow(edm_data)/2)+1, nrow(edm_data))
lib_evt <- c(1, max(edm_data[phe_ind==0, which=TRUE]))
pred_evt <- c(max(edm_data[phe_ind==0, which=TRUE])+1, nrow(edm_data))

### Clearly need to remove the weekly cycle of ED volume (ts needs to be stationary)
# seasonal_model_nth <- glm(nonTH_mem_cnt ~ DoW2*phe_ind + Month2*phe_ind + phe_ind + DoW + Month + time, data=edm_data)
# seasonal_model_th <- glm(TH_mem_cnt ~ DoW2*phe_ind + Month2*phe_ind + phe_ind + DoW + Month + time, data=edm_data)
# seasonal_model_pvaved <- glm(pvav_ed_mem_cnt ~ DoW2*phe_ind + Month2*phe_ind + phe_ind + DoW + Month + time, data=edm_data)
# seasonal_model_unced <- glm(unc_ed_mem_cnt ~ DoW2*phe_ind + Month2*phe_ind + phe_ind + DoW + Month + time, data=edm_data)
seasonal_model_nth <- glm(nonTH_mem_cnt ~ DoW + DoW2 + Month + Month2 + time, data=edm_data)
seasonal_model_th <- glm(TH_mem_cnt ~ DoW + DoW2  + Month + Month2 + time, data=edm_data)
seasonal_model_pvaved <- glm(pvav_ed_mem_cnt ~ DoW + DoW2 + Month + Month2 + time, data=edm_data)
seasonal_model_unced <- glm(unc_ed_mem_cnt ~ DoW + DoW2  + Month + Month2 + time, data=edm_data)

# Plot predicted lines for each model for ease of interpretation
par(mfrow=c(2,2))
plot(predict(seasonal_model_pvaved),type="l") 
plot(predict(seasonal_model_unced),type="l")
plot(predict(seasonal_model_th),type="l")
plot(predict(seasonal_model_nth),type="l")

# Add predicted values and remove from raw ts data to remove baseline, weekly, and montly cycles
edm_data[,"nth_mem_cnt_seas_t":=predict(seasonal_model_nth)]
edm_data[, "nonTH_mem_cnt_glm_adj":= nonTH_mem_cnt-nth_mem_cnt_seas_t]
edm_data[,"th_mem_cnt_seas_t":=predict(seasonal_model_th)]
edm_data[, "th_mem_cnt_glm_adj":= TH_mem_cnt-th_mem_cnt_seas_t]
edm_data[,"pvav_ed_mem_cnt_seas_t":=predict(seasonal_model_pvaved)]
edm_data[, "pvav_ed_mem_cnt_glm_adj":= pvav_ed_mem_cnt-pvav_ed_mem_cnt_seas_t]
edm_data[,"unc_ed_mem_cnt_seas_t":=predict(seasonal_model_unced)]
edm_data[, "unc_ed_mem_cnt_glm_adj":= unc_ed_mem_cnt-unc_ed_mem_cnt_seas_t]

# Plot to see raw vs. seasonal and baseline adjusted ts
par(mfrow=c(4,3))
plot(edm_data[,nonTH_mem_cnt],type="l") 
plot(predict(seasonal_model_nth),type="l", col="red")
plot(edm_data[,nonTH_mem_cnt_glm_adj],type="l", col="blue")
plot(edm_data[,TH_mem_cnt],type="l")
plot(predict(seasonal_model_th),type="l", col="red")
plot(edm_data[,th_mem_cnt_glm_adj],type="l", col="blue")
plot(edm_data[,pvav_ed_mem_cnt],type="l")
plot(predict(seasonal_model_pvaved),type="l", col="red")
plot(edm_data[,pvav_ed_mem_cnt_glm_adj],type="l", col="blue")
plot(edm_data[,unc_ed_mem_cnt],type="l")
plot(predict(seasonal_model_unced),type="l", col="red")
plot(edm_data[,unc_ed_mem_cnt_glm_adj],type="l", col="blue")
     
edm_df <- as.data.frame(edm_data)

#######################################################################################
##################### Pre-PHE time segment ############################################
#######################################################################################

#########################################################################
###### Find Embedding Dimension and Identify Nonlinearity with smap #####
#########################################################################

### Raw Avoidable + Potentially Avoidable + Unclassified ED visits ###
#######################################################
### Raw ED member count ###
simplex_ed_pre <- simplex(edm_df$pvav_ed_mem_cnt, lib=lib_pre, pred=pred_pre, E=1:50)
plot(simplex_ed_pre$E, simplex_ed_pre$rho, type="l") # Optimal E = 5
simplex_ed_pre <- simplex(edm_df$pvav_ed_mem_cnt, lib=lib_pre, pred=pred_pre, E=5, tp=1:300)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$pvav_ed_mem_cnt, lib=lib_pre, pred=pred_pre, E=5)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=4 which suggests nonlinear dynamics

### Seasonally Adjusted ED member count ###
simplex_ed_pre <- simplex(edm_df$pvav_ed_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=1:50)
plot(simplex_ed_pre$E[1:10], simplex_ed_pre$rho[1:10], type="l") # Optimal E = 2
simplex_ed_pre <- simplex(edm_df$pvav_ed_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=2, tp=1:300)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$pvav_ed_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=2)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=3 which suggests nonlinear dynamics
#############################

### Unclassified ED visits ###
#######################################################
### Raw ED member count ###
simplex_ed_pre <- simplex(edm_df$unc_ed_mem_cnt, lib=lib_pre, pred=pred_pre, E=1:50)
plot(simplex_ed_pre$E[1:5], simplex_ed_pre$rho[1:5], type="l") # Optimal E = 1
simplex_ed_pre <- simplex(edm_df$unc_ed_mem_cnt, lib=lib_pre, pred=pred_pre, E=1, tp=1:300)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$unc_ed_mem_cnt, lib=lib_pre, pred=pred_pre, E=1)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=0 which suggests no nonlinear dynamics

### Seasonally Adjusted ED member count ###
simplex_ed_pre <- simplex(edm_df$unc_ed_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=1:50)
plot(simplex_ed_pre$E[1:10], simplex_ed_pre$rho[1:10], type="l") # Optimal E = 9
simplex_ed_pre <- simplex(edm_df$unc_ed_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=9, tp=1:300)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$unc_ed_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=9)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=2 which suggests nonlinear dynamics
#############################

### NonTH member count ###
##########################
# Raw Data
simplex_ed_pre <- simplex(edm_df$nonTH_mem_cnt, lib=lib_pre, pred=pred_pre, E=1:10)
plot(simplex_ed_pre$E[1:10], simplex_ed_pre$rho[1:10], type="l") # Optimal E = 7 or maybe 3
simplex_ed_pre <- simplex(edm_df$nonTH_mem_cnt, lib=lib_pre, pred=pred_pre, E=3, tp=1:100)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$nonTH_mem_cnt, lib=lib_pre, pred=pred_pre, E=3)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=6 which suggests nonlinear dynamics

# Seasonally Adjusted
simplex_ed_pre <- simplex(edm_df$nonTH_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=1:50)
plot(simplex_ed_pre$E[1:15], simplex_ed_pre$rho[1:15], type="l") # Optimal E = 7
simplex_ed_pre <- simplex(edm_df$nonTH_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=7, tp=1:100)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$nonTH_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=7)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=8 which suggests nonlinear dynamics
##########################

### TH member count ###
##########################
# Raw Data
simplex_ed_pre <- simplex(edm_df$TH_mem_cnt, lib=lib_pre, pred=pred_pre, E=1:100)
plot(simplex_ed_pre$E[1:30], simplex_ed_pre$rho[1:30], type="l") # Optimal E = 22
simplex_ed_pre <- simplex(edm_df$TH_mem_cnt, lib=lib_pre, pred=pred_pre, E=22, tp=1:100)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$TH_mem_cnt, lib=lib_pre, pred=pred_pre, E=22)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=4 which suggests nonlinear dynamics

# Seasonally Adjusted
simplex_ed_pre <- simplex(edm_df$th_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=1:100)
plot(simplex_ed_pre$E[1:30], simplex_ed_pre$rho[1:30], type="l") # Optimal E = 21
simplex_ed_pre <- simplex(edm_df$th_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=21, tp=1:100)
plot(simplex_ed_pre$tp, simplex_ed_pre$rho, type="l") # Prediction accuracy decay over time
smap_output <- s_map(edm_df$th_mem_cnt_glm_adj, lib=lib_pre, pred=pred_pre, E=21)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)") # Peak of rho is around theta=3 which suggests nonlinear dynamics, but will perhaps quite a bit of noise
##########################


#### Now to do multivariate EDM, we can include all three variables, but we have to normalize them to not distort the state space
# Normalize each variable (x - mean(x,...)/sd(x,...))
normalize <- function(x, ...) {
  (x - mean(x, ...))/sd(x, ...)
}

edm_data <- setDT(edm_data)

edm_data <- edm_data[,`:=`(
  "pvav_ed_mc_adj_norm"=normalize(pvav_ed_mem_cnt_glm_adj)
  ,"unc_ed_mc_adj_norm"=normalize(unc_ed_mem_cnt_glm_adj)
  ,"th_mc_adj_norm"=normalize(th_mem_cnt_glm_adj)
  ,"nth_mc_adj_norm"=normalize(nonTH_mem_cnt_glm_adj)
  ,"pvav_ed_mc_norm"=normalize(pvav_ed_mem_cnt)
  ,"unc_ed_mc_norm"=normalize(unc_ed_mem_cnt)
  ,"th_mc_norm"=normalize(TH_mem_cnt)
  ,"nth_mc_norm"=normalize(nonTH_mem_cnt)
)]

edm_df <- as.data.frame(edm_data)




#==============================================================================================================================================
###############################################################################################################################################
###############################################################################################################################################
####################### P R E - P H E #########################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#==============================================================================================================================================

################################################################################
############### Simplex projection to find best embedding dimension ############
################################################################################
# Find embedding dimension for each of the raw variables
vars <- c("pvav_ed_mem_cnt", "unc_ed_mem_cnt", "nonTH_mem_cnt","TH_mem_cnt")
simplex_out_r <- lapply(vars, function(var) {
  simplex(edm_df[edm_df$phe_ind==0, c("time", var)], E = 2:22, lib = lib_pre, pred=pred_pre)
})
names(simplex_out_r) <- vars

par(mfrow=c(2,2))
for (var in names(simplex_out_r)) {
  plot(simplex_out_r[[var]]$E, simplex_out_r[[var]]$rho, type = "l", xlab = "Embedding Dimension (E)", 
       ylab = "Forecast Skill (rho)", main = var)
}

raw_best_E <- sapply(simplex_out_r, function(df) {
  df$E[which.max(df$rho)]
})
raw_best_E

# Find embedding dimension for each of the seasonally adjusted variables
vars <- c("pvav_ed_mem_cnt_glm_adj", "unc_ed_mem_cnt_glm_adj", "nonTH_mem_cnt_glm_adj","th_mem_cnt_glm_adj")
simplex_out_sa <- lapply(vars, function(var) {
  simplex(edm_df[edm_df$phe_ind==0, c("time", var)], E = 2:22, lib = lib_pre, pred=pred_pre)
})
names(simplex_out_sa) <- vars

par(mfrow=c(2,2))
for (var in names(simplex_out_sa)) {
  plot(simplex_out_sa[[var]]$E, simplex_out_sa[[var]]$rho, type = "l", xlab = "Embedding Dimension (E)", 
       ylab = "Forecast Skill (rho)", main = var)
}

season_adj_best_E <- sapply(simplex_out_sa, function(df) {
  df$E[which.max(df$rho)]
})
season_adj_best_E

# Find embedding dimension for each of the seasonally adjusted AND normalized variables
vars <-   c("pvav_ed_mc_adj_norm","unc_ed_mc_adj_norm","nth_mc_adj_norm","th_mc_adj_norm")
simplex_out_san <- lapply(vars, function(var) {
  simplex(edm_df[edm_df$phe_ind==0, c("time", var)], E = 2:22, lib = lib_pre, pred=pred_pre)
})
names(simplex_out_san) <- vars

par(mfrow=c(2,2))
for (var in names(simplex_out_san)) {
  plot(simplex_out_san[[var]]$E, simplex_out_san[[var]]$rho, type = "l", xlab = "Embedding Dimension (E)", 
       ylab = "Forecast Skill (rho)", main = var)
}

sa_norm_best_E <- sapply(simplex_out_san, function(df) {
  df$E[which.max(df$rho)]
})
sa_norm_best_E
###############################################################################
###############################################################################
###############################################################################

###############################################################################
################# Nonlinear check using smap ##################################
###############################################################################
# Raw data nonlinear check
vars <- c("pvav_ed_mem_cnt", "unc_ed_mem_cnt", "nonTH_mem_cnt","TH_mem_cnt")
smap_out <- lapply(vars, function(var) {
  s_map(edm_df[edm_df$phe_ind==0, c("time", var)], E = raw_best_E[var], lib = lib_pre, 
        pred = pred_pre)
})
names(smap_out) <- names(simplex_out_r)

par(mfrow = c(2, 2))
for (var in names(smap_out)) {
  plot(smap_out[[var]]$theta, smap_out[[var]]$rho, type = "l", xlab = "Nonlinearity (theta)", 
       ylab = "Forecast Skill (rho)", main = var)
}

# Seasonally adjusted nonlinear check
vars <- c("pvav_ed_mem_cnt_glm_adj", "unc_ed_mem_cnt_glm_adj", "nonTH_mem_cnt_glm_adj","TH_mem_cnt_glm_adj")
smap_out <- lapply(vars, function(var) {
  s_map(edm_df[edm_df$phe_ind==0, c("time", var)], E = season_adj_best_E[var], lib = lib_pre, 
        pred = pred_pre)
})
names(smap_out) <- names(simplex_out_sa)

par(mfrow = c(2, 2))
for (var in names(smap_out)) {
  plot(smap_out[[var]]$theta, smap_out[[var]]$rho, type = "l", xlab = "Nonlinearity (theta)", 
       ylab = "Forecast Skill (rho)", main = var)
}

# Seasonally adjusted and normalized nonlinear check
vars <-   c("pvav_ed_mc_adj_norm","unc_ed_mc_adj_norm","nth_mc_adj_norm","th_mc_adj_norm")
smap_out <- lapply(vars, function(var) {
  s_map(edm_df[edm_df$phe_ind==0, c("time", var)], E = sa_norm_best_E[var], lib = lib_pre, 
        pred = pred_pre)
})
names(smap_out) <- names(simplex_out_san)

par(mfrow = c(2, 2))
for (var in names(smap_out)) {
  plot(smap_out[[var]]$theta, smap_out[[var]]$rho, type = "l", xlab = "Nonlinearity (theta)", 
       ylab = "Forecast Skill (rho)", main = var)
}
###############################################################################
###############################################################################
###############################################################################

###############################################################################
############### Convergence Cross Mapping (CCM) ###############################
###############################################################################
###### This tests whether two or more variables are part of the same system, 
###### meaning they are causally related ######################################
###############################################################################

# Actually, the CCM here is for the entire time-series. Maybe filter down to pre and during PHE and re-run.

# Variables to use
#"pvav_ed_mc_adj_norm"
#"unc_ed_mc_adj_norm"
#"TH_mc_adj_norm"
#nTH_var = "nth_mc_adj_norm"

### avoidable, potentially avoidable, and unclassified ED <---> Non-TH
# non-TH causally drives pvav ED (Proactive effect - using office/outpatient should reduce ED visits)
pvavED_xmap_nTH <- ccm(edm_data[phe_ind==0], E=7, lib_column="pvav_ed_mc_adj_norm"
                       ,target_column = "nth_mc_adj_norm", lib_sizes = seq(7, 100, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE)
pvavED_xmap_nTH_means <- ccm_means(pvavED_xmap_nTH)

# pvav ED causally drives non-TH (Reactive effect - 7 and 30 day follow-up quality measures)
nTH_xmap_pvavED <- ccm(edm_data[phe_ind==0], E=7, lib_column="nth_mc_adj_norm"
                       ,target_column = "pvav_ed_mc_adj_norm" , lib_sizes = seq(7, 100, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE, stats_only=FALSE)
nTH_xmap_pvavED_means <- ccm_means(nTH_xmap_pvavED)

# Plot Results
plot(pvavED_xmap_nTH_means$lib_size, pmax(0, pvavED_xmap_nTH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Pre-COVID: Av/Pv/Unc ED <--> Non-Telehealth")
lines(nTH_xmap_pvavED_means$lib_size, pmax(0, nTH_xmap_pvavED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("non-TH causes ED (Proactive", "ED causes non-TH (Reactive)"), 
       col = c("blue", 
               "red"), 
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))



### avoidable, potentially avoidable, and unclassified ED <---> Non-TH
# non-TH causally drives pvav ED (Proactive effect - using office/outpatient should reduce ED visits)
pvavED_xmap_nTH <- ccm(edm_data[phe_ind==1,], E=7, lib_column="pvav_ed_mc_adj_norm"
                       ,target_column = "nth_mc_adj_norm", lib_sizes = seq(7, 100, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE)
pvavED_xmap_nTH_means <- ccm_means(pvavED_xmap_nTH)

# pvav ED causally drives non-TH (Reactive effect - 7 and 30 day follow-up quality measures)
nTH_xmap_pvavED <- ccm(edm_data[phe_ind==1,], E=7, lib_column="nth_mc_adj_norm"
                       ,target_column = "pvav_ed_mc_adj_norm" , lib_sizes = seq(7, 100, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE, stats_only=FALSE)
nTH_xmap_pvavED_means <- ccm_means(nTH_xmap_pvavED)

# Plot Results
plot(pvavED_xmap_nTH_means$lib_size, pmax(0, pvavED_xmap_nTH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="COVID: Av/Pv/Unc ED <--> Non-Telehealth")
lines(nTH_xmap_pvavED_means$lib_size, pmax(0, nTH_xmap_pvavED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("non-TH causes ED (Proactive", "ED causes non-TH (Reactive)"), 
       col = c("blue", 
               "red"), 
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

### avoidable, potentially avoidable, and unclassified ED <---> TH
# pvav ED causally drive TH
pvavED_xmap_TH <- ccm(edm_data[phe_ind==0,], E=17, lib_column="pvav_ed_mc_adj_norm"
                       ,target_column = "th_mc_adj_norm", lib_sizes = seq(12, 100, by=12), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE, stats_only=FALSE)
pvavED_xmap_TH_means <- ccm_means(pvavED_xmap_TH)

# TH causally drives pvav ED
TH_xmap_pvavED <- ccm(edm_data[phe_ind==0,], E=17, lib_column="th_mc_adj_norm"
                       ,target_column = "pvav_ed_mc_adj_norm", lib_sizes = seq(17, 100, by=17), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE, stats_only=FALSE)
TH_xmap_pvavED_means <- ccm_means(TH_xmap_pvavED)

# Plot Results
plot(pvavED_xmap_TH_means$lib_size, pmax(0, pvavED_xmap_TH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Pre-COVID: Av/Pv/Unc ED <--> Telehealth")
lines(TH_xmap_pvavED_means$lib_size, pmax(0, TH_xmap_pvavED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("bottomright", 
       legend = c("TH causes ED (Proactive)", "ED causes TH (Reactive)"), 
       col = c("blue", 
               "red"),
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


pvavED_xmap_TH <- ccm(edm_data[phe_ind==1,], E=17, lib_column="pvav_ed_mc_adj_norm"
                      ,target_column = "th_mc_adj_norm", lib_sizes = seq(12, 100, by=12), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE, stats_only=FALSE)
pvavED_xmap_TH_means <- ccm_means(pvavED_xmap_TH)

# TH causally drives pvav ED
TH_xmap_pvavED <- ccm(edm_data[phe_ind==1,], E=17, lib_column="th_mc_adj_norm"
                      ,target_column = "pvav_ed_mc_adj_norm", lib_sizes = seq(17, 100, by=17), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE, stats_only=FALSE)
TH_xmap_pvavED_means <- ccm_means(TH_xmap_pvavED)

# Plot Results
plot(pvavED_xmap_TH_means$lib_size, pmax(0, pvavED_xmap_TH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="COVID: Av/Pv/Unc ED <--> Telehealth")
lines(TH_xmap_pvavED_means$lib_size, pmax(0, TH_xmap_pvavED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("TH causes ED (Proactive)", "ED causes TH (Reactive)"), 
       col = c("blue", 
               "red"),
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


### unclassified ED <---> Non-TH
# non-TH causally drives pvav ED (Proactive effect - using office/outpatient should reduce ED visits)
uncED_xmap_nTH <- ccm(edm_data, E=7, lib_column="unc_ed_mc_adj_norm"
                       ,target_column = "nth_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE)
uncED_xmap_nTH_means <- ccm_means(uncED_xmap_nTH)

# pvav ED causally drives non-TH (Reactive effect - using office/outpatient in ED 7- and 30-day follow-ups)
nTH_xmap_uncED <- ccm(edm_data, E=7, lib_column="nth_mc_adj_norm"
                       ,target_column = "unc_ed_mc_adj_norm" , lib_sizes = seq(7, 365, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE, tp=10)
nTH_xmap_uncED_means <- ccm_means(nTH_xmap_uncED)

# Plot Results
plot(uncED_xmap_nTH_means$lib_size, pmax(0, uncED_xmap_nTH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Unclassified ED <--> Non-Telehealth")
lines(nTH_xmap_uncED_means$lib_size, pmax(0, nTH_xmap_uncED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("nonTH causes ED (Proactive)", "ED causes nonTH (Reactive)"), 
       col = c("blue", 
               "red"), 
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

### unclassified ED <---> TH
uncED_xmap_TH <- ccm(edm_data, E=7, lib_column="unc_ed_mc_adj_norm"
                      ,target_column = "th_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE)
uncED_xmap_TH_means <- ccm_means(uncED_xmap_TH)


TH_xmap_uncED <- ccm(edm_data, E=7, lib_column="th_mc_adj_norm"
                      ,target_column = "unc_ed_mc_adj_norm" , lib_sizes = seq(7, 365, by=7), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE, tp=10)
TH_xmap_uncED_means <- ccm_means(TH_xmap_uncED)

# Plot Results
plot(uncED_xmap_TH_means$lib_size, pmax(0, uncED_xmap_TH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Unclassified ED <--> Telehealth")
lines(TH_xmap_uncED_means$lib_size, pmax(0, TH_xmap_uncED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("TH causes ED (Proactive)", "ED causes TH (Reactive)"), 
       col = c("blue", 
               "red"),
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
###############################################################################
###############################################################################
###############################################################################


#==============================================================================================================================================
###############################################################################################################################################
###############################################################################################################################################
############################### P H E #########################################################################################################
###############################################################################################################################################
###############################################################################################################################################
#==============================================================================================================================================

################################################################################
############### Simplex projection to find best embedding dimension ############
################################################################################
# Find embedding dimension for each of the seasonally adjusted AND normalized variables
vars <-   c("pvav_ed_mc_adj_norm","unc_ed_mc_adj_norm","nth_mc_adj_norm","th_mc_adj_norm")
simplex_out_san <- lapply(vars, function(var) {
  simplex(edm_df[, c("time", var)], E = 2:22, lib = lib_phe, pred=pred_phe)
})
names(simplex_out_san) <- vars

par(mfrow=c(2,2))
for (var in names(simplex_out_san)) {
  plot(simplex_out[[var]]$E, simplex_out_san[[var]]$rho, type = "l", xlab = "Embedding Dimension (E)", 
       ylab = "Forecast Skill (rho)", main = var)
}

sa_norm_best_E <- sapply(simplex_out_san, function(df) {
  df$E[which.max(df$rho)]
})
sa_norm_best_E

# best E for each variable in the list in order:
# 5 4 7 7
# compare that to pre-phe
# 3 2 7 12
###############################################################################
###############################################################################
###############################################################################

###############################################################################
################# Nonlinear check using smap ##################################
###############################################################################
# Seasonally adjusted and normalized nonlinear check
vars <-   c("pvav_ed_mc_adj_norm","unc_ed_mc_adj_norm","nth_mc_adj_norm","th_mc_adj_norm")
smap_out <- lapply(vars, function(var) {
  s_map(edm_df[, c("time", var)], E = sa_norm_best_E[var], lib = lib_phe, pred = pred_phe)
})
names(smap_out) <- names(simplex_out_san)

par(mfrow = c(2, 2))
for (var in names(smap_out)) {
  plot(smap_out[[var]]$theta, smap_out[[var]]$rho, type = "l", xlab = "Nonlinearity (theta)", 
       ylab = "Forecast Skill (rho)", main = var)
}
# In comparison to pre-phe, the evidence for nonlinear dynamics is about the same for each variable, with most difference being the pvav_ed time series.
###############################################################################
###############################################################################
###############################################################################

###############################################################################
############### Convergence Cross Mapping (CCM) ###############################
###############################################################################
###### This tests whether two or more variables are part of the same system, 
###### meaning they are causally related ######################################
###############################################################################


# Variables to use
#"pvav_ed_mc_adj_norm"
#"unc_ed_mc_adj_norm"
#"TH_mc_adj_norm"
#nTH_var = "nth_mc_adj_norm"

### avoidable, potentially avoidable, and unclassified ED <---> Non-TH
# non-TH causally drives pvav ED (Proactive effect - using office/outpatient should reduce ED visits)
PHE_pvavED_xmap_nTH <- ccm(edm_data, E=7, lib_column="pvav_ed_mc_adj_norm"
                       ,target_column = "nth_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE)
PHE_pvavED_xmap_nTH_means <- ccm_means(PHE_pvavED_xmap_nTH)

# pvav ED causally drives non-TH (Reactive effect - 7 and 30 day follow-up quality measures)
PHE_nTH_xmap_pvavED <- ccm(edm_data, E=7, lib_column="nth_mc_adj_norm"
                       ,target_column = "pvav_ed_mc_adj_norm" , lib_sizes = seq(7, 365, by=7), num_samples = 100
                       ,random_libs=TRUE, replace=TRUE, silent=TRUE, tp=10)
PHE_nTH_xmap_pvavED_means <- ccm_means(PHE_nTH_xmap_pvavED)

# Plot Results
plot(PHE_pvavED_xmap_nTH_means$lib_size, pmax(0, PHE_pvavED_xmap_nTH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Av/Pv/Unc ED <--> Non-Telehealth")
lines(PHE_nTH_xmap_pvavED_means$lib_size, pmax(0, PHE_nTH_xmap_pvavED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("nonTH causes ED (Proactive", "ED causes nonTH (Reactive)"), 
       col = c("blue", 
               "red"), 
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

### avoidable, potentially avoidable, and unclassified ED <---> TH
# pvav ED causally drive TH
PHE_pvavED_xmap_TH <- ccm(edm_data, E=21, lib_column="pvav_ed_mc_adj_norm"
                      ,target_column = "th_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE)
PHE_pvavED_xmap_TH_means <- ccm_means(PHE_pvavED_xmap_TH)

# TH causally drives pvav ED
PHE_TH_xmap_pvavED <- ccm(edm_data, E=21, lib_column="th_mc_adj_norm"
                      ,target_column = "pvav_ed_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE)
PHE_TH_xmap_pvavED_means <- ccm_means(PHE_TH_xmap_pvavED)

# Plot Results
plot(PHE_pvavED_xmap_TH_means$lib_size, pmax(0, PHE_pvavED_xmap_TH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Av/Pv/Unc ED <--> Telehealth")
lines(PHE_TH_xmap_pvavED_means$lib_size, pmax(0, PHE_TH_xmap_pvavED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("TH causes ED (Proactive)", "ED causes TH (Reactive)"), 
       col = c("blue", 
               "red"),
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

### unclassified ED <---> Non-TH
# non-TH causally drives pvav ED (Proactive effect - using office/outpatient should reduce ED visits)
PHE_uncED_xmap_nTH <- ccm(edm_data, E=7, lib_column="unc_ed_mc_adj_norm"
                      ,target_column = "nth_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE)
PHE_uncED_xmap_nTH_means <- ccm_means(PHE_uncED_xmap_nTH)

# pvav ED causally drives non-TH (Reactive effect - using office/outpatient in ED 7- and 30-day follow-ups)
PHE_nTH_xmap_uncED <- ccm(edm_data, E=7, lib_column="nth_mc_adj_norm"
                      ,target_column = "unc_ed_mc_adj_norm" , lib_sizes = seq(7, 365, by=7), num_samples = 100
                      ,random_libs=TRUE, replace=TRUE, silent=TRUE, tp=10)
PHE_nTH_xmap_uncED_means <- ccm_means(PHE_nTH_xmap_uncED)

# Plot Results
plot(PHE_uncED_xmap_nTH_means$lib_size, pmax(0, PHE_uncED_xmap_nTH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Unclassified ED <--> Non-Telehealth")
lines(PHE_nTH_xmap_uncED_means$lib_size, pmax(0, PHE_nTH_xmap_uncED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("nonTH causes ED (Proactive)", "ED causes nonTH (Reactive)"), 
       col = c("blue", 
               "red"), 
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

### unclassified ED <---> TH
PHE_uncED_xmap_TH <- ccm(edm_data, E=7, lib_column="unc_ed_mc_adj_norm"
                     ,target_column = "th_mc_adj_norm", lib_sizes = seq(7, 365, by=7), num_samples = 100
                     ,random_libs=TRUE, replace=TRUE, silent=TRUE)
PHE_uncED_xmap_TH_means <- ccm_means(PHE_uncED_xmap_TH)


PHE_TH_xmap_uncED <- ccm(edm_data, E=7, lib_column="th_mc_adj_norm"
                     ,target_column = "unc_ed_mc_adj_norm" , lib_sizes = seq(7, 365, by=7), num_samples = 100
                     ,random_libs=TRUE, replace=TRUE, silent=TRUE, tp=10)
PHE_TH_xmap_uncED_means <- ccm_means(PHE_TH_xmap_uncED)

# Plot Results
plot(PHE_uncED_xmap_TH_means$lib_size, pmax(0, PHE_uncED_xmap_TH_means$rho), type="l", col="blue", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim=c(0,.9), main="Unclassified ED <--> Telehealth")
lines(PHE_TH_xmap_uncED_means$lib_size, pmax(0, PHE_TH_xmap_uncED_means$rho), col="red", xlab = "Library Size", ylab = "Cross Map Skill (rho)")
legend("topright", 
       legend = c("TH causes ED (Proactive)", "ED causes TH (Reactive)"), 
       col = c("blue", 
               "red"),
       lwd=1,
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
###############################################################################
###############################################################################
###############################################################################


# Test the time prediction (time lag between one causal variable and the other)
vars <- c("th_mc_adj_norm","pvav_ed_mc_adj_norm")
parms <- expand.grid(lib_column = vars, target_column = vars, tp = -90:90)
parms <- parms[parms$lib_column != parms$target_column,]
parms$E <- 7

output <- do.call(rbind, lapply(seq_len(NROW(parms)), function(i) {
  ccm(edm_data, E = parms$E[i], lib_sizes = NROW(edm_data), 
      random_libs = FALSE, lib_column = parms$lib_column[i], target_column = parms$target_column[i], 
      tp = parms$tp[i], silent = TRUE)
}))

output$direction <- paste(output$lib_column, "xmap to\n", output$target_column)

library(ggplot2)
time_delay_ccm_fig <- ggplot(output, aes(x = tp, y = rho, color = direction)) + 
  geom_line() + theme_bw()
print(time_delay_ccm_fig)

################################################################################
######################### Multivariate embedding ###############################
################################################################################
ed_th_nth_all <- edm_df[,c("time","pvav_ed_mem_cnt","unc_ed_mem_cnt","TH_mem_cnt","nonTH_mem_cnt","pvav_ed_mc_norm","unc_ed_mc_norm","th_mc_norm","nth_mc_norm","pvav_ed_mem_cnt_glm_adj", "unc_ed_mem_cnt_glm_adj","th_mem_cnt_glm_adj", "nonTH_mem_cnt_glm_adj","pvav_ed_mc_adj_norm", "unc_ed_mc_adj_norm","th_mc_adj_norm", "nth_mc_adj_norm")]
ed_th_nth_pre <- edm_df[edm_df$phe_ind==0,c("time","pvav_ed_mem_cnt","unc_ed_mem_cnt","TH_mem_cnt","nonTH_mem_cnt","pvav_ed_mc_norm","unc_ed_mc_norm","th_mc_norm","nth_mc_norm","pvav_ed_mem_cnt_glm_adj", "unc_ed_mem_cnt_glm_adj","th_mem_cnt_glm_adj", "nonTH_mem_cnt_glm_adj","pvav_ed_mc_adj_norm", "unc_ed_mc_adj_norm","th_mc_adj_norm", "nth_mc_adj_norm")]
ed_th_nth_phe <- edm_df[edm_df$phe_ind==1,c("time","pvav_ed_mem_cnt","unc_ed_mem_cnt","TH_mem_cnt","nonTH_mem_cnt","pvav_ed_mc_norm","unc_ed_mc_norm","th_mc_norm","nth_mc_norm","pvav_ed_mem_cnt_glm_adj", "unc_ed_mem_cnt_glm_adj","th_mem_cnt_glm_adj", "nonTH_mem_cnt_glm_adj","pvav_ed_mc_adj_norm", "unc_ed_mc_adj_norm","th_mc_adj_norm", "nth_mc_adj_norm")]



lib_all <- c(1,round(nrow(ed_th_nth_all)/2))
pred_all <- c(round(nrow(ed_th_nth_all)/2)+1, nrow(ed_th_nth_all))
lib_pre <- c(1,round(nrow(ed_th_nth_pre)/2)) 
pred_pre <- c(round(nrow(ed_th_nth_pre)/2)+1, nrow(ed_th_nth_pre))
lib_phe <- c(1,round(nrow(ed_th_nth_phe)/2)) 
pred_phe <- c(round(nrow(ed_th_nth_phe)/2)+1, nrow(ed_th_nth_phe))

pre_mEDM <- block_lnlp(ed_th_nth_pre, lib=lib_pre, pred=pred_pre, first_column_time=TRUE, target_column = "pvav_ed_mc_adj_norm", columns = c("pvav_ed_mc_adj_norm","th_mc_adj_norm","nth_mc_adj_norm"), save_smap_coefficients = TRUE, stats_only=FALSE)
phe_mEDM <- block_lnlp(ed_th_nth_phe, lib=lib_phe, pred=pred_phe, first_column_time=TRUE, target_column = "pvav_ed_mc_adj_norm", columns = c("pvav_ed_mc_adj_norm","th_mc_adj_norm","nth_mc_adj_norm"), save_smap_coefficients = TRUE, stats_only=FALSE)
all_mEDM <- block_lnlp(ed_th_nth_all, lib=lib_all, pred=pred_all, first_column_time=TRUE, target_column = "pvav_ed_mc_adj_norm", columns = c("pvav_ed_mc_adj_norm","th_mc_adj_norm","nth_mc_adj_norm"), save_smap_coefficients = TRUE, stats_only=FALSE)

k_list <- c(1, 3, "sqrt", "all")
subdata <- ed_th_nth_pre[,c(14,16,17)]
mv_pre <- multiview(subdata, lib=lib_pre, pred=pred_pre, E=7, max_lag=7, k=k_list, target_column = "pvav_ed_mc_adj_norm", stats_only=FALSE, save_lagged_block=TRUE, silent=TRUE)

################################################################################
################################################################################
################################################################################




# Make surrogate data
num_surr <- 1000
surr_maxT <- make_surrogate_data(thrips_block$maxT_degC, method = "seasonal", 
                                 T_period = 12, num_surr = num_surr)
surr_Rain <- make_surrogate_data(thrips_block$Rain_mm, method = "seasonal", 
                                 T_period = 12, num_surr = num_surr)

ccm_rho_surr <- data.frame(maxT = numeric(num_surr), Rain = numeric(num_surr))

for (i in 1:num_surr) {
  ccm_rho_surr$maxT[i] <- ccm(cbind(thrips_block$Thrips_imaginis, surr_maxT[, 
                                                                            i]), E = 8, lib_column = 1, target_column = 2, lib_sizes = NROW(thrips_block), 
                              replace = FALSE, silent = TRUE)$rho
  
  ccm_rho_surr$Rain[i] <- ccm(cbind(thrips_block$Thrips_imaginis, surr_Rain[, 
                                                                            i]), E = 8, lib_column = 1, target_column = 2, lib_sizes = NROW(thrips_block), 
                              replace = FALSE, silent = TRUE)$rho
}


######################
vars <- c("th_mc_adj_norm", "nth_mc_adj_norm", "pvav_ed_mc_adj_norm")

n <- NROW(thrips_block)
ccm_rho_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars), dimnames = list(vars, 
                                                                                       vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    out_temp <- ccm(thrips_block, E = 8, lib_column = ccm_from, target_column = ccm_to, 
                    lib_sizes = n, replace = FALSE, silent = TRUE)
    ccm_rho_matrix[ccm_from, ccm_to] <- out_temp$rho
  }
}

corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars, 
                                                                              vars))

for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    cf_temp <- ccf(thrips_block[, ccm_from], thrips_block[, ccm_to], type = "correlation", 
                   lag.max = 6, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(cf_temp))
  }
}

##### Ultimately, ccm showed similar results in pre vs. PHE, so for simplicity, will continue forward with the entire time series.
