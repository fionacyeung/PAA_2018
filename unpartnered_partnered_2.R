# source("unpartnered_partnered_2.R")

library(knitr)
library(ggplot2)
# library(entropy)

###################### files and libraries for the data ######################

#setwd("C:\\UCLA\\research_projects\\PAA_2018")
data_file = "C:\\UCLA\\thesis_ideas\\homeless_poverty\\Unpartnered&NewPartnered_Final\\Unpartnered&NewPartnered_Final.dta"

library(Matrix)
library(readstata13)

data = read.dta13(data_file)

##################### randomly reduce size of single sample #############

reduce_singles = FALSE
tgt_single = 0.01 # for example, number_of_single_females  = tgt_single*number_of_paired_females
iterations = 100

###################### data processing parameters ###############

use_last_wave_only = TRUE
complete_cases_only = TRUE


###################### sanity check for the data #####################

idx = which(!is.na(data$allrespartid_w))
paired = data[idx,c("pid", "epppnum", "spanel_all", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]

# sanity check
oddidx = seq(1,nrow(paired),by=2)
odds=paired[oddidx,]
evens = paired[-oddidx,]
consq = as.numeric(row.names(odds))-as.numeric(row.names(evens))
all(consq==-1) # this should be true since couples are on consecutive rows
nrow(paired)%%2 == 0 # pairs are paired
unique(data$spanel_all) # unique panels
all(paired$newrelunpar==1) # all pairs are new (newrelupar = 1)

################## data processing function (for different years separately) #######################

process_data = function(data, complete_cases_only) {

  # separate the couples from the singles
  idx = which(!is.na(data$allrespartid_w))
  paired = data[idx,c("pid", "epppnum", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
  paired$pair_id = rep(1:(nrow(paired)/2), each=2)
  single = data[-idx,c("pid", "epppnum", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
  single$pair_id = rep(0,nrow(single))
  
  # order the partners at the same index
  paired_females = paired[paired$female_t == "Female",] # their spouses have the same indices in paired_males
  single_females = single[single$female_t == "Female",]
  paired_males = paired[paired$female_t == "Male",]
  single_males = single[single$female_t == "Male",]
  # sanity check
  nrow(paired_females) == nrow(paired_males)
  
  # only keep the singles that have never been married
  idx = which(single_females$pid %in% paired_females$pid)
  single_females = single_females[-idx,]
  last_wave = max(unique(data$wavetime))
  single_females = single_females[single_females$wavetime==last_wave,]
  idx = which(single_males$pid %in% paired_males$pid)
  single_males = single_males[-idx,]
  single_males = single_males[single_males$wavetime==last_wave,]
  
  
  if (complete_cases_only) {
    # get rid of the incomplete cases for pairs
    incompl_idx = which(!complete.cases(paired_females))
    paired_females = paired_females[-incompl_idx,]
    paired_males = paired_males[-incompl_idx,]
    incompl_idx = which(!complete.cases(paired_males))
    paired_females = paired_females[-incompl_idx,]
    paired_males = paired_males[-incompl_idx,]
    
    # get rid of the incomplete cases for singles
    single_females$allrespartid_w = rep(0, nrow(single_females))
    single_males$allrespartid_w = rep(0, nrow(single_males))
    incompl_idx = which(!complete.cases(single_females))
    single_females = single_females[-incompl_idx,]
    incompl_idx = which(!complete.cases(single_males))
    single_males = single_males[-incompl_idx,]
    # sanity check
    nrow(paired_females) == nrow(paired_males)
  }
  
  # all females (paired females first)
  Xdata = rbind(paired_females, single_females)
  # Xdata = paired_females
  # all males (paired males first)
  Zdata = rbind(paired_males, single_males)
  # Zdata = paired_males
  # sanity checks
  all(Xdata$female_t == "Female")
  all(Zdata$female_t == "Male")
  
  # get the weight for the pair (if both partners are from the same wave, just use the one with lower epppnum)
  pair_w_idx = apply(cbind(Xdata$epppnum[1:nrow(paired_females)], Zdata$epppnum[1:nrow(paired_males)]), 1, which.min)
  pair_w = cbind(Xdata$wpfinwgt_t[1:nrow(paired_females)], Zdata$wpfinwgt_t[1:nrow(paired_males)])
  pair_w = pair_w[cbind(1:length(pair_w_idx),pair_w_idx)]
  # get the weight for all females and males
  X_w=Xdata$wpfinwgt_t
  Z_w=Zdata$wpfinwgt_t
  # # sanity check
  # pair_min_epppnum = apply(cbind(Xdata$epppnum[nrow(paired_females)], Zdata$epppnum[nrow(paired_males)]), 1, min)
  # all(pair_min_epppnum < 200)
  
  # only keep columns that are attributes
  Xdata = Xdata[,c("tage_t", "race_t", "educlevel_t")]
  Zdata = Zdata[,c("tage_t", "race_t", "educlevel_t")]
  
  
  # create the adjacency matrix
  n_pairs = nrow(paired_females)
  n_sw = nrow(Xdata) - n_pairs
  n_sm = nrow(Zdata) - n_pairs
  
  # observed matching (sparse matrix)
  mu = bdiag(Diagonal(n_pairs),Matrix(0,nrow=n_sw,ncol=n_sm))
  
  Xdata = as.matrix(Xdata)
  Zdata = as.matrix(Zdata)
  
  return(list(Xdata=Xdata, Zdata=Zdata, mu=mu, 
              paired_females=paired_females, paired_males=paired_males,
              single_females=single_females, single_males=single_males,
              X_w=X_w, Z_w=Z_w, pair_w=pair_w))
}

# process_data = function(data, use_last_wave_only, complete_cases_only) {
#   
#   # only use last wave of the panel
#   if (use_last_wave_only) {
#     last_wave = max(unique(data$wavetime))
#     last_wave_idx = which(data$wavetime == last_wave)
#     data = data[last_wave_idx,]
#   }
#   
#   # separate the couples from the singles
#   idx = which(!is.na(data$allrespartid_w))
#   paired = data[idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
#   paired$pair_id = rep(1:(nrow(paired)/2), each=2)
#   single = data[-idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
#   single$pair_id = rep(0,nrow(single))
#   
#   # order the partners at the same index
#   paired_females = paired[paired$female_t == "Female",] # their spouses have the same indices in paired_males
#   single_females = single[single$female_t == "Female",]
#   paired_males = paired[paired$female_t == "Male",]
#   single_males = single[single$female_t == "Male",]
#   # sanity check
#   nrow(paired_females) == nrow(paired_males)
#   
#   if (complete_cases_only) {
#     # get rid of the incomplete cases for pairs
#     incompl_idx = which(!complete.cases(paired_females))
#     paired_females = paired_females[-incompl_idx,]
#     paired_males = paired_males[-incompl_idx,]
#     incompl_idx = which(!complete.cases(paired_males))
#     paired_females = paired_females[-incompl_idx,]
#     paired_males = paired_males[-incompl_idx,]
#     
#     # get rid of the incomplete cases for singles
#     single_females$allrespartid_w = rep(0, nrow(single_females))
#     single_males$allrespartid_w = rep(0, nrow(single_males))
#     incompl_idx = which(!complete.cases(single_females))
#     single_females = single_females[-incompl_idx,]
#     incompl_idx = which(!complete.cases(single_males))
#     single_males = single_males[-incompl_idx,]
#     # sanity check
#     nrow(paired_females) == nrow(paired_males)
#   }
#   
#   # all females (paired females first)
#   Xdata = rbind(paired_females, single_females)
#   # all males (paired males first)
#   Zdata = rbind(paired_males, single_males)
#   # sanity checks
#   all(Xdata$female_t == "Female")
#   all(Zdata$female_t == "Male")
#   
#   
#   # only keep columns that are attributes
#   Xdata = Xdata[,c("tage_t", "race_t", "educlevel_t")]
#   Zdata = Zdata[,c("tage_t", "race_t", "educlevel_t")]
#   
#   
#   # create the adjacency matrix
#   n_pairs = nrow(paired_females)
#   n_sw = nrow(Xdata) - n_pairs
#   n_sm = nrow(Zdata) - n_pairs
#   
#   # observed matching (sparse matrix)
#   mu = bdiag(Diagonal(n_pairs),Matrix(0,nrow=n_sw,ncol=n_sm))
#   
#   Xdata = as.matrix(Xdata)
#   Zdata = as.matrix(Zdata)
#   
#   return(list(Xdata=Xdata, Zdata=Zdata, mu=mu, 
#               paired_females=paired_females, paired_males=paired_males,
#               single_females=single_females, single_males=single_males))
# }


######################## pre-process data ##############################

# copy co-habitant's characteristics into the spouse's characteristics 
# (we don't distinguish cohabitation and marriage for now)
cohabidx = which(data$marrcohabt_rev==2)
data$s_wpfinwgt_t[cohabidx] = data$p_wpfinwgt_t[cohabidx]
data$s_tage_t[cohabidx] = data$p_tage_t[cohabidx]
data$s_female_t[cohabidx] = data$p_female_t[cohabidx]
data$s_race_t[cohabidx] = data$p_race_t[cohabidx]
data$s_educlevel_t[cohabidx] = data$p_educlevel_t[cohabidx]

# use numeric values for factors
data$educlevel_t = as.character(data$educlevel_t)
data$educlevel_t[data$educlevel_t == "<HS"] = 1
data$educlevel_t[data$educlevel_t == "HS"] = 2
data$educlevel_t[data$educlevel_t == "SomeCollege"] = 3
data$educlevel_t[data$educlevel_t == "BA+"] = 4
data$educlevel_t = as.numeric(data$educlevel_t)
data$race_t = as.character(data$race_t)
data$race_t[data$race_t == "Hispanic"] = 1
data$race_t[data$race_t == "Black"] = 2
data$race_t[data$race_t == "White"] = 3
data$race_t[data$race_t == "Asian"] = 4
data$race_t = as.numeric(data$race_t)

# data frames by panel year
data1996 = data[data$spanel_all==1996,]
data2001 = data[data$spanel_all==2001,]
data2004 = data[data$spanel_all==2004,]
data2008 = data[data$spanel_all==2008,]
# sanity check
nrow(data1996) + nrow(data2001) + nrow(data2004) + nrow(data2008) == nrow(data)

###################### more data processing ###################

processed_1996 = process_data(data1996, complete_cases_only)
processed_2001 = process_data(data2001, complete_cases_only)
processed_2004 = process_data(data2004, complete_cases_only)
processed_2008 = process_data(data2008, complete_cases_only)
processed_all = list(Xdata=rbind(processed_1996$Xdata, processed_2001$Xdata, processed_2004$Xdata, processed_2008$Xdata),
                           Zdata=rbind(processed_1996$Zdata, processed_2001$Zdata, processed_2004$Zdata, processed_2008$Zdata),
                           mu = bdiag(processed_1996$mu, processed_2001$mu, processed_2004$mu, processed_2008$mu),
                           paired_females=rbind(processed_1996$paired_females, processed_2001$paired_females, processed_2004$paired_females, processed_2008$paired_females),
                           paired_males=rbind(processed_1996$paired_males, processed_2001$paired_males, processed_2004$paired_males, processed_2008$paired_males),
                           single_females=rbind(processed_1996$single_females, processed_2001$single_females, processed_2004$single_females, processed_2008$single_females),
                           single_males=rbind(processed_1996$single_males, processed_2001$single_males, processed_2004$single_males, processed_2008$single_males),
                           X_w = c(processed_1996$X_w, processed_2001$X_w, processed_2004$X_w, processed_2008$X_w),
                           Z_w = c(processed_1996$Z_w, processed_2001$Z_w, processed_2004$Z_w, processed_2008$Z_w),
                           pair_w = c(processed_1996$pair_w, processed_2001$pair_w, processed_2004$pair_w, processed_2008$pair_w))

print(paste0("1996: ", nrow(processed_1996$paired_females)*2/(nrow(processed_1996$single_females)+nrow(processed_1996$single_males)+nrow(processed_1996$paired_females)*2), " are paired"))
print(paste0("2001: ", nrow(processed_2001$paired_females)*2/(nrow(processed_2001$single_females)+nrow(processed_2001$single_males)+nrow(processed_2001$paired_females)*2), " are paired"))
print(paste0("2004: ", nrow(processed_2004$paired_females)*2/(nrow(processed_2004$single_females)+nrow(processed_2004$single_males)+nrow(processed_2004$paired_females)*2), " are paired"))
print(paste0("2008: ", nrow(processed_2008$paired_females)*2/(nrow(processed_2008$single_females)+nrow(processed_2008$single_males)+nrow(processed_2008$paired_females)*2), " are paired"))
print(paste0("all: ", nrow(processed_all$paired_females)*2/(nrow(processed_all$single_females)+nrow(processed_all$single_males)+nrow(processed_all$paired_females)*2), " are paired"))

################# source the algo files and load the libraries ################

library(tictoc)
# library(ggplot2)

source("fitrpm_R_CP.R")
source("loglikelihood_CP.R")
source("equality_constraint_CP.R")
source("rpm.model.matrix.R")
source("choice_probability.R")
source("check_CP.R")
source("create_counterfactual_distri.R")
source("asymptotic_var.R")

################## source some files for plotting data ################

source("edu_hypo_hyper_same.R")

################## set forumula ##########################

# specify the formula for utilities
# example: ff = ~ b1cov("f1") + b2cov("f1") + b1absdiff("f1",1) + b2absdiff("f1",1)

# takes a few seconds
ff = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t")

# takes > 1 min
# ff = ~ b1homophily("educlevel_t") + b2homophily("educlevel_t") + b1homophily("race_t") + b2homophily("race_t")
# ff = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t") + b1homophily("race_t") + b2homophily("race_t") # takes a a couple minutes

# ff = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t") +
#   b1absdiff("educlevel_t",1) + b2absdiff("educlevel_t",1) +
#   b1greaterthan("educlevel_t") + b2greaterthan("educlevel_t")

# interactions
# ff = ~ b1homophily("educlevel_t","race_t") + b2homophily("educlevel_t","race_t")

################## run parameters ############################

# symmetric beta for b1 and b2
symmetric = FALSE

# sampling protocol
sample = "INDIV" # sampling individuals
# sample = "COUPLE" # sampling couples only
# sample = "HOUSEHOLD" # sampling households, which can be a single individual or a couple


################## algorithm parameters #######################

control = list("algorithm"="NLOPT_LD_SLSQP", "symmetric"=symmetric, "sampling_protocol"=sample,
               "xtol_rel"=1.0e-8, "print_level"=0,"maxeval"=1000, 
               "ftol_rel"=1.0e-8,"check_derivatives"=FALSE,"ftol_abs"=1.0e-6,"hessian"=TRUE) 

# starting parameter for estimation
# theta_0 = c(rep(0,numBeta), rep(1,numGamma))
theta_0 = NULL


################################################################
################# run algorithm ################################



tic.clearlog()
tic("start")

############# fit 1996 data #####################

Xdata = processed_1996$Xdata
Zdata = processed_1996$Zdata
mu = processed_1996$mu
X_w = processed_1996$X_w
Z_w = processed_1996$Z_w
pair_w = processed_1996$pair_w

# Compute MLE based on an observed matching
out <- fitrpm_R_CP(ff, mu, Xdata, Zdata, X_w, Z_w, pair_w, theta_0, control=control)

toc(log=TRUE, quiet=FALSE)

print("Coeff:")
print(out$solution)
print("equality:")
print(out$eq)
print("Likelihood ratio:")
print(as.numeric(out$loglik/out$loglik.null))
print("Standard error:")
print(out$covar)

# output table
dff = data.frame(out$solution)
kable(dff)

  
# check the reconstructed joint density for the matching generated from 
# the estimated parameters

# best_theta = find_best_theta(beta_med, beta_sim, numBeta, numGamma)

# compare estimated joint probabilities with truth
pmfj = check_CP_latent(ff, out$solution, mu, Xdata, Zdata, X_w, Z_w, pair_w, control[["sampling_protocol"]], control[["symmetric"]])
print("estimated joint probabilities")
print(pmfj$pmfj_est)
print("observed joint probabilities")
print(pmfj$pmfj_obs)

# KL-divergence = sum(p*log(p/q))
kl_1 = sum(pmfj$pmfj_est*log(pmfj$pmfj_est/pmfj$pmfj_obs), na.rm=T)
print("KL-divergence (1): p = 1996 est. preference, q = 1996 observed")
print(kl_1)
# kl_2 = KL.plugin(freqs1 = matrix(pmfj$pmfj_est,ncol=1),freqs2 = matrix(pmfj$pmfj_obs,ncol=1))
# print("KL-divergence (2): p = 1996 est. preference, q = 1996 observed")
# print(kl_2)

# compare counterfactual joint probabilities with observed
pmfjc = create_counterfactual_distri(ff, out$solution, processed_2008$mu, processed_2008$Xdata, processed_2008$Zdata, 
                                     processed_2008$X_w, processed_2008$Z_w, processed_2008$pair_w, control)

print("counterfactual joint probabilities")
print(pmfjc$pmfj_cf)
print("observed joint probabilities")
print(pmfjc$pmfj_obs)
# KL-divergence = sum(p*log(p/q))
klc = sum(pmfjc$pmfj_cf*log(pmfjc$pmfj_cf/pmfjc$pmfj_obs), na.rm=T)
print("KL-divergence: p = counterfactual 2008 with 1996 preference, q = 2008 observed")
print(klc)

log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
tic.clearlog()
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
writeLines(unlist(log.txt))

############# fit 2001 data ######################

Xdata = processed_2001$Xdata
Zdata = processed_2001$Zdata
mu = processed_2001$mu
X_w = processed_2001$X_w
Z_w = processed_2001$Z_w
pair_w = processed_2001$pair_w
out2001 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, X_w, Z_w, pair_w, theta_0, control=control)

dff2001 = data.frame(out2001$solution)

############# fit 2004 data ######################

Xdata = processed_2004$Xdata
Zdata = processed_2004$Zdata
mu = processed_2004$mu
X_w = processed_2004$X_w
Z_w = processed_2004$Z_w
pair_w = processed_2004$pair_w
out2004 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, X_w, Z_w, pair_w, theta_0, control=control)

dff2004 = data.frame(out2004$solution)

############# fit 2008 data ######################

Xdata = processed_2008$Xdata
Zdata = processed_2008$Zdata
mu = processed_2008$mu
X_w = processed_2008$X_w
Z_w = processed_2008$Z_w
pair_w = processed_2008$pair_w
out2008 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, X_w, Z_w, pair_w, theta_0, control=control)

dff2008 = data.frame(out2008$solution)

# ############# fit all data ######################
# 
# Xdata = processed_all$Xdata
# Zdata = processed_all$Zdata
# mu = processed_all$mu
# X_w = processed_all$X_w
# Z_w = processed_all$Z_w
# pair_w = processed_all$pair_w
# outall <- fitrpm_R_CP(ff, mu, Xdata, Zdata, X_w, Z_w, pair_w, theta_0, control=control)
# 
# dffall = data.frame(outall$solution)


############### combine coeffs from all panels ##################

print(kable(cbind(dff, dff2001, dff2004, dff2008), col.names = c(1996, 2001, 2004, 2008)))

####### compare estimated joint probabilities with truth ########
pmfj = check_CP_latent(ff, out$solution, mu, Xdata, Zdata, X_w, Z_w, pair_w, control[["sampling_protocol"]], control[["symmetric"]])
print("estimated joint probabilities")
print(pmfj$pmfj_est)
print("observed joint probabilities")
print(pmfj$pmfj_obs)
kl_1 = sum(pmfj$pmfj_est*log(pmfj$pmfj_est/pmfj$pmfj_obs), na.rm=T)
print("KL-divergence: p = 1996 est. preference, q = 1996 observed")
print(kl_1)
# kl_2 = KL.plugin(freqs1 = matrix(pmfj$pmfj_est,ncol=1),freqs2 = matrix(pmfj$pmfj_obs,ncol=1))
# print("KL-divergence (2): p = 1996 est. preference, q = 1996 observed")
# print(kl_2)


pmfj2001 = check_CP_latent(ff, out2001$solution, processed_2001$mu, processed_2001$Xdata, processed_2001$Zdata, 
                           processed_2001$X_w, processed_2001$Z_w, processed_2001$pair_w, control[["sampling_protocol"]], control[["symmetric"]])
print("estimated 2001 joint probabilities")
print(pmfj2001$pmfj_est)
print("observed 2001 joint probabilities")
print(pmfj2001$pmfj_obs)
kl_1 = sum(pmfj2001$pmfj_est*log(pmfj2001$pmfj_est/pmfj2001$pmfj_obs), na.rm=T)
print("KL-divergence: p = 2001 est. preference, q = 2001 observed")
print(kl_1)
# kl_2 = KL.plugin(freqs1 = matrix(pmfj2001$pmfj_est,ncol=1),freqs2 = matrix(pmfj2001$pmfj_obs,ncol=1))
# print("KL-divergence (2): p = 2001 est. preference, q = 2001 observed")
# print(kl_2)

pmfj2004 = check_CP_latent(ff, out2004$solution, processed_2004$mu, processed_2004$Xdata, processed_2004$Zdata, 
                           processed_2004$X_w, processed_2004$Z_w, processed_2004$pair_w, control[["sampling_protocol"]], control[["symmetric"]])
print("estimated 2004 joint probabilities")
print(pmfj2004$pmfj_est)
print("observed 2004joint probabilities")
print(pmfj2004$pmfj_obs)
kl_1 = sum(pmfj2004$pmfj_est*log(pmfj2004$pmfj_est/pmfj2004$pmfj_obs), na.rm=T)
print("KL-divergence: p = 2004 est. preference, q = 2004 observed")
print(kl_1)
# kl_2 = KL.plugin(freqs1 = matrix(pmfj2004$pmfj_est,ncol=1),freqs2 = matrix(pmfj2004$pmfj_obs,ncol=1))
# print("KL-divergence (2): p = 2004 est. preference, q = 2004 observed")
# print(kl_2)

pmfj2008 = check_CP_latent(ff, out2008$solution, processed_2008$mu, processed_2008$Xdata, processed_2008$Zdata, 
                           processed_2008$X_w, processed_2008$Z_w, processed_2008$pair_w, control[["sampling_protocol"]], control[["symmetric"]])
print("estimated 2008 joint probabilities")
print(pmfj2008$pmfj_est)
print("observed 2008 joint probabilities")
print(pmfj2008$pmfj_obs)
kl_1 = sum(pmfj2008$pmfj_est*log(pmfj2008$pmfj_est/pmfj2008$pmfj_obs), na.rm=T)
print("KL-divergence: p = 2008 est. preference, q = 2008 observed")
print(kl_1)
# kl_2 = KL.plugin(freqs1 = matrix(pmfj2008$pmfj_est,ncol=1),freqs2 = matrix(pmfj2008$pmfj_obs,ncol=1))
# print("KL-divergence (2): p = 2008 est. preference, q = 2008 observed")
# print(kl_2)

###### next counterfactual ---- using 2001 preferences with 2008 availability #####
# compare counterfactual joint probabilities with observed
pmfjc2001 = create_counterfactual_distri(ff, out2001$solution, processed_2008$mu, processed_2008$Xdata, processed_2008$Zdata, 
                             processed_2008$X_w, processed_2008$Z_w, processed_2008$pair_w, control)

print("counterfactual joint probabilities")
print(pmfjc2001$pmfj_cf)
print("observed joint probabilities")
print(pmfjc2001$pmfj_obs)
# KL-divergence = sum(p*log(p/q))
klc = sum(pmfjc2001$pmfj_cf*log(pmfjc2001$pmfj_cf/pmfjc2001$pmfj_obs), na.rm=T)
print("KL-divergence: p = counterfactual 2008 with 2001 preference, q = 2008 observed")
print(klc)

############### plot the data ####################

library(ggplot2)

df_married_single_edu_m = data.frame(edu=c(processed_1996$paired_males$educlevel_t, processed_1996$single_males$educlevel_t,
                                           processed_2001$paired_males$educlevel_t, processed_2001$single_males$educlevel_t,
                                           processed_2004$paired_males$educlevel_t, processed_2004$single_males$educlevel_t,
                                           processed_2008$paired_males$educlevel_t, processed_2008$single_males$educlevel_t),
                                     panel=c(rep("1996",nrow(processed_1996$paired_males)+nrow(processed_1996$single_males)),
                                             rep("2001",nrow(processed_2001$paired_males)+nrow(processed_2001$single_males)),
                                             rep("2004",nrow(processed_2004$paired_males)+nrow(processed_2004$single_males)),
                                             rep("2008",nrow(processed_2008$paired_males)+nrow(processed_2008$single_males))),
                                     status=c(rep(c("married","single"), c(nrow(processed_1996$paired_males),nrow(processed_1996$single_males))),
                                              rep(c("married","single"), c(nrow(processed_2001$paired_males),nrow(processed_2001$single_males))),
                                              rep(c("married","single"), c(nrow(processed_2004$paired_males),nrow(processed_2004$single_males))),
                                              rep(c("married","single"), c(nrow(processed_2008$paired_males),nrow(processed_2008$single_males)))))

ggplot(df_married_single_edu_m, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + 
  ggtitle("Male education: married vs. single") + facet_grid(panel ~ .)


df_married_single_edu_f = data.frame(edu=c(processed_1996$paired_females$educlevel_t, processed_1996$single_females$educlevel_t,
                                           processed_2001$paired_females$educlevel_t, processed_2001$single_females$educlevel_t,
                                           processed_2004$paired_females$educlevel_t, processed_2004$single_females$educlevel_t,
                                           processed_2008$paired_females$educlevel_t, processed_2008$single_females$educlevel_t),
                                     panel=c(rep("1996",nrow(processed_1996$paired_females)+nrow(processed_1996$single_females)),
                                             rep("2001",nrow(processed_2001$paired_females)+nrow(processed_2001$single_females)),
                                             rep("2004",nrow(processed_2004$paired_females)+nrow(processed_2004$single_females)),
                                             rep("2008",nrow(processed_2008$paired_females)+nrow(processed_2008$single_females))),
                                     status=c(rep(c("married","single"), c(nrow(processed_1996$paired_females),nrow(processed_1996$single_females))),
                                              rep(c("married","single"), c(nrow(processed_2001$paired_females),nrow(processed_2001$single_females))),
                                              rep(c("married","single"), c(nrow(processed_2004$paired_females),nrow(processed_2004$single_females))),
                                              rep(c("married","single"), c(nrow(processed_2008$paired_females),nrow(processed_2008$single_females)))))

ggplot(df_married_single_edu_f, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Female education: married vs. single") + facet_grid(panel ~ .)

# hist(processed_1996$paired_females[,"educlevel_t"], freq=T)
# hist(processed_1996$single_females[,"educlevel_t"], freq=T)


##################### female hypo-, hyper-gamy, and same edu ###############################

edu_hypo_hyper_same_1996 = edu_hypo_hyper_same(processed_1996$paired_females, processed_1996$paired_males)
edu_hypo_hyper_same_2001 = edu_hypo_hyper_same(processed_2001$paired_females, processed_2001$paired_males)
edu_hypo_hyper_same_2004 = edu_hypo_hyper_same(processed_2004$paired_females, processed_2004$paired_males)
edu_hypo_hyper_same_2008 = edu_hypo_hyper_same(processed_2008$paired_females, processed_2008$paired_males)

married_female = data.frame(edu=rep(c("hypogamy","homophily","hypergamy"),4), 
                            freq=c(edu_hypo_hyper_same_1996$hypo, edu_hypo_hyper_same_1996$same, edu_hypo_hyper_same_1996$hyper,
                                   edu_hypo_hyper_same_2001$hypo, edu_hypo_hyper_same_2001$same, edu_hypo_hyper_same_2001$hyper,
                                   edu_hypo_hyper_same_2004$hypo, edu_hypo_hyper_same_2004$same, edu_hypo_hyper_same_2004$hyper,
                                   edu_hypo_hyper_same_2008$hypo, edu_hypo_hyper_same_2008$same, edu_hypo_hyper_same_2008$hyper),
                            panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=3)))

ggplot(married_female, aes(x=edu, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Female: same, hypo-, hyper-gamy in education")


##################### male hypo-, hyper-gamy, and same edu ###############################

edu_hypo_hyper_same_1996 = edu_hypo_hyper_same(processed_1996$paired_males, processed_1996$paired_females)
edu_hypo_hyper_same_2001 = edu_hypo_hyper_same(processed_2001$paired_males, processed_2001$paired_females)
edu_hypo_hyper_same_2004 = edu_hypo_hyper_same(processed_2004$paired_males, processed_2004$paired_females)
edu_hypo_hyper_same_2008 = edu_hypo_hyper_same(processed_2008$paired_males, processed_2008$paired_females)

married_male = data.frame(edu=rep(c("hypogamy","homophily","hypergamy"),4), 
                          freq=c(edu_hypo_hyper_same_1996$hypo, edu_hypo_hyper_same_1996$same, edu_hypo_hyper_same_1996$hyper,
                                 edu_hypo_hyper_same_2001$hypo, edu_hypo_hyper_same_2001$same, edu_hypo_hyper_same_2001$hyper,
                                 edu_hypo_hyper_same_2004$hypo, edu_hypo_hyper_same_2004$same, edu_hypo_hyper_same_2004$hyper,
                                 edu_hypo_hyper_same_2008$hypo, edu_hypo_hyper_same_2008$same, edu_hypo_hyper_same_2008$hyper),
                          panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=3)))

ggplot(married_male, aes(x=edu, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Male: same, hypo-, hyper-gamy in education")



################ plot married females and males education distribution ########################

df1996couples = data.frame(edu=c(processed_1996$paired_males$educlevel_t, processed_1996$paired_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_1996$paired_males)), rep("female", nrow(processed_1996$paired_females)))))
ggplot(df1996couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("1996 married female and male")


df2001couples = data.frame(edu=c(processed_2001$paired_males$educlevel_t, processed_2001$paired_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_2001$paired_males)), rep("female", nrow(processed_2001$paired_females)))))
ggplot(df2001couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2001 married female and male")

df2004couples = data.frame(edu=c(processed_2004$paired_males$educlevel_t, processed_2004$paired_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_2004$paired_males)), rep("female", nrow(processed_2004$paired_females)))))
ggplot(df2004couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2004 married female and male")

df2008couples = data.frame(edu=c(processed_2008$paired_males$educlevel_t, processed_2008$paired_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_2008$paired_males)), rep("female", nrow(processed_2008$paired_females)))))
ggplot(df2008couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2008 married female and male")


############### plot single females and males education distribution #######################

df1996singles = data.frame(edu=c(processed_1996$single_males$educlevel_t, processed_1996$single_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_1996$single_males)), rep("female", nrow(processed_1996$single_females)))))
ggplot(df1996singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("1996 single female and male")

df2001singles = data.frame(edu=c(processed_2001$single_males$educlevel_t, processed_2001$single_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_2001$single_males)), rep("female", nrow(processed_2001$single_females)))))
ggplot(df2001singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2001 single female and male")

df2004singles = data.frame(edu=c(processed_2004$single_males$educlevel_t, processed_2004$single_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_2004$single_males)), rep("female", nrow(processed_2004$single_females)))))
ggplot(df2004singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2004 single female and male")

df2008singles = data.frame(edu=c(processed_2008$single_males$educlevel_t, processed_2008$single_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(processed_2008$single_males)), rep("female", nrow(processed_2008$single_females)))))
ggplot(df2008singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2008 single female and male")


################### contingency tables (rows are females)

table(processed_1996$paired_females$educlevel_t, processed_1996$paired_males$educlevel_t)
table(processed_2001$paired_females$educlevel_t, processed_2001$paired_males$educlevel_t)
table(processed_2004$paired_females$educlevel_t, processed_2004$paired_males$educlevel_t)
table(processed_2008$paired_females$educlevel_t, processed_2008$paired_males$educlevel_t)




