# source("unpartnered_partnered.R")
library(knitr)
library(ggplot2)

###################### files and libraries for the data ######################

#setwd("C://UCLA//thesis_ideas//homeless_poverty//PAA_annual_meeting_2018//poster//unpartnered_partnered")
data_file = "C:\\UCLA\\thesis_ideas\\homeless_poverty\\Unpartnered&NewPartnered_Final\\Unpartnered&NewPartnered_Final.dta"

library(Matrix)
library(readstata13)

data = read.dta13(data_file)

##################### randomly reduce size of single sample #############

reduce_singles = FALSE
tgt_single = 0.01 # for example, number_of_single_females  = tgt_single*number_of_paired_females
iterations = 100

###################### only use the last wave of the panel ###############

use_last_wave_only = TRUE

###################### sanity check for the data #####################

idx = which(!is.na(data$allrespartid_w))
paired = data[idx,c("pid", "spanel_all", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]

# sanity check
oddidx = seq(1,nrow(paired),by=2)
odds=paired[oddidx,]
evens = paired[-oddidx,]
consq = as.numeric(row.names(odds))-as.numeric(row.names(evens))
all(consq==-1) # this should be true since couples are on consecutive rows
nrow(paired)%%2 == 0 # pairs are paired
unique(data$spanel_all) # unique panels
all(paired$newrelunpar==1) # all pairs are new (newrelupar = 1)


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


###################### 1996 panel #############################

# separate the couples from the singles
idx = which(!is.na(data1996$allrespartid_w))
paired1996 = data1996[idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
paired1996$pair_id = rep(1:(nrow(paired1996)/2), each=2)
single1996 = data1996[-idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
single1996$pair_id = rep(0,nrow(single1996))

# order the partners at the same index
paired1996_females = paired1996[paired1996$female_t == "Female",] # their spouses have the same indices in paired1996_males
single1996_females = single1996[single1996$female_t == "Female",]
paired1996_males = paired1996[paired1996$female_t == "Male",]
single1996_males = single1996[single1996$female_t == "Male",]
# sanity check
nrow(paired1996_females) == nrow(paired1996_males)

# get rid of the incomplete cases for pairs
incompl_idx = which(!complete.cases(paired1996_females))
paired1996_females = paired1996_females[-incompl_idx,]
paired1996_males = paired1996_males[-incompl_idx,]
incompl_idx = which(!complete.cases(paired1996_males))
paired1996_females = paired1996_females[-incompl_idx,]
paired1996_males = paired1996_males[-incompl_idx,]

# get rid of the incomplete cases for singles
single1996_females$allrespartid_w = rep(0, nrow(single1996_females))
single1996_males$allrespartid_w = rep(0, nrow(single1996_males))
incompl_idx = which(!complete.cases(single1996_females))
single1996_females = single1996_females[-incompl_idx,]
incompl_idx = which(!complete.cases(single1996_males))
single1996_males = single1996_males[-incompl_idx,]
# sanity check
nrow(paired1996_females) == nrow(paired1996_males)

# reduce single sample
if (reduce_singles) {
  keep_idx = sample(1:nrow(single1996_females), tgt_single*nrow(paired1996_females))
  single1996_females = single1996_females[keep_idx,]
  keep_idx = sample(1:nrow(single1996_males), tgt_single*nrow(paired1996_males))
  single1996_males = single1996_males[keep_idx,]
}

# only use last wave of the panel
if (use_last_wave_only) {
  last_wave = max(unique(data1996$wavetime))
  last_wave_idx = which(paired1996_females$wavetime == last_wave)
  paired1996_females = paired1996_females[last_wave_idx,]
  paired1996_males = paired1996_males[last_wave_idx,]
  single1996_females = single1996_females[single1996_females$wavetime==last_wave,]
  single1996_males = single1996_males[single1996_males$wavetime==last_wave,]
}

# all females (paired females first)
Xdata1996 = rbind(paired1996_females, single1996_females)
# all males (paired males first)
Zdata1996 = rbind(paired1996_males, single1996_males)
# sanity checks
all(Xdata1996$female_t == "Female")
all(Zdata1996$female_t == "Male")


# only keep columns that are attributes
Xdata1996 = Xdata1996[,c("tage_t", "race_t", "educlevel_t")]
Zdata1996 = Zdata1996[,c("tage_t", "race_t", "educlevel_t")]


# create the adjacency matrix
n_pairs = nrow(paired1996_females)
n_sw = nrow(Xdata1996) - n_pairs
n_sm = nrow(Zdata1996) - n_pairs

# observed matching (sparse matrix)
mu1996 = bdiag(Diagonal(n_pairs),Matrix(0,nrow=n_sw,ncol=n_sm))

Xdata1996 = as.matrix(Xdata1996)
Zdata1996 = as.matrix(Zdata1996)


######################### 2001 panel #################################

# separate the couples from the singles
idx = which(!is.na(data2001$allrespartid_w))
paired2001 = data2001[idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
paired2001$pair_id = rep(1:(nrow(paired2001)/2), each=2)
single2001 = data2001[-idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
single2001$pair_id = rep(0,nrow(single2001))

# order the partners at the same index
paired2001_females = paired2001[paired2001$female_t == "Female",] # their spouses have the same indices in paired2001_males
single2001_females = single2001[single2001$female_t == "Female",]
paired2001_males = paired2001[paired2001$female_t == "Male",]
single2001_males = single2001[single2001$female_t == "Male",]
# sanity check
nrow(paired2001_females) == nrow(paired2001_males)

# get rid of the incomplete cases for pairs
incompl_idx = which(!complete.cases(paired2001_females))
paired2001_females = paired2001_females[-incompl_idx,]
paired2001_males = paired2001_males[-incompl_idx,]
incompl_idx = which(!complete.cases(paired2001_males))
paired2001_females = paired2001_females[-incompl_idx,]
paired2001_males = paired2001_males[-incompl_idx,]

# get rid of the incomplete cases for singles
single2001_females$allrespartid_w = rep(0, nrow(single2001_females))
single2001_males$allrespartid_w = rep(0, nrow(single2001_males))
incompl_idx = which(!complete.cases(single2001_females))
single2001_females = single2001_females[-incompl_idx,]
incompl_idx = which(!complete.cases(single2001_males))
single2001_males = single2001_males[-incompl_idx,]
# sanity check
nrow(paired2001_females) == nrow(paired2001_males)

# reduce single sample
if (reduce_singles) {
  keep_idx = sample(1:nrow(single2001_females), tgt_single*nrow(paired2001_females))
  single2001_females = single2001_females[keep_idx,]
  keep_idx = sample(1:nrow(single2001_males), tgt_single*nrow(paired2001_males))
  single2001_males = single2001_males[keep_idx,]
}

# only use last wave of the panel
if (use_last_wave_only) {
  last_wave = max(unique(data2001$wavetime))
  last_wave_idx = which(paired2001_females$wavetime == last_wave)
  paired2001_females = paired2001_females[last_wave_idx,]
  paired2001_males = paired2001_males[last_wave_idx,]
  single2001_females = single2001_females[single2001_females$wavetime==last_wave,]
  single2001_males = single2001_males[single2001_males$wavetime==last_wave,]
}

# all females (paired females first)
Xdata2001 = rbind(paired2001_females, single2001_females)
# all males (paired males first)
Zdata2001 = rbind(paired2001_males, single2001_males)
# sanity checks
all(Xdata2001$female_t == "Female")
all(Zdata2001$female_t == "Male")

# only keep columns that are attributes
Xdata2001 = Xdata2001[,c("tage_t", "race_t", "educlevel_t")]
Zdata2001 = Zdata2001[,c("tage_t", "race_t", "educlevel_t")]


# create the adjacency matrix
n_pairs = nrow(paired2001_females)
n_sw = nrow(Xdata2001) - n_pairs
n_sm = nrow(Zdata2001) - n_pairs

# observed matching (sparse matrix)
mu2001 = bdiag(Diagonal(n_pairs),Matrix(0,nrow=n_sw,ncol=n_sm))

Xdata2001 = as.matrix(Xdata2001)
Zdata2001 = as.matrix(Zdata2001)


######################### 2004 panel #################################

# separate the couples from the singles
idx = which(!is.na(data2004$allrespartid_w))
paired2004 = data2004[idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
paired2004$pair_id = rep(1:(nrow(paired2004)/2), each=2)
single2004 = data2004[-idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
single2004$pair_id = rep(0,nrow(single2004))

# order the partners at the same index
paired2004_females = paired2004[paired2004$female_t == "Female",] # their spouses have the same indices in paired2004_males
single2004_females = single2004[single2004$female_t == "Female",]
paired2004_males = paired2004[paired2004$female_t == "Male",]
single2004_males = single2004[single2004$female_t == "Male",]
# sanity check
nrow(paired2004_females) == nrow(paired2004_males)

# get rid of the incomplete cases for pairs
incompl_idx = which(!complete.cases(paired2004_females))
paired2004_females = paired2004_females[-incompl_idx,]
paired2004_males = paired2004_males[-incompl_idx,]
incompl_idx = which(!complete.cases(paired2004_males))
paired2004_females = paired2004_females[-incompl_idx,]
paired2004_males = paired2004_males[-incompl_idx,]

# get rid of the incomplete cases for singles
single2004_females$allrespartid_w = rep(0, nrow(single2004_females))
single2004_males$allrespartid_w = rep(0, nrow(single2004_males))
incompl_idx = which(!complete.cases(single2004_females))
single2004_females = single2004_females[-incompl_idx,]
incompl_idx = which(!complete.cases(single2004_males))
single2004_males = single2004_males[-incompl_idx,]
# sanity check
nrow(paired2004_females) == nrow(paired2004_males)

# reduce single sample
if (reduce_singles) {
  keep_idx = sample(1:nrow(single2004_females), tgt_single*nrow(paired2004_females))
  single2004_females = single2004_females[keep_idx,]
  keep_idx = sample(1:nrow(single2004_males), tgt_single*nrow(paired2004_males))
  single2004_males = single2004_males[keep_idx,]
}

# only use last wave of the panel
if (use_last_wave_only) {
  last_wave = max(unique(data2004$wavetime))
  last_wave_idx = which(paired2004_females$wavetime == last_wave)
  paired2004_females = paired2004_females[last_wave_idx,]
  paired2004_males = paired2004_males[last_wave_idx,]
  single2004_females = single2004_females[single2004_females$wavetime==last_wave,]
  single2004_males = single2004_males[single2004_males$wavetime==last_wave,]
}

# all females (paired females first)
Xdata2004 = rbind(paired2004_females, single2004_females)
# all males (paired males first)
Zdata2004 = rbind(paired2004_males, single2004_males)
# sanity checks
all(Xdata2004$female_t == "Female")
all(Zdata2004$female_t == "Male")

# only keep columns that are attributes
Xdata2004 = Xdata2004[,c("tage_t", "race_t", "educlevel_t")]
Zdata2004 = Zdata2004[,c("tage_t", "race_t", "educlevel_t")]


# create the adjacency matrix
n_pairs = nrow(paired2004_females)
n_sw = nrow(Xdata2004) - n_pairs
n_sm = nrow(Zdata2004) - n_pairs

# observed matching (sparse matrix)
mu2004 = bdiag(Diagonal(n_pairs),Matrix(0,nrow=n_sw,ncol=n_sm))

Xdata2004 = as.matrix(Xdata2004)
Zdata2004 = as.matrix(Zdata2004)


######################### 2008 panel #################################


# separate the couples from the singles
idx = which(!is.na(data2008$allrespartid_w))
paired2008 = data2008[idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
paired2008$pair_id = rep(1:(nrow(paired2008)/2), each=2)
single2008 = data2008[-idx,c("pid", "spanel_all", "wavetime", "wpfinwgt_t", "tage_t", "female_t", "race_t", "educlevel_t", "allrespartid_w", "newrelunpar" )]
single2008$pair_id = rep(0,nrow(single2008))

# order the partners at the same index
paired2008_females = paired2008[paired2008$female_t == "Female",] # their spouses have the same indices in paired2008_males
single2008_females = single2008[single2008$female_t == "Female",]
paired2008_males = paired2008[paired2008$female_t == "Male",]
single2008_males = single2008[single2008$female_t == "Male",]
# sanity check
nrow(paired2008_females) == nrow(paired2008_males)

# get rid of the incomplete cases for pairs
incompl_idx = which(!complete.cases(paired2008_females))
paired2008_females = paired2008_females[-incompl_idx,]
paired2008_males = paired2008_males[-incompl_idx,]
incompl_idx = which(!complete.cases(paired2008_males))
paired2008_females = paired2008_females[-incompl_idx,]
paired2008_males = paired2008_males[-incompl_idx,]

# get rid of the incomplete cases for singles
single2008_females$allrespartid_w = rep(0, nrow(single2008_females))
single2008_males$allrespartid_w = rep(0, nrow(single2008_males))
incompl_idx = which(!complete.cases(single2008_females))
single2008_females = single2008_females[-incompl_idx,]
incompl_idx = which(!complete.cases(single2008_males))
single2008_males = single2008_males[-incompl_idx,]
# sanity check
nrow(paired2008_females) == nrow(paired2008_males)

# reduce single sample
if (reduce_singles) {
  keep_idx = sample(1:nrow(single2008_females), tgt_single*nrow(paired2008_females))
  single2008_females = single2008_females[keep_idx,]
  keep_idx = sample(1:nrow(single2008_males), tgt_single*nrow(paired2008_males))
  single2008_males = single2008_males[keep_idx,]
}

# only use last wave of the panel
if (use_last_wave_only) {
  last_wave = max(unique(data2008$wavetime))
  last_wave_idx = which(paired2008_females$wavetime == last_wave)
  paired2008_females = paired2008_females[last_wave_idx,]
  paired2008_males = paired2008_males[last_wave_idx,]
  single2008_females = single2008_females[single2008_females$wavetime==last_wave,]
  single2008_males = single2008_males[single2008_males$wavetime==last_wave,]
}

# all females (paired females first)
Xdata2008 = rbind(paired2008_females, single2008_females)
# all males (paired males first)
Zdata2008 = rbind(paired2008_males, single2008_males)
# sanity checks
all(Xdata2008$female_t == "Female")
all(Zdata2008$female_t == "Male")

# only keep columns that are attributes
Xdata2008 = Xdata2008[,c("tage_t", "race_t", "educlevel_t")]
Zdata2008 = Zdata2008[,c("tage_t", "race_t", "educlevel_t")]


# create the adjacency matrix
n_pairs = nrow(paired2008_females)
n_sw = nrow(Xdata2008) - n_pairs
n_sm = nrow(Zdata2008) - n_pairs

# observed matching (sparse matrix)
mu2008 = bdiag(Diagonal(n_pairs),Matrix(0,nrow=n_sw,ncol=n_sm))

Xdata2008 = as.matrix(Xdata2008)
Zdata2008 = as.matrix(Zdata2008)

############### choose which data set you want to run algorithm with ######################

Xdata = Xdata1996
Zdata = Zdata1996
mu = mu1996

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


################## set forumula ##########################

# specify the formula for utilities
# example: ff = ~ b1cov("f1") + b2cov("f1") + b1absdiff("f1",1) + b2absdiff("f1",1)

# takes a few seconds
# ff = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t")

# takes > 1 min
# ff = ~ b1homophily("educlevel_t") + b2homophily("educlevel_t") + b1homophily("race_t") + b2homophily("race_t")
# ff = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t") + b1homophily("race_t") + b2homophily("race_t") # takes a a couple minutes

ff = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t") +
  b1absdiff("educlevel_t",1) + b2absdiff("educlevel_t",1) +
  b1greaterthan("educlevel_t") + b2greaterthan("educlevel_t")

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


################# run algorithm ################################



tic.clearlog()
tic("start")

# Compute MLE based on an observed matching
out <- fitrpm_R_CP(ff, mu, Xdata, Zdata, theta_0, control=control)

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
pmfj = check_CP_latent(ff, out$solution, mu, Xdata, Zdata, symmetric)
print("estimated joint probabilities")
print(pmfj$pmfj_est)
print("observed joint probabilities")
print(pmfj$pmfj_obs)

# KL-divergence = sum(p*log(p/q))
kl = -sum(pmfj$pmfj_est*log(pmfj$pmfj_est/pmfj$pmfj_obs), na.rm=T)
print("KL-divergence: p = 1996 est. preference, q = 1996 observed")
print(kl)

# compare counterfactual joint probabilities with observed
pmfjc = create_counterfactual_distri(ff, out$solution, mu2008, Xdata, Zdata, Xdata2008, Zdata2008, symmetric)
print("counterfactual joint probabilities")
print(pmfjc$pmfj_est)
print("observed joint probabilities")
print(pmfjc$pmfj_obs)

# KL-divergence = sum(p*log(p/q))
klc = -sum(pmfjc$pmfj_est*log(pmfjc$pmfj_est/pmfjc$pmfj_obs), na.rm=T)
print("KL-divergence: p = counterfactual 2008 with 1996 preference, q = 2008 observed")
print(klc)

log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
tic.clearlog()
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
writeLines(unlist(log.txt))

############# fit 2001 data ######################

Xdata = Xdata2001
Zdata = Zdata2001
mu = mu2001
out2001 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, theta_0, control=control)

dff2001 = data.frame(out2001$solution)

############# fit 2004 data ######################

Xdata = Xdata2004
Zdata = Zdata2004
mu = mu2004
out2004 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, theta_0, control=control)

dff2004 = data.frame(out2004$solution)

############# fit 2008 data ######################

Xdata = Xdata2008
Zdata = Zdata2008
mu = mu2008
out2008 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, theta_0, control=control)

dff2008 = data.frame(out2008$solution)


############### combine coeffs from all panels ##################

kable(cbind(dff, dff2001, dff2004, dff2008), col.names = c(1996, 2001, 2004, 2008))


############# data exploration #####################

##################### female hypo-, hyper-gamy, and same edu ###############################

edu_hypo_hyper_same_1996 = edu_hypo_hyper_same(paired1996_females, paired1996_males)
edu_hypo_hyper_same_2001 = edu_hypo_hyper_same(paired2001_females, paired2001_males)
edu_hypo_hyper_same_2004 = edu_hypo_hyper_same(paired2004_females, paired2004_males)
edu_hypo_hyper_same_2008 = edu_hypo_hyper_same(paired2008_females, paired2008_males)

married_female = data.frame(edu=rep(c("hypogamy","homophily","hypergamy"),4), 
                            freq=c(edu_hypo_hyper_same_1996$hypo, edu_hypo_hyper_same_1996$same, edu_hypo_hyper_same_1996$hyper,
                                   edu_hypo_hyper_same_2001$hypo, edu_hypo_hyper_same_2001$same, edu_hypo_hyper_same_2001$hyper,
                                   edu_hypo_hyper_same_2004$hypo, edu_hypo_hyper_same_2004$same, edu_hypo_hyper_same_2004$hyper,
                                   edu_hypo_hyper_same_2008$hypo, edu_hypo_hyper_same_2008$same, edu_hypo_hyper_same_2008$hyper),
                            panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=3)))

ggplot(married_female, aes(x=edu, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Female: same, hypo-, hyper-gamy in education")


##################### male hypo-, hyper-gamy, and same edu ###############################

edu_hypo_hyper_same_1996 = edu_hypo_hyper_same(paired1996_males, paired1996_females)
edu_hypo_hyper_same_2001 = edu_hypo_hyper_same(paired2001_males, paired2001_females)
edu_hypo_hyper_same_2004 = edu_hypo_hyper_same(paired2004_males, paired2004_females)
edu_hypo_hyper_same_2008 = edu_hypo_hyper_same(paired2008_males, paired2008_females)

married_male = data.frame(edu=rep(c("hypogamy","homophily","hypergamy"),4), 
                            freq=c(edu_hypo_hyper_same_1996$hypo, edu_hypo_hyper_same_1996$same, edu_hypo_hyper_same_1996$hyper,
                                   edu_hypo_hyper_same_2001$hypo, edu_hypo_hyper_same_2001$same, edu_hypo_hyper_same_2001$hyper,
                                   edu_hypo_hyper_same_2004$hypo, edu_hypo_hyper_same_2004$same, edu_hypo_hyper_same_2004$hyper,
                                   edu_hypo_hyper_same_2008$hypo, edu_hypo_hyper_same_2008$same, edu_hypo_hyper_same_2008$hyper),
                            panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=3)))

ggplot(married_male, aes(x=edu, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Male: same, hypo-, hyper-gamy in education")



################ plot married females and males education distribution ########################

df1996couples = data.frame(edu=c(paired1996_males$educlevel_t, paired1996_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(paired1996_males)), rep("female", nrow(paired1996_females)))))
ggplot(df1996couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("1996 married female and male")


df2001couples = data.frame(edu=c(paired2001_males$educlevel_t, paired2001_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(paired2001_males)), rep("female", nrow(paired2001_females)))))
ggplot(df2001couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2001 married female and male")

df2004couples = data.frame(edu=c(paired2004_males$educlevel_t, paired2004_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(paired2004_males)), rep("female", nrow(paired2004_females)))))
ggplot(df2004couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2004 married female and male")

df2008couples = data.frame(edu=c(paired2008_males$educlevel_t, paired2008_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(paired2008_males)), rep("female", nrow(paired2008_females)))))
ggplot(df2008couples, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2008 married female and male")


############### plot single females and males education distribution #######################

df1996singles = data.frame(edu=c(single1996_males$educlevel_t, single1996_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(single1996_males)), rep("female", nrow(single1996_females)))))
ggplot(df1996singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("1996 single female and male")

df2001singles = data.frame(edu=c(single2001_males$educlevel_t, single2001_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(single2001_males)), rep("female", nrow(single2001_females)))))
ggplot(df2001singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2001 single female and male")

df2004singles = data.frame(edu=c(single2004_males$educlevel_t, single2004_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(single2004_males)), rep("female", nrow(single2004_females)))))
ggplot(df2004singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2004 single female and male")

df2008singles = data.frame(edu=c(single2008_males$educlevel_t, single2008_females$educlevel_t), 
                           gender=as.factor(c(rep("male", nrow(single2008_males)), rep("female", nrow(single2008_females)))))
ggplot(df2008singles, aes(x=edu, fill=gender)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) + ggtitle("2008 single female and male")


############## plot proportion of people who chose to marry vs. remain single ######################
# both genders
dfmarry = data.frame(status=rep(c("married","single"),4), 
                          freq=c(nrow(paired1996), nrow(single1996_females)+nrow(single1996_males), 
                                 nrow(paired2001), nrow(single2001_females)+nrow(single2001_males),
                                 nrow(paired2004), nrow(single2004_females)+nrow(single2004_males),
                                 nrow(paired2008), nrow(single2008_females)+nrow(single2008_males)),
                          panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=2)))

ggplot(dfmarry, aes(x=status, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Both genders: married vs. single")

# both genders (ratio)
dfmarryr = data.frame(freq=c((nrow(single1996_females)+nrow(single1996_males))/nrow(paired1996), 
                            (nrow(single2001_females)+nrow(single2001_males))/nrow(paired2001),
                            (nrow(single2004_females)+nrow(single2004_males))/nrow(paired2004),
                            (nrow(single2008_females)+nrow(single2008_males))/nrow(paired2008)),
                     panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=2)))

ggplot(dfmarryr, aes(x=panel, y=freq)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Both genders: single/married")



# female only (raw number)
dfmarryf = data.frame(status=rep(c("married","single"),4), 
                     freq=c(nrow(paired1996_females), nrow(single1996_females), 
                            nrow(paired2001_females), nrow(single2001_females),
                            nrow(paired2004_females), nrow(single2004_females),
                            nrow(paired2008_females), nrow(single2008_females)),
                     panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=2)))

ggplot(dfmarryf, aes(x=status, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Female: married vs. single")


# # female only (ratio)
# dfmarryfr = data.frame(freq=c(nrow(paired1996_females)/nrow(single1996_females), 
#                              nrow(paired2001_females)/nrow(single2001_females),
#                              nrow(paired2004_females)/nrow(single2004_females),
#                              nrow(paired2008_females)/nrow(single2008_females)),
#                       panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=2)))
# 
# ggplot(dfmarryfr, aes(x=panel, y=freq)) + geom_bar(stat="identity", position="dodge") + 
#   ggtitle("Female: married/single")


# male only
dfmarrym = data.frame(status=rep(c("married","single"),4), 
                      freq=c(nrow(paired1996_males), nrow(single1996_males), 
                             nrow(paired2001_males), nrow(single2001_males),
                             nrow(paired2004_males), nrow(single2004_males),
                             nrow(paired2008_males), nrow(single2008_males)),
                      panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=2)))

ggplot(dfmarrym, aes(x=status, y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Male: married vs. single")


####################### raw female edu ##############################

df_female_edu = data.frame(edu=c(paired1996_females$educlevel_t, single1996_females$educlevel_t,
                                 paired2001_females$educlevel_t, single2001_females$educlevel_t,
                                 paired2004_females$educlevel_t, single2004_females$educlevel_t,
                                 paired2008_females$educlevel_t, single2008_females$educlevel_t),
                           panel=c(rep("1996",nrow(paired1996_females)+nrow(single1996_females)),
                                   rep("2001",nrow(paired2001_females)+nrow(single2001_females)),
                                   rep("2004",nrow(paired2004_females)+nrow(single2004_females)),
                                   rep("2008",nrow(paired2008_females)+nrow(single2008_females))))
ggplot(df_female_edu, aes(x=edu, fill=panel, color=panel)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Female: education")

####################### raw male edu ##############################

df_male_edu = data.frame(edu=c(paired1996_males$educlevel_t, single1996_males$educlevel_t,
                                 paired2001_males$educlevel_t, single2001_males$educlevel_t,
                                 paired2004_males$educlevel_t, single2004_males$educlevel_t,
                                 paired2008_males$educlevel_t, single2008_males$educlevel_t),
                           panel=c(rep("1996",nrow(paired1996_males)+nrow(single1996_males)),
                                   rep("2001",nrow(paired2001_males)+nrow(single2001_males)),
                                   rep("2004",nrow(paired2004_males)+nrow(single2004_males)),
                                   rep("2008",nrow(paired2008_males)+nrow(single2008_males))))
ggplot(df_male_edu, aes(x=edu, fill=panel, color=panel)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Male: education")

####################### female homophily for education #######################

homophily1996f = edu_homophily(paired1996_females, paired1996_males)
homophily2001f = edu_homophily(paired2001_females, paired2001_males)
homophily2004f = edu_homophily(paired2004_females, paired2004_males)
homophily2008f = edu_homophily(paired2008_females, paired2008_males)

df_homophily_edu_f = data.frame(edu=rep(c("<HS","HS","SomeCollege","BA+"),4),
                                freq=c(homophily1996f$edu1, homophily1996f$edu2, homophily1996f$edu3, homophily1996f$edu4,
                                       homophily2001f$edu1, homophily2001f$edu2, homophily2001f$edu3, homophily2001f$edu4,
                                       homophily2004f$edu1, homophily2004f$edu2, homophily2004f$edu3, homophily2004f$edu4,
                                       homophily2008f$edu1, homophily2008f$edu2, homophily2008f$edu3, homophily2008f$edu4),
                                panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=4)))
ggplot(df_homophily_edu_f, aes(x=factor(edu,level=c("<HS","HS","SomeCollege","BA+")), y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Female: education homophily")+xlab("education")


####################### male homophily for education #######################

homophily1996m = edu_homophily(paired1996_males, paired1996_females)
homophily2001m = edu_homophily(paired2001_males, paired2001_females)
homophily2004m = edu_homophily(paired2004_males, paired2004_females)
homophily2008m = edu_homophily(paired2008_males, paired2008_females)

df_homophily_edu_m = data.frame(edu=rep(c("<HS","HS","SomeCollege","BA+"),4),
                                freq=c(homophily1996m$edu1, homophily1996m$edu2, homophily1996m$edu3, homophily1996m$edu4,
                                       homophily2001m$edu1, homophily2001m$edu2, homophily2001m$edu3, homophily2001m$edu4,
                                       homophily2004m$edu1, homophily2004m$edu2, homophily2004m$edu3, homophily2004m$edu4,
                                       homophily2008m$edu1, homophily2008m$edu2, homophily2008m$edu3, homophily2008m$edu4),
                                panel=as.factor(rep(c(1996, 2001, 2004, 2008),each=4)))
ggplot(df_homophily_edu_m, aes(x=factor(edu,level=c("<HS","HS","SomeCollege","BA+")), y=freq, fill=panel, color=panel)) + geom_bar(stat="identity", position="dodge") + 
  ggtitle("Male: education homophily")+xlab("education")


##################### female single vs. married by edu ########################

df_married_single_edu_f = data.frame(edu=c(paired1996_females$educlevel_t, single1996_females$educlevel_t,
                                           paired2001_females$educlevel_t, single2001_females$educlevel_t,
                                           paired2004_females$educlevel_t, single2004_females$educlevel_t,
                                           paired2008_females$educlevel_t, single2008_females$educlevel_t),
                                     panel=c(rep("1996",nrow(paired1996_females)+nrow(single1996_females)),
                                             rep("2001",nrow(paired2001_females)+nrow(single2001_females)),
                                             rep("2004",nrow(paired2004_females)+nrow(single2004_females)),
                                             rep("2008",nrow(paired2008_females)+nrow(single2008_females))),
                                    status=c(rep(c("married","single"), c(nrow(paired1996_females),nrow(single1996_females))),
                                             rep(c("married","single"), c(nrow(paired2001_females),nrow(single2001_females))),
                                             rep(c("married","single"), c(nrow(paired2004_females),nrow(single2004_females))),
                                             rep(c("married","single"), c(nrow(paired2008_females),nrow(single2008_females)))))

ggplot(df_married_single_edu_f, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Female education: married vs. single")

# not much difference
df_married_single_edu_f_1996 = subset(df_married_single_edu_f, panel==1996)
ggplot(df_married_single_edu_f_1996, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Female education 1996: married vs. single")

# not much difference
df_married_single_edu_f_2008 = subset(df_married_single_edu_f, panel==2008)
ggplot(df_married_single_edu_f_2008, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Female education 2008: married vs. single")

##################### male single vs. married by education ####################

df_married_single_edu_m = data.frame(edu=c(paired1996_males$educlevel_t, single1996_males$educlevel_t,
                                           paired2001_males$educlevel_t, single2001_males$educlevel_t,
                                           paired2004_males$educlevel_t, single2004_males$educlevel_t,
                                           paired2008_males$educlevel_t, single2008_males$educlevel_t),
                                     panel=c(rep("1996",nrow(paired1996_males)+nrow(single1996_males)),
                                             rep("2001",nrow(paired2001_males)+nrow(single2001_males)),
                                             rep("2004",nrow(paired2004_males)+nrow(single2004_males)),
                                             rep("2008",nrow(paired2008_males)+nrow(single2008_males))),
                                     status=c(rep(c("married","single"), c(nrow(paired1996_males),nrow(single1996_males))),
                                              rep(c("married","single"), c(nrow(paired2001_males),nrow(single2001_males))),
                                              rep(c("married","single"), c(nrow(paired2004_males),nrow(single2004_males))),
                                              rep(c("married","single"), c(nrow(paired2008_males),nrow(single2008_males)))))

ggplot(df_married_single_edu_m, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Male education: married vs. single")


# not much difference
df_married_single_edu_m_1996 = subset(df_married_single_edu_m, panel==1996)
ggplot(df_married_single_edu_m_1996, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Male education 1996: married vs. single")

# not much difference
df_married_single_edu_m_2008 = subset(df_married_single_edu_m, panel==2008)
ggplot(df_married_single_edu_m_2008, aes(x=edu, fill=status, color=status)) + geom_histogram(binwidth=.5, alpha=.5, position="dodge") + 
  scale_x_discrete(name ="Education", limits=c("<HS","HS","SomeCollege","BA+")) +
  ggtitle("Male education 2008: married vs. single")
