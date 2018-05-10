# source("C://UCLA//thesis_ideas//homeless_poverty//PAA_annual_meeting_2018//poster//unpartnered_partnered//timing_analysis//unpartnered_partnered.R")
library(knitr)
library(ggplot2)

###################### files and libraries for the data ######################

#setwd("C://UCLA//thesis_ideas//homeless_poverty//PAA_annual_meeting_2018//poster//unpartnered_partnered//timing_analysis")
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

################### files required by algorithm ####################################

source("..\\fitrpm_R_CP.R")
source("..\\loglikelihood_CP.R")
source("..\\equality_constraint_CP.R")
source("..\\rpm.model.matrix.R")
source("..\\choice_probability.R")
source("..\\check_CP.R")
source("..\\create_counterfactual_distri.R")

################## set forumula ##########################

# specify the formula for utilities
# example: ff = ~ b1cov("f1") + b2cov("f1") + b1absdiff("f1",1) + b2absdiff("f1",1)

# takes a few seconds
ff1 = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t")

# takes > 1 min
ff2 = ~ b1homophily("educlevel_t") + b2homophily("educlevel_t") + b1homophily("race_t") + b2homophily("race_t")
ff3 = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t") + b1homophily("race_t") + b2homophily("race_t") # takes a a couple minutes

ff4 = ~ b1nodematch("educlevel_t") + b2nodematch("educlevel_t") +
  b1absdiff("educlevel_t",1) + b2absdiff("educlevel_t",1) +
  b1greaterthan("educlevel_t") + b2greaterthan("educlevel_t")

# interactions
ff5 = ~ b1homophily("educlevel_t","race_t") + b2homophily("educlevel_t","race_t")

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

################## store the timing result #####################

max_size = 100
df = data.frame("network size" = numeric(max_size), "# coefficients estimated" = numeric(max_size),
                "# characteristics groups" = numeric(max_size),
                "elaspsed time" = numeric(max_size), "# paired" = numeric(max_size), 
                "# unpaired" = numeric(max_size), "user time" = numeric(max_size),
                "system time" = numeric(max_size))
all_out = list()

################# data sets for timing analysis ##################

Xdata_1 = Xdata1996
Zdata_1 = Zdata1996
Xdata_2 = rbind(Xdata1996, Xdata2001)
Zdata_2 = rbind(Zdata1996, Zdata2001)
Xdata_3 = rbind(Xdata_2, Xdata2004)
Zdata_3 = rbind(Zdata_2, Zdata2004)
Xdata_4 = rbind(Xdata_3, Xdata2008)
Zdata_4 = rbind(Zdata_3, Zdata2008)
Xdata_5 = rbind(Xdata_4, Xdata_4)
Zdata_5 = rbind(Zdata_4, Zdata_4)
Xdata_6 = rbind(Xdata_5, Xdata_5)
Zdata_6 = rbind(Zdata_5, Zdata_5)
Xdata_7 = rbind(Xdata_6, Xdata_6)
Zdata_7 = rbind(Zdata_6, Zdata_6)

mu1 = mu1996
mu2=bdiag(list(mu1, mu2001))
mu3=bdiag(list(mu2, mu2004))
mu4=bdiag(list(mu3, mu2008))
mu5=bdiag(list(mu4, mu4))
mu6=bdiag(list(mu5, mu5))
mu7=bdiag(list(mu6, mu6))

################# run algorithm ################################

ii = 1
Xdata_names = c("Xdata_1", "Xdata_2", "Xdata_3", "Xdata_4", "Xdata_5", "Xdata_6", "Xdata_7")
Zdata_names = c("Zdata_1", "Zdata_2", "Zdata_3", "Zdata_4", "Zdata_5", "Zdata_6", "Zdata_7")
mu_names = c("mu1", "mu2", "mu3", "mu4", "mu5", "mu6", "mu7")
ff_names = c("ff1", "ff2", "ff3", "ff4", "ff5")


for(jj in 1:length(ff_names)) {
  
  print(paste0("jj = ", jj))
  
  ff = eval(parse(text=ff_names[jj]))
  
  for (kk in 1:length(Xdata_names)) {
    
    print(paste0("kk = ", kk))
    
    Xdata=eval(parse(text=Xdata_names[kk]))
    Zdata=eval(parse(text=Zdata_names[kk]))
    mu=eval(parse(text=mu_names[kk]))
    
    start_1 = proc.time()
    out_1 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, theta_0, control=control)
    tmp = proc.time() - start_1
    df[ii,] = c(nrow(Xdata)+nrow(Zdata), length(out_1$coef), length(out_1$solution)-length(out_1$coef), 
                tmp[3], sum(mu), nrow(Xdata)+nrow(Zdata)-2*sum(mu), tmp[1], tmp[2])
    all_out[[ii]] = out_1$solution
    
    ii=ii+1
  }
}

# I forgot Xdata_7, Zdata_7
for(jj in 1:length(ff_names)) {
  
  print(paste0("jj = ", jj))
  
  ff = eval(parse(text=ff_names[jj]))
  
  kk=7
    
    print(paste0("kk = ", kk))
    
    Xdata=eval(parse(text=Xdata_names[kk]))
    Zdata=eval(parse(text=Zdata_names[kk]))
    mu=eval(parse(text=mu_names[kk]))
    
    start_1 = proc.time()
    out_1 <- fitrpm_R_CP(ff, mu, Xdata, Zdata, theta_0, control=control)
    tmp = proc.time() - start_1
    df[ii,] = c(nrow(Xdata)+nrow(Zdata), length(out_1$coef), length(out_1$solution)-length(out_1$coef), 
                tmp[3], sum(mu), nrow(Xdata)+nrow(Zdata)-2*sum(mu), tmp[1], tmp[2])
    all_out[[ii]] = out_1$solution
    
    ii=ii+1
}

########### tabulate the timing results ###########################

df=df[1:(ii-1),]
df$X..paired = df$X..paired * 2

save.image("timing_analysis.RData")

unique_network_size = unique(df$network.size)
unique_coeff_estimated = unique(df$X..coefficients.estimated)
unique_char_groups = unique(df$X..characteristics.groups)
for (jj in unique_char_groups) {
  print(kable(subset(df, X..characteristics.groups==jj, select=1:6)))
}

# by # groups
kable(subset(df, X..characteristics.groups==8 & X..coefficients.estimated==14, select=1:6), 
      col.names=c("network size", "# coeff", "# groups", "run time", "# paired indiv.", "# unpaired indiv."), 
      row.names = F, booktabs=T)
kable(subset(df, X..characteristics.groups==32 & X..coefficients.estimated==12, select=1:6), 
      col.names=c("network size", "# coeff", "# groups", "run time", "# paired indiv.", "# unpaired indiv."), 
      row.names = F, booktabs=T)

# by network size
df_tmp = subset(df, network.size==14527, select=1:4)
df_tmp = df_tmp[order(df_tmp$X..characteristics.groups, df_tmp$X..coefficients.estimated),]
kable(df_tmp, col.names=c("network size", "# coeff", "# groups", "run time"), 
      row.names = F, booktabs=T)

df_tmp = subset(df, network.size==527704, select=1:4)
df_tmp = df_tmp[order(df_tmp$X..characteristics.groups, df_tmp$X..coefficients.estimated),]
kable(df_tmp, col.names=c("network size", "# coeff", "# groups", "run time"), 
      row.names = F, booktabs=T)

# example pmfj plot (using the largest network size)
out_2 <- fitrpm_R_CP(ff4, mu7, Xdata_7, Zdata_7, theta_0, control=control)

