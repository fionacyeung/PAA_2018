edu_hypo_hyper_same = function(paired_females, paired_males) {
  
  # number of females who married males with higher education
  f1_idx = which(paired_females$educlevel_t == 1)
  n_2andhigher_males_married_1_females = sum(paired_males$educlevel_t[f1_idx] > 1)
  
  f2_idx = which(paired_females$educlevel_t == 2)
  n_3andhigher_males_married_2_females = sum(paired_males$educlevel_t[f2_idx] > 2)
  
  f3_idx = which(paired_females$educlevel_t == 3)
  n_4_males_married_3_females = sum(paired_males$educlevel_t[f3_idx] > 3)
  
  sum_females_married_higher_males = n_2andhigher_males_married_1_females+n_3andhigher_males_married_2_females+n_4_males_married_3_females
  
  
  # number of females who married males with same education
  f1_idx = which(paired_females$educlevel_t == 1)
  n_1_males_married_1_females = sum(paired_males$educlevel_t[f1_idx] == 1)
  
  f2_idx = which(paired_females$educlevel_t == 2)
  n_2_males_married_2_females = sum(paired_males$educlevel_t[f2_idx] == 2)
  
  f3_idx = which(paired_females$educlevel_t == 3)
  n_3_males_married_3_females = sum(paired_males$educlevel_t[f3_idx] == 3)
  
  f4_idx = which(paired_females$educlevel_t == 4)
  n_4_males_married_4_females = sum(paired_males$educlevel_t[f4_idx] == 4)
  
  sum_females_married_same_edu_males = n_1_males_married_1_females+n_2_males_married_2_females+n_3_males_married_3_females+n_4_males_married_4_females
  
  
  # number of females who married males with lower education
  f2_idx = which(paired_females$educlevel_t == 2)
  n_1_males_married_2_females = sum(paired_males$educlevel_t[f2_idx] < 2)
  
  f3_idx = which(paired_females$educlevel_t == 3)
  n_2andlower_males_married_3_females = sum(paired_males$educlevel_t[f3_idx] < 3)
  
  f4_idx = which(paired_females$educlevel_t == 4)
  n_3andlower_males_married_4_females = sum(paired_males$educlevel_t[f4_idx] < 4)
  
  sum_females_married_lower_males = n_1_males_married_2_females+n_2andlower_males_married_3_females+n_3andlower_males_married_4_females
  
  return(list(hypo=sum_females_married_lower_males, same=sum_females_married_same_edu_males, hyper=sum_females_married_higher_males))
}


