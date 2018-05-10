edu_homophily = function(paired_females, paired_males) {
  
  # number of females who married males with higher education
  f1_idx = which(paired_females$educlevel_t == 1)
  n_1_males_married_1_females = sum(paired_males$educlevel_t[f1_idx] == 1)
  
  f2_idx = which(paired_females$educlevel_t == 2)
  n_2_males_married_2_females = sum(paired_males$educlevel_t[f2_idx] == 2)
  
  f3_idx = which(paired_females$educlevel_t == 3)
  n_3_males_married_3_females = sum(paired_males$educlevel_t[f3_idx] == 3)
  
  f4_idx = which(paired_females$educlevel_t == 4)
  n_4_males_married_4_females = sum(paired_males$educlevel_t[f3_idx] == 4)
  
  return(list(edu1=n_1_males_married_1_females, edu2=n_2_males_married_2_females,
              edu3=n_3_males_married_3_females, edu4=n_4_males_married_4_females))
}


