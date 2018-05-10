# PAA_2018

This is a snapshot of the code in the fitrpm_CP repository at the time when I was preparing the poster for PAA 2018 using the data set called "Unpartnered&NewPartnered_Final.dta". 

Important file: unpartnered_partnered_poster.R
This is the top level code that reads in the data, pre-processes it, divides it into different panels according to the year. It then calls the core algorithm entry function, "fitrpm_R_CP" (starting at line 412 in the initial version). It also outputs some graphs after this call. 

Data and processing:
The single individuals are reported repeatedly until they get married or co-habit with their partners. In the pre-processing step, I only used the last panel of each year for estimation. Otherwise, the participants will be repeated (i.e. will be over-counted in estimation). Also, marriage and cohabitation are treated as the same in this initial version. 

This initial version does estimation for different panels (1996, 2001, 2004, and 2008) separately. Estimation was never performed across panels in this initial version.

Timing analysis:
Some code that was used in generating the results for timing analysis (shown at my ATC meeting) using this data set is included in the folder "timing_analysis".

Known problems in this initial version:
1) Some coefficients have negative values. Do they make sense? Need to understand why. Some experiments regarding this problem were documented towards the end of data_analysis.docx.
2) The "standard error" printout after the "fitrpm_R_CP" call is actually just the covariance of the estimates in this initial version. However, the covariance computed doesn't look correct. Need to fix this.