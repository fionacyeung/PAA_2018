From: Weden, Margaret <mweden@rand.org>
Date: Fri, Nov 10, 2017 at 7:50 PM
Subject: Unpartnered & New Partner Data
To: "Fiona Yeung (fiona.c.yeung@gmail.com)" <fiona.c.yeung@gmail.com>
Cc: "Michael S. Rendall (mrendall@umd.edu)" <mrendall@umd.edu>

Hi Fiona,

Hope you are doing well. I am happy to report that the next installment of data is all set to go.

I have produced a person-year file of individuals  that includes respondents who were unpartnered at a given wave, and respondents who were in new partnerships at a given wave. The file is in the ‘long’ format in that there are multiple records for a respondent if s/he is observed as unpartnered in multiple waves (and there are a couple who have multiple partnerships). I am providing a variable list as a word document that identifies the time-varying variables. As before, I am including a record for each respondent in a partnership.

In addition it is noteworthy that we are currently only considering respondents who entered new partnerships from an unpartnered state (i.e. not married and not cohabiting). Thus, for this rendition of the data, there are no new partnerships from individuals transitioning from cohabitation to marriage; no new partnerships for individuals transitioning from one cohabitation to another cohabitation without passing through an unpartnered state; and no new partnerships for individuals transitioning from separated to married or separated to cohabiting without passing through an unpartnered state.

I should also mention that respondents who reported that their partnership status changed from unpartnered to “married with spouse absent” are also not included in the new partnerships because all partner characteristics are not observed for these cases.

Let me know if you see anything strange.

It is a complicated procedure to wrangle this data and the grad student working on it has done a great job. But there may be some inconsistencies that I didn’t find. This version of the data reflects extensive cleaning to identify cases that inappropriately appeared as ‘new partnerships’ in the previous data set.  Also, I have saved the data as a Stata file. I think that R easily reads in Stata files, but let me know if this is a problem.

The data is at:

https://drive.google.com/open?id=1u_k25IdOLH71dEj5pyzdl6SE3R_skSSB

(In compressed file called “Unpartnered&NewPartnered_Final.zip” of the SIPP>SIPP New Relationships Selection Analysis_wMI)

There should be 277,526 unpartnered individuals and 14,210 newly partnered (of which 8,204 are new marriages).

. ta marrcohabt_rev newrelunpar


marrcohabt_ |      newrelunpar
        rev |         0          1 |     Total
------------+----------------------+----------
unpartnered |   277,526          0 |   277,526
    new mar |         0      8,204 |     8,204
  new cohab |         0      6,006 |     6,006
------------+----------------------+----------
      Total |   277,526     14,210 |   291,736

 

The variable statistics are :


    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         pid |          0
  spanel_all |    291,736    2002.089    4.631926       1996       2008
     epppnum |    291,736      138.94    139.3585        101       1207
       ssuid |          0
    wavetime |    291,736    2.473407    1.202034          1          5
-------------+---------------------------------------------------------
  wpfinwgt_t |    291,736    3468.361    1622.757          0   49089.65
      tage_t |    291,736    33.34688    12.48464         18         59
    female_t |    291,736    .5188835    .4996441          0          1
      race_t |    284,020    2.620108    .7372853          1          4
 educlevel_t |    287,372     2.58821    .9617887          1          4
-------------+---------------------------------------------------------

s_wpfinwgt_t |      8,204    2725.132     1486.45    21.2684   18931.27
    s_tage_t |      8,204     31.7979    9.779645         18         59
  s_female_t |      8,204          .5    .5000305          0          1
    s_race_t |      8,056    2.703823    .7232352          1          4
s_educleve~t |      8,015    2.809482    .9646923          1          4

-------------+---------------------------------------------------------
p_wpfinwgt_t |      6,007    2930.392    1869.696    42.8857    22144.9
    p_tage_t |      6,007    30.96055    9.507675         18         59
  p_female_t |      6,007    .5000832    .5000416          0          1
    p_race_t |      5,815    2.728289    .6581887          1          4
p_educleve~t |      5,917    2.638499    .9362497          1          4
-------------+---------------------------------------------------------

allrespart~w |     14,210    315.9529    293.6376        101       1204
marrcohabt~v |    291,736    .0692955    .3250665          0          2
 newrelunpar |    291,736    .0487084    .2152581          0          1

Best,

MMW
