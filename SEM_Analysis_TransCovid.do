use "H:\Working_Directory\MTI_data_May01_31.dta", clear

drop if new_case <= 0

sum trip_per oc_trip mile_per transit_p consum_p stay_home soc_dist nwtrip_per
sum d_COVID ctrace_work test_done import_case naffect_p4 case_neighbor new_case_avg
sum days_EmDec days_PubMask days_Quartn
sum above60_p black_p hisp_p male_p popden emplden unempl_rate

sort state county
gen extra=_n
saveold "H:\Working_Directory\May_sel_county.dta", replace

gen log_worker = log(worker)
gen log_lowwage = log(lowwage)
gen log_medwage = log(medwage)
gen log_hiwage = log(hiwage)
gen log_ret5_den = log(ret5_den)
gen log_ret8_den = log(ret8_den)
gen log_act_den = log(act_den)
gen log_network_den = log(network_den)

gen log_l_trip_per = log(l_trip_per)
gen log_l_nwtrip_per = log(l_nwtrip_per)
gen log_l_oc_trip = log(l_oc_trip)

gen frac_newcase = new_case*100/totpop
gen log_naffect_p4 = log(naffect_p4)
gen nonwfh_p = 100-wfh_p
gen non_l_wfh_p = 100-l_wfh_p
gen nonsoc_dist = 100-soc_dist
gen non_l_soc_dist = 100-l_soc_dist
gen log_ctrace_work = log(ctrace_work)
gen log_d_ILI = log(d_ILI)

// The SEM model 
sem           (trip_per@1   stay_home     oc_trip       mile_per      transit_p     consum_p      l_soc_dist    l_trip_per    l_oc_trip     l_mile_per    l_nwtrip_per  l_consum_p <- Mobility) ///
(Mobility <-  log_new_case_avg            above60_p     black_p       hisp_p        male_p        log_popden    log_emplden   unempl_rate   days_EmDec    ) ///
(log_new_case_avg <-        Mobility      log_d_COVID   log_ctrace_work             log_test_done casen_impl    above60_p     black_p       hisp_p        male_p        log_popden    days_PubMask  days_Quartn   ) ///
(log_d_COVID <-             above60_p     black_p       hisp_p        male_p        log_popden    unempl_rate   ) ///
(log_ctrace_work <-         above60_p     black_p       hisp_p        male_p        log_popden    unempl_rate   ) ///
(log_test_done <-           black_p       hisp_p        log_popden    unempl_rate   ) ///
(casen_impl <-              black_p       hisp_p        log_popden    unempl_rate   ) ///
, method(mlmv) ///
cov(e.trip_per*e.oc_trip) ///
cov(e.trip_per*e.mile_per) ///
cov(e.trip_per*e.consum_p) ///
cov(e.trip_per*e.l_soc_dist) ///
cov(e.trip_per*e.l_trip_per) ///
cov(e.trip_per*e.l_oc_trip) ///
cov(e.trip_per*e.l_mile_per) ///
cov(e.trip_per*e.l_nwtrip_per) ///
cov(e.trip_per*e.l_consum_p) ///
cov(e.stay_home*e.l_oc_trip) ///
cov(e.oc_trip*e.mile_per) ///
cov(e.oc_trip*e.l_soc_dist) ///
cov(e.oc_trip*e.l_trip_per) ///
cov(e.oc_trip*e.l_oc_trip) ///
cov(e.oc_trip*e.l_mile_per) ///
cov(e.oc_trip*e.l_nwtrip_per) ///
cov(e.oc_trip*e.l_consum_p) ///
cov(e.mile_per*e.stay_home) ///
cov(e.mile_per*e.consum_p) ///
cov(e.mile_per*e.l_soc_dist) ///
cov(e.mile_per*e.l_trip_per) ///
cov(e.mile_per*e.l_oc_trip) ///
cov(e.mile_per*e.l_mile_per) ///
cov(e.mile_per*e.l_nwtrip_per) ///
cov(e.mile_per*e.l_consum_p) ///
cov(e.transit_p*e.stay_home) ///
cov(e.transit_p*e.l_trip_per) ///
cov(e.transit_p*e.l_oc_trip) ///
cov(e.transit_p*e.l_mile_per) ///
cov(e.transit_p*e.l_nwtrip_per) ///
cov(e.transit_p*e.l_consum_p) ///
cov(e.consum_p*e.l_soc_dist) ///
cov(e.consum_p*e.l_trip_per) ///
cov(e.consum_p*e.l_oc_trip) ///
cov(e.consum_p*e.l_mile_per) ///
cov(e.consum_p*e.l_nwtrip_per) ///
cov(e.consum_p*e.l_consum_p) ///
cov(e.l_soc_dist*e.stay_home) ///
cov(e.l_soc_dist*e.l_trip_per) ///
cov(e.l_soc_dist*e.l_oc_trip) ///
cov(e.l_soc_dist*e.l_mile_per) ///
cov(e.l_soc_dist*e.l_nwtrip_per) ///
cov(e.l_soc_dist*e.l_consum_p) ///
cov(e.l_trip_per*e.stay_home) ///
cov(e.l_trip_per*e.l_oc_trip) ///
cov(e.l_trip_per*e.l_mile_per) ///
cov(e.l_trip_per*e.l_nwtrip_per) ///
cov(e.l_trip_per*e.l_consum_p) ///
cov(e.l_mile_per*e.l_oc_trip) ///
cov(e.l_mile_per*e.l_nwtrip_per) ///
cov(e.l_mile_per*e.l_consum_p) ///
cov(e.l_nwtrip_per*e.stay_home) ///
cov(e.l_nwtrip_per*e.l_oc_trip) ///
cov(e.l_nwtrip_per*e.l_consum_p) ///
cov(e.l_consum_p*e.stay_home) ///
cov(e.l_consum_p*e.l_oc_trip) ///
cov(e.log_ctrace_work*e.log_test_done) ///
cov(e.log_ctrace_work*e.casen_impl) ///
cov(e.log_test_done*e.casen_impl) 

estat eqgof
estat gof, stats(all)
estat teffect

// Prediction for May
predict yhat_linf, xb(log_new_case_avg)
gen yhat_inf = exp(yhat_linf)
predict fac_mob, latent(Mobility)
sort log_new_case_avg

ttest yhat_linf == log_new_case_avg
gen diff = yhat_linf - log_new_case_avg
gen diff2 = (yhat_linf - log_new_case_avg)^2
sum diff2, meanonly
scalar ms = r(mean)
scalar rmse = (ms)^0.5
di rmse

sort yhat_linf
//sort countyID
gen slno = _n
line log_new_case_avg yhat_linf slno, legend(size(medsmall))

saveold "H:\Working_Directory\Prediction_May_base.dta", replace
export delimited using "H:\Working_Directory\Prediction_May_base.csv", nolabel replace



// Prediction for June
use "H:\Working_Directory\MTI_data_June01_30_Mayneighbor.dta", clear
sort state county

drop if new_case <= 0

gen log_worker = log(worker)
gen log_lowwage = log(lowwage)
gen log_medwage = log(medwage)
gen log_hiwage = log(hiwage)
gen log_ret5_den = log(ret5_den)
gen log_ret8_den = log(ret8_den)
gen log_act_den = log(act_den)
gen log_network_den = log(network_den)

gen log_l_trip_per = log(l_trip_per)
gen log_l_nwtrip_per = log(l_nwtrip_per)
gen log_l_oc_trip = log(l_oc_trip)

gen frac_newcase = new_case*100/totpop
gen log_naffect_p4 = log(naffect_p4)
gen nonwfh_p = 100-wfh_p
gen non_l_wfh_p = 100-l_wfh_p
gen nonsoc_dist = 100-soc_dist
gen non_l_soc_dist = 100-l_soc_dist
gen log_ctrace_work = log(ctrace_work)
gen log_d_ILI = log(d_ILI)


predict yhat_linf, xb(log_new_case_avg)
gen yhat_inf = exp(yhat_linf)
predict fac_mob, latent(Mobility)
sort log_new_case_avg

ttest yhat_linf == log_new_case_avg
gen diff = yhat_linf - log_new_case_avg
gen diff2 = (yhat_linf - log_new_case_avg)^2
sum diff2, meanonly
scalar ms = r(mean)
scalar rmse = (ms)^0.5
di rmse

sort yhat_linf

gen slno = _n
line log_new_case_avg yhat_linf slno, legend(size(medsmall))

saveold "H:\Working_Directory\Prediction_June_base.dta", replace
export delimited using "H:\Working_Directory\Prediction_June_base.csv", nolabel replace


// Scenario 1: Hypothetical Travel Restriction
use "H:\Working_Directory\MTI_data_June01_30_Mayneighbor.dta", clear
sort state county

drop if new_case <= 0
/* Travel Restriction Scenario */
replace trip_per = trip_per*0.80
replace oc_trip = oc_trip*0.80
replace mile_per = mile_per*0.80
replace consum_p = consum_p*0.80
replace stay_home = stay_home*1.2


gen log_worker = log(worker)
gen log_lowwage = log(lowwage)
gen log_medwage = log(medwage)
gen log_hiwage = log(hiwage)
gen log_ret5_den = log(ret5_den)
gen log_ret8_den = log(ret8_den)
gen log_act_den = log(act_den)
gen log_network_den = log(network_den)

gen log_l_trip_per = log(l_trip_per)
gen log_l_nwtrip_per = log(l_nwtrip_per)
gen log_l_oc_trip = log(l_oc_trip)

gen frac_newcase = new_case*100/totpop
gen log_naffect_p4 = log(naffect_p4)
gen nonwfh_p = 100-wfh_p
gen non_l_wfh_p = 100-l_wfh_p
gen nonsoc_dist = 100-soc_dist
gen non_l_soc_dist = 100-l_soc_dist
gen log_ctrace_work = log(ctrace_work)
gen log_d_ILI = log(d_ILI)


predict yhat_linf, xb(log_new_case_avg)
gen yhat_inf = exp(yhat_linf)
predict fac_mob, latent(Mobility)
sort log_new_case_avg

ttest  yhat_linf == log_new_case_avg
gen diff = yhat_linf - log_new_case_avg
gen diff2 = (yhat_linf - log_new_case_avg)^2
sum diff2, meanonly
scalar ms = r(mean)
scalar rmse = (ms)^0.5
di rmse

sort countyID
line yhat_linf log_new_case_avg countyID if countyID<100, legend(size(medsmall))

saveold "H:\Working_Directory\Prediction_June_trvlrestrict.dta", replace

drop countyID county state fips_str totpop soc_dist stay_home trip_per oc_trip os_trip mile_per wtrip_per nwtrip_per transit_p d_COVID d_ILI test_gap ctrace_work hbed_util icu_util new_case new_case_avg act_case import_case exposure test_done hbed_avail icu_avail vent_need unempl_claim unempl_rate wfh_p infl_rate consum_p above60_p med_inc black_p hisp_p male_p popden emplden hotspot death_rate l_soc_dist l_stay_home l_trip_per l_oc_trip l_os_trip l_mile_per l_wtrip_per l_nwtrip_per l_transit_p l_exposure l_wfh_p l_consum_p workers lowwage medwage hiwage ret5_den ret8_den act_den network_den log_soc_dist log_stay_home log_trip_per log_oc_trip log_os_trip log_mile_per log_wtrip_per log_nwtrip_per log_transit_p log_d_COVID log_new_case log_new_case_avg log_import_case log_exposure log_test_done log_med_inc log_popden log_emplden log_hotspot new_case5 naffect_p4 sh_naffect_p4 case_neighbor casen_imp casen_impl casen_oct days_EmDec days_BarRes days_CaseIsol days_GathRec days_GathRes days_NEBusCl days_OBusCl days_PubMask days_Quartn days_RestRes days_SchCl days_StayHome days_BarRest_Res log_worker log_lowwage log_medwage log_hiwage log_ret5_den log_ret8_den log_act_den log_network_den log_l_trip_per log_l_nwtrip_per log_l_oc_trip frac_newcase log_naffect_p4 nonwfh_p non_l_wfh_p nonsoc_dist non_l_soc_dist log_ctrace_work log_d_ILI fac_mob diff diff2
rename yhat_linf lpredtr
rename yhat_inf predtr
merge 1:1 fips_num using "H:\Working_Directory\Prediction_June_base.dta", keepusing(new_case_avg yhat_inf yhat_linf)
drop _merge
saveold "H:\Working_Directory\Prediction_June_trvlrestrict.dta", replace
export delimited using "H:\Working_Directory\Prediction_June_trvlrestrict1.csv", nolabel replace


// Scenario 2: Forecasting Mobility Changes
use "H:\Working_Directory\MTI_data_June01_30.dta", clear
sort state county
drop if new_case <= 0

gen log_worker = log(worker)
gen log_lowwage = log(lowwage)
gen log_medwage = log(medwage)
gen log_hiwage = log(hiwage)
gen log_ret5_den = log(ret5_den)
gen log_ret8_den = log(ret8_den)
gen log_act_den = log(act_den)
gen log_network_den = log(network_den)

gen log_l_trip_per = log(l_trip_per)
gen log_l_nwtrip_per = log(l_nwtrip_per)
gen log_l_oc_trip = log(l_oc_trip)

gen frac_newcase = new_case*100/totpop
gen log_naffect_p4 = log(naffect_p4)
gen nonwfh_p = 100-wfh_p
gen non_l_wfh_p = 100-l_wfh_p
gen nonsoc_dist = 100-soc_dist
gen non_l_soc_dist = 100-l_soc_dist
gen log_ctrace_work = log(ctrace_work)
gen log_d_ILI = log(d_ILI)

predict yhattrp, xb(trip_per)
predict yhatoctrp, xb(oc_trip)
predict yhatmile, xb(mile_per)
predict yhatcon, xb(consum_p)
predict yhatsh, xb(stay_home)

// Forecast by Treating Mobility as dependent (expected reduction in cases)

replace new_case_avg = 0.9*new_case_avg
replace log_new_case_avg = log(new_case_avg)

predict predtrp, xb(trip_per)
predict predoctrp, xb(oc_trip)
predict predmile, xb(mile_per)
predict predcon, xb(consum_p)
predict predsh, xb(stay_home)

sum trip_per oc_trip mile_per consum_p stay_home
sum predtrp predoctrp predmile predcon predsh

gen diff1 = (predtrp-trip_per)/trip_per
gen diff2 = (predoctrp-oc_trip)/oc_trip
gen diff3 = (predmile-mile_per)/mile_per
gen diff4 = (predcon-consum_p)/consum_p
gen diff5 = (predsh-stay_home)/stay_home

keep if diff1<-0.01 & diff2<-0.01 & diff3<-0.01 & diff4<-0.01 & diff5>0.01 & diff1>-0.20 & diff2>-0.20 & diff3>-0.20 & diff4>-0.20 & diff5<0.20
gen ch_trip_per = predtrp-trip_per if county=="lamar county" & state=="georgia"
gen ch_octrip = predoctrp-oc_trip if county=="lamar county" & state=="georgia"
gen ch_mile_per = predmile-mile_per if county=="lamar county" & state=="georgia"
gen ch_consum = predcon-consum_p if county=="lamar county" & state=="georgia"
gen ch_stay_home = predsh-stay_home if county=="lamar county" & state=="georgia"

keep if fips_num==13171
sum trip_per oc_trip mile_per consum_p stay_home
sum predtrp predoctrp predmile predcon predsh


saveold "H:\Working_Directory\Prediction_June_mobest.dta", replace
export delimited using "H:\Working_Directory\Prediction_June_mobest.csv", nolabel replace
