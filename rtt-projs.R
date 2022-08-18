rm(list=ls())

require(tidyverse)
require(lubridate)
require(scales)
require(zoo)

setwd("S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/projects/corona/rtt/ode model (2022)/england application/")

##########################################################################
# INPUTS

# calibration
months_to_calibrate_from<-7
switch_reneging_thold<-0.025
switch_reneging_rate_now<-0.025

# scenarios
referrals_annual_growth<-c(0,1,5)
capacity_annual_growth<-c(0,5,10)
forecast_horizon<-365*5

# plotting
date_plot_from<-"2019-04-01"
fix_mean_treatment_wait<-TRUE
show_period_from<-"2019-04-01"   #must be >= date_plot_from
show_period_to<-"2020-02-29"
res_png<-400

#output subfolder
out_subf<-"results2"

##########################################################################
# FUNCTIONS

Wt_fn<-function(t,W0,l0,l1,c0,c1,p) {
  expand_grid(t,W0,l0,l1,c0,c1,p) %>%
    rowwise() %>%
    mutate(W=1/p*((l0+l1*t)-(c0+c1*t)-l1/p+c1/p)+(W0-1/p*(l0-c0-(l1/p-c1/p)))*exp(-p*t)) %>%
    ungroup()
}

res_fn<-function(t,W0,l0,l1,c0,c1,p) {
  expand_grid(t,W0,l0,l1,c0,c1,p) %>%
    mutate(t1=as.numeric(t)-as.numeric(min(t))+1) %>%
    rowwise() %>%
    mutate(W=1/p*((l0+l1*t1)-(c0+c1*t1)-l1/p+c1/p)+(W0-1/p*(l0-c0-(l1/p-c1/p)))*exp(-p*t1)) %>%
    ungroup() %>%
    mutate(W=replace(W,which(W<0),NA)) %>%
    mutate(reneges=W*p) %>%
    mutate(arrivals=l0+l1*t1) %>%
    mutate(treatments=c0+c1*t1) %>%
    mutate(prop_clock_stops_reneges=reneges/(reneges+treatments)) %>%
    mutate(mean_treatment_wait=W/treatments) %>%
    mutate(mean_pathway_duration=W/(reneges+treatments)) %>%
    group_by(W0,l0,l1,c0,c1,p) %>%
    select(-t1)
}

plot_res2<-function(dat) {
  tprovider<-unique(dat %>% .$provider)
  tspecialty<-unique(dat %>% .$specialty)
  dat %>%
    mutate(proj=case_when(metric=="prop_clock_stops_reneges"~proj*100,
                          TRUE~proj)) %>%
    mutate(actual=case_when(metric=="prop_clock_stops_reneges"~actual*100,TRUE~actual)) %>%
    mutate(metric=factor(metric,levels=c("arrivals","treatments","W","mean_treatment_wait"))) %>%
    mutate(metric=recode(metric,"arrivals"="Referrals (per day)","mean_treatment_wait"="Mean wait for treatment (months)",
                         "prop_clock_stops_reneges"="ROTT (%)",
                         "treatments"="Treatments (per day)","W"="Waiting list size")) %>%
    mutate(referrals_annual_growth = forcats::fct_relabel(referrals_annual_growth, .fun = ~glue::glue("{.x}%")),
           capacity_annual_growth = forcats::fct_relabel(capacity_annual_growth, .fun = ~glue::glue("{.x}%"))) %>%
    rename("Annual referral growth"="referrals_annual_growth","Annual capacity growth"="capacity_annual_growth") %>%
    mutate(hist_avg=mean(actual[date>=show_period_from & date <=show_period_to])) %>%
    #addition here
    filter(metric %in% c("Referrals (per day)","Treatments (per day)","Waiting list size","Mean wait for treatment (months)")) %>%
    ggplot(aes(x=date)) +
    geom_rect(aes(xmin=date_calibrate_from,xmax=max(rtt_dat_trust$date),ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.5) +
    geom_vline(xintercept=forecast_start_date,linetype="dashed") +
    geom_line(aes(y=hist_avg),colour="grey",linetype="dashed") +
    geom_line(aes(y=proj,colour=`Annual referral growth`,linetype=`Annual capacity growth`)) +
    geom_line(aes(y=actual)) +
    scale_color_manual(values=c("green2", "orange", "red2")) +
    #scale_colour_viridis_d(direction=-1,begin=0.2,end=0.8,option="rocket") +
    scale_y_continuous(labels=comma) +
    labs(title=paste0(tprovider," - ",tspecialty),
         subtitle=paste0("Projections based on actual data for referrals and treatments over the last ",months_to_calibrate_from," months to end ",as.yearmon(max(rtt_dat_trust$date))),
         caption=paste0("For reference, the dashed horizontal line represents the mean from ",as.yearmon(show_period_from)," to ",as.yearmon(show_period_to))) +
    facet_wrap(~metric,scales="free_y") +
    theme_bw() +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(face="bold"),
          plot.subtitle=element_text(face="italic"),
          plot.caption=element_text(face="italic"))
}

plot2_res2<-function(dat,metric1) {
  tprovider<-unique(dat %>% .$provider)
  dat %>%
    mutate(proj=case_when(metric=="prop_clock_stops_reneges"~proj*100,
                          TRUE~proj)) %>%
    mutate(actual=case_when(metric=="prop_clock_stops_reneges"~actual*100,TRUE~actual)) %>%
    mutate(metric=recode(metric,"arrivals"="Referrals (per day)","mean_treatment_wait"="Mean wait for treatment (months)",
                         "prop_clock_stops_reneges"="ROTT (%)",
                         "treatments"="Treatments (per day)","W"="Waiting list size")) %>%
    filter(metric==metric1) %>%
    mutate(referrals_annual_growth = forcats::fct_relabel(referrals_annual_growth, .fun = ~glue::glue("{.x}%")),
           capacity_annual_growth = forcats::fct_relabel(capacity_annual_growth, .fun = ~glue::glue("{.x}%"))) %>%
    rename("Annual referral growth"="referrals_annual_growth","Annual capacity growth"="capacity_annual_growth") %>%
    mutate(hist_avg=mean(actual[date>=show_period_from & date <=show_period_to])) %>%
    ggplot(aes(x=date)) +
    geom_rect(aes(xmin=date_calibrate_from,xmax=max(rtt_dat_trust$date),ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.5) +
    geom_vline(xintercept=forecast_start_date,linetype="dashed") +
    geom_line(aes(y=hist_avg),colour="grey",linetype="dashed") +
    geom_line(aes(y=proj,colour=`Annual referral growth`,linetype=`Annual capacity growth`)) +
    geom_line(aes(y=actual)) +
    scale_color_manual(values=c("green2", "orange", "red2")) +
    #scale_colour_viridis_d(direction=-1,begin=0.2,end=0.8,option="rocket") +
    scale_y_continuous(labels=comma) +
    labs(title=paste0(tprovider," - ",metric1),
         subtitle=paste0("Projections based on actual data for referrals and treatments over the last ",months_to_calibrate_from," months to end ",as.yearmon(max(rtt_dat_trust$date))," (shaded grey area)"),
         caption=paste0("For reference, the dashed horizontal line represents the mean from ",as.yearmon(show_period_from)," to ",as.yearmon(show_period_to))) +
    facet_wrap(~specialty,scales="free_y") +
    theme_bw() +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(face="bold"),
          plot.subtitle=element_text(face="italic"),
          plot.caption=element_text(face="italic"))
}

plot3_res2<-function(dat) {
  tprovider<-unique(dat %>% .$provider)
  dat %>%
    mutate(proj=case_when(metric=="prop_clock_stops_reneges"~proj*100,
                          TRUE~proj)) %>%
    mutate(actual=case_when(metric=="prop_clock_stops_reneges"~actual*100,TRUE~actual)) %>%
    mutate(metric=factor(metric,levels=c("arrivals","treatments","W","mean_treatment_wait"))) %>%
    mutate(metric=recode(metric,"arrivals"="Referrals (per day)","mean_treatment_wait"="Mean wait for treatment (months)",
                         "prop_clock_stops_reneges"="ROTT (%)",
                         "treatments"="Treatments (per day)","W"="Waiting list size")) %>%
    mutate(referrals_annual_growth = forcats::fct_relabel(referrals_annual_growth, .fun = ~glue::glue("{.x}%")),
           capacity_annual_growth = forcats::fct_relabel(capacity_annual_growth, .fun = ~glue::glue("{.x}%"))) %>%
    rename("Annual referral growth"="referrals_annual_growth","Annual capacity growth"="capacity_annual_growth") %>%
    #addition here
    filter(metric %in% c("Referrals (per day)","Treatments (per day)")) %>%
    pivot_longer(cols=c(`Annual referral growth`,`Annual capacity growth`),names_to="type",values_to="value") %>%
    group_by(date,provider,specialty,uom,metric) %>%
    summarise(projmin=min(proj),projmax=max(proj),actual) %>%
    distinct() %>%
    rename('Metric'='metric') %>%
    ggplot(aes(x=date)) +
    geom_rect(aes(xmin=date_calibrate_from,xmax=max(rtt_dat_trust$date),ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.5) +
    geom_vline(xintercept=forecast_start_date,linetype="dashed") +
    geom_ribbon(aes(ymin=projmin,ymax=projmax,fill=Metric),alpha=0.5) +
    geom_line(aes(y=actual,colour=Metric)) +
    scale_y_continuous(labels=comma) +
    labs(title=paste0(tprovider," - Model inputs, min/max range"),
         subtitle=paste0("Annual growth %s applied to actual data for referrals and treatments over the last ",months_to_calibrate_from," months to end ",as.yearmon(max(rtt_dat_trust$date))," (shaded grey area)")) +
    facet_wrap(~specialty,scales="free_y") +
    theme_bw() +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(face="bold"),
          plot.subtitle=element_text(face="italic"),
          plot.caption=element_text(face="italic"))
}



##########################################################################
# APPLICATION

# input and clean data
rtt_dat_trust<-read.csv("calc-dat.csv") %>%
  rename("date"="period","provider"="trust") %>%
  filter(date>date_plot_from) %>%
  group_by(specialty,provider,date,metric) %>%
  summarise(value=sum(value)) %>%
  pivot_wider(names_from="metric",values_from="value") %>%
  select(provider,specialty,date,"arrivals"="n_clckstrt","W"="n_wl","treatments"="n_clckstp","reneges"="n_rn","mean_treatment_wait"="mean_wait") %>%
  mutate(prop_clock_stops_reneges=reneges/(reneges+treatments)) %>%
  pivot_longer(cols=-c(provider,specialty,date),names_to="metric",values_to="value") %>%
  mutate(value=case_when(metric=="W"~value,
                         metric=="prop_clock_stops_reneges"~value,
                         metric=="mean_treatment_wait"~value*7,
                         TRUE~value/lubridate::days_in_month(date))) %>%
  mutate(date=as.Date(date)) %>%
  mutate(uom=paste0(provider,"-",specialty)) %>%
  ungroup()

#addition here - remove any uom's which don't have data in the most recent period from months_to_calibrate_from
rtt_dat_trust<-left_join(rtt_dat_trust,
                         rtt_dat_trust %>%
                            group_by(provider,metric,specialty) %>%
                            filter(date>=max(rtt_dat_trust$date) %m-% months(months_to_calibrate_from-1)) %>%
                            summarise(n=n()),
                      by=c("provider","metric","specialty")) %>%
  ungroup() %>%
  filter(n==months_to_calibrate_from) %>%
  select(-n)


forecast_start_date<-ceiling_date(max(rtt_dat_trust$date),unit="month")
date_calibrate_from<-max(rtt_dat_trust$date) %m-% months(months_to_calibrate_from-1)
Tx<-seq(forecast_start_date,forecast_start_date+forecast_horizon,1)

##########################################################################
# LOOP FOR RESULTS
rtt_dat_trust1<-rtt_dat_trust

# LOOP OVER EACH TRUST
for (prov in unique(rtt_dat_trust1$provider)) {
  rtt_dat_trust<-rtt_dat_trust1 %>% filter(provider==prov)
  # generate raw outputs
  res<-do.call("rbind",lapply(unique(rtt_dat_trust$uom), function(u) {
    tryCatch({
      tprovider<-unique(rtt_dat_trust %>% filter(uom==u) %>% .$provider)
      tspecialty<-unique(rtt_dat_trust %>% filter(uom==u) %>% .$specialty)
      x<-rtt_dat_trust %>%
        filter(uom==u) %>%
        select(-c(provider,specialty,uom)) %>%
        filter(date>=date_calibrate_from)
      if (any(x %>% filter(metric=="W") %>% .$value <100)) return(NULL)
      W0<-x %>%
        filter(date==max(date) & metric=="W") %>%
        .$value
      l0<-x %>%
        filter(metric=="arrivals") %>%
        .$value %>%
        mean
      c0<-x %>%
        filter(metric=="treatments") %>%
        .$value %>%
        mean
      tmp_p<-x %>%
        filter(metric %in% c("treatments","reneges")) %>%
        group_by(metric) %>%
        summarise(value=sum(value)) %>%
        pivot_wider(names_from=metric,values_from=value) %>%
        mutate(value=reneges/(treatments+reneges)) %>%
        .$value
      if (c0>l0 & tmp_p==0) return(NULL)
      if (tmp_p<switch_reneging_thold) tmp_p<-switch_reneging_rate_now
      p<-tmp_p*c0/(W0*(1-tmp_p))
      l1<-l0/365*(referrals_annual_growth/100)
      c1<-c0/365*(capacity_annual_growth/100)
      out<-res_fn(Tx,W0,l0,l1,c0,c1,p) %>%
        mutate(provider=tprovider,specialty=tspecialty,uom=u) %>%
        ungroup()
      return(out)},error=function(e) NULL)
  }))
  if (is.null(res)) next
  
  # include extra detail in outputs
  res2<-res %>%
    mutate(referrals_annual_growth=round(l1/l0*365*100,5)) %>%
    mutate(capacity_annual_growth=round(c1/c0*365*100,5)) %>%
    pivot_longer(cols=-c(t,referrals_annual_growth,capacity_annual_growth,uom,provider,specialty),names_to="metric",values_to="value") %>%
    rename("date"="t") %>%
    mutate(type="proj") %>%
    bind_rows(rtt_dat_trust %>% 
                mutate(referrals_annual_growth=referrals_annual_growth[1],capacity_annual_growth=capacity_annual_growth[1]) %>%
                filter(uom %in% unique(res$uom)) %>%
                mutate(type="actual")) %>%
    filter(metric %in% c("W","arrivals","treatments","prop_clock_stops_reneges","mean_treatment_wait")) %>%
    pivot_wider(names_from="type",values_from="value") %>%
    mutate(referrals_annual_growth=factor(referrals_annual_growth),capacity_annual_growth=factor(capacity_annual_growth)) %>%
    mutate(actual=case_when(metric=="mean_treatment_wait"~actual/30,TRUE~actual)) %>%
    mutate(proj=case_when(metric=="mean_treatment_wait"~proj/30,TRUE~proj))

  if (fix_mean_treatment_wait==TRUE) {
    res2<-res2 %>%
      group_by(uom,metric) %>%
      mutate(act_t0=ifelse(metric=="mean_treatment_wait",actual[which(date==max(rtt_dat_trust$date))],NA)) %>%
      mutate(proj_t0=ifelse(metric=="mean_treatment_wait",proj[which(date==forecast_start_date)],NA)) %>%
      mutate(scaler=proj_t0/act_t0) %>%
      mutate(proj=ifelse(metric=="mean_treatment_wait",proj/scaler,proj)) %>%
      select(-c(act_t0,proj_t0,scaler))
  }
  
  # 1. for trust level, just for referrals/treatments
  png(paste0(getwd(),"/",out_subf,"/",tolower(prov)," - 1 model inputs (data to end ",tolower(as.yearmon(max(rtt_dat_trust$date))),").png"),height=7,width=10,units="in",res=res_png)
  torder<-res2 %>% 
    filter(provider==prov & metric=="mean_treatment_wait" & date==max(rtt_dat_trust$date)) %>%
    group_by(specialty) %>%
    summarise(order=sum(actual,na.rm=TRUE)) %>%
    arrange(desc(order)) %>%
    filter(!specialty=="Other") %>%
    bind_rows(data.frame(specialty="Other",order=0))
  print(
    plot3_res2(
      res2 %>% 
        filter(specialty %in% torder$specialty))
  )
  dev.off()
  
  # 2. for trust level, just for waiting list size
  png(paste0(getwd(),"/",out_subf,"/",tolower(prov)," - 2 waiting list (data to end ",tolower(as.yearmon(max(rtt_dat_trust$date))),").png"),height=7,width=10,units="in",res=res_png)
  torder<-res2 %>% 
    filter(provider==prov & metric=="W" & date==max(rtt_dat_trust$date)) %>%
    group_by(specialty) %>%
    summarise(order=sum(actual,na.rm=TRUE)) %>%
    arrange(desc(order)) %>%
    filter(!specialty=="Other") %>%
    bind_rows(data.frame(specialty="Other",order=0))
  print(
    plot2_res2(
      res2 %>% 
        filter(specialty %in% torder$specialty)
      ,"Waiting list size")
  )
  dev.off()
  
  # 3. for trust level, just for waiting times
  png(paste0(getwd(),"/",out_subf,"/",tolower(prov)," - 3 waiting times (data to end ",tolower(as.yearmon(max(rtt_dat_trust$date))),").png"),height=7,width=10,units="in",res=res_png)
  torder<-res2 %>% 
    filter(provider==prov & metric=="mean_treatment_wait" & date==max(rtt_dat_trust$date)) %>%
    group_by(specialty) %>%
    summarise(order=sum(actual,na.rm=TRUE)) %>%
    arrange(desc(order)) %>%
    filter(!specialty=="Other") %>%
    bind_rows(data.frame(specialty="Other",order=0))
  print(
    plot2_res2(
      res2 %>% 
        filter(specialty %in% torder$specialty)
      ,"Mean wait for treatment (months)")
  )
  dev.off()
  
  
  # 4. for trust-total levels
  if ("Total" %in% res2$specialty) {
    png(paste0(getwd(),"/",out_subf,"/",tolower(prov)," - 4 total (data to end ",tolower(as.yearmon(max(rtt_dat_trust$date))),").png"),height=7,width=8,units="in",res=res_png)
    print(
      res2 %>%
        filter(specialty=="Total") %>%
        plot_res2(.)
    )
    dev.off()
  }
  
  
}









