rm(list=ls())
setwd('C:\\Users\\jms41081\\Documents\\Econometrics\\caetano\\gathergood_replication\\CSV_files')
CPI.data<-read.csv('CPI_ONS.CSV')
library(haven)
library(hmi)
library(sem)
library(stringr)
library(AER)
###
#upload this data
count.to.seventeen<-seq(from=1,to=17,by=1)
letter.list<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q')
#for each number
for (val in count.to.seventeen){
  #upload data: indresp
  file.name<-paste('b',letter.list[val],'_indresp.CSV',sep='')
  df<-read.csv(file.name)
  df_name<-paste('indresp_',val,sep='')
  assign(df_name,df)
  #hhresp
  file.name<-paste('b',letter.list[val],'_hhresp.CSV',sep='')
  df<-read.csv(file.name)
  df_name<-paste('hhresp_',val,sep='')
  assign(df_name,df)
  #and jobhist
  file.name<-paste('b',letter.list[val],'_jobhist.CSV',sep='')
  df<-read.csv(file.name)
  df_name<-paste('jobhist_',val,sep='')
  assign(df_name,df)
}
#we also need cross-wave data
cross.wave.data<-read.csv('cross_wave_data.CSV')

#we require the following variables: anxiety, labour market status (employed, self-employed, 
#inactive, incapacitated, unemployed), county, industry, age, gender, GHQ12 score, 

#ethnicity, marital status, education, homeownership status, smoker, occupational pension, 
#recent labor market history, household income
#procedure: first create 17 datasets (one per wave) which have only the data we want

indresp.list<-list(indresp_1,indresp_2,indresp_3,indresp_4,indresp_5,indresp_6,indresp_7,indresp_8,indresp_9,indresp_10,
                   indresp_11,indresp_12,indresp_13,indresp_14,indresp_15,indresp_16,indresp_17)

hhresp.list<-list(hhresp_1,hhresp_2,hhresp_3,hhresp_4,hhresp_5,hhresp_6,hhresp_7,hhresp_8,hhresp_9,hhresp_10,
                  hhresp_11,hhresp_12,hhresp_13,hhresp_14,hhresp_15,hhresp_16,hhresp_17)

jobhist.list<-list(jobhist_1,jobhist_2,jobhist_3,jobhist_4,jobhist_5,jobhist_6,jobhist_7,jobhist_8,jobhist_9,jobhist_10,
                   jobhist_11,jobhist_12,jobhist_13,jobhist_14,jobhist_15,jobhist_16,jobhist_17)

#specifically, for each number 1 to 17
for (val in count.to.seventeen){
  #select the right indresp
  indresp_n<-as.data.frame(indresp.list[val])
  hhresp_n<-as.data.frame(hhresp.list[val])
  jobhist_n<-as.data.frame(jobhist.list[val])
  
#create some variables
  var.1<-'pid'
  var.2<-paste('b',letter.list[val],'_hlprbi',sep='')
  var.3<-paste('b',letter.list[val],'_jbstat',sep='')
  var.4<-paste('b',letter.list[val],'_gor_dv',sep='')
  var.5<-paste('b',letter.list[val],'_age_dv',sep='')
  var.6<-paste('b',letter.list[val],'_sex',sep='')
  var.7<-paste('b',letter.list[val],'_scghqa',sep='')
  var.8<-paste('b',letter.list[val],'_scghqb',sep='')
  var.9<-paste('b',letter.list[val],'_scghqc',sep='')
  var.10<-paste('b',letter.list[val],'_scghqd',sep='')
  var.11<-paste('b',letter.list[val],'_scghqe',sep='')
  var.12<-paste('b',letter.list[val],'_scghqf',sep='')
  var.13<-paste('b',letter.list[val],'_scghqg',sep='')
  var.14<-paste('b',letter.list[val],'_scghqh',sep='')
  var.15<-paste('b',letter.list[val],'_scghqi',sep='')
  var.16<-paste('b',letter.list[val],'_scghqj',sep='')
  var.17<-paste('b',letter.list[val],'_scghqk',sep='')
  var.18<-paste('b',letter.list[val],'_scghql',sep='')
  var.19<-paste('b',letter.list[val],'_mrjsic',sep='')
  #marital status
  var.20<-paste('b',letter.list[val],'_mastat',sep='')
  #education
  var.21<-paste('b',letter.list[val],'_qfedhi',sep='')
  #smoker
  var.22<-paste('b',letter.list[val],'_smoker',sep='')
  if(val==9){
  var.22<-paste('b',letter.list[val],'_ncigs',sep='')
  }
  #household number
  HID<-paste('b',letter.list[val],'_hid',sep='')
  #year employment spell began
  jbhist.yr<-paste('b',letter.list[val],'_jhendy4',sep='')
  #self-employed var
  jbhist.sem<-paste('b',letter.list[val],'_jhsemp',sep='')
  #current job var
  current.job.var<-paste('b',letter.list[val],'_jbbgy',sep='')
  #job status variable
  jobhist.stat.var<-paste('b',letter.list[val],'_jhstat',sep='')
  #spouse ID
  spouse.ID<-paste('b',letter.list[val],'_sppid_bh',sep='')
    
  #hh variable definitions:
  #homeownership
  home.owner<-paste('b',letter.list[val],'_hsownd_bh',sep='')
  #we need real household income
  household.income<-paste('b',letter.list[val],'_hhneti',sep='')
  #occupational pension contribution
  occ.cont<-paste('b',letter.list[val],'_contr',sep='')
  
  #get the variables that come from the HH dataset
  hh.vars<-as.data.frame(cbind(hhresp_n[match(indresp_n[,HID],hhresp_n[,1]),household.income]/CPI.data[2+val,2],
                               hhresp_n[match(indresp_n[,HID],hhresp_n[,1]),home.owner],
                               hhresp_n[match(indresp_n[,HID],hhresp_n[,1]),occ.cont]))
  
  #and the race variable from the cross-wave dataset
  race.var<-as.data.frame(cross.wave.data[match(as.numeric(indresp_n[,var.1]),as.numeric(cross.wave.data[,'pid'])),'race_bh'])
  colnames(race.var)<-'race'
  
  #We also need dummies for unemployment, self-employment, incapacitated and inactive in the last 3 years
  employ.hist.dummies<-as.data.frame(matrix(nrow=nrow(indresp_n),ncol=4))
  
  #start by assuming they had some spells outside of the labor force
  employ.hist.dummies[,1]<-1
  
  #for all people where their current job began more than 3 years ago, all dummies are zero
  col.no<-match(current.job.var,colnames(indresp_n))
  colnames(indresp_n)[col.no]<-'current_job'
  df<-subset(indresp_n,current_job<=1987+val)
  df<-subset(df,current_job!=(-8))
  df<-subset(df,current_job!=(-7))
  ins<-match(df[,'pid'],indresp_n[,'pid'])
  employ.hist.dummies[ins,]<-0
  
  #for unemployment, sickness and self-emloyed in the last three years 
  #(first rename variables so we can use the subset function)
  col.no<-match(jbhist.yr,colnames(jobhist_n))
  colnames(jobhist_n)[col.no]<-'year'
  col.no<-match(jobhist.stat.var,colnames(jobhist_n))
  colnames(jobhist_n)[col.no]<-'job_hist_stat'
  col.no<-match(jbhist.sem,colnames(jobhist_n))
  colnames(jobhist_n)[col.no]<-'self_employed'
  df.hist<-subset(jobhist_n,year>=1987+val)
  
  #now get unemployment spells
  df.unemp<-subset(df.hist,job_hist_stat==3)
  #find out who we're talking about and fill this in
  unemps<-match(df.unemp[,'pid'],indresp_n[,'pid'])
  employ.hist.dummies[unemps,2]<-1
  employ.hist.dummies[unemps,1]<-0
  
  #same for sick
  df.sick<-subset(df.hist,job_hist_stat==7)
  #find out who we're talking about and fill this in
  sicks<-match(df.sick[,'pid'],indresp_n[,'pid'])
  employ.hist.dummies[sicks,3]<-1
  employ.hist.dummies[sicks,1]<-0
  
  #and self-employed
  df.se<-subset(df.hist,self_employed==2)
  #find out who we're talking about and fill this in
  ses<-match(df.se[,'pid'],indresp_n[,'pid'])
  employ.hist.dummies[ses,4]<-1
  employ.hist.dummies[ses,1]<-0
  
  #need a variable for spousal job mkt status
  spouse.jm.status<-as.data.frame(matrix(nrow=nrow(indresp_n),ncol=1),data=0)
  colnames(spouse.jm.status)<-'spouse_job_status'
  col.no<-match(spouse.ID,colnames(indresp_n))
  colnames(indresp_n)[col.no]<-'spouse_ID'
  df.spouse.stat<-subset(indresp_n,spouse_ID>0)
  spouse.jm.status[na.omit(match(df.spouse.stat[,'spouse_ID'],indresp_n[,1])),1]<-indresp_n[na.omit(match(df.spouse.stat[,'spouse_ID'],indresp_n[,1])),var.3]
  
  
  #stitch them into a data frame
df<-as.data.frame(cbind(indresp_n[,var.1],indresp_n[,var.2],indresp_n[,var.3],
                                 indresp_n[,var.4],indresp_n[,var.5],indresp_n[,var.6],
                                 indresp_n[,var.7],indresp_n[,var.8],indresp_n[,var.9],
                                 indresp_n[,var.10],indresp_n[,var.11],indresp_n[,var.12],
                                 indresp_n[,var.13],indresp_n[,var.14],indresp_n[,var.15],
                                 indresp_n[,var.16],indresp_n[,var.17],indresp_n[,var.18],
                        indresp_n[,var.19],indresp_n[,var.20],indresp_n[,var.21],indresp_n[,var.22],
                        spouse.jm.status,hh.vars,employ.hist.dummies,
                        matrix(ncol=1,nrow=nrow(indresp_n),data=val),race.var))
#set collumn names
colnames(df)<-c('person_ID','anxiety','employment_status','county','age','sex','GHQ12_q1','GHQ12_q2',
                'GHQ12_q3','GHQ12_q4','GHQ12_q5','GHQ12_q6','GHQ12_q7','GHQ12_q8','GHQ12_q9','GHQ12_q10',
                'GHQ12_q11','GHQ12_q12','industry','marital_status','education','smoker','spouse_employment_status',
                'household_income','home_owner','occupational_pension_contr','inactive_last_three_years',
                'unemployed_last_three_years', 'incapacitated_last_three_years', 'selfemployed_last_three_years',
                 'wave_number','race')

#name it
df.name<-paste('data.wave.',val,sep='')
assign(df.name,df)

}


#for everything which is a labelled double, re-set as a factor: anxiety, employment_status, county, age, sex, GHQ12_q1','GHQ12_q2',
#'GHQ12_q3','GHQ12_q4','GHQ12_q5','GHQ12_q6','GHQ12_q7','GHQ12_q8','GHQ12_q9','GHQ12_q10',
#'GHQ12_q11','GHQ12_q12, industry, marital status, education, smoker
var.conversion.list<-c('anxiety', 'employment_status', 'county', 'sex', 'GHQ12_q1','GHQ12_q2',
'GHQ12_q3','GHQ12_q4','GHQ12_q5','GHQ12_q6','GHQ12_q7','GHQ12_q8','GHQ12_q9','GHQ12_q10',
'GHQ12_q11','GHQ12_q12', 'industry', 'marital_status', 'education', 'smoker')
for (var in var.conversion.list){
  data.wave.1[,var]<-as.factor(data.wave.1[,var])
  data.wave.2[,var]<-as.factor(data.wave.2[,var])
  data.wave.3[,var]<-as.factor(data.wave.3[,var])
  data.wave.4[,var]<-as.factor(data.wave.4[,var])
  data.wave.5[,var]<-as.factor(data.wave.5[,var])
  data.wave.6[,var]<-as.factor(data.wave.6[,var])
  data.wave.7[,var]<-as.factor(data.wave.7[,var])
  data.wave.8[,var]<-as.factor(data.wave.8[,var])
  data.wave.9[,var]<-as.factor(data.wave.9[,var])
  data.wave.10[,var]<-as.factor(data.wave.10[,var])
  data.wave.11[,var]<-as.factor(data.wave.11[,var])
  data.wave.12[,var]<-as.factor(data.wave.12[,var])
  data.wave.13[,var]<-as.factor(data.wave.13[,var])
  data.wave.14[,var]<-as.factor(data.wave.14[,var])
  data.wave.15[,var]<-as.factor(data.wave.15[,var])
  data.wave.16[,var]<-as.factor(data.wave.16[,var])
  data.wave.17[,var]<-as.factor(data.wave.17[,var])
}

#for wave 9, need to recode smoker variable: this is because for wave 9 we have number of cigarettes, not a binary variable
smoker.count<-seq(from=1,to=nrow(data.wave.9),by=1)
for (val in smoker.count){
  if(data.wave.9[val,'smoker']==(-8)){
    data.wave.9[val,'smoker']<-2
  }
  if(as.numeric(as.character(data.wave.9[val,'smoker']))>0){
    data.wave.9[val,'smoker']<-1
  }
}


#the full dataset is just sticking together all the data waves
full.data.set<-rbind(data.wave.1,data.wave.2,data.wave.3,data.wave.4,data.wave.5,data.wave.6,data.wave.7,data.wave.8,data.wave.9,
                     data.wave.10,data.wave.11,data.wave.12,data.wave.13,data.wave.14,data.wave.15,data.wave.16,
                     data.wave.17)

write_dta(full.data.set,'gathergood_data.dta')

#subset to get rid of null responses
full.data.set<-subset(full.data.set,anxiety!=(-9))
full.data.set<-subset(full.data.set,anxiety!=(-8))
full.data.set<-subset(full.data.set,anxiety!=(-7))

full.data.set<-subset(full.data.set,employment_status!=(-9))
full.data.set<-subset(full.data.set,employment_status!=(-2))
full.data.set<-subset(full.data.set,employment_status!=(-1))

full.data.set<-subset(full.data.set,county!=(-9))

#for age we need to convert back to numeric
full.data.set<-subset(full.data.set,age>=18)
full.data.set<-subset(full.data.set,age<65)

#now do the GHQ responses
full.data.set<-subset(full.data.set,GHQ12_q1!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q1!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q1!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q1!=(-1))
full.data.set<-subset(full.data.set,GHQ12_q1!=(-2))

full.data.set<-subset(full.data.set,GHQ12_q2!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q2!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q2!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q2!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q2!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q3!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q3!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q3!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q3!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q3!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q4!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q4!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q4!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q4!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q4!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q5!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q5!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q5!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q5!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q5!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q6!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q6!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q6!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q6!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q6!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q7!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q7!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q7!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q7!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q7!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q8!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q8!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q8!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q8!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q8!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q9!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q9!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q9!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q9!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q9!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q10!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q10!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q10!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q10!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q10!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q11!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q11!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q11!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q11!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q11!=(-1))

full.data.set<-subset(full.data.set,GHQ12_q12!=(-9))
full.data.set<-subset(full.data.set,GHQ12_q12!=(-7))
full.data.set<-subset(full.data.set,GHQ12_q12!=(-8))
full.data.set<-subset(full.data.set,GHQ12_q12!=(-2))
full.data.set<-subset(full.data.set,GHQ12_q12!=(-1))

#industry
full.data.set<-subset(full.data.set,industry!=(-9))
full.data.set<-subset(full.data.set,industry!=(-7))
full.data.set<-subset(full.data.set,industry!=(-8))
full.data.set<-subset(full.data.set,industry!=(-3))
 
#at this point, also set industry levels to being 1-10
full.data.set[,'industry']<-as.character(full.data.set[,'industry'])
full.data.set[,'industry']<-as.numeric(str_sub(full.data.set[,'industry'], start = 0, end = -4))
full.data.set[,'industry'][is.na(full.data.set[,'industry'])]=0
full.data.set[,'industry']<-as.factor(full.data.set[,'industry'])

#race
full.data.set<-subset(full.data.set,race!=(-9))

#employment status
full.data.set<-subset(full.data.set,employment_status!=(-9))
full.data.set<-subset(full.data.set,employment_status!=(97))
full.data.set<-subset(full.data.set,employment_status!=(-1))
full.data.set<-subset(full.data.set,employment_status!=(-2))

#sex
full.data.set<-subset(full.data.set,sex!=(-7))

#education
full.data.set<-subset(full.data.set,education!=(-7))

#smoker
full.data.set<-subset(full.data.set,smoker!=(-1))
full.data.set<-subset(full.data.set,smoker!=(-7))
full.data.set<-subset(full.data.set,smoker!=(0))
full.data.set<-subset(full.data.set,smoker!=(-2))
full.data.set<-subset(full.data.set,smoker!=(-9))


#also need to get rid of the datapoints where we have NAs for all occupational history dummies
full.data.set<-subset(full.data.set, is.na(inactive_last_three_years)==FALSE|is.na(unemployed_last_three_years)==FALSE|
is.na(incapacitated_last_three_years)==FALSE|is.na(selfemployed_last_three_years)==FALSE)

#replace nas with zeros
full.data.set[,26:29][is.na(full.data.set[,26:29])]=0

#need to stack pairs of waves on top of each other to get the dataset we need for the regression
wave.paired.data<-as.data.frame(matrix(nrow=0,ncol=2*ncol(full.data.set)))
colnames(wave.paired.data)[1:ncol(full.data.set)]<-colnames(full.data.set)
first<-ncol(full.data.set)+1
last<-2*ncol(full.data.set)
colnames(wave.paired.data)[first:last]<-paste(colnames(full.data.set),'_prime',sep='')

full.data.set[,'person_ID']<-as.numeric(full.data.set[,'person_ID'])
person.IDs<-unique(full.data.set[,'person_ID'])

#calculate how many unique people we have
length(person.IDs)

#okay, now this is cleaned up, let's use it to get the proper data waves
data.wave.1<-subset(full.data.set,wave_number==1)
data.wave.2<-subset(full.data.set,wave_number==2)
data.wave.3<-subset(full.data.set,wave_number==3)
data.wave.4<-subset(full.data.set,wave_number==4)
data.wave.5<-subset(full.data.set,wave_number==5)
data.wave.6<-subset(full.data.set,wave_number==6)
data.wave.7<-subset(full.data.set,wave_number==7)
data.wave.8<-subset(full.data.set,wave_number==8)
data.wave.9<-subset(full.data.set,wave_number==9)
data.wave.10<-subset(full.data.set,wave_number==10)
data.wave.11<-subset(full.data.set,wave_number==11)
data.wave.12<-subset(full.data.set,wave_number==12)
data.wave.13<-subset(full.data.set,wave_number==13)
data.wave.14<-subset(full.data.set,wave_number==14)
data.wave.15<-subset(full.data.set,wave_number==15)
data.wave.16<-subset(full.data.set,wave_number==16)
data.wave.17<-subset(full.data.set,wave_number==17)

#stitching these together to get a dataset paired by waves
#for waves 1 to 16
wave.count<-seq(from=1,to=16,by=1)
#also make a list of waves
wave.list<-list(data.wave.1,data.wave.2,data.wave.3,data.wave.4,data.wave.5,data.wave.6,data.wave.7,data.wave.8,data.wave.9,data.wave.10,
                data.wave.11,data.wave.12,data.wave.13,data.wave.14,data.wave.15,data.wave.16,data.wave.17)
for (val in wave.count){
  wave<-as.data.frame(wave.list[val])
  wave.prime<-as.data.frame(wave.list[val+1])
  wave.prime<-as.data.frame(wave.prime[match(unique(wave[,1]),wave.prime[,1]),])
  new.data<-cbind(wave, wave.prime)
  colnames(new.data)<-colnames(wave.paired.data)
  wave.paired.data<-rbind(wave.paired.data,new.data)
}


ss.wave.paired.data<-subset(wave.paired.data,is.na(employment_status)==FALSE)

individual.by.wave<-as.data.frame(paste(ss.wave.paired.data[,'person_ID'],'_',ss.wave.paired.data[,'wave_number'],sep=''))
colnames(individual.by.wave)<-'person_by_wave'

ss.wave.paired.data<-cbind(individual.by.wave,ss.wave.paired.data) 

#save this
write.csv(ss.wave.paired.data,'wave_paired_data.CSV')

#need to create mental health and mental health prime variables
mental.health.vars<-as.data.frame(matrix(nrow=nrow(wave.paired.data),ncol=3,data=0))
colnames(mental.health.vars)<-c('mental_health','mental_health_prime','delta_mental_health')

#first convert these into 1s and zeros
wave.paired.data[,'GHQ12_q1']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q1']))/4)
wave.paired.data[,'GHQ12_q2']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q2']))/4)
wave.paired.data[,'GHQ12_q3']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q3']))/4)
wave.paired.data[,'GHQ12_q4']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q4']))/4)
wave.paired.data[,'GHQ12_q5']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q5']))/4)
wave.paired.data[,'GHQ12_q6']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q6']))/4)
wave.paired.data[,'GHQ12_q7']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q7']))/4)
wave.paired.data[,'GHQ12_q8']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q8']))/4)
wave.paired.data[,'GHQ12_q9']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q9']))/4)
wave.paired.data[,'GHQ12_q10']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q10']))/4)
wave.paired.data[,'GHQ12_q11']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q11']))/4)
wave.paired.data[,'GHQ12_q12']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q12']))/4)

wave.paired.data[,'GHQ12_q1_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q1_prime']))/4)
wave.paired.data[,'GHQ12_q2_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q2_prime']))/4)
wave.paired.data[,'GHQ12_q3_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q3_prime']))/4)
wave.paired.data[,'GHQ12_q4_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q4_prime']))/4)
wave.paired.data[,'GHQ12_q5_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q5_prime']))/4)
wave.paired.data[,'GHQ12_q6_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q6_prime']))/4)
wave.paired.data[,'GHQ12_q7_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q7_prime']))/4)
wave.paired.data[,'GHQ12_q8_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q8_prime']))/4)
wave.paired.data[,'GHQ12_q9_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q9_prime']))/4)
wave.paired.data[,'GHQ12_q10_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q10_prime']))/4)
wave.paired.data[,'GHQ12_q11_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q11_prime']))/4)
wave.paired.data[,'GHQ12_q12_prime']<-round(as.numeric(as.character(wave.paired.data[,'GHQ12_q12_prime']))/4)

mental.health.vars[,'mental_health']<-(-as.numeric(wave.paired.data[,'GHQ12_q1'])+as.numeric(wave.paired.data[,'GHQ12_q2'])-
  as.numeric(wave.paired.data[,'GHQ12_q3'])-as.numeric(wave.paired.data[,'GHQ12_q4'])+
  as.numeric(wave.paired.data[,'GHQ12_q5'])+as.numeric(wave.paired.data[,'GHQ12_q6'])-
  as.numeric(wave.paired.data[,'GHQ12_q7'])-as.numeric(wave.paired.data[,'GHQ12_q8'])+
  as.numeric(wave.paired.data[,'GHQ12_q9'])+as.numeric(wave.paired.data[,'GHQ12_q10'])+
  as.numeric(wave.paired.data[,'GHQ12_q11'])-as.numeric(wave.paired.data[,'GHQ12_q12']))

mental.health.vars[,'mental_health_prime']<-(-as.numeric(wave.paired.data[,'GHQ12_q1_prime'])+as.numeric(wave.paired.data[,'GHQ12_q2_prime'])-
                                               as.numeric(wave.paired.data[,'GHQ12_q3_prime'])-as.numeric(wave.paired.data[,'GHQ12_q4_prime'])+
                                               as.numeric(wave.paired.data[,'GHQ12_q5_prime'])+as.numeric(wave.paired.data[,'GHQ12_q6_prime'])-
                                               as.numeric(wave.paired.data[,'GHQ12_q7_prime'])-as.numeric(wave.paired.data[,'GHQ12_q8_prime'])+
                                               as.numeric(wave.paired.data[,'GHQ12_q9_prime'])+as.numeric(wave.paired.data[,'GHQ12_q10_prime'])+
                                               as.numeric(wave.paired.data[,'GHQ12_q11_prime'])-as.numeric(wave.paired.data[,'GHQ12_q12_prime']))

mental.health.vars[,'delta_mental_health']<-mental.health.vars[,'mental_health_prime']-mental.health.vars[,'mental_health']

hist(mental.health.vars[,'delta_mental_health'])
ss.wave.paired.data<-cbind(ss.wave.paired.data,mental.health.vars)

#convert the occupational pension variables into dummies
ss.wave.paired.data[,'occupational_pension_contr']<-ifelse(ss.wave.paired.data[,'occupational_pension_contr']>0,1,0)
ss.wave.paired.data[,'occupational_pension_contr_prime']<-ifelse(ss.wave.paired.data[,'occupational_pension_contr_prime']>0,1,0)

#also get change in HH income
delta.hh.income<-as.data.frame(ss.wave.paired.data[,'household_income_prime']-ss.wave.paired.data[,'household_income'])
colnames(delta.hh.income)<-'delta_hh_income'

ss.wave.paired.data<-cbind(ss.wave.paired.data,delta.hh.income)

ss.wave.paired.data<-subset(ss.wave.paired.data,employment_status_prime!=97)
ss.wave.paired.data<-subset(ss.wave.paired.data,marital_status_prime!=-2)
ss.wave.paired.data<-subset(ss.wave.paired.data,marital_status_prime!=-1)
ss.wave.paired.data<-subset(ss.wave.paired.data,marital_status_prime!=-9)

ss.wave.paired.data<-subset(ss.wave.paired.data,is.na(incapacitated_last_three_years_prime)==FALSE)


#need to create the unemployment-by-county rate, and an unemployment-by-county-and-age-and-industry rate. Also need age bins of two kinds

#create a key for sorting people by education dummy
age.dummies.key.1<-matrix(data=0,nrow=10,ncol=3)
age.dummies.key.2<-matrix(data=0,nrow=3,ncol=3)

age.dummies.key.1[,1]<-seq(from=1,to=10,by=1)
age.dummies.key.1[,2]<-seq(from=18,by=5,length.out=10)
age.dummies.key.1[,3]<-seq(from=22,by=5,length.out=10)

age.dummies.key.2[,1]<-seq(from=1,to=3,by=1)
age.dummies.key.2[1,2]<-18
age.dummies.key.2[1,3]<-29

age.dummies.key.2[2,2]<-30
age.dummies.key.2[2,3]<-39

age.dummies.key.2[3,2]<-40
age.dummies.key.2[3,3]<-49

age.dummies<-as.data.frame(matrix(data=0,nrow=65-17,ncol=13))

colnames(age.dummies)<-c('age_dummy_1','age_dummy_2','age_dummy_3','age_dummy_4','age_dummy_5','age_dummy_6','age_dummy_7','age_dummy_8','age_dummy_9',
                         'age_dummy_10','age_under_30','age_30_to_39','age_40_to_49')

#for each age dummy
for (val in age.dummies.key.1[,1]){
  #get all the people we care about
  df<-subset(ss.wave.paired.data,age_prime<=age.dummies.key.1[val,3])
  df<-subset(df,age_prime>=age.dummies.key.1[val,2])
  age.dummies[match(df[,1],ss.wave.paired.data[,1]),val]<-1
}

for (val in age.dummies.key.2[,1]){
  #get all the people we care about
  df<-subset(ss.wave.paired.data,age_prime<=age.dummies.key.2[val,3])
  df<-subset(df,age_prime>=age.dummies.key.2[val,2])
  age.dummies[match(df[,1],ss.wave.paired.data[,1]),val+10]<-1
}


ss.wave.paired.data<-cbind(ss.wave.paired.data,age.dummies)

#set industries to a numeric variable
ss.wave.paired.data[,'industry_prime']<-as.factor(ss.wave.paired.data[,'industry_prime'])

ss.wave.paired.data<-subset(ss.wave.paired.data,age_prime<65)
ss.wave.paired.data<-subset(ss.wave.paired.data,age_prime>=18)

#now for the emplyoment dummies
unemployment.rate.dummies<-as.data.frame(matrix(data=0,nrow=nrow(ss.wave.paired.data),ncol=2))
colnames(unemployment.rate.dummies)<-c('unemployment_rate_county','unemployment_rate_AI')

#create a list of industries and counties
industry.list<-levels(ss.wave.paired.data[,'industry_prime'])
county.list<-levels(ss.wave.paired.data[,'county_prime'])
#for each wave
for (yr in wave.count){
#and each county
for (c in county.list){
  #get just the people in this county in this year
  df<-subset(ss.wave.paired.data,county_prime==c)
  df<-subset(df,wave_number==yr)
  #get the unemployment rate
  unemployment.rate<-sum(df$employment_status_prime==3)/(sum(df$employment_status_prime==1)+sum(df$employment_status_prime==2)+sum(df$employment_status_prime==3))
  #tack this on as our unemployment rate variable 
  unemployment.rate.dummies[match(df[,1],ss.wave.paired.data[,1]),'unemployment_rate_county']<-unemployment.rate
}
  #now do the same by industry and age
  for (i in industry.list){
    for (a in age.dummies.key.1[,1]){
  df<-subset(ss.wave.paired.data,wave_number==yr)
  df<-subset(df,industry_prime==i)
  col.no<-match(paste('age_dummy_',a,sep=''),colnames(df))
  colnames(df)[col.no]<-'relevant_age'
  df<-subset(df,relevant_age==1)
  unemployment.rate<-sum(df$employment_status_prime==3)/(sum(df$employment_status_prime==1)+sum(df$employment_status_prime==2)+sum(df$employment_status_prime==3))
  unemployment.rate.dummies[match(df[,1],ss.wave.paired.data[,1]),'unemployment_rate_AI']<-unemployment.rate
  
    }
  }
}

ss.wave.paired.data<-cbind(ss.wave.paired.data,unemployment.rate.dummies)

ss.wave.paired.data[,'anxiety_prime']<-as.numeric(ss.wave.paired.data[,'anxiety_prime'])-2
ss.wave.paired.data[,'anxiety']<-as.numeric(ss.wave.paired.data[,'anxiety'])-2
ss.wave.paired.data<-subset(ss.wave.paired.data,anxiety_prime>=0)
ss.wave.paired.data<-subset(ss.wave.paired.data,anxiety>=0)
change.in.anxiety<-as.data.frame(ss.wave.paired.data[,'anxiety_prime']-ss.wave.paired.data[,'anxiety'])
colnames(change.in.anxiety)<-'change_in_anxiety'
ss.wave.paired.data<-cbind(ss.wave.paired.data,change.in.anxiety)

ss.wave.paired.data[,'wave_number']<-as.factor(ss.wave.paired.data[,'wave_number'])

ss.wave.paired.data<-subset(ss.wave.paired.data,occupational_pension_contr_prime>=0)

#let's create some dummies for unemployment, selfemployment, incapacitated, and inactive
#for this we need and unemployment dummy
unemployment.prime.dummy<-as.data.frame(ifelse(ss.wave.paired.data[,'employment_status_prime']==3,1,0))
colnames(unemployment.prime.dummy)<-'unemployment_prime_dummy'
ss.wave.paired.data<-cbind(ss.wave.paired.data,unemployment.prime.dummy)

#self-employment
selfemployed.prime.dummy<-as.data.frame(ifelse(ss.wave.paired.data[,'employment_status_prime']==1,1,0))
colnames(selfemployed.prime.dummy)<-'selfemployed_prime_dummy'
ss.wave.paired.data<-cbind(ss.wave.paired.data,selfemployed.prime.dummy)

#incapacitated
sick.prime.dummy<-as.data.frame(ifelse(ss.wave.paired.data[,'employment_status_prime']==8,1,0))
colnames(sick.prime.dummy)<-'sick_prime_dummy'
ss.wave.paired.data<-cbind(ss.wave.paired.data,sick.prime.dummy)

#inactive
inactive.prime.dummy<-
  as.data.frame(ifelse(ss.wave.paired.data[,'employment_status_prime']==4|ss.wave.paired.data[,'employment_status_prime']==5|ss.wave.paired.data[,'employment_status_prime']==6|
                         ss.wave.paired.data[,'employment_status_prime']==97,1,0))
colnames(inactive.prime.dummy)<-'inactive_prime_dummy'
ss.wave.paired.data<-cbind(ss.wave.paired.data,inactive.prime.dummy)

#I need to create some 'wave' dummies too
wave.dummies<-as.data.frame(matrix(nrow=nrow(ss.wave.paired.data),ncol=16,data=0))
#for each wave
for (val in wave.count){
  #get the dummies
  wave.dummies[,val]<-ifelse(ss.wave.paired.data[,'wave_number']==val,1,0)
  #and name this col
  colnames(wave.dummies)[val]<-paste('wave_',val,sep='')
  
}
ss.wave.paired.data<-cbind(ss.wave.paired.data,wave.dummies)

#marital status dummies
marital.status.dummies<-as.data.frame(matrix(data=0,nrow=nrow(ss.wave.paired.data),ncol=3))
colnames(marital.status.dummies)<-c('married','divorced','widowed')
marital.status.dummies[,'married']<-ifelse(ss.wave.paired.data[,'marital_status']==1,1,0)
marital.status.dummies[,'divorced']<-ifelse(ss.wave.paired.data[,'marital_status']==4,1,0)
marital.status.dummies[,'widowed']<-ifelse(ss.wave.paired.data[,'marital_status']==3,1,0)

#education dummies
education.dummies<-as.data.frame(matrix(data=0,nrow=nrow(ss.wave.paired.data),ncol=3))
colnames(education.dummies)<-c('degree','Alevel_or_equiv','olevel_or_equiv')
education.dummies[,'degree']<-ifelse(ss.wave.paired.data[,'education_prime']==1|ss.wave.paired.data[,'education_prime']==2|
                                       ss.wave.paired.data[,'education_prime']==3|ss.wave.paired.data[,'education_prime']==4|
                                       ss.wave.paired.data[,'education_prime']==5,1,0)
education.dummies[,'Alevel_or_equiv']<-ifelse(ss.wave.paired.data[,'education_prime']==6|ss.wave.paired.data[,'education_prime']==10,1,0)
education.dummies[,'olevel_or_equiv']<-ifelse(ss.wave.paired.data[,'education_prime']==7|ss.wave.paired.data[,'education_prime']==8|
                                                ss.wave.paired.data[,'education_prime']==9,1,0)

#race dummy
race.dummy<-as.data.frame(matrix(data=0,nrow=nrow(ss.wave.paired.data),ncol=1))
colnames(race.dummy)<-'race_dummy'
race.dummy[,'race_dummy']<-ifelse(ss.wave.paired.data[,'race']!=1,1,0)

#home ownership variable 1 or 0
ss.wave.paired.data[,'home_owner_prime']<-ifelse(ss.wave.paired.data[,'home_owner_prime']==1|ss.wave.paired.data[,'home_owner_prime']==2|ss.wave.paired.data[,'home_owner_prime']==4,1,0)

ss.wave.paired.data<-cbind(ss.wave.paired.data,marital.status.dummies,education.dummies,race.dummy)

#replace nas with zeros
ss.wave.paired.data[is.na(ss.wave.paired.data)]=0
#now do some more subsetting to get rid of null results which still exist
ss.wave.paired.data<-subset(ss.wave.paired.data,sex_prime!=(-7))
ss.wave.paired.data<-subset(ss.wave.paired.data,home_owner_prime!=(-8))
ss.wave.paired.data<-subset(ss.wave.paired.data,home_owner_prime!=(-7))
ss.wave.paired.data<-subset(ss.wave.paired.data,home_owner_prime!=(-2))
ss.wave.paired.data<-subset(ss.wave.paired.data,home_owner_prime!=(-1))
ss.wave.paired.data<-subset(ss.wave.paired.data,smoker_prime!=(-1))
ss.wave.paired.data<-subset(ss.wave.paired.data,smoker_prime!=(-7))
ss.wave.paired.data<-subset(ss.wave.paired.data,smoker_prime!=(-2))
ss.wave.paired.data<-subset(ss.wave.paired.data,industry_prime!=(-7))
ss.wave.paired.data<-subset(ss.wave.paired.data,industry_prime!=(-3))


#now run the table III regressions
GHQ.reg.col.1<-lm(delta_mental_health~unemployment_prime_dummy+selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
              factor(sex_prime)+home_owner_prime+
              factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
              unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
              age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
              age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
              selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)


GHQ.reg.col.2<-lm(delta_mental_health~unemployment_prime_dummy+selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                    unemployment_rate_county+unemployment_rate_county*unemployment_prime_dummy+
                    unemployment_rate_county*selfemployed_prime_dummy+unemployment_rate_county*sick_prime_dummy+
                    unemployment_rate_county*inactive_prime_dummy+
                    factor(sex_prime)+home_owner_prime+
                    factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                    unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                    age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                    age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                    selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                    wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                    married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)


#and for anxiety
anxiety.reg.col.1<-lm(change_in_anxiety~unemployment_prime_dummy+selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                        factor(sex_prime)+home_owner_prime+
                        factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                        unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                        age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                        age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                        selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                        wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                        married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)


anxiety.reg.col.2<-lm(change_in_anxiety~unemployment_prime_dummy+selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                        unemployment_rate_county+unemployment_rate_county*unemployment_prime_dummy+
                        unemployment_rate_county*selfemployed_prime_dummy+unemployment_rate_county*sick_prime_dummy+
                        unemployment_rate_county*inactive_prime_dummy+
                        factor(sex_prime)+home_owner_prime+
                        factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                        unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                        age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                        age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                        selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                        wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                        married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)


#table IV regressions


#gender interactions
GHQ.gender.int<-lm(delta_mental_health~unemployment_prime_dummy+factor(sex_prime)+unemployment_prime_dummy*factor(sex_prime)+
                     home_owner_prime+
                     factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                     unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                     age+factor(industry_prime)+
                     selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                     wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                     married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)

anxiety.gender.int<-lm(change_in_anxiety~unemployment_prime_dummy+factor(sex_prime)+unemployment_prime_dummy*factor(sex_prime)+
                         home_owner_prime+
                         factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                         unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                         age+factor(industry_prime)+
                         selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                         wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                         married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)

#and age interactions
GHQ.age.int<-lm(delta_mental_health~unemployment_prime_dummy+unemployment_prime_dummy*age_under_30+unemployment_prime_dummy*age_30_to_39+
                     unemployment_prime_dummy*age_40_to_49+factor(sex_prime)+
                     home_owner_prime+
                     factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                     unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                     age+factor(industry_prime)+
                     selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                     wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                     married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)


anxiety.age.int<-lm(change_in_anxiety~unemployment_prime_dummy+unemployment_prime_dummy*age_under_30+unemployment_prime_dummy*age_30_to_39+
                         unemployment_prime_dummy*age_40_to_49+factor(sex_prime)+
                         home_owner_prime+
                         factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                         unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                         age+factor(industry_prime)+
                         selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                         wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                         married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)


#run the IV regressions. Firstly, I need to create some spoousal employment satus dummies
#for this we need and unemployment dummy
unemployment.prime.spouse<-as.data.frame(ifelse(ss.wave.paired.data[,'spouse_employment_status_prime']==3,1,0))
colnames(unemployment.prime.spouse)<-'unemployment_prime_spouse_dummy'
ss.wave.paired.data<-cbind(ss.wave.paired.data,unemployment.prime.spouse)

#we should also subset to get rid of null results on Spouse's employment status
ss.wave.paired.data<-subset(ss.wave.paired.data,spouse_employment_status_prime!=97)
ss.wave.paired.data<-subset(ss.wave.paired.data,spouse_employment_status_prime!=(-9))
ss.wave.paired.data<-subset(ss.wave.paired.data,spouse_employment_status_prime!=(-1))
ss.wave.paired.data<-subset(ss.wave.paired.data,spouse_employment_status_prime!=(-2))

GHQ.tsls<-ivreg(delta_mental_health~unemployment_prime_dummy+selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                  unemployment_prime_spouse_dummy+
                  unemployment_prime_spouse_dummy*unemployment_prime_dummy+unemployment_prime_spouse_dummy*selfemployed_prime_dummy+
                  unemployment_prime_spouse_dummy*sick_prime_dummy+inactive_prime_dummy+
                  factor(sex_prime)+home_owner_prime+
                  factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                  unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                  age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                  age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                  selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                  wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                  married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv|unemployment_rate_AI+
                  selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                  unemployment_prime_spouse_dummy+
                  unemployment_prime_spouse_dummy*unemployment_prime_dummy+unemployment_prime_spouse_dummy*selfemployed_prime_dummy+
                  unemployment_prime_spouse_dummy*sick_prime_dummy+inactive_prime_dummy+
                  factor(sex_prime)+home_owner_prime+
                  factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                  unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                  age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                  age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                  selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                  wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                  married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)

anxiety.tsls<-ivreg(change_in_anxiety~unemployment_prime_dummy+selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                      unemployment_prime_spouse_dummy+
                      unemployment_prime_spouse_dummy*unemployment_prime_dummy+unemployment_prime_spouse_dummy*selfemployed_prime_dummy+
                      unemployment_prime_spouse_dummy*sick_prime_dummy+inactive_prime_dummy+
                      factor(sex_prime)+home_owner_prime+
                      factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                      unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                      age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                      age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                      selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                      wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                      married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv|unemployment_rate_AI+
                      selfemployed_prime_dummy+sick_prime_dummy+inactive_prime_dummy+
                      unemployment_prime_spouse_dummy+
                      unemployment_prime_spouse_dummy*unemployment_prime_dummy+unemployment_prime_spouse_dummy*selfemployed_prime_dummy+
                      unemployment_prime_spouse_dummy*sick_prime_dummy+inactive_prime_dummy+
                      factor(sex_prime)+home_owner_prime+
                      factor(smoker_prime)+household_income_prime+delta_hh_income+occupational_pension_contr_prime+
                      unemployed_last_three_years_prime+incapacitated_last_three_years_prime+inactive_last_three_years_prime+
                      age_dummy_1+age_dummy_2+age_dummy_3+age_dummy_4+age_dummy_5+age_dummy_6+
                      age_dummy_7+age_dummy_8+age_dummy_9+factor(industry_prime)+
                      selfemployed_last_three_years_prime+wave_1+wave_2+wave_3+wave_4+wave_5+wave_6+wave_7+wave_8+
                      wave_9+wave_10+wave_11+wave_12+wave_13+wave_14+wave_15+
                      married+divorced+widowed+race_dummy+degree+Alevel_or_equiv+olevel_or_equiv, data=ss.wave.paired.data)