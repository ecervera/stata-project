
clear all
set more off
******************************************************
*Project: Female managers and firm performance: Evidence from the non-agricultural sectors in Caribbean Countries_Economic Modelling
*By: Lo Bue and Martinez Zarzoso
*Purpose: Replication dofile
*Stata Version: 16
******************************************************

*trace the file
//local user 	  "mlobue" // USER'S NAME- To run this code change the name in brackets with your user name in Dropbox
local raw "2-data" // PATH WHERE THE DATA IS STORED
local result "4-results" // PATH WHERE THE RESULTS ARE STORED
use "`raw'/GENDER_manager_Data_IADB_FIRMs_LA_ProteQIN 13 Countries NO ID Var (All USD & NEW_ADD_WEIGHT) 20150319.dta" // open the PROTEqIN file, monetary var already in USD

 ***********************************************************************
*Creation of  variables for the regression analysis
***********************************************************************
*Numeric Code for Country and sector dummies, SECTOR already numeric
encode COUNTRY, gen(countryc) 
*encode SECTOR, gen(sectorc)
tab GEND1A
*Replace not know by missing 
replace GEND1A=. if GEND1A<0 /*How would you characterize the gender composition of the owners/shareholders of*/
replace GEND2A=. if GEND2A<0 /*How would you characterize the gender composition of the management group*/
*************************************
*Female dummy variables creation*
*************************************
tab OLD_B7A /*Gender of the top manager*/
gen tfem=OLD_B7A
replace tfem=. if OLD_B7A==-44
replace tfem=1 if OLD_B7A==2
replace tfem=0 if OLD_B7A==1
label var tfem "Female Top Manager"
tab tfem
****************************************
 *GEND1B- Is the largest owner a female?
 ****************************************
 gen ofem=GEND1B
 replace ofem=0 if GEND1B==2
 replace ofem=0 if GEND1B==-44
 tab ofem
 label var ofem "The main owner is a female"
 tab tfem ofem
 **************************************
/*What percentage of ownership does she own?*/
tab GEND1D /* 54% missing observations*/
gen femopc=GEND1D
replace femopc=0 if GEND1D==-44
replace femopc=. if GEND1D<-44
label var femopc "Percentage of the firm owned by female:GEND1D"
tab femopc
*GEND1A:Owners of the firm categories:1:all men,2:pre-dom men,3:equal,4:pre-dom women,5:all women  
gen fem_cat=GEND1A if GEND1A>0 
tab fem_cat
label var fem_cat "GEND1A>0:owners=1 all men,2:predom men,3:equal,4:predom women,5:all women"

gen femmore=1 if fem_cat==3|fem_cat==4|fem_cat==5
replace femmore=0 if fem_cat==1|fem_cat==2
tab femmore
label var femmore "1 if at least 50% of the owners are female:fem_cat"
****************************************
*gender diversity should be defined as firms having both females and males in the management team
gen gendivo=0
replace gendivo=1 if fem_cat==2 |fem_cat==3|fem_cat==4
label var gendivo "There are males and females among owners"
*******************************
*GEND2A:Management composition categories:1:all men,2:pre-dom men,3:equal,4:pre-dom women,5:all women 
gen tfem_cat=GEND2A if GEND2A>0
tab tfem_cat
label var tfem_cat "GEND2A>0:managers=1 all men,2:predom men,3:equal,4:predom women,5:all women"
gen tfemmore=1 if tfem_cat==3|tfem_cat==4|tfem_cat==5
replace tfemmore=0 if tfem_cat==1|tfem_cat==2
tab tfemmore
label var tfemmore "1 if at least 50% among managers are female:tfem_cat"
****************************************
*gender diversity should be defined as firms having both females and males in the management team
gen gendivm=0
replace gendivm=1 if tfem_cat==2 |tfem_cat==3|tfem_cat==4
label var gendivm "There are males and females in management team"


**********************
*Interaction between owner more and manager more female**
gen tfemfem=femmore*tfemmore
tab tfemfem
label var tfemfem"1 if at least 50% are female among the manager and owners:tfemfem"
*****************************
**Years of experience of female manager in the firm/this is only avaiable for females**
gen expin=GEND1F if GEND1F>0
sum expin 
************************************************************
**Experience in other this is only avaiable for females****
gen expout=GEND1G if GEND1G>0
sum expout 
*****************************
**Manager experience in the same sector in years**
gen expsec=B7 if B7>0
sum expsec
label var expin "years of experience in the firm"
label var expout "years of experience in other firm"
label var expsec "years of experience in the same sector"
********************************
*dummy for concentration of ownership
gen owncon=B2 if B2>0
tab owncon
label var owncon "Percent owned by the largest owner"
********************************
*Number of employees at the end of fiscal year
gen nworkers=I2A if I2A>0
sum nworkers
gen lnnworkers=log(nworkers)
label var nworkers "Number of employees at the end of last fiscal year"
label var lnnworkers "Ln Number of employees"

**** FIRM SIZE PROXIED BY NR OF WORKERS
ta nworkers 
hist nworkers
**First classification: <=10; 11-50; >50
gen fisize0="missing"
replace fisize0="small 0-10" if nworkers <=10
replace fisize0="medium 11-50" if nworkers > 10 & nworkers <51
replace fisize0="large 51-1782" if nworkers >=51
ta fisize0
**Second classification: <=50; 51-199; >=200

gen fisize="missing"
replace fisize="small 0-50" if nworkers <=50
replace fisize="medium 51-199" if nworkers > 50 & nworkers <200
replace fisize="large 200-1782" if nworkers >=200

ta fisize
ta fisize0 tfem, row
ta fisize0 tfemmore, row
ta fisize tfem, row
ta fisize tfemmore, row
********************************
**Number of female employees*
des I2B2A I2B2B I2B3
sum I2B2A I2B2B I2B3

gen femempl_man=I2B1 if I2B1>=0
gen femempl_skill=I2B2A   if   I2B2A>=0
gen femempl_unsk=I2B2B  if  I2B2B>=0
gen femempl_noprod=I2B3  if I2B3>=0
gen femempl= (femempl_man+femempl_skill+femempl_unsk+femempl_noprod)
sum femempl*
corre femmore tfem ofem femempl
sum nworkers
gen pcfemempl=femempl*100/nworkers
sum pcfemempl
gen pcfemman=femempl_man*100/I2A1 
replace pcfemman=100 if pcfemman>100
sum pcfem*
label var femempl "No. of employees who were female at end of last fiscal yr"
label var femempl_man "No. of females in management at end of last fiscal yr"
label var femempl_skill "No. of skilled females at end of last fiscal yr"
label var femempl_unsk "No. of Unskilled females in at end of last fiscal yr"
label var femempl_nopro "No. of non-production females in at end of last fiscal yr"
label var pcfemempl "Percent of female employees at end of last fiscal yr"
label var pcfemman "Percent of female managers at end of last fiscal yr"
************************************
*education required for managers*
************************************
 tab  I6A2
gen edumanav=I6A2 if I6A2>0
replace edumanav=1 if I6A2==9
tab edumanav
gen gradu=0
replace gradu=1 if edumanav==7
replace gradu=1 if edumanav==8
tab gradu
label var gradu "The manager aver edu is university"
label var edumanav "Average education level of the manager, 1-8"
**********************
gen edumanmin=I6A1 if I6A1>0
tab edumanmin
replace edumanmin=1 if I6A1==9
corre edu*
gen gradumin=0
replace gradumin=1 if edumanmin==7
replace gradumin=1 if edumanmin==8
tab gradumin

label var gradumin "The manager min education level is college"
label var edumanmin "Minimum education level of the manager, 1-8"
**********************************
*Average salaries of the managers
**********************************
des I5*
/*I5A1     Managers
I5A2       Managers Avg. wage (gross) last year
I5A3       Managers (previous year)
I5A4       Managers Avg. wage (gross) previous year
I5A5       Managers (one year from now)
I5A6       Managers Avg. wage (gross) one year from now
*/
*should be deflated************
gen wagem11=I5A2 
replace wagem11=. if I5A2<0
gen lnwagem11=log(wagem11)
gen wagem12=I5A4
replace wagem12=. if I5A4<0
gen lnwagem12=log(wagem12)
****************
label var wagem11 "Average Manager Wages in 2011 (USD)"
label var wagem12 "Average Manager Wages in 2012 (USD)"

label var lnwagem11 "ln Average Manager Wages in 2011"
label var lnwagem12 "ln Average Manager Wages in 2012"
**************************************************
gen wagem12d=wagem12
replace wagem12d=(wagem12*100/(100+3.3769)) if a1==134
replace wagem12d=(wagem12*100/(100+1.9734)) if a1==127
replace wagem12d=(wagem12*100/(100+4.5333)) if a1==137
replace wagem12d=(wagem12*100/(100+1.3568)) if a1==138
replace wagem12d=(wagem12*100/(100+2.4108)) if a1==139
replace wagem12d=(wagem12*100/(100+2.3919)) if a1==140
replace wagem12d=(wagem12*100/(100+6.8982)) if a1==123
replace wagem12d=(wagem12*100/(100+0.816))  if a1==141
replace wagem12d=(wagem12*100/(100+4.1776)) if a1==142
replace wagem12d=(wagem12*100/(100+5.0069)) if a1==144
replace wagem12d=(wagem12*100/(100+9.2603)) if a1==780
replace wagem12d=(wagem12*100/(100+2.5984)) if a1==143
********************************************
gen wagem11d=wagem11
replace wagem11d=(wagem11*100/(100+3.4567)) if a1==134
replace wagem11d=(wagem11*100/(100+3.1988)) if a1==127
replace wagem11d=(wagem11*100/(100+9.4322)) if a1==137
replace wagem11d=(wagem11*100/(100+1.1312)) if a1==138
replace wagem11d=(wagem11*100/(100+3.0335)) if a1==139
replace wagem11d=(wagem11*100/(100+4.9777)) if a1==140
replace wagem11d=(wagem11*100/(100+7.5297)) if a1==123
replace wagem11d=(wagem11*100/(100+5.8352))  if a1==141
replace wagem11d=(wagem11*100/(100+2.7694)) if a1==142
replace wagem11d=(wagem11*100/(100+17.7118)) if a1==144
replace wagem11d=(wagem11*100/(100+5.1071)) if a1==780
replace wagem11d=(wagem11*100/(100+3.1860)) if a1==143
******************************************
label var wagem11d "Average Manager Wages in 2011 (USD, deflated)"
label var wagem12d "Average Manager Wages in 2012 (USD, deflated)"
gen lnwagem11d=log(wagem11d)
gen lnwagem12d=log(wagem12d)
label var lnwagem11d "Ln Average Manager Wages in 2011"
label var lnwagem12d "Ln Average Manager Wages in 2012"

**************************************************
**Performance Variables to measure productivity**
*************************************************
*Total Sales 2011 and 2012
des K1*
sum K1A K1B 
gen sales11=K1A if K1A>=0
gen sales12=K1B if K1B>=0
sum sales11 sales12
label var sales11 "USD sales in year 2011"
label var sales12 "USD sales in year 2012"
gen lnsales11=log(sales11)
gen lnsales12=log(sales12)
label var lnsales11 "ln sales in year 2011"
label var lnsales12 "ln sales in year 2012"
*I have the inflation in each Caribbean country from the WDI in 2012 and 2011 and will divide the sales by the corresponding CPI

/*	Antigua&Barbuda	Bahamas	Belize(122)	Barbados Dominica Grenada Guyana Jamaica St. Kitts and Nevis St.Lucia Suriname Trinidad&T St. Vincent and the Grenadines
2011	3,4567	  3,1988			9,4322	 1,1312	 3,0335	  4,9777	7,5297	 5,8352	         2,7694	 17,7118	5,1071		3,1860
2012	3,3769	  1,9734			4,5333	 1,3568	 2,4108	  2,3919	6,8982	 0,8161	         4,1776	 5,0069	    9,2603		2,5984
 */
 
gen sales12d=sales12
replace sales12d=(sales12*100/(100+3.3769)) if a1==134
replace sales12d=(sales12*100/(100+1.9734)) if a1==127
replace sales12d=(sales12*100/(100+4.5333)) if a1==137
replace sales12d=(sales12*100/(100+1.3568)) if a1==138
replace sales12d=(sales12*100/(100+2.4108)) if a1==139
replace sales12d=(sales12*100/(100+2.3919)) if a1==140
replace sales12d=(sales12*100/(100+6.8982)) if a1==123
replace sales12d=(sales12*100/(100+0.816))  if a1==141
replace sales12d=(sales12*100/(100+4.1776)) if a1==142
replace sales12d=(sales12*100/(100+5.0069)) if a1==144
replace sales12d=(sales12*100/(100+9.2603)) if a1==780
replace sales12d=(sales12*100/(100+2.5984)) if a1==143
********************************************
gen sales11d=sales11
replace sales11d=(sales11*100/(100+3.4567)) if a1==134
replace sales11d=(sales11*100/(100+3.1988)) if a1==127
replace sales11d=(sales11*100/(100+9.4322)) if a1==137
replace sales11d=(sales11*100/(100+1.1312)) if a1==138
replace sales11d=(sales11*100/(100+3.0335)) if a1==139
replace sales11d=(sales11*100/(100+4.9777)) if a1==140
replace sales11d=(sales11*100/(100+7.5297)) if a1==123
replace sales11d=(sales11*100/(100+5.8352))  if a1==141
replace sales11d=(sales11*100/(100+2.7694)) if a1==142
replace sales11d=(sales11*100/(100+17.7118)) if a1==144
replace sales11d=(sales11*100/(100+5.1071)) if a1==780
replace sales11d=(sales11*100/(100+3.1860)) if a1==143
********************************************

label var sales11d "Sales in year 2011 (USD, deflated)"
label var sales12d "Sales in year 2012 (USD, deflated)"

gen lnsales11d=log(sales11d)
gen lnsales12d=log(sales12d)

label var lnsales11d "Ln Sales in year 2011"
label var lnsales12d "Ln Sales in year 2012"

gen difsales=(sales12d-sales11d)/sales11d
sum difsales
label var difsales "Growth in sales in year 2012 (USD, deflated)"


*Labor Productivity 2011,2012:Sales per worker
gen labpro11=sales11d/nworkers
gen labpro12=sales12d/nworkers
gen lnlabpro11=log(labpro11)
gen lnlabpro12=log(labpro12)

label var labpro11 "Total sales per worker in 2011(USD, deflated)"  
label var labpro12 "Total sales per worker in 2012(USD, deflated)"  

label var lnlabpro11 "Ln Total sales per worker in 2011"  
label var lnlabpro12 "Ln Total sales per worker in 2012"  

*Materials and intermediate goods:K2A total costs of raw materials and intermediate goods used in production
gen mat=K2A
replace mat=. if K2A<0
lab var mat   "total costs of raw materials and intermediate goods used in production"
*************************************************
gen matd=mat
replace matd=(mat*100/(100+3.3769)) if a1==134
replace matd=(mat*100/(100+1.9734)) if a1==127
replace matd=(mat*100/(100+4.5333)) if a1==137
replace matd=(mat*100/(100+1.3568)) if a1==138
replace matd=(mat*100/(100+2.4108)) if a1==139
replace matd=(mat*100/(100+2.3919)) if a1==140
replace matd=(mat*100/(100+6.8982)) if a1==123
replace matd=(mat*100/(100+0.816))  if a1==141
replace matd=(mat*100/(100+4.1776)) if a1==142
replace matd=(mat*100/(100+5.0069)) if a1==144
replace matd=(mat*100/(100+9.2603)) if a1==780
replace matd=(mat*100/(100+2.5984)) if a1==143
********************************************

sum mat matd
lab var matd   "deflated total costs of raw materials and intermediate goods used in production"
gen lnmat=log(mat)
lab var lnmat   " Ln total costs of raw materials and intermediate goods used in production"

gen lnmatd=log(matd)
lab var lnmatd   "Ln deflated total costs of raw materials and intermediate goods used in production"

*VA per worker
gen lnvapw11=log((sales11d-matd)/nworkers) 
gen lnvapw12=log((sales12d-matd)/nworkers) 

label var lnvapw11 "Ln Value Added per worker in 2011"  
label var lnvapw12 "Ln Value Added per worker in 2012"  



*Total labor Costs:K2B Total costs of labor incl. wages,salaries,bonuses and social payments
gen lcost=K2B 
replace lcost=. if K2B<0
sum lcost
gen lnlcost=log(lcost)

*************************************************
gen lcostd=lcost
replace lcostd=(lcost*100/(100+3.3769)) if a1==134
replace lcostd=(lcost*100/(100+1.9734)) if a1==127
replace lcostd=(lcost*100/(100+4.5333)) if a1==137
replace lcostd=(lcost*100/(100+1.3568)) if a1==138
replace lcostd=(lcost*100/(100+2.4108)) if a1==139
replace lcostd=(lcost*100/(100+2.3919)) if a1==140
replace lcostd=(lcost*100/(100+6.8982)) if a1==123
replace lcostd=(lcost*100/(100+0.816))  if a1==141
replace lcostd=(lcost*100/(100+4.1776)) if a1==142
replace lcostd=(lcost*100/(100+5.0069)) if a1==144
replace lcostd=(lcost*100/(100+9.2603)) if a1==780
replace lcostd=(lcost*100/(100+2.5984)) if a1==143
********************************************
gen lnlcostd=log(lcostd)

lab var lcost "Total costs of labor incl. wages,salaries,bonuses and social payments"
lab var lnlcost "Ln Total costs of labor incl. wages,salaries,bonuses and social payments"
lab var lcostd "Deflated Total costs of labor incl. wages,salaries,bonuses and social payments"
lab var lnlcostd "Ln Deflated total costs of labor incl. wages,salaries,bonuses and social payments"
*I have to deflate materials and capital__> DONE

**capital=K6A, net book value of machinery vehicles, and equipment in last fiscal year
gen k=K6A if K6A>0
gen lnk=log(k)
lab var k   "Net book value of machinery vehicles, and equipment in last fiscal year"
lab var lnk   "Ln Net book value of machinery vehicles, and equipment in last fiscal year"

*************************************************
gen kd=k
replace kd=(k*100/(100+3.3769)) if a1==134
replace kd=(k*100/(100+1.9734)) if a1==127
replace kd=(k*100/(100+4.5333)) if a1==137
replace kd=(k*100/(100+1.3568)) if a1==138
replace kd=(k*100/(100+2.4108)) if a1==139
replace kd=(k*100/(100+2.3919)) if a1==140
replace kd=(k*100/(100+6.8982)) if a1==123
replace kd=(k*100/(100+0.816))  if a1==141
replace kd=(k*100/(100+4.1776)) if a1==142
replace kd=(k*100/(100+5.0069)) if a1==144
replace kd=(k*100/(100+9.2603)) if a1==780
replace kd=(k*100/(100+2.5984)) if a1==143
********************************************
gen lnkd=log(kd)

lab var kd "Net value of capital (USD, deflated)"
lab var lnkd "Ln Net value of capital"


***********************************************  
**Control Variables Firm and Constraints**
*************************************************
*Did you purchase Fixed assets in your last fiscal year?
tab J3A
gen fixas=J3A
replace fixas=0 if J3A==2
tab fixas          
label var fixas "The firm purchases fixed assets"
**Foreign Ownership**
*Is the establishment part of a larger firm? 
gen multi=0 if  A2A==2
replace multi=1 if A2A==1
tab multi
lab var multi "The establishment part of a larger firm"
*What percentage of this firm is owned by private foreign individuals, companies or organizations 
gen foreign=B3B
replace foreign=1 if B3B>0

lab var foreign "Percentage of the firm owned by foreigners"

**Exporting (Direct+Indirect Exports)**
gen expdi=D6B+D6A1
tab expdi
replace expdi=0 if expdi<0
replace expdi=100 if expdi>100  
sum expdi
rename expdi exppc
lab var exppc "Percentage of direct and indirect exports"
*dummy exporter
gen exporter=0
replace exporter=1 if exppc>0
tab exporter
lab var exporter "The firm exports"
**Age of the firm** //Assuming that year of the survey is 2013
tab B6A
gen startyr=B6A if B6A<2014|B6A>1669
lab var startyr "Year in which the firm started operations"
gen age=2013-B6A if B6A>1669
replace age=0.5 if age==0
gen lnage=log(age)
sum age 
lab var age "Age of the firm"
lab var lnage "Ln Age of the firm"
**Current Legal Status of Firm**
tab B1
gen legals=B1 
tab legals
 label var legals "Firms current legal status,1-6;B1"
gen shareh=0
replace shareh=1 if legals==1
replace shareh=1 if legals==2
tab shareh
lab var shareh "shareholding company"
**Sole Prop.
gen solepro=0
replace solepro=1 if legals==3
tab solepro
lab var solepro "Sole propietorship"
gen partner=0
replace partner=1 if legals==4
tab partner
lab var partner "Partnership including limited liability"
gen limpartner=0
replace limpartner=1 if legals==5
tab limpartner
lab var limpartner "Limited Partnership"
gen other=0
replace other=1 if legals==6
tab other
lab var other "Other current legal status"

**SECTOR of the Firm**
*MANUFACTURE
tab SECTOR if SECTOR<45
gen manuf=0 if SECTOR>44
replace manuf=1 if SECTOR<45
lab var manuf "Manufacturing activities"
*retail
gen retail=0
replace retail=1 if SECTOR==52
tab retail
lab var retail "Retail activities"

*other SERVICEs
tab SECTOR if SECTOR<45 
gen otserv=0 
replace otserv=1 if SECTOR>44&SECTOR!=52
tab otserv
lab var otserv "Other services"

**Main Market**
tab D5A
**Formal Status of the firm**
*Is your establishment formally registered? very few informal <5
tab B6C

********************
*BUSINESS CONSTRAINS
********************
**Informal Competition 
*Does this establishment compete against unregistered or informal firms?
tab D10A
gen infcomp=0 if D10A==2
replace infcomp=1 if D10A==1
lab var infcomp"The firm compete against informal firms"
**Owning a Website
tab C4B
gen web=0 if C4B==2
replace web=1 if C4B==1
tab web
lab var web "The firm uses its own website"
**Technical Assistance 
*Does your establishment currently benefit from any technical assistance programs? 
tab M1B
gen techas=0 if M1B==2
replace techas=1 if M1B==1
lab var techas "The firm benefits from any technical assistance programs"

**Innovation (2 possible proxies)
*Does this establishment have a department or a group of professionals dedicated to innovation (research and development, service)? 
gen innovd=0 if E1B==2
replace innovd=1 if E1B==1
lab var innovd "Innovation department"
*In the last three years, did this establishment introduce to the market a new or significantly improved good or service? 
gen innovp=0 if E1A==2
replace innovp=1 if E1A==1
lab var innovp "Innovation introduced"

corr innovd innovp
*sum characteristics of the firm
sum  lnage shareh partner limpartner multi foreign export manuf retail otserv  web  techas innovd innovp

**Institutional Constrain variables (Obstacles)**
gen tel=L1A
gen elec=L1B
gen trans=L1C
gen land=L1D
gen policy=L1M
gen taxrates=L1E
gen taxadmin=L1F
gen customs=L1G
gen labour=L1H
gen skills=L1I
gen permits=L1J
gen accesfinance=L1K
gen costfinance=L1L
gen macroenv=L1N 
gen corruption=L1O
gen crime=L1P
gen competitors=L1Q
gen corruption2=L3 
*sum business constraints
sum  skills permits accesfinance  macroenv corruption crime infcomp
sum age lnage SECTOR tel elec trans land policy taxrates taxadmin customs labour 

replace tel=. if tel==-99
replace elec=. if elec==-99
replace trans=. if trans==-99
replace land=. if land==-99
replace policy =. if policy==-99|policy==-66
replace taxrates=. if taxrates==-99
replace taxadmin=. if taxadmin==-99
replace labour=. if labour==-99
replace customs=. if customs==-99|customs==-77|customs==-66
replace skills=. if skills==-99
replace permits=. if permits==-99|permits==-66
replace accesfinance=. if accesfinance==-99|accesfinance==-66
replace costfinance=. if costfinance==-99
replace macroenv=. if macroenv==-99
replace corruption=. if corruption==-99|corruption==-66
replace crime=. if crime==-99
replace competitors=. if competitors==-99|competitors==-66
replace corruption2=. if corruption2==-99
label variable tel "Telecomunications"
label variable elec "Electricity"
label variable trans "Transportation"
label variable land "Access to land"
label variable policy "Political environment"
label variable taxrates "Tax rates"
label variable taxadmin "Tax administration"
label variable customs "Customs and trade regulations"
label variable labour "Labour regulations"
label variable skills "Inadequately educated workforce"
label variable permits "Business licencing and permits"
label variable accesfinance "Access to finance"
label variable costfinance "Cost of finance -Interest rate"
label variable macroenv "Macroeconomic environment"
label variable corruption "Corruption"
label variable crime "Crime, theft and disorder"
label variable competitors "Practices of competitors"

*change dummies to unitary pc
 gen foreign1=foreign/100
 gen owncon1=owncon/100 

 


              *************************************
              *           TABLE 1                 *
              *************************************
tabstat  tfem  tfemmore ofem femmore, by(SECTOR) stat(mean)  format(%5.0g) la(40) 

              *************************************
              *           TABLE 2                 *
              *************************************
			               
global treat tfem
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 
*t2docx $ylist $xmana $xfirm $xconstr using Table2.docx, replace by(tfem)


              *************************************
              *              TABLE 3              *
              *************************************  
global treat tfem 
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 

*with sector (17-1) and country (13-1)dummies, for the variable female top manager
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR i.countryc, robust
outreg2 using "`result'/table3.xls", replace dec(3) excel e(r2_a) label  drop (i.SECTOR i.countryc) addtext(Country FE, Yes, Sector FE, Yes)

*with sector*country dummies for the variable female top manager
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc, robust
outreg2 using "`result'/table3.xls", append dec(3) excel label e(r2_a) drop (SECTOR#countryc) addtext(Country-Sector FE, Yes)

*with sector*country dummies for the variable female top manager: services
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/table3.xls", append  excel dec(3) label e(r2_a)  drop (SECTOR#countryc) addtext(Country-Sector FE, Yes, Services, Yes)

*with sector*country dummies for the variable female top manager: manuf
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/table3.xls" , append excel dec(3) label  e(r2_a) drop (SECTOR#countryc) addtext(Country-Sector FE, Yes, Manufactures, Yes)



              *************************************
              *              TABLE 4              *
		      *************************************
global treat tfem 
global ylist lnlabpro12
global xmana lnwagem12d edumanav 
global xfirm lnage fixas shareh foreign export  web techas innovd 
global xconstr skills   accesfinance  tel elec  policy   taxadmin 

*here create constraint variables as a dummy variable, i.e. polity1=1 if policy is seen as a constraint, that is binary instead of discrete and from 0 to 4

gen policy1=0
replace policy1=1 if policy>0

gen accesfi1=0
replace accesfi1=1 if accesfinance>0

gen taxadmin1=0
replace taxadmin1=1 if taxadmin>0

gen tel1=0
replace tel1=1 if tel>0

gen elec1=0
replace elec1=1 if elec>0

gen skills1=0
replace skills1=1 if skills>0

*POLITICAL ENVIRONMENT
reg $ylist  $xmana $xfirm  skills  taxadmin accesfinance  tel elec    SECTOR#countryc i.tfem#i.policy1, robust
outreg2 using "`result'/table4.xls", replace dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  taxadmin accesfinance  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes)
lincom _b[1.tfem#1.policy1] - _b[0.tfem#1.policy1]


reg $ylist  $xmana $xfirm  skills  taxadmin accesfinance  tel elec    SECTOR#countryc i.tfem#i.policy1 if manuf==1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  taxadmin accesfinance  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Manufactures, Yes)
lincom _b[1.tfem#1.policy1] - _b[0.tfem#1.policy1]

reg $ylist  $xmana $xfirm  skills  taxadmin accesfinance  tel elec    SECTOR#countryc i.tfem#i.policy1 if manuf==0, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  taxadmin accesfinance  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Services, Yes)
lincom _b[1.tfem#1.policy1] - _b[0.tfem#1.policy1]


*ACCESS TO FINANCE
reg $ylist $xmana $xfirm  skills  taxadmin  tel elec policy    SECTOR#countryc i.tfem#i.accesfi1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  taxadmin policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes)
lincom _b[1.tfem#1.accesfi1] - _b[0.tfem#1.accesfi1]

reg $ylist $xmana $xfirm  skills  taxadmin  tel elec policy    SECTOR#countryc i.tfem#i.accesfi1 if manuf==1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  taxadmin policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Manufactures, Yes)
lincom _b[1.tfem#1.accesfi1] - _b[0.tfem#1.accesfi1]


reg $ylist $xmana $xfirm  skills  taxadmin  tel elec policy    SECTOR#countryc i.tfem#i.accesfi1 if manuf==0, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  taxadmin policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Services, Yes)
lincom _b[1.tfem#1.accesfi1] - _b[0.tfem#1.accesfi1]
											
* TAX ADMIN													
reg $ylist $xmana $xfirm  skills  accesfinance  tel elec policy    SECTOR#countryc i.tfem#i.taxadmin1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  accesfinance policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes)
lincom _b[1.tfem#1.taxadmin1] - _b[0.tfem#1.taxadmin1]

reg $ylist $xmana $xfirm  skills  accesfinance  tel elec policy    SECTOR#countryc i.tfem#i.taxadmin1 if manuf==1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  accesfinance policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Manufactures, Yes)
lincom _b[1.tfem#1.taxadmin1] - _b[0.tfem#1.taxadmin1]


reg $ylist $xmana $xfirm  skills  accesfinance  tel elec policy    SECTOR#countryc i.tfem#i.taxadmin1 if manuf==0, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  skills  accesfinance policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Services, Yes)
lincom _b[1.tfem#1.taxadmin1] - _b[0.tfem#1.taxadmin1]


*SKILLS
reg $ylist $xmana $xfirm  taxadmin  accesfinance  tel elec policy    SECTOR#countryc i.tfem#i.skills1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes)
lincom _b[1.tfem#1.skills1] - _b[0.tfem#1.skills1]

reg $ylist $xmana $xfirm  taxadmin  accesfinance  tel elec policy    SECTOR#countryc i.tfem#i.skills1 if manuf==1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Manufactures, Yes)
lincom _b[1.tfem#1.skills1] - _b[0.tfem#1.skills1]


reg $ylist $xmana $xfirm  taxadmin  accesfinance  tel elec policy    SECTOR#countryc i.tfem#i.skills1 if manuf==0, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  tel elec    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Services, Yes)
lincom _b[1.tfem#1.skills1] - _b[0.tfem#1.skills1]



*ELECTRICITY
reg $ylist $xmana $xfirm  taxadmin  accesfinance skills tel  policy    SECTOR#countryc i.tfem#i.elec1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  tel skills    SECTOR#countryc) label  addtext(Country-Sector FE, Yes)
lincom _b[1.tfem#1.elec1] - _b[0.tfem#1.elec1]

reg $ylist $xmana $xfirm  taxadmin  accesfinance skills tel  policy    SECTOR#countryc i.tfem#i.elec1 if manuf==1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  tel skills    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Manufactures, Yes)
lincom _b[1.tfem#1.elec1] - _b[0.tfem#1.elec1]


reg $ylist $xmana $xfirm  taxadmin  accesfinance skills tel  policy    SECTOR#countryc i.tfem#i.elec1 if manuf==0, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  tel skills    SECTOR#countryc) label  addtext(Country-Sector FE, Yes, Services, Yes)
lincom _b[1.tfem#1.elec1] - _b[0.tfem#1.elec1]


*TELECOMMUNICATION
reg $ylist $xmana $xfirm  taxadmin  accesfinance skills elec  policy    SECTOR#countryc i.tfem#i.tel1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  elec skills    SECTOR#countryc) label  addtext(Country-Sector FE, Yes)
lincom _b[1.tfem#1.tel1] - _b[0.tfem#1.tel1]


reg $ylist $xmana $xfirm  taxadmin  accesfinance skills elec  policy    SECTOR#countryc i.tfem#i.tel1 if manuf==1, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  elec skills    SECTOR#countryc) label  addtext(Country-Sector FE, Yes,Manufactures, Yes)
lincom _b[1.tfem#1.tel1] - _b[0.tfem#1.tel1]


reg $ylist $xmana $xfirm  taxadmin  accesfinance skills elec  policy    SECTOR#countryc i.tfem#i.tel1 if manuf==0, robust
outreg2 using "`result'/table4.xls", append dec(3) excel e(r2_a) drop ($xmana $xfirm  taxadmin  accesfinance policy  elec skills    SECTOR#countryc) label  addtext(Country-Sector FE, Yes,Services, Yes)
lincom _b[1.tfem#1.tel1] - _b[0.tfem#1.tel1]




              *************************************
              *              TABLE 5              * 
		      *************************************

global treat tfem 
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 

 
 
xi:oaxaca $ylist i.SECTOR i.countryc, by(tfem) 
outreg2 using "`result'/table5.xls", replace dec(3) excel label  drop () title("NC") addtext(Country FE, Yes, Sector FE, Yes) 

xi:oaxaca $ylist $xmana i.SECTOR i.countryc, by(tfem)
outreg2 using "`result'/table5.xls", append dec(3) excel label  drop () title("TM") addtext(Country FE, Yes, Sector FE, Yes)

xi:oaxaca $ylist $xmana $xfirm  i.SECTOR i.countryc, by(tfem) 
outreg2 using "`result'/table5.xls", append dec(3) excel label  drop () title("FIRM") addtext(Country FE, Yes, Sector FE, Yes)

xi:oaxaca $ylist $xmana $xfirm $xconstr i.SECTOR i.countryc, by(tfem) 
outreg2 using "`result'/table5.xls", append dec(3) excel label  drop () title("CONS") addtext(Country FE, Yes, Sector FE, Yes)

xi:oaxaca $ylist $xmana $xfirm $xconstr i.SECTOR i.countryc, by(tfem) weights (2) 
outreg2 using "`result'/table5.xls", append dec(3) excel label  drop () title("NC") addtext(Country FE, Yes, Sector FE, Yes) 
 
 
              *************************************
              *              TABLE 6              * 
              ************************************* 
  set seed 123
 
 nopo decomp lnlabpro12 i.countryc i.SECTOR, by(tfem) kmatch(ps) kmopt(pscmd(probit) bw(0.001))
 est store cs 
 nopo decomp lnlabpro12 lnwagem12d edumanav  i.countryc i.SECTOR, by(tfem) kmatch(ps) kmopt(pscmd(probit) bw(0.001))
 est store mcs
 nopo decomp lnlabpro12 lnwagem12d lnage  shareh foreign i.countryc i.SECTOR, by(tfem) kmatch(ps) kmopt(pscmd(probit) bw(0.001))
 est store fcs 
 nopo decomp lnlabpro12 lnwagem12d lnage shareh foreign   skills  accesfinance  elec macroenv taxadmin i.countryc i.SECTOR, by(tfem) kmatch(ps) kmopt(pscmd(probit) bw(0.001)) 
  est store ccs 
 
esttab cs mcs fcs ccs using "`result'/table6.xls", se nonumbers nonotes mtitles( "Country and Sector FE" "+Managers' characteristics" "+Firm Characteristics" "+Environment' Constraints") stats(nA mshareuwA nB mshareuwB bwidth, label("N(A)" "% matched A" "N(B)" "% matched B" "Bandwidth"))
 
 
 
              *************************************
              *              FIGURE 1              *
		      *************************************             
qreg lnlabpro12 i.tfem lnwagem12d edumanmin edumanav lnage lnnworkers fixas shareh partner limpartner multi foreign export web techas innovd innovp   skills permits accesfinance macroenv corruption crime tel elec trans land  taxrates taxadmin customs labour

qregplot  tfem    , q(5(2.5)95) ols			  
			  
	
              *************************************
              *              FIGURE 2              *
		      *************************************  		  
global treat tfem 
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 


reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==1, robust
estimates store AB
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==2, robust
estimates store BAR
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==3, robust
estimates store BEL
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==4, robust
estimates store DOM
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==5, robust
estimates store GRE
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==6, robust
estimates store GUY
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==7, robust
estimates store JAM
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==8, robust
estimates store StL
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==9, robust
estimates store StK
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==10, robust
estimates store StV
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==11, robust
estimates store SUR
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==12, robust
estimates store BAH
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR if countryc==13, robust
estimates store TT


coefplot  (AB, mlabels (tfem = 1 "A&B") pstyle(p1)) (BAR, mlabels (tfem = 1 "BAR")pstyle(p1)) ///
(BEL, mlabels (tfem = 1 "BEL")pstyle(p1)) (DOM, mlabels (tfem = 1 "DOM")pstyle(p1)) ///
(GRE, mlabels (tfem = 1 "GRE")pstyle(p1)) (GUY, mlabels (tfem = 1 "GUY")pstyle(p1)) ///
(JAM, mlabels (tfem = 1 "JAM")pstyle(p1)) (StL, mlabels (tfem = 1 "StL")pstyle(p1)) ///
(StK, mlabels (tfem = 1 "StK")pstyle(p1)) (StV, mlabels (tfem = 1 "StV")pstyle(p1)) ///
(SUR, mlabels (tfem = 1 "SUR")pstyle(p1)) (BAH, mlabels (tfem = 1 "BAH")pstyle(p1)) ///
(TT, mlabels (tfem = 1 "T&T")pstyle(p1)), keep($treat) levels (90) yline(0) vertical legend(off)
graph export "`result'/figure2.png", replace

			  
			  
			  
			  
			  
			  
			  
********************************************************************************		  
**************************APPENDIX**********************************************			  
********************************************************************************			  

              *************************************
              *      APPENDIX  TABLE A1           *
              *************************************
tabstat  tfem  tfemmore ofem femmore, by(COUNTRY) stat(mean)  format(%5.0g) la(40) 


              *************************************
              *      APPENDIX  TABLE A2           *
              *************************************
fsum $treat $ylist $xmana $xfirm $xconstr, uselabel

bysort $treat: sum $ylist $xmana $xfirm $xconstr
tabstat $ylist $xmana $xfirm $xconstr, by($treat) columns(statistics) varwidth(20)



			  *************************************
              *      APPENDIX  TABLE A3           *
              *************************************

		   set seed 123

nopo decomp lnlabpro12 lnwagem12d lnage  shareh foreign, by(tfem) kmatch(em) 
 est store em
nopo decomp lnlabpro12 lnwagem12d lnage  shareh foreign, by(tfem) kmatch(ps) 
 est store ps
nopo decomp lnlabpro12 lnwagem12d lnage  shareh foreign, by(tfem) kmatch(md) 
 est store md
nopo decomp lnlabpro12 lnwagem12d lnage  shareh foreign, by(tfem) kmatch(ps) kmopt(pscmd(probit) bw(0.0001))
 est store probit

 
 esttab em ps md probit using "`result'/tableA3.xls", se nonumbers nonotes mtitles("exact" "prop. score" "multi. dist." "probit ps") stats(nA mshareuwA nB mshareuwB bwidth, label("N(A)" "% matched A" "N(B)" "% matched B" "Bandwidth"))	  
			  
			  
			  
			  *************************************
              *      APPENDIX  TABLE A4           *
              *************************************
*top part: Target VARIABLE=1 if at least 50% of managers are female
              
global treat tfemmore 
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export manuf retail  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 

*with sector (17-1) and country (13-1)dummies, for the variable > 50% female managers
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR i.countryc, robust
outreg2 using "`result'/tableA4.xls", replace dec(3) excel label  keep(tfemmore) addtext(Country FE, Yes, Sector FE, Yes)

*with sector*country dummies for the variable > 50% female managers
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc, robust
outreg2 using "`result'/tableA4.xls", append dec(3) excel label  keep(tfemmore) addtext(Country-Sector FE, Yes)

*with sector*country dummies for the variable > 50% female managers: services
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/tableA4.xls", append  excel dec(3) label  keep(tfemmore) addtext(Country-Sector FE, Yes, Services, Yes)

*with sector*country dummies for the variable > 50% female managers: manuf
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/tableA4.xls", append excel dec(3) label  keep(tfemmore) addtext(Country-Sector FE, Yes, Manufactures, Yes)
	  
*Bottom part:	Target VARIABLE01 if There are males and females in management team		  
			  
global treat gendivm
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export manuf retail  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 
			  
	*with sector (17-1) and country (13-1)dummies, 
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR i.countryc, robust
outreg2 using "`result'/tableA4.xls", append dec(3) excel label  keep(gendivm) addtext(Country FE, Yes, Sector FE, Yes)

*with sector*country dummies 
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc, robust
outreg2 using "`result'/tableA4.xls", append dec(3) excel label  keep(gendivm) addtext(Country-Sector FE, Yes)

*with sector*country dummies for  services
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/tableA4.xls", append  excel dec(3) label  keep(gendivm) addtext(Country-Sector FE, Yes, Services, Yes)

*with sector*country dummies for  manuf
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/tableA4.xls", append excel dec(3) label  keep(gendivm) addtext(Country-Sector FE, Yes, Manufactures, Yes)		  
			  
			  *************************************
              *      APPENDIX  TABLE A5           *
              *************************************			  
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export manuf retail  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 
		  
			  
			  
xi:oaxaca $ylist i.SECTOR i.countryc, by(tfemmore) 
outreg2 using "`result'/tableA5.xls", replace dec(3) excel label  drop () title("NC") addtext(Country FE, Yes, Sector FE, Yes)

xi:oaxaca $ylist $xmana i.SECTOR i.countryc, by(tfemmore) 
outreg2 using "`result'/tableA5.xls", append dec(3) excel label  drop () title("TM") addtext(Country FE, Yes, Sector FE, Yes)

xi:oaxaca $ylist $xmana $xfirm  i.SECTOR i.countryc, by(tfemmore) relax
outreg2 using "`result'/tableA5.xls", append dec(3) excel label  drop () title("FIRM") addtext(Country FE, Yes, Sector FE, Yes)

xi:oaxaca $ylist $xmana $xfirm $xconstr i.SECTOR i.countryc, by(tfemmore) relax
outreg2 using "`result'/tableA5.xls", append dec(3) excel label  drop () title("CONS") addtext(Country FE, Yes, Sector FE, Yes)
		  
			  *************************************
              *      APPENDIX  TABLE A6           *
              *************************************			  
		
gen pcfemman1=pcfemman/100
	
global xmana lnwagem11d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export   web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 
	
	reg lnlabpro12 pcfemman1  $xmana $xfirm $xconstr i.SECTOR i.countryc, robust cluster(strata)
	outreg2 using "`result'/tableA6.xls", replace dec(3) excel label  keep(pcfemman1) addtext(Country FE, Yes, Sector FE, Yes)
	
	reg lnlabpro12 c.pcfemman1##c.pcfemman1  $xmana $xfirm $xconstr i.SECTOR i.countryc, robust cluster(strata)
	outreg2 using "`result'/tableA6.xls", append dec(3) excel label keep (c.pcfemman1  c.pcfemman1#c.pcfemman1) addtext(Country FE, Yes, Sector FE, Yes)
	
	reg lnlabpro12 c.pcfemman1##c.pcfemman1   $xmana $xfirm $xconstr SECTOR#countryc, robust cluster(strata)	
	outreg2 using "`result'/tableA6.xls", append dec(3) excel label  keep (c.pcfemman1  c.pcfemman1#c.pcfemman1)  addtext(Country-Sector FE, Yes)
	
	reg lnlabpro12 c.pcfemman1##c.pcfemman1   $xmana $xfirm $xconstr SECTOR#countryc  if manuf==0,  robust cluster(strata)	
	outreg2 using "`result'/tableA6.xls", append  excel dec(3) label  keep (c.pcfemman1  c.pcfemman1#c.pcfemman1)  addtext(Country-Sector FE, Yes, Services, Yes)
	
	reg lnlabpro12 c.pcfemman1##c.pcfemman1   $xmana $xfirm $xconstr SECTOR#countryc if manuf==1,  robust cluster(strata)	
	outreg2 using "`result'/tableA6.xls", append excel dec(3) label  keep (c.pcfemman1  c.pcfemman1#c.pcfemman1)  addtext(Country-Sector FE, Yes, Manufactures, Yes)
	
		
		
			  
			  *************************************
              *      APPENDIX  TABLE A7           *
              *************************************
gen pcfemunsk=I2B2B*100/I2A2B
gen pcfemskil=I2B2A*100/I2A2A if I2B2A>=0
gen pcfemnopro=I2B3*100/I2A3 if I2B3>=0
sum pc*
*Regression analysis with Female Top and the interaction witht the share of female employees, it is only significant when using the share of unskilled female workers, not when using skilled or non-production female workers
************************************
gen tfem_pcfememp=tfem*pcfemempl 
gen lnfemempl=log(femempl)
gen tfem_lnfememp=tfem*pcfems

global treat tfem##c.pcfemunsk
global ylist lnlabpro12
global xmana lnwagem11d edumanmin edumanav expsec 
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export manuf retail  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 

*with sector (17-1) and country (13-1)dummies, for the variable female top manager
reg $ylist $treat $xmana $xfirm $xconstr i.SECTOR i.countryc, robust
outreg2 using "`result'/tableA7.xls", replace dec(3) excel label  drop (i.SECTOR i.countryc) addtext(Country FE, Yes, Sector FE, Yes)

*with sector*country dummies for the variable female top manager
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc, robust
outreg2 using "`result'/tableA7.xls", append dec(3) excel label  drop (SECTOR#countryc) addtext(Country-Sector FE, Yes)

*with sector*country dummies for the variable female top manager: services
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/tableA7.xls", append  excel dec(3) label  drop (SECTOR#countryc) addtext(Country-Sector FE, Yes, Services, Yes)

*with sector*country dummies for the variable female top manager: manuf
reg $ylist $treat $xmana $xfirm $xconstr SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/tableA7.xls", append excel dec(3) label  drop (SECTOR#countryc) addtext(Country-Sector FE, Yes, Manufactures, Yes)

*marginal effects
margins, dydx(tfem pcfemunsk)
margins, dydx(*)
*Graph to see differences in female labor force participation, when the manager is a female or a male
kdensity pcfemempl if tfem==1, addplot((kdensity pcfemempl if tfem==0, lpattern(dash)))

                   **********************
                  *APPENDIX KERNEL GRAPHS*
                  ************************
 
 *FIGURE A1
kdensity lnlabpro11 if tfem==0, addplot( (kdensity lnlabpro11 if tfem==1, lpattern("-")))   legend(order(1 "No Top Female Manager" 2 "Top Female Manager" ))    title (Kernel Density) 
 
 
 *FIGURE A2	
kdensity lnlabpro12 if tfem_cat==1, addplot( (kdensity lnlabpro12 if tfem_cat==2,lp("-") )  (kdensity lnlabpro12 if tfem_cat==3, lp(".") ) (kdensity lnlabpro12 if tfem_cat==4, lp("-.-.") )  (kdensity lnlabpro11 if tfem_cat==5, lp("_.") )  ) legend(order(1 "All Males" 2 ">50% Males" 3 "equaly M/F" 4 ">50% Females" 5 "All Females"))    title (Kernel Density) 
  
 
 
 *FIGURE A3	
kdensity lnlabpro12 if fem_cat==1, addplot( (kdensity lnlabpro12 if fem_cat==2,lp("-") )  (kdensity lnlabpro12 if fem_cat==3, lp(".") ) (kdensity lnlabpro12 if fem_cat==4, lp("-.-.") )  (kdensity lnlabpro11 if fem_cat==5, lp("_.") )  ) legend(order(1 "All Males" 2 ">50% Males" 3 "equaly M/F" 4 ">50% Females" 5 "All Females"))    title (Kernel Density) 
   
              *************************************
              *      APPENDIX  FIGURE A4           *
              *************************************   
global treat tfem 
global ylist lnlabpro12
global xmana lnwagem12d edumanmin edumanav expsec
global xfirm lnage lnnworkers fixas shareh partner limpartner multi foreign export  web techas innovd innovp
global xconstr skills permits accesfinance macroenv corruption crime tel elec trans land policy taxrates taxadmin customs labour 

**access to finance
reg accesfinance $treat $xmana $xfirm  i.SECTOR i.countryc, robust
outreg2 using "`result'/olsconstraint.xls", replace dec(3) excel e(r2_a) label  drop (i.SECTOR i.countryc) addtext(Constraint, Finance, Country FE, Yes, Sector FE, Yes)
estimates store fi1
*with sector*country dummies for the variable female top manager
reg accesfinance $treat $xmana $xfirm  SECTOR#countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, Finance, Country-Sector FE, Yes)
estimates store fi2
*with sector*country dummies for the variable female top manager: services
reg accesfinance $treat $xmana $xfirm  SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, Finance, Country-Sector FE, Yes, Services, Yes)
estimates store fi3
*with sector*country dummies for the variable female top manager: manuf
reg accesfinance $treat $xmana $xfirm  SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, Finance, Country-Sector FE, Yes, Manufactures, Yes)
estimates store fi4
coefplot (fi1, label(Country and Sector FE) pstyle(p5) msymbol(diamond)) ///
(fi2, label(Country x Sector FE) pstyle(p5)msymbol(triangle)) ///
(fi3, label(Services) pstyle(p5)msymbol(circle_hollow )) ///
(fi4, label(Manufacturing) pstyle(p5)msymbol(circle)) ///
, keep(tfem) xline(0)  title("Access to finance") 
graph save "`result'/finance.gph", replace

**electricity
reg elec $treat $xmana $xfirm  i.SECTOR i.countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (i.SECTOR i.countryc) addtext(Constraint, elec, Country FE, Yes, Sector FE, Yes)
estimates store e1
reg elec $treat $xmana $xfirm  SECTOR#countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, elec, Country-Sector FE, Yes)
estimates store e2
reg elec $treat $xmana $xfirm  SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, elec, Country-Sector FE, Yes, Services, Yes)
estimates store e3
reg elec $treat $xmana $xfirm  SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, elec, Country-Sector FE, Yes, Manufactures, Yes)
estimates store e4
coefplot (e1, label(Country and Sector FE) pstyle(p5) msymbol(diamond)) ///
(e2, label(Country x Sector FE) pstyle(p5)msymbol(triangle)) ///
(e3, label(Services) pstyle(p5)msymbol(circle_hollow )) ///
(e4, label(Manufacturing) pstyle(p5)msymbol(circle)) ///
, keep(tfem) xline(0)  title("Access to electricity") 
graph save "`result'/electricity.gph", replace


**political environment
reg policy $treat $xmana $xfirm  i.SECTOR i.countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (i.SECTOR i.countryc) addtext(Constraint, policy, Country FE, Yes, Sector FE, Yes)
estimates store p1
reg policy $treat $xmana $xfirm  SECTOR#countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, policy, Country-Sector FE, Yes)
estimates store p2
reg policy $treat $xmana $xfirm  SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, policy, Country-Sector FE, Yes, Services, Yes)
estimates store p3
reg policy $treat $xmana $xfirm  SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, policy, Country-Sector FE, Yes, Manufactures, Yes)
estimates store p4
coefplot (p1, label(Country and Sector FE) pstyle(p5) msymbol(diamond)) ///
(p2, label(Country x Sector FE) pstyle(p5)msymbol(triangle)) ///
(p3, label(Services) pstyle(p5)msymbol(circle_hollow )) ///
(p4, label(Manufacturing) pstyle(p5)msymbol(circle)) ///
, keep(tfem) xline(0)  title("Issues in the political environment") 
graph save "`result'/policy.gph", replace

***tax administration
reg taxadmin $treat $xmana $xfirm  i.SECTOR i.countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (i.SECTOR i.countryc) addtext(Constraint, taxadmin, Country FE, Yes, Sector FE, Yes)
estimates store ta1
reg taxadmin $treat $xmana $xfirm  SECTOR#countryc, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, taxadmin, Country-Sector FE, Yes)
estimates store ta2
reg taxadmin $treat $xmana $xfirm  SECTOR#countryc if manuf==0, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, taxadmin, Country-Sector FE, Yes, Services, Yes)
estimates store ta3
reg taxadmin $treat $xmana $xfirm  SECTOR#countryc if manuf==1, robust
outreg2 using "`result'/olsconstraint.xls", append dec(3) excel e(r2_a) label  drop (SECTOR#countryc) addtext(Constraint, taxadmin, Country-Sector FE, Yes, Manufactures, Yes)
estimates store ta4
coefplot (ta1, label(Country and Sector FE) pstyle(p5) msymbol(diamond)) ///
(ta2, label(Country x Sector FE) pstyle(p5)msymbol(triangle)) ///
(ta3, label(Services) pstyle(p5)msymbol(circle_hollow )) ///
(ta4, label(Manufacturing) pstyle(p5)msymbol(circle)) ///
, keep(tfem) xline(0)  title("Tax Administration") 
graph save "`result'/tax.gph", replace


grc1leg "`result'/finance.gph" "`result'/electricity.gph", cols(2) graphregion(fcolor(white)) saving("`result'/fiel.gph", replace)
grc1leg "`result'/policy.gph" "`result'/tax.gph", cols(2) graphregion(fcolor(white)) saving("`result'/pota.gph", replace)
graph combine "`result'/fiel.gph" "`result'/pota.gph" ,imargin(small) cols(1) ///
graphregion(fcolor(white) margin(zero)) ysize(4.5)  ///
saving("`result'/FigureA4.gph", replace)   
   
               *************************************
              *      APPENDIX  FIGURE A5           *
              *************************************   
kdensity pcfemempl if tfem==1, addplot((kdensity pcfemempl if tfem==0, lpattern(dash)))
   

