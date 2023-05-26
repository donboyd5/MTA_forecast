
# libraries and constants -------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# download ----------------------------------------------------------------
# https://data.ny.gov/Government-Finance/New-York-State-Local-and-Local-Purpose-Taxes-and-F/2vni-8tmb
url <- "https://data.ny.gov/api/views/2vni-8tmb/rows.csv?accessType=DOWNLOAD&sorting=true"
fn <- "New_York_State__Local__and_Local_Purpose_Taxes_and_Fees_Collected_Monthly_by_the_Department_of_Taxation_and_Finance__Beginning_April_1996.csv"

fpath <- file.path(here::here("data", "dtf", fn))

download.file(url, destfile=fpath, mode="wb")


# get previously saved monthly taxes from open data --------------------------------------------

fn <- "New_York_State__Local__and_Local_Purpose_Taxes_and_Fees_Collected_Monthly_by_the_Department_of_Taxation_and_Finance__Beginning_April_1996.csv"
df <- read_csv(here::here("data", "dtf", fn))
glimpse(df)
skim(df)
count(df, `Month of Collections`) |> ht() # goes to Sept 2022 as of 3/22/2023
count(df |> filter(str_detect(`Month of Collections`, "2022")), `Month of Collections`)

## CAUTION: verify that column names are consistent with the new variable names being given below ----
df2 <- df |> 
  setNames(c("sfy", "monthyear", "type", "tax", "value", "fymonth", "typesort", "taxsort")) |> 
  mutate(month=ifelse(fymonth <= 9, fymonth + 3, fymonth - 9),
         year=ifelse(month > 3, sfy - 1, sfy),
         date=as.Date(paste0(year, "/", month, "/", 1))) |> 
  arrange(date)

count(df2, fymonth, month)
count(df2, month, fymonth)

# df2 |> 
#   filter(year==2021, month==7) |> 
#   arrange(value)


# create vnames and unique vnames ------------------------------------------------------------------
# CAUTION: DTF did not keep one-one correspondence between type-tax and typesort-taxsort -- uniqueness appears to be tax names!!!

count(df2, typesort, type, taxsort, tax) |> filter(str_detect(tax, coll("peer", ignore_case=TRUE)))
count(df2, typesort, type, taxsort, tax) |> filter(typesort==2, taxsort %in% 19:20)

df2 |> filter(typesort==2, taxsort %in% 19) |> count(date, typesort, taxsort, type, tax) 

# typesort type                           taxsort tax                                          n
# <dbl> <chr>                            <dbl> <chr>                                    <int>
# 2 Excise and User Taxes and Fees      21 Peer-to-Peer Car Sharing Tax                 5
# 6 Local/local purpose taxes            5 MCTD Peer-to-Peer Car Sharing Tax            5
# 6 Local/local purpose taxes           23 Regional Peer-to-Peer Transportation Tax     5

# typesort type                           taxsort tax                             n
#   1        2 Excise and User Taxes and Fees      19 Vapor Products Registration     7
# 2        2 Excise and User Taxes and Fees      19 Vapor Products Tax             30
# 3        2 Excise and User Taxes and Fees      20 Vapor Products Tax              7

# here is where number 19 changed tax names - they split registration from tax
# 30 2022-06-01        2      19 Excise and User Taxes and Fees Vapor Products Tax              1
# 31 2022-07-01        2      19 Excise and User Taxes and Fees Vapor Products Registration     1

# COME BACK TO THIS (3/10/2023) maybe make this:
# 2,19,Excise and User Taxes and Fees,Vapor Products Registration (tax until June 2022, registration after),
# 2,Excise and User Taxes and Fees,20,Vapor Products Tax,excise_vapetax2


# Peer-to-Peer Car Sharing Tax
vnamesdf <- read_csv('
typesort,type,taxsort,tax,vname
1,Personal Income Tax,1,Withholding Tax,pit_wh
1,Personal Income Tax,2,Estimated Tax,pit_est
1,Personal Income Tax,3,Final Payments,pit_fin
1,Personal Income Tax,4,Delinquencies,pit_del
1,Personal Income Tax,5,Limited Liability Company Fees,pit_llcfees
1,Personal Income Tax,6,Total Gross Collections,pit_gross
1,Personal Income Tax,7,Less:  Refunds and Minor Offsets,pit_ref
1,Personal Income Tax,8,Less:  State/City Offsets,pit_scoffset
1,Personal Income Tax,9,Net Collections,pit_net

2,Excise and User Taxes and Fees,1,Sales and Use Tax,excise_sut
2,Excise and User Taxes and Fees,2,Motor Fuel Taxes,excise_mft
2,Excise and User Taxes and Fees,3,Cigarette Tax,excise_cit
2,Excise and User Taxes and Fees,4,Tobacco Products Tax,excise_tpt
2,Excise and User Taxes and Fees,5,Cigarette Retailer Licenses Fees,excise_cigfees
2,Excise and User Taxes and Fees,6,Cigarette Retailer Vending Machine Stickers,excise_citstickers
2,Excise and User Taxes and Fees,7,Medical Marijuana,excise_medpot
2,Excise and User Taxes and Fees,8,Opioid Excise,excise_opioid
2,Excise and User Taxes and Fees,7,Adult Use Cannabis,excise_weed
2,Excise and User Taxes and Fees,7,Alcoholic Beverage Tax,excise_abt
2,Excise and User Taxes and Fees,8,Alcoholic Beverage Tax,excise_abt
2,Excise and User Taxes and Fees,9,Alcoholic Beverage Tax,excise_abt
2,Excise and User Taxes and Fees,8,Truck Mileage Tax,excise_tmt
2,Excise and User Taxes and Fees,9,Truck Mileage Tax,excise_tmt
2,Excise and User Taxes and Fees,10,Truck Mileage Tax,excise_tmt
2,Excise and User Taxes and Fees,9,Highway Use Certificates of Registration,excise_huregs
2,Excise and User Taxes and Fees,10,Highway Use Certificates of Registration,excise_huregs
2,Excise and User Taxes and Fees,11,Highway Use Certificates of Registration,excise_huregs
2,Excise and User Taxes and Fees,10,Fuel Use Tax,excise_fut
2,Excise and User Taxes and Fees,11,Fuel Use Tax,excise_fut
2,Excise and User Taxes and Fees,12,Fuel Use Tax,excise_fut
2,Excise and User Taxes and Fees,11,IFTA Fuel Use Tax,excise_iftafut
2,Excise and User Taxes and Fees,12,IFTA Fuel Use Tax,excise_iftafut
2,Excise and User Taxes and Fees,13,IFTA Fuel Use Tax,excise_iftafut
2,Excise and User Taxes and Fees,12,IFTA Decal Fees and Trip Permits,excise_iftafees
2,Excise and User Taxes and Fees,13,IFTA Decal Fees and Trip Permits,excise_iftafees
2,Excise and User Taxes and Fees,14,IFTA Decal Fees and Trip Permits,excise_iftafees
2,Excise and User Taxes and Fees,13,Passenger Car Rental Tax,excise_carrental
2,Excise and User Taxes and Fees,14,Passenger Car Rental Tax,excise_carrental
2,Excise and User Taxes and Fees,15,Passenger Car Rental Tax,excise_carrental
2,Excise and User Taxes and Fees,14,Hotel Room Occupancy Tax,excise_hotel
2,Excise and User Taxes and Fees,15,Hotel Room Occupancy Tax,excise_hotel
2,Excise and User Taxes and Fees,16,Hotel Room Occupancy Tax,excise_hotel
2,Excise and User Taxes and Fees,15,Nonrefillable Beverage Containers,excise_containers
2,Excise and User Taxes and Fees,16,Nonrefillable Beverage Containers,excise_containers
2,Excise and User Taxes and Fees,17,Nonrefillable Beverage Containers,excise_containers
2,Excise and User Taxes and Fees,17,Vapor Products Registration,excise_vapereg
2,Excise and User Taxes and Fees,18,Vapor Products Registration,excise_vapereg
2,Excise and User Taxes and Fees,19,Vapor Products Tax,excise_vapetax
2,Excise and User Taxes and Fees,21,Peer-to-Peer Car Sharing Tax, excise_p2pcarshare

3,Business Taxes,1,Corporation Franchise Tax (Articles 9A & 13),bus_cft
3,Business Taxes,2,Corporation Franchise Tax (Articles 9A & 13),bus_cft
3,Business Taxes,2,"Corporation Taxes (Article 9 - excludes sections 180, 186A, A&E, PSC, and 186F)",bus_corp
3,Business Taxes,3,"Corporation Taxes (Article 9 - excludes sections 180, 186A, A&E, PSC, and 186F)",bus_corp
3,Business Taxes,3,"Utilities Taxes (Article 9 - sections 186A, A&E, and PSC)",bus_util
3,Business Taxes,4,"Utilities Taxes (Article 9 - sections 186A, A&E, and PSC)",bus_util
3,Business Taxes,4,Public Safety Communications Surcharge (Article 9 - section 186F),bus_psc
3,Business Taxes,5,Public Safety Communications Surcharge (Article 9 - section 186F),bus_psc
3,Business Taxes,5,Banking Corporations Franchise Tax (Article 32),bus_bank
3,Business Taxes,6,Banking Corporations Franchise Tax (Article 32),bus_bank
3,Business Taxes,6,Insurance Corporations Franchise Tax (Article 33),bus_insure
3,Business Taxes,7,Insurance Corporations Franchise Tax (Article 33),bus_insure
3,Business Taxes,7,Insurance Direct Writings Tax (Article 33A),bus_insdirect
3,Business Taxes,8,Insurance Direct Writings Tax (Article 33A),bus_insdirect
3,Business Taxes,8,Petroleum Business Tax (Article 13A prior to 9/1/1990),bus_pbtpre1990
3,Business Taxes,9,Petroleum Business Tax (Article 13A prior to 9/1/1990),bus_pbtpre1990
3,Business Taxes,9,Petroleum Business Tax (Article 13A on and after 9/1/1990),bus_pbt
3,Business Taxes,10,Petroleum Business Tax (Article 13A on and after 9/1/1990),bus_pbt
3,Business Taxes,10,"Oil Users (Article 9, section 182A)",bus_oil
3,Business Taxes,11,"Oil Users (Article 9, section 182A)",bus_oil
3,Business Taxes,1,Lubricating Oils,bus_lube
3,Business Taxes,2,Lubricating Oils,bus_lube
3,Business Taxes,11,Lubricating Oils,bus_lube
3,Business Taxes,12,Pass Through Entity Tax,bus_ptet

4,Property Transfer Taxes,1,Estate Tax,ptt_estate
4,Property Transfer Taxes,2,Estate Tax,ptt_estate
4,Property Transfer Taxes,2,Real Estate Transfer Tax,ptt_rett
4,Property Transfer Taxes,3,Real Estate Transfer Tax,ptt_rett
4,Property Transfer Taxes,3,Real Property Transfer Gains Tax,ptt_rpgt
4,Property Transfer Taxes,4,Real Property Transfer Gains Tax,ptt_rpgt
4,Property Transfer Taxes,4,Gift Tax,ptt_gift
4,Property Transfer Taxes,5,Gift Tax,ptt_gift

5,Other Revenues,1,Pari-Mutuel Taxes,other_parimutuel
5,Other Revenues,2,Hazard Waste Assessments (Environmental Conservation Law),other_hazard
5,Other Revenues,3,Waste Tire Management and Recycling Fees (Environmental Conservation Law),other_tires
5,Other Revenues,4,Returnable Beverage Container Deposits (Environmental Conservation Law),other_deposits
5,Other Revenues,5,Tax Return Preparer Registration Fees,other_taxprep
5,Other Revenues,6,Racing Admissions Tax,other_racing
5,Other Revenues,7,Boxing and Wrestling Exhibitions Tax,other_boxing
5,Other Revenues,7,Authorized Combative Sports Tax,other_combat
5,Other Revenues,8,Wireless Communications Service Surcharges (County Law),other_wireless
5,Other Revenues,9,Office of General Services Procurement Fees,other_ogsfees
5,Other Revenues,10,Employer Compensation Expense Tax,other_ecet

6,Local/local purpose taxes,1,"MTA Corporate Surcharge (Articles 9, 9A, 32, and 33)",local_mtasurch
6,Local/local purpose taxes,2,MTA Passenger Car Rental Tax,local_mtacarrental
6,Local/local purpose taxes,3,MCTD Mobility Tax,local_mtapmt
6,Local/local purpose taxes,4,MCTD Taxicab Ride Tax,local_mtataxi
6,Local/local purpose taxes,5,New York City Alcoholic Beverage Tax,local_nycabt
6,Local/local purpose taxes,6,New York City Personal Income Tax Net Collections,local_nycpitnet
6,Local/local purpose taxes,7,New York City Personal Income Tax Gross Collections,local_nycpitgross
6,Local/local purpose taxes,8,Less:  New York City Personal Income Tax Refunds and Minor Offsets,local_nycpitrefunds
6,Local/local purpose taxes,9,Plus:  New York City Personal Income Tax State/City Offsets,local_nycpitscoffset
6,Local/local purpose taxes,10,Local Sales and Use Taxes,local_sut
6,Local/local purpose taxes,11,Stock Transfer Tax,local_stt
6,Local/local purpose taxes,12,Yonkers Personal Income Tax Net Collections,local_yonkerspitnet
6,Local/local purpose taxes,13,Yonkers Personal Income Tax Gross Collections,local_yonkerspitgross
6,Local/local purpose taxes,14,Less:  Yonkers Personal Income Tax Refunds and Minor Offsets,local_yonkerspitrefunds
6,Local/local purpose taxes,15,Plus:  Yonkers Personal Income Tax State/City Offsets,local_yonkerspitscoffset
6,Local/local purpose taxes,16,MCTMT Personal Income Tax Net Collections,local_mtapmtpitnet
6,Local/local purpose taxes,17,MCTMT Personal Income Tax Gross Collections,local_mtapmtpitgross
6,Local/local purpose taxes,18,Less:  MCTMT Personal Income Tax Refunds and Minor Offsets,local_mtapmtpitrefunds
6,Local/local purpose taxes,19,Plus:  MCTMT Personal Income Tax State/City Offsets,local_mtapmtpitscoffset
6,Local/local purpose taxes,20,"Article 9, Section 186g Local Surcharge",local_surchargeart9
6,Local/local purpose taxes,21,Congestion Surcharge,local_congestion
6,Local/local purpose taxes,22,New York City Real Estate Transfer,local_nycrett
')

# create unique vnames

uvnamesdf <- vnamesdf |> 
  select(type, tax, vname) |> 
  distinct() |> 
  arrange(type, tax, vname)

# do we have any cases where a vname or tax name is repeated for different corresponding items?
count(vnamesdf, vname, tax) |> 
  group_by(vname) |> 
  mutate(nu=length(unique(tax))) |> 
  ungroup() |> 
  filter(nu>1) # none, good

count(vnamesdf, vname, tax) |> 
  group_by(tax) |> 
  mutate(nu=length(unique(vname))) |> 
  ungroup() |> 
  filter(nu>1) # ecet not a problem any more

glimpse(vnamesdf)
tmp <- count(vnamesdf, vname, tax)



# correct typesort error and put vnames on file ---------------------------

# df3 has 3 changes:
# - drop some unneeded records for mobility tax
# - fix the typesort error for one tax
# - bring in my vnames

df3 <- df2 |> 
  filter(!(tax=="MCTD Mobility Tax" & sfy < 2010)) |> # remove unnecessary missing data
  
  # save original values before doing fixes
  mutate(value_original=value) |> 
  
  # fix a bad typesort value
  mutate(typesort=ifelse(date=="2020-03-01" & tax=="Employer Compensation Expense Tax" & typesort==6,
                         5,
                         typesort)) |> 
  select(type, tax, date, sfy, value, value_original, typesort, taxsort) |> 
  left_join(uvnamesdf, by=c("type", "tax")) 

glimpse(df3)
tmp <- count(df3, typesort, type, vname, tax)
# tmp <- count(df3, typesort, type, vname, tax, taxsort)


## informational --- how to find the typesort error fixed above
# stubs <- df2 |> 
#   select(typesort, type, taxsort, tax) |> 
#   distinct() |> 
#   arrange(typesort, type, taxsort, tax)
# 
# stubs |> 
#   write_csv(here::here("data", "taxstubs.csv")) # inspect this file to find error
# 
# df_err <- df2 |> 
#   filter(tax=="Employer Compensation Expense Tax") |> 
#   arrange(date, typesort)
# count(df_err, typesort)
# df_err |> filter(typesort==6)

# next line shows the error -- typesort should be 5, not 6
# 2020 20-Mar Other Revenues Employer Compensation Expense Tax 107 12 6 10 3 2020 2020-03-01

# define fixes to MCTD Mobility taxes -------------------------------------

# type                      tax               date         sfy value value_original typesort taxsort vname       
# <chr>                     <chr>             <date>     <dbl> <dbl>          <dbl>    <dbl>   <dbl> <chr>       
# 1 Local/local purpose taxes MCTD Mobility Tax 2016-11-01  2017     0              0        6       3 local_mtapmt
# 2 Local/local purpose taxes MCTD Mobility Tax 2017-11-01  2018     0              0        6       3 local_mtapmt
# 3 Local/local purpose taxes MCTD Mobility Tax 2018-11-01  2019     0              0        6       3 local_mtapmt
# 4 Local/local purpose taxes MCTD Mobility Tax 2019-11-01  2020     0              0        6       3 local_mtapmt
# 5 Local/local purpose taxes MCTD Mobility Tax 2021-07-01  2022     0              0        6       3 local_mtapmt

df3 |> 
  filter(tax=="MCTD Mobility Tax", sfy==2020)

# apparent errors:
# sfy 2020 Oct Nov are both in Oct, need to split

fixes_mobility <- read_csv(
  "date, vname, fixval
2016-11-01, local_mtapmt, 96840
2017-11-01, local_mtapmt, 96741
2018-11-01, local_mtapmt, 99141
2019-10-01, local_mtapmt, 125215
2019-11-01, local_mtapmt, 102558
2021-07-01, local_mtapmt, 111347
")
fixes_mobility



## define fixes to April 2021 ----

# CAUTION: THis is a hypothesis, not yet verified
# fix PIT component of mobility tax by subtraction

# local_mtapmtpitnet local_mtapmtpitgross local_mtapmtpitrefunds local_mtapmtpitscoffset 
# the problem with April 2021 is that reported refunds were 
# equal to reported gross tax, 25,673 
# A more reasonable estimate of refunds is 1,000
# so the fix is to change refunds to 1000
# and change the net to the reported amount minus of zero plus the change in refunds of 24673
# local_mtapmtpitnet MCTMT Personal Income Tax Net Collections -- reported was 25,673
# local_mtapmtpitrefunds Less:  MCTMT Personal Income Tax Refunds and Minor Offsets -- reported 25,673
df3 |> 
  filter(str_detect(vname, "local_mtapmt"), date=="2021-04-01")

df3 |> 
  filter(str_detect(vname, "local_mtapmt"), year(date)==2022)

df3 |> 
  filter(str_detect(vname, "local_mtapmt"), year(date)==2022)


fixes_pmtapr2021 <- read_csv(
  "date, vname, fixval
2021-04-01, local_mtapmtpitrefunds, 1000
2021-04-01, local_mtapmtpitnet, 24673
")

## combine the fixes ----
fixes <- bind_rows(fixes_mobility, fixes_pmtapr2021)
fixes

# correct the data errors ----
df4 <- df3 |> 
  # fix mobility tax by using replacement values
  left_join(fixes, by = c("vname", "date")) |> 
  mutate(value=ifelse(!is.na(fixval), fixval, value_original)) |> 
  select(-fixval)


# verify corrections
df4 |> 
  filter(tax=="Employer Compensation Expense Tax") |> 
  count(typesort)

df4 |>
  filter(value != value_original) |> 
  gt() |> 
  tab_header(title="Values in DTF monthly tax database that I changed from reported values") |> 
  fmt_number(columns=c(value, value_original),
             decimals = 0)


# save labeled file ----
saveRDS(df4, here::here("data", "dtf", "taxmonthly.rds"))



# MCTD Mobility Tax ----

taxmonthly <- readRDS(here::here("data", "dtf", "taxmonthly.rds"))

taxmonthly |> 
  filter(str_detect(vname, "mtapmt"), date=="2020-06-01")

# pit part of pmt is net=gross + scoffset - refunds
#   local_mtapmtpitnet = local_mtapmtpitgross + local_mtapmtpitscoffset - local_mtapmtpitrefunds 

# pmt check
## save pmt data ----
pmt1 <- taxmonthly |> 
  filter(str_detect(vname, "mtapmt"), date >= "2009-10-01") |> 
  select(date, vname, value) |> 
  pivot_wider(names_from = vname) |> 
  mutate(checkpmt=local_mtapmtpitgross + local_mtapmtpitscoffset - local_mtapmtpitrefunds,
         diff=checkpmt - local_mtapmtpitnet,
         totpmt=naz(local_mtapmt) + naz(local_mtapmtpitnet))
pmt1

pmt <- pmt1 |> 
  select(-c(checkpmt, diff)) |> 
  relocate(totpmt, .after=date)
pmt
glimpse(pmt)
summary(pmt)
pmt |> filter(totpmt==max(totpmt)) # looks like Nov 2009 included collections from prior period or required large est pmt??
ht(pmt)

pmt |> 
  mutate(year=year(date)) |> 
  summarise(across(-date, sum), .by=year)

pmt |> 
  mutate(year=year(date)) |> 
  filter(year %in% 2021:2022)



saveRDS(pmt, here::here("data", "dtf", "pmt_collections.rds"))

