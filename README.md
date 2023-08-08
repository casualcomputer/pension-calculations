Notes about Public Service Pension Plan (PSPP)
================

## Introduction

This report quantifies the net present value of the Public Service
Pension Plan (PSPP), providing a monetary resolution to the contentious
debates surrounding its benefits for public servants. Is PSPP as
valuable as you think?

## Scenario

Assume Jack joined the public service at 23 in 2020, and plans to retire
at 55. Suppose he will live until 85. How much are PSPP future payments
worth in today’s dollar? Assume the highest average income over 5
consecutive years is \$75,000.

## Use PSPP Pension Calculator:

We obtain the payment schedule for the above scenario using the [pension
calculator](http://apppen-penapp.tpsgc-pwgsc.gc.ca/penavg-penben_prod/cpr-pbc/accueil-welcome/prep.action)
from the PSPC website, as follows:

| Description                   | Effective From Date | Effective To Date | Type             | Monthly Payments | Reduction multiplier | Monthly Reduced Payments |
|:------------------------------|:--------------------|:------------------|:-----------------|:-----------------|:---------------------|-------------------------:|
| Lifetime pension              | 2062-03-01          | 9999-01-01        | Deferred Annuity | \$2,867.68       | 1                    |                  2867.68 |
| Bridge benefit (Up to age 65) | 2062-03-01          | 2062-03-31        | Deferred Annuity | \$1,101.43       | 1                    |                  1101.43 |
| Lifetime pension              | 2052-03-01          | 9999-01-01        | Annual Allowance | \$1,875.41       | 75%                  |                  1406.56 |
| Bridge benefit (Up to age 65) | 2052-03-01          | 2062-03-31        | Annual Allowance | \$1,101.43       | 75%                  |                   826.07 |

Note that results from the pension calculator is saved as ‘Pension
benefits options report.pdf’ in the ‘data’ folder.

## Create a Monthly Payment Schedule

We do some data joins to create a table containing PSPP’s monthly
payment schedule.

``` r
suppressPackageStartupMessages(library("dplyr"))

# create a data frame with all payment dates
start_date = "2052-03-01"
end_date = "2082-03-01"
Date <- as.POSIXlt(seq(as.Date(start_date), as.Date(end_date), by="month"))
date_df = data.frame(date=Date)

# load a data frame with results from PSPC's pension calculator
payment_df = read.csv("data/payment_df.csv") %>% janitor::clean_names() 
payment_df$effective_from_date = as.Date(payment_df$effective_from_date)
payment_df$effective_to_date = as.Date(payment_df$effective_to_date)

# join tables to create a table with pension payments in each month
payment_schedule_df = date_df %>% left_join(payment_df, join_by(date >= effective_from_date , 
                                                                date <= effective_to_date))  
payment_schedule_df$monthly = as.numeric(payment_schedule_df$monthly)

#create a table summarizing payments by month
payment_summary_df = payment_schedule_df %>% 
    group_by(date) %>% 
    summarise(total_payment = sum(monthly))

#     The result is a table that contains retirement payments from the PSPP,
## starting from 2052-03-01 (retirement date at 55) to 2082-03-01 (age 85): 
payment_summary_df 
```

    ## # A tibble: 361 × 2
    ##    date                total_payment
    ##    <dttm>                      <dbl>
    ##  1 2052-03-01 00:00:00         2233.
    ##  2 2052-04-01 00:00:00         2233.
    ##  3 2052-05-01 00:00:00         2233.
    ##  4 2052-06-01 00:00:00         2233.
    ##  5 2052-07-01 00:00:00         2233.
    ##  6 2052-08-01 00:00:00         2233.
    ##  7 2052-09-01 00:00:00         2233.
    ##  8 2052-10-01 00:00:00         2233.
    ##  9 2052-11-01 00:00:00         2233.
    ## 10 2052-12-01 00:00:00         2233.
    ## # ℹ 351 more rows

## Calculate the Net Present Value (NPV) of PSPP

Now, we want to figure out how much the pension payments are worth in
today’s dollar\*.

``` r
suppressPackageStartupMessages(library("FinancialMath"))
suppressPackageStartupMessages(library("scales"))
suppressPackageStartupMessages(library("lubridate"))

pension_npv = NPV(cf0=0, cf=payment_summary_df$total_payment, 
          times=-interval(payment_summary_df$date, mdy(03012023)) %/% months(1), i=0)

cat(paste0("The PSPP's NPV is:",dollar(pension_npv)," (",mdy(03012023)," dollar)"))
```

    ## The PSPP's NPV is:$1,299,935 (2023-03-01 dollar)

## How much are PSPP cashflows worth in today’s dollar?

``` r
rate_vector = seq(0,0.15,0.01) #a vector of annual discount rate
pension_value_vector = c()
for (rate in rate_vector){
    curr_value = NPV(cf0=0, cf=payment_summary_df$total_payment, 
        times=-interval(payment_summary_df$date, mdy(03012023)) %/% months(1), i=rate/12)
    pension_value_vector = c(pension_value_vector,curr_value)
}
return_df = data.frame(annual_discount_rate = rate_vector, pension_npv=pension_value_vector)
return_df$pension_npv = dollar(return_df$pension_npv )
return_df$monthly_discount_rate = percent(return_df$annual_discount_rate/12)
return_df$annual_discount_rate = percent(return_df$annual_discount_rate)
names(return_df) = c("ROI (Annual)","Cash required for investment (2023 dollar)","ROI (Monthly)")
kable(return_df)
```

| ROI (Annual) | Cash required for investment (2023 dollar) | ROI (Monthly) |
|:-------------|:-------------------------------------------|:--------------|
| 0.0%         | \$1,299,935                                | 0.000%        |
| 1.0%         | \$824,441                                  | 0.083%        |
| 2.0%         | \$526,561                                  | 0.167%        |
| 3.0%         | \$338,698                                  | 0.250%        |
| 4.0%         | \$219,411                                  | 0.333%        |
| 5.0%         | \$143,143                                  | 0.417%        |
| 6.0%         | \$94,040                                   | 0.500%        |
| 7.0%         | \$62,206                                   | 0.583%        |
| 8.0%         | \$41,425                                   | 0.667%        |
| 9.0%         | \$27,766                                   | 0.750%        |
| 10.0%        | \$18,728                                   | 0.833%        |
| 11.0%        | \$12,709                                   | 0.917%        |
| 12.0%        | \$8,674                                    | 1.000%        |
| 13.0%        | \$5,952                                    | 1.083%        |
| 14.0%        | \$4,106                                    | 1.167%        |
| 15.0%        | \$2,846                                    | 1.250%        |

Valued in 2023 currency, the Net Present Value (NPV) of cash inflows
generated by PSPP payments equates to \$526,561 over 31.75 years of
service, considering a 2% discount factor (representing the inflation
rate). This translates to \$16,585 when distributed over the 31.75-year
tenure of service.
