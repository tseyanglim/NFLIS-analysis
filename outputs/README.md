# Figure data table codebooks

## Figure 1 data
- `Drug.sample` - substance category, as defined in the article
- `Time` - year & month in `YYYY-MM` format
- `Total_samples` - total number of samples in this month containing reports of the relevant substance category
- `Fent_present_samples` - total number of samples in this month containing reports of the relevant substance category and *also* containing reports of fentanyl
- `Percent_fent_cooccurrence_monthly` - % of samples in this month of the relevant substance category with co-occurring fentanyl reported, i.e. `Fent_present_samples / Total_samples * 100`
- `Percent_fent_12month_average` - centred 12-month moving average of `Percent_fent_cooccurrence_monthly`

## Figure 2 data
- `State` - 2-letter ANSI/USPS standard state abbreviations, with `0.US` for national aggregate values (for 50 states + DC, excluding territories)
- `Time` - year in `YYYY` format
- `Listed_no_fent` - total number of samples for this year and state containing reports of any of the nine main substance categories examined (see article) but *without* reports of co-occurring fentanyl
- `Listed_fent` - total number of samples for this year and state containing reports of any of the nine main substance categories examined *and* reports of co-occurring fentanyl
- `Fent_only` - total number of samples for this year and state containing reports of fentanyl, *without* any non-fentanyl substances reported
- `Nonlisted_and_fent` - total number of samples for this year and state containing reports of fentanyl, with co-occcuring non-fentanyl substances reported but *not* any of the main substance categories examined
- `Nonlisted_only` - total number of samples for this year and state *without* reports of fentanyl *or* any of the main substance categories examined
- `Perc_listed_cooccurring_fent` - % of samples for this year and state across all main substance categories examined with co-occurring fentanyl reported, i.e. `Listed_fent / (Listed_no_fent + Listed_fent) * 100`
- `Perc_total_samples_fent` - % of all samples for this year and state containing any reports of fentanyl, i.e. `(Listed_fent + Fent_only + Nonlisted_and_fent) / (Listed_no_fent + Listed_fent + Fent_only + Nonlisted_and_fent + Nonlisted_only) * 100`

## Figure 3 data
- `State` - 2-letter ANSI/USPS standard state abbreviations
- `Drug.sample` - substance category, as defined in the article
- `Time` - year in `YYYY` format
- `Total_samples` - total number of samples for this year and state containing reports of the relevant substance category
- `Fent_present_samples` - total number of samples for this year and state containing reports of the relevant substance category and *also* containing reports of fentanyl
- `Percent_fent_cooccurrence` - % of samples for this year and state of the relevant substance category with co-occurring fentanyl reported, i.e. `Fent_present_samples / Total_samples * 100`

## Other files
- `Monthly MK trend test.csv` reports Mann-Kendall trend test results (*tau* and *p* values) for trends in monthly national aggregate fentanyl co-occurrence by substance category, corresponding to **Figure 1**
- `MK trend test p values.csv` and `MK trend test tau values.csv` report Mann-Kendall trend test *p* and *tau* values respectively for trends in annual state-level fentanyl co-occurrence by substance category, corresponding to **Figure 3**
- `MK Summary Table.xlsx` combines the annual Mann-Kendall trend test results from `MK trend test p values.csv` and `MK trend test tau values.csv` and applies conditional formatting for ease of interpretation, as displayed in the Supplementary Material (Supplementary Table 5).
