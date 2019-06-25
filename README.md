# Ecology Data Viz (previously Ecology _#TidyTuesday_)

Much like [TidyTuesday](https://github.com/rfordatascience/tidytuesday), Eco-Data Viz is in the spirit of TidyTuesday focusing on working with ecological data in the R-environment with an emphassis on summarizing and displaying ecological data to a broader audience. All are welcome to join in the fun. 


## Data sets

| Date (Tuesday) | Week Number | Data Source | Data location |
|:---:|:-----:|:----|:------|
| 2019-06-18 |<!--format(as.Date("2019-06-18"),"%V")`-->25| [Iowa DNR AQuIA](https://programs.iowadnr.gov/aquia/search) | [queryResults.csv](./Data/20190618/)|
| 2019-06-25 |<!--format(as.Date("2019-06-25"),"%V")`-->26| [NSIDC](http://nsidc.org/greenland-today/) | [Online Extracted using _rJSON_](https://nsidc.org/greenland-today/greenland-surface-melt-extent-interactive-chart/)|


***

### 2019-06-18 (Iowa Lake Microcystin Concentration)

Q<sub>1</sub> Is the #cyanoHAB season starting earlier this year (2019) than the past 13 year period?
<br>
<br>

<img src="./Plots/png/20190617_iowaDNR_microsystin.png" align="center" width = "70%"/>

<center> Top seven highest observed historic #microcystin concentrations in Iowa. Looks like some are starting the season with a bang. #HABs.</center>
<br>
<br>

Q<sub>2</sub> Has peak microcystin concentrations shifted to earlier in the year over the 13-year period of record?
<br>
<br>

<img src="./Plots/png/20190617_iowaDNR_Peakmicrosystin.png" align="center" width = "70%"/>

<center>Has the annual maximum and timing of the peak Microcystin changed over time in Iowa's top seven highest #microcystin lakes for the 13 years?</center>

***

### 2019-06-25 (Greenland Surface Melt Extent)

1. Replicate [Greenland Daily Melt Plot](https://nsidc.org/greenland-today/)

<img src="./Plots/png/20190625_greenland.png" align="center" width = "70%"/>

<center>Calendar year 2019 Greenland surface melt extent relative to the 1979 to 2018 period of record.</center>

<br>

Q<sub>1</sub> Has the average trend in Greenland melt area increased?

<img src="./Plots/png/20190625_annualmean_greenland.png" align="center" width = "70%"/>

<center>Annual average Greenland melt area with 95% confidence interval for calendar year 1979 to 2018. The annual average has significantly increased over this period of record (Kendall Trend analysis: $\tau$ = 0.62; $\rho$ <0.01).</center>

<br>

Q<sub>2</sub> Has the peak melt area increased during the period of record?

<img src="./Plots/png/20190625_DOY_peak_greenland.png" align="center" width = "70%"/>

<center>Day of max melt area between calendar year 1979 to 2018. </center>



***