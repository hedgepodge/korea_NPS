# korea_NPS
National Patient Sample data by South Korea's Health Insurance Review &amp; Assessment Service

## [`import_nps.r`](https://github.com/hyung1118/korea_NPS/import_nps.r)
Fuction to import NPS data to the global environment in R.
input: directory
output: nps_1#_$ where # is the last digit of the year and $ is the first digit of the table name.

[`readr`](https://github.com/tidyverse/readr)

### Example. 
``` r
import_nps("~/NPS-2014")
import_nps("~/NPS-2015")
import_nps("~/NPS-2016")
```

-> output are nps_14_2, nps_14_3, nps_14_4, nps_14_5, nps_15_2, nps_15_3, nps_15_4, nps_15_5, nps_16_2, nps_16_3, nps_16_4, nps_16_5.
To be specific, nps_14_2 means 200table of year 2014.
