# OECS Scorecard Scraper

This github repository hosts the web scraper used to collect the data to be fed into the [OECS Scorecard](https://cds-tools.shinyapps.io/OECS-scorecard/). 

There are two indicators that have not been automated, which are the following indicators:
1. Value added per formal employee (EC$)
2. Formal employment-population ratio, 15+ (%)

<hr>

Stablity of indicator collection means that collection of indicator depends on web scraping or collection methodologies other than APIs collected through maintained libraries. The indicators source and stability of collection are summarized below:


| Stable  | Source |
| ------------- | ------------- |
| ❌ | ECCB |
| ✅ | WDI  |
| ❌ | UNODC  |
| ❌ | UN population projections  |
| ❌ | IRENASTAT  |
| ✅ | WHO  |
| ❌ |  National statistical bureaus  |
| ✅ |   UN Comtradr  |

