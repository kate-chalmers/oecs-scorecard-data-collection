# OECS Scorecard Scraper

This github repository hosts the web scraper used to collect the data to be fed into the OECS Scorecard.

There are four indicators that have not been automated, which are the following indicators:
1. Value added per formal employee (EC$)
2. Formal employment-population ratio, 15+ (%)
3. Intentional homicides (per 100,000 people)
4. Rates of police-recorded offenses (robbery) (per 100,000 population)

UNODC xlsx cannot be downloaded with R script running on Github Actions (SSL security error). The datasets may be downloaded from here, which can then replace previous versions in the [*additional data storage*](https://github.com/kate-chalmers/oecs-scorecard-data-collection/tree/main/additional%20data%20storage) folder. <br>

**Homicide here:** https://dataunodc.un.org/dp-intentional-homicide-victims (click download dataset) <br>
**Robbery here:** https://dataunodc.un.org/dp-crime-violent-offences (click download dataset) <br>

The formal employment and value added per formal employee data will have to be collected and calculated seperately, but should also be included in the above folder. <br>

<hr>

Stablity of indicator collection means that collection of indicator depends on web scraping or collection methodologies other than APIs collected through maintained libraries. The indicators source and stability of collection are summarized below:

| Stable  | Source |
| ------------- | ------------- |
| ❌ | ECCB |
| ✅ | WDI  |
| ❌ | UN population projections  |
| ❌ | IRENASTAT  |
| ✅ | WHO  |
| ❌ |  National statistical bureaus  |
| ✅ |   UN Comtradr  |

