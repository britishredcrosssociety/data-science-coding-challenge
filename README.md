# Coding challenge

## Overview
Scraping, cleaning, analysing, and visualising open data is a common task you would experience as a Data Scientist at the British Red Cross. Our operational teams often require location-based insights around different types of inequalities.

For this challenge, we would like you to explore and select a dataset from the recently published 2021 Census which can be accessed on [nomis](https://www.nomisweb.co.uk/sources/census_2021). We would like you to then:

- programatically scrape your selected data from the web
- perform an exploratory and/or inferential analyses
- use R Shiny visualise the results

## Tips:
- When selecting the parameters for the data on nomis, we recommend selecting `Lower layer Super Output Areas` for the type of geography. Other geographies are also acceptable
- we like lean, concise, well structured code
- we are looking for location-based insights
- we like Shiny apps that are formatted as an [R package](https://mastering-shiny.org/scaling-packaging.html)
- we like Shiny apps that use [modules](https://mastering-shiny.org/scaling-modules.html)
- our [geographr](https://github.com/humaniverse/geographr) R package may prove to be helpful
- the end result of the Shiny app should be understandable to a non-technical audience

## Submission
1. Fork this repository
2. Implement the request listed above
3. Submit a pull request between [insert date one] & [insert date two]

## FAQ
*Can I complete this work in Python?*

No. While you will be expected to understand/use Python alongside R, most of the teams existing code base (e.g., the [humaniverse](https://github.com/humaniverse))is built in R, and it is a requirement for the role that you are able to maintain and expand this. This does not mean future analyses cannot be written in Python (or your language of choice).
