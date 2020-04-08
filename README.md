README
================
Caspar J. van Lissa
4/1/2020

# COVID-19 Metadata

A collection of relevant country/city level metadata about the COVID-19
pandemic, made interoperable for secondary analysis. Curated by [Data
scientists Against Corona](https://dataversuscorona.com/),
collaborators: *Caspar van Lissa*, *Tim Draws*, *Andrii Grygoryshyn*,
*Konstantin Tomić*, and *Malte Lüken*.

## Available data sets

The following data sets have been processed:

|Category    |Information                                        |Source                                  |URL                                                                                            |Progress|Folder          |License                           |Reference                                                                              |
|------------|---------------------------------------------------|----------------------------------------|-----------------------------------------------------------------------------------------------|--------|----------------|----------------------------------|---------------------------------------------------------------------------------------|
|Mobility    |Google mobility data                               |Google                                  |https://www.google.com/covid19/mobility/                                                       |Done    |google_mobility |                                  |                                                                                       |
|Risk level  |Hospital data per country                          |WHO Health workforce/facilities database|https://apps.who.int/gho/data/node.main.HWF                                                    |Done    |WHO_OECD        |                                  |                                                                                       |
|Risk level  |Health infrastructure per country data             |OECD Health care resources database     |https://stats.oecd.org/index.aspx?queryid=30183                                                |Done    |WHO_OECD        |                                  |                                                                                       |
|Policies    |Government effectiveness                           |Worldwide Governance Indicators         |www.govindicators.org                                                                          |Done    |WB_GOV          |CC-BY 3.0                         |                                                                                       |
|Policies    |COVID-19 specific regulation policies              |Oxford Tracker for Regulation Policies  |https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker|Done    |Ox_CGRT         |CC-BY 4.0                         |Hale, Thomas and Samuel Webster (2020)                                                 |
|Preparedness|Global Health Security Index                       |Nuclear Threat Initiative               |https://www.ghsindex.org/                                                                      |Done    |GHS             |CC BY-NC-ND�4.0                   |                                                                                       |
|COVID19     |Number of cases and fatalities                     |CSSE Global Cases                       |https://systems.jhu.edu/                                                                       |Done    |CSSE            |Copyright (academic use permitted)|<a href = "https://doi.org/10.1016/S1473-3099(20)30120-1">Dong, Du, & Gardner, 2020</a>|
|Economic    |World Development Indicators                       |World Bank                              |https://datacatalog.worldbank.org/dataset/world-development-indicators                         |Done    |WB_WDI          |CC-BY 4.0                         |                                                                                       |
|Response    |Number of tests                                    |Our world in data                       |                                                                                               |        |OWID_Tests      |                                  |                                                                                       |
|Economic    |Doing Business                                     |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/435/data.csv                    |        |WB_BUSINESS     |CC-BY 4.0                         |                                                                                       |
|Mobility    |Logistics Performance Index                        |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/50/data.csv                     |        |WB_LOGISTICS    |CC-BY 4.0                         |                                                                                       |
|            |Failed States Index                                |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/97/data.csv                     |        |WB_FAILED       |CC-BY 4.0                         |                                                                                       |
|            |Freedom House                                      |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/997/data.csv                    |        |WB_FREEDOM      |CC-BY 4.0                         |                                                                                       |
|            |Global Indicators of Regulatory Governance         |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/50/data.csv                     |        |WB_GOVERNANCE   |CC-BY 4.0                         |                                                                                       |
|            |Institutional Profiles Database                    |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/999/data.csv                    |        |WB_INSTITUTIONAL|CC-BY 4.0                         |                                                                                       |
|            |Worldwide Buresucracy Indicators                   |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/4127/data.csv                   |        |WB_BUREAUCRACY  |CC-BY 4.0                         |                                                                                       |
|            |United Nations Conference on Trade and Development |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/513/data.csv                    |        |WB_TRADE_DEV    |CC-BY 4.0                         |                                                                                       |
|            |Press Freedom Index by Reporters without Borders   |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/1000/data.csv                   |        |WB_PRESS_FREE   |CC-BY 4.0                         |                                                                                       |
|            |Education Statistics                               |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/748/data.csv                    |        |WB_EDUCATION    |CC-BY 4.0                         |                                                                                       |
|            |Gender Statistics                                  |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/747/data.csv                    |        |WB_GENDER       |CC-BY 4.0                         |                                                                                       |
|            |Travel & Tourism Competitiveness                   |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/78/data.csv                     |        |WB_TOURISM      |CC-BY 4.0                         |                                                                                       |
|            |World Travel & Tourism Counsil                     |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/79/data.csv                     |        |WB_WTTC         |CC-BY 4.0                         |                                                                                       |
|            |Poverty ans Equity Data                            |World Bank                              |https://s3.amazonaws.com/datascope-ast-datasets-nov29/datasets/3755/data.csv                   |        |WB_POV_EQUITY   |CC-BY 4.0                         |                                                                                       |




## Folder structure:

| Folder  | Description                                                                                                                                                           | Permissions    |
| :------ | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :------------- |
| data    | Metadata sources in .csv format (intermediate formats are acceptable until they can be made tidy).                                                                    | Do not edit    |
| scripts | (R)-scripts                                                                                                                                                           | Human editable |
| doc     | Documentation for your contribution, ideally in Rmarkdown format. Rmarkdown can contain code chunks. Elaborate functions should be relegated to the ‘scripts’ folder. | Human editable |

## How to use

Fork or clone this repository (for GitHub beginners: You can also click
the green button that says “Clone or download”, and download a .zip).
All data are in the `/data` folder. Some data are rarely updated (e.g.,
annual data), and some are updated daily. To ensure that you have access
to the latest data for frequently updated sources, run the R-script in
the `run_me.R` file, in the main folder.

## Standards for data

Every source is condensed into one data file in `.csv` format, according
to these specifications:

  - Data should be available on the country- or community-and-country
    level.
  - Recent data are the focus; if multi-year data is available, older
    years *can* be dropped
  - All variable names should be lower case
  - Mandatory variables are `country` (plain text country), and
    `countryiso3` (ISO3 country code)
  - Optionally, a `region` variable can be added
  - Data should be in wide format: One row per country, one column per
    variable

## Standards for data dictionary

A `data_dictionary.csv` is available for each data set, *unless the file
contents are immediately clear from the file*. This data dictionary
includes:

  - `variable`: The name of the variable in the data file
  - `description`: The description of this variable

Any other important information per variable can be included in this
dictionary, such as sources, weights, etc.

## News

The following issues are ongoing:

  - Adding more databases; feel free to make a suggest or request a
    database
    [here](https://github.com/cjvanlissa/COVID19_metadata/issues)
  - Added time-since first occurrence for Oxford policy / incidence
    trackers
  - Added last observation carried forward for WHO data

## License

This project is under a GNU GPL v3 open source license (see the LICENSE
file). Individual data sources have different licenses; always check the
license before publishing based on these data.

## Contributing and Contact Information

This project is open for collaborators with valuable expertise.
Contribute by:

  - Filing a GitHub issue
    [here](https://github.com/cjvanlissa/COVID19_metadata/issues)
  - Making a pull request
    [here](https://github.com/cjvanlissa/COVID19_metadata/pulls)

By participating in this project, you agree to abide by the [Contributor
Code of Conduct v2.0](https://www.contributor-covenant.org/).

# A WORCS Project

This project is based on the Workflow for Open Reproducible Code in
Science (WORCS). For more details, please read the preprint at
<https://osf.io/zcvbs/>.

# WORCS - steps to follow for each project

## Study design phase

1.  Create a new Private repository on github, copy the <https://> link
    to clipboard  
    The link should look something like
    <https://github.com/yourname/yourrepo.git>
2.  In Rstudio, click File \> New Project \> New directory \> WORCS
    Project Template
    1.  Paste the GitHub Repository address in the textbox
    2.  Keep the checkbox for `renv` checked if you want to document all
        dependencies (recommended)
    3.  Select a preregistration template
3.  Write the preregistration `.Rmd`
4.  In the top-right corner of Rstudio, select the Git tab, select the
    checkboxes next to all files, and click the Commit button. Write an
    informative message for the commit, e.g., “Preregistration”, again
    click Commit, and then click the green Push arrow to send your
    commit to GitHub
5.  Go to the GitHub repository for this project, and tag the Commit as
    a preregistration
6.  Optional: Render the preregistration to PDF, and upload it to
    AsPredicted.org or OSF.io as an attachment
7.  Optional: Add study Materials (to which you own the rights) to the
    repository. It is possible to solicit feedback (by opening a GitHub
    Issue) and acknowledge outside contributions (by accepting Pull
    requests)

## Data analysis phase

8.  Read the data into R, and document this procedure in
    `prepare_data.R`
9.  Use `open_data()` or `closed_data()` to store the data
10. Write the manuscript in `Manuscript.Rmd`, using code chunks to
    perform the analyses.
11. Regularly commit your progress to the Git repository; ideally, after
    completing each small and clearly defined task. Use informative
    commit messages. Push the commits to GitHub.
12. Cite essential references with one at-symbol
    (`[@essentialref2020]`), and non-essential references with a double
    at-symbol (`[@@nonessential2020]`).

## Submission phase

13. To save the state of the project library (all packages used), call
    `renv::snapshot()`. This updates the lockfile, `renv.lock`.
14. To render the paper with essential citations only for submission,
    change the line `knit: worcs::cite_all` to `knit:
    worcs::cite_essential`. Then, press the Knit button to generate a
    PDF

## Publication phase

13. Make the GitHub repository public
14. [Create an OSF
    project](https://help.osf.io/hc/en-us/articles/360019737594-Create-a-Project);
    although you may have already done this in Step 6.
15. [Connect your GitHub repository to the OSF
    project](https://help.osf.io/hc/en-us/articles/360019929813-Connect-GitHub-to-a-Project)
16. Add an Open Science statement to the manuscript, with a link to the
    OSF project
17. Optional: [Publish preprint in a not-for-profit preprint repository
    such as PsyArchiv, and connect it to your existing OSF
    project](https://help.osf.io/hc/en-us/articles/360019930533-Upload-a-Preprint)
      - Check [Sherpa Romeo](http://sherpa.ac.uk/romeo/index.php) to be
        sure that your intended outlet allows the publication of
        preprints; many journals do, nowadays - and if they do not, it
        is worth considering other outlets.
