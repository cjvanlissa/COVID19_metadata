README
================
Caspar J. van Lissa
4/1/2020

# COVID-19 Metadata

A collection of relevant country/city level metadata about the COVID-19
pandemic, made interoperable for secondary analysis. Curated by [Data
scientists Against Corona](https://dataversuscorona.com/),
collaborators: *Caspar van Lissa*, *Tim Draws*, *Andrii Grygoryshyn*,
and *Konstantin Tomić*.

## Available data sets

The following data sets have been processed:

| Category     | Information                            | Source                                                                                                                                                 | Folder             | License                            | Reference                                                                               |
| :----------- | :------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------- | :----------------- | :--------------------------------- | :-------------------------------------------------------------------------------------- |
| Mobility     | Google mobility data                   | <a href = "https://www.google.com/covid19/mobility/">Google</a>                                                                                        | google\_mobility   |                                    |                                                                                         |
| Policies     | Government effectiveness               | <a href = "www.govindicators.org">Worldwide Governance Indicators</a>                                                                                  | WB\_GOV            | CC-BY 3.0                          |                                                                                         |
| Policies     | COVID-19 specific regulation policies  | <a href = "https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker">Oxford Tracker for Regulation Policies</a> | Ox\_CGRT           | CC-BY 4.0                          | Hale, Thomas and Samuel Webster (2020)                                                  |
| Preparedness | Global Health Security Index           | <a href = "https://www.ghsindex.org/">Nuclear Threat Initiative</a>                                                                                    | GHS                | CC BY-NC-ND 4.0                    |                                                                                         |
| COVID19      | Number of cases and fatalities         | <a href = "https://systems.jhu.edu/">CSSE Global Cases</a>                                                                                             | CSSE               | Copyright (academic use permitted) | <a href = "https://doi.org/10.1016/S1473-3099(20)30120-1">Dong, Du, & Gardner, 2020</a> |
| Economic     | World Development Indicators           | <a href = "https://datacatalog.worldbank.org/dataset/world-development-indicators">World Bank</a>                                                      | WB\_WDI            | CC-BY 4.0                          |                                                                                         |
| Risk level   | Hospital data per country              | <a href = "https://apps.who.int/gho/data/node.main.HWF">WHO Health workforce/facilities database</a>                                                   | <i>In progress</i> |                                    |                                                                                         |
| Risk level   | Health infrastructure per country data | <a href = "https://stats.oecd.org/index.aspx?queryid=30183">OECD Health care resources database</a>                                                    | <i>In progress</i> |                                    |                                                                                         |

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
to the latest data for frequently updated sources, run the following
function in R from the project folder:

    initialize_database()

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
  - Linking databases by ISO3 country codes

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
