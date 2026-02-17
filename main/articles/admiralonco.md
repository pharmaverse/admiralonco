# Get Started

## Introduction

As this is a package extension, if you are new to
[admiral](https://pharmaverse.github.io/admiral/) then the best place to
first start reading would be the [Get
Started](https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html)
page. This extension package follows the same main idea and conventions,
and re-uses many functions from
[admiral](https://pharmaverse.github.io/admiral/), so it is important to
thoroughly understand these to be able to use
[admiralonco](https://pharmaverse.github.io/admiralonco/).

## Creating Oncology ADaM Datasets

For the oncology ADaM data structures, an overview of the flow and
example function calls for the most common steps are provided by the
following vignettes:

- [Creating a Basic
  ADRS](https:/pharmaverse.github.io/admiralonco/main/articles/adrs_basic.md)
- [Creating ADRS (Including Non-standard
  Endpoints)](https:/pharmaverse.github.io/admiralonco/main/articles/adrs.md)
- [Creating
  ADTTE](https:/pharmaverse.github.io/admiralonco/main/articles/adtte.md)
- [Creating
  ADTR](https:/pharmaverse.github.io/admiralonco/main/articles/adtr.md)

[admiralonco](https://pharmaverse.github.io/admiralonco/) also provides
template R scripts as a starting point. They can be created by calling
[`use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/use_ad_template.html)
from [admiral](https://pharmaverse.github.io/admiral/), e.g.,

``` r
library(admiral)
```

``` r
use_ad_template(
  adam_name = "adrs",
  save_path = "./ad_adrs.R",
  package = "admiralonco"
)
```

A list of all available templates can be obtained by
[`list_all_templates()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/list_all_templates.html)
from [admiral](https://pharmaverse.github.io/admiral/):

``` r
list_all_templates(package = "admiralonco")
#> Existing ADaM templates in package 'admiralonco':
#> • ADRS_BASIC
#> • ADRS
#> • ADTR
#> • ADTTE
```

## Support

Support is provided via the [admiral Slack
channel](https://pharmaverse.slack.com/).
