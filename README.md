# Melbourne House Market Map

This repository contains a [Shiny application](https://www.rstudio.com/products/shiny/) that provides an information visualization solution to display the Melbourne housing market during 2016 and 2017. The app includes interactive features such as a map with property locations and a price distribution plot, allowing users to filter properties based on type, region, and budget.

## Features

![screenshot](/img/screenshot.png)

- **A**: dashboard for selecting property types, areas, and a budget range
- **B**: distribution plot displaying the properties that meet the constraints in A
- **C**: detailed property information, including addresses, room configurations, and prices
- **D**: clusters containing the properties that meet the constraints in A
- **E**: measure tool to calculate the distance between two specified locations

## Usage

1. Clone this repository to the local machine.
2. Open [RStudio IDE](https://www.rstudio.com/categories/rstudio-ide/).
3. Uncomment the following block in `main.R` to install dependencies.

```r
# install and activate packages
imported_packages <- c(
  "readr", "leaflet", "ggplot2", "shiny", "bslib", "scales", "dplyr"
)

installed_packages <- rownames(installed.packages())
for (i in imported_packages) {
  if (!(i %in% installed_packages)) {
    install.packages(i)
  }
  library(i, character.only = TRUE)
}
```

3. Run the app by clicking the <kbd>Run</kbd> button in RStudio IDE.

## References

[1] T. Pino, “[Melbourne Housing Snapshot](https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot).” Sep. 2017. Accessed: Sep. 13, 2023.

## Acknowledgements

- **icons**: [Flaticon](https://www.flaticon.com/free-icons/search-engine)
- **UI themes**: [Bootswatch](https://bootswatch.com/)
