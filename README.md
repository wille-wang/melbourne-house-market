# Melbourne House Market Map

This repository contains a [Shiny](https://www.rstudio.com/products/shiny/) application that provides an interactive visualization of the Melbourne housing market during 2016 and 2017. The application features a map of property locations, a price distribution plot, and customizable filters based on property type, region, and budget.

## Features

![screenshot](/img/screenshot.png)

- **A**: dashboard for selecting property types, regions, and budget ranges
- **B**: price distribution plot displaying properties that meet the constraints in A
- **C**: detailed property information, including addresses, room configurations, and prices
- **D**: property clusters that meet the selected filters in A
- **E**: measurement tool to calculate distances between specific locations

## Usage

```sh
git clone https://github.com/wille-wang/melbourne-house-market.git
cd melbourne-house-market
Rscript main.R
# Visit the page shown in the terminal
```

The application can also run in [RStudio IDE](https://www.rstudio.com/categories/rstudio-ide/).

## References

- T. Pino, “[Melbourne Housing Snapshot](https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot).” Sep. 2017. Accessed: Sep. 13, 2023.

## Acknowledgements

- **icons**: [Flaticon](https://www.flaticon.com/free-icons/search-engine)
- **UI themes**: [Bootswatch](https://bootswatch.com/)
