
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qualtrics-viz

<!-- badges: start -->
<!-- badges: end -->

qualtrics-viz is a Shiny app that imports, processes, and visualizes
Qualtrics survey data.

<img src="https://res.cloudinary.com/dn83gtg0l/image/upload/v1665605093/qualtricsviz.png" width="90%" />

The app leverages the [qsurvey](https://github.com/jamesdunham/qsurvey)
and [qualtRics](https://github.com/JasperHG90/qualtRics) packages to
ingest the data via the Qualtrics API and extract questions and choices.
The app can currently process and visualize multiple choice, matrix,
pick-group-rank, bipolar, rank order, slider, and drilldown question
types.

For many of the question types, plot colors can be changed via the
`spectrumInput` widget from
[dreamRs/shinyWidgets](https://shinyapps.dreamrs.fr/shinyWidgets/).

All plots can be downloaded and resized as necessary.

All question data is also rendered in a [gt](https://gt.rstudio.com/)
table. Formatting of the table is dependent on question type and
determined internally. Tables are downloaded via a screenshot using the
[dreamRs/capture](https://github.com/dreamRs/capture) function.
