# Psychology Visualisation Tool

This repository includes the code for a Shiny App that enables psychologists to visualise scores from cognitive ability and academic achievement tests. The platform is built using R and Shiny and can be accessed and freely utilised via https://github.com/jakekraska/psychvisualisation

## Authors and Contributions

Jake Kraska wrote code for this platform. Jake is the Principal Psychologist at [Level Up Psychology](https://www.leveluppsychology.com.au)

## Acknowledgement and License

You are free to use and modify this code. If you do, please acknowledge Jake Kraska. This app is licenced under Apache License, Version 2.0.

## Correspondence

All correspondence regarding this code can be directed to [Jake Kraska](mailto:jake@leveluppsychology.com.au)

## Local Installation and Running

This platform can be ran locally by following the below instructions.

* Clone this repo to your local machine using https://github.com/jakekraska/psychvisualisation
* Open app.R in your preferred R IDE, such as [R Studio](https://rstudio.com/)
* Set the working directory to the source file within the IDE or using `setwd(dirname(rstudioapi::getActiveDocumentContext($path)))`
* Install the necessary packages using the `install.packages("PACKAGENAME")` command
    * shiny
    * bsplus
    * htmltools
    * ggplot2
    * dplyr
    * tidyr
    * stringr
    * readr
    * shinyjs
    * colourpicker
    * shinythemes
* The code is not commented heavily, although minimal knowledge of psychometrics, R and Shiny should allow anyone to edit, modify or deploy this app.

## Deployment

There are several guides on deploying Shiny Apps. For example https://shiny.rstudio.com/deploy/