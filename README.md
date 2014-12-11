Hackathoner Profiler
======

Shiny app for gathering simple information about the participants of a hackathon (initially the [Bayes Impact Hackathon] [1]).

### Demo

Go to http://hackprofiler.alizee.io

### Install & Run

In R, you'll need to install the following dependencies: 
```
install.packages(c("shiny", "RSQLite", "fmsb", "rjson", "jsonlite"))
```

Then launch the app from R with either:
```
library(shiny)
runGithub("antoine-lizee/BIHack")
```

or cloning the repository (`git clone ...`) and running the downloaded code in its folder:
```
library(shiny)
runApp()
```

If you want to investigate further, turn on the `b_DEBUG` switch in the `global.R` file.


### More info

Have a look at the ["More Information"] [2] pane of the app to have more context on the what, why and how of this piece of code.

[1]: http://www.bayesimpact.org/hack
[2]: http://hackprofiler.alizee.io/#Information
