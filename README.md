
# Midnight Coders COM3018 Coursework README
Analysis of the Relevance of Antenatal Data Points in the Prediction of Low Birth Weight in the United States


The file `modelX_all.R` runs all model tests to get the accuracy to set up the code.




## Important Information

The data was taken from Big Data Labs and saved as a .csv file.

Two libraries are required to run the code: caret (https://topepo.github.io/caret/) and ggplot2 (https://ggplot2.tidyverse.org/).
RStudio should install the libraries when running the code, but if not:

1. Go into "Packages"
2. Click "Install"
3. Search for "caret", and install
4. Search for "ggplot2", and install


## Usage
To run, first edit `modelX_all.R`. On line 45 you will see:
```r
df<-read.csv("./data/natality_10k.csv")
```
This can be changed to use any `natality_Xk` file with `X` rows. 


Once the desired amount of data to be trained/tested is selected, run the file.

Note: It may take a while to run even small amounts of data (depending on how powerful your machine is).

If you would like to train/test individual models, each file can be ran. These files include the model used in the name.

The file `cigarette_testing.R`  tests the accuracy of the models that may additionally include cigarette data.

The file `ultrasound_testing.R` removes the is_male field (as some places may not be able to check access to an ultrasound machine) to see if the accuracy is still high. The plurarilty is changed to 1 or more

