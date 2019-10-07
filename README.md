# STAT-628-Module-2: BodyFat-Project
This repo contains the codes, report, app and PPT for the project Male BodyFat Predictor written by Lingfeng Zhu (me), Ruochen Yin, Jiahan Li and Chong Wei for STAT-682. All codes are based on R.

## Dataset
The BodyFat data set is a real data set of 252 men with measurements of their percentage of body fat and various body circumference measurements.  
* Refer to ```data/BodyFat.csv``` for the raw dataset.   
* Refer to ```data/DataDescription.docx``` for more details about the dataset.  
* Refer to ```data/cleaned.csv``` for the cleaned dataset.

## Codes
All the codes are based on R 3.5.3. Please check your R version if you want to replicate our analysis.  
* Refer to ```code/data_cleaning.R``` for the data cleaning progress.  
* Refer to ```code/modeling.R``` for the modeling, feature seleciton and any other related analysis progress.
* Refer to ```code/BodyFat/app.R``` for the source codes of shiny application.

## App
I have completed a shiny application who runs our male body fat calculator in real-time.   
**Shiny APP web address:** https://lingfengzhu.shinyapps.io/bodyfat/  
**Remarks:** 
* The application can only predict body fat percentage for male since the BodyFat dataset contains data of males only.
* Input values are limited within a common range for normal males.

## Images
All  the related graphs and tables (if any) can be found in the folder ```image```.

## Summary
The summary file and presentation slides can be found as following:
* Refer to ```module2.pdf``` for the final summary of our analysis.
* Refer to ```module2.ipynb``` for the executive jupyter notebook (R) file of our analysis.

## Acknowledgement
Prof. HYUNSEUNG KANG, UW-Madison
STAT 628, UW-Madison

## Contact information
Contact us if you are interested in our analysis or have any questions about our work:
* Lingfeng Zhu  lzhu88@wisc.edu
* Ruochen Yin   ryin26@wisc.edu
* Jiahan Li     jli936@wisc.edu
* Chong Wei     cwei48@wisc.edu
