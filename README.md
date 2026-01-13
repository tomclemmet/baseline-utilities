# Baseline Utilities
Code repository for the study "Validating Methods for Estimating Age and Sex Adjusted Baseline Utilities".
## Abstract
Age- and sex-adjusted HSUV baselines are sometimes required for HTA submissions, especially since NICE’s 2022 introduction of absolute/proportional QALY shortfalls to determine severity modifiers. Research on estimating these baselines has been sparse and has not considered a broad range of models. I implement a stratified k-fold cross-validation procedure to test the performance of a selection of baseline utility models: an ALDVMM, a range of polynomial models and a range of RCS models. My results fail to support the DSU’s recommendation of the ALDVMM. Polynomial models are useful but higher-order versions produce counterintuitive predictions for older people. RCS models avoid this problem, with the optimal number of knots (i.e. model flexibility) being around 7. However, the differences in models’ baselines seem unlikely to be decisive in economic evaluations, and there are significant difficulties in estimating HSUVs for older people which further research ought to thoroughly investigate.
 
## Folder Structure
Note that this diagram contains a range of data and image files not available in the repository. This is because some of these files are very large. The raw data files can be accessed from <https://ukdataservice.ac.uk/> and <https://www.ons.gov.uk/>, and all plots and other output files can be reproduced using the code in the script folder. The `run-everything.R` file can be used to easily run all of these scripts in the correct order.
```
baseline-utilities
│   .gitattributes
│   .gitignore
│   baseline-utilities.Rproj
│   README.md
│   run-everything.R
│   session-info.txt
│
├───data
│       hse.csv
│       lifetables-1820.csv
│       npara-baselines.csv
│       para-baselines.csv
│
├───output
│   ├───1-literature
│   │       fig-01--baselines.png
│   │       fig-02--baselines.png
│   │
│   ├───2-data
│   │       app-a1--regression.html
│   │       fig-03--means.png
│   │       fig-04--distributions.png
│   │       fig-05--means.png
│   │       fig-06--variation.png
│   │       tab-01--summary.csv
│   │       tab-02--errors.csv
│   │
│   ├───3-results
│   │       app-b1--errors.csv
│   │       app-b2--errors.csv
│   │       app-c1--qales.csv
│   │       fig-07--kfold.png
│   │       fig-08--rmse.png
│   │       fig-09--rmse.png
│   │       fig-10--errors.png
│   │       fig-11--me.png
│   │       fig-12--me.png
│   │       fig-13--qale.png
│   │       kfold-results.csv
│   │       me-sex.png
│   │       models.txt
│   │       rmse-sex.png
│   │
│   └───4-discussion
│           app-d1--rcs7.csv
│           app-d2--rcs7.png
│
├───raw-data
│   │   hse03ai.tab
│   │   hse04ai.tab
│   │   hse05ai.tab
│   │   hse06ai.tab
│   │   hse08ah.tab
│   │   hse08ai.tab
│   │   hse10ai.tab
│   │   hse11ai.tab
│   │   hse12ai.tab
│   │   hse14ai.tab
│   │
│   └───data-dictionaries
│           hse03ai_UKDA_Data_Dictionary.rtf
│           hse04ai_UKDA_Data_Dictionary.rtf
│           hse05ai_UKDA_Data_Dictionary.rtf
│           hse06ai_UKDA_Data_Dictionary.rtf
│           hse08ai_ukda_data_dictionary.rtf
│           hse10ai_ukda_data_dictionary.rtf
│           hse11ai_ukda_data_dictionary.rtf
│           hse12ai_ukda_data_dictionary.rtf
│           hse14ai_ukda_data_dictionary.rtf
│
└───script
    │   kfold.R
    │   load-hse.R
    │
    └───tables-and-figures
            1-literature.R
            2-data.R
            3-results.R
            4-discussion.R
```
