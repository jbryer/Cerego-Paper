# The Use of Adaptive Learning Technology in Mathematics and Biology

**Authors:** [Jason Bryer](https://bryer.org) and [Bruce Homer](https://brucehomer.ws.gc.cuny.edu)

**Abstract:** The use of technology to adapt learning to students' performance is not new (Skinner, 1958). However, with increased availability of computers, tablets, and smart phones, reimagining of Skinner's initial teaching machines has proliferated in the EdTech market. This study examines the use of Cerego, and adaptive learning technology framework, as a supplement to asynchronous online mathematics and biology courses. Using propensity score analysis to adjust for selection bias, results suggest that students who use Cerego as a supplement score between 5 and 13 percentage points higher than their non-using counterparts in most weekly quiz grades. Results suggest there is little or no impact on overall course grade, in large part due to the inconsistent use of Cerego throughout the course.

This repository contains the [data](Data/) and [analysis scripts](R/) to reproduce the results of the paper. The [figures](Figures/) and [tables](Tables/) contain the output from the analysis scripts and are included in the [manuscript](https://github.com/jbryer/Cerego-Paper/blob/master/Manuscript/The_Use_of_Adaptive_Learning_Technology_in_Online_Courses.pdf) which is created from [R markdown](https://github.com/jbryer/Cerego-Paper/blob/master/Manuscript/The_Use_of_Adaptive_Learning_Technology_in_Online_Courses.Rmd).

The analysis is completed using the following R scripts:

* **[`Cerego-Analysis.R`](R/Cerego-Analysis.R) - This is the primary R script for running the analysis. This will utilize the R scripts listed below.**
* [`cv.bal.psa.R`](R/cv.bal.psa.R) - Utility function based upon the `PSAgraphics` package to create balance plots.
* [`match.results.R`](R/match.results.R) - Utility function that returns summary information from a `MatchIt` call.
* [`missing.plot.R`](R/missing.plot.R) - Utility function to create a missing data plot.
* [`plotting.R`](R/plotting.R) - Utility functions to generate the results plots.
* [`psa.cerego.R`](R/psa.cerego.R) - Utility function that runs propensity score analysis. This will run both matching and stratification methods.

**Author Note:** Correspondence concerning this article should be addressed to Jason M. Bryer, City University of New York, School of Professional Studies, 119 W 31st Street, New York, NY 10001. Email: jason.bryer@cuny.edu 

**Funding:** This work was supported by the Bill and Melinda Gates Foundation. Opinions expressed herein are those of the authors and do not necessarily represent the views of the Foundation.

**Conflict of Interest:** All authors declare that they have no conflicts of interest.

**Ethics Approval and Consent:** The authors assert that approval was obtained from an ethics review board (IRB) at Excelsior College in Albany, NY. Consent was obtained from participants of the study.

