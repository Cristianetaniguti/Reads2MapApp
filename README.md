![Development](https://img.shields.io/badge/development-active-blue.svg)
[![R-CMD-check](https://github.com/Cristianetaniguti/Reads2MapApp/workflows/R-CMD-check/badge.svg)](https://github.com/Cristianetaniguti/Reads2MapApp/actions)

<p align="center">
<br>
<img src="https://github.com/Cristianetaniguti/Reads2Map/assets/7572527/6074320a-0eba-44b9-88e1-b89eda8aad70" width="450"/>
<br>
<p/>

## Reads2Map App

This shiny app plots graphics for descriptive analyses of the datasets generated by the [Reads2Map workflow](https://github.com/Cristianetaniguti/Reads2Map) workflows. Upload your results in the `Upload data` session. 

Each icon at the left corner of this page points to a different dataset and produces interactive plots.

By now, only one chromosome is evaluated in both workflows. However, the EmpiricalReads2Map workflow also provide the complete VCF file to users reproduce only the chosen pipeline in the other chromosomes. Therefore, users can select the best pipeline using one chromosome and apply it to the others without spent all the time and computer resources needed to compare all the methods for all chromosomes. We showed an example of Reads2Map workflows usage in [Quickstart](https://cristianetaniguti.github.io/Tutorials/Reads2Map/Setup_and_run_Reads2Map_workflows.html)

### How to use 

**warning**: The App current version is 0.0.1, it is compatible with Reads2Map workflows:

* SimulatedReads2Map_v1.0.2 or above
* EmpiricalReads2Map_v1.5.0 or above
* EmpiricalMap_v1.3.0 or above


```{r, eval=FALSE}
devtools::install_github('Cristianetaniguti/Reads2MapApp')
Reads2MapApp::run_app()
```

## How to cite

* [Taniguti, C. H.; Taniguti, L. M.; Amadeu, R. R.; Lau, J.; de Siqueira Gesteira, G.; Oliveira, T. de P.; Ferreira, G. C.; Pereira, G. da S.;  Byrne, D.;  Mollinari, M.; Riera-Lizarazu, O.; Garcia, A. A. F. Developing best practices for genotyping-by-sequencing analysis in the construction of linkage maps. GigaScience, 12, giad092. https://doi.org/10.1093/gigascience/giad092](https://doi.org/10.1093/gigascience/giad092)

