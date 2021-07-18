[![Continuous Integration](https://github.com/OpenSourceEconomics/ose-data-science-course-project-Abraham-newbie/actions/workflows/ci.yml/badge.svg)](https://github.com/OpenSourceEconomics/ose-data-science-course-project-Abraham-newbie/actions/workflows/ci.yml) [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/OpenSourceEconomics/ose-data-science-course-project-Abraham-newbie/HEAD?filepath=https%3A%2F%2Fgithub.com%2FOpenSourceEconomics%2Fose-data-science-course-project-Abraham-newbie%2Fblob%2Fmaster%2Fproject.ipynb) [![nbviewer](https://raw.githubusercontent.com/jupyter/design/master/logos/Badges/nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/OpenSourceEconomics/ose-data-science-course-project-Abraham-newbie/blob/master/project.ipynb)

# Replication of Barrera et. al. 


This repository contains the replication of the paper from **Barrera-Osorio, Felipe and Blakeslee, David S and Hoover, Matthew and Linden, Leigh and Raju, Dhushyanth and Ryan, Stephen P "Delivering Education to the Underserved through a Public-Private Partnership Program in Pakistan."**

The code and data can be found [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UWXULC) and the paper can be accessed [here](http://documents1.worldbank.org/curated/en/868011504015520701/pdf/WPS8177.pdf).

## Brief Description of the Paper

The paper evaluates a program that randomly assigned private schools to underserved villages in Pakistan. Schools in the program were provided a per student subsidy to provide tuition free primary education,with hald of the treated villaged receiving a higher subsidy for female students. The program increased both average enrollment and test scores, and program schools were of higher quality than nearby public schools.

This jupyter notebook contained in this repository attempts to replicate the main findings of the paper and expand on it with visualizations of the treatment effect,additional analyses as well as supplementary robustness checks.

## Additional Notes

The replication is conducted using R. Additional functions required to plot the graphs can be found in the auxiliary folder [here](https://github.com/OpenSourceEconomics/ose-data-science-course-project-Abraham-newbie/tree/master/auxiliary) and plots/graphs which cannot be directly reproduced due to constraints of CI or github (e.g 3-d plots,dynamic plots) can be found in the files folder [here](https://github.com/OpenSourceEconomics/ose-data-science-course-project-Abraham-newbie/tree/master/files).



## Running the Notebook

To run this reproducible notebook,firstly clone the notebook and proceed as follows in your conda terminal.

> $ conda env create -f environment.yml
  $ conda activate project_env