TEST 
![](CDI-Logo.png)

# MLIV - Machine Learning for Impact Variation

## Project Description

**Goal:** The goal of the MLIV project is to investigate the value and limitations of using machine learning to detect the presence of heterogeneous subgroup impacts in randomized control trials (RCTs) in education and other policy domains.

**Research Design and Methods:** The proposed study will address three broad questions:

    (1) Can ML methods replicate published findings on heterogeneity?
    (2) Do ML methods suggest effects are heterogeneous in studies in which published findings have not found evidence of heterogeneous effects?
    (3) Which characteristics of studies make it more likely that ML will be a productive technique for investigating the presence of heterogeneous effects?

To investigate these questions, we will

    (1) conduct secondary analyses of datasets from the multi-site RCT's of Career Academies, of Growth Mindset, and of the Accelerated Studies in Associate Program (ASAP) at CUNY and in Ohio (pooled); and 
    (2) conduct a "calibrated" simulation study in order to investigate the circumstances under which the ML methods are more likely to be valuable.

**Products:** Our project will produce an open-source R software package to assist applied researchers in using the methods that we find to be useful, as well as R notebooks that allow researchers to replicate our analyses. In addition to the R package, the proposed project will produce

-   a research brief that will provide guidance to applied researchers in education and other social science disciplines about the value and limitations of ML methods for identifying subgroup variation, based on empirical evidence produced by our project, as well as recommendations about when and how the methods can be pre-specified in analysis plans and implemented in practice;
-   a methodological paper submitted to a peer-reviewed journal, which will provide the opportunity for rigorous review of our implementation of existing methods and of advancements we make;
-   at least one presentation at a conference attended by evaluation researchers in education and possibly other domains; and
-   a webinar for applied researchers that will walk them through our applied findings and how to implement the use of ML methods for investigating treatment effect heterogeneity.

**Use in Applied Education Research:** If ML approaches can perform as well or better than existing standards for pre-specification of subgroup analyses, then they may be preferred over current practice. Rather than relying on pre-specifying in an analysis plan a small number of baseline characteristics - which may not be the right ones - researchers can pre-specify the methodology for assessing heterogeneity, allowing the findings to be data-driven but not "fishing."

## Set Up & Prerequisites for Collaborators

-   Decide where you will store the project on your computer. Ex. `"/path/to/file/on/collaborators/machine/"`
-   Clone the repository. Ex. `"/path/to/file/on/collaborators/machine/MLIV"`
-   Outside of the cloned repository folder, create a folder called "Fake", where the fake data will be stored. Ex. `"/path/to/file/on/collaborators/machine/Fake"`
-   Impute your path to fake data in vectors in `R/configs.R`
-   Install all the packages in `R\packages.R` to avoid package errors

# Folders

The following are key folders in the repository:

-   `R` consists of helper R scripts that load packages and set data directories
-   `CA` consists of R Markdowns where we load, clean, subset, and save Career Academies Data
-   `ASAP` consists of R Markdowns where we load, clean, subset, and save ASAP Data
-   `archive` consists of R programs that are our backups copies of some relevant code
-   `synthpop_copula` consists of R Markdowns that explore the Synthpop package and develop the Synthpop + Copula approach
-   `simulation_pipeline` is our MAIN folder that consists of R scripts that create initial fake data, load all data, create fake data for simulation, conduct a simulation study


# Where results are stored

- `results` - The folder where many of our simulation results are stored.
  - `predicted_IATEs` - This is a place where partial simulation files are stored.  Each chunk in the parallel code is saved as a separate file.  This is a cache that can be deleted without loss of information.
  - `combined_IATEs_by_queen` - Stacked the fragment files by queen.  Built from `predicted_IATEs`.
  - `aggregated_IATEs` - The simulation iterations are summarized to get performance for each test point for each model.  Built from `combined_IATEs_by_queen`
  - `logs` - Each simulation call is logged.
  
  

# Getting started

-   Complete all in `Set Up & Prerequisites for Collaborators`.
-   Try running `02_MLIV_make_data.R`.
-   If works, you are all set.
-   If produces an error, write an email to David.
-   If you want to learn more about functions created, read `MLIV_DM.txt`
