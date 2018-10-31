This repository contains the data and code necessary to reproduce the
analysis in the manuscript “Modeling spatiotemporal abundance of mobile
wildlife in highly variable environments using boosted GAMLSS hurdle
models” by Smith et al. (accepted at *Ecology and Evolution*).

Authors (affiliation):  
- Adam D. Smith<sup>1</sup> (Department of Natural Resources Science,
University of Rhode Island, Kingston, Rhode Island 02881 USA)  
- Benjamin Hofner<sup>2</sup> (Department of Medical Informatics,
Biometry and Epidemiology, Friedrich-Alexander-University
Erlangen-Nuremberg, Waldstraße 6, 91054 Erlangen, Germany)  
- Juliet S. Lamb (Department of Natural Resources Science, University of
Rhode Island, Kingston, Rhode Island 02881 USA), - Jason E. Osenkowski
(Rhode Island Department of Environmental Management, 277 Great Neck
Road, West Kingston, Rhode Island 02892 USA)  
- Taber Allison (American Wind Wildlife Institute, 1110 Vermont Avenue,
NW, Suite 950, Washington, D.C. 20005 USA)  
- Giancarlo Sadoti (Department of Geography, University of Nevada, Reno,
Nevada, 89557 USA)  
- Scott R. McWilliams (Department of Natural Resources Science,
University of Rhode Island, Kingston, Rhode Island 02881 USA)  
- Peter W. C. Paton (Department of Natural Resources Science, University
of Rhode Island, Kingston, Rhode Island 02881 USA)

<sup>1</sup>Present address: United States Fish and Wildlife Service,
National Wildlife Refuge System, Inventory and Monitoring Branch, 135
Phoenix Rd., Athens, GA 30605 USA <sup>2</sup>Present address: Section
Biostatistics, Paul-Ehrlich-Institut, Paul-Ehrlich-Straße 51-59, 63225
Langen, Germany

To run the code and compile the manuscript and supplemental materials,
you’ll need:

1.  **[R](https://www.r-project.org/)**
2.  **[RStudio](https://www.rstudio.com/products/rstudio/download/preview/)**
    is highly recommended, though not technically required.
3.  **LaTeX distribution**: If you don’t know what this means, we
    recommend you use `tinytex`. Install `tinytex` in R using the
    following command and following the onscreen prompts:

        install.packages("tinytex")
        tinytex::install_tinytex()

    Other LaTeX distribution options include
    [MiKTeX](http://miktex.org/download) for Windows,
    [MacTeX](https://tug.org/mactex/mactex-download.html) for Mac OS X,
    and
    [TeXLive](https://www.tug.org/texlive/doc/texlive-en/texlive-en.html#x1-140003)
    for Linux.

4.  **pandoc**: pandoc is installed if you’re using a relatively recent
    version of RStudio.

Compiling the manuscript and supplemental materials
---------------------------------------------------

Provided you have the above software installed, you can compile and
generate the manuscript and supplemental material pdfs by opening and
`knit`ting the following files:

1.  `Manuscript/NanSound_main_document.Rmd`
2.  `Manuscript/S1_StabilitySelection.Rmd`
3.  `Manuscript/S2_EarlyStopping.Rmd`
4.  `Manuscript/S3_StableCovariateEffects.Rmd`
5.  `Manuscript/S4_ConditionalOverdispersion.Rmd`
6.  `Manuscript/S5_SeasonalAnimation.Rmd`

The Supplemental `Rmd` files assume the main manuscript `Rmd` was `knit`
first.

We also include a document that illustrates how to visualize the
spatiotemporal covariates used in this study:
`Manuscript/Visualizing_covariates.Rmd`. It can be `knit` equivalently
to the Supplemental material.

Rerunning the analysis
----------------------

Given the computing power and time necessary to recreate the analysis,
the analysis code is maintained separately of manuscript compilation.
The relevant files to recreate the analysis are:

1.  `R/create_final_data.R`: assembles the final data set (duck counts
    and environmental variables) for analysis
2.  `R/model_coei.R`, `R/model_coei.R`, and `R/model_coei.R`: for each
    species, slightly restructure the data for analysis and conduct the
    boosted logistic GAM for occupancy and GAMLSS hurdle model for
    abundance, including early stopping and stability selection
3.  `R/model_formula_decomp.R`: this simply sets up the general form
    (i.e., formulas) of the models (see Materials and Methods -
    Synthesis section of manuscript) and is called by the modelling
    functions.

**NOTE**: The *large* resulting model files for each species group are
stored in the `Results_coei`, `Results_scot`, and `Results_ltdu`
directories. These files are necessary for compiling the manuscript and
supplemental material and will be overwritten if `R/model_****.R` is
run.
