
<h4 align = "right">Adriano de Bernardi Schneider, Ph.D.<br> Colby T. Ford, Ph.D.<br>John Williams<br> Mike Cioce<br>Daniel Janies, Ph.D.</h3>

# StrainHub
Strainhub is designed as a web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and a metadata associated file. The software maps the metadata onto the tree and performs a parsimony ancestry reconstruction step to create links between the associated metadata and enable the construction of the network.

![StrainHub CHIKV Network](https://github.com/supramap/transmission_graphs/raw/master/chikv_StrainHub_network.png "Sample Chikungunya Virus Network")


## Use StrainHub Online

You'll have the option of either running a parsimony reconstruction on your phylogenetic tree, and for that you will need 2 files to get started:

1) A phylogenetic tree formatted in Newick tree format generated through your preferred phylogenetic search method (e.g. BEAST, TNT, RAxML, IQTree).

2) A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g. host, country, risk group)

OR

You'll run phylogeography using BEAST and for that you will only need 1 file to get started:

1) A maximum clade credibility tree (MCC) generated through Phylogeographic Diffusion in [Discrete](http://beast.community/workshop_discrete_diffusion) or [Continuous](http://beast.community/workshop_continuous_diffusion) Space in BEAST.

For more information, click [here](ABOUT.md).

[Try Out StrainHub Online](https://colbyford.shinyapps.io/strainhub/)

## Run StrainHub Locally
To run, download the repository and open the `strainhub/app.R` file and run the following script.
```r
library(shiny)
runApp()
```

## References

Adriano de Bernardi Schneider, Arboviruses: The Hidden Path of an Imminent Threat, University of North Carolina at Charlotte, 2018, ProQuest (10979260). ISBN: 9780438668379 (https://pqdtopen.proquest.com/pubnum/10979260.html)
