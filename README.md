# StrainHub
[![Version: 1.0.2](https://img.shields.io/badge/version-1.0.2-green.svg)](UPDATES.md)
[![Version: 1.0.2](https://img.shields.io/github/issues/abschneider/StrainHub.svg)](https://github.com/abschneider/StrainHub/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Strainhub is designed as a web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and associated metadata. The software maps the metadata onto the tree and performs a parsimony ancestry reconstruction step to create links between the associated metadata and enable the construction of the network. Users also have the option to skip the StrainHub ancestry reconstruction step by generating a maximum clade credibility tree (MCC) through BEAST phylogeography.

<p align="center">
  <img src="https://github.com/abschneider/StrainHub/blob/master/host_network_example.png" alt="Sample Host Transmission Network" width="350"/>
</p>


## Use StrainHub Online

You'll have the option of either running a parsimony reconstruction on your phylogenetic tree, and for that you will need 2 files to get started:

1) A phylogenetic tree formatted in Newick tree format generated through your preferred phylogenetic search method (e.g., BEAST, TNT, RAxML, IQTree).

2) A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g., host, country, risk group)

OR

You'll run phylogeography using BEAST and for that you will only need 1 file to get started:

1) A maximum clade credibility tree (MCC) generated through Phylogeographic Diffusion in [Discrete](http://beast.community/workshop_discrete_diffusion) or [Continuous](http://beast.community/workshop_continuous_diffusion) Space in BEAST.

For more information, click [here](ABOUT.md).

[Try Out StrainHub Online](http://strainhub.io)

## Run StrainHub Locally
To run, download the repository and open the `strainhub/app.R` file and run the following script.
```r
library(shiny)
runApp()
```
## Authors

<h4 align = "left">Adriano de Bernardi Schneider, Ph.D.<br>Colby T. Ford, Ph.D.<br>Reilly Hostager<br>John Williams<br>Michael Cioce<br>Ümit V. Çatalyürek, Ph.D.<br>Joel O. Wertheim, Ph.D.<br>Daniel Janies, Ph.D.</h4>

## References

Adriano de Bernardi Schneider, Arboviruses: The Hidden Path of an Imminent Threat, University of North Carolina at Charlotte, 2018, ProQuest (10979260). ISBN: 9780438668379 (https://pqdtopen.proquest.com/pubnum/10979260.html)
