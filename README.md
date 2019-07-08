# StrainHub

[![Version: 1.0.1](https://img.shields.io/badge/version-1.0.0-green.)](UPDATES.md)
[![Version: 1.0.0](https://img.shields.io/github/issues/abschneider/StrainHub.svg)](https://github.com/abschneider/StrainHub/issues)
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

## How To Cite

Text Citation:

```
Adriano de Bernardi Schneider, Colby T Ford, Reilly Hostager, John Williams, Michael Cioce, Ümit V. Çatalyürek, Joel O Wertheim, Daniel Janies
bioRxiv 650283; doi: https://doi.org/10.1101/650283
```

BibTex Citation:

```
@article {de Bernardi Schneider650283,
    author = {de Bernardi Schneider, Adriano and Ford, Colby T and Hostager, Reilly and Williams, John and Cioce, Michael and {\c C}ataly{\"u}rek, {\"U}mit V. and Wertheim, Joel O and Janies, Daniel},
    title = {StrainHub: A phylogenetic tool to construct pathogen transmission networks},
    elocation-id = {650283},
    year = {2019},
    doi = {10.1101/650283},
    publisher = {Cold Spring Harbor Laboratory},
    abstract = {In epidemiology of infectious diseases, networks have been used to reconstruct contacts among patients. Summarizing networks using pathogen metadata (e.g., host species and place of isolation) and a phylogenetic tree is a nascent approach. In this paper, we introduce a tool for reconstructing transmission networks in arbitrary space from phylogenetic information and metadata. Our goals are to provide a means of deriving new insights and infection control strategies based on the dynamics of the pathogen derived from networks and metrics. We created a web-based application, called StrainHub, in which a user can input a phylogenetic tree along with characters derived from metadata using their preferred tree search method. StrainHub generates a transmission network for the user metadata based on character state changes mapped on the phylogenetic tree. The user has the option to calculate centrality metrics on the nodes including betweenness, closeness, degree, and a new metric, source/hub ratio. The outputs include the network with values for metrics on its nodes and the tree with characters reconstructed. All of these results can be exported for further analysis.},
    URL = {https://www.biorxiv.org/content/early/2019/05/31/650283},
    eprint = {https://www.biorxiv.org/content/early/2019/05/31/650283.full.pdf},
    journal = {bioRxiv}
}
```
