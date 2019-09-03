<h1 style="text-align: center;"><span style="background-color: #2c3e50; color: #ffffff; font-family: 'Lato', sans-serif;"> StrainHub </span></h1>

[![Version: 1.0.1](https://img.shields.io/badge/version-1.0.0-green.svg)](UPDATES.md)
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

1) A maximum clade credibility tree (MCC) generated through Phylogeographic Diffusion in [Discrete](http://beast.community/workshop_discrete_diffusion) or [Continuous](http://beast.community/workshop_continuous_diffusion) Space in BEAST or Phylogeography in BEAST2 using [Mascot](https://taming-the-beast.org/tutorials/Mascot-Tutorial/).

For more information, click [here](ABOUT.md).

[![StrainHub.io](https://img.shields.io/badge/Try%20Out%20StrainHub%20Online-StrainHub.io-blue.svg?logo=r&style=for-the-badge&labelColor=2C3E50&color=3498DB)](https://strainhub.io)

## Run StrainHub Locally

To run, download the repository and open the `strainhub/app.R` file and run the following script.

```r
library(shiny)
runApp()
```

If you need to install all the required packages to run StrainHub, you can do so by running the `strainhub/install_packages.R` script.


## Authors

<h4 align = "left">Adriano de Bernardi Schneider, Ph.D.<br>Colby T. Ford, Ph.D.<br>Reilly Hostager<br>John Williams<br>Michael Cioce<br>Ümit V. Çatalyürek, Ph.D.<br>Joel O. Wertheim, Ph.D.<br>Daniel Janies, Ph.D.</h4>

## How To Cite

Text Citation:

```
Adriano de Bernardi Schneider, Colby T Ford, Reilly Hostager, John Williams, Michael Cioce, Ümit V Çatalyürek, Joel O Wertheim, Daniel Janies, StrainHub: A phylogenetic tool to construct pathogen transmission networks, Bioinformatics, btz646, https://doi.org/10.1093/bioinformatics/btz646
```

BibTex Citation:

```
@article{10.1093/bioinformatics/btz646,
    author = {de Bernardi Schneider, Adriano and Ford, Colby T and Hostager, Reilly and Williams, John and Cioce, Michael and Çatalyürek, Ümit V and Wertheim, Joel O and Janies, Daniel},
    title = "{StrainHub: A phylogenetic tool to construct pathogen transmission networks}",
    journal = {Bioinformatics},
    year = {2019},
    month = {08},
    abstract = "{In exploring the epidemiology of infectious diseases, networks have been used to reconstruct contacts among individuals and/or populations. Summarizing networks using pathogen metadata (e.g., host species and place of isolation) and a phylogenetic tree is a nascent, alternative approach. In this paper, we introduce a tool for reconstructing transmission networks in arbitrary space from phylogenetic information and metadata. Our goals are to provide a means of deriving new insights and infection control strategies based on the dynamics of the pathogen lineages derived from networks and centrality metrics. We created a web-based application, called StrainHub, in which a user can input a phylogenetic tree based on genetic or other data along with characters derived from metadata using their preferred tree search method. StrainHub generates a transmission network based on character state changes in metadata, such as place or source of isolation, mapped on the phylogenetic tree. The user has the option to calculate centrality metrics on the nodes including betweenness, closeness, degree, and a new metric, the source/hub ratio. The outputs include the network with values for metrics on its nodes and the tree with characters reconstructed. All of these results can be exported for further analysis.strainhub.io and https://github.com/abschneider/StrainHub}",
    issn = {1367-4803},
    doi = {10.1093/bioinformatics/btz646},
    url = {https://doi.org/10.1093/bioinformatics/btz646},
    eprint = {http://oup.prod.sis.lan/bioinformatics/advance-article-pdf/doi/10.1093/bioinformatics/btz646/29171171/btz646.pdf},
}
```
