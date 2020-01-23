<p align="left">
  <img src="https://github.com/abschneider/StrainHub/blob/beta/Tree2.png" alt="StrainHub logo" width="350"/>
</p>

[![Version: 1.0.7](https://img.shields.io/badge/version-1.0.7-green.svg)](UPDATES.md)
[![Version: 1.0.7](https://img.shields.io/github/issues/abschneider/StrainHub.svg)](https://github.com/abschneider/StrainHub/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

StrainHub is designed as a web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and associated metadata. The software maps the metadata onto the tree and performs a parsimony ancestry reconstruction step to create links between the associated metadata and enable the construction of the network. Users have the option to build a tree utilizing their method of preference outside StrainHub or build a tree utilizing a FASTA file within StrainHub with the Neighbor-Joining algorithm. Alternatively, the user can skip the StrainHub ancestry reconstruction step by generating a maximum clade credibility tree (MCC) through BEAST phylogeography or input a previously generated list of edges in order to build the transmission network. Additionally, the user can input a file with geographic cooordinates associated with the character of interest and have the network plotted into a map.

<p align="center">
  <img src="https://github.com/abschneider/StrainHub/blob/master/host_network_example.png" alt="Sample Host Transmission Network" width="350"/>
</p>


## Use StrainHub Online

StrainHub currently offers four methods to enable the visualization of transmission networks, each with different file format requirements:

**Parsimony**

You'll have the option of running a parsimony reconstruction on your phylogenetic tree, and for that you will need 2 files to get started:

1) A phylogenetic tree formatted in Newick tree format generated through your preferred phylogenetic search method (e.g., BEAST, TNT, RAxML, IQTree).

2) A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g., host, country, risk group)

**BEAST Phylogeography**

You'll run phylogeography using BEAST and for that you will only need 1 file to get started:

1) A maximum clade credibility tree (MCC) generated through Phylogeographic Diffusion in [Discrete](http://beast.community/workshop_discrete_diffusion) or [Continuous](http://beast.community/workshop_continuous_diffusion) Space in BEAST.

**Quick Tree**

You'll create a tree directly from your alignment using a NJ algorithm within StrainHub and run parsimony reconstruction on the given tree. You will need 2 files and to know your outgroup sequence:

1) An alignment file in FASTA format generated through your preferred alignment method (e.g., MAFFT, Geneious, Clustal, MUSCLE).

2)  A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g., host, country, risk group)

**List of Edges**

You'll create a network directly from a list of edges previously generated in another software, you will need only 1 file:

1) List of edges in a CSV formatted file.

**Geographic Coordinates**

Additionally, if you want to plot your transmission network on a map you will need 1 additional file that does not vary between the methods:

1) Geographic coordinates for each character of interest in a CSV formatted file.


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

Please cite the reference below if you are using StrainHub.

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

Additionally, if you use the Map feature of StrainHub please also cite the reference below:

Text Citation:

```
Schneider, A.D.B., Ochsenreiter, R., Hostager, R., Hofacker, I.L., Janies, D. and Wolfinger, M.T., 2019. Updated Phylogeny of Chikungunya Virus Suggests Lineage-Specific RNA Architecture. Viruses, 11(9), p.798.
```

BibTex Citation:

```
@article{schneider2019updated,
  title={Updated Phylogeny of Chikungunya Virus Suggests Lineage-Specific RNA Architecture},
  author={de Bernardi Schneider, Adriano and Ochsenreiter, Roman and Hostager, Reilly and Hofacker, Ivo L and Janies, Daniel and Wolfinger, Michael T},
  journal={Viruses},
  volume={11},
  number={9},
  pages={798},
  year={2019},
  publisher={Multidisciplinary Digital Publishing Institute}
}
```
