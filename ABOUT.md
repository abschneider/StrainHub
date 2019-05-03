# StrainHub
<details>
<summary><strong><em>Table of Contents</em></strong></summary>

* [About StrainHub](#about-strainhub)
* [Getting Started](#getting-started)
  - [Example Files](#example-files)
* [Network Quality](#network-quality)
* [How To Cite](#how-to-cite)
* [References](#references)
* [Contact](#contact)

</details>

About StrainHub
-----------------

StrainHub is designed as a web-based software to generate pathogen transmission networks and associated metrics from a combination of a phylogenetic tree and a metadata associated. The software enables the construction of the network by either  mapping the metadata onto the tree and performing a parsimony ancestry reconstruction step to create links between the associated metadata or parsing a BEAST phylogeography maximum clade credibility tree.

Getting Started
-----------------

In order to construct the disease transmission network the user is presented with two options, run the entire pipeline or skip straight to the visualization part. 

To run the entire pipeline, two files will need to be generated.

1) A phylogenetic tree formatted in Newick tree format generated through your preferred phylogenetic search method (e.g., BEAST, TNT, RAxML, IQTree).

2) A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g., host, country, risk group) 
 
_Taxa ID with missing data should be excluded prior to the analysis. Order does not matter on both files, the metadata is sorted automatically before mapped into the tree based on the header of the first column of the CSV file (Accession)._

A template for building the metadata file can be downloaded [here](https://raw.githubusercontent.com/abschneider/StrainHub/master/data/example_metadata.csv). Do not change the header of the Accession column as it is necessary to identify and reorder the metadata according to the tree file.

### Example files:
- [Phylogenetic Tree](https://raw.githubusercontent.com/abschneider/StrainHub/master/data/example_tree.phy)
- [Metadata File](https://raw.githubusercontent.com/abschneider/StrainHub/master/data/example_metadata.csv) 

To skip the ancestry reconstruction step, the user will have to run BEAST phylogeography, and then one file will need to be generated.

1) A maximum clade credibility tree (MCC) generated through Phylogeographic Diffusion in [Discrete](http://beast.community/workshop_discrete_diffusion) or [Continuous](http://beast.community/workshop_continuous_diffusion) Space in BEAST in NEXUS format.

Our app will parse the MCC tree and prompt the user to select which trait he wants to evaluate. Once the user has selected the trait, the user will have to select the trait probability threshold the user wants to use to consider the relationship between two nodes valid (we like 0.9 as a threshold, but this will depend on the user and his knowledge about his dataset).

_The threshold set by the user is utilized to filter out any pair of nodes within the phylogenetic tree that may have lower probability, thus being ignored within the transmission network step._ 

### Example file:
- [MCC Tree](https://raw.githubusercontent.com/abschneider/StrainHub/master/data/batRABV.mcc.tree)


Metrics
-----------------

The nodes within the network are scaled based on the user selected metric. The current metrics available on StrainHub are:

- __Betweenness Centrality__

	Betweenness centrality of node i is the sum of all the ratios between all the paths g from node j to node k that go through node i over all possible paths from node j to node k.

	Epidemiological meaning: How important a location/host is as the shortest path / intermediary connecting other locations/hosts within the transmission network.

- __Closeness Centrality__

	Closeness centrality is the inverse of the average distance between pk and the other nodes.

	Epidemiological meaning: How important a location/host is given its distance within the transmission network to other locations/hosts.

- __Degree Centrality__

	Degree centrality of node k is the sum of all links of other nodes to node k over the total number of nodes – 1.

	Epidemiological meaning: How important a location/host is within the transmission network given the number of times that a disease emerges from or to that point (indegree or outdegree).

	Degree Centrality is also subdivided in Indegree and Outdegree Centrality, which calculate the links directed only in or out of the note, respectively.

- __Source Hub Ratio (SHR)__

	The Source Hub Ratio of a node i is the sum of all shifts from location “i” to other locations over the sum of all shifts from and to location ”i”.

	Epidemiological meaning: How important a node is within the network as source of the disease, ignoring centrality within network.

		1 = all shifts originated on node i (source)

		0.5 = same amount of shifts from and to node i (hub)

		0 = all shifts occurred to node i (dead end)


_Although only one metric can be selected for scaling the nodes within the network, all the metrics are calculated and can be visualized as well as downloaded on the tab "Metrics"._

Network Quality
------------------------

The overall quality of the transmission network generated on StrainHub rests on the quality of the user input data.

How To Cite
------------------------
_Citation Coming Soon..._


Authors
------------------------

<h4 align = "left">Adriano de Bernardi Schneider, Ph.D.<br>Colby T. Ford, Ph.D.<br>Reilly Hostager<br>John Williams<br> Michael Cioce<br>Ümit V. Çatalyürek, Ph.D.<br>Joel O. Wertheim, Ph.D.<br>Daniel Janies, Ph.D.</h3>

References
------------------------

[Adriano de Bernardi Schneider, _Arboviruses: The Hidden Path of an Imminent Threat_, University of North Carolina at Charlotte, 2018, ProQuest (10979260). ISBN: 9780438668379](https://pqdtopen.proquest.com/pubnum/10979260.html)

Contact
------------------------
Questions? Comments? Contact us [here](mailto:adebernardischneider@ucsd.edu).
