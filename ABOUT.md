StrainHub
-----------------

Strainhub is designed as a web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and a metadata associated file. The software maps the metadata onto the tree and performs a parsimony ancestry reconstruction step to create links between the associated metadata and enable the construction of the network. 

Getting Started
-----------------

In order to construct the disease transmission network two files will need to be generated.

1) A phylogenetic tree formatted in Newick tree format generated through your preferred phylogenetic search method (e.g. BEAST, TNT, RAxML, IQTree).

2) A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g. host, country, risk group) 
 
_Taxa ID with missing data should be excluded prior to the analysis. Order does not matter on both files, the metadata is sorted automatically before mapped into the tree based on the header of the first column of the CSV file (Accession)._

A template for building the metadata file can be downloaded [here](https://github.com/supramap/transmission_graphs/raw/master/data/template.csv). Do not change the header of the Accession column as it is necessary to identify and reorder the metadata according to the tree file.

__Example files:__
- [Phylogenetic Tree](https://github.com/supramap/transmission_graphs/raw/master/data/chikv_westernafrica.phy)
- [Metadata File](https://github.com/supramap/transmission_graphs/raw/master/data/chikv_westernafrica_metadata.csv) 

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

Quality of the Network
------------------------

The overall quality of the transmission network generated on StrainHub rests on the quality of the user input data.

References
------------------------

[Adriano de Bernardi Schneider, _Arboviruses: The Hidden Path of an Imminent Threat_, University of North Carolina at Charlotte, 2018, ProQuest (10979260). ISBN: 9780438668379](https://pqdtopen.proquest.com/pubnum/10979260.html)
