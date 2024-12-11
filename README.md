# Lancaster University Energy Usage Ranking

 This dashboard benchmarks the different sources used in the Lancaster University buildings. Given a data range chosen, an anomaly score is assigned to each building using a nearest-neighbor approach. In brief, an anomalous observation will have large distances compared with the rest of the observations, particularly with its nearest neighbors. The anomaly score is the sum of the distances of an observation with its nearest neighbors.  The options and parameters provided are meant to explore different perspectives and explain the behavior of source usage at the University.

The left sidebar contains the different options to compute the analysis: 
- **Download**: Download the computed scores, total source usage, and building information in CSV format. 
- **Source supply**: Choose the meter type among electricity, water, gas, and heat records. 
- **Date Range**:  Select the initial and final dates to extract the meter records. 
- **Standardization**: The **Standard deviation scaling** checkbox triggers the scale of each time series, dividing the observations by their empirical standard deviations. The **Mean subtraction** triggers to center the time series by subtracting their mean.
- **Distance**: Select the checkbox of the distances you want to include in the analysis. The distances, Cort, Wasserstein, CortNorm, Coherence, mvLWS, and Band depth, have a low computation time. Meanwhile, the PDC, CGCI, RGPDC, and PMIME have high computational time because these distances are computed based on parametric models. 
- **Node highlight**: Select and highlight the bottom node in the regression tree, and then only the buildings in that node are plotted.  When the value is 0, the regression tree is displayed fully, and all records are plotted in the bottom graph.  
- **Tree depth**: The regression tree can be simple, with few partitions, or complex with several partitions. This input controls the maximum depth of the tree using the most significant explaining factors. The complexity is limited to using only significant explaining factors.   
- **Explaining factors**: These checkboxes select the building information used in the rules to build the regression tree. When all checkboxes are unselected, the tree panel will show an error. 

The **Parameters** tab allows you to change each distance's parameters. The distances that do not appear on this tab do not require further specifications for its computation. 

This tool was supported by the UK Research and Innovation (UKRI) Engineering and Physical Sciences Research Council (EPSRC) under the project titled "Reducing End Use Energy Demand in Commercial Settings Through Digital Innovation," grant number EP/T025964/1.
