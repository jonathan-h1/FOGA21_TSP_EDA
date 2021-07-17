## Welcome ...

... to our dashboard! Within this application, you can explore the effects of the traveling salesperson (TSP) feature normalization. Feel free to generate different plots and toy around with varying sets of instances and sizes. In case of any questions, please do not hesitate to contact us. You find our contact information on the first page of our paper. 


###  Datasets and Instances 

On the left-hand side, you will always find a Panel. Here, you can choose which instance types and sizes should be currently displayed and adjust the axis of the plots. First, you can select which algorithm should be displayed, whose performance will be related to the feature values. In our dashboard, we included the three most important algorithms for the TSP: the exact Concorde algorithm (Applegate et al. 2006), the Lin-Kernighan-Helsgaun heuristic – LKH – (Lin, Kernighan 1973; Helsgaun 2009) as well as Edge Assembly Crossover – EAX (Nagata, Kobayashi 1997).  
  
Below, you find different performance measures which we used to evaluate the three algorithms. They are based on the Penalized Average Runtime – PAR10 – and Penalized Quantile Runtime – PQR10 – values.  
  
Next, you can use the checkboxes to display different instances or different instance sets. The other instance types are summarized in the groups below. The ECJ instances include all instances presented by Kerschke et al. in their paper, while the FOGA instances are used by Bossek et al. (Kerschke et al. 2018; Bossek et al. 2019). Finally, the last instance sets stem from Seiler et al. and include all the instances with the term "evolved" in their name. 
Lastly, you can choose different instance sizes. Most evolved or artificially generated instanced have fixed instance sizes in the interval [500, 2000]. Activate the checkbox "other" to display the unstandardized instances sizes of the national and TSPlib instances. 


### Detailed Data Table 
In the next tab, you find a data table that displays all different TSP feature values. In the first columns, you see the instance root sets, their respective group, and the name of the instance. Then, most importantly, you can filter for normalized or unnormalized feature values. In the following columns, you find all features and their respective values. Use the search in the top-right corner 
to browse for specific terms. 


### Feature Scatterplots 
In this tab, scatterplots of two features are generated. Choose which features are displayed by selecting them from the drop-down list labeled "X" and "Y," respectively. On the right-hand side, you can activate checkboxes: the first one colors the instances according to their size. The second one displays the simple normalization of Pihera and Musliu below (Pihera, Musliu 2014).  
  
Below, you find the generated plots. On the left-hand side, the normalized features are displayed. On the right-hand side, you see the unnormalized features (or the normalization conducted by Pihera and Musliu). If you did not activate the checkbox "Color instances according to measure" above, the features are colored according to the algorithm's performances. You can choose the performance measure as well as the algorithm in the side panel.  
  
Finally, if you mark instances in either plot, the data table on the bottom of the dashboard is filled with data of the respective features. 


### Feature Correlation Plots 
In the next tab, you can find heatmaps displaying the correlation among the features themselves. We used Spearman's rank correlation coefficient. Above, you see the heatmap for the normalized features and below for their unnormalized counterparts. Red indicated a high correlation, blue a low value, and white displayed correlation values around 0. On the border of the plots, you find dendrograms of hierarchical clustering solutions. Feel free to adjust the number of clusters by using the scroll bar on top of the page. 


### Algorithm Performance Correlation Plots
In the final tab, you are able to explore the correlation of the features with the performance of the algorithms. Choose one algorithm in the side panel and the second algorithm in the tab itself. If you want to facilitate comparing the normalized feature correlation on the left with the unnormalized feature correlation on the right, activate the checkbox "Show change of position." Then, arrows will indicate how the values changed due to the normalization. They will point from their original position in the unnormalized plot to their new position in the normalized graph. Adjust the number of arrows displayed by using the scroll bar.  
  
Again, feel free to brush over the points in the plots to fill the data tables below with the respective feature values. 

### References 

David L. Applegate, Robert E. Bixby, Vasek Chvátal, and William J. Cook. 2006. The Traveling Salesman Problem. Princeton University Press.

Jakob Bossek, Pascal Kerschke, Aneta Neumann, Markus Wagner, Frank Neumann, and Heike Trautmann. 2019. Evolving Diverse TSP Instances by Means of Novel and Creative Mutation Operators. In Proceedings of the 15th ACM/SIGEVO Conference on Foundations of Genetic Algorithms (Potsdam, Germany) (FOGA ’19). Association for Computing Machinery, 58–71. https://doi.org/10.1145/3299904.3340307

Keld Helsgaun. 2009. General k-Opt Submoves for the Lin–Kernighan TSP Heuristic. Mathematical Programming Computation 1 (10 2009), 119–163. https://doi.org/10.1007/s12532-009-0004-6

Pascal Kerschke, Lars Kotthoff, Jakob Bossek, Holger Hoos, and Heike Trautmann. 2018. Leveraging TSP Solver Complementarity through Machine Learning. Evolutionary Computation 26, 4 (2018), 597 – 620. https://doi.org/10.1162/evco_a_00215

Shen Lin and Brian W. Kernighan. 1973. An Effective Heuristic Algorithm for the Traveling-Salesman Problem. Operations Research 21, 2 (04 1973), 498–516. https://doi.org/10.1287/opre.21.2.498

Yuichi Nagata and Shigenobu Kobayashi. 1997. Edge Assembly Crossover: A High-power Genetic Algorithm for the Traveling Salesman Problem. In Proceedings of the Seventh International Conference on Genetic Algorithms (ICGA 1997). Morgan-Kaufmann, San Francisco, CA, 450–457. https://doi.org/10.1287/ijoc.1120.0506

Josef Pihera and Nysret Musliu. 2014. Application of Machine Learning to Algorithm Selection for TSP. In Proceedings of the 2014 IEEE 26th International Conference on Tools with Artificial Intelligence (ICTAI ’14). IEEE Computer Society, USA, 47 – 54. https://doi.org/10.1109/ICTAI.2014.18

Moritz V. Seiler, Janina S. Pohl, Jakob Bossek, Pascal Kerschke, Heike Trautmann. 2020. Deep Learning as a Competitive Feature-Free Approach for Automated Algorithm Selection on the Traveling Salesperson Problem. In: Bäck T. et al. (eds) Parallel Problem Solving from Nature – PPSN XVI. PPSN 2020. Lecture Notes in Computer Science, vol 12269. Springer, Cham. https://doi.org/10.1007/978-3-030-58112-1_4
