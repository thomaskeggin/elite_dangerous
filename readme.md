# Elite Dangerous
For playing about with data from Elite Dangerous.

Tree calculated from the scaled base stats of each ship.
Ship-ship distances are the Euclidean distances based on ship stats, which were then used to hierarchically cluster the ships. I then used kmeans to find 12 functional groups (number of groups based on me playing the game too much and eye-balling the general role of each ship).
!["Attribute-based distance tree for the ships.](https://github.com/TKeggin/elite_dangerous/blob/main/plot_readme/ship_tree.jpg)

Using the scaled bases stats of each ship, calculated a simple PCA to decompose the base stats of the different ships to hopefully show their general roles a bit better.
![PCA of ships based on their attributes.](https://github.com/TKeggin/elite_dangerous/blob/main/plot_readme/ship_pca.jpg)

Love me a barplot, just wanted to highlight the strengths of each functional group. Hopefully by now it should be easier to pick out a subset of ships to compare for a particular role. E.g., Group 4 make good explorers given their high jump range, speed, agility - but they are generally rubbish at everything else.
![Barplot of mean group attributes.](https://github.com/TKeggin/elite_dangerous/blob/main/plot_readme/ship_niche.jpg)

