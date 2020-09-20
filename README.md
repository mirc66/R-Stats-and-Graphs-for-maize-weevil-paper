# R-Stats-and-Graphs-for-maize-weevil-paper
Data wrangling, statistical analysis and plots for publication Attraction-of-Sitophilus-zeamais-Coleoptera-Curculionidae-to-four-host-plants.

The code sections include the output of the run program  commented out.

In this experiments maize weevils were reared in 4 different grain substrates: Barley, Maize, Rice, Milo.  The weevils were then placed in an arena with 4 plants to choose from:Milo, Maize, Oats and Rice.   The weevels present on the leaf,root and soil of each plant were counted as well as the ones located loose in the arena.  The location of the plants in the arena were rotated so  location would not be a cause of the selection.

9_20_20WeevilbyLocAllGit.R:  analyzes the data by location of the plant in the arena.  This data met requirements for Anova and Tukey 
1_23_20KruskalWallis by rearing and plant location.R: anlayzes the data by rearing(grain) and plant selected.  This data did not meet normality so non-parametric methods used like Kurskal Wallis.
