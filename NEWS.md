# mvdalab 1.1

* Added a `NEWS.md` file to track changes to the package.

* Changed order of factor leves in PE function from factor(df$Type, levels(df$Type)[c(2, 1)]) to 
* factor(df$Type, levels(df$Type)[c(1, 2)]) - NLA

* Corrected %Variation Explained in PE function for PLS models - NLA

* Corrected BCa CI formula - NLA

* Changed color scheme to black for many of the plots in order to avoid confusion - NLA

* Added functions for plusminusFit - RB and NLA

* Added the ability to change the cut-off value on the VIP analysis and VIP plot

* Added to smc summary and plot if the results are corrected for auto-correlation

# mvdalab 1.2

* Updated cross-validation procedure for PCA

* Added 'wrtpls' algorithm to plsFit function

* Change font sizes throughout for graphs

# mvdalab 1.3

* Bug fixes

# mvdalab 1.4

* Bug fixes

# mvdalab 1.5

* Removed contrast argument from plsFit.
* Bug fixes

# mvdalab 1.6

* Added 'parallel' as optional for plsFit.
* Bug fixes