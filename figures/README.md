# Figure Description

These figures are generated from the raw CSV files found in the "rawdata" folder. Note that only wild-type thrombi with no truncations in the image stacks are used. In other words, we use the following files: 

- 6.21.17 WT1 1 min.csv
- 4.18.19 WT5 1 min no-p.csv
- 2.11.19 WT4 1 min p.csv
- 9.18.18 WT2 5 min.csv
- 2.11.19 WT6 5 min no-p.csv
- 2.11.19 WT5 5 min no-p.csv
- 12.13.18 WT4 5 min.csv
- 7.30.19 S2 20 min.csv
- 8.27.19 S4 20 min.csv

### Figure 1A: Thrombus, Platelet, and Vault Volumes
Displays the thrombus volume (i.e. platelet volume + vault volume), platelet volume (i.e. non-empty space), and vault volumes (i.e. empty space within the thormbus). 

![Figure 1A](https://github.com/oliverszhao/mousethrombi/blob/main/figures/Figure1A.png)

### Figure 1B: Platelet Type Distribution
Displays the platelet volumes of each thrombus, split by platelet type (i.e. degranulated, tightly adherent, and loosely adherent).

![Figure 1B](https://github.com/oliverszhao/mousethrombi/blob/main/figures/Figure1B.png)

### Figure 1C: Extravascular vs Intravascular Platelet Distribution
Displays the platelet volumes located on the extravascular and intravascular sides of the thrombus. Note that these values do not add up to total thrombus volume, as it excludes platelets located within the cavity/vessel wall. 

![Figure 1C](https://github.com/oliverszhao/mousethrombi/blob/main/figures/Figure1C.png)

### Figures 2A, 2B, 2C: Platelet Distribution by Time and Area per Type
Displays the relative platelet volumes of each type, distribution in different locations. Note however, that the relative volume indicates the relative amount of platelets of a specific platelet type located in a specific location **relative to the total amount of that *specific* platelet type within a given thrombus**.
In short, Figures 2A, 2B, and 2C provide **no** information on the absolute volume of different platelet types. 

![Figure 2A](https://github.com/oliverszhao/mousethrombi/blob/main/figures/Figure2A.png)
![Figure 2B](https://github.com/oliverszhao/mousethrombi/blob/main/figures/Figure2B.png)
![Figure 2C](https://github.com/oliverszhao/mousethrombi/blob/main/figures/Figure2C.png)
