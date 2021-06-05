
# Data input file

This application accepts **CSV** and **TXT** format data files.  When converting files from EXCEL, make sure that numbers are converted to general format.  **No thousand comma "," s.**


# Data type

The application accepts 3 data types
* Run
* S-R
* Escapement

# Run data type 

Run data type consists of **Calendar Year**, Spawner (Escapement) size, Run size, and run age proportion from the youngest to the oldest.   

**Run input table example** 

| Year | |  Spawner| | Run    | | Age 3  | |  Age 4 | | Age 5  | | Age 6  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
|1966  | |1000   | |4500  | | 0.01 | | 0.45 | | 0.43 | |  0.06  |
|1967  | |1200   | |6000  | | 0.03 | | 0.48 | | 0.42 | |  0.07  |
|1968  | |2500   | |8250  | | 0.01 | | 0.42 | | 0.52 | |  0.06  |
|1969  | |3500   | |12500 | | 0.05 | | 0.56 | | 0.38 | |  0.01  |
|1970  | |2000   | |4000  | | 0.01 | |	0.42 | | 0.52 | |	 0.06  |
|1971  | |1600   | |3000  | | 0.01 | |	0.42 | | 0.52 | |  0.06  |
|1972  | |900    | |2000  | | 0.01 | |	0.42 | | 0.52 | |  0.06  |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
  
---
From the input table, the app creates a brood table starting the first calendar year's escapement and brood year recruitment.

| b.Year | |Spawner| |b.Age3 | |b.Age4 | |b.Age5 | |b.Age6 | |Recruit| 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|______|_|_______|_|_______|_|______|_|______|_|______|_|______|
| 1959| |       | |     | |     | |     | | 270| |        |
| 1961| |       | |     | |     | | 1935| | 420| |        |
| 1962| |       | |     | | 2025| | 2520| | 495| |        |
| 1963| |       | | 45  | | 2880| | 4290| | 125| | 7340   |
| 1964| |       | | 180 | | 3465| | 4750| | 240| | 8615   |
| 1965| |       | | 83  | | 7000| | 2080| | 180| | 9343   |
| 1966| | 1000  | | 625 | | 1680| | 1560| | 120| | 3985   |
| 1967| | 1200  | | 40  | | 1260| | 1040| |    | |        |
| 1968| | 2500  | | 30  | | 840 | |     | |    | |        |
| 1969| | 3500  | | 20  | |     | |     | |    | |        |
| 1970| | 2000  | |	    | |     | |	    | |    | |	      |
| 1971| | 1600  | |	    | |     | |	    | |    | |        |
| 1972| |  900  | |	    | |     | |	    |	|    | |        |
|______|_|_______|_|_______|_|______|_|______|_|______|_|______|
---

**Special case**  
The input data calculates brood year recruit several ages before the calendar year.  In the above example, a complete brood year starts from **1966**, but brood year recruit starting from **1963**.  When you **DO** have escapement data starting 1963, you can hack the table by inserting  **Real** escapement and **Dummy** run and age data. 

| Year | |  Spawner| | Run    | | Age 3  | |  Age 4 | | Age 5  | | Age 6  | 
|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|---:|-------:|
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|
| 1963 | | 2500  || 3500 | | 0.2  | | 0.2  | | 0.3  | |  0.3   |
| 1964|  | 12000| | 13000 || 0.2  | | 0.2 |  | 0.3 |  |  0.3  |
| 1965 | | 3200  || 3250  || 0.2  | | 0.2  | | 0.3|   |  0.3   |
| 1966|  | 1000 | | 4500  || 0.01 | | 0.45|  | 0.43|  |  0.06  |
| 1967 | | 1200  || 6000|  | 0.03 | | 0.48 | | 0.42 | |  0.07  |
| 1968|  | 2500 | | 8250 | | 0.01 | | 0.42|  | 0.52|  |  0.06  |
| 1969 | | 3500|  | 12500| | 0.05 | | 0.56 | | 0.38|  |  0.01  |
| 1970  || 2000 | |	4000  || 0.01 | |	0.42|	 | 0.52|  |	 0.06  |
| 1971  || 1600 | |	3000  || 0.01  ||	0.42 | | 0.52|  |  0.06  |
| 1972  ||  900  ||	2000  || 0.01|  |	0.42| | 0.52|  |  0.06  |
|_____|_|_______|_|______|_|_____|_|_____|_|_____|_|_____|

---
Analyses methods available for this data type are: 
* Escapement Only Analyses 
  * Percentile Analyses 
  * Risk Analyses 
  
* SR Analyses 
* MSE (Management Strategy Evaluation) Analyses 


# S-R data type 

Run data type consists of **Brood  Year**, Spawner (Escapement) size and  Recruit (Brood year return) size.    


| Year  | | Spawner|| Recruit| 
|-------:|---:|-------:|---:|-------:|
|____|_|______|_|______|
| 1966 |  | 1000|   | 2500   |
| 1967|   | 1200|   | 7300   |
| 1968 |  | 2500 |  | 4250   |
| 1969|   | 3500|   | 5250   |
|____|_|______|_|______|
---
Analyses methods available for this data type are: 
* SR Analyses 


# Escapement Only data type 

Escapement only data type consists of **Calendar Year** and Spawner (Escapement) size. Only Percentile and Risk Analyses are available for this data type.    

| Year   | Spawner| 
|-------:|-------:|
| 1966   | 1000   |
| 1967   | 1200   |
| 1968   | 2500   |
| 1969   | 3500   | 

Analyses methods available for this data type are: 
* Escapement Only Analyses 
  * Percentile Analyses 
  * Risk Analyses 