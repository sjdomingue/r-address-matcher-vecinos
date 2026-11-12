\# R Address Matcher \& Nearest Neighbor Search



An R-based tool that matches addresses using a custom similarity algorithm 

and returns the nearest neighbors based on street number, parity, and street type.



\## Problem



Matching messy, unstructured address data (e.g., "CL 10 # 5-20" vs "CALLE 10 5 22") 

is a manual and error-prone task. Without a standardized system, it takes hours 

to cross-reference data.



\## Solution



A script that:

\- Connects to a SQLite database using `DBI` and `RSQLite`.

\- Normalizes and extracts components (street type, number, parity, words).

\- Calculates a weighted similarity score (Number, Parity, Type, Words).

\- Filters and ranks the top N neighbors that exceed a similarity threshold.

\- Includes a script to generate a synthetic database for testing.



\## Result



Reduces the time needed to match and validate addresses from \*\*hours to minutes\*\*, 

with a transparent and reproducible scoring system. 



\## Demo



!\[Demo](docs/demo.png)



\## Installation (3 steps)



1\. Open the project in RStudio.

2\. Install required packages: `install.packages(c("DBI", "RSQLite", "readr", "dplyr"))`.

3\. Run the script: `source("matcher.R")`.



\## Stack



\- R

\- SQLite (via DBI/RSQLite)

\- Dplyr (data manipulation)

\- Synthetic test data generated automatically



\## License



MIT — see \[LICENSE](LICENSE).



> Data disclaimer: this repository uses ONLY synthetic data. No real 

> customer data or proprietary corporate data are included.

