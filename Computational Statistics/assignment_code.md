# AUEB M.Sc. in Applied Statistics
## Course: Computational Statistics
## Winter 2020
## Lecturer: P. Besbeas
## Author: Konstantina Tsami

## Language: R


### PAPER 1

#### Task 1 



Data Loading
```
load Data_GlobalIdx2;
plot(dates, 100 * Dataset.EB3M)
datetick('x'), xlabel('Date'), ylabel('Daily Yield (%)')
title('3-Month Euribor as a Daily Effective Yield')
```
