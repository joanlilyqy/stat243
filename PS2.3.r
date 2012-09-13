# R
#### Stat243
#### Problem Set 2
#### 3. President speech analysis

rm(list=ls(all=TRUE)) #remove all objects

system("wget -q -O 'index_pres.html' 'http://www.presidency.ucsb.edu/sou.php#axzz265cEKp1a'")
system("fromdos index_pres.html")
