#!/bin/bash

#### Stat 243 Fall 2012
#### Problem Set 1

#### 1.(a)
# Download .csv from FEC
wget -O "CandidateSummary.csv" "http://www.fec.gov/data/CandidateSummary.do?format=csv"

# Extract Senate, and then split into 'REP' and 'DEM'
grep "\"S\"" CandidateSummary.csv | grep "\"REP\"" > REP.csv
grep "\"S\"" CandidateSummary.csv | grep "\"DEM\"" > DEM.csv

# Preprocessing before spliting fields
sed 's/\([^",]\),/\1/g' REP.csv | sed 's/[$"]//g' > REPclean.csv
sed 's/\([^",]\),/\1/g' DEM.csv | sed 's/[$"]//g' > DEMclean.csv

# Sort on total contribution and display the richest five
echo "The five Senate candidates with the largest total contributions"
echo "[ID,Name,Office,State,Party,Description,TotalContribution]"

echo ""
echo "in the Repubilican Party :"
cut -d ',' -f 2,3,4,5,7,8,20 REPclean.csv | sort -n -r -t ',' -k 7 | head -n 5
echo ""
echo "in the Democratic Party :"
cut -d ',' -f 2,3,4,5,7,8,20 DEMclean.csv | sort -n -r -t ',' -k 7 | head -n 5
rm -f *.csv


#### 1.(b)
wget "ftp://ftp.fec.gov/FEC/2012/cn12.zip"
wget "ftp://ftp.fec.gov/FEC/2012/indiv12.zip"
unzip -q \*.zip # get cn.txt,itcont.txt
rm -f *.zip

# Clear cn.txt down to current candidates for presidential race year 2012
cut cn.txt -d '|' -f 1-6,9-11 | grep "|2012|US|P|C|" > pres_cn.txt

# Grab PC ID with last name
NAME="OBAMA"
PCID=$(grep "|${NAME}," pres_cn.txt | cut -d '|' -f 8 | head -n 1)

# Individual contribution counts (only over $200 is recorded, exclude negative number counts)
# National counts
echo "The number of contributions above \$200 nationwide for ${NAME} is :"
grep "${PCID}" itcont.txt | cut -d '|' -f 15 | grep -v "-" | wc -l
# California counts
echo "The number of contributions above \$200 in California for ${NAME} is :"
grep "${PCID}" itcont.txt | grep "CA" | cut -d '|' -f 15 | grep -v "-" | wc -l


#### 1.(c) (need to run 1.(b) first)
# Get the last name(s) of the 2012 presidential candidates in the REP and DEM
# These are candidates of interest for my discussion
grep "|DEM\|REP|" pres_cn.txt | cut -d '|' -f 2 | cut -d ',' -f 1 > name_cn.txt
cnName=$(cat name_cn.txt)


# Functionalize 1.(b)
function getContNation () {
    ID=$(grep "|${1}," pres_cn.txt | cut -d '|' -f 8 | head -n 1)
    NM=$(grep "${ID}" pres_cn.txt | cut -d '|' -f 2)
    echo "${NM} :"
    grep "${ID}" itcont.txt | cut -d '|' -f 15 | grep -v "-" | wc -l
}

function getContCA () {
    ID=$(grep "|${1}," pres_cn.txt | cut -d '|' -f 8 | head -n 1)
    NM=$(grep "${ID}" pres_cn.txt | cut -d '|' -f 2)
    echo "${NM} :"
    grep "${ID}" itcont.txt | grep "CA" | cut -d '|' -f 15 | grep -v "-" | wc -l
}


echo "The number of contributions above \$200 nationwide for "
for name in $cnName
do
    getContNation $name
done

echo "The number of contributions above \$200 in California (CA) for "
for name in $cnName
do
    getContCA $name
done

rm -f *txt












