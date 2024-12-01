dayNumber=$(printf "%02d" $1)
echo $dayNumber

cp template/DayX.hs src/Day$dayNumber.hs
sed -i "s/DayX/Day$dayNumber/" src/Day$dayNumber.hs

echo "-- started at $(date)" >> src/Day$dayNumber.hs

git add src/Day$dayNumber.hs
