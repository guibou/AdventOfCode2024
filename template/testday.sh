dayNumber=$(printf "%02d" $1)
echo $dayNumber

cp template/DayXSpec.hs tests/Day${dayNumber}Spec.hs
sed -i "s/DayX/Day$dayNumber/" tests//Day${dayNumber}Spec.hs

git add tests/Day${dayNumber}Spec.hs
