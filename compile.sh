ghc $1
output_file=`echo $1 | cut -d. -f1`
mv $output_file ./build/
