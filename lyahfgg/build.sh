# danger: super brittle!

output_filename=`echo $1 | cut -d. -f1`.out
output_location="./build/${output_filename}"

ghc $1 -o $output_filename

mv ${output_filename} ${output_location}
echo "You may execute your program with: ${output_location}"
