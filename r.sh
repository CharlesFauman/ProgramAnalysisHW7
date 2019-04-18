set -e 
set -o pipefail
echo "compiling"
make
echo "running"
cat test23.c | ./a.out > out.txt