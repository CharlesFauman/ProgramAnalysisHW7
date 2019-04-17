set -e 
set -o pipefail
echo "compiling"
make
echo "running"
cat test00.c | ./a.out