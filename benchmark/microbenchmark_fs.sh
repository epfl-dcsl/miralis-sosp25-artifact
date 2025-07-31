########################
# filesystem Microbenchmark
########################

cd keystone-iozone
# Fast one (to run benchmarks faster)
taskset -c 1 ./iozone -i 0 -i 1 -i 2 -i 3 -i 4 -i 5 -I -s 128k -s 256k -s 512k -s 1M -s 2M -s 4M -r 4k -r 8k -r 16k -r 32k -r 64k -r 128k
# Normal one
# taskset -c 1 ./iozone -a 
cd ..