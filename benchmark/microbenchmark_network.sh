########################
# Network Microbenchmark
########################

cd netperf
# Start the server 
taskset -c 1 netserver
cd ..