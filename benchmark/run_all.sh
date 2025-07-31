DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"
./run_mysql.sh "$1"
./run_kv_workload.sh "$1"
./run_microbenchmarks.sh "$1"

./run_idle_benchmark.sh "$1"
./run_compilation_redis.sh "$1"
./run_compilation_linux_kernel.sh "$1"
