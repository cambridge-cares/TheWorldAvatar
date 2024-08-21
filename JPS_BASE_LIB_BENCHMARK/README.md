# JPS Base Lib Benchmark

This project uses JMH to benchmark JPS Base Lib.

## How to run

1. Build the benchmark project

```
mvn clean verify
```

2. Run the executable JAR

```
java -jar target/benchmarks.jar
```

To save benchmark result to a JSON file, run

```
java -jar target/benchmarks.jar -rf json -rff [FILENAME]
```