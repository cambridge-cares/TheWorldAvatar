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

Other available formats include text, csv, scsv, json, latex.

To only run test with one of the benchmark class, run

```
java -jar target/benchmarks.jar org.openjdk.jmh.Main uk.ac.cam.cares.jps.[CLASS]
```

To overwrite values of parameters, run

```
java -jar target/benchmarks.jar -p [PARAM]=[VALUE1],[VALUE2],[VALUE3...]
```

## Useful notes

@Benchmark annotates an actual benchmarking test.
@Setup annotates functions that are called before actual benchmarking. It takes _LEVEL_ as a parameter; Level.Trial indicates a function to be run once before benchmarking the entire class, whereas Level.Invocation indicates a function to be run before even benchmark call.