# JPS Base Lib Benchmark

This project uses [Java Microbenchmark Harness (JMH)](https://github.com/openjdk/jmh) to benchmark the JPS Base Lib.

## How to run

1. Build the benchmark project

```
mvn clean verify
```

2. Run the executable JAR

```
java -jar target/benchmarks.jar
```

To save benchmark result to a file, run

```
java -jar target/benchmarks.jar -rf [FORMAT] -rff [FILENAME]
```

Available formats include text, csv, scsv, json, latex.

To only run test with one of the benchmark class or a method, run

```
java -jar target/benchmarks.jar org.openjdk.jmh.Main uk.ac.cam.cares.jps.[CLASS](.[METHOD])
```

To overwrite values of parameters, run

```
java -jar target/benchmarks.jar -p [PARAM1]=[VALUE1],[VALUE2],[VALUE3...] -p [PARAM2]=[VALUE4],[VALUE5],[VALUE6...]
```

### Parameters

* TimeSeriesClientIntegrationBenchmark
    * rdbMode: this controls how the relational database is set up. It only accepts "DEFAULT" or "REDUCEDTABLE".
    * numTimeSeries: the number of time series to be created for benchmarking.
    * numCol: the number of columns i.e. variables in each time series.
    * numRow: the number of row i.e. entries in each time series.

## Useful notes

@Benchmark annotates an actual benchmarking test.
@Setup annotates functions that are called before actual benchmarking. It takes _LEVEL_ as a parameter; Level.Trial indicates a function to be run once before benchmarking the entire class, whereas Level.Invocation indicates a function to be run before every benchmark call.