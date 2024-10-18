/*
 * Copyright (c) 2014, Oracle America, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 *  * Neither the name of Oracle nor the names of its contributors may be used
 *    to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

package uk.ac.cam.cares.jps;

import java.util.concurrent.TimeUnit;

import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;
import org.testcontainers.containers.PostgreSQLContainer;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.List;
import java.util.ArrayList;
import java.util.Random;
import java.time.Instant;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

import java.sql.Connection;
import java.sql.SQLException;
import org.jooq.Table;

@State(Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 5, time = 100, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class TimeSeriesClientIntegrationBenchmark {

    // Time series test data
    private static List<List<String>> dataIRIs;
    private static List<List<Class<?>>> classes;
    static List<String> units;
    static List<TimeSeries<Instant>> tss;
    Random random = new Random();
    String characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    // Will create two Docker containers for Blazegraph and postgreSQL

    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();
    // Create Docker container with postgres 13.3 image from Docker Hub

    private static final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    public enum RDBBackend {
        DEFAULT,
        REDUCEDTABLE
    }

    @Param({ "DEFAULT" })
    private static RDBBackend rdbMode; // how the relational database should be set up

    @Param({ "1" })
    private int numTimeSeries; // number of time series
    @Param({ "1" })
    private int numCol; // number of column in time series
    @Param({ "1" })
    private int numRow; // number of row in time series

    // CREATE SAMPLE DATA FOR TESTING

    private List<String> getTimeSeriesIRI(int offset, int numDataSeries) {
        List<String> dataIRI = new ArrayList<>();
        for (int i = 0; i < numDataSeries; i++) {
            dataIRI.add(String.format("http://data%d", offset + i + 1));
        }
        return dataIRI;
    }

    private void addData(String timeUnit, List<Class<?>> dataClass, int n) {
        int offset = 0;
        int nClass = dataClass.size();
        for (int i = 0; i < n; i++) {
            List<String> dataIRI = getTimeSeriesIRI(offset, nClass);
            dataIRIs.add(dataIRI);
            classes.add(dataClass);
            units.add(timeUnit);
            offset += nClass;
        }
    }

    @Setup(Level.Trial)
    public void initialiseData() {

        // Prepare for creation of time series

        dataIRIs = new ArrayList<>();
        classes = new ArrayList<>();
        units = new ArrayList<>();

        // Initialise time unit for all test data series
        String timeUnit = "http://s";

        // Initialise time series with numCol associated data series
        // Specify type of data for each column
        List<Class<?>> dataClass = new ArrayList<>();
        for (int i = 0; i < numCol; i++) {
            switch (i % 3) {
                case 0:
                    dataClass.add(Double.class);
                    break;
                case 1:
                    dataClass.add(String.class);
                    break;
                case 2:
                    dataClass.add(Integer.class);
                    break;
                default:
                    break;
            }
        }
        addData(timeUnit, dataClass, numTimeSeries);

        // Prepare sample data to be added

        tss = new ArrayList<>();

        List<Instant> time = new ArrayList<>();
        Instant currentInstant = Instant.now();

        for (int i = 0; i < numRow; i++) {
            time.add(currentInstant);
            currentInstant = currentInstant.plusSeconds(1);
        }

        for (int i = 0; i < dataIRIs.size(); i++) {

            TimeSeries<Instant> ts = new TimeSeries<>(time, dataIRIs.get(i),
                    sampleTimeSeriesData(classes.get(i), numRow));
            tss.add(ts);
        }

    }

    private List<Double> sampleTimeSeriesDouble(int nt) {
        List<Double> value = new ArrayList<>();
        for (int i = 0; i < nt; i++) {
            value.add(random.nextDouble());
        }
        return value;
    }

    private List<Integer> sampleTimeSeriesInteger(int nt) {
        List<Integer> value = new ArrayList<>();
        for (int i = 0; i < nt; i++) {
            value.add(random.nextInt());
        }
        return value;
    }

    private List<String> sampleTimeSeriesString(int nt) {
        List<String> value = new ArrayList<>();
        int length = 10;
        for (int i = 0; i < nt; i++) {
            StringBuilder randomString = new StringBuilder(length);
            for (int j = 0; j < length; j++) {
                randomString.append(characters.charAt(random.nextInt(characters.length())));
            }
            value.add(randomString.toString());
        }
        return value;
    }

    private List<List<?>> sampleTimeSeriesData(List<Class<?>> dataClass, int nt) {
        List<List<?>> values = new ArrayList<>();
        for (Class<?> c : dataClass) {
            // cannot use switch for class
            if (c == Double.class) {
                values.add(sampleTimeSeriesDouble(nt));
            } else if (c == String.class) {
                values.add(sampleTimeSeriesString(nt));
            } else if (c == Integer.class) {
                values.add(sampleTimeSeriesInteger(nt));
            }
        }
        return values;
    }

    @Setup(Level.Trial)
    public void startContainers() {

        if (!blazegraph.isRunning()) {
            // Start Blazegraph container
            blazegraph.start();
        }

        if (!postgres.isRunning()) {
            // Start postgreSQL container
            postgres.start();
        }
    }

    // TESTS STARTING FROM A DATABASE WITHOUT ANY EXISTING TIME SERIES

    // Clear all tables after each test to ensure clean slate
    private static void clearDatabase(RemoteRDBStoreClient rdbStoreClient) throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            DSLContext context = DSL.using(conn, SQLDialect.POSTGRES);
            List<Table<?>> tables = context.meta().getTables();
            for (Table<?> table : tables) {
                context.dropTable(table).cascade().execute();
            }
        }
    }

    // Clear all tables after each test to ensure clean slate
    private static void clearTriples(RemoteStoreClient kbClient) {
        kbClient.executeUpdate(BlazegraphContainer.DELETE_ALL_QUERY);
    }

    @State(Scope.Benchmark)
    public static class NoTableState {

        // This state will have no time series tables.

        protected static TimeSeriesClient<Instant> tsClient;
        protected static RemoteRDBStoreClient rdbStoreClient;

        @Setup(Level.Invocation)
        public void initialise() {
            RemoteStoreClient kbClient;
            try {

                // Set up a kb client that points to the location of the triple store
                kbClient = blazegraph.getRemoteStoreClient();

                // Initialise TimeSeriesClient client with pre-configured kb client

                switch (rdbMode) {
                    case DEFAULT:
                        tsClient = new TimeSeriesClient<>(kbClient, Instant.class);
                        break;
                    case REDUCEDTABLE:
                        TimeSeriesRDBClientWithReducedTables<Instant> rdbClient = new TimeSeriesRDBClientWithReducedTables<>(
                                Instant.class);
                        tsClient = new TimeSeriesClient<>(kbClient, rdbClient);
                        break;
                    default:
                        // shouldn't be here
                        break;
                }

                // Configure database access
                rdbStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(),
                        postgres.getPassword());

                clearTriples(kbClient);
                clearDatabase(rdbStoreClient);

            } catch (Exception e) {
                throw new JPSRuntimeException(
                        "Docker container startup failed. Please try running tests again",
                        e);
            }
        }
    }

    @Benchmark
    public void testBulkInitTimeSeries(NoTableState state) throws SQLException {
        try (Connection conn = NoTableState.rdbStoreClient.getConnection()) {
            NoTableState.tsClient.bulkInitTimeSeries(dataIRIs, classes, units, conn);
        }
    }

    @Benchmark
    public void testInitTimeSeries(NoTableState state) throws SQLException {
        try (Connection conn = NoTableState.rdbStoreClient.getConnection()) {
            for (int i = 0; i < dataIRIs.size(); i++) {
                NoTableState.tsClient.initTimeSeries(dataIRIs.get(i), classes.get(i), units.get(i), conn);
            }
        }
    }

    // TESTS STARTING WITH EMPTY TIME SERIES

    @State(Scope.Benchmark)
    public static class EmptyTableState extends NoTableState {

        // This state will have time series table initialised without any data.

        @Setup(Level.Invocation)
        @Override
        public void initialise() {
            super.initialise();
            try (Connection conn = rdbStoreClient.getConnection()) {
                tsClient.bulkInitTimeSeries(dataIRIs, classes, units, conn);
            } catch (SQLException e) {
                throw new JPSRuntimeException(
                        "SQLException when trying to initialise time series for benchmark.",
                        e);
            }
        }
    }

    @Benchmark
    public void testBulkAddTimeSeriesData(EmptyTableState state) throws SQLException {
        try (Connection conn = EmptyTableState.rdbStoreClient.getConnection()) {
            EmptyTableState.tsClient.bulkaddTimeSeriesData(tss, conn);
        }
    }

    @Benchmark
    public void testAddTimeSeriesData(EmptyTableState state) throws SQLException {
        try (Connection conn = EmptyTableState.rdbStoreClient.getConnection()) {
            for (TimeSeries<Instant> ts : tss) {
                EmptyTableState.tsClient.addTimeSeriesData(ts, conn);
            }
        }
    }

    @Benchmark
    public void testGetAllTimeSeries(EmptyTableState state, Blackhole blackhole) {
        blackhole.consume(EmptyTableState.tsClient.getAllTimeSeries());
    }

    // TESTS STARTING WITH INITIALISED TIME SERIES

    @State(Scope.Benchmark)
    public static class PopulatedTableState extends EmptyTableState {

        // This state will have time series table initialised and with data.

        @Setup(Level.Invocation)
        @Override
        public void initialise() {
            super.initialise();
            try (Connection conn = rdbStoreClient.getConnection()) {
                tsClient.bulkaddTimeSeriesData(tss, conn);
            } catch (SQLException e) {
                throw new JPSRuntimeException(
                        "SQLException when trying to populate time series for benchmark.",
                        e);
            }
        }
    }

    @Benchmark
    public void testGetTimeSeries(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        try (Connection conn = PopulatedTableState.rdbStoreClient.getConnection()) {
            for (List<String> dataIRI : dataIRIs) {
                blackhole.consume(PopulatedTableState.tsClient.getTimeSeries(dataIRI, conn));
            }
        }
    }

    @FunctionalInterface
    public interface DatumIRIFunction {
        void apply(String datumIRI, Connection conn, Blackhole blackhole) throws SQLException;
    }

    public void loopDatum(PopulatedTableState state, Blackhole blackhole, DatumIRIFunction function)
            throws SQLException {
        try (Connection conn = PopulatedTableState.rdbStoreClient.getConnection()) {
            for (List<String> dataIRI : dataIRIs) {
                for (String datumIRI : dataIRI) {
                    function.apply(datumIRI, conn, blackhole);
                }
            }
        }
    }

    @Benchmark
    public void testGetLatestData(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getLatestData(datumIRI, conn));
        });
    }

    @Benchmark
    public void testGetOldestData(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getOldestData(datumIRI, conn));
        });
    }

    @Benchmark
    public void testGetMaxTime(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getMaxTime(datumIRI, conn));
        });
    }

    @Benchmark
    public void testGetMinTime(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getMinTime(datumIRI, conn));
        });
    }

    public void loopNumericDatum(PopulatedTableState state, Blackhole blackhole, DatumIRIFunction function)
            throws SQLException {
        try (Connection conn = PopulatedTableState.rdbStoreClient.getConnection()) {
            for (int i = 0; i < dataIRIs.size(); i++) {
                List<String> dataIRI = dataIRIs.get(i);
                List<Class<?>> dataClass = classes.get(i);
                for (int j = 0; j < dataIRI.size(); j++) {
                    String datumIRI = dataIRI.get(j);
                    Class<?> datumClass = dataClass.get(j);
                    if ((datumClass == Double.class) || (datumClass == Integer.class)) {
                        function.apply(datumIRI, conn, blackhole);
                    }
                }
            }
        }
    }

    @Benchmark
    public void testGetAverage(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopNumericDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getAverage(datumIRI, conn));
        });
    }

    @Benchmark
    public void testGetMaxValue(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopNumericDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getMaxValue(datumIRI, conn));
        });
    }

    @Benchmark
    public void testGetMinValue(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopNumericDatum(state, blackhole, (datumIRI, conn, bh) -> {
            bh.consume(PopulatedTableState.tsClient.getMinValue(datumIRI, conn));
        });
    }

    @Benchmark
    public void testDeleteIndividualTimeSeries(PopulatedTableState state, Blackhole blackhole) throws SQLException {
        loopDatum(state, blackhole, (datumIRI, conn, bh) -> {
            PopulatedTableState.tsClient.deleteIndividualTimeSeries(datumIRI, conn);
        });
    }

    @Benchmark
    public void testDeleteTimeSeries(PopulatedTableState state) throws SQLException {
        try (Connection conn = PopulatedTableState.rdbStoreClient.getConnection()) {
            List<String> tsIRIs = PopulatedTableState.tsClient.getAllTimeSeries();
            for (String tsIRI : tsIRIs) {
                PopulatedTableState.tsClient.deleteTimeSeries(tsIRI, conn);
            }
        }
    }

    @Benchmark
    public void testDeleteAll(PopulatedTableState state) throws SQLException {
        try (Connection conn = PopulatedTableState.rdbStoreClient.getConnection()) {
            PopulatedTableState.tsClient.deleteAll(conn);
        }
    }

}
