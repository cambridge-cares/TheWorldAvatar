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

import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.List;
import java.util.ArrayList;
import java.time.Instant;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

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

    // TimeSeries client (with RDB and Sparql client)
    protected static TimeSeriesClient<Instant> tsClient;
    private static RemoteRDBStoreClient rdbStoreClient;
    private static RemoteStoreClient kbClient;

    // Time series test data
    private List<String> dataIRI_1, dataIRI_2;
    private List<Class<?>> dataClass_1, dataClass_2;
    private String timeUnit;

    // Will create two Docker containers for Blazegraph and postgreSQL
    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private static final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    @Setup(Level.Trial)
    public void initialiseData() {
        // Initialise time unit for all test data series
        timeUnit = "http://s";
        /*
         * Initialise 1st time series with 3 associated data series
         */
        dataIRI_1 = new ArrayList<>();
        dataIRI_1.add("http://data1");
        dataIRI_1.add("http://data2");
        dataIRI_1.add("http://data3");
        // Specify type of data for each column (most data will be in doubles, but one
        // can specify different data types)
        dataClass_1 = new ArrayList<>();
        dataClass_1.add(Double.class);
        dataClass_1.add(String.class);
        dataClass_1.add(Integer.class);
        /*
         * Initialise 2nd time series with only one associated data series
         */
        dataIRI_2 = new ArrayList<>();
        dataIRI_2.add("http://data4");
        // Specify type of data for each column (most data will be in doubles, but one
        // can specify different data types)
        dataClass_2 = new ArrayList<>();
        dataClass_2.add(Double.class);

    }

    @Setup(Level.Invocation)
    public static void initialiseTimeSeriesClient() {
        try {
            if (!blazegraph.isRunning()) {
                // Start Blazegraph container
                blazegraph.start();
            }

            if (!postgres.isRunning()) {
                // Start postgreSQL container
                postgres.start();
            }

            // Set up a kb client that points to the location of the triple store
            kbClient = blazegraph.getRemoteStoreClient();

            // Initialise TimeSeriesClient client with pre-configured kb client
            tsClient = new TimeSeriesClient<>(kbClient, Instant.class);

            // Configure database access
            rdbStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(),
                    postgres.getPassword());

            clearTriples();
            clearDatabase();

        } catch (Exception e) {
            throw new JPSRuntimeException(
                    "TimeSeriesClientIntegrationTest: Docker container startup failed. Please try running tests again",
                    e);
        }
    }

    // Clear all tables after each test to ensure clean slate
    private static void clearDatabase() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            DSLContext context = DSL.using(conn, SQLDialect.POSTGRES);
            List<Table<?>> tables = context.meta().getTables();
            for (Table<?> table : tables) {
                context.dropTable(table).cascade().execute();
            }
        }
    }
    
    // Clear all tables after each test to ensure clean slate
    private static void clearTriples() {
        kbClient.executeUpdate(BlazegraphContainer.DELETE_ALL_QUERY);
    }

    @Benchmark
    public void testBulkInitTimeSeries() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            List<List<String>> dataIRIs = new ArrayList<>();
            dataIRIs.add(dataIRI_1);
            dataIRIs.add(dataIRI_2);

            List<List<Class<?>>> classes = new ArrayList<>();
            classes.add(dataClass_1);
            classes.add(dataClass_2);

            List<String> units = new ArrayList<>();
            units.add(timeUnit);
            units.add(timeUnit);

            tsClient.bulkInitTimeSeries(dataIRIs, classes, units, conn);

        }
    }

}
