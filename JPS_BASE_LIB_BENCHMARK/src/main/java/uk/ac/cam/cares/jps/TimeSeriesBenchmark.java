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

import java.util.ArrayList;
import java.util.List;
import java.time.Instant;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import org.postgis.Point;

@State(Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 5, time = 100, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 200, timeUnit = TimeUnit.MILLISECONDS)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
public class TimeSeriesBenchmark {

    @Param({"1000"})
    private int size;
    private boolean printToScreen;

    TimeSeries<Instant> ts;
    List<String> dataIRI = new ArrayList<>();
    List<Instant> timeList = new ArrayList<>();
    List<Double> data1 = new ArrayList<>();
    List<String> data2 = new ArrayList<>();
    List<Integer> data3 = new ArrayList<>();
    List<Double> data4 = new ArrayList<>();
    List<Point> data5 = new ArrayList<>();
    List<List<?>> dataToAdd = new ArrayList<>();

    @Setup(Level.Trial)
    public void initialiseData() {
        dataIRI.add("http://data1");
        dataIRI.add("http://data2");
        dataIRI.add("http://data3");
        dataIRI.add("http://data4");
        dataIRI.add("http://data5");
        for (int i = 0; i < size; i++) {
            timeList.add(Instant.now().plusSeconds(i));
            data1.add(Double.valueOf(i));
            data2.add(String.valueOf(i));
            data3.add(Integer.valueOf(i));
            data4.add(Double.valueOf(i));

            Point point = new Point();
            point.setX(i);
            point.setY(i);
            data5.add(point);
        }
        data4.set(3, null);
        dataToAdd.add(data1);
        dataToAdd.add(data2);
        dataToAdd.add(data3);
        dataToAdd.add(data4);
        dataToAdd.add(data5);

        printToScreen = true;
    }

    @Benchmark
    public void testGetTimes() {
        ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
        List<Instant> tmpTimeList = ts.getTimes();
        if (printToScreen) {
            System.out.println("The length of the list is: " + tmpTimeList.size());
            printToScreen = false;
        }
    }

}
