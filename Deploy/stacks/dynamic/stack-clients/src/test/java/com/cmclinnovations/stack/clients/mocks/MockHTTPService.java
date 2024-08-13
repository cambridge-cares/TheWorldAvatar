package com.cmclinnovations.stack.clients.mocks;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.mockserver.integration.ClientAndServer;
import org.mockserver.model.HttpRequest;
import org.mockserver.model.RequestDefinition;

public class MockHTTPService extends ClientAndServer {

    protected final URL url;

    protected MockHTTPService() throws MalformedURLException {
        super(ThreadLocalRandom.current().nextInt(5000, 5500));

        Assertions.assertTrue(hasStarted(10, 2, TimeUnit.SECONDS));

        url = new URL("http", "localhost", getPort(), "/");
    }

    public void verifyCalls(RequestDefinition... expectations) throws AssertionError {

        HttpRequest[] recordedRequests = retrieveRecordedRequests(null);
        String recordedPaths = Stream.of(recordedRequests).map(req ->req.getMethod() +": "+ req.getPath().getValue())
                .collect(Collectors.joining("\n", "\n", "\n"));
        Assertions.assertEquals(expectations.length, recordedRequests.length,
                "Wrong number of calls.\n"
                        + Stream.of(expectations).map(exp -> exp.toString())
                                .collect(Collectors.joining("\n", "\n", "\n"))
                        + "vs"
                        + recordedPaths);
        Assertions.assertAll(Stream.of(expectations)
                .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
                .entrySet().stream()
                .map(entry -> () -> Assertions.assertEquals(entry.getValue(),
                        retrieveRecordedRequests(entry.getKey()).length,
                        "Wrong number of calls with the following definition: '"
                                + entry.getKey().toString() + "\nthe following were run:"+ recordedPaths)));

    }
}
