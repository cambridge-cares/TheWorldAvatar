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
        super(ThreadLocalRandom.current().nextInt(50000, 55000));

        Assertions.assertTrue(hasStarted(10, 2, TimeUnit.SECONDS));

        url = new URL("http", "localhost", getPort(), "/");
    }

    public void verifyCalls(RequestDefinition... expectations) throws AssertionError {
    
        HttpRequest[] recordedRequests = retrieveRecordedRequests(null);
    
        Assertions.assertAll(Stream.concat(
                Stream.of(() -> Assertions.assertEquals(expectations.length, recordedRequests.length,
                        "Wrong number of calls.")),
                Stream.of(expectations)
                        .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
                        .entrySet().stream()
                        .map(entry -> () -> Assertions.assertEquals(entry.getValue(),
                                retrieveRecordedRequests(entry.getKey()).length,
                                "Wrong number of calls with the following definition: '"
                                        + entry.getKey().toString()))));
    
    }
}
