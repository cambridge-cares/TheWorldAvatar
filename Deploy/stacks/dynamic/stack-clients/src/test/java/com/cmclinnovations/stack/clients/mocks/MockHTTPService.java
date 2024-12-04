package com.cmclinnovations.stack.clients.mocks;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.AssertionFailureBuilder;
import org.junit.jupiter.api.Assertions;
import org.mockserver.client.MockServerClient;
import org.mockserver.integration.ClientAndServer;
import org.mockserver.model.HttpRequest;
import org.mockserver.model.HttpResponse;
import org.mockserver.model.Not;
import org.mockserver.model.RegexBody;
import org.mockserver.model.RequestDefinition;

public class MockHTTPService extends ClientAndServer {

    private static final RegexBody EMPTY_BODY = Not.not(RegexBody.regex(".*"));

    public static enum Method {
        GET,
        HEAD,
        POST,
        PUT,
        DELETE
    }

    protected final URL url;

    private List<RequestDefinition> expectations = new ArrayList<>();

    protected MockHTTPService() throws MalformedURLException {
        super(ThreadLocalRandom.current().nextInt(5000, 5500));

        Assertions.assertTrue(hasStarted(10, 2, TimeUnit.SECONDS));

        url = new URL("http", "localhost", getPort(), "/");
    }

    public MockServerClient reset() {
        expectations.clear();
        return super.reset();
    }

    public void addExpectation(Method method, String path, int returnCode) {
        addExpectation(method, path, returnCode, HttpRequest.request(), HttpResponse.response());
    }

    public void addExpectation(Method method, String path, int returnCode, HttpRequest requestExtras) {
        addExpectation(method, path, returnCode, requestExtras, HttpResponse.response());
    }

    public void addExpectation(Method method, String path, int returnCode, HttpResponse responseExtras) {
        addExpectation(method, path, returnCode, HttpRequest.request(), responseExtras);
    }

    public void addExpectation(Method method, String path, int returnCode,
            HttpRequest requestExtras, HttpResponse responseExtras) {
        if (null == requestExtras.getBody()) {
            // Add empty body check
            requestExtras.withBody(EMPTY_BODY);
        }
        requestExtras.withPath(path).withMethod(method.toString());
        when(requestExtras).respond(responseExtras.withStatusCode(returnCode));
        expectations.add(requestExtras);
    }

    public void verifyCalls() throws AssertionError {
        verifyCalls(expectations.toArray(HttpRequest[]::new));
    }

    public void verifyCalls(HttpRequest... expectations) throws AssertionError {
        List<HttpRequest> allRecordedRequests = List.of(retrieveRecordedRequests(null));
        List<HttpRequest> unaccountedForRecordedRequests = new ArrayList<>(allRecordedRequests);

        for (var entry : Stream.of(expectations)
                .collect(Collectors.groupingBy(Function.identity(), LinkedHashMap::new, Collectors.counting()))
                .entrySet()) {
            HttpRequest[] recordedRequests = retrieveRecordedRequests(entry.getKey());
            assertRequestsEqual(entry.getValue().intValue(), recordedRequests.length,
                    "Failed to match the following calls.", unaccountedForRecordedRequests, entry.getKey());
            unaccountedForRecordedRequests.removeAll(List.of(recordedRequests));
        }

        assertRequestsEqual(expectations.length, allRecordedRequests.size(), "Missing expected requests.",
                unaccountedForRecordedRequests);
    }

    private void assertRequestsEqual(int expected, int actual, String message,
            List<HttpRequest> actualRequests, HttpRequest... expectedRequests) {
        if (expected != actual) {
            AssertionFailureBuilder.assertionFailure() //
                    .message(getFailureMessageGenerator(message, actualRequests, expectedRequests)) //
                    .buildAndThrow();
        }
    }

    private Supplier<String> getFailureMessageGenerator(String message, List<HttpRequest> actualRequests,
            HttpRequest... expectedRequests) {
        String expected = (0 == expectedRequests.length) ? "\n{}\n"
                : Stream.of(expectedRequests)
                        // Remove empty body check
                        .map(exp -> exp.getBody() == EMPTY_BODY ? exp.withBody("") : exp)
                        .map(RequestDefinition::toString)
                        .collect(Collectors.joining("\n", "\n", "\n"));
        String actual = actualRequests.stream()
                .map(RequestDefinition::toString)
                .collect(Collectors.joining("\n", "\n", "\n"));
        return () -> message + "\n%EXPECTS" + expected + "%EXPECTE\n%ACTUALS" + actual + "%ACTUALE\n";

    }
}
