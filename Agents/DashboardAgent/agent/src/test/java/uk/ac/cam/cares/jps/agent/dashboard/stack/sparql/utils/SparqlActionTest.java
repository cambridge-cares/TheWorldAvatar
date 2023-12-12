package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils;

import org.apache.jena.rdfconnection.RDFConnection;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SparqlActionTest {
    @Test
    public void testExecute() throws Exception {
        // Create a mock RDFConnection
        RDFConnection mockConnection = Mockito.mock(RDFConnection.class);
        String mockEndpoint = "http://example.com/sparql";
        // Define the behavior of the mock connection, e.g., do nothing
        Mockito.doNothing().when(mockConnection).querySelect(Mockito.anyString(), Mockito.any());
        // Create an instance of SparqlAction using a lambda expression
        SparqlAction action = (conn, endpoint) -> {
            // Verify that the action is taking the same inputs
            assertEquals(mockEndpoint, endpoint);
            assertEquals(mockConnection, conn);
            // Execute a random SELECT query
            conn.querySelect("Sparql Query", (qs) -> {
            });
        };
        // Call the execute method of the SparqlAction instance
        action.execute(mockConnection, mockEndpoint);
        // Verify that the mockConnection's method was called as expected
        Mockito.verify(mockConnection, Mockito.times(1)).querySelect(Mockito.anyString(), Mockito.any());
    }
}