package uk.ac.cam.cares.jps;


import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.query.*;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.sparql.exec.http.QueryExecutionHTTP;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.sql.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class IntegrationTestUtils {
    public static final String SPARQL_ENDPOINT = "http://172.17.0.1:9998/blazegraph/namespace";
    public static final String SRC_SPARQL_ENDPOINT = SPARQL_ENDPOINT + "/kb/sparql";
    public static final String TGT_SPARQL_ENDPOINT = SPARQL_ENDPOINT + "/test/sparql";
    public static final String SAMPLE_RDF_SUBJECT = "http://www.example.org/subject";
    public static final String SAMPLE_RDF_PREDICATE = "http://www.example.org/predicate";
    public static final String SAMPLE_RDF_OBJECT = "http://www.example.org/object";
    public static final String SPARQL_DELETE = "DELETE WHERE {?s ?p ?o}";
    public static final String SPARQL_INSERT = "INSERT DATA {<" + SAMPLE_RDF_SUBJECT + "> <" + SAMPLE_RDF_PREDICATE + "> <" + SAMPLE_RDF_OBJECT + ">}";
    public static final String SQL_JDBC = "jdbc:postgresql://172.17.0.1:5431/";
    public static final String SQL_DEFAULT_JDBC = SQL_JDBC + "postgres";
    public static final String SQL_TGT_JDBC = SQL_JDBC + "test";
    public static final String SQL_USER = "user";
    public static final String SQL_PASS = "pg123";
    public static final String TIME_CLASS = "INSTANTANEOUS";

    public static void createNamespace(String namespace) {
        // Generate XML properties for request
        String payload =
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" +
                        "<!DOCTYPE properties SYSTEM \"http://java.sun.com/dtd/properties.dtd\">" +
                        "<properties>" +
                        "  <entry key=\"com.bigdata.rdf.sail.truthMaintenance\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.textIndex\">false</entry>" +
                        "  <entry key=\"com.bigdata.namespace." + namespace + ".lex.com.bigdata.btree.BTree.branchingFactor\">400</entry>" +
                        "  <entry key=\"com.bigdata.namespace." + namespace + ".spo.com.bigdata.btree.BTree.branchingFactor\">1024</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.justify\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.axiomsClass\">com.bigdata.rdf.axioms.NoAxioms</entry>" +
                        "  <entry key=\"com.bigdata.rdf.sail.namespace\">" + namespace + "</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.quads\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.store.AbstractTripleStore.geoSpatial\">false</entry>" +
                        "  <entry key=\"com.bigdata.rdf.sail.isolatableIndices\">false</entry>" +
                        "</properties>";
        StringEntity configEntity = new StringEntity(payload, ContentType.create("application/xml", "UTF-8"));
        // Create a new post request
        HttpPost request = new HttpPost(SPARQL_ENDPOINT);
        request.setHeader("Accept", "application/xml");
        request.addHeader("Content-Type", "application/xml");
        request.setEntity(configEntity);
        // Send request
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            httpClient.execute(request);
        } catch (IOException e) {
            throw new JPSRuntimeException("Unable to create namespace: " + e.getMessage());
        }
    }

    public static Queue<String> query(String endpoint) {
        // Generate results as a queue
        Queue<String> result = new ArrayDeque<>();
        // Create query to retrieve all triples
        StringBuilder queryString = new StringBuilder();
        queryString.append("SELECT ?s ?p ?o WHERE {?s ?p ?o}");
        Query query = QueryFactory.create(queryString.toString());
        try (QueryExecution qExec = QueryExecutionHTTP.service(endpoint, query)) {
            org.apache.jena.query.ResultSet results = qExec.execSelect();
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                result.offer(soln.get("s").toString());
                result.offer(soln.get("p").toString());
                result.offer(soln.get("o").toString());
            }
        }
        return result;
    }

    public static void updateEndpoint(String endpoint, String updateQuery) {
        try (RDFConnection conn = RDFConnection.connect(endpoint)) {
            UpdateRequest update = UpdateFactory.create(updateQuery);
            conn.update(update);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to update queries at SPARQL endpoint: " + e.getMessage());
        }
    }

    public static Connection connectDatabase(String jdbc) {
        try {
            return DriverManager.getConnection(jdbc, SQL_USER, SQL_PASS);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to connect to test database: " + e.getMessage());
        }
    }

    public static void updateDatabase(Connection connection, String query) {
        try (Statement statement = connection.createStatement()) {
            statement.executeUpdate(query);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to execute updates: " + e.getMessage());
        }
    }

    public static Map<String, Map<String, Object>> queryDatabase(Connection connection, String query) {
        try (Statement statement = connection.createStatement()) {
            boolean hasResult = statement.execute(query);
            // If there is a result set, process it or else return null
            // Result set cannot be accessed once statement is closed
            if (hasResult) {
                Map<String, Map<String, Object>> results = new HashMap<>();
                java.sql.ResultSet rs = statement.getResultSet();
                // Retrieves all the column names dynamically
                int columnCount = rs.getMetaData().getColumnCount();
                List<String> columnNames = new ArrayList<>();
                for (int i = 1; i <= columnCount; i++) {
                    columnNames.add(rs.getMetaData().getColumnName(i));
                }
                // Retrieve all the values based on their names dynamically
                int row = 1;
                while (rs.next()) {
                    Map<String, Object> temp = new HashMap<>();
                    for (String columnName : columnNames) {
                        Object columnValue = rs.getObject(columnName);
                        temp.put(columnName, columnValue);
                    }
                    results.put("row" + row++, temp);
                }
                return results;
            }
            return null;
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to execute query: " + e.getMessage());
        }
    }

    public static Timestamp convertToTimeStamp(String timestamp) {
        // Define the date and time format of the input string
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
        // Parse the string to a LocalDateTime object
        LocalDateTime localDateTime = LocalDateTime.parse(timestamp, formatter);
        return Timestamp.valueOf(localDateTime);
    }

    public static String selectAllDataQuery(String table) {
        return "SELECT * FROM \"" + table + "\"";
    }

    public static String dropTableQuery(String table) {
        return "DROP TABLE IF EXISTS \"" + table + "\"";
    }
}
