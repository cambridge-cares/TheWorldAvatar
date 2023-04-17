package uk.ac.cam.cares.jps.sql;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.DataBridgeAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.ByteArrayOutputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Creates a client to the postGIS container to execute the shell commands.
 *
 * @author qhouyee
 */
public class PostGISClient extends ContainerClient {
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    private final String postGISContainerId;
    private static final String PASSWORD_OPTION = "PGPASSWORD='";
    private static final String USER_OPTION = "-U ";
    private static final String SERVER_OPTION = "-h ";
    private static final String PORT_OPTION = "-p ";
    private static final String WHITESPACE = " ";

    /**
     * Standard Constructor intialising the client based on the endpoint.
     *
     * @param isStack A boolean indicating whether the agent is running in a stack.
     */
    public PostGISClient(boolean isStack) {
        postGISContainerId = isStack ? getContainerId("postgis") : null;
    }

    /**
     * Transfers only the data in the specified table. The schema is excluded as it may present out of memory errors as mentioned:
     * https://stackoverflow.com/questions/3195125/copy-a-table-from-one-database-to-another-in-postgres/16708680#16708680
     */
    public String transferTable(String table, String srcJdbcUrl, String srcUser, String srcPass, String tgtJdbcUrl, String tgtUser, String tgtPass) {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
        StringBuilder psqlCmd = new StringBuilder();
        String[] srcDetails = retrieveComponentsFromUrl(srcJdbcUrl);
        String[] tgtDetails = retrieveComponentsFromUrl(tgtJdbcUrl);
        // Dump all the SQL data in this table on the source database
        psqlCmd.append(PASSWORD_OPTION).append(srcPass).append("' pg_dump ")
                .append(USER_OPTION).append(srcUser).append(WHITESPACE)
                .append(SERVER_OPTION).append(srcDetails[0]).append(WHITESPACE)
                .append(PORT_OPTION).append(srcDetails[1]).append(WHITESPACE)
                .append("-t \\\"").append(table).append("\\\" ")
                .append(srcDetails[2]).append(WHITESPACE)
                // Transfer to target database in the same table
                .append("| ").append(PASSWORD_OPTION).append(tgtPass).append("' psql ")
                .append(USER_OPTION).append(tgtUser).append(WHITESPACE)
                .append(SERVER_OPTION).append(tgtDetails[0]).append(WHITESPACE)
                .append(PORT_OPTION).append(tgtDetails[1]).append(WHITESPACE)
                .append(tgtDetails[2]);
        if(postGISContainerId!=null) {
            String execId = createComplexCommand(postGISContainerId, "bash", "-c", psqlCmd.toString())
                    .withOutputStream(outputStream)
                    .withErrorStream(errorStream)
                    .exec();
            handleErrors(errorStream, execId);
            return "";
        } else {
            return psqlCmd.toString();
        }
    }

    /**
     * Retrieves the server, port, and database name from the jdbc url.
     */
    public String[] retrieveComponentsFromUrl(String jdbcUrl) {
        String[] components = new String[3];
        // An intermediate temp string to store the retrieved portion
        String tmp;
        // Compile the regular expression pattern to retrieve the server, port, and db name portions
        Matcher m = Pattern.compile("jdbc:postgresql://(.+)").matcher(jdbcUrl);
        // Extract the items only if it matches
        if (m.find()) {
            tmp = m.group(1);
            int colonIndex = tmp.indexOf(":");
            int slashIndex = tmp.indexOf("/");
            components[0] = tmp.substring(0, colonIndex); // Server
            components[1] = tmp.substring(colonIndex + 1, slashIndex); // Port
            components[2] = tmp.substring(slashIndex+1); // Database name
        } else {
            LOGGER.fatal("Invalid JDBC url: " + jdbcUrl);
            throw new IllegalArgumentException("Invalid JDBC url: " + jdbcUrl);
        }
        return components;
    }

    private void handleErrors(ByteArrayOutputStream errorStream, String execId) {
        long commandErrorCode = getCommandErrorCode(execId);
        if (0 != commandErrorCode) {
            throw new JPSRuntimeException("Docker exec command returned '" + commandErrorCode
                    + "' and wrote the following to stderr:\n" + errorStream.toString());
        } else {
            LOGGER.warn("Docker exec command returned '0' but wrote the following to stderr:\n{}", errorStream);
        }
    }
}
