package uk.ac.cam.cares.jps.agent.status;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import javax.servlet.ServletContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.record.TestRecord;

/**
 *
 * @author Michael
 */
public class TemplateHandler {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TemplateHandler.class);

    /**
     *
     * @param definition
     * @return
     */
    public static String getTestResultStub(ServletContext context, TestDefinition definition) {
        String result = "";
        try {
            String template = getResourceFileAsString(context, "test-result-stub.html");
            result = template.replaceAll("NAME", definition.getName());

            // Get the latest result for this test
            TestRecord latestResult = StatusAgent.HANDLER.getRecordStore().getLatestRecord(definition);

            if (latestResult != null) {
                String success = (latestResult.getResult()) ? "success" : "failure";
                result = result.replaceAll("RESULT", success);
                result = result.replaceAll("TIME", latestResult.getExecutionTime().replaceAll("_", " "));
            } else {
                result = result.replaceAll("RESULT", "Not yet executed");
                result = result.replaceAll("TIME", "n/a");
            }

            System.out.println(result);
            return result;

        } catch (Exception exception) {
            LOGGER.error("template error", exception);
        }

        return result;
    }

    /**
     * Reads given resource file as a string.
     *
     * @param fileName path to the resource file
     * @return the file's contents
     * @throws IOException if read fails for any reason
     */
    private static String getResourceFileAsString(ServletContext context, String fileName) throws IOException {
        try (InputStream is = context.getResourceAsStream(fileName)) {
            if (is == null) return null;

            try (InputStreamReader isr = new InputStreamReader(is);  BufferedReader reader = new BufferedReader(isr)) {
                return reader.lines().collect(Collectors.joining(System.lineSeparator()));
            }
        }
    }
}
