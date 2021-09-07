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
 * Handles reading template HTML files and injecting values.
 *
 * @author Michael Hillman
 */
public class TemplateHandler {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TemplateHandler.class);

    /**
     * Read template for test stubs within the main dashboard page.
     *
     * @param context
     * @param latestResult
     * @return
     */
    public static String getDashboardStub(ServletContext context, TestDefinition definition, TestRecord latestResult) {
        String result = "";
        try {
            String template = getResourceFileAsString(context, "test-stub-dashboard.html");
            result = template.replaceAll("NAME", definition.getName());
            result = result.replaceAll("TYPE", definition.getType().toString());

            if (latestResult != null) {
                String success = (latestResult.getResult()) ? "success" : "failure";
                result = result.replaceAll("RESULT", success);
                result = result.replaceAll("CLASS", success);
                result = result.replaceAll("TIME", latestResult.getExecutionTime().replaceAll("_", " "));
            } else {
                result = result.replaceAll("RESULT", "not yet executed");
                result = result.replaceAll("CLASS", "not-yet-run");
                result = result.replaceAll("TIME", "n/a");
            }
            return result;

        } catch (Exception exception) {
            LOGGER.error("template error", exception);
        }
        return result;
    }

    /**
     * Read template for test stubs within individual test summary pages.
     *
     * @param context
     * @param latestResult
     * @return
     */
    public static String getSummaryStub(ServletContext context, TestDefinition definition, TestRecord record) {
        String result = "";
        try {
            String template = getResourceFileAsString(context, "test-stub-summary.html");
            result = template.replaceAll("NAME", definition.getName());
            result = result.replaceAll("TYPE", definition.getType().toString());

            if (record != null) {
                String success = (record.getResult()) ? "success" : "failure";
                result = result.replaceAll("RESULT", success);
                result = result.replaceAll("CLASS", success + " summary-stub");
                result = result.replaceAll("TIME", record.getExecutionTime().replaceAll("_", " "));
            } else {
                result = result.replaceAll("RESULT", "not yet executed");
                result = result.replaceAll("CLASS", "not-yet-run");
                result = result.replaceAll("TIME", "n/a");
            }
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

            try (InputStreamReader isr = new InputStreamReader(is); BufferedReader reader = new BufferedReader(isr)) {
                return reader.lines().collect(Collectors.joining(System.lineSeparator()));
            }
        }
    }
}
