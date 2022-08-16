package uk.ac.cam.cares.jps.agent.status.record;

import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Map.Entry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import uk.ac.cam.cares.jps.agent.status.define.TestType;

/**
 * Stores, reads, and writes historic TestRecord instances.
 *
 * @author Michael Hillman
 */
public class TestRecordStoreMarshaller {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TestRecordStoreMarshaller.class);

    /**
     * Location of the test records file.
     */
    private static Path RECORD_FILE;

    /**
     * Determines the location of the record file.
     */
    private static void determineFileLocation() {
        String env = System.getenv("RECORD_FILE");

        if (env == null || env.isEmpty()) {
            String home = System.getProperty("user.home");
            RECORD_FILE = Paths.get(home, ".jps", "test-records.json");
        } else {
            RECORD_FILE = Paths.get(env);
        }
    }

    /**
     * Writes the current store of test records to the disk.
     */
    public static void writeRecords(TestRecordStore store) {
        if (RECORD_FILE == null) determineFileLocation();

        JSONArray recordArray = new JSONArray();

        for (TestRecord record : store.getRecords()) {
            JSONObject object = new JSONObject();

            object.put("result", record.getResult());
            object.put("time", record.getExecutionTime());

            JSONObject definition = new JSONObject();
            definition.put("name", record.getDefinition().getName());
            definition.put("type", record.getDefinition().getType());

            JSONObject inputs = new JSONObject();
            for (Entry<String, String> input : record.getDefinition().getInputs().entrySet()) {
                inputs.put(input.getKey(), input.getValue());
            }
            definition.put("inputs", inputs);

            object.put("definition", definition);
            recordArray.put(object);
        }

        try (FileWriter writer = new FileWriter(RECORD_FILE.toFile())) {
            writer.write(recordArray.toString());
            writer.flush();
            
            LOGGER.info("Test records file has been updated!");
        } catch (IOException ioException) {
            LOGGER.error("Could not write 'test-record-store.json' file!", ioException);
        }
    }

    /**
     * Reads the serialised record store into memory.
     *
     * @return
     */
    public static TestRecordStore readRecords() {
        if (RECORD_FILE == null) determineFileLocation();
        return readRecords(RECORD_FILE);
    }

    /**
     * Reads the serialised record store into memory.
     *
     * @param file
     *
     * @return
     */
    public static TestRecordStore readRecords(Path file) {
        TestRecordStore store = new TestRecordStore();

        try {
            String jsonContent = Files.readString(file);
            JSONArray recordArray = new JSONArray(jsonContent);

            for (int i = 0; i < recordArray.length(); i++) {
                JSONObject recordEntry = recordArray.getJSONObject(i);

                TestRecord testRecord = new TestRecord();
                testRecord.setResult(recordEntry.getBoolean("result"));
                testRecord.setExecutionTime(recordEntry.getString("time"));

                JSONObject definitionEntry = recordEntry.getJSONObject("definition");
                TestDefinition definition = new TestDefinition(
                        definitionEntry.getString("name"),
                        TestType.valueOf(definitionEntry.getString("type"))
                );

                JSONObject inputsEntry = definitionEntry.getJSONObject("inputs");
                Iterator<String> inputKeys = inputsEntry.keys();
                while (inputKeys.hasNext()) {
                    String innerKey = inputKeys.next();
                    definition.setInput(innerKey, inputsEntry.getString(innerKey));
                }

                testRecord.setDefinition(definition);
                store.addRecord(testRecord);
            }

            return store;

        } catch (FileNotFoundException | NoSuchFileException exception) {
            LOGGER.warn("Could not find existing test records, could be first execution?");
            return new TestRecordStore();

        } catch (Exception exception) {
            LOGGER.error("Could not read existing test records!", exception);
            return new TestRecordStore();
        }
    }

}
// End of class.
