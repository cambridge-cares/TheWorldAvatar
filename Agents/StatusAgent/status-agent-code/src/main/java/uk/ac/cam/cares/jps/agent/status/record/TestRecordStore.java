package uk.ac.cam.cares.jps.agent.status.record;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Stores, reads, and writes historic TestRecord instances.
 *
 * @author Michael Hillman
 */
public class TestRecordStore {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(TestRecordStore.class);

    /**
     * List of historical test records.
     */
    private static final List<TestRecord> RECORDS = new ArrayList<>();

    /**
     * Add a new record to the store.
     *
     * @param record new record.
     */
    public static synchronized void addRecord(TestRecord record) {
        RECORDS.add(record);
    }

    /**
     * Writes the current store of test records to the disk.
     */
    public static void writeRecords() {
        JSONArray recordArray = new JSONArray();

        for (TestRecord record : RECORDS) {
            JSONObject object = new JSONObject();

            object.put("name", record.getDefinition().getName());
            object.put("group", record.getDefinition().getGroup());
            object.put("time", record.getExecutionTime());
            object.put("result", record.getResult());

            recordArray.put(object);
        }

        try ( FileWriter writer = new FileWriter("test-record-store.json")) {
            writer.write(recordArray.toString());
            writer.flush();
        } catch (IOException ioException) {
            LOGGER.error("Could not write 'test-record-store.json' file!", ioException);
        }
    }
    
}
// End of class.
