package uk.ac.cam.cares.jps.agent.status.record;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;

/**
 * Stores, reads, and writes historic TestRecord instances.
 *
 * @author Michael Hillman
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class TestRecordStore {

    /**
     * List of historical test records.
     */
    @XmlElement
    private final List<TestRecord> records = new ArrayList<>();

    /**
     *
     */
    public synchronized List<TestRecord> getRecords() {
        return Collections.unmodifiableList(records);
    }

    /**
     * Add a new record to the store.
     *
     * @param record new record.
     */
    public synchronized void addRecord(TestRecord record) {
        records.add(record);
        Collections.sort(records);
    }

    /**
     *
     * @param definition
     * @return
     */
    public synchronized TestRecord getLatestRecord(TestDefinition definition) {
        for (int i = 0; i < records.size(); i++) {
            if (records.get(i).getDefinition().equals(definition)) {
                return records.get(i);
            }
        }
        return null;
    }

    /**
     *
     * @param definition
     * @return
     */
    public synchronized List<TestRecord> getRecordsForTest(TestDefinition definition) {
        List<TestRecord> matches = new ArrayList<>();
        for (int i = 0; i < records.size(); i++) {
            if (records.get(i).getDefinition().equals(definition)) {
                matches.add(records.get(i));
            }
        }
        return matches;
    }

}
// End of class.
