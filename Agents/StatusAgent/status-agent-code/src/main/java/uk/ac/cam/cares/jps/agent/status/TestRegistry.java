package uk.ac.cam.cares.jps.agent.status;

import uk.ac.cam.cares.jps.agent.status.define.TestDefinition;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import uk.ac.cam.cares.jps.agent.status.define.AvailabilityTestDefinition;

/**
 * Stores tests to be run.
 *
 * @author Michael Hillman
 */
public class TestRegistry {

    /**
     * Tests to be executed.
     */
    private static final List<TestDefinition> DEFINITIONS = new ArrayList<>();

    // Generate definitions of tests to run. In future, these definitions should be read
    // from a file so that tests can be added without having to regenerate Docker images.
    static {

        // =========================================================== //
        // ===== Availability tests for development KG endpoints ===== //
        // =========================================================== //
        AvailabilityTestDefinition dev_ontogasgrid = new AvailabilityTestDefinition(
                "dev/ontogasgrid",
                "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ontogasgrid/sparql"
        );
        DEFINITIONS.add(dev_ontogasgrid);

        AvailabilityTestDefinition dev_landuse = new AvailabilityTestDefinition(
                "dev/landuse",
                "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/landuse/sparql"
        );
        DEFINITIONS.add(dev_landuse);

        AvailabilityTestDefinition dev_backup = new AvailabilityTestDefinition(
                "dev/ts_backup",
                "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ts_backup/sparql"
        );
        DEFINITIONS.add(dev_backup);

        // ========================================================== //
        // ===== Availability tests for production KG endpoints ===== //
        // ========================================================== //
        AvailabilityTestDefinition prod_ontogasgrid = new AvailabilityTestDefinition(
                "prod/ontogasgrid",
                "https://kg.cmclinnovations.com/blazegraph_geo/namespace/ontogasgrid/sparql"
        );
        DEFINITIONS.add(prod_ontogasgrid);

        AvailabilityTestDefinition prod_landuse = new AvailabilityTestDefinition(
                "prod/landuse",
                "https://kg.cmclinnovations.com/blazegraph_geo/namespace/landuse/sparql"
        );
        DEFINITIONS.add(prod_landuse);
    }

    /**
     * Returns the list of defined tests.
     *
     * @return defined tests.
     */
    public static synchronized List<TestDefinition> getDefinedTests() {
        return Collections.unmodifiableList(DEFINITIONS);
    }

}
// End of class.
