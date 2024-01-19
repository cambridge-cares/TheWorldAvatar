package uk.ac.cam.cares.jps.agent.gPROMS.test;

import junit.framework.TestCase;
import org.junit.Before;
import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgentException;

public class gPROMSAgentExceptionTest extends TestCase {

    private String errorMessage;

    @Before
    public void createErrorMessage() {
        errorMessage = "This is a gPROMSAgentException";
    }

    public void testThrowGPROMSAgentExceptionWithMessage() {
        try {
            throw new gPROMSAgentException(errorMessage);
        } catch (gPROMSAgentException exception) {
            assertEquals(errorMessage, exception.getMessage());
        }
    }

    public void testThrowGPROMSAgentExceptionWithMessageAndCause() {
        try {
            throw new gPROMSAgentException(errorMessage, null);
        } catch (gPROMSAgentException exception) {
            assertEquals(errorMessage, exception.getMessage());
            assertNull(exception.getCause());
        }
    }

}
