package uk.ac.cam.cares.ogm.models.test;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.DatatypeModel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DatatypeModelTest {

    @Test
    public void testNewDatatypeModelMethod() throws NoSuchMethodException {

        Class datatypeModel = DatatypeModel.class;
        assertEquals(1, datatypeModel.getDeclaredMethods().length);
        assertNotNull(datatypeModel.getDeclaredMethod("getNode"));
    }
}
