package uk.ac.cam.ceb.como.compchem.parser;

import uk.ac.cam.ceb.como.compchem.CompChem;
import nu.xom.Document;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author pb556
 */
public class CompChemTest {

    /**
     * Test of addChemIDNamespace method, of class CompChem.
     */
    @Test
    public void testAddChemIDNamespace() {

        CompChem cc = new CompChem();
        System.out.println(cc.getJobListModules().size());
        System.out.println(new Document(cc).toXML());
    }
}
