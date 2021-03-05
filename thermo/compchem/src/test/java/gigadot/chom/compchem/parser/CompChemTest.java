package gigadot.chom.compchem.parser;

import gigadot.chom.compchem.CompChem;
import nu.xom.Document;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author wp214
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
