package gigadot.chom.compchem;

import gigadot.chom.compchem.parser.GaussianParser;
import java.io.IOException;
import org.junit.Test;

/**
 *
 * @author wp214
 */
public class CompChemWrapperTest {

    /**
     * Test of getCompchem method, of class CompChemWrapper.
     */
    @Test
    public void testGetCompchem() throws IOException {
        System.out.println("getCompchem");
        CompChemWrapper instance = new CompChemWrapper();
        CompChemIOUtils.write(System.out, instance.getCompchem());
        String file = "src/test/resources/gau/como/SiO2-linear.g03";
        GaussianParser gp = new GaussianParser();
        CompChem cc = gp.parse(file);
        CompChemWrapper ccw = new CompChemWrapper(cc);
        CompChemIOUtils.write(System.out, ccw.getCompchem());
//        CompChem expResult = null;
//        CompChem result = instance.getCompchem();
//        assertEquals(expResult, result);
//        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
}
