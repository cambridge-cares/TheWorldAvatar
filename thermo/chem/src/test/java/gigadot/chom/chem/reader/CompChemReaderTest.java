package gigadot.chom.chem.reader;

import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemIOUtils;
import gigadot.chom.compchem.parser.GaussianParser;
import java.io.File;
import org.junit.Test;

/**
 *
 * @author wp214
 */
public class CompChemReaderTest {

    /**
     * Test of read method, of class CompChemReader.
     * @throws Exception
     */
    @Test
    public void testRead() throws Exception {
        System.out.println("parse");
        String file = "src/test/resources/gigadot/chom/gaussian/(CH3)3SiO.g03";
        GaussianParser gp = new GaussianParser();
        CompChem cc = gp.parse(file);
        final String cmlfile = "target/(CH3)3SiO.cml";
        CompChemIOUtils.write(new File(cmlfile), cc);
        CompChemReader ccr = new CompChemReader();
        ccr.read(cmlfile);
        System.out.println(ccr);
    }
}
