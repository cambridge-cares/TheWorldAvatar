package uk.ac.cam.ceb.como.chem.reader;

import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.GaussianParser;
import uk.ac.cam.ceb.como.compchem.CompChem;
import java.io.File;
import org.junit.Test;

/**
 *
 * @author pb556
 */

public class CompChemParserTest {
	
//    /**
//     * Test of read method, of class CompChemReader.
//     * @throws Exception
//     */
//    @Test
//    public void testRead() throws Exception {
//        System.out.println("parse");
//        String file = "src/test/resources/uk/ac/cam/ceb/como/gaussian/(CH3)3SiO.g03";
//        GaussianParser gp = new GaussianParser();
//        CompChem cc = gp.parse(file);
//        final String cmlfile = "target/(CH3)3SiO.cml";
//        CompChemIOUtils.write(new File(cmlfile), cc);
//        CompChemParser ccr = new CompChemParser();
//        ccr.set(cmlfile);
//        ccr.parse();
//        System.out.println(ccr);
//    }
}