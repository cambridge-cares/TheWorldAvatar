package gigadot.chom.compchem.parser.gaussian;

import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemIOUtils;
import gigadot.chom.compchem.parser.GaussianParser;
import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import org.apache.commons.io.FileUtils;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author wp214
 */
public class GaussianParserTest {

    /**
     * Test of parse method, of class GaussianParser.
     * @throws Exception
     */
    @Test
    @Ignore
    public void testParse() throws Exception {
        System.out.println("parse");
        String file = "src/test/resources/gau/como/SiO2-linear.g03";
        GaussianParser instance = new GaussianParser();
        CompChem cc = instance.parse(file);
        CompChemIOUtils.write(new File("target/sio2.cml"), cc);
        cc = CompChemIOUtils.read(new File("target/sio2.cml"));
        CompChemIOUtils.write(System.out, cc);
    }

    @Test
    public void testParseHindered() throws Exception {
        System.out.println("parse");
        String file = "src/test/resources/gau/ti/C12H28O4TiFreq.g03";
        GaussianParser instance = new GaussianParser();
        CompChem cc = instance.parse(file);
        CompChemIOUtils.write(new File("target/C12H28O4TiFreq.cml"), cc);
        cc = CompChemIOUtils.read(new File("target/C12H28O4TiFreq.cml"));
        CompChemIOUtils.write(System.out, cc);
    }

    @Test
    @Ignore
    public void testReadFiles() throws Exception {
        Collection<File> gauFiles = FileUtils.listFiles(new File("src/test/resources/gau"), null, true);
        GaussianParser gauAR = new GaussianParser();
        for (Iterator<File> it = gauFiles.iterator(); it.hasNext();) {
            File gau = it.next();
            System.out.println("File : " + gau.getPath());
            gauAR.parse(gau.getPath());
        }
    }
}
