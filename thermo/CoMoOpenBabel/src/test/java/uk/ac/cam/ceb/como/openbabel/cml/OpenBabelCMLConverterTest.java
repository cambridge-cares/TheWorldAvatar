/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.cml;

import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelCMLConverter;
import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelException;
import java.io.File;
import org.apache.commons.io.FileUtils;
import org.junit.Test;
/**
 *
 * @author pb556
 */
public class OpenBabelCMLConverterTest {

    @Test
    public void tempDirTest() throws OpenBabelException {
        // check if folder exist
        OpenBabelCMLConverter.setTempDir(new File("test_data/"));
        OpenBabelCMLConverter.convert(MockCMLMolecules.getH2O().toXML(), "inchi", "--SNon");
        assert(new File("test_data/.jbabel/").exists());
        FileUtils.deleteQuietly(new File("test_data/.jbabel"));
    }

    public void executeTest() throws OpenBabelException {
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getH2O().toXML(), "inchi", "--SNon").
                replace(System.getProperty("line.separator"), "").equals("InChI=1S/H2O/h1H2"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H4().toXML(), "inchi", "--SNon").
                replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H4/c1-2/h1-2H2"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H6O().toXML(), "inchi", "--SNon").
                replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3"));
        
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getH2O().toXML(), "inchi", "").
                replace(System.getProperty("line.separator"), "").equals("InChI=1S/H2O/h1H2"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H4().toXML(), "inchi", "").
                replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H4/c1-2/h1-2H2"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H6O().toXML(), "inchi", "").
                replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3"));
        
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getH2O().toXML(), "smi", "").
                replace(System.getProperty("line.separator"), "").trim().equals("O"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H4().toXML(), "smi", "").
                replace(System.getProperty("line.separator"), "").trim().equals("C=C"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H6O().toXML(), "smi", "").
                replace(System.getProperty("line.separator"), "").trim().equals("CCO"));
        
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getH2O().toXML(), "can", "-I --unique").
                replace(System.getProperty("line.separator"), "").trim().equals("O"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H4().toXML(), "can", "-I --unique").
                replace(System.getProperty("line.separator"), "").trim().equals("C=C"));
        assert(OpenBabelCMLConverter.convert(
                MockCMLMolecules.getC2H6O().toXML(), "can", "-I --unique").
                replace(System.getProperty("line.separator"), "").trim().equals("CCO"));
    }
}