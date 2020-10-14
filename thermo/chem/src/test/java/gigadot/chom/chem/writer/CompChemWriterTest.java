package gigadot.chom.chem.writer;

import gigadot.chom.chem.reader.GaussianReader;
import gigadot.chom.chem.structure.Compound;
import org.junit.Test;

/**
 *
 * @author wp214
 */
public class CompChemWriterTest {
    /**
     * Test of write method, of class CMLWriter.
     * @throws Exception
     */
    @Test
    public void testWrite() throws Exception {
        GaussianReader gauReader =  new GaussianReader();
        String filename = "../bin/test/ThermoCalculator/C2H5OH.g03";
        Compound compound = gauReader.read(filename);
        CompChemWriter cmlWriter = new CompChemWriter();
        cmlWriter.write("target/C2H5OH.cml", compound);
    }
//
//    @Test
//    @Ignore
//    public void testRepeatedWrite() throws Exception {
//        GaussianReader gauReader =  new GaussianReader();
//        String filename = "../bin/test/ThermoCalculator/C2H5OH.g03";
//        Compound jdoc = gauReader.read(filename);
//        CMLWriter cmlWriter = new CMLWriter();
//        cmlWriter.write("target/C2H5OH-1.cml", jdoc);
//        CMLReader cmlReader = new CMLReader();
//        cmlReader.read("target/C2H5OH-1.cml", jdoc);
//        cmlWriter.write("target/C2H5OH-2.cml", jdoc);
//        Document doc1 = XMLTools.readXML("target/C2H5OH-1.cml");
//        Document doc2 = XMLTools.readXML("target/C2H5OH-2.cml");
//        XOMTestCase.assertEquals(doc1, doc2);
//        cmlReader.read("target/C2H5OH-2.cml", jdoc);
//        cmlWriter.write("target/C2H5OH-3.cml", jdoc);
//        doc1 = XMLTools.readXML("target/C2H5OH-2.cml");
//        doc2 = XMLTools.readXML("target/C2H5OH-3.cml");
//        XOMTestCase.assertEquals(doc1, doc2);
//    }

}
