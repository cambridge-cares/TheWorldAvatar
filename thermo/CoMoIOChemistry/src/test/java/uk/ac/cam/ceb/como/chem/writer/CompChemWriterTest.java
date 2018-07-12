package uk.ac.cam.ceb.como.chem.writer;

import org.junit.Ignore;
import uk.ac.cam.ceb.como.io.chem.file.writer.compchem.CompChemWriter;
import org.junit.Test;

import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundConverter;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.io.chem.file.parser.ChemFileParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.GaussianParser;

/**
 *
 * @author pb556
 */

public class CompChemWriterTest {
    
	/**
     * Test of write method, of class CMLWriter.
     * @throws Exception
     */
	
    @Test
    @Ignore
    public void testWrite() throws Exception {
    	
        GaussianParser gauReader =  new FrequencyParser();
//      GaussianParser<String> gParser = new GaussianParser<String>();
        
    	String filename = "src/test/resources/uk/ac/cam/ceb/como/gaussian/C2H5OH.g03";
        gauReader.set(filename);
        
        gauReader.parse();
//        Compound compound = gauReader.get();
        CompChemWriter cmlWriter = new CompChemWriter();
        cmlWriter.set("target/C2H5OH.cml");
        cmlWriter.setContent(CompoundConverter.convert(new CompChemWrapper((CompChem) gauReader.get()).getFinalMolecule()));
        cmlWriter.write();
//        cmlWriter.write("target/C2H5OH.cml", compound);
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