/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.cml.format;

import uk.ac.cam.ceb.como.openbabel.cml.format.InChISNon;
import uk.ac.cam.ceb.como.openbabel.cml.format.CanonicalSMILES;
import uk.ac.cam.ceb.como.openbabel.cml.format.SMILES;
import uk.ac.cam.ceb.como.openbabel.cml.format.InChI;
import uk.ac.cam.ceb.como.openbabel.cml.format.Format;
import uk.ac.cam.ceb.como.openbabel.cml.MockCMLMolecules;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class FormatTest {

    @Test
    public void InChISNonTest() throws Exception {
        Format f = new InChISNon();
        f.setCMLMolecule(MockCMLMolecules.getH2O());
        assert (f.getString().replace(System.getProperty("line.separator"), "").equals("InChI=1S/H2O/h1H2"));

        f.setCMLMolecule(MockCMLMolecules.getC2H4());
        assert(f.getString().replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H4/c1-2/h1-2H2"));

        f.setCMLMolecule(MockCMLMolecules.getC2H6O());
        assert(f.getString().replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3"));
    }

    @Test
    public void InChITest() throws Exception {
        Format f = new InChI();
        f.setCMLMolecule(MockCMLMolecules.getH2O());
        assert (f.getString().replace(System.getProperty("line.separator"), "").equals("InChI=1S/H2O/h1H2"));

        f.setCMLMolecule(MockCMLMolecules.getC2H4());
        assert(f.getString().replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H4/c1-2/h1-2H2"));

        f.setCMLMolecule(MockCMLMolecules.getC2H6O());
        assert(f.getString().replace(System.getProperty("line.separator"), "").equals("InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3"));
    }

    @Test
    public void CanonicalSMILESTest() throws Exception {
        Format f = new CanonicalSMILES();
        f.setCMLMolecule(MockCMLMolecules.getH2O());
        String eq = f.getString().replace(System.getProperty("line.separator"), "").trim();
        assert (f.getString().replace(System.getProperty("line.separator"), "").trim().equals("O"));

        f.setCMLMolecule(MockCMLMolecules.getC2H4());
        assert(f.getString().replace(System.getProperty("line.separator"), "").trim().equals("C=C"));

        f.setCMLMolecule(MockCMLMolecules.getC2H6O());
        assert(f.getString().replace(System.getProperty("line.separator"), "").trim().equals("CCO"));
    }

    @Test
    public void SMILESTest() throws Exception {
        Format f = new SMILES();
        f.setCMLMolecule(MockCMLMolecules.getH2O());
        assert (f.getString().replace(System.getProperty("line.separator"), "").trim().equals("O"));

        f.setCMLMolecule(MockCMLMolecules.getC2H4());
        assert(f.getString().replace(System.getProperty("line.separator"), "").trim().equals("C=C"));

        f.setCMLMolecule(MockCMLMolecules.getC2H6O());
        assert(f.getString().replace(System.getProperty("line.separator"), "").trim().equals("CCO"));
    }
}