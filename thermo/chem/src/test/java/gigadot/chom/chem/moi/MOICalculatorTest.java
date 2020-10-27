package gigadot.chom.chem.moi;

import gigadot.chom.model.brownie.Atom;
import gigadot.chom.chem.structure.Structure;
import gigadot.chom.chem.structure.StructureImpl;
import gigadot.chom.chem.structure.tool.StructureTools;
import org.apache.log4j.Logger;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author wp214
 */
public class MOICalculatorTest {
    private Logger logger = Logger.getLogger(getClass());

    @Test(expected = java.lang.NullPointerException.class)
    public void testSetNullStructure() {
        MOICalculator mc = new MOICalculator(null);
        double[] axis_direction = {0d, 1d, 0d};
        double[] ref_point = {0d, 0d, 0d};
        double moi = mc.calculateMOI(axis_direction, ref_point);
    }

    /**
     * Test of setStructure method, of class MOICalculator.
     */
    @Test
    public void testSetWrongAxisDirection() {
        Structure structure = new StructureImpl();
        Atom a1 = new Atom("C");
        structure.addAtom(a1);
        a1.setCoordinateInA(0d, 0d, 0d);
        MOICalculator mc = new MOICalculator(structure);
        double[] axis_direction1 = {0d};
        double[] ref_point = {0d, 0d, 0d};
        try {
            double moi = mc.calculateMOI(axis_direction1, ref_point);
        } catch (ArrayIndexOutOfBoundsException ex) {
            assertTrue(true);
        }
        // if array is bigger then only use the frist 3 elements.
        double[] axis_direction2 = {0d, 1d, 0d, 0d, 1d, 0d};
        double moi = mc.calculateMOI(axis_direction2, ref_point);
        assertEquals(0d, moi, 1e-55);
    }

    /**
     * Test of setStructure method, of class MOICalculator.
     */
    @Test
    public void testCalculateMOIForSingleAtom() {
        System.out.println("setStructure");
        Structure s1 = new StructureImpl();
        Atom a1 = new Atom("C");
        s1.addAtom(a1);
        a1.setCoordinateInA(1d, 0d, 0d);
        Structure s2 = new StructureImpl();
        Atom a2 = new Atom("C");
        s2.addAtom(a2);
        a2.setCoordinateInA(0d, 1d, 1d);
        MOICalculator mc1 = new MOICalculator(s1);
        MOICalculator mc2 = new MOICalculator(s2);
        double[] axis_direction = {0d, 1d, 0d};
        double[] ref_point = {0d, 0d, 0d};
        double moi1 = mc1.calculateMOI(axis_direction, ref_point);
        System.out.println(moi1);
        double moi2 = mc2.calculateMOI(axis_direction, ref_point);
        System.out.println(moi2);
        assertEquals(moi1, moi2, 1e-60);
        assertEquals(moi1, 1.9926465393960003E-46, 1e-60);
    }

    /**
     * Test of setStructure method, of class MOICalculator.
     */
    @Test
    public void testCalculateMOIForThreeAtoms() {
        System.out.println("setStructure");
        Structure s1 = new StructureImpl();
        Atom a1 = new Atom("C");
        s1.addAtom(a1);
        a1.setCoordinateInA(1d, 1d, 0d);
        Atom a2 = new Atom("C");
        s1.addAtom(a2);
        a2.setCoordinateInA(0d, 1.2d, 1d);
        Atom a3 = new Atom("C");
        s1.addAtom(a3);
        a3.setCoordinateInA(1d, 4d, 1d);
        MOICalculator mc1 = new MOICalculator(s1);
        double[] axis_direction = {0d, 1d, 0d};
        double[] ref_point = {0d, 0d, 0d};
        double moi1 = mc1.calculateMOI(axis_direction, ref_point);
        System.out.println(moi1);
        assertEquals(moi1, 1.9926465393960003E-46 * 4, 1e-60);
    }

    @Test
    public void warn() {
        logger.warn("calculatePMOI and MOI in non SI unit are not tested.");
    }
}
