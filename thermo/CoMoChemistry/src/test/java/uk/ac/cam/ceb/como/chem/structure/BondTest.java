package uk.ac.cam.ceb.como.chem.structure;

import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author pb556
 */
public class BondTest {

    private Logger logger = Logger.getLogger(getClass().getName());
    private Bond bond = null;
    private Atom atomA = null, atomB = null;

    public BondTest() {
        atomA = new Atom("H");
        atomB = new Atom("H");
        bond = Bond.createBond(atomA, atomB, BondType.SINGLE);
    }

    /**
     * Test of getTorsionBarrierInkcal_Per_mol method, of class Bond.
     */
    @Test
    public void testGetTorsionBarrierInkcal_Per_mol() {
        logger.info("setTorsionBarrierInkcal_Per_mol");
        double torsionEInkcal_Per_mol = 1.0;
        bond.setTorsionBarrierInkcal_Per_mol(torsionEInkcal_Per_mol);
        logger.info("getTorsionBarrierInkcal_Per_mol");
        assertEquals(1, bond.getTorsionBarrierInkcal_Per_mol(), 0.0);
        logger.info("getTorsionBarrier");
        assertEquals(1 * PhysicalConstants.Cal * 1000, bond.getTorsionBarrier(), 0.0);
    }

    /**
     * Test of getTorsionBarrier method, of class Bond.
     */
    @Test
    public void testGetTorsionBarrier() {
        logger.info("setTorsionBarrier");
        double torsionE = 1.0;
        bond.setTorsionBarrier(torsionE);
        logger.info("getTorsionBarrier");
        assertEquals(1 / PhysicalConstants.Cal / 1000, bond.getTorsionBarrierInkcal_Per_mol(), 0.0);
        logger.info("getTorsionBarrier");
        assertEquals(1, bond.getTorsionBarrier(), 0.0);
    }

    /**
     * Test of getBondLength method, of class Bond.
     */
    @Test
    public void testGetBondLength() {
        logger.info("getBondLength");
        assertEquals(0, bond.getBondLength(), 0.0);
        atomA.setCoordinateInA(0, 0, 0);
        atomB.setCoordinateInA(1, 0, 0);
        assertEquals(1 * PhysicalConstants.A, bond.getBondLength(), 0.0);
        atomA.setCoordinateInA(0, 0, 0);
        atomB.setCoordinateInA(1, 1, 1);
        assertEquals(Math.sqrt(3) * PhysicalConstants.A, bond.getBondLength(), 0.0);
        atomA.setCoordinateInA(0, 0, 0);
        atomB.setCoordinateInA(1, 2, 5);
        assertEquals(Math.sqrt(30) * PhysicalConstants.A, bond.getBondLength(), 0.0);
    }

    /**
     * Test of getBondLengthInA method, of class Bond.
     */
    @Test
    public void testGetBondLengthInA() {
        logger.info("getBondLengthInA");
        assertEquals(0, bond.getBondLengthInA(), 0.0);
        atomA.setCoordinateInA(0, 0, 0);
        atomB.setCoordinateInA(1, 0, 0);
        assertEquals(1, bond.getBondLengthInA(), 0.0);
        atomA.setCoordinateInA(0, 0, 0);
        atomB.setCoordinateInA(1, 1, 1);
        assertEquals(Math.sqrt(3), bond.getBondLengthInA(), 0.0);
        atomA.setCoordinateInA(0, 0, 0);
        atomB.setCoordinateInA(1, 2, 5);
        assertEquals(Math.sqrt(30), bond.getBondLengthInA(), 0.0);
    }
}