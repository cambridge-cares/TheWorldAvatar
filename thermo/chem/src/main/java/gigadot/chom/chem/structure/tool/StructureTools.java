package gigadot.chom.chem.structure.tool;

import gigadot.chom.chem.moi.MOICalculator;
import gigadot.chom.chem.structure.Structure;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.model.brownie.Atom;
import gigadot.chom.model.chemistry.Element;
import gigadot.chom.model.util.ModelUtils;
import gigatools.lite.constant.PhysicalConstants;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Weerapong Phadungsukanan
 */
public class StructureTools {

//    public static Atom createAtom(String symbol) {
//        return new Atom(PeriodicTable.getElement(symbol));
//    }
    /**
     * Threshold at which MOI is considered to be zero. (= 0.1 of {@link PhysicalConstants.AtomicMOI})
     */
    public static final double MOITreshold = 1E-1 * PhysicalConstants.AtomicMOI;

    /**
     * Determine if a molecular structure is a linear molecule.
     *
     * @param structure JChemDocument object to test.
     * @return true if linear, else false.
     */
    public static boolean isLinear(Structure structure) {
        if (!isSingleAtom(structure)) {
            PMOI pmoi = calculatePrincipalMOI(structure);
            double[] pI = pmoi.get3ArrayMOI();
            return isMOIZero(pI[0]) || isMOIZero(pI[1]) || isMOIZero(pI[2]);
        } else {
            return false;
        }
    }

    public static List<Atom> getAtomList(Structure structure) {
        List<Atom> atoms = new ArrayList<Atom>();
        for (int i = 0; i < structure.getAtomCount(); i++) {
            final Atom atom = structure.getAtom(i);
            atoms.add(atom);
        }
        return atoms;
    }

    public static List<Atom> getImplodedAtomListByElement(Structure structure) {
        List<Atom> atomList = getAtomList(structure);
        Map<Element, Integer> elementCountMap = ModelUtils.getImplodedElementCountMapByElement(atomList);
        List<Atom> newAtomList =  new ArrayList<Atom>();
        for (Map.Entry<Element, Integer> entry : elementCountMap.entrySet()) {
            Element element = entry.getKey();
            Integer count = entry.getValue();
            Atom atom = new Atom(element);
            atom.setCount(count);
            newAtomList.add(atom);
        }
        return newAtomList;
    }

    /**
     * Determine if a molecular structure contains only one atom.
     *
     * @param structure JChemDocument object to test.
     * @return true if single atom, else false.
     */
    public static boolean isSingleAtom(Structure structure) {
        return structure.getAtomCount() == 1;
    }

    /**
     * Calculate the principal moment of inertia (PMOI) and its axles of a given
     * {@link org.jchem.structure.Compound}.
     *
     * @param structure Instance of Compound interface.
     * @return A clone of PrincipalMOI object.
     */
    public static PMOI calculatePrincipalMOI(Structure structure) {
        MOICalculator moiCal = new MOICalculator(structure);
        return moiCal.calculatePrincipalMOI();
    }

    /**
     * Check if value of given molecular MOI in SI units is equal to zero by comparing to {@link #MOITreshold}. The
     * value of MOI is zero if it is between +/- {@link #MOITreshold} exclusively.
     *
     * @param moi MOI in SI units. Typically, the magnitude is in order of 10<sup>-38</sup>.
     * @return true if MOI is between +/- {@link #MOITreshold}.
     */
    public static boolean isMOIZero(double moi) {
        return (-MOITreshold < moi) && (moi < MOITreshold);
    }
//    public static double[] centreOfMass(Structure structure) {
//        double[] CM = {0d, 0d, 0d};
//        for (int i = 0; i < structure.getAtomCount(); i++) {
//            Atom atom = structure.getAtom(i);
//            Element e = atom.getElement();
//            CM[0] += e.atomicWeight * atom.getX();
//            CM[1] += e.atomicWeight * atom.getY();
//            CM[2] += e.atomicWeight * atom.getZ();
//
//        }
//        ArrayMathUtils.copyAndScale(CM, CM, 1d / getWeigth());
//        return CM;
//    }
}
