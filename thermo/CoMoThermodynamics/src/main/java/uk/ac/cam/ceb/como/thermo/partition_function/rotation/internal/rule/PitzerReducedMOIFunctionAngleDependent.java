/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule;

import java.util.Collection;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 *
 * @author pb556
 */

public class PitzerReducedMOIFunctionAngleDependent extends Function<Double, Double> {

    private Bond bond = null;
    private Collection<Bond> allRotorBonds = null;
    private RotorRule rule = null;

    public PitzerReducedMOIFunctionAngleDependent(RotorRule rule, Bond bond, Collection<Bond> allRotorBonds) {
        this.rule = rule;
        this.bond = bond;
        this.allRotorBonds = allRotorBonds;
    }

//    protected Logger logger = Logger.getLogger(getClass());
//    private PitzerMOICalculator PMOICal = new PitzerMOICalculator();
//    private ThermoAnalyzable thermoAnalyzable = null;
//    private CMLMolecule mol = null;
//    private boolean considerCoupling = false;
//    private Bond bond = null;
//    private List<Bond> allRotorBonds = null;
//    
//    public PitzerReducedMOIFunctionAngleDependent(ThermoAnalyzable thermoAnalyzable, CMLMolecule mol, Bond bond, List<Bond> allRotorBonds, boolean considerCoupling) {
//        this.thermoAnalyzable = thermoAnalyzable;
//        this.bond = bond;
//        this.allRotorBonds = allRotorBonds;
//        this.mol = mol;
//        this.considerCoupling = considerCoupling;
//    }
//    
//    @Override
//    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
//        Compound compound = null;
//        if (thermoAnalyzable instanceof CMLMolecule) {
//            compound = CompoundConverter.convert((CMLMolecule) thermoAnalyzable);
//        } else if (thermoAnalyzable instanceof CompChem) {
//            IRCompChemWrapper ccw = new IRCompChemWrapper((CompChem) thermoAnalyzable);
//            compound = CompoundConverter.convert(ccw.getFinalMolecule());
//        } else if (thermoAnalyzable instanceof Compound) {
//            compound = (Compound) thermoAnalyzable;
//        }
//        if (compound != null) {
//            
//            // rotate the top
//            // CMLMolecule mol = CompoundConverter.convert(compound);
//            CMLBond torsBond = mol.getBondByAtomIds(bond.getAtomA().getId(), bond.getAtomB().getId());
//            RotationalMotionDiscretiser discretiser = new RotationalMotionDiscretiser();
//            discretiser.setReferenceGeometry(mol);
//            discretiser.setTorsionalBond(torsBond);
//            try {
//                CMLMolecule rotMol = discretiser.rotate(x).getCMLMolecule();
//                compound = CompoundConverter.convert(rotMol);
//            } catch (Exception ex) {
//                logger.warn("Molecular rotation could not be performed. Ground state geometry is used instead!");
//            }
//            PMOICal.setCompound(compound);
//            PMOICal.setAxisType(PitzerMOICalculator.AxisType.AXIS_OF_ROTOR_BOND);
//            // check publication!
//            if (considerCoupling) {
//                return PMOICal.getMOI(PitzerMOICalculator.MOIApproxLevel.COUPLING_INTERNAL_ROTATIONS_MOI, bond, allRotorBonds);
//            }
//            return PMOICal.getReducedMOI(bond);
//        }
//        return 0.0;
//    }
    
    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        return rule.getReducedMOI(x, bond, allRotorBonds);
    }
}