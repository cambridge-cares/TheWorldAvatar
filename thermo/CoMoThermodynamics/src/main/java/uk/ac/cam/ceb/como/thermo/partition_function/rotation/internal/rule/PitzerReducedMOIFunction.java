/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule;

import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.chem.moi.PitzerMOICalculator;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.compchem.CompChem;
import java.util.Collection;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundConverter;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 *
 * @author pb556
 */
public class PitzerReducedMOIFunction extends Function<Double, Double> {

    private PitzerMOICalculator PMOICal = new PitzerMOICalculator();
    private ThermoAnalyzable thermoAnalyzable = null;
    private Bond bond = null;
    private Collection<Bond> allRotorBonds = null;
    private boolean considerCoupling = false;
    
    public PitzerReducedMOIFunction(ThermoAnalyzable thermoAnalyzable, Bond bond, Collection<Bond> allRotorBonds, boolean considerCoupling) {
        this.thermoAnalyzable = thermoAnalyzable;
        this.bond = bond;
        this.allRotorBonds = allRotorBonds;
        this.considerCoupling = considerCoupling;
    }
    
    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        Compound compound = null;
        if (thermoAnalyzable instanceof CMLMolecule) {
            compound = CompoundConverter.convert((CMLMolecule) thermoAnalyzable);
        } else if (thermoAnalyzable instanceof CompChem) {
            IRCompChemWrapper ccw = new IRCompChemWrapper((CompChem) thermoAnalyzable);
            compound = CompoundConverter.convert(ccw.getFinalMolecule());
        } else if (thermoAnalyzable instanceof Compound) {
            compound = (Compound) thermoAnalyzable;
        }
        if (compound != null) {
            PMOICal.setCompound(compound);
            PMOICal.setAxisType(PitzerMOICalculator.AxisType.AXIS_OF_ROTOR_BOND);
            // check publication!
            if (considerCoupling) {
                return PMOICal.getMOI(PitzerMOICalculator.MOIApproxLevel.COUPLING_INTERNAL_ROTATIONS_MOI, bond, allRotorBonds);
            }
            return PMOICal.getReducedMOI(bond);
        }
        return 0.0;
    }
    
}
