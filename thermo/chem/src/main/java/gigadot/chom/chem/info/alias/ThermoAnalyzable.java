package gigadot.chom.chem.info.alias;

import gigadot.chom.chem.structure.Structure;
import gigadot.chom.chem.info.ThermoInfo;
import gigadot.chom.chem.info.VibrationInfo;
import gigadot.chom.compchem.info.ComputedInfo;
import gigadot.chom.compchem.info.MolecularInfo;

/**
 * should be moved if possible but doesn't seem so.
 * this interface is for Thermochemistry module only. Do not use it anywhere except in Structure CompCoundDelegator class
 * @author Weerapong
 */
public interface ThermoAnalyzable extends Structure, MolecularInfo, ComputedInfo, VibrationInfo, ThermoInfo  {

}
