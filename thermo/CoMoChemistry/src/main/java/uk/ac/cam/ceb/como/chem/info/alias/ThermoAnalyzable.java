package uk.ac.cam.ceb.como.chem.info.alias;

import uk.ac.cam.ceb.como.chem.structure.Structure;
import uk.ac.cam.ceb.como.chem.info.ThermoInfo;
import uk.ac.cam.ceb.como.chem.info.VibrationInfo;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfo;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfo;

/**
 * should be moved if possible but doesn't seem so.
 * this interface is for Thermochemistry module only. Do not use it anywhere except in Structure CompCoundDelegator class
 * @author pb556
 */
public interface ThermoAnalyzable extends Structure, MolecularInfo, ComputedInfo, VibrationInfo, ThermoInfo  {

}
