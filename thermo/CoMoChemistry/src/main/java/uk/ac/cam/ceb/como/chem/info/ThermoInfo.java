package uk.ac.cam.ceb.como.chem.info;

import uk.ac.cam.ceb.como.chem.property.EnthalpyOfFormation;
import uk.ac.cam.ceb.como.compchem.info.Info;

/**
 *
 * @author pb556
 */
public interface ThermoInfo extends Info {

    EnthalpyOfFormation getHf();

    void setHf(EnthalpyOfFormation hf);

}
