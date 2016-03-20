package uk.ac.cam.ceb.como.chem.info;

import uk.ac.cam.ceb.como.chem.property.EnthalpyOfFormation;

/**
 *
 * @author pb556
 */
public class ThermoInfoImpl implements ThermoInfo {

    private EnthalpyOfFormation hf;

    @Override
    public void clear() {
        hf = null;
    }

    @Override
    public EnthalpyOfFormation getHf() {
        return (hf != null) ? new EnthalpyOfFormation(hf) : null;
    }

    @Override
    public void setHf(EnthalpyOfFormation hf) {
        if (hf != null) {
            this.hf = new EnthalpyOfFormation(hf);
        } else {
            this.hf = null;
        }
    }
}
