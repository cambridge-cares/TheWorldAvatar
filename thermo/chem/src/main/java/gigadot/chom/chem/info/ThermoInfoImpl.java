package gigadot.chom.chem.info;

import gigadot.chom.compchem.info.Info;
import gigadot.chom.model.cookie.EnthalpyOfFormation;

/**
 *
 * @author wp214
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
