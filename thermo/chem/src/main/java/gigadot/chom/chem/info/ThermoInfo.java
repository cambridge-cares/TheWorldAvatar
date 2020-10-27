package gigadot.chom.chem.info;

import gigadot.chom.compchem.info.Info;
import gigadot.chom.model.cookie.EnthalpyOfFormation;

/**
 *
 * @author Weerapong
 */
public interface ThermoInfo extends Info {

    EnthalpyOfFormation getHf();

    void setHf(EnthalpyOfFormation hf);

}
