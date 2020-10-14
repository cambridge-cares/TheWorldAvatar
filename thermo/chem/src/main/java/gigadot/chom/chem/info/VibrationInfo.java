package gigadot.chom.chem.info;

import gigadot.chom.chem.property.Vibration;
import gigadot.chom.compchem.info.Info;

/**
 *
 * @author Weerapong
 */
public interface VibrationInfo extends Info {

    void addVibration(Vibration vibration);

    void clearVibrations();

    int getVibrationCount();

    Vibration getVibration(int index);

    void insertVibration(Vibration vibration, int i);
    
}
