package uk.ac.cam.ceb.como.chem.info;

import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.compchem.info.Info;

/**
 *
 * @author pb556
 */
public interface VibrationInfo extends Info {

    void addVibration(Vibration vibration);
    
    void addVibrations(Vibrations vibrations);

    void clearVibrations();

    int getVibrationCount();

    Vibration getVibration(int index);
    
    Vibrations getVibrations();

    void insertVibration(Vibration vibration, int i);
    
}
