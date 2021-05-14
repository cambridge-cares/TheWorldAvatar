package uk.ac.cam.ceb.como.chem.info;

import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.property.Vibrations;

/**
 *
 * @author pb556
 */

public class VibrationInfoImpl implements VibrationInfo {

    /**
     * A list of FrequencyData object.
     */
    private final Vibrations vibrationList = new Vibrations();

    @Override
    public void clear() {
        vibrationList.clear();
    }

    @Override
    public Vibration getVibration(int index) {
        return vibrationList.get(index);
    }

    @Override
    public int getVibrationCount() {
        return vibrationList.size();
    }

    @Override
    public void addVibration(Vibration vibration) {
        vibrationList.add(vibration);
    }

    @Override
    public void clearVibrations() {
        vibrationList.clear();
    }

    @Override
    public void insertVibration(Vibration vibration, int i) {
        vibrationList.add(i, vibration);
    }

    @Override
    public void addVibrations(Vibrations vibrations) {
        vibrationList.addAll(vibrations);
    }

    @Override
    public Vibrations getVibrations() {
        return vibrationList;
    }
}