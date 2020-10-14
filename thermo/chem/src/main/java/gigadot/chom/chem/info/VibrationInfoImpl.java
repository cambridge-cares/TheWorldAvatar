package gigadot.chom.chem.info;

import gigadot.chom.chem.property.Vibration;
import gigadot.chom.compchem.info.Info;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author wp214
 */
public class VibrationInfoImpl implements VibrationInfo {

    /**
     * A list of FrequencyData object.
     */
    private List<Vibration> vibrationList = new ArrayList<Vibration>();

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
}
