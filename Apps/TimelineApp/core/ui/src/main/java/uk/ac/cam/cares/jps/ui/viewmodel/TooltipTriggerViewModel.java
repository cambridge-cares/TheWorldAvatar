package uk.ac.cam.cares.jps.ui.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class TooltipTriggerViewModel extends ViewModel {

    private final MutableLiveData<Boolean> shouldTriggerTooltip = new MutableLiveData<>(false);
    public LiveData<Boolean> getShouldTriggerTooltips() {
        return shouldTriggerTooltip;
    }

    public void requestTooltipTrigger() {
        shouldTriggerTooltip.setValue(true);
    }

    public void clearTrigger() {
        shouldTriggerTooltip.setValue(false);
    }
}
