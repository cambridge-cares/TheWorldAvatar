package uk.ac.cam.cares.jps.ui.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class TooltipTriggerViewModel extends ViewModel {

    private final MutableLiveData<Boolean> shouldTriggerTooltip = new MutableLiveData<>(false);
    private boolean triggerPending = false;

    public LiveData<Boolean> getShouldTriggerTooltips() {
        return shouldTriggerTooltip;
    }

    public void requestTooltipTrigger() {
        triggerPending = true;
        shouldTriggerTooltip.setValue(true);
    }

    public void clearTrigger() {
        triggerPending = false;
        shouldTriggerTooltip.setValue(false);
    }

    public boolean isTriggerPending() {
        return triggerPending;
    }
}
