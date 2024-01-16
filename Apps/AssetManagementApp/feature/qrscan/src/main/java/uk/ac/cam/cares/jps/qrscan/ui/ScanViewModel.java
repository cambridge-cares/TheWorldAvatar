package uk.ac.cam.cares.jps.qrscan.ui;

import android.graphics.Rect;

import androidx.lifecycle.MutableLiveData;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.camera.CameraViewModel;

public class ScanViewModel extends CameraViewModel {

    private final Logger LOGGER = Logger.getLogger(ScanViewModel.class);
    private final MutableLiveData<Rect> bBox = new MutableLiveData<>();
    int imageWidth;
    int imageHeight;

    final Object lock = new Object();
    private String tentativeUrl = "";
    private String prevTentativeUrl = "";
    private final MutableLiveData<String> confirmedUrl = new MutableLiveData<>();

    ConfirmUrlThread confirmUrlThread;

    private class ConfirmUrlThread extends Thread {
        @Override
        public void run() {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }

            synchronized (lock) {
                if (prevTentativeUrl.equals(tentativeUrl)) {
                    confirmedUrl.postValue(tentativeUrl);
                    LOGGER.debug("State transit to " + UrlConfirmState.CONFIRMED.name());
                    state = UrlConfirmState.CONFIRMED;
                } else {
                    LOGGER.debug("State transit to " + UrlConfirmState.DETECTING.name());
                    state = UrlConfirmState.DETECTING;
                }
                tentativeUrl = "";
                prevTentativeUrl = "";
            }
        }
    }

    public void setBBoxAndResolution(Rect rect, int imageWidth, int imageHeight) {
        this.bBox.setValue(rect);
        this.imageWidth = imageWidth;
        this.imageHeight = imageHeight;
    }

    public MutableLiveData<Rect> getBBox() {
        return bBox;
    }

    public MutableLiveData<String> getConfirmedUrl() {
        return confirmedUrl;
    }

    enum UrlConfirmState {
        DETECTING,
        CONFIRMING,
        CONFIRMED
    }

    UrlConfirmState state = UrlConfirmState.DETECTING;
    private void setTentativeUrl(String url) {
        synchronized (lock) {
            tentativeUrl = url;
            if (prevTentativeUrl.isEmpty()) {
                prevTentativeUrl = url;
            }
        }

    }
    public void setUrl(String url) {
        LOGGER.debug("Current state: " + state.name() + " , url: " + url);
        switch (state) {
            case DETECTING:
                setTentativeUrl(url);
                if (!tentativeUrl.isEmpty() && (confirmUrlThread == null || !confirmUrlThread.isAlive())) {
                    confirmUrlThread = new ConfirmUrlThread();
                    confirmUrlThread.start();
                    LOGGER.debug("State transit to " + UrlConfirmState.CONFIRMING.name());
                    state = UrlConfirmState.CONFIRMING;
                }
                break;
            case CONFIRMING:
                // still can set tentative url, but will not trigger thread to confirm new tentative url
                // new state transition happens in ConfirmUrlThread and depends on the confirmation result
                setTentativeUrl(url);
                break;
            case CONFIRMED:
                // termination state
                break;
        }
    }

    public void resetUrlState() {
        state = UrlConfirmState.DETECTING;
        tentativeUrl = "";
        prevTentativeUrl = "";
        confirmedUrl.setValue("");
    }

}
