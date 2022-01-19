package uk.ac.cam.cares.jps.aws;

public interface Watcher {

    boolean isStopped();

    void stopThread();

    void doOnChange();

    void setCallback(WatcherCallback cb);
}
