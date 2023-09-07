package uk.ac.cam.cares.jps.data;

public interface RepositoryCallback {
    // called when data is ready to be consumed by ViewModel
    // ViewModel should implement this interface

    void onSuccess(Object result);
    void onFailure(Throwable error);
}
