package uk.ac.cam.cares.jps.utils;

/**
 * Interface for callbacks used by repositories to handle data retrieval results.
 * ViewModel classes should implement this interface to receive data from repositories.
 *
 * @param <T> The type of data returned by the callback.
 */
public interface RepositoryCallback<T> {
    /**
     * Called when data is successfully retrieved and ready to be consumed by ViewModel.
     *
     * @param result The data retrieved from the repository.
     */
    void onSuccess(T result);

    /**
     * Called when an error occurs during data retrieval.
     *
     * @param error The error that occurred during data retrieval.
     */
    void onFailure(Throwable error);
}
