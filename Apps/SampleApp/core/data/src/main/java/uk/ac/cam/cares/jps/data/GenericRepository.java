package uk.ac.cam.cares.jps.data;

import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * An interface for passing data from the data sources to the View model.
 */
public interface GenericRepository<T> {
    /**
     * Retrieves information associated with the provided ID.
     *
     * @param id                 The specified ID.
     * @param repositoryCallback Callback to handle the result of the data retrieval.
     */
    void getInfo(String id, RepositoryCallback<T> repositoryCallback);
}
