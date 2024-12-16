package uk.ac.cam.cares.jps.data;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.TodoWithUser;
import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.network.NetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * The TodoRepository class acts as a bridge between the ViewModel and the data sources
 * (network and local storage). It is used to combine data from different sources, namely
 * from TodoNetworkSource and UserNetworkSource. If there is no need to combine data, the
 * repository will simply pass the response from the data source to the upper level (ViewModel).
 */
public class TodoRepository implements GenericRepository<TodoWithUser> {
    private static final Logger LOGGER = Logger.getLogger(TodoRepository.class);
    private final NetworkSource<Todo> todoNetworkSource;
    private final NetworkSource<User> userNetworkSource;

    /**
     * Constructor
     *
     * @param todoNetworkSource The source for todo-related data, obtained through network calls.
     * @param userNetworkSource The source for user-related data, obtained through network calls.
     */
    public TodoRepository(NetworkSource<Todo> todoNetworkSource, NetworkSource<User> userNetworkSource) {
        // The instantiation of the network sources are done by dependency injection
        this.todoNetworkSource = todoNetworkSource;
        this.userNetworkSource = userNetworkSource;
        LOGGER.debug("The repository has been successfully set up...");
    }

    /**
     * Retrieves both to do and User information associated with the provided ID.
     *
     * @param id                 The ID of the to do.
     * @param repositoryCallback Callback to handle the result of the data retrieval.
     */
    public void getInfo(String id, RepositoryCallback<TodoWithUser> repositoryCallback) {
        LOGGER.debug("Retrieving to do and user information for " + id);
        todoNetworkSource.getData(id,
                todo -> userNetworkSource.getData(todo.getUserId(), user -> repositoryCallback.onSuccess(new TodoWithUser(todo, user)), repositoryCallback::onFailure),
                repositoryCallback::onFailure);
    }
}
