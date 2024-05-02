package uk.ac.cam.cares.jps.data;

import android.util.Pair;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.network.TodoNetworkSource;
import uk.ac.cam.cares.jps.network.UserNetworkSource;

/**
 * The TodoRepository class acts as a bridge between the ViewModel and the data sources
 * (network and local storage). It is used to combine data from different sources, namely
 * from TodoNetworkSource and UserNetworkSource. If there is no need to combine data, the
 * repository will simply pass the response from the data source to the upper level (ViewModel).
 */
public class TodoRepository {
    private static final Logger LOGGER = Logger.getLogger(TodoRepository.class);
    private final TodoNetworkSource todoNetworkSource;
    private final UserNetworkSource userNetworkSource;

    /**
     * Constructor
     *
     * @param todoNetworkSource The source for todo-related data, obtained through network calls.
     * @param userNetworkSource The source for user-related data, obtained through network calls.
     */
    public TodoRepository(TodoNetworkSource todoNetworkSource, UserNetworkSource userNetworkSource) {
        // The instantiation of the network sources are done by dependency injection
        this.todoNetworkSource = todoNetworkSource;
        this.userNetworkSource = userNetworkSource;
    }

    /**
     * Retrieves both Todo and User information associated with the provided ID.
     *
     * @param id                 The ID of the Todo.
     * @param repositoryCallback Callback to handle the result of the data retrieval.
     */
    public void getTodoAndUserInfo(String id, RepositoryCallback<Pair<Todo, User>> repositoryCallback) {
        todoNetworkSource.getData(id,
                todo -> userNetworkSource.getData(todo.getUserId(), user -> {
                    repositoryCallback.onSuccess(new Pair<>(todo, user));
                }, repositoryCallback::onFailure),
                repositoryCallback::onFailure);
    }
}
