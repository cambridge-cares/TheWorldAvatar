package uk.ac.cam.cares.jps.data;

import android.util.Pair;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.network.TodoNetworkSource;
import uk.ac.cam.cares.jps.network.UserNetworkSource;

/**
 * Repository is used to combine data from different sources (network, local storage on different data)
 * This class demonstrates combining information acquired from TodoNetworkSource and UserNetworkSource.
 * If there is no need to combine data, the repository will simply pass the response from data source to upper level (ViewModel)
 */
public class TodoRepository {
    private static final Logger LOGGER = Logger.getLogger(TodoRepository.class);
    private TodoNetworkSource todoNetworkSource;
    private UserNetworkSource userNetworkSource;

    public TodoRepository(TodoNetworkSource todoNetworkSource, UserNetworkSource userNetworkSource) {
        // The instantiation of the network sources are done by dependency injection
        this.todoNetworkSource = todoNetworkSource;
        this.userNetworkSource = userNetworkSource;
    }

    public void getTodoAndUserInfo(String id, RepositoryCallback<Pair<Todo, User>> repositoryCallback) {
        todoNetworkSource.getTodo(id,
                todo -> {
                    userNetworkSource.getUser(todo.getUserId(), user -> {
                        repositoryCallback.onSuccess(new Pair<>(todo, user));
                    }, errorUser -> repositoryCallback.onFailure(errorUser));
                },
                errorTodo -> { // this block is a lambda simplification of `new Response.ErrorListener`, and can be simplified with lambda expression repositoryCallback::onFailure
                    repositoryCallback.onFailure(errorTodo);
                });
    }
}
