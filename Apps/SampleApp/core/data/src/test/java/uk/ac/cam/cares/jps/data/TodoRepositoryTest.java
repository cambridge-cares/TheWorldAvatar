package uk.ac.cam.cares.jps.data;

import static org.junit.Assert.assertEquals;

import android.util.Pair;

import com.android.volley.VolleyError;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.data.testdouble.FakeTodoNetworkSource;
import uk.ac.cam.cares.jps.data.testdouble.FakeUserNetworkSource;
import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;

public class TodoRepositoryTest {
    private static TodoRepository todoRepository;
    private static final FakeTodoNetworkSource TODO_NETWORK_SOURCE = new FakeTodoNetworkSource();
    private static final FakeUserNetworkSource USER_NETWORK_SOURCE = new FakeUserNetworkSource();

    @Before
    public void setUp() {
        todoRepository = new TodoRepository(TODO_NETWORK_SOURCE, USER_NETWORK_SOURCE);
    }

    @Test
    public void getTodoAndUserInfo_Success() {
        Todo expectedTodo;
        todoRepository.getInfo("7", new RepositoryCallback<>() {
            @Override
            public void onSuccess(Pair<Todo, User> result) {
                assertEquals(TODO_NETWORK_SOURCE.getId(), result.first.getId());
                assertEquals(TODO_NETWORK_SOURCE.getUserId(), result.first.getUserId());
                assertEquals(TODO_NETWORK_SOURCE.getTitle(), result.first.getTitle());
                assertEquals(TODO_NETWORK_SOURCE.getCompleted(), result.first.getCompleted());
                assertEquals(USER_NETWORK_SOURCE.getId(), result.second.getId());
                assertEquals(USER_NETWORK_SOURCE.getName(), result.second.getName());
                assertEquals(USER_NETWORK_SOURCE.getUserName(), result.second.getUsername());
                assertEquals(USER_NETWORK_SOURCE.getEmail(), result.second.getEmail());
            }

            @Override
            public void onFailure(Throwable error) {
                // Empty callback as no error will be thrown
            }
        });
    }

    @Test
    public void getTodoAndUserInfo_Failure() {
        todoRepository.getInfo("8", new RepositoryCallback<>() {
            @Override
            public void onSuccess(Pair<Todo, User> result) {
                // Empty callback as it will be unsuccessful
            }

            @Override
            public void onFailure(Throwable error) {
                // Test will fail at the to do network source stage, and will not reach the user network source stage
                assertEquals(VolleyError.class, error.getClass());
                assertEquals(TODO_NETWORK_SOURCE.getErrorMessage(), error.getMessage());
            }
        });
    }
}