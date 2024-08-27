package uk.ac.cam.cares.jps.data;

import static org.junit.Assert.assertEquals;

import com.android.volley.VolleyError;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.data.testdouble.FakeTodoNetworkSource;
import uk.ac.cam.cares.jps.data.testdouble.FakeUserNetworkSource;
import uk.ac.cam.cares.jps.model.TodoWithUser;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

public class TodoRepositoryTest {
    private static TodoRepository todoRepository;
    private static final FakeTodoNetworkSource TODO_NETWORK_SOURCE = new FakeTodoNetworkSource();
    private static final FakeUserNetworkSource USER_NETWORK_SOURCE = new FakeUserNetworkSource();

    @Before
    public void setUp() {
        todoRepository = new TodoRepository(TODO_NETWORK_SOURCE, USER_NETWORK_SOURCE);
    }

    @Test
    public void getInfo_Success() {
        todoRepository.getInfo("7", new RepositoryCallback<>() {
            @Override
            public void onSuccess(TodoWithUser result) {
                assertEquals(TODO_NETWORK_SOURCE.getId(), result.todo().getId());
                assertEquals(TODO_NETWORK_SOURCE.getUserId(), result.todo().getUserId());
                assertEquals(TODO_NETWORK_SOURCE.getTitle(), result.todo().getTitle());
                assertEquals(TODO_NETWORK_SOURCE.getCompleted(), result.todo().getCompleted());
                assertEquals(USER_NETWORK_SOURCE.getId(), result.user().getId());
                assertEquals(USER_NETWORK_SOURCE.getName(), result.user().getName());
                assertEquals(USER_NETWORK_SOURCE.getUserName(), result.user().getUsername());
                assertEquals(USER_NETWORK_SOURCE.getEmail(), result.user().getEmail());
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
            public void onSuccess(TodoWithUser result) {
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