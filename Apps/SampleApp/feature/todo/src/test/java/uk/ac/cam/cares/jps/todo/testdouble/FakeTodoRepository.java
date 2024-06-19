package uk.ac.cam.cares.jps.todo.testdouble;

import uk.ac.cam.cares.jps.data.GenericRepository;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.TodoWithUser;
import uk.ac.cam.cares.jps.model.User;

public class FakeTodoRepository implements GenericRepository<TodoWithUser> {
    private static String SAMPLE_ID = "";
    private static final String SAMPLE_USER_ID = "6165";
    private static final String SAMPLE_TITLE = "Do you view this?";
    private static final boolean SAMPLE_COMPLETED = false;
    private static final String SAMPLE_NAME = "Unknown";
    private static final String SAMPLE_USERNAME = "user553";
    private static final String SAMPLE_EMAIL = "unknown@user.com";

    /**
     * Fake repository constructor accepting an id.
     */
    public FakeTodoRepository(String id) {
        SAMPLE_ID = id;
    }

    /**
     * A method to retrieve hardcoded user data for testing.
     */
    public void getInfo(String id, RepositoryCallback<TodoWithUser> repositoryCallback) {
        if (id.equals(SAMPLE_ID)) {
            Todo fakeTodo = new Todo(SAMPLE_USER_ID, SAMPLE_ID, SAMPLE_TITLE, SAMPLE_COMPLETED);
            User fakeUser = new User(SAMPLE_USER_ID, SAMPLE_NAME, SAMPLE_USERNAME, SAMPLE_EMAIL);
            repositoryCallback.onSuccess(new TodoWithUser(fakeTodo, fakeUser));
        } else {
            repositoryCallback.onFailure(new IllegalArgumentException());
        }
    }

    public String getId() {
        return SAMPLE_ID;
    }

    public String getUserId() {
        return SAMPLE_USER_ID;
    }

    public String getTitle() {
        return SAMPLE_TITLE;
    }

    public boolean getCompleted() {
        return SAMPLE_COMPLETED;
    }

    public String getName() {
        return SAMPLE_NAME;
    }

    public String getUserName() {
        return SAMPLE_USERNAME;
    }

    public String getEmail() {
        return SAMPLE_EMAIL;
    }
}
