package uk.ac.cam.cares.jps.model;

/**
 * This is a model class which structure follows the result JSON from <a href="https://jsonplaceholder.typicode.com/todos/">...</a>{id}
 * And here is a sample result of <a href="https://jsonplaceholder.typicode.com/todos/2">...</a>
 * {
 * "userId": 1,
 * "id": 2,
 * "title": "quis ut nam facilis et officia qui",
 * "completed": false
 * }
 */
public class Todo {
    private final String userId;
    private final String id;
    private final String title;
    private final Boolean completed;

    /**
     * Constructor
     *
     * @param userId    User ID returned from the API.
     * @param id        ID returned from the API.
     * @param title     Title returned from the API.
     * @param completed A boolean indicator returned from the API.
     */
    public Todo(String userId, String id, String title, Boolean completed) {
        this.userId = userId;
        this.id = id;
        this.title = title;
        this.completed = completed;
    }

    /**
     * Getter method to return User ID.
     *
     * @return User ID returned from the API.
     */
    public String getUserId() {
        return this.userId;
    }

    /**
     * Getter method to return ID.
     *
     * @return ID returned from the API.
     */
    public String getId() {
        return this.id;
    }

    /**
     * Getter method to return title.
     *
     * @return Title returned from the API.
     */
    public String getTitle() {
        return this.title;
    }

    /**
     * Getter method to return a boolean indicator from the API.
     *
     * @return A boolean indicator returned from the API.
     */
    public Boolean getCompleted() {
        return this.completed;
    }
}
