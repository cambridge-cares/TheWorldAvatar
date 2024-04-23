package uk.ac.cam.cares.jps.model;

/**
 * This is a model class which structure follows the result JSON from <a href="https://jsonplaceholder.typicode.com/todos/">...</a>{id}
 * And here is a sample result of <a href="https://jsonplaceholder.typicode.com/todos/2">...</a>
 * {
 *     "userId": 1,
 *     "id": 2,
 *     "title": "quis ut nam facilis et officia qui",
 *     "completed": false
 * }
 */
public class Todo {
    private String userId;
    private String id;
    private String title;
    private Boolean completed;

    public Todo(String userId, String id, String title, Boolean completed) {
        this.userId = userId;
        this.id = id;
        this.title = title;
        this.completed = completed;
    }

    public String getUserId() {
        return userId;
    }

    public String getId() {
        return id;
    }

    public String getTitle() {
        return title;
    }

    public Boolean getCompleted() {
        return completed;
    }
}
