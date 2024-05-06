package uk.ac.cam.cares.jps.model;

/**
 * This is a data model that encapsulate the JSON result from both user and to do API
 */
public class TodoWithUser {
    private final Todo todo;
    private final User user;

    /**
     * Constructor
     *
     * @param todo To do data model parsing data returned from the API.
     * @param user User data model parsing data returned from the API.
     */
    public TodoWithUser(Todo todo, User user) {
        this.todo = todo;
        this.user = user;
    }

    /**
     * Getter method to return User ID.
     *
     * @return User ID returned from the API.
     */
    public String getUserId() {
        return todo.getUserId();
    }

    /**
     * Getter method to return ID.
     *
     * @return ID returned from the API.
     */
    public String getId() {
        return todo.getId();
    }

    /**
     * Getter method to return title.
     *
     * @return Title returned from the API.
     */
    public String getTitle() {
        return todo.getTitle();
    }

    /**
     * Getter method to return a boolean indicator from the API.
     *
     * @return A boolean indicator returned from the API.
     */
    public Boolean getCompleted() {
        return todo.getCompleted();
    }

    /**
     * Getter method to return name of the user from the API.
     *
     * @return Name of the user returned from the API.
     */
    public String getName() {
        return user.getName();
    }

    /**
     * Getter method to return username from the API.
     *
     * @return Username returned from the API.
     */
    public String getUsername() {
        return user.getUsername();
    }

    /**
     * Getter method to return email of the user from the API.
     *
     * @return email of the user returned from the API.
     */
    public String getEmail() {
        return user.getEmail();
    }

    public Todo getTodo() {
        return todo;
    }

    public User getUser() {
        return user;
    }
}
