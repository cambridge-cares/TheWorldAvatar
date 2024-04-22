package uk.ac.cam.cares.jps.model;


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
