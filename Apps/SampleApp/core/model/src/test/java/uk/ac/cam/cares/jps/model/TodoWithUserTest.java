package uk.ac.cam.cares.jps.model;

import static org.junit.Assert.*;

import org.junit.Test;

public class TodoWithUserTest {
    @Test
    public void testConstructorAndGetters() {
        // Create a sample Todo
        Todo todo = new Todo("1", "481","Buy groceries", false);

        // Create a sample User
        User user = new User("1", "User One", "user123","User@april.biz");

        // Create a TodoWithUser instance
        TodoWithUser todoWithUser = new TodoWithUser(todo, user);

        // Test getters
        assertEquals(todo, todoWithUser.todo());
        assertEquals(user, todoWithUser.user());
    }
}