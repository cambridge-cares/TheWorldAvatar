package uk.ac.cam.cares.jps.todo;

import static org.junit.Assert.assertEquals;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;

import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.todo.testdouble.FakeTodoRepository;

public class TodoViewModelTest {
    @Rule
    public TestRule rule = new InstantTaskExecutorRule();

    @Test
    public void getTodoAndUser_Success(){
        // Set up repository with the hardcoded id of 2 for this sample app
        // In reality, the id may not always be 2 and can be selected from the Fragment
        FakeTodoRepository repository = new FakeTodoRepository("2");
        TodoViewModel subject = new TodoViewModel(repository);
        // Execute function
        subject.getTodoAndUser();
        // Retrieve and verify the live data values
        Todo subjectTodo = subject.getTodo().getValue();
        assertEquals(repository.getId(), subjectTodo.getId());
        assertEquals(repository.getUserId(), subjectTodo.getUserId());
        assertEquals(repository.getTitle(), subjectTodo.getTitle());
        assertEquals(repository.getCompleted(), subjectTodo.getCompleted());

        User subjectUser = subject.getUser().getValue();
        assertEquals(repository.getUserId(), subjectUser.getId());
        assertEquals(repository.getName(), subjectUser.getName());
        assertEquals(repository.getUserName(), subjectUser.getUsername());
        assertEquals(repository.getEmail(), subjectUser.getEmail());
    }

    @Test
    public void getTodoAndUser_Failure(){
        // Set up repository with invalid ID and view model
        FakeTodoRepository repository = new FakeTodoRepository("5");
        TodoViewModel subject = new TodoViewModel(repository);
        // Verify error-less initial state
        assertEquals(Boolean.FALSE, subject.getHasError().getValue());
        // Execute function
        subject.getTodoAndUser();
        // Verify error state has changed
        assertEquals(Boolean.TRUE, subject.getHasError().getValue());
    }
}