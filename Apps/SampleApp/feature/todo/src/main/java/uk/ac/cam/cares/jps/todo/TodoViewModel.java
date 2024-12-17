package uk.ac.cam.cares.jps.todo;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.GenericRepository;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.TodoWithUser;
import uk.ac.cam.cares.jps.model.User;

@HiltViewModel
public class TodoViewModel extends ViewModel {
    private final GenericRepository<TodoWithUser> todoRepository;
    // LiveData to hold these states
    private final MutableLiveData<Todo> _todo = new MutableLiveData<>();
    private final MutableLiveData<User> _user = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _hasError = new MutableLiveData<>();

    // Expose immutable LiveData to observe these states
    private final LiveData<Todo> todo = _todo;
    private final LiveData<User> user = _user;
    private final LiveData<Boolean> hasError = _hasError;

    /**
     * Constructor for TodoViewModel.
     *
     * @param todoRepository Repository for managing the data.
     */
    @Inject
    public TodoViewModel(GenericRepository<TodoWithUser> todoRepository) {
        this.todoRepository = todoRepository;
        this._hasError.postValue(false);
    }

    /**
     * Fetches the data.
     */
    public void getTodoAndUser() {
        todoRepository.getInfo("2", new RepositoryCallback<>() {
            @Override
            public void onSuccess(TodoWithUser result) {
                _todo.postValue(result.todo());
                _user.postValue(result.user());
            }

            @Override
            public void onFailure(Throwable error) {
                _hasError.postValue(true);
            }
        });
    }

    // Getters to retrieve the current data in immutable states
    // Prevent any accidental data manipulation to the backend if we only wish to observe data
    public LiveData<Todo> getTodo() {
        return todo;
    }

    public LiveData<User> getUser() {
        return user;
    }

    public LiveData<Boolean> getHasError() {
        return hasError;
    }
}
