package uk.ac.cam.cares.jps.todo;

import android.util.Pair;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.TodoRepository;
import uk.ac.cam.cares.jps.model.*;

@HiltViewModel
public class TodoViewModel extends ViewModel {
    private TodoRepository todoRepository;

    private MutableLiveData<Todo> _todo = new MutableLiveData<>();
    private MutableLiveData<User> _user = new MutableLiveData<>();
    private MutableLiveData<Boolean> _hasError = new MutableLiveData<>();

    private LiveData<Todo> todo = _todo;
    private LiveData<User> user = _user;
    private LiveData<Boolean> hasError = _hasError;


    @Inject
    public TodoViewModel(TodoRepository todoRepository) {
        this.todoRepository = todoRepository;
    }

    public void getTodoAndUser() {
        todoRepository.getTodoAndUserInfo("2", new RepositoryCallback<>() {
            @Override
            public void onSuccess(Pair<Todo, User> result) {
                _todo.postValue(result.first);
                _user.postValue(result.second);
            }

            @Override
            public void onFailure(Throwable error) {
                _hasError.postValue(true);
            }
        });
    }

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
