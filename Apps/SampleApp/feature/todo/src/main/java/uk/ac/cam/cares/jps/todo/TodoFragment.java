package uk.ac.cam.cares.jps.todo;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.todo.databinding.TodoFragmentBinding;

@AndroidEntryPoint
public class TodoFragment extends Fragment {

    TodoFragmentBinding binding;
    TodoViewModel todoViewModel;

    // Inflate the XML layout for this fragment into a collection of View objects that can be manipulated in code.
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = TodoFragmentBinding.inflate(inflater);
        todoViewModel = new ViewModelProvider(this).get(TodoViewModel.class);

        return binding.getRoot();
    }

    // Actions to be taken after view is created
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        binding.setLifecycleOwner(this);
        binding.setTodoViewModel(todoViewModel);

        binding.topAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());
        binding.secondPageBt.setOnClickListener(view1 -> NavHostFragment.findNavController(this).navigate(R.id.action_todo_fragment_to_todo_second_setting));
        binding.getTodoBt.setOnClickListener(view1 -> todoViewModel.getTodoAndUser());

        todoViewModel.getTodo().observe(getViewLifecycleOwner(), todo -> {
            if (todo != null) {
                binding.todoLayout.setVisibility(View.VISIBLE);
            }
        });
        todoViewModel.getUser().observe(getViewLifecycleOwner(), user -> binding.userLayout.setVisibility(View.VISIBLE));
    }
}
