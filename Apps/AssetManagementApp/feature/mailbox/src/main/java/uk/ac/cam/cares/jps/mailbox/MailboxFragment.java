package uk.ac.cam.cares.jps.mailbox;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.data.Mail;
import uk.ac.cam.cares.jps.mailbox.databinding.FragmentMailboxBinding;

@AndroidEntryPoint
public class MailboxFragment extends Fragment {

    private FragmentMailboxBinding binding;
    private final Logger LOGGER = Logger.getLogger(MailboxFragment.class);
    private MailboxViewModel viewModel;
    private MailboxAdapter adapter;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentMailboxBinding.inflate(inflater);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        viewModel = new ViewModelProvider(this).get(MailboxViewModel.class);

        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.mailbox);

        viewModel.getMailList().observe(getViewLifecycleOwner(), mailList -> {
            LOGGER.info("get maillist");
            adapter.updateMails(mailList);
        });
        viewModel.retrieveMailListFromRepository();

        adapter = new MailboxAdapter();
        binding.mailList.setLayoutManager(new LinearLayoutManager(view.getContext()));
        binding.mailList.setAdapter(adapter);
    }
}
