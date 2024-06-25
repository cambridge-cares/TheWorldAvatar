package uk.ac.cam.cares.jps.addmaintenance;

import static uk.ac.cam.cares.jps.utils.SerializationUtils.deserializeStringToObject;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.datepicker.MaterialDatePicker;

import java.io.IOException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.addmaintenance.databinding.FragmentAddMaintenanceBinding;
import uk.ac.cam.cares.jps.model.AssetInfo;

@AndroidEntryPoint
public class AddMaintenanceFragment extends Fragment {
    FragmentAddMaintenanceBinding binding;
    AddMaintenanceViewModel viewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentAddMaintenanceBinding.inflate(inflater);
        viewModel = new ViewModelProvider(this).get(AddMaintenanceViewModel.class);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        try {
            AssetInfo assetInfo = (AssetInfo) deserializeStringToObject(getArguments().getString("assetinfo"));
            viewModel.initMaintenanceInfoFromAssetInfo(assetInfo);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }

        configureAppBar();
        initTextField();
        configureDatePicker();
        configureTextChangedListener();
        configureTextErrorMessage();

        viewModel.isMaintenanceUpdated.observe(getViewLifecycleOwner(), isMaintenanceUpdated -> {
            if (isMaintenanceUpdated) {
                Toast.makeText(requireContext(), R.string.add_maintenance_succeeded,Toast.LENGTH_SHORT).show();
                NavHostFragment.findNavController(this).navigateUp();
            } else {
                Toast.makeText(requireContext(), R.string.add_maintenance_failed,Toast.LENGTH_SHORT).show();
            }
        });
    }

    private void initTextField() {
        binding.idText.setText(viewModel.maintenance.getId());
        binding.lastServiceText.setText(viewModel.maintenance.getLastServiceDate());
        binding.nextServiceText.setText(viewModel.maintenance.getNextServiceDate());
        binding.intervalText.setText(viewModel.maintenance.getInterval());
        binding.serviceProviderText.setText(viewModel.maintenance.getServiceProvider());
    }

    private void configureTextChangedListener() {

        binding.lastServiceText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {
            }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

            @Override
            public void afterTextChanged(Editable editable) {
                viewModel.maintenance.setLastServiceDate(editable.toString());
            }
        });

        binding.nextServiceText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {
            }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

            @Override
            public void afterTextChanged(Editable editable) {
                viewModel.maintenance.setNextServiceDate(editable.toString());
            }
        });

        binding.serviceProviderText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

            @Override
            public void afterTextChanged(Editable editable) {
                viewModel.maintenance.setServiceProvider(editable.toString());
                viewModel.isServiceProviderMissing.setValue(false);
            }
        });

        binding.intervalText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

            @Override
            public void afterTextChanged(Editable editable) {
                viewModel.maintenance.setInterval(editable.toString());
            }
        });
    }

    private void configureDatePicker() {
        binding.lastServiceTextLayout.setEndIconOnClickListener(view1 -> {
            MaterialDatePicker<Long> datePicker = MaterialDatePicker.Builder.datePicker()
                    .setTitleText("Select dates")
                    .setSelection(MaterialDatePicker.todayInUtcMilliseconds())
                    .build();
            datePicker.addOnPositiveButtonClickListener(selection -> {
                Instant instant = Instant.ofEpochMilli(selection);
                LocalDateTime localDateTime = LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                binding.lastServiceText.setText(formatter.format(localDateTime));

                viewModel.isServiceDateMissing.setValue(false);
            });
            datePicker.show(requireActivity().getSupportFragmentManager(), null);
        });

        binding.nextServiceTextLayout.setEndIconOnClickListener(view1 -> {
            MaterialDatePicker<Long> datePicker = MaterialDatePicker.Builder.datePicker()
                    .setTitleText("Select dates")
                    .setSelection(MaterialDatePicker.todayInUtcMilliseconds())
                    .build();
            datePicker.addOnPositiveButtonClickListener(selection -> {
                Instant instant = Instant.ofEpochMilli(selection);
                LocalDateTime localDateTime = LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                binding.nextServiceText.setText(formatter.format(localDateTime));

                viewModel.isServiceDateMissing.setValue(false);
            });
            datePicker.show(requireActivity().getSupportFragmentManager(), null);
        });
    }

    private void configureAppBar() {
        ((TextView) binding.appBar.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.add_maintenance);
        binding.appBar.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());

        binding.doneBt.setOnClickListener(view -> {

            viewModel.addMaintenance();
        });
    }

    private void configureTextErrorMessage() {
        viewModel.isServiceDateMissing.observe(getViewLifecycleOwner(), isServiceDateMissing -> {
            if (isServiceDateMissing) {
                binding.lastServiceTextLayout.setError(getResources().getString(R.string.service_date_cannot_be_empty));
                binding.nextServiceTextLayout.setError(getResources().getString(R.string.service_date_cannot_be_empty));
            } else {
                binding.lastServiceTextLayout.setError(null);
                binding.nextServiceTextLayout.setError(null);
            }
        });

        viewModel.isTimeTravel.observe(getViewLifecycleOwner(), isTimeTravel -> {
            if (isTimeTravel) {
                binding.lastServiceTextLayout.setError(getResources().getString(R.string.next_service_date_before_last_service_date));
                binding.nextServiceTextLayout.setError(getResources().getString(R.string.next_service_date_before_last_service_date));
            } else {
                binding.lastServiceTextLayout.setError(null);
                binding.nextServiceTextLayout.setError(null);
            }
        });

        viewModel.isServiceProviderMissing.observe(getViewLifecycleOwner(), isServiceProviderMissing -> {
            if (isServiceProviderMissing) {
                binding.serviceProviderLayout.setError(getResources().getString(R.string.field_is_required));
            } else {
                binding.serviceProviderLayout.setError(null);
            }
        });
    }
}
