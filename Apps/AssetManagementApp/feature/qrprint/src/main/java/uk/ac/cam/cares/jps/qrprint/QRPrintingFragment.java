package uk.ac.cam.cares.jps.qrprint;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.model.PrintItem;
import uk.ac.cam.cares.jps.qrprint.databinding.FragmentQrPrintBinding;

@AndroidEntryPoint
public class QRPrintingFragment extends Fragment {

    private FragmentQrPrintBinding binding;
    private QRPrintingViewModel viewModel;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentQrPrintBinding.inflate(inflater);
        viewModel = new ViewModelProvider(this).get(QRPrintingViewModel.class);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        ((TextView) view.findViewById(uk.ac.cam.cares.jps.ui.R.id.instance_title)).setText(R.string.qr_printing);
        view.findViewById(uk.ac.cam.cares.jps.ui.R.id.back_bt).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());

        ListUpdate printListUpdate = item -> {
            viewModel.removePrintingItem(item);
            viewModel.addUnprintedItem(item);
        };
        RemovablePrintItemAdapter printItemAdapter = new RemovablePrintItemAdapter(viewModel.getPrintingList().getValue(), printListUpdate, R.drawable.ripple_remove_circle);
        printItemAdapter.setCurrentVisibility(View.GONE);
        binding.printingListRv.setLayoutManager(new LinearLayoutManager(requireContext()));
        binding.printingListRv.setAdapter(printItemAdapter);

        ListUpdate unprintedListUpdate = item -> {
            viewModel.removeUnprintedItem(item);
            viewModel.addPrintingItem(item);
        };
        RemovablePrintItemAdapter unprintedItemAdapter = new RemovablePrintItemAdapter(viewModel.getUnprintedList().getValue(), unprintedListUpdate, R.drawable.ripple_add_circle);
        unprintedItemAdapter.setCurrentVisibility(View.VISIBLE);
        binding.unprintedListRv.setLayoutManager(new LinearLayoutManager(requireContext()));
        binding.unprintedListRv.setAdapter(unprintedItemAdapter);

        binding.editTv.setOnClickListener(getEditOnClickListener());

        binding.addIdBt.setOnClickListener(v -> {
            PrintItem item = new PrintItem(binding.idEditText.getText().toString().trim(), null, null);
            if (!item.getInventoryID().isEmpty()) {
                viewModel.addPrintingItem(item);

                binding.idEditText.setText("");
            }
        });

        binding.printBt.setOnClickListener(v -> {
            viewModel.printSelectedItems();
        });

        viewModel.getIsPrintSuccess().observe(getViewLifecycleOwner(), isPrintSuccess -> {
            if (isPrintSuccess) {
                Toast.makeText(requireContext(), R.string.print_success, Toast.LENGTH_SHORT).show();
            } else {
                Toast.makeText(requireContext(), R.string.print_failure, Toast.LENGTH_SHORT).show();
            }
        });

        viewModel.getPrintingList().observe(getViewLifecycleOwner(), printItemAdapter::updateItems);
        viewModel.getUnprintedList().observe(getViewLifecycleOwner(), unprintedItemAdapter::updateItems);

        viewModel.getAllPrintItems();
    }

    private View.OnClickListener getEditOnClickListener() {
        return view -> {
            setEditPrintListUIVisibility(View.VISIBLE);

            binding.editTv.setText(R.string.done);
            binding.editTv.setOnClickListener(getDoneOnClickListener());
        };
    }

    private View.OnClickListener getDoneOnClickListener() {
        return view -> {
            setEditPrintListUIVisibility(View.GONE);
            binding.editTv.setText(R.string.edit_bt_text);
            binding.editTv.setOnClickListener(getEditOnClickListener());
        };
    }

    private void setEditPrintListUIVisibility(int visibility) {
        binding.idSearchLayout.setVisibility(visibility);
        RecyclerView.LayoutManager layoutManager = binding.printingListRv.getLayoutManager();
        ((RemovablePrintItemAdapter) binding.printingListRv.getAdapter()).setCurrentVisibility(visibility);
        int firstVisibleItemPosition = ((LinearLayoutManager) layoutManager).findFirstVisibleItemPosition();
        int lastVisibleItemPosition = ((LinearLayoutManager) layoutManager).findLastVisibleItemPosition();

        if (firstVisibleItemPosition == -1 || lastVisibleItemPosition == -1) {
            return;
        }

        for (int i = firstVisibleItemPosition; i <= lastVisibleItemPosition; i++) {
            View itemView = layoutManager.findViewByPosition(i);
            itemView.findViewById(R.id.remove_bt).setVisibility(visibility);
        }
    }

    public interface ListUpdate {
        void removeItemFromPrintList(PrintItem item);
    }
}
