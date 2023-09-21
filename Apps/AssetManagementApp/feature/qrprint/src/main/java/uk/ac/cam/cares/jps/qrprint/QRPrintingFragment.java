package uk.ac.cam.cares.jps.qrprint;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.data.PrintItemModel;
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

        ListUpdate printListUpdate = item -> {
            viewModel.removePrintingItem(item);

            if (item.getStatus() == null) {
                // item is added by input box
                return;
            }

            if (item.getStatus()) {
                viewModel.addPrintedItem(item);
            } else {
                viewModel.addUnprintedItem(item);
            }
        };
        RemovablePrintItemAdapter printItemAdapter = new RemovablePrintItemAdapter(viewModel.getPrintingList().getValue(), printListUpdate, R.drawable.ripple_remove_circle);
        printItemAdapter.setCurrentVisibility(View.GONE);
        binding.printingListRv.setLayoutManager(new LinearLayoutManager(requireContext()));
        binding.printingListRv.setAdapter(printItemAdapter);

        PrintItemAdapter printedItemAdapter = new PrintItemAdapter(viewModel.getPrintedList().getValue());
        binding.printedListRv.setLayoutManager(new LinearLayoutManager(requireContext()));
        binding.printedListRv.setAdapter(printedItemAdapter);

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
            PrintItemModel item = new PrintItemModel(binding.idEditText.getText().toString(), null, null, null);
            viewModel.addPrintingItem(item);

            binding.idEditText.setText("");
        });

        viewModel.getPrintingList().observe(getViewLifecycleOwner(), printItemAdapter::updateItems);
        viewModel.getPrintedList().observe(getViewLifecycleOwner(), printedItemAdapter::updateItems);
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
        void removeItemFromPrintList(PrintItemModel item);
    }
}
