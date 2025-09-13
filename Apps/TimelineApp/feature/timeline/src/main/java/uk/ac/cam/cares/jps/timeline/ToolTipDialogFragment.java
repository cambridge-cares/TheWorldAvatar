package uk.ac.cam.cares.jps.timeline;

import android.app.Dialog;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.fragment.app.DialogFragment;

import uk.ac.cam.cares.jps.timelinemap.R;

public class ToolTipDialogFragment extends DialogFragment {
    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        return new AlertDialog.Builder(requireContext())
                .setTitle(R.string.tool_tips)
                .setMessage(R.string.tips_information)
                .setPositiveButton("OK", (dialog, which) -> dialog.dismiss())
                .create();
    }
}
