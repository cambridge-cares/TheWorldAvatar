package uk.ac.cam.cares.jps.ui;

import android.content.Context;
import android.content.DialogInterface;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

public class UiUtils {
    public static void showNotImplementedDialog(Context context) {
        new MaterialAlertDialogBuilder(context)
                .setMessage(R.string.not_yet_implemented)
                .setPositiveButton(R.string.ok, (dialogInterface, i) -> {})
                .show();
    }
}
