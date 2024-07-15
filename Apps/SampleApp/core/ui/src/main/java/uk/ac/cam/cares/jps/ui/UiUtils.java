package uk.ac.cam.cares.jps.ui;

import android.content.Context;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

/**
 * Shareable UI elements.
 */
class UiUtils {
    /**
     * Private constructor to prevent instantiation of this utility class.
     */
    private UiUtils() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Displays a dialog indicating that a feature is not yet implemented.
     *
     * @param context The context in which the dialog should be shown.
     */
    public static void showNotImplementedDialog(Context context) {
        new MaterialAlertDialogBuilder(context)
                .setMessage(R.string.not_yet_implemented)
                .setPositiveButton(R.string.ok, (dialogInterface, i) -> {
                })
                .show();
    }
}
