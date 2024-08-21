package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import android.content.Context;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.coordinatorlayout.widget.CoordinatorLayout;

import com.google.android.material.bottomsheet.BottomSheetBehavior;

/**
 * Base class for bottom sheet in the map fragment
 */
public abstract class BottomSheet{
    LinearLayoutCompat bottomSheet;

    /**
     * Constructor of the class
     * @param context fragment context
     */
    public BottomSheet(@NonNull Context context) {
        init(context);
    }

    /**
     * Inflate ui for the bottomSheet layout object and init other variable of the bottom sheet object
     * @param context fragment context
     */
    abstract void init(Context context);

    /**
     * get the bottom sheet layout
     * @return bottom sheet layout
     */
    public LinearLayoutCompat getBottomSheet() {
        return bottomSheet;
    }
}
