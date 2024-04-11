package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import android.content.Context;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.coordinatorlayout.widget.CoordinatorLayout;

import com.google.android.material.bottomsheet.BottomSheetBehavior;

public abstract class BottomSheet{
    LinearLayoutCompat bottomSheet;

    public BottomSheet(@NonNull Context context) {
        init(context);
    }

    abstract void init(Context context);

    public LinearLayoutCompat getBottomSheet() {
        return bottomSheet;
    }
}
