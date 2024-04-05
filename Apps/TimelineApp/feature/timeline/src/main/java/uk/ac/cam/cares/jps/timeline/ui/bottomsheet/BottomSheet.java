package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import android.content.Context;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.coordinatorlayout.widget.CoordinatorLayout;

import com.google.android.material.bottomsheet.BottomSheetBehavior;

public abstract class BottomSheet implements CoordinatorLayout.AttachedBehavior{
    LinearLayoutCompat bottomSheet;

    public BottomSheet(@NonNull Context context, ViewGroup parent) {
        init(context, parent);
    }

    abstract void init(Context context, ViewGroup parent);

    @NonNull
    @Override
    public CoordinatorLayout.Behavior<LinearLayoutCompat> getBehavior() {
        BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior = BottomSheetBehavior.from(bottomSheet);
        bottomSheetBehavior.setPeekHeight(200);
        bottomSheetBehavior.setFitToContents(false);
        return bottomSheetBehavior;
    }

    public LinearLayoutCompat getBottomSheet() {
        return bottomSheet;
    }
}
