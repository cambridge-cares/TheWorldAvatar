package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static android.view.View.inflate;

import android.content.Context;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

import uk.ac.cam.cares.jps.timelinemap.R;

public class NormalBottomSheet extends BottomSheet {


    public NormalBottomSheet(@NonNull Context context, ViewGroup viewGroup) {
        super(context, viewGroup);
    }

    @Override
    void init(Context context, ViewGroup parent) {
        bottomSheet = inflate(context, R.layout.bottom_sheet_widget, parent).findViewById(R.id.bottom_sheet);
    }
}
