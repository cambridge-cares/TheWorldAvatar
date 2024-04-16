package uk.ac.cam.cares.jps.timeline;

import android.view.View;

import androidx.annotation.NonNull;
import androidx.coordinatorlayout.widget.CoordinatorLayout;

import com.mapbox.maps.plugin.scalebar.ScaleBarPlugin;

import uk.ac.cam.cares.jps.timelinemap.R;

public class MapLayoutBehavior extends CoordinatorLayout.Behavior {

    private ScaleBarPlugin scaleBarPlugin;

    public MapLayoutBehavior(ScaleBarPlugin scaleBarPlugin) {
        this.scaleBarPlugin = scaleBarPlugin;
    }

    @Override
    public boolean layoutDependsOn(@NonNull CoordinatorLayout parent, @NonNull View child, @NonNull View dependency) {
        return dependency.getId() == R.id.bottom_sheet_container;
    }

    @Override
    public boolean onDependentViewChanged(@NonNull CoordinatorLayout parent, @NonNull View child, @NonNull View dependency) {


        return super.onDependentViewChanged(parent, child, dependency);
    }
}
