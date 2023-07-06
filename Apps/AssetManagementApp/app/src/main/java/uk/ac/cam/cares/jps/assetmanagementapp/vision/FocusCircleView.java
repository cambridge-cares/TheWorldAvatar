package uk.ac.cam.cares.jps.assetmanagementapp.vision;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.os.Handler;
import android.util.AttributeSet;
import android.view.View;

import androidx.core.content.ContextCompat;

import uk.ac.cam.cares.jps.assetmanagementapp.R;

public class FocusCircleView extends View {
    private Paint paint;
    private float x;
    private float y;
    private boolean isVisible;

    public FocusCircleView(Context context) {
        super(context);
        init();
    }

    public FocusCircleView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        paint = new Paint();
        paint.setColor(ContextCompat.getColor(getContext(), R.color.white));
        paint.setStyle(Paint.Style.STROKE);
        paint.setStrokeWidth(2f);
        isVisible = false;
    }


    public void showCircle(float x, float y) {
        this.x = x;
        this.y = y;
        isVisible = true;
        invalidate();

        // Schedule a task to hide the circle after 2 seconds
        new Handler().postDelayed(new Runnable() {
            @Override
            public void run() {
                hideCircle();
            }
        }, 2500);
    }

    public void hideCircle() {
        isVisible = false;
        invalidate();
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        if (isVisible) {
            canvas.drawCircle(x, y, 100, paint);
        }
    }
}
