package uk.ac.cam.cares.jps.ui.tooltip;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;

/**
 * View that draws a dimmed overlay with a transparent hole to highlight UI areas.
 */
public class TooltipOverlayView extends View {

    private final Paint dimPaint;
    private final Paint clearPaint;

    private RectF holeRect; // Highlight area
    private float cornerRadius = 24f; // Rounded corner radius in px

    public TooltipOverlayView(Context context) {
        this(context, null);
    }

    public TooltipOverlayView(Context context, AttributeSet attrs) {
        super(context, attrs);

        // Paint for dimmed background
        dimPaint = new Paint();
        dimPaint.setColor(0xCC000000); // slightly stronger semi-transparent black
        dimPaint.setStyle(Paint.Style.FILL);

        // Paint for clearing the hole
        clearPaint = new Paint();
        clearPaint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.CLEAR));
        clearPaint.setAntiAlias(true);

        // Required for PorterDuff to work properly
        setLayerType(View.LAYER_TYPE_HARDWARE, null);
    }

    /**
     * Sets the transparent area.
     */
    public void setHole(RectF rect) {
        this.holeRect = rect;
        invalidate();
    }

    /**
     * Sets rounded corner radius of the hole.
     */
    public void setCornerRadius(float radius) {
        this.cornerRadius = radius;
        invalidate();
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);

        // Fill entire screen with dim color
        canvas.drawRect(0, 0, getWidth(), getHeight(), dimPaint);

        // Cut transparent hole
        if (holeRect != null) {
            canvas.drawRoundRect(holeRect, cornerRadius, cornerRadius, clearPaint);
        }
    }

    /**
     * Passes touch events *only* through the transparent hole.
     */
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (holeRect != null && holeRect.contains(event.getX(), event.getY())) {
            return false; // Let touches pass through
        }
        return true; // Consume touches outside the hole
    }
}
