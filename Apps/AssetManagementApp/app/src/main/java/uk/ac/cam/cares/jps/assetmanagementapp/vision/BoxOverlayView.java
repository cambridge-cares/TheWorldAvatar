package uk.ac.cam.cares.jps.assetmanagementapp.vision;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Matrix;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.view.View;

import androidx.core.content.ContextCompat;

import uk.ac.cam.cares.jps.assetmanagementapp.R;


public class BoxOverlayView extends View {
    private RectF rect;

    private final Matrix transformationMatrix = new Matrix();

    private int imageWidth;
    private int imageHeight;
    private float scaleFactor = 1.0f;
    private float postScaleWidthOffset;
    private float postScaleHeightOffset;

    public BoxOverlayView(Context context) {
        super(context);
        init();
    }

    public BoxOverlayView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setBackgroundColor(Color.TRANSPARENT);
    }

    public void setRect(Rect rect) {
        this.rect = new RectF(rect);
        invalidate();
    }

    public void setImageInfo(int imageWidth, int imageHeight) {
        this.imageWidth = imageWidth;
        this.imageHeight = imageHeight;
    }

    private float translateX(float x) {
//        if (isImageFlipped) {
//            return getWidth() - (scale(x) - postScaleWidthOffset);
//        } else {
//            return scale(x) - postScaleWidthOffset;
//        }

        return scale(x) - postScaleWidthOffset;
    }

    private float translateY(float y) {
        return scale(y) - postScaleHeightOffset;
    }

    public float scale(float imagePixel) {
        return imagePixel * scaleFactor;
    }

    private RectF translateRect(RectF rect) {
        float x0 = translateX(rect.left);
        float x1 = translateX(rect.right);
        rect.left = Math.min(x0, x1);
        rect.right = Math.max(x0, x1);
        rect.top = translateY(rect.top);
        rect.bottom = translateY(rect.bottom);
        return rect;
    }

    private void updateTransformationIfNeeded() {
        float viewAspectRatio = (float) getWidth() / getHeight();
        float imageAspectRatio = (float) imageWidth / imageHeight;
        postScaleWidthOffset = 0;
        postScaleHeightOffset = 0;
        if (viewAspectRatio > imageAspectRatio) {
            // The image needs to be vertically cropped to be displayed in this view.
            scaleFactor = (float) getWidth() / imageWidth;
            postScaleHeightOffset = ((float) getWidth() / imageAspectRatio - getHeight()) / 2;
        } else {
            // The image needs to be horizontally cropped to be displayed in this view.
            scaleFactor = (float) getHeight() / imageHeight;
            postScaleWidthOffset = ((float) getHeight() * imageAspectRatio - getWidth()) / 2;
        }

        transformationMatrix.reset();
        transformationMatrix.setScale(scaleFactor, scaleFactor);
        transformationMatrix.postTranslate(-postScaleWidthOffset, -postScaleHeightOffset);

//        if (isImageFlipped) {
//            transformationMatrix.postScale(-1f, 1f, getWidth() / 2f, getHeight() / 2f);
//        }
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);

        Paint paint = new Paint();
        paint.setColor(ContextCompat.getColor(getContext(), R.color.banner_color));
        paint.setStyle(Paint.Style.STROKE);
        paint.setStrokeWidth(10f);

        updateTransformationIfNeeded();

        if (rect != null) {
            canvas.drawRect(translateRect(rect), paint);
        }

    }
}
