package com.turbostool.client.newGraphic {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

public class NGContainer extends NGDrawable {
    private var myDrawables: Collection;

    public function NGContainer(frame: LocalFrame2d, height: Number, layer: Number, drawables: Collection) {
        super(frame, height, layer);
        myDrawables = drawables;
    }

    override protected function drawContent(ngg:NGGraphics):void {
        var it: Iterator = myDrawables.iterator();
        while (it.hasNext()) {
            (it.next() as NGDrawable).draw(ngg);
        }
    }

    override public function get myWidth(): Number {
        var minX: Number = Number.POSITIVE_INFINITY;
        var maxX: Number = Number.NEGATIVE_INFINITY;
        var it:Iterator = myDrawables.iterator();
        while (it.hasNext()) {
            var ngd: NGDrawable = it.next() as NGDrawable;
            var frame:LocalFrame2d = ngd.myFrame;
            var w2:Number = ngd.myWidth / 2;
            var l_2:Number = ngd.myLength / 2;
            var delta: Number = Math.max(Math.abs(w2 * Math.cos(frame.myAngle)), Math.abs(l_2 * Math.sin(frame.myAngle)));
            maxX = Math.max(frame.myR.myX + delta, maxX);
            minX = Math.min(frame.myR.myX - delta, minX);
        }
        return maxX - minX;
    }

    override public function get myLength(): Number {
        var minY: Number = Number.POSITIVE_INFINITY;
        var maxY: Number = Number.NEGATIVE_INFINITY;
        var it:Iterator = myDrawables.iterator();
        while (it.hasNext()) {
            var ngd: NGDrawable = it.next() as NGDrawable;
            var frame:LocalFrame2d = ngd.myFrame;
            var w2:Number = ngd.myWidth / 2;
            var l_2:Number = ngd.myLength / 2;
            var delta: Number = Math.max(Math.abs(w2 * Math.sin(frame.myAngle)), Math.abs(l_2 * Math.cos(frame.myAngle)));
            maxY = Math.max(frame.myR.myY + delta, maxY);
            minY = Math.min(frame.myR.myY - delta, minY);
        }
        return maxY - minY;
    }
}
}