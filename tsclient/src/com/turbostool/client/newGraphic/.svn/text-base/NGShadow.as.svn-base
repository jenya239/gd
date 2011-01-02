package com.turbostool.client.newGraphic {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.utils.Vector2d;

import flash.display.BitmapData;

public class NGShadow extends NGDrawable {
    private static const LIGHT_ANGLE: Number = Math.PI / 18;

    private var myTexture: BitmapData;
    private var myPolygon: Polygon;
    private var myTexWidth: Number;
    private var myTexLength: Number;
    private var myTransparency: Boolean;

    public function NGShadow(frame: LocalFrame2d, pol: Polygon, texture: BitmapData, texWidth: Number, texLength: Number, transparent: Boolean = false) {
        super(frame, 0, Number.POSITIVE_INFINITY);
        myTexture = texture;
        myPolygon = pol;
        myTexWidth = texWidth;
        myTexLength = texLength;
        myTransparency = transparent;
    }

    override protected function drawContent(ngg: NGGraphics): void {
        ngg.drawPolygon(myTexture, myTexWidth, myTexLength, myPolygon);
    }

    protected static function internalCreatePolyFromRect(width: Number, length: Number, height: Number): Polygon {
        var border: Number = height * Math.tan(LIGHT_ANGLE);
        var w2: Number = width / 2 + border;
        var l_2: Number = length / 2 + border;
        var poly: Polygon = new Polygon();
        poly.addVertex(new Vector2d(+w2, +l_2));
        poly.addVertex(new Vector2d(+w2, -l_2));
        poly.addVertex(new Vector2d(-w2, -l_2));
        poly.addVertex(new Vector2d(-w2, +l_2));
        return poly;
    }

    protected static function createShadowPoly(height: Number, poly:Polygon) : Polygon {
        var borderShift: Number = height * Math.tan(LIGHT_ANGLE);
        return poly.createParallel(borderShift);
    }

    public static function createRectShadow(rect: NGTexturedRect, texture: BitmapData = null): NGShadow {
        if (rect.myHeight <= 0) {
            return null;
        }
        var poly: Polygon = internalCreatePolyFromRect(rect.myWidth, rect.myLength, rect.myHeight);
        //var borderPoly: Polygon = createShadowPoly(rect);
        if (texture == null) {
            texture = new BitmapData(1, 1, false, 0xff0000);
        }
        //return new NGShadow(rect.myFrame, borderPoly, texture, rect.myWidth, rect.myLength);
        return null;
    }

    public static function createCarShadow(car: NGCarView, texture: BitmapData = null): NGShadow {
        if (texture == null) {
            texture = new BitmapData(1, 1, false, 0xffff00);
        }
        return new NGCarShadow(car, texture);
    }

}
}