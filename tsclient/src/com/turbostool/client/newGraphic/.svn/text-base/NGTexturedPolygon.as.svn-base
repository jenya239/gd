package com.turbostool.client.newGraphic {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;
import com.turbostool.client.utils.collections.ReadOnlyCollection;

import flash.display.BitmapData;

public class NGTexturedPolygon extends NGDrawable {
    private var myTexture: BitmapData;
    private var myPolygonProperty: Polygon;
    private var myTexWidth: Number;
    private var myTexLength: Number;
    private var myRepeatProperty: Boolean;
    private var myTransparency: Boolean;
    private var myRadiusSqrProperty: Number;

    private var myGeometricCenterProperty: Vector2d;

    public function NGTexturedPolygon(frame: LocalFrame2d, height: Number, layer: Number, pol: Polygon,
                                      texture: BitmapData, texWidth: Number, texLength: Number,
                                      repeat: Boolean = true, transparent: Object = null) {
        super(frame, height, layer);

        myTexture = convertTexture(texture, transparent);

        myPolygonProperty = pol;
        normalize();

        myTexWidth = texWidth;
        myTexLength = texLength;
        myRepeatProperty = repeat;
        myTransparency = transparent == null ? myTexture.transparent : transparent as Boolean;
    }

    override public function get myWidth(): Number {
        var minX: Number = Number.POSITIVE_INFINITY;
        var maxX: Number = Number.NEGATIVE_INFINITY;
        var it: Iterator = myPolygon.vertexIterator();
        while (it.hasNext()) {
            var point: Vector2d = it.next() as Vector2d;
            maxX = Math.max(point.myX, maxX);
            minX = Math.min(point.myX, minX);
        }
        return maxX - minX;
    }

    override public function get myLength(): Number {
        var minY: Number = Number.POSITIVE_INFINITY;
        var maxY: Number = Number.NEGATIVE_INFINITY;
        var it: Iterator = myPolygon.vertexIterator();
        while (it.hasNext()) {
            var point: Vector2d = it.next() as Vector2d;
            maxY = Math.max(point.myY, maxY);
            minY = Math.min(point.myY, minY);
        }
        return maxY - minY;
    }

    override protected function drawContent(ngg: NGGraphics): void {
        ngg.drawPolygon(myTexture, myTexWidth, myTexLength, myPolygon, myRepeatProperty);
    }

    public function get myTextureLength(): Number {
        return myTexLength;
    }

    public function set myTextureLength(newLength: Number): void {
        myTexLength = newLength;
    }

    public function get myTextureWidth(): Number {
        return myTexWidth;
    }

    public function set myTextureWidth(newWidth: Number): void {
        myTexWidth = newWidth;
    }

    public function get myRepeat(): Boolean {
        return myRepeatProperty;
    }

    public function get myVertices(): Collection {
        return new ReadOnlyCollection(myPolygon.getVertices());
    }

    public function getPolyCenter(): Vector2d {
        return myFrame.getGlobal(myPolygon.getMiddlePoint());
    }

    public function getPolyMiddlePoint(): Vector2d {
        return myPolygon.getMiddlePoint();
    }

    public function setTexture(texture: BitmapData): void {
        myTexture = convertTexture(texture, myTransparency);
    }

    override public function clone(): NGDrawable {
        var newFrame: LocalFrame2d = myFrame.clone() as LocalFrame2d;
        var newPoly: Polygon = myPolygon.clone() as Polygon;
        var newTexture: BitmapData = copyTexture(myTexture);
        var result: NGTexturedPolygon = new NGTexturedPolygon(newFrame, myHeight, myLayer, newPoly, newTexture, myTexWidth, myTexLength, myRepeatProperty, myTransparency);
        return result;
    }

    public function get myPolygon():Polygon {
        return myPolygonProperty;
    }

    override public function get myRadiusSqr(): Number {
        return myRadiusSqrProperty;
    }

    override public function get myGeometricCenter(): Vector2d {
        return myGeometricCenterProperty;
    }

    public function normalize(): void {
        if (myPolygon.getVertices().length() == 0) {
            myRadiusSqrProperty = 0;
            myGeometricCenterProperty = Vector2d.getZero();
            return;
        }
        var middlePoint: Vector2d = myPolygon.getMiddlePoint();
        myRadiusSqrProperty = 0;
        var it: Iterator = myPolygon.vertexIterator();
        while (it.hasNext()) {
            var rSqr: Number = (it.next() as Vector2d).difference2d_shared(middlePoint).lengthSqr();
            if (myRadiusSqrProperty < rSqr) {
                myRadiusSqrProperty = rSqr;
            }
        }
        myGeometricCenterProperty = myFrame.getGlobal(middlePoint);
    }

    public function insertAfter(index: int, v: Vector2d): void {
        myPolygon.getVertices().insertAfter(index, v);
        normalize();
    }

    public function deleteIndex(index:int): void {
        myPolygon.removeVertex(index);
        normalize();
    }

    public function copyToVertex(vertexIndex : int, source: Vector2d): void {
        source.copyTo2d(myPolygon.getVertices().getItemAt(vertexIndex) as Vector2d);
        normalize();
    }

    public function set myPolygon(p: Polygon): void {
        myPolygonProperty = p;
        normalize();
    }

    override public function set myR(newR: Vector2d): void {
        normalize();
        super.myR = newR;
    }

    override public function set myAngle(newAngle: Number): void {
        normalize();
        super.myAngle = newAngle;
    }

}
}