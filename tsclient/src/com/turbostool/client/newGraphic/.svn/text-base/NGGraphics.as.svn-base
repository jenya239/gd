package com.turbostool.client.newGraphic
{
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.Iterator;
import com.turbostool.client.utils.collections.PointCache;

import flash.display.BitmapData;
import flash.display.GradientType;
import flash.display.Graphics;
import flash.display.SpreadMethod;
import flash.geom.Matrix;
import flash.geom.Point;

public class NGGraphics
{
    private var _graphics: Graphics;
    private var _smooth: Boolean = true;
    private var _transformStack: MatrixStack = new MatrixStack();

    private var _currentSequence: PointCache = new PointCache();

    private var _objectsPerFrame: Number = 0;

    public function get graphics():Graphics
    {
        return _graphics;
    }

    public function set graphics(value: Graphics): void
    {
        _graphics = value;
    }

    public function get myProductMatrix(): Matrix
    {
        return _transformStack.getProduct();
    }

    public function start(color: uint = 0, thickness: Number = 1):void
    {
        _graphics.lineStyle(thickness, color);
        _graphics.beginFill(0);//без этого рисование линий страшно глючит -- пытается все линии рисовать как одну фигуру с самопересечениями
    }

    public function finish(): void
    {
        _graphics.lineStyle();
        _graphics.endFill();

    }

    public function drawLineCertainly(x1: Number, y1: Number, x2: Number, y2: Number): void
    {
        _graphics.moveTo(x1, y1);
        _graphics.lineTo(x2, y2);
    }

    public function drawLine(x1: Number, y1: Number, x2: Number, y2: Number, color: uint = 0, sickness: Number = 1): void
    {
        var transformMatrix: Matrix = _transformStack.getProduct();
        _graphics.lineStyle(sickness, color);
        _graphics.beginFill(0);//без этого рисование линий страшно глючит -- пытается все линии рисовать как одну фигуру с самопересечениями

        clearSequence();
        addToSequence(transformMatrix, x1, y1);
        addToSequence(transformMatrix, x2, y2);
        drawSequence();

        _graphics.lineStyle();
        _graphics.endFill();
        //trace("drawLine([" + x1 + ", " + y1 + "], [" + x2 + ", " + y2 + "])");
    }

    public function drawRay(x0: Number, y0: Number, angle: Number, length: Number, color: uint = 0, sickness: Number = 1): void
    {
        drawLine(x0, y0, x0 + length * Math.cos(angle), y0 + length * Math.sin(angle), color, sickness);
    }

    public function drawShiftedTexturedRect(bitmap: BitmapData, width: Number, length: Number, bitmapWidth: Number, bitmapLength: Number, textureOffsetX: Number = 0, textureOffsetY: Number = 0): void
    {
        internalDrawTexturedRect(bitmap, width, length, bitmapWidth, bitmapLength, textureOffsetX, textureOffsetY, false);
    }

    private var _textureMatrix2: Matrix = new Matrix();//optimization?

    private function internalDrawTexturedRect(bitmap: BitmapData, width: Number, length: Number, bitmapWidth: Number, bitmapLength: Number, textureOffsetX: Number, textureOffsetY: Number, repeat: Boolean): void
    {
        var w2:Number = width / 2;
        var l_2:Number = length / 2;

        //var textureMatrix: Matrix = new Matrix();//optimization?
        _textureMatrix2.identity();
        var transformMatrix: Matrix = _transformStack.getProduct();

        _textureMatrix2.scale(bitmapWidth / bitmap.width, bitmapLength / bitmap.height);
        _textureMatrix2.translate(- bitmapWidth / 2 - textureOffsetX, - bitmapLength / 2 - textureOffsetY);

        _textureMatrix2.concat(transformMatrix);

        _graphics.beginBitmapFill(bitmap, _textureMatrix2, repeat, _smooth);

        clearSequence();
        addToSequence(transformMatrix, w2, l_2);
        addToSequence(transformMatrix, -w2, l_2);
        addToSequence(transformMatrix, -w2, -l_2);
        addToSequence(transformMatrix, w2, -l_2);
        addToSequence(transformMatrix, w2, l_2);
        drawSequence();

        _graphics.endFill();
    }

    private function pointLength(x: Number, y: Number): Number
    {
        if (x == 0.0 && y == 1.0) return 1.0;
        if (x == 1.0 && y == 0.0) return 1.0;
        return Math.sqrt(x*x + y*y);
    }

    private function convertTo_shared(ort1: Point, ort2: Point, vectorX: Number, vectorY: Number, resultPoint: Point): void
    {
        resultPoint.x = (ort1.x * vectorX + ort1.y * vectorY) / pointLength(ort1.x, ort1.y);
        resultPoint.y = (ort2.x * vectorX + ort2.y * vectorY) / pointLength(ort2.x, ort2.y);
    }

    private function convertFrom_shared(vec1: Point, vec2: Point, targetVector: Point, resultPoint: Point): void
    {
        convertTo_shared(vec1, vec2, 1, 0, _sharedPoint2);
        convertTo_shared(vec1, vec2, 0, 1, _sharedPoint3);
        convertTo_shared(_sharedPoint2, _sharedPoint3, targetVector.x, targetVector.y, resultPoint);
    }

    private var _sharedPoint1: Point = new Point();
    private var _sharedPoint2: Point = new Point();
    private var _sharedPoint3: Point = new Point();
    private var _sharedPoint4: Point = new Point();
    private var _sharedPoint5: Point = new Point();

    private var _sharedMatrix1: Matrix = new Matrix();

    public static function transformPoint(m: Matrix, v: Point, result: Point): void
    {
//        var r: Point = m.transformPoint(v);
//        result.x = r.x;
//        result.y = r.y;
        var x: Number = m.a*v.x + m.c*v.y + m.tx;
        var y: Number = m.b*v.x + m.d*v.y + m.ty;
        result.x = x;
        result.y = y;        
    }

    public static function copyMatrix(source: Matrix, dest: Matrix): void
    {
        dest.a = source.a;
        dest.b = source.b;
        dest.c = source.c;
        dest.d = source.d;
        dest.tx = source.tx;
        dest.ty = source.ty;
    }

    public function correctTextureMatrix(m: Matrix, bitmap: BitmapData): void
    {
        var cloneMatrix: Matrix = _sharedMatrix1;
        copyMatrix(m, cloneMatrix)
        //var cloneMatrix: Matrix = m.clone();
        cloneMatrix.tx = 0;
        cloneMatrix.ty = 0;

        //var cornerX: Point = new Point(bitmap.width, 0);
        //var cornerY: Point = new Point(0, bitmap.height);
        var corner1:Point =_sharedPoint4;
        var corner2:Point =_sharedPoint5;
        corner1.x = bitmap.width;
        corner1.y = 0.0;
        corner2.x = 0.0;
        corner2.y = bitmap.height;
        transformPoint(cloneMatrix, corner1, corner1);
        transformPoint(cloneMatrix, corner2, corner2);

        //var vectorT: Point = new Point(m.tx, m.ty);       
        //var vectorT2: Point = new Point();
        var vectorT2: Point = _sharedPoint1;
        convertTo_shared(corner1, corner2, m.tx, m.ty, vectorT2)
        vectorT2.x = vectorT2.x % pointLength(corner1.x, corner1.y);
        vectorT2.y = vectorT2.y % pointLength(corner2.x, corner2.y)
        convertFrom_shared(corner1, corner2, vectorT2, vectorT2);

        m.tx = vectorT2.x;
        m.ty = vectorT2.y;
    }

    private var _textureMatrix: Matrix = new Matrix();

    public function drawPolygon(bitmap: BitmapData, textureWidth: Number, textureLength: Number, polygon:Polygon, repeat: Boolean = false):void
    {
        if (polygon.getVertices().length() < 2)
        {
            return;
        }
        _objectsPerFrame++;

        var transformMatrix: Matrix = _transformStack.getProduct();
        _textureMatrix.identity();

        _textureMatrix.scale(textureWidth / bitmap.width, textureLength / bitmap.height);
        _textureMatrix.translate(- textureWidth / 2, - textureLength / 2);

        _textureMatrix.concat(transformMatrix);

        correctTextureMatrix(_textureMatrix, bitmap);

        _graphics.beginBitmapFill(bitmap, _textureMatrix, repeat, _smooth);

        clearSequence();
        var v: Vector2d = polygon.lastVertex();
        addToSequence(transformMatrix, v.myX, v.myY);
        var it:Iterator = polygon.vertexIterator();
        while (it.hasNext())
        {
            v = it.next() as Vector2d;
            addToSequence(transformMatrix, v.myX, v.myY);
        }
        drawSequence();

        _graphics.endFill();
    }

    public function drawTexturedRect(bitmap: BitmapData, width: Number, length: Number): void
    {
        internalDrawTexturedRect(bitmap, width, length, width, length, 0, 0, false);
    }

    public function drawTiledRect(bitmap: BitmapData, width: Number, length: Number, scale: Number): void
    {
        internalDrawTexturedRect(bitmap, width, length, bitmap.width * scale, bitmap.height * scale, 0, 0, true);
    }


    private var ppp: Point = new Point();

    private function addToSequence(transformMatrix: Matrix, x:Number, y:Number): void
    {
        ppp.x = x;
        ppp.y = y;
        var newPoint: Point = _currentSequence.addAndGetNextFreeItem();
        transformPoint(transformMatrix, ppp, newPoint);

    }

    private function clearSequence(): void
    {
        _currentSequence.clear();
    }

    private function drawSequence(): void
    {
        if (true)
        {
            var p: Point = _currentSequence.getPoint(0);
            _graphics.moveTo(p.x, p.y);
            for (var i: Number = 1; i < _currentSequence.size(); i++)
            {
                p = _currentSequence.getPoint(i);
                _graphics.lineTo(p.x, p.y);
            }
        }
    }

    private function overlapsWithScreen(): Boolean
    {
        return true;
    }

    public function getSmooth(): Boolean
    {
        return _smooth;
    }

    public function setSmooth(newSmooth: Boolean): void
    {
        _smooth = newSmooth;
    }

    public function pushTransform(m: Matrix): void
    {
        _transformStack.pushMatrix(m);
    }

    public function popTransform(): void
    {
        _transformStack.popMatrix();
    }

    public function clear(): void
    {
        if (_graphics != null)
        {
            _graphics.clear();
        }
        //trace(__objectsPerFrame);
        _objectsPerFrame = 0;
    }

    public function getTransform():Matrix
    {
        return _transformStack.getProduct();
    }


}
}