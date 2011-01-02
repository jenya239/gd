package com.turbostool.client.utils
{
public class Ort2d extends Vector2d
{
    public function Ort2d(phi:Number) {
        super(Math.cos(phi), Math.sin(phi));
    }

    override public function length():Number {
        return 1;
    }

    public static function createFromVector2d(v:Vector2d):Ort2d {
        var ort:Ort2d = new Ort2d(0);
        ort.myX = v.myX;
        ort.myY = v.myY;
        return ort;
    }
}
}