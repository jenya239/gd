package com.turbostool.client.dynamicEngine
{
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.utils.Utils;

public class BodyBorder
{
    public var myMinX:Number;
    public var myMaxX:Number;
    public var myMinY:Number;
    public var myMaxY:Number;

    public function equals(o:BodyBorder):Boolean {
        return Utils.equal(myMaxX, o.myMaxX) &&
               Utils.equal(myMaxY, o.myMaxY) &&
               Utils.equal(myMinX, o.myMinX) &&
               Utils.equal(myMinY, o.myMinY);
    }

    public function toString():String {
        return "minX = " + myMinX + "\n maxX = " + myMaxX + "\n minY = " + myMinY + "\n maxY = " + myMaxY;
    }

    public function canIntersect(piece:Piece2d):Boolean {
        /*
         return !(
         Math.min( piece.myBegin.myX, piece.myEnd.myX) > myMaxX
         || Math.min( piece.myBegin.myY, piece.myEnd.myY) > myMaxY
         || Math.max( piece.myBegin.myX, piece.myEnd.myX) < myMinX
         || Math.max( piece.myBegin.myY, piece.myEnd.myY) < myMinY
         );
         */
        return ( piece.myBegin.myX <= myMaxX || piece.myEnd.myX <= myMaxX )
                && ( piece.myBegin.myY <= myMaxY || piece.myEnd.myY <= myMaxY )
                && ( piece.myBegin.myX >= myMinX || piece.myEnd.myX >= myMinX )
                && ( piece.myBegin.myY >= myMinY || piece.myEnd.myY >= myMinY );
    }
}
}