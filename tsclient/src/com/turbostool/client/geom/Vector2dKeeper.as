package com.turbostool.client.geom
{
import com.turbostool.client.utils.Vector2d;

public class Vector2dKeeper
{
    public var myDifference1:Vector2d;
    public var myDifference2:Vector2d;
    public var myRotated1:Vector2d;
    public var myRotated2:Vector2d;

    public function Vector2dKeeper() {
        myDifference1 = Vector2d.getZero();
        myDifference2 = Vector2d.getZero();
        myRotated1 = Vector2d.getZero();
        myRotated2 = Vector2d.getZero();
    }

}
}