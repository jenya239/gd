package com.turbostool.client.dynamicEngine
{
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.newGraphic.NGCamera;
import com.turbostool.client.newGraphic.NGGraphicEngine;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.Iterator;

public class CameraLink
{
    private var myTargetProperty:ICameraLinkable;

    public function get myTarget():ICameraLinkable
    {
        return myTargetProperty;
    }

    public function set myTarget(value:ICameraLinkable):void
    {
        myTargetProperty = value;
        myNGCamera.myFrame.myAngle = value.myFrame.myAngle;
        myPrevVelocity = myTarget.myVelocity.clone() as Vector2d;
    }

    private function get myNGCamera(): NGCamera
    {
        return myNGGEngine.getCamera();
    }

    private static const MIN_VEL:Number = 1;
    private static const LOOKUP_HEIGHT: Number = 45;
    private static const LOOKUP_DURATION: Number = 4 * 1000;
    private static const MAX_CAMERA_HEIGHT: Number = 60;
    private static const MIN_CAMERA_HEIGHT: Number = 30;
    private static const CAMERA_ZOOM_PERIOD: Number = 1.2;
    private static const SHAKE_AMPLITUDE: Number = 5.0;
    private static const SHAKE_TIME: Number = 0.4;
    private static const SHAKE_CAUSING_MIN_SPEED: Number = 15;
    private static const ANGULAR_VELOCITY: Number = Math.PI / 6;

    private var myCameraHeightSpeed: Number = 0;
    private var myBeginLowVelocityTime: Number = 0;
    private var myLookupMode: Boolean = true;

    private function updateHeight(dt: Number): void
    {
        var targetHeight: Number;
        if (myLookupMode)
        {
            targetHeight = LOOKUP_HEIGHT;
            if (myTarget.myVelocity.length() >= MIN_VEL)
            {
                myBeginLowVelocityTime = 0;
                myLookupMode = false;
            }
        }
        else
        {
            targetHeight = MAX_CAMERA_HEIGHT * (1 + myTarget.myVelocity.length() / 10 ) / 3;
            if (myTarget.myVelocity.length() < MIN_VEL)
            {
                if (myBeginLowVelocityTime == 0)
                {
                    myBeginLowVelocityTime = Utils.now();
                }
                else if (Utils.now() - myBeginLowVelocityTime > LOOKUP_DURATION)
                {
                    myLookupMode = true;
                }
            }
        }
        var acceleration: Number = 2 * ((targetHeight - myNGCamera.myHeight) - myCameraHeightSpeed * CAMERA_ZOOM_PERIOD) / Utils.sqr(CAMERA_ZOOM_PERIOD);
        myCameraHeightSpeed += acceleration * dt;
        myNGCamera.myHeight += myCameraHeightSpeed * dt;
        myNGCamera.myHeight = Utils.getNear(myNGCamera.myHeight, MIN_CAMERA_HEIGHT, MAX_CAMERA_HEIGHT);
    }


    public function setPolygon(pol:Vector2dSequence):void
    {
        myPieces = new Array();
        var it:Iterator = pol.pieceIterator();
        while (it.hasNext())
        {
            myPieces.push(it.next());
        }
    }

    public function findNearestCameraPathPiece(): Piece2d
    {
        var minDistance:Number = Number.MAX_VALUE;
        var currD:Number = 0;
        var res: Piece2d = null;
        for (var i:uint = 0; i < myPieces.length; i++)
        {
            currD = myPieces[i].distanceTo(myTarget.myFrame.myR);
            if (currD < minDistance)
            {
                minDistance = currD;
                res = myPieces[i];
            }
        }
        return res;
    }

    public function get myAngle(): Number
    {
        return myNGCamera.myFrame.myAngle;
    }

    private function getAngle0(angle: Number): Number
    {
        var pi2: Number = 2 * Math.PI;
        if (angle > 0)
        {
            return angle - pi2 * Math.floor(angle / pi2);
        }
        if (angle < 0)
        {
            return angle + pi2 * (1 + Math.floor(- angle / pi2));
        }
        return 0;
    }

    private var myCamAngularVelocity:Number = 0;
    private var myPieces:Array = new Array();
    private var myCameraPathPiece: Piece2d;

    private function updateAngle(dt: Number): void
    {//update angle
        myCameraPathPiece = findNearestCameraPathPiece();
        var newAngle:Number = getAngle0(myCameraPathPiece.getPieceVector().getAngle() - Math.PI / 2);
        var curAngle: Number = getAngle0(myNGCamera.myFrame.myAngle);
        var oldAngle: Number = curAngle;
        var delta: Number = ANGULAR_VELOCITY * dt;
        if (Math.abs(curAngle - newAngle) < Math.PI)
        {
            if (curAngle < newAngle)
            {
                curAngle += delta;
            }
            else
            {
                curAngle -= delta;
            }
        }
        else
        {
            if (curAngle < newAngle)
            {
                curAngle -= delta;
            }
            else
            {
                curAngle += delta;
            }
        }
        if (Utils.isBetween(newAngle, oldAngle, curAngle))
        {
            curAngle = newAngle;
        }
        myNGCamera.myFrame.myAngle = curAngle;
    }


    private var myNGGEngine: NGGraphicEngine = NGGraphicEngine.instance;
    private var myMaxVelocity: Number = 100;

    private function updateOffset(dt: Number): void
    {
        //car velocity in camera frame -> точке на экране
        var carV: Vector2d = myNGCamera.myFrame.getLocalNonLength(myTarget.myVelocity);
        if (carV.length() > myMaxVelocity)
        {
            myMaxVelocity = carV.length();
        }
        var maxOffsetX: Number = myNGGEngine.width * 0;
        var maxOffsetY: Number = myNGGEngine.height * 0.25;
        var targetOffsetX: Number = - Utils.sign(carV.myX) * Math.pow(Math.abs(carV.myX) / myMaxVelocity, 0.25) * maxOffsetX;
        var targetOffsetY: Number = - Utils.sign(carV.myY) * Math.pow(Math.abs(carV.myY) / myMaxVelocity, 0.25) * maxOffsetY;
        var offsetVelocity: Number = 50;
        var deltaX: Number = Utils.sign(targetOffsetX - myNGGEngine.centerOffsetX) * offsetVelocity * dt;
        var deltaY: Number = Utils.sign(targetOffsetY - myNGGEngine.centerOffsetY) * offsetVelocity * dt;
        if (Utils.isBetween(targetOffsetX, myNGGEngine.centerOffsetX, myNGGEngine.centerOffsetX + deltaX))
        {
            myNGGEngine.centerOffsetX = targetOffsetX;
        }
        else
        {
            myNGGEngine.centerOffsetX += deltaX;
        }
        if (Utils.isBetween(targetOffsetY, myNGGEngine.centerOffsetY, myNGGEngine.centerOffsetY + deltaY))
        {
            myNGGEngine.centerOffsetY = targetOffsetY;
        }
        else
        {
            myNGGEngine.centerOffsetY += deltaY;
        }
    }

    private var myPrevVelocity: Vector2d;
    private var myShakeTime: Number = -1;

    private function updateShaking(dt: Number): void
    {
        if (Utils.inRange(myShakeTime, 0, SHAKE_TIME))
        {
            myShakeTime += dt;
            myNGGEngine.centerOffsetX += (0.5 - Math.random()) * SHAKE_AMPLITUDE;
            myNGGEngine.centerOffsetY += (0.5 - Math.random()) * SHAKE_AMPLITUDE;
        }
        else
        {
            myShakeTime = -1;
            if (myPrevVelocity.difference2d_shared(myTarget.myVelocity).length() > SHAKE_CAUSING_MIN_SPEED)
            {
                myShakeTime = 0;
            }
        }
        myTarget.myVelocity.copyTo2d(myPrevVelocity);
    }


    public function step(dt:Number):void
    {
        updateHeight(dt);
        myTarget.myFrame.myR.copyTo2d(myNGCamera.myFrame.myR);
        updateAngle(dt);
        updateOffset(dt);
        updateShaking(dt);
    }
}
}