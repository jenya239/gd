package com.turbostool.client.game.components.car{
import com.turbostool.client.Tracker;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.ChangeTriggerMessage;
import com.turbostool.client.net.messages.ReduceNitroCountRequest;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

public class CarController {
    private static const CLUTCH_CHANGE_MAX_VELOCITY: Number = 2;

    private var myCar:CarModel;
    private var myPreviousEvent:CarControlEvent;
    private const STOP_VELOCITY:Number = 0.5;
    private var _nitroUseTime:Number = 0;

    public function CarController(car:CarModel)
    {
        myCar = car;
        myPreviousEvent = new CarControlEvent(false, RudderControl.NONE, false, false, 0, CarControlEvent.GEAR_NONE);
    }

    public function onControl(event:CarControlEvent):void
    {
        myCar.myHandBrake = event.myHandBrake;
        myCar.myRudderControl.myAction = event.myRudderControl;
        if (event.myRudderControl == RudderControl.ANGLE)
        {
            myCar.myRudderControl.myAngle = event.myRudderAngle;
        }

        myCar.myAccelerate = false;
        myCar.myBrake = false;
        var carV:Number = myCar.getCarDirectionVelocity();

        if (event.myBrake)
        {
            if (carV <= 0)
            {
                if (!(carV == 0 && myPreviousEvent.myBrake))
                {
                    myCar.setGear(0);
                    myCar.myAccelerate = true;
                }
                else
                {
                    myCar = myCar;
                }
            }
            else
            {
                if ((myCar as CarModel).getChassis().isRails() && carV < STOP_VELOCITY)
                {
                    myCar.fullStop();
                }
                else
                {
                    myCar.myBrake = true;
                }
            }
        }

        if (event.myAccelerate)
        {
            if (carV > 0 || Utils.isZero(carV))
            {
                if (myCar.getGear() < 1)
                {
                    myCar.setGear(1);
                }
                myCar.myAccelerate = true;
            }
            else
            {
                if ((myCar as CarModel).getChassis().isRails() && (carV > - STOP_VELOCITY))
                {
                    myCar.fullStop();
                }
                else
                {
                    myCar.myBrake = true;
                }
            }
        }

        var newGear: int = myCar.getTransmission().myNumber + event.myGearBoxAction;
        if (myCar.getTransmission().validGearNumber(newGear))
        {
            myCar.getTransmission().myNumber = newGear;
        }

        if (event.myClutchChange)
        {
            myCar.myCarDynamicEngine.myClutch = !myCar.myCarDynamicEngine.myClutch;
        }

        if (event.myNitro)
        {
            if (checkNitro()) {
                SessionSocket.instance.sendMessage(new ReduceNitroCountRequest());
                SessionSocket.instance.sendMessage(new ChangeTriggerMessage(ChangeTriggerMessage.NITRO_TIP, -1));
                startNitro();
                Client.instance.modelsStorage.userInfo.nitroCount -= 1;
                _nitroUseTime = Utils.now();

                Tracker.instance.trackEvent("nitro", "use");
            }
        }

        myPreviousEvent = event;
    }

    public function get myNitroUseTime():Number {
        return _nitroUseTime;
    }

    protected function checkNitro():Boolean
    {
        var nitroCount:Number = Client.instance.modelsStorage.userInfo.nitroCount;
        var nitroTime:Number = (Client.instance.modelsStorage.globalInfo == null)
                ? 20000
                : Client.instance.modelsStorage.globalInfo.nitroTime;
        return (nitroCount > 0)
                && ( (Utils.now() - myNitroUseTime) > nitroTime);
    }

    protected function startNitro(): void
    {
        var angle:Number = myCar.getChassis().getHull().myFrame.myAngle - Math.PI / 2;
        var nitroImpulse:Number = (Client.instance.modelsStorage.globalInfo == null)
                ? 10
                : Client.instance.modelsStorage.globalInfo.nitroImpulse;
        var velMod:Number = nitroImpulse / 3.6;
        var deltaVel:Vector2d = Vector2d.createByAngle(angle, velMod);
        myCar.myCarDynamicEngine.myHull.myVelocity.add2d(deltaVel);
        myCar.myCarDynamicEngine.myChassis.updateWheelVelocity();
    }


}
}