package com.turbostool.client.game.components
{
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.Wheel;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;

public class CarTraceInfo
{
    public static const TRACES_COUNT: Number = 10;

    private var myPreviousWheelCoordinate:ArrayList = new ArrayList();
    private var myCar:Car;
    private var myWorld: RaceWorld;

    private static const MIN_SLIP_COUNT: Number = 1;

    public function CarTraceInfo(car:Car, raceModel: RaceModel, iterations:int = TRACES_COUNT) {
        myCar = car;
        myWorld = raceModel.getRaceWorld();
        myPreviousWheelCoordinate.addItem(new Vector2d(0, 0));
        myPreviousWheelCoordinate.addItem(new Vector2d(0, 0));
        myPreviousWheelCoordinate.addItem(new Vector2d(0, 0));
        myPreviousWheelCoordinate.addItem(new Vector2d(0, 0));
        updateCoordinates();
    }

    private function updateCoordinates():void {
        var wheels:Collection = myCar.carModel.getChassis().getWheels();
        for (var i:int = 0; i < 4; i++) {
            var wheel:Wheel = wheels.getItemAt(i) as Wheel;
            wheel.myR.copyTo2d(myPreviousWheelCoordinate.getItemAt(i) as Vector2d);
        }
    }

    private var wheelNotInGrip: Array = [0, 0, 0, 0];

    public function updateTraceQueue():void {
        var wheels:Collection = myCar.carModel.getChassis().getWheels();
        var debugContactVY : String = "";
        for (var i:int = 0; i < 4; i++) {
            var wheel:Wheel = wheels.getItemAt(i) as Wheel;
            if (! wheel.myRoadGrip && Math.abs(wheel.getContactVY()) > 1.5) {
                myWorld.addTrace((myPreviousWheelCoordinate.getItemAt(i) as Vector2d), wheel.myR);
            }
        }

        updateCoordinates();
    }
}
}