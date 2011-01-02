package com.turbostool.client.game.components
{
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.HashMap;
import com.turbostool.client.utils.collections.Iterator;

public class WheelTraceModel
{
    private var myCarHashProperty:HashMap;
    private var myRaceModel:RaceModel;

    public function WheelTraceModel(raceModel:RaceModel) {
        myCarHashProperty = new HashMap();
        myRaceModel = raceModel;
        myCarHash.synchronizeWithCollection(raceModel.getCars(), createCarTraceInfo);
    }

    private function createCarTraceInfo(car: Car): CarTraceInfo {
        return new CarTraceInfo(car, myRaceModel);
    }

    public function getDynamics():Collection
    {
        return new ArrayList(this);
    }

    public function getCollidables():Collection
    {
        return new ArrayList();
    }

    public function saveState():void {

    }

    public function step(dt:Number):void {
    }

    public function rollback():void {
    }

    public function calcCoordinates(dt:Number):void {
    }

    public function updateTraces():void {
        myCarHash.synchronizeWithCollection(myRaceModel.getCars(), createCarTraceInfo);
        var it:Iterator = myCarHash.iterator();
        while (it.hasNext()) {
            var car:Car = it.next() as Car;
            var carTrace:CarTraceInfo = myCarHash.getValue(car) as CarTraceInfo;
            carTrace.updateTraceQueue();
        }
    }

    public function calcForces():void {
    }

    public function nullForces():void {
    }

    public function get myCarHash():HashMap {
        return myCarHashProperty;
    }
}
}