package com.turbostool.client.game
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.LocalLapTimeEvent;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.net.messages.LapTimeMessage;
import com.turbostool.client.utils.Ort2d;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

import flash.events.EventDispatcher;

public class RaceModel extends EventDispatcher
{
    private var _cars: ArrayList = new ArrayList();
    private var _raceWorld: RaceWorld;
    private var _carLastPosition: Vector2d = null;
    private var _lapNumber: int;

    public function checkEvents(): void
    {
        var it: Iterator = getCars().iterator();
        while (it.hasNext())
        {
            var car: Car = it.next() as Car;
            if (car.isLocal)
            {
                updateCar(car);
            }
        }
    }

    public function setRaceWorld(raceWorld: RaceWorld): void
    {
        _raceWorld = raceWorld;
    }

    public function set lapNumber(lapNumber: int): void
    {
        _lapNumber = lapNumber;
    }

    [Bindable]
    public function get lapNumber(): int
    {
        return _lapNumber;
    }

    private function updateCar(car: Car): void
    {
        var time: Number = Utils.now();
        if (car.lastPosition != null)
        {
            var piece: Piece2d = new Piece2d(car.lastPosition, car.myR);
            var startLine: Piece2d = _raceWorld.getStartLine();
            var curR: Vector2d = car.myR.difference2d(startLine.myBegin);
            var ort: Ort2d = startLine.getPieceVector().ortogonal2d().ort2d();
            if (startLine.intersects(piece))
            {
                var product: Number = ort.scalarProduct(curR);
                if (car.currentLap == -1)
                {
                    // if we cross line first time - our race begins
                    if (product > 0)
                    {
                        car.currentLap++;
                        dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.START, time, -1)));
                        EventManager.instance.dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.START, time, -1)));
                    }
                }
                else
                {
                    if (product <= 0)
                    {
                        // if we uncross line before we complete first lap - our race resets
                        car.currentLap--;
                        if (car.currentLap == -1)
                        {
                            dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.RESET, 0, -1)));
                            EventManager.instance.dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.RESET, 0, -1)));
                        }

                    }
                    // if any other cases we just increnment or decrement currentLap counter after cross or uncross correspondently
                    else
                    {
                        car.currentLap++;
                        // if currentLap reaches lapNumber in current race - we have finished (and then immidiatelly started again)
                        if (car.currentLap == _lapNumber)
                        {
                            dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.FINISH, time, -1)));
                            EventManager.instance.dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.FINISH, time, -1)));

                            dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.START, time, -1)));
                            EventManager.instance.dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.START, time, -1)));
                            car.currentLap = 0;
                        } else
                        {
                            dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.LAP, time, -1)));
                            EventManager.instance.dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.LAP, time, -1)));
                        }
                    }
                }
            }
        }
        car.lastPosition = car.myR.clone() as Vector2d;
    }

    public function getCars(): Collection
    {
        return _cars;
    }

    public function getCorrectPoint(): Vector2d
    {
        var startLine: Piece2d = _raceWorld.getStartLine();
        return startLine.myBegin.sum2d(startLine.getPieceVector().numberProduct2d(1 / 2));
    }

    public function getRaceWorld(): RaceWorld
    {
        return _raceWorld;
    }

    public function resetCarLastPosition(car: Car): void
    {
        car.lastPosition = null;
        car.currentLap = -1;
        dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.RESET, 0, -1)));
        EventManager.instance.dispatchEvent(new LocalLapTimeEvent(new LapTimeMessage(LapTimeMessage.RESET, 0, -1)));
    }

    [Bindable(event="changeLocalCar")]
    public function get localCar(): Car
    {
        var it:Iterator = getCars().iterator();
        while (it.hasNext())
        {
            var car:Car = it.next() as Car;
            if (car.isLocal)
            {
                return car;
            }
        }
        return null;
    }

    public function getRemoteCarByID(clientID: int): Car {
        var it: Iterator = getCars().iterator();
        while (it.hasNext()) {
            var car:Car = it.next() as Car;
            if (car.isLocal) {
                continue;
            }
            if (car.clientID == clientID) {
                return car;
            }
        }
        return null;
    }

    public function resetOneCar(car: Car): void
    {
        var carModel:CarModel;
        carModel = car.carModel;
        getRaceWorld().getStartPositionFrame().copyTo(carModel.myHull.myFrame);
        carModel.fullStop();
        carModel.myHull.saveState();
        carModel.getTransmission().myNumber = 1;
        carModel.engine.myEngineCoef = 0;
        carModel.myBrake = false;
        carModel.myAccelerate = false;
        resetCarLastPosition(car);
    }

    public function get lapsText(): String
    {
        if (localCar.currentLap == -1 || localCar.currentLap >= lapNumber)
        {
            return "";
        }
        else
        {
            return (localCar.currentLap + 1) + "/" + lapNumber;
        }
    }

    public function startEmergencyStop(): void
    {
        this.localCar.carModel.startEmergencyStop();
    }

    //

}
}
