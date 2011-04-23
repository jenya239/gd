package com.turbostool.client.dynamicEngine
{
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.components.WheelTraceModel;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarDynamicEngine;
import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.*;

public class DynamicEngine
{
    private var _raceModel: RaceModel = new RaceModel();
    private var _wheelTraceModel: WheelTraceModel = new WheelTraceModel(raceModel);
    private var _collisionCount: int = 0;
    private var _collCountBig: int = 0;
    private var _tickCount: int = 0;
    private var _lastCollCountTime: Number = 0;
    private var _collisionFreqProperty: Number = 0;
    public var substepCount: int = 10;

    public function get cars(): ArrayList
    {
        return raceModel.getCars() as ArrayList;
    }

    private function getObstacles(car: CarModel): Collection
    {
        var list: Collection = raceModel.getRaceWorld().getObstacles(car.getBorder());
        return list;
    }

    private function calcCollFreq(car:CarModel): void
    {
        _tickCount++;
        if (car == raceModel.localCar.carModel)
        {
            var now: Number = Utils.now();
            var delta: Number = Math.abs(now - _lastCollCountTime);
            if (delta > 500)
            {
                if (_tickCount > 0)
                {
                    _collisionFreqProperty = _collCountBig / _tickCount;
                }
                _lastCollCountTime = now;
                _collCountBig = 0;
                _tickCount = 0;
            }
        }
    }

    private function tryCollision(car: CarModel, dt: Number):void
    {
        calcCollFreq(car);
        _collisionCount++;
        if (_collisionCount > 5)
        {
            car.rollback();
            car.myAngularVelocity = 0;
            car.fullStop();
            return;
        }
        var cde:CarDynamicEngine = car.myCarDynamicEngine;
        cde.calcCoordinates(dt);

        var obstacleIt:Iterator = getObstacles(car).iterator();
        while (obstacleIt.hasNext())
        {
            var obstacle:ObstacleModel = obstacleIt.next() as ObstacleModel;
            var collisionPoint: Vector2d = obstacle.tryBodyCollision(car);
            if (collisionPoint != null)
            {
                //вот здесь хочется опять пройтись по всем препятсвиям
                //obstacleIt = getObstacles(car).iterator();
                //	while (obstacleIt.hasNext()){
                //и если было опять столкновение, то взять отрезок, с которым произошло столкновение
                //		collisionPoint = ObstacleModel(obstacleIt.next()).tryBodyCollision(car);
                //		if( collisionPoint != null ){
                //и сдвинуть авто надо вдоль своей оси в направлении от этого отрезка
                //var carCollV: Vector2d = Vector2d(collisionPoint).difference2d(car.myR);
                //var dist: Number = car.myHull.myLength / 2;
                //car.myR.addMultiplied(car.myOrientation, - (carCollV.scalarProduct(car.myOrientation) * dist));
                //
                tryCollision(car, dt);
                if (car == raceModel.localCar.carModel)
                {
                    _collCountBig++;
                }
                return;
                //	}
                //}
                //а если во второй раз столкновения не было, то просто return, в смысле не надо дальше по препятсвиям идти
                //return;
            }
        }
    }

		//sound experiment
		import flash.media.*;
		[Embed(source="/assets/sounds/1000rpm.mp3")]
		[Bindable]
		private var rpm1000Cls:Class;
		[Embed(source="/assets/sounds/2000rpm.mp3")]
		[Bindable]
		private var rpm2000Cls:Class;
		[Embed(source="/assets/sounds/3000rpm.mp3")]
		[Bindable]
		private var rpm3000Cls:Class;
		[Embed(source="/assets/sounds/4000rpm.mp3")]
		[Bindable]
		private var rpm4000Cls:Class;
		[Embed(source="/assets/sounds/6000rpm.mp3")]
		[Bindable]
		private var rpm6000Cls:Class;
		private var sounds:Array = new Array(
			{rpm:1000, snd:new rpm1000Cls()},
			{rpm:2000, snd:new rpm2000Cls()},
			{rpm:3000, snd:new rpm3000Cls()},
			{rpm:4000, snd:new rpm4000Cls()},
			{rpm:60000, snd:new rpm6000Cls()}
		);
		private var sndChannel:SoundChannel = null;
		private var currentSound: Sound = null;
	  private function soundExperiment(rpm: Number): void{
			for( var i:int = 0; i < sounds.length; i++ ){
				if( rpm < sounds[i].rpm ){
					if( sounds[i].snd != currentSound ){
						if( sndChannel != null ) sndChannel.stop();
						currentSound = sounds[i].snd;
						sndChannel = sounds[i].snd.play();
					}
					break;
				}
			}
		}
		//end sound experiment

    /**
     * Вот этот метод было очень тяжело разработать.
     * Логично считать, что столкновение в среднем занимает один квант времени.
     */
    private function fullStep(dt: Number, localOrRemote: Boolean):void
    {
        var carIt: Iterator = cars.iterator();
        //while (carIt.hasNext())
                    //var car: Car = carIt.next() as Car;
        for(var i:int=0; i<cars.length(); i++)
        {
            var car: Car = Car(cars.getItemAt(i));
            var cde: CarDynamicEngine = car.carModel.myCarDynamicEngine;
            if (car.isLocal && localOrRemote)
            {
                _collisionCount = 0;
                tryCollision(car.carModel, dt);
                //cde.calcCoordinates(dt);
                cde.saveState();
                cde.nullForces();
                cde.calcForces();
                cde.calcVelocity(dt);
								soundExperiment(car.carModel.getRPM());
            } else
            if (!car.isLocal && !localOrRemote)
            {
                //tryCollision(car.carModel, dt);
                cde.calcCoordinates(dt);
                //cde.saveState();
                //cde.nullForces();
                //cde.calcForces();
                //cde.calcVelocity(dt);
            }
        }
    }

    public function step(nowTime: Number, lastQuantTime: Number, timeQuant: Number, localOrRemote: Boolean): Number
    {
        var delta: Number = nowTime - lastQuantTime;
        if (delta > 5000)
        {
            delta = 5000;
            lastQuantTime = nowTime - delta;
        }
        var numberOfQuantsSinceLast: Number = int(delta / timeQuant);

        //trace("delta = " + delta + ", numberOfQuantsSinceLast = " + numberOfQuantsSinceLast);

        for (var i: int = 0; i < numberOfQuantsSinceLast; i++)
        {
            fullStep(timeQuant / 1000.0, localOrRemote);
        }
        _wheelTraceModel.updateTraces();

        var updatedLastQuantTime: Number = lastQuantTime + numberOfQuantsSinceLast * timeQuant;
        return updatedLastQuantTime;
    }

    public function get raceModel(): RaceModel
    {
        return _raceModel;
    }

    public function get collFreq(): Number
    {
        return _collisionFreqProperty;
    }
}
}