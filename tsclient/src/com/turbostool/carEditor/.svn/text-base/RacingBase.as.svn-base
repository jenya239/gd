package com.turbostool.carEditor
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.GameEngine;
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.screens.BaseScreen;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;

import mx.containers.Canvas;

public class RacingBase extends BaseScreen
{
    public var cnvRender: Canvas;

    public var gameEngine: GameEngine;
    private var _app: CarEditor = CarEditor.instance;
    private var _manager: EventManager = EventManager.instance;

    public function RacingBase()
    {
        _manager.addEventListener(RouteSelectedEvent.ROUTE_SELECTED, onRouteSelected);
        _manager.addEventListener(CarSelectedEvent.CAR_SELECTED, onCarSelected);
    }

    public function init(): void
    {
        gameEngine.canvas = cnvRender;

        EventManager.instance.subscribe();

        var path: Vector2dSequence = new Vector2dSequence();
        path.addVertex(new Vector2d(0, -1));
        path.addVertex(new Vector2d(0, 1));
        var raceWorld: RaceWorld = new RaceWorld(
                new ArrayList(), new Piece2d(new Vector2d(0, 0), new Vector2d(2, 0)), path, "tt", new ArrayList()
                );
        gameEngine.registerWorld(raceWorld);
        _manager.dispatchEvent(new RouteSelectedEvent(raceWorld));
        var car: Car = new Car(Car.LOCAL_CAR_ID);
        gameEngine.replaceLocalCar(car);
        _manager.dispatchEvent(new CarSelectedEvent(car));
        gameEngine.start();
    }

    private function onRouteSelected(e: RouteSelectedEvent): void
    {
        gameEngine.registerWorld(e.raceWorld);
    }

    private function onCarSelected(e: CarSelectedEvent): void
    {
        e.car.clientID = Car.LOCAL_CAR_ID;
        gameEngine.replaceLocalCar(e.car);
    }
}
}