package com.turbostool.client.game
{
import com.turbostool.client.event.CarLoadErrorEvent;
import com.turbostool.client.event.CarLoadedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.RouteAndCarLoadErrorEvent;
import com.turbostool.client.event.RouteAndCarLoadedEvent;
import com.turbostool.client.event.RouteLoadErrorEvent;
import com.turbostool.client.event.RouteLoadedEvent;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarLoader;
import com.turbostool.client.game.route.RouteLoader;

import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.EventDispatcher;

public class RouteAndCarLoader extends EventDispatcher
{
    private var _routeLoader: RouteLoader;
    private var _carLoader: CarLoader;
    private var _routeName: String;
    private var _carName: String;
    private var _clientID: int;

    public function RouteAndCarLoader(routeLoader:RouteLoader, carLoader:CarLoader)
    {
        _routeLoader = routeLoader;
        _carLoader = carLoader;
    }

    public function load(routedName: String, carName: String, clientID: int): void
    {
        addRouteListeners();

        _routeName = routedName;
        _carName = carName;
        _routeLoader.loadWorldByName(routedName);
        _clientID = clientID;
    }


    public function onRouteLoaded(e: RouteLoadedEvent): void
    {
        if (e.routeName == _routeName)
        {
            removeRouteListeners();
            addCarListeners();
            _carLoader.loadCar(_carName, Client.instance.modelsStorage.userInfo.carFileName, _clientID);
        }        
    }

    public function onRouteLoadError(e: Event): void
    {
        removeRouteListeners();
        dispatchEvent(new RouteAndCarLoadErrorEvent(_routeName, _carName));
    }

    public function onCarLoaded(e: CarLoadedEvent): void
    {
        removeCarListeners();
        e.car.clientID = Car.LOCAL_CAR_ID;
        dispatchEvent(new RouteAndCarLoadedEvent(_routeName, _carName, _routeLoader.getRaceWorld(_routeName), e.car));
    }

    public function onCarLoadError(e: Event): void
    {
        removeCarListeners();
        dispatchEvent(new RouteAndCarLoadErrorEvent(_routeName, _carName));
    }

    private function addRouteListeners(): void
    {
        EventManager.instance.addEventListener(RouteLoadedEvent.ROUTE_LOADED, onRouteLoaded);
        EventManager.instance.addEventListener(RouteLoadErrorEvent.ROUTE_LOAD_ERROR, onRouteLoadError);
    }

    private function addCarListeners(): void
    {
        EventManager.instance.addEventListener(CarLoadedEvent.CAR_LOADED, onCarLoaded);
        EventManager.instance.addEventListener(CarLoadErrorEvent.CAR_LOAD_ERROR, onCarLoadError);
    }

    private function removeRouteListeners(): void
    {
        EventManager.instance.removeEventListener(RouteLoadedEvent.ROUTE_LOADED, onRouteLoaded);
        EventManager.instance.removeEventListener(RouteLoadErrorEvent.ROUTE_LOAD_ERROR, onRouteLoadError);
    }

    private function removeCarListeners(): void
    {
        EventManager.instance.removeEventListener(CarLoadedEvent.CAR_LOADED, onCarLoaded);
        EventManager.instance.removeEventListener(CarLoadErrorEvent.CAR_LOAD_ERROR, onCarLoadError);
    }
}
}
