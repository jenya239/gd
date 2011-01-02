package com.turbostool.carEditor
{
import com.turbostool.client.event.CarLoadedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.RouteLoadErrorEvent;
import com.turbostool.client.event.RouteLoadedEvent;
import com.turbostool.client.game.components.car.CarLoader;
import com.turbostool.client.game.route.RouteLoader;

import flash.events.Event;
import flash.events.IOErrorEvent;
import flash.net.URLLoader;
import flash.net.URLRequest;

import mx.containers.Canvas;
import mx.controls.Alert;
import mx.controls.ComboBox;
import mx.controls.Label;
import mx.events.FlexEvent;

public class LoaderPanelBase extends Canvas
{
    private var _routeLoader: RouteLoader = RouteLoader.instance;
    private var _carLoader: CarLoader = CarLoader.instance;
    private var _manager: EventManager = EventManager.instance;
    private var _app: CarEditor = CarEditor.instance;

    public var cbCars: ComboBox;
    public var cbRoutes: ComboBox;
    public var lblFocus: Label;

    public var savedCarName: String = "";

    public function LoaderPanelBase()
    {
        super();
        _manager.addEventListener(RouteLoadedEvent.ROUTE_LOADED, onRouteLoaded);
        _manager.addEventListener(RouteLoadErrorEvent.ROUTE_LOAD_ERROR, onRouteLoadError);
        _manager.addEventListener(CarLoadedEvent.CAR_LOADED, onCarLoaded);
        _app.addEventListener(FlexEvent.APPLICATION_COMPLETE, onAppComplete);
    }

    private function onCarsChange(e: Event): void
    {
        _carLoader.loadCar(cbCars.selectedLabel, cbCars.selectedLabel, 0);
        lblFocus.setFocus();
    }

    private function onRoutesChange(e: Event): void
    {
        _routeLoader.loadWorldByName(cbRoutes.selectedLabel);
        lblFocus.setFocus();
    }

    public function loadCarList(): void
    {
        function onCarsLoaded(e: Event): void
        {
            var xmlList: XMLList = new XML(carsLoader.data).children();
            var carsArray: Array = new Array();
            for each(var xmlNode: XML in xmlList)
            {
                carsArray.push(xmlNode.attribute("fileNameBase").toString());
            }
            cbCars.dataProvider = carsArray;
            if (savedCarName != "") {
                cbCars.selectedItem = savedCarName;
            }
        }

        function onIOError(e: Event): void
        {
            cbCars.dataProvider = ["2101", "2108", "bmw", "dodge", "focus", "porshe", "subaru", "veyron"];
        }

        //try{
        var carsLoader: URLLoader = new URLLoader();
        carsLoader.addEventListener(Event.COMPLETE, onCarsLoaded);
        carsLoader.addEventListener(IOErrorEvent.IO_ERROR, onIOError);
        carsLoader.load(new URLRequest("carServices.php?action=list"));
        //}catch( e: Error ){
        //	cbCars.dataProvider = ["2101", "2108", "bmw", "dodge", "focus", "porshe", "subaru", "veron"];
        //}
    }

    private function loadRouteList(): void
    {
        function onRoutesLoaded(e: Event): void
        {
            var xmlList: XMLList = new XML(routesLoader.data).children();
            var routesArray: Array = new Array();
            for each(var xmlNode: XML in xmlList)
            {
                routesArray.push(xmlNode.attribute("fileNameBase").toString());
            }
            cbRoutes.dataProvider = routesArray;
        }

        var routesLoader: URLLoader = new URLLoader();
        routesLoader.addEventListener(Event.COMPLETE, onRoutesLoaded);
        routesLoader.load(new URLRequest("data/routes/routes.xml"));
    }

    private function onAppComplete(e: FlexEvent): void
    {
        cbCars.addEventListener(Event.CHANGE, onCarsChange);
        cbRoutes.addEventListener(Event.CHANGE, onRoutesChange);
        loadCarList();
        loadRouteList();


        _routeLoader.loadWorldByName("raceMuha");
        _carLoader.loadCar("dodge", "dodge", 0);
    }

    private function onRouteLoaded(e: RouteLoadedEvent): void
    {
        _manager.dispatchEvent(new RouteSelectedEvent(_routeLoader.getRaceWorld(e.routeName)));
    }

    private function onRouteLoadError(e: RouteLoadErrorEvent): void
    {
        Alert.show(e.error.toString());
    }

    private function onCarLoaded(e: CarLoadedEvent): void
    {
        cbCars.selectedItem = e.carName;
        savedCarName = e.carName;
        _manager.dispatchEvent(new CarSelectedEvent(e.car));
    }

}
}