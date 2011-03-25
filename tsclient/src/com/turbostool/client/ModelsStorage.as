package com.turbostool.client
{

import com.turbostool.client.gas.Gas;
import com.turbostool.client.model.CarInfo;
import com.turbostool.client.model.GasInfo;
import com.turbostool.client.model.GlobalInfo;
import com.turbostool.client.model.InvitesInfo;
import com.turbostool.client.model.LevelUpInfo;
import com.turbostool.client.model.RouteInfo;
import com.turbostool.client.model.UserDailyScoreInfo;
import com.turbostool.client.model.UserInfo;
import com.turbostool.client.model.VkontakteInfo;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.collections.FilteredCollection;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.TimerEvent;
import flash.utils.Timer;

import mx.binding.utils.ChangeWatcher;
import mx.collections.ArrayCollection;
import mx.collections.Sort;
import mx.collections.SortField;

[Bindable]
public class ModelsStorage extends EventDispatcher
{
    private var _userInfo: UserInfo;

    private var _routes: Array;
    private var _cars: Array;  //короче, TracingUtils.obj показал, что индексы в этом массиве просто
															//последовательные
    private var _shop: Array;
    private var _carShop: Array;
    private var _inbox:Array;
    private var _gas:GasInfo;
    private var _friends: ArrayCollection;
    private var _tips:Array;
    private var _leagues:Array;
    private var _globalInfo: GlobalInfo;
    private var _vkontakteInfo: VkontakteInfo;
    private var _invitesInfo: InvitesInfo;
    private var _cities: Array;
    private var _citiesCollection: ArrayCollection;
    private var _topScores: Array;

    private static var instance:ModelsStorage;

    public function ModelsStorage()
    {
        if (instance != null) throw new TSError("Model Storage had been");

        instance = this;
        _gas = new GasInfo();
        ChangeWatcher.watch(this, ["userInfo", "currentCity"], function(): void {
            dispatchEvent(new Event("updateRoutes"));
        });
    }

    public function get leagues():Array {
        return _leagues;
    }

    public function set leagues(val:Array):void {
        _leagues = val;
    }

    public function get tips():Array {
        return _tips;
    }

    public function set tips(val:Array):void {
        _tips = val;
    }

    public function get carShop(): Array {
        //Alert.show("modelsStorage get carShop" + _carShop);
        return _carShop;
    }

    public function set carShop(val:Array):void {
        //trace("set car shop");
        //Alert.show("modelsStorage set carShop");
        _carShop = val;
    }

    public function get userInfo(): UserInfo
    {
        return _userInfo;
    }

    public function set userInfo(val: UserInfo): void
    {
        _userInfo = val;
        dispatchEvent(new Event("updateRoutes"));
    }

    public function get gas():GasInfo {
        return _gas;
    }

    public function set gas(val:GasInfo):void {
        _gas = val;
        var myTimer:Timer = new Timer(500, 1);
        myTimer.addEventListener("timer", gasTimerHandler);
        myTimer.start();
    }

    private function gasTimerHandler(event:TimerEvent):void {
        if (Gas.getInstance() != null)
            Gas.getInstance().changeFuelCount();
    }

    public function getRouteInfoByID(routeID: int): RouteInfo
    {
        for each(var routeInfo: RouteInfo in _routes)
        {
            if (routeInfo.id == routeID)
            {
                return routeInfo;
            }
        }
        return null;
    }

    public function getCarInfoByID(carID: int): CarInfo
    {
        for each(var carInfo: CarInfo in _cars)
        {
            if (carInfo.id == carID)
            {
                return carInfo;
            }
        }
        return null;
    }

    public function get cars():Array
    {
        return _cars;
    }

    public function get routes():Array
    {
        return _routes;
    }

    public function set routes(val:Array):void
    {
        _routes = val;
        dispatchEvent(new Event("updateRoutes"));
    }

    [Bindable(event="updateRoutes")]
    public function get availableRoutes():ArrayCollection
    {
        var routes: FilteredCollection = new FilteredCollection(_routes, filterRoute);
        var sort: Sort = new Sort();
        sort.fields = [new SortField("minLevel", true, true, true)];
        routes.sort = sort;
        routes.refresh();
        return routes;
    }

    private function filterRoute(routeInfo: RouteInfo): Boolean
    {
        return userInfo.level >= routeInfo.minLevel &&
               ((routeInfo.isHomeCity && userInfo.currentCity < 3) || (routeInfo.isBattleCity && userInfo.currentCity == 3));
    }

    public function set cars(val: Array): void
    {
        _cars = val;
    }

    public function get shop(): Array
    {
        return _shop;
    }

    public function set shop(value: Array): void
    {
        _shop = value;
    }

    public function get globalInfo(): GlobalInfo
    {
        return _globalInfo;
    }

    public function set globalInfo(value: GlobalInfo):void
    {
        _globalInfo = value;
    }

    public function get inbox(): Array
    {
        return _inbox;
    }

    public function set inbox(value: Array): void
    {
        _inbox = value;
    }

    public function get vkontakteInfo(): VkontakteInfo {
        return _vkontakteInfo;
    }

    public function set vkontakteInfo(val: VkontakteInfo):void {
        _vkontakteInfo = val;
    }

    public function get friends(): ArrayCollection
    {
        return _friends;
    }

    public function set friends(value: ArrayCollection): void
    {
        _friends = value;
    }

    public function get invitesInfo(): InvitesInfo
    {
        return _invitesInfo;
    }

    public function set invitesInfo(value: InvitesInfo): void
    {
        _invitesInfo = value;
    }

    public function get cities(): Array
    {
        return _cities;
    }

    public function set cities(value: Array): void
    {
        _cities = value;
        _citiesCollection = new ArrayCollection(value);

        var sort: Sort = new Sort();
        sort.fields = [new SortField("id", false, false, true)];
        _citiesCollection.sort = sort;
        _citiesCollection.refresh();

        dispatchEvent(new Event("updateCitiesCollection"));
    }

    [Bindable(event="updateCitiesCollection")]
    public function get citiesCollection(): ArrayCollection
    {
        return _citiesCollection;
    }

    public function get topScores():Array
    {
        return _topScores;
    }

    public function set topScores(value:Array):void
    {
        _topScores = value;
        dispatchEvent(new Event("updateTopScoresCollection"));
    }

    [Bindable(event="updateTopScoresCollection")]
    public function getTopScoresByCity(city: Number): ArrayCollection
    {
        var collection: ArrayCollection = new FilteredCollection(_topScores, function (score: UserDailyScoreInfo): Boolean {
            return score.homeCity == city;
        });
        for (var i: int = 0; i < collection.length; i++)
        {
            collection.getItemAt(i).position = "" + (i + 1) + ".";
        }
        return collection;
    }
}
}