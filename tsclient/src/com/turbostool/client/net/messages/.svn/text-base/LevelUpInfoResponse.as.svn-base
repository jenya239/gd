package com.turbostool.client.net.messages {
import com.turbostool.client.model.CarClass;
import com.turbostool.client.model.ItemClass;
import com.turbostool.client.model.ItemInfo;
import com.turbostool.client.model.RouteInfo;
import com.turbostool.client.model.UserInfo;

import mx.collections.ArrayCollection;

public class LevelUpInfoResponse extends ReasonedResponse {

    public static const LEVEL_UP_INFO: String = "levelUpInfo";

    private var _newUpgrades: Array;
    private var _newCarsCollection: ArrayCollection;
    private var _newCars: Array;
    private var _newRoutes: Array;

    public function LevelUpInfoResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get newUpgrades():Array {
        return _newUpgrades;
    }

    public function set newUpgrades(value:Array):void {
        var items: ArrayCollection = new ArrayCollection();
        for each(var itemClass: ItemClass in value) {
            items.addItem(itemClass.toItemInfo());
        }

        _newUpgrades = items.toArray();
    }

    public function get newCars():Array {
        return _newCars;
    }

    public function set newCars(value:Array):void {
        _newCars = value;

        newCarsCollection = new ArrayCollection(_newCars);
    }

    public function get newCarsCollection():ArrayCollection {
        return _newCarsCollection;
    }

    public function set newCarsCollection(value:ArrayCollection):void {
        _newCarsCollection = value;
    }

    public function get newRoutes():Array {
        return _newRoutes;
    }

    public function set newRoutes(value:Array):void {
        var routes: ArrayCollection = new ArrayCollection();
        for each(var r: RouteInfo in value) {
            r.displayName = r.displayName.toLocaleUpperCase();
            routes.addItem(r);
        }

        _newRoutes = routes.toArray();
    }
}
}