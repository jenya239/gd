package com.turbostool.client {
import com.google.analytics.AnalyticsTracker;
import com.google.analytics.GATracker;

import com.turbostool.client.utils.Utils;

import flash.display.DisplayObject;

public class Tracker {
    private static var _tracker: AnalyticsTracker;
    private static var _instance: Tracker;

    public function Tracker() {
    }

    public static function create(displayObject: DisplayObject): void {
        _tracker = new GATracker(displayObject, "UA-8353521-2", "AS3", false);
        _instance = new Tracker();
    }

    public static function get instance(): Tracker {
        return _instance;
    }

    public function trackTrans(name: String, category: String, price: Number, realPrice: Number): void {
        if (isDeveloper) return;
        
        if(realPrice > 0) {
            var orderID: String = "" + Utils.now();
            var city: String;

            switch(Client.instance.modelsStorage.userInfo.homeCity) {
                case 1:
                    city = "Blue";
                    break;
                case 2:
                    city = "Red";
                    break;
            }

            _tracker.addTrans(orderID, "", realPrice, 0, 0, city, "", "");
            _tracker.addItem(orderID, "", name, category, realPrice, 1);

            _tracker.trackTrans();

            _tracker.trackPageview("/customer/realmoney/" + Client.instance.modelsStorage.userInfo.level);
            _tracker.trackEvent("Payment", "Gold", "Level: " + Client.instance.modelsStorage.userInfo.level, realPrice);
        } else {
            _tracker.trackPageview("/customer/virtualmoney/" + Client.instance.modelsStorage.userInfo.level);
            _tracker.trackEvent("Payment", "Rubles", "Level: " + Client.instance.modelsStorage.userInfo.level, realPrice);
        }
    }

    private function get isDeveloper(): Boolean
    {
        if (!Client.instance.isAPIWrapper) return true;
        if (Client.instance.modelsStorage != null && Client.instance.modelsStorage.userInfo != null && Client.instance.modelsStorage.userInfo.hasRole("dev")) return true;        
        return false;
    }

    public function trackEvent(category: String, action: String, label: String = null, value: Number = Number.NaN): void
    {
        if (isDeveloper) return;
        _tracker.trackEvent(category, action, label, value);
    }

    public function trackPageview(url: String): void
    {
        if (isDeveloper) return;
        _tracker.trackPageview(url);
    }
}
}