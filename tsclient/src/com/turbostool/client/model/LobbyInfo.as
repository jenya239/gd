package com.turbostool.client.model
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.EventDispatcher;

[Bindable]
public class LobbyInfo extends EventDispatcher
{
    public static const DIRECTION_FORWARD: String = "forward";

    public static const STATUS_NONE: String = "enum_status_none";
    public static const STATUS_HALL: String = "enum_status_hall";
    public static const STATUS_RACING: String = "enum_status_racing";

    public static const LOBBY_TYPE_NORMAL: String = "normal";
    public static const LOBBY_TYPE_TEAM: String = "team";

    public var id: int;
    public var creatorName: String;
    public var routeID: int;
    public var playerCountBlue: int;
    public var playerCountRed: int;
    public var playerMax: int;
    public var status: String;
    public var timerEnd: Number;
    public var allowedCarID: int;
    public var lapNumber: int;
    public var direction: String;
    public var league: int;
    public var creatorRating:int;
    public var creatorCarName:String;
    public var stake:int;
    public var type: String;

    public function LobbyInfo()
    {
    }

    public function get routeName(): String
    {
        return Client.instance.modelsStorage.getRouteInfoByID(routeID).displayName;
    }

    public function get routeInfo(): String
    {
        var lapsStr: String = " / " + lapNumber
        var directionStr: String = direction == LobbyInfo.DIRECTION_FORWARD ? "" : " / " + Client.instance.str("reverse");
        return routeName + lapsStr + directionStr;
    }

    public function get carName(): String
    {
        return allowedCarID < 0 ? Client.instance.str("any") : Client.instance.modelsStorage.getCarInfoByID(allowedCarID).displayName;
    }

    public function get statusText(): String
    {
        switch (status)
                {
            case STATUS_HALL:
                return Client.instance.str("hall");
            case STATUS_RACING:
                return Client.instance.str("racing");
            default:
                return "?";
        }
    }

    public function fireUpdateTimer(): void
    {
        dispatchEvent(new Event("updateTimer"));
    }

    [Bindable(event="updateTimer")]
    public function get timerText(): String
    {
        return Utils.formatCountdown(timerEnd);
    }

    [Bindable(event="updateTimer")]
    public function get secsToStart(): int
    {
        var timer: Number = timerEnd - Utils.now();
        if (timer < 0) timer = 0;
        return int(timer / 1000);
    }

    [Bindable(event="updateTimer")]
    public function get timerShortText(): String
    {
        var secs: int = secsToStart;
        return ((secs < 10) ? "0" : "") + secs;
    }

    public function get isReverse(): Boolean
    {
        return direction != DIRECTION_FORWARD;
    }

    public function get playerCount(): Number
    {
        return playerCountBlue + playerCountRed;
    }

    [Bindable(event="updateTimer")]
    public function get waitingPlayers(): Boolean
    {
        if (type == LOBBY_TYPE_NORMAL)
        {
            return (playerMax > 1) && (playerCount <= 1);
        }
        else
        {
            return playerCountBlue < 1 || playerCountRed < 1;
        }

    }

    [Bindable(event="updateTimer")]
    public function get mayJoin(): Boolean
    {
        if (type == LOBBY_TYPE_NORMAL || league == 4)
        {
            return checkLevel && (playerCount < playerMax) && (status == STATUS_HALL) && ((league == 4) ? ( (stake < Client.instance.modelsStorage.userInfo.money) && Math.abs(creatorRating - Client.instance.modelsStorage.userInfo.rating) < 200  ) : true );
        }
        else
        {
            var isTeamNotFull: Boolean = false;
            if(Client.instance.modelsStorage.userInfo.homeCity == 1)
                isTeamNotFull = playerCountBlue < playerMax / 2;
            else
                isTeamNotFull = playerCountRed < playerMax / 2;

            return checkLevel && isTeamNotFull && status == STATUS_HALL;
        }
    }

    [Bindable(event="updateTimer")]
    public function get checkLevel(): Boolean
    {
        var modelsStorage: ModelsStorage = Client.instance.modelsStorage;
        if (modelsStorage == null) return false;
        var routeInfo:RouteInfo = modelsStorage.getRouteInfoByID(routeID);
        if (routeInfo == null) return false;
        if (modelsStorage.userInfo == null) return false;
        var routeMinLvl: int = routeInfo.minLevel;
        var userLevel: int = modelsStorage.userInfo.level;
        return userLevel >= routeMinLvl;
    }
}
}