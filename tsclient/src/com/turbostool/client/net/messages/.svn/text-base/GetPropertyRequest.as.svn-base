package com.turbostool.client.net.messages
{
public class GetPropertyRequest extends ServerRequest
{
    public static const GET_PROPERTY: String = "get";

    public static const GAME_TYPES: String = "gameTypes";
    public static const CARS: String = "cars";
    public static const LOBBIES: String = "lobbies";
    public static const ROUTES: String = "routes";
    public static const RATINGS: String = "ratings";
    public static const USER_INFO: String = "userInfo";
    public static const USER_STATE: String = "userState";
    public static const GLOBAL_INFO: String = "globalInfo";
    public static const SHOP_INFO: String = "shopInfo";
    public static const CAR_SHOP_INFO: String = "carShopInfo";
    public static const GAS_INFO: String = "gasInfo";
    public static const VKONTAKTE_INFO: String = "vkontakteInfo";
    public static const CITIES: String = "cities";
    public static const INBOX: String = "inbox";
    public static const TOP_USER_DAILY_SCORES: String = "topUserDailyScores";

    private var _property: String;

    public function GetPropertyRequest(property: String)
    {
        super(GET_PROPERTY);

        _property = property;
    }

    [Serializable(order=1)]
    public function get property(): String
    {
        return _property;
    }
}
}