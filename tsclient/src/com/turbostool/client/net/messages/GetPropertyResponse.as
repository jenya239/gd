package com.turbostool.client.net.messages
{
public class GetPropertyResponse extends ReasonedResponse
{
    public static const GET_GAME_TYPES_RESPONSE: String = "getGameTypesResponse";
    public static const GET_CARS_RESPONSE: String = "getCarsResponse";
    public static const GET_LOBBIES_RESPONSE: String = "getLobbiesResponse";
    public static const GET_ROUTES_RESPONSE: String = "getRoutesResponse";
    public static const GET_LOBBY_RESPONSE: String = "getLobbyResponse";
    public static const GET_RATINGS_RESPONSE: String = "getRatingsResponse";
    public static const GET_USERINFO_RESPONSE: String = "getUserInfoResponse";
    public static const GET_GLOBALINFO_RESPONSE: String = "getGlobalInfoResponse";
    public static const GET_SHOPINFO_RESPONSE: String = "getShopInfoResponse";
    public static const GET_CARSHOPINFO_RESPONSE: String = "getCarShopInfoResponse";
    public static const GET_GAS_RESPONSE: String = "getGasInfoResponse";
    public static const GET_TIP_RESPONSE: String = "getTipInfoResponse";
    public static const GET_LEAGUE_RESPONSE: String = "getLeagueInfoResponse";
    public static const GET_INBOX_RESPONSE: String = "getInboxResponse";
    public static const GET_VKONTAKTEINFO_RESPONSE: String = "getVkontakteInfoResponse";
    public static const GET_SECUREUSERINFO_RESPONSE: String = "getSecureUserInfoResponse";
    public static const GET_FRIENDINFOS_RESPONSE: String = "getFriendInfosResponse";
    public static const GET_INVITESINFO_RESPONSE: String = "getInvitesInfoResponse";
    public static const GET_CITIES_RESPONSE: String = "getCitiesResponse";
    public static const GET_TOPUSERDAILYSCORES_RESPONSE: String = "getTopUserDailyScoresResponse";

    private var _property: Object;

    public function GetPropertyResponse(channel: String = ChanneledMessage.GLOBAL_CHANNEL)
    {
        super(channel);
    }

    public function get property(): Object
    {
        return _property;
    }

    public function get propertyAsArray(): Array
    {
        return _property as Array;
    }

    public function set property(val: Object): void
    {
        _property = val;
    }
}
}