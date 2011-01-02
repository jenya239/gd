package com.turbostool.client.event
{
import flash.events.Event;

public class AuthorizationChangedEvent extends Event
{
    public static const AUTHORIZATION_CHANGED: String = "authorizationChanged";

    public var authorized: Boolean;
    public var isQuickConnect: Boolean;

    public function AuthorizationChangedEvent(authorized: Boolean, isQuickConnect: Boolean = false)
    {
        super(AUTHORIZATION_CHANGED);
        this.authorized = authorized;
        this.isQuickConnect = isQuickConnect;
    }
}
}