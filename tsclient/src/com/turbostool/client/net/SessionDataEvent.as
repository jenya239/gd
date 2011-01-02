package com.turbostool.client.net {
public class SessionDataEvent extends TSDataEvent {
    public static const ADD_SESSION:String = 'addSessionData';
    public static const REMOVE_SESSION:String = 'removeSessionData';
    public static const MAGIC_NUMBER_SELF:int = -1;
    private var mySessionId:int;

    public function SessionDataEvent(type:String, sessionId:int) {
        super(type);
        mySessionId = sessionId;
    }

    public function getSessionId():int {
        return mySessionId;
    }

    override public function toString(): String {
        return type + ". magic number: " + mySessionId;
    }

}
}