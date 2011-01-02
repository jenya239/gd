package com.turbostool.client.event {
import com.turbostool.client.model.UserInfo;
import flash.events.Event;
public class NickClickEvent extends Event{

    public static const NICK_CLICK: String = "nickClick";

    private var _link: String;
    private var _userID: Number;

    public function get link(): String {
        return _link;
    }

    public function NickClickEvent(link: String, userID: Number) {
        super(NICK_CLICK);
        
        _link = link;
        _userID = userID;
    }

    public function get userID(): Number {
        return _userID;
    }
}
}