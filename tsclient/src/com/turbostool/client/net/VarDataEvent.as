package com.turbostool.client.net
{
public class VarDataEvent extends SessionDataEvent {
    public static const VAR_DATA:String = 'varData';

    public static const UNDEFINED_VALUE:String = 'undefined';

    private var myName:String;
    private var myValue:String;

    public function VarDataEvent(type:String, sessionId:int, name:String, value:String) {
        super(type, sessionId);
        myName = name;
        myValue = value;
    }

    public function getName():String {
        return myName;
    }

    public function getValue():String {
        return myValue;
    }

    public static function createMessage(name:String, value:String):VarDataEvent {
        return new VarDataEvent(VAR_DATA, SessionDataEvent.MAGIC_NUMBER_SELF, name, value);
    }

    override public function toString():String {
        var begin:String = (getSessionId() == SessionDataEvent.MAGIC_NUMBER_SELF)
                ? " для себя "
                : " от " + getSessionId() + " ";
        return 'VarMessage' + begin + '. var={' + myName + '=>' + myValue + '}';
    }
}
}