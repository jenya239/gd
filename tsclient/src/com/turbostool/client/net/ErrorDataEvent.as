package com.turbostool.client.net
{
public class ErrorDataEvent extends TSDataEvent {
    public static const ERROR_DATA:String = 'errorData';

    public static const IMPOSSIBLE_MESSAGE:int = 2;
    public static const INCORRECT_BYTES:int = 3;
    public static const NOTINITIALIZED_REMOTE_SESSION:int = 3;

    private var myCode:int;
    private var myDescription:String;

    public function ErrorDataEvent(type:String, code:int, description:String) {
        super(type);
        myCode = code;
        myDescription = description;
    }

    public function getCode():int {
        return myCode;
    }

    public function getDescription():String {
        return myDescription;
    }

    public static function createMessage(code:int, descr:String = null):ErrorDataEvent {
        var d:String = (descr == null)
                ? getDescription(code)
                : descr + ' (' + getDescription(code) + ')';
        return new ErrorDataEvent(ErrorDataEvent.ERROR_DATA, code, d);
    }

    public static function getDescription(code:int):String {
        switch (code) {
            case 2: return 'Incorrect pair message-state'; break;
            case 3: return 'Incorrect message bytes'; break;
            case 4: return 'Обращение к локально не инициализированной удаленной сессии'; break;
            default: return 'Unknown error code';
        }
    }
}
}