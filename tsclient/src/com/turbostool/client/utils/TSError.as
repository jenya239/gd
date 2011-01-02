package com.turbostool.client.utils {
import mx.controls.Alert;

public class TSError extends Error {
    private var myNestedError:Error = null;

    public function TSError(message:String = null, nestedError:Error = null) {
        super(message == null ? '' : message);
        myNestedError = nestedError;
        Alert.show(message);
    }

    override public function getStackTrace():String {
        var stackTrace:String = super.getStackTrace();
        if (myNestedError != null) {
            stackTrace += '\n Nested Error (' + myNestedError + ') Stack Trace \n'
                    + myNestedError.getStackTrace();
        }
        return stackTrace;
    }

    public function toString():String {
        return message;
    }

}
}