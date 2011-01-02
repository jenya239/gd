package com.turbostool.client.utils.logger {
import mx.controls.TextArea;
import mx.core.Application;
import mx.logging.*;
import mx.logging.targets.TraceTarget;

public class PanelTarget extends TraceTarget {
    //жуткий костыыыыыыыыль x_x
    override public function logEvent(event:LogEvent):void {
        return;
        var date:Date = new Date();
        var msg:String = '[' + date.toLocaleTimeString() + '] ['
                + LogEvent.getLevelString(event.level) + '] [' + event.message + ']';
        var ta: TextArea = (Application.application['taMessages'] as TextArea);
        ta.htmlText = msg + '<br/>' + ta.htmlText;
    }
}
}