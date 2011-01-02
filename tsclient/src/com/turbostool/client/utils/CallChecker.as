package com.turbostool.client.utils
{
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

public class CallChecker
{
    private var myMethodRecords:Collection;

    public function CallChecker() {
        reset();
    }

    public function reset():void {
        myMethodRecords = new ArrayList();
    }

    public function getCalls(methodName:String):Array {
        var it:Iterator = myMethodRecords.iterator();
        while (it.hasNext()) {
            var rec:MethodRecord = it.next() as MethodRecord;
            if (rec.getName() == methodName) {
                return rec.getCalls();
            }
        }
        return new Array();
    }

    public function check(methodName:String):void {
        var now:Date = new Date();

        var it:Iterator = myMethodRecords.iterator();
        while (it.hasNext()) {
            var rec:MethodRecord = it.next() as MethodRecord;
            if (rec.getName() == methodName) {
                rec.getCalls().push(now.time);
                return;
            }
        }
        myMethodRecords.addItem(new MethodRecord(methodName, now.time));
        for (var i:int; i < 100 * 100 * 100; i++) {
            var f:int = 0;
        }
    }
}
}