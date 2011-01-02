package com.turbostool.client.event
{
import flash.events.Event;

public class ProgressEvent2 extends Event
{
    public static const PROGRESS_EVENT2: String = "progressEvent2";
    public var current: Number;
    public var total: Number;
    public var bytesLoaded: int;

    public function ProgressEvent2(current: Number, total: Number, bytesLoaded: int)
    {
        super(PROGRESS_EVENT2);
        this.current = current;
        this.total = total;
        this.bytesLoaded = bytesLoaded;
    }

    override public function clone(): Event
    {
        return new ProgressEvent2(this.current, this.total, this.bytesLoaded);
    }
}
}