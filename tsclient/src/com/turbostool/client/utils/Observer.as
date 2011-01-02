package com.turbostool.client.utils {
public class Observer {
    public var handler : Function;

    public function set source(source : *) : void
    {
        handler.call();
    }
}
}