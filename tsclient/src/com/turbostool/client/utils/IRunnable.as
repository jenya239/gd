package com.turbostool.client.utils {
public interface IRunnable {
    function run():void;

    function stop():void;

    function isWorking():Boolean;
}
}