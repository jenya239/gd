package com.turbostool.client.utils.collections
{
public class Pair {
    private var myFirstProperty:Object;
    private var mySecondProperty:Object;

    public function Pair(first:Object, second:Object) {
        myFirst = first;
        mySecond = second;
    }

    public function get myFirst():Object {
        return myFirstProperty;
    }

    public function set myFirst(setValue:Object):void {
        myFirstProperty = setValue;
    }

    public function get mySecond():Object {
        return mySecondProperty;
    }

    public function set mySecond(setValue:Object):void {
        mySecondProperty = setValue;
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Pair)) return false;
        var pair:Pair = (o as Pair);
        return ((myFirst == pair.myFirst) && (mySecond == pair.mySecond))
                || ((myFirst == pair.mySecond) && (mySecond == pair.myFirst));
    }

    public function toString():String {
        return '(' + myFirst.toString() + '; ' + mySecond.toString() + ')';
    }

}
}