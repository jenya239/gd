package com.turbostool.client.utils.collections
{
public class Queue
{
    private var myQueue: Array = new Array();

    public function push(item: Object): void {
        myQueue.push(item);
    }

    public function pop(): Object {
        return myQueue.shift();
    }

    public function peek(): Object {
        return myQueue[0];
    }

    public function addCollection(c: Collection): void {
        var it: Iterator = c.iterator();
        while (it.hasNext()) {
            push(it.next());
        }
    }

    public function isEmpty(): Boolean {
        return myQueue.length == 0;
    }

    public function contains(o: Object): Boolean {
        return myQueue.indexOf(o) > -1;
    }

    public function length(): Number {
        return myQueue.length;
    }

}
}