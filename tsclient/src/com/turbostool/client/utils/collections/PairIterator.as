package com.turbostool.client.utils.collections
{
public class PairIterator implements Iterator
{
    private var myFirstCursor:int;
    private var mySecondCursor:int;
    private var myCollection:Collection;
    private var myIt1:Iterator;
    private var myIt2:Iterator;
    private var myO1:Object;
    private var myReturnedPairCount:int;

    public function PairIterator(collection:Collection) {
        myCollection = collection;
        myReturnedPairCount = 0;
        //myIt1 = myCollection.iterator();
        //myIt2 = myCollection.iterator();
    }

    public function hasNext():Boolean {
        return (myReturnedPairCount < getPairCount());
    }

    public function next():Object {
        if (myIt1 == null) {
            myIt1 = myCollection.iterator();
            myO1 = myIt1.next();
            myIt2 = myCollection.iterator();
            myIt2.next();
        }
        if (!myIt2.hasNext()) {
            myO1 = myIt1.next();
            myIt2 = myCollection.iterator();
            while (myIt2.hasNext()) {
                if (myIt2.next() == myO1) break;
            }
        }
        myReturnedPairCount++;
        return new Pair(myO1, myIt2.next());
    }

    private function getPairCount():int {
        return (myCollection.length() * (myCollection.length() - 1)) / 2;
    }

}
}