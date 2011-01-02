package com.turbostool.client.utils.collections
{


public class HashMap
{
    protected var myKeys: ArrayList;
    protected var myValues: ArrayList;

    public function HashMap()
    {
        createArrays();
    }

    protected function createArrays(): void
    {
        myKeys = new ArrayList();
        myValues = new ArrayList();
    }

    public function getKeys(): Array
    {
        return myKeys.toArray();
    }

    public function length():Number
    {
        return myKeys.length();
    }

    public function containsKey(item:Object):Boolean
    {
        return myKeys.contains(item);
    }

    public function containsValue(item:Object):Boolean
    {
        return myValues.contains(item);
    }

    public function isEmpty():Boolean
    {
        return (this.length() == 0);
    }

    public function iterator():Iterator
    {
        return myKeys.iterator();
    }

    public function clear():void
    {
        myKeys.clear();
        myValues.clear();
    }

    public function getKey(value:Object):Object
    {
        var valueIndex: int = myValues.getItemIndex(value);
        if (valueIndex == -1)
            return null;
        else
            return myKeys.getItemAt(valueIndex);
    }

    public function getValue(key:Object):Object
    {
        var keyIndex: int = myKeys.getItemIndex(key);
        if (keyIndex == -1)
            return null;
        else
            return myValues.getItemAt(keyIndex);
    }

    public function setValue(key:Object, value: Object): void
    {
        var keyIndex: int = myKeys.getItemIndex(key);
        if (keyIndex > -1)
        {
            myValues.toArray()[keyIndex] = value;
        }
        else
        {
            myKeys.addItem(key);
            myValues.addItem(value);
        }
    }

    public function getIndex(value: Object): int
    {
        return myValues.getItemIndex(value);
    }

    public function remove(key: Object): void
    {
        var keyIndex: int = myKeys.getItemIndex(key);
        if (keyIndex > -1)
        {
            myKeys.removeItemAt(keyIndex);
            myValues.removeItemAt(keyIndex);
        }
    }

    public function synchronizeWithCollection(collection:Collection, createValue:Function):void
    {
        //delete some thing
        var it:Iterator = this.iterator();
        var arrayOfIndexesToRemove:ArrayList = new ArrayList();
        while (it.hasNext())
        {
            var currKey:Object = it.next();
            if (! collection.contains(currKey))
            {
                arrayOfIndexesToRemove.addItem(currKey);
            }
        }
        for (var i:int = 0; i < arrayOfIndexesToRemove.length(); i++)
        {
            remove(arrayOfIndexesToRemove.getItemAt(i));
        }
        //add some thing
        it = collection.iterator();
        while (it.hasNext())
        {
            var curr:Object = it.next();
            if (! this.containsKey(curr))
            {
                this.setValue(curr, createValue(curr));
            }
        }
    }
}
}