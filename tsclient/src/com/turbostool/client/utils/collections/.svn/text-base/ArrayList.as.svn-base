package com.turbostool.client.utils.collections {


public class ArrayList implements Collection {

    public function ArrayList(item:Object = null)
    {
        items = new Array();
        if (item != null) addItem(item);
        _iterator_shared = new CollectionIterator(this);
    }

    //------------------------------------------------------------------------------

    public function addItem(item:Object):Boolean
    {
        //todo why couldnt we add nulls??
        //if ( item == null )
        //	return false;

        items.push(item);
        return true;
    }

    //------------------------------------------------------------------------------

    public function clear():void
    {
        items = new Array();
    }

    //------------------------------------------------------------------------------

    public function contains(item:Object):Boolean
    {
        return ( getItemIndex(item) > -1 );
    }

    //------------------------------------------------------------------------------

    public function getItemAt(index:Number):Object
    {
        if ((index < 0) || (index > items.length - 1)) throw new Error(index + ' out of bound');
        return ( items[ index ] );
    }

    //------------------------------------------------------------------------------

    private var _iterator_shared: CollectionIterator; 

    public function iterator():Iterator
    {
        //return ( Iterator( new CollectionIterator( this )));
        //return new CollectionIterator(this);
        _iterator_shared.cursor = 0;
        return _iterator_shared;
    }

    //------------------------------------------------------------------------------

    public function length():Number
    {
        return items.length;
    }

    //------------------------------------------------------------------------------

    public function isEmpty():Boolean
    {
        return ( items.length == 0 );
    }

    //------------------------------------------------------------------------------

    public function removeItem(item:Object):Boolean
    {
        var itemIndex:Number = getItemIndex(item);
        if (itemIndex < 0)
            return false;

        items.splice(itemIndex, 1);
        return true;
    }

    public function removeItemAt(index: int):Boolean
    {
        if (index < 0)
            return false;

        items.splice(index, 1);
        return true;
    }

    //------------------------------------------------------------------------------

    public function toArray() : Array
    {
        return items;
    }

    //------------------------------------------------------------------------------

    //	public function getItemIndex( item:Object ):Number
    //	{
    //		for ( var i:uint=0; i<items.length; i++ )
    //		{
    //			if ( items[ i ] == item )
    //				return i;
    //		}
    //		return -1;
    //		//throw new Error('Element ' + item + ' not exist in this ArrayList');
    //	}

    public function getItemIndex(item:Object):Number
    {
        for (var i:uint = 0; i < items.length; i++)
        {
            if (items[ i ] == item)
                return i;
        }
        return -1;
        //throw new Error('Element ' + item + ' not exist in this ArrayList');
    }

    //------------------------------------------------------------------------------

    public function addCollection(collection:Collection):void {
        var iterator:CollectionIterator = collection.iterator() as CollectionIterator;
        while (iterator.hasNext()) {
            addItem(iterator.next());
        }
    }


    //------------------------------------------------------------------------------

    public function pairIterator():Iterator {
        return new PairIterator(this);
    }

    //------------------------------------------------------------------------------

    public function subtract(collection:Collection):void {
        var iterator:Iterator = collection.iterator();
        while (iterator.hasNext()) {
            removeItem(iterator.next());
        }
    }

    //------------------------------------------------------------------------------

    public function cloneCollection():Collection {
        var l:ArrayList = new ArrayList();
        l.addCollection(this);
        return l;
    }

    //------------------------------------------------------------------------------

    public function sort(compareFunction:Function):void {
        items.sort(compareFunction);
    }

    public function removeCollection(collection:Collection):void {
        var it:Iterator = collection.iterator();
        while (it.hasNext()) {
            removeItem(it.next());
        }
    }

    public function lastElement():Object {
        return items[items.length - 1];
    }

    public function insertSorted(o: Object, comparator :Function): void {
        for (var i: int = 0; i < length(); i++) {
            if (comparator(o, items[i]) < 0) {
                insertAfter(i - 1, o);
                return;
            }
        }
        addItem(o);
    }

    public function insertAfter(index: int, o: Object): void {
        var newArray: Array = new Array();
        for (var i: int = 0; i <= index; i++) {
            newArray[i] = items[i];
        }
        newArray[index + 1] = o;
        for (i = index + 2; i <= items.length; i++) {
            newArray[i] = items[i - 1];
        }
        items = newArray;
    }

    public function get fuckToString():String {
        var result: String = '[ ';
        for (var i:int = 0; i < items.length; i++) {
            result += items[i].fuckToString() + ' ';
        }
        result += ']';
        return result;
    }

    public function reverse(): void {
        items.reverse();
    }

    protected var items:Array;
}

}